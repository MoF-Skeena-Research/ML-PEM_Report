# required libraries
library(rgrass7)
library(raster)
library(stars)
library(rgdal)
library(rmapshaper)

#### A bit about this script:
# The main idea is to clump together cells in a raster that are similar in close
# proximity to each other so that the raster is despeckled and then 
# after that polygonizing occurs. I've tried finding different resources for
# creating a smooth polygon output, but it was to no avail - the best solution
# remains using GRASS GIS so no matter what, this script requires GRASS to be 
# installed on your machine. This should be taken care of in the script, 
# though it's currently limited to a Windows operating system.

# There are 3 options (clean_levels) for producing the final output. Before
# starting any of the options, SAGA GIS is used to despeckle the raster using
# a built in toolchain. Next, the options are carried out:

# 1. clean_level 1, or the lowest amount of cleaning. Using GRASS, the clumped
# raster is imported and polygonized (a smoothing option is applied in the 
# polygonize step here). Next, the polygon is smoothed and cleaned.

# 2/3. clean_level 2/3, or medium/high cleaning. After the clumped raster is made, the 
# stars package is used to polygonize the raster (note: stars uses the 
# gdal_polygonize algorithm here, makes it fairly efficient). The output polygon
# is blocky from the raster, so it then gets simplified using Andy Teucher's 
# rmapshaper package (the polygon is topologically correct!). This polygon is
# loaded into GRASS where it is then smoothed and cleaned. The difference 
# between clean level 2 and 3 is the option in the ms_simplify function
# "keep_shapes". Keeping all the shapes will result in longer processing times
# but look more similar to the clumped raster. Not keeping the shapes is the 
# fastest option of all options here, but may provide too much cleaning to the 
# final polygon.

# Define input raster (the output model prediction)
# rast_in <- raster("./EagleHills_AOI/2_sample_design/stage1_Analysis/models/allbgc_r_samplepts_all_total_attributed.gpkg/quick_map/response.tif")
#  rast_in <- raster("D:/GitHub/PEM_Methods_DevX/PEM_standards_manuscripts/models/paper_all_var/maps/forest_combo_bgcs.tif")
# clean_level = 3
# tempdir = "C:/Users/whmacken/AppData/Local/Temp/RtmpElPU6p/"
raster_to_polygon <- function(rast_in, clean_level = 3) {
  
  ## Input checks
  if(!class(rast_in) %in% "RasterLayer")
    stop("rast_in is not a RasterLayer object")
  
  # Need to have a file path to the raster for later functions. If it doesn't 
  # exist and it's not already in storage, create a temp file for it
  if(!(file.exists(rast_in@file@name) && 
       rast_in@data@fromdisk && 
       grepl("^INT", dataType(rast_in)))) {
    
    writeRaster(rast_in, file.path(tempdir(), "rast_in.tif"), 
                datatype = "INT2U", overwrite = TRUE)
    rast_in <- raster(file.path(tempdir(), "rast_in.tif"))
  }
  
  
  start_time <- Sys.time()
  
  # Get SAGA and GRASS GIS file path locations
  source("./_functions/get_saga.R")
  source("./_functions/get_grass.R")
  
  saga_cmd <- get_saga()
  grass_cmd <- get_grass()
  
  ## This next part may not be necessary but might help with matching CRS info...
  # Attempt to find proj code by stripping the datum, ellps, and towgs info from 
  # proj4strings and matching by the remaining string
  
  proj_lookup <- paste(grep(
    "+datum|+ellps|+towgs84|+init", unlist(strsplit(
      projection(rast_in), " ")), 
    invert = TRUE, value = TRUE), collapse = " ")
  
  proj_ref <- rgdal::make_EPSG() %>% 
    dplyr::mutate(simple = gsub(
      "\\+datum=NAD83 |\\+ellps=GRS80 |\\+towgs84=0,0,0,0,0,0,0 |\\+towgs84=0,0,0 ", "", prj4)) %>% 
    dplyr::filter(simple == proj_lookup)
  
  # Filter multiple by getting the best result
  proj_code <- as.numeric(
    proj_ref[apply(adist(proj_lookup, proj_ref$prj4), 1, which.min), "code"])
  
  # Set PROJ_LIB environment variable to newly downloaded PROJ, or it will cause problems
  old_projlib <- Sys.getenv("PROJ_LIB")
  Sys.setenv("PROJ_LIB" = file.path(
    Sys.getenv("SystemDrive"), "OSGeo4W64/share/proj"))
  
  # Run SAGA toolchain, saving grid to a tempfile
  system(paste(saga_cmd, "grid_filter SieveAndClump", 
               "-CLASSES", rast_in@file@name, 
               "-FILTERED", file.path(tempdir(), "response_filtered"),
               "-MODE", 1, # 0 looks only at 4 surrounding cells, 1 looks at all 8
               "-SIEVE", 10)) # Sieving threshold, clumps smaller than this many cells are eliminated
  
  # This is where the different methods will diverge:
  # clean_level = 1 means raster is despeckled and then polygonized thereafter
  # this takes the longest amount of time but is most reflective of the 
  # despeckled raster; however, you do get a bit of a "staircase" effect in the
  # output polygon
  if(clean_level == 1) {
    
    # Initialize GRASS
    loc <- initGRASS(gisBase = "C:/OSGeo4W64/apps/grass/grass78",
                     mapset = "PERMANENT",
                     override = TRUE
    )
    
    # assign GRASS projection according to data set
    execGRASS("g.proj",
              flags = c("c", "quiet"),
              proj4 = st_crs(proj_code)$proj4string
    )
    
    # assign GRASS extent and resolution
    execGRASS("g.region",
              flags = c("quiet"),
              n = as.character(rast_in@extent@ymax),
              s = as.character(rast_in@extent@ymin),
              e = as.character(rast_in@extent@xmax),
              w = as.character(rast_in@extent@xmin),
              res = as.character(res(rast_in)[1])
    )
    
    # Need to use sp for writing raster so that GRASS can read it in
    use_sp()
    
    # Convert output SAGA grid to integer type and load temp grid into GRASS
    writeRaster(
      raster(file.path(tempdir(), "response_filtered.sdat")), 
      file.path(tempdir(), "response_filtered.tif"), 
      datatype = "INT2U", overwrite = TRUE
    )
    
    writeRAST(
      readGDAL(file.path(tempdir(), "response_filtered.tif"), silent = TRUE), 
      "rast_filtered", flags = c("quiet", "overwrite")
    )
    
    # Polygonize the grid in GRASS, note the flags here are different than 
    # the flags used in the GRASS only method
    message("GRASS is polygonizing the clumped raster grid, please wait")
    execGRASS("r.to.vect", flags = c("s", "v", "overwrite"), 
              input = "rast_filtered", 
              output = "vect_filtered", 
              type = "area"
    )
    
    # Clean levels 2 and 3 uses the stars package to first polygonize the raster,
    # and then rmapshaper package to simplify polygon geometries (smoothing the
    # staircase look). If a high degree of cleaning is called (clean_level = 3),
    # the resulting polygon layer will have a cleaner look to it because small
    # polygon geometries get cleaned up during the ms_simplify function when the
    # parameter keep_shapes is set to FALSE. Using clean_level 2 is a good middle
    # ground since it keeps the small polygon shapes and the end result doesn't 
    # have a severe staircase look to it
  } else if(clean_level %in% c(2, 3)) {
    
    # Stars package uses gdal_polygonize in the st_as_sf function, might 
    # as well use that here for simplicity!
    shp <- st_as_stars(raster(file.path(tempdir(), "response_filtered.sdat"))) %>% 
      st_as_sf(as_points = FALSE, merge = TRUE, na.rm = TRUE, use_integer = TRUE) %>% 
      {if(clean_level == 2) {
        ms_simplify(., keep_shapes = TRUE)
      } else {
        ms_simplify(., keep_shapes = FALSE)
      }} %>% 
      ms_simplify(keep_shapes = TRUE) %>% 
      st_make_valid() %>% 
      {if(clean_level == 2) {
        st_cast(., "POLYGON")
      } else {
        st_cast(., "MULTIPOLYGON")
      }} %>% 
      st_set_crs(proj_code)
    
    bbox <- st_bbox(shp)
    
    # Initialize GRASS
    loc <- initGRASS(gisBase = "C:/OSGeo4W64/apps/grass/grass78",
                     mapset = "PERMANENT",
                     override = TRUE
    )
    
    # assign GRASS projection according to data set
    execGRASS("g.proj",
              flags = c("c", "quiet"),
              proj4 = st_crs(shp)$proj4string
    )
    
    # assign GRASS extent and resolution
    execGRASS("g.region",
              flags = c("quiet"),
              n = as.character(bbox["ymax"]),
              s = as.character(bbox["ymin"]),
              e = as.character(bbox["xmax"]),
              w = as.character(bbox["xmin"]),
              res = as.character(res(rast_in)[1])
    )
    
    use_sf()
    
    writeVECT(shp, "vect_filtered", v.in.ogr_flags = "overwrite")
  }
  
  # Generalizing helps to remove the staircase appearance of a grid. Adjusting
  # the threshold might be something to try. Source: 
  # https://grass.osgeo.org/grass78/manuals/v.generalize.html
  execGRASS("v.generalize", flags = "overwrite", 
            input = "vect_filtered", 
            output = "vect_smooth", 
            method = "chaiken", 
            threshold = res(rast_in)[1] / 100
  )
  
  # Clean up little extra blobs left over from polygonizing
  # Threshold is set here to the area of 3 raster cells, it seems to look nice
  execGRASS("v.clean", flags = "overwrite", 
            input = "vect_smooth",
            output = "vect_clean", 
            type = "area",
            tool = "rmarea",
            threshold = prod(res(rast_in), 3))
  
  # Alternate tool that does the same thing: SAGA GIS's polygon generalization
  # Would need to read the filtered vector into R, save the shape somewhere,
  # and then run the tool. 
  # Using the SAGA tool has now been tested and I found it's a lot slower and
  # not as good. Stick with GRASS tool
  
  # Read data into R as an sf dataframe, and use the ms_simplify function
  # to simplify the polygon geometries
  use_sf()
  new_polys <- readVECT("vect_clean") %>% 
    {if(clean_level == 1) {
      dplyr::rename(., ss_call = cat)
    } else {
      dplyr::rename(., ss_call = response_filtered)
    }} %>% 
    dplyr::select(ss_call)
  
  # Reset proj_lib
  Sys.setenv("PROJ_LIB" = old_projlib)
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  return(new_polys)
}

# despeck <- raster_despeckle(rast_in = rast_in)

