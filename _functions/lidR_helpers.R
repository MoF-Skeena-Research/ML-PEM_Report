# Function for creating height metrics of interest all at once. Unfortunately, 
# user defined functions for the lidR package need to be created in the R 
# environment in order to run the function properly (i.e.: there are 
# problems scripting this in to the main function)
my_metrics <- function(Z) {
  height <- c()
  for(hp in height_percentiles) {
    height <- paste(height, 
                    paste0("p", hp, " = quantile(Z, probs = ", hp / 100, ")"), 
                    sep = ",\n ")
  }
  eval(parse(text = paste(
    "metrics <- list(sd = sd(Z)", height, ")
          return(metrics)", sep = "")))
}

# Filter noise functions for LAS catalog
lasfilternoise <- function(las, ...) {
  UseMethod("lasfilternoise", las)
}

lasfilternoise.LAS <- function(las, sensitivity) {
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- lasmergespatial(las, p95, "p95")
  las <- lasfilter(las, Z < p95 * sensitivity)
  las$p95 <- NULL
  return(las)
}

lasfilternoise.LAScluster <- function(las, sensitivity) {
  las <- readLAS(las)
  if (is.empty(las)) return(NULL)
  las <- lasfilternoise(las, sensitivity)
  las <- lasfilter(las, buffer == 0)
  return(las)
}

lasfilternoise.LAScatalog <- function(las, sensitivity) {
  opt_select(las) <-  "*"
  opt_chunk_buffer(las) <- 10
  options <- list(need_output_file = TRUE, 
                  need_buffer = TRUE, 
                  automerge = TRUE)
  output <- catalog_apply(
    las, lasfilternoise, sensitivity = sensitivity, .options = options)
  return(output)
}

# Point cloud transformation function for LAS catalog
lastransform.LAScluster <- function(las, CRSobj) {
  las <- readLAS(las)
  if (is.empty(las)) return(NULL)
  las <- lastransform(las, CRSobj)
  las <- lasfilter(las, buffer == 0)
  return(las)
}

lastransform.LAScatalog <- function(las, CRSobj) {
  opt_select(las) <-  "*"
  opt_chunk_buffer(las) <- 10
  options <- list(need_output_file = TRUE, 
                  need_buffer = TRUE, 
                  automerge = TRUE)
  output <- catalog_apply(las, lastransform, CRSobj = CRSobj, .options = options)
  return(output)
}

# Reusable DEM creation function
dem_create <- function(filtered_ctg, res, CRSobj, shape_out_dir, raster_out_dir) {
  # Create output folders if they doesn't already exist
  opt_output_files(filtered_ctg) <- ""
  shape_out_dir <- file.path(shape_out_dir, paste0(res, "m"))
  raster_out_dir <- file.path(raster_out_dir, paste0(res, "m"))
  
  if(!dir.exists(raster_out_dir)) dir.create(raster_out_dir, recursive = TRUE)
  if(!dir.exists(shape_out_dir)) dir.create(shape_out_dir, recursive = TRUE)
  
  # Produce a DEM and mask layer, since the DEM that is generated interpolates 
  # areas where there was no LiDAR. The mask layer is then made as a polygon 
  # which is used to mask the DEM in the end since the raster mask has holes 
  # in it from waterbodies etc.
  message(paste0("Producing ", res, "m DEM"))
  set_lidr_threads(availableCores() / 2)
  plan(multisession, workers = availableCores() / 2)
  dem <- grid_terrain(las = filtered_ctg, res = res, algorithm = tin(), 
                      keep_lowest = FALSE, full_raster = FALSE)
  plan("default")
  set_lidr_threads(0)
  message("Masking interpolated area where point cloud is absent")
  
  # Create a mask using all las points. It will be full of holes though, so 
  # It will need to be further processed
  mask <- grid_metrics(las = filtered_ctg, ~length(Z) * 0, res = res)
  
  # Create polygon of study area from the DEM with no holes in it
  aoi_poly <- st_as_stars(mask) %>% 
    st_as_sf(as_points = FALSE, merge = TRUE, na.rm = TRUE, use_integer = TRUE) %>% 
    st_geometry() %>% 
    lapply(., function(x) 
      x[which.max(lapply(x, function(y) st_area(st_polygon(list(y)))))]) %>% 
    st_multipolygon() %>%
    st_combine() %>%
    st_cast("POLYGON") %>%
    st_sf(crs = CRSobj@projargs) %>% 
    slice(which.max(st_area(.)))
  
  mask_aoi <- fasterize(aoi_poly, dem)
  
  # Mask and save outputs
  dem_masked <- mask(dem, mask_aoi)
  writeRaster(dem_masked, file.path(raster_out_dir, "dem.tif"), overwrite = TRUE)
  st_write(aoi_poly, file.path(shape_out_dir, "aoi.gpkg"), 
           delete_dsn = TRUE, delete_layer = TRUE) 
  return(dem_masked)
}
