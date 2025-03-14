#' Push inputlist to a fine resolution.
#'
#' Uses raster::disagregate to push a set of rasters to a target resolution.  Note that input raster resolution need to be divisible by the target resolution.
#'
#' @param inputFileList a character vector specifiying the location of all the input rasters.  Input list should only be a raster type (e.g. .tif).  Best practice is to use list.files(full.names = TRUE).
#' @param output destination of the output files.
#' @param targetRes desired resolution to convert to.
#' @keywords raster, disaggregate
#' @export
#' @examples
#' l <- list.files("e:/covariates/10")
#' cv_FineRes(l, output = "e:/covariates/2.5")
# 
# 
# cov_fineres <- function(inputFileList, output = "e:/tmp/2.5", targetRes = 2.5){
# 
#   ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
#          dir.create(file.path(output), recursive = TRUE), "Directory Already Exisits")        #create tmpOut
# 
#   for(i in inputFileList){
#     
#     ### testing parms
#     # i  <- inputFileList[1]
#     # targetRes <- 2.5
#     # output = "e:/tmp/2.5/"
#     print(paste("Processing:", i))
#     r  <- raster::raster(i)
#     px <- raster::res(r)[1]
#     r  <- raster::disaggregate(r, px/targetRes)  ## This will through an error if not an integer
#     raster::writeRaster(r, paste(output, basename(i), sep = "/"), overwrite = TRUE)
#   }
# }




#  Option 2: downsample based on a tempate raster. 
# this is helpful where the whole process was not followed and the rasters are not perfectly aligned. 
# this will downsample and reproject in line with a template raster 

# 1: downsample course scales for finer resolution and match extent

#library(getSpatialData)
library(raster)
library(sf)
library(sp)
library(mapview)
#library(velox) 
library(tidyverse)
#library(RStoolbox)
#library(mapedit)
library(rasterVis)
library(purrr)
#library(geosphere)
#library(RColorBrewer)

# input folder : 
# target raster :

cov_fineres_match <- function(downscale_folder, template_folder) {

    # testing lines 
    #downscale_folder <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/Buck_AOI/1_map_inputs/covariates/10m"
    #template_folder <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/Buck_AOI/1_map_inputs/covariates/5m"

  
    downscalefiles <- list.files(file.path(downscale_folder),pattern = ".tif", full.names = TRUE)
    rasterfiles <- list.files(file.path(template_folder),pattern = ".tif", full.names = TRUE)
    template <- raster::raster(rasterfiles[1])
    template_res <- res(template)
    template_ext <- extent(template)
    out_file <- file.path(downscale_folder, "downscale") 

    ifelse(!dir.exists(file.path(out_file )),              #if tmpOut Does not Exists
           dir.create(file.path(out_file ), recursive = TRUE), "Directory Already Exisits")        #create tmpOut


    for(i in 1:length(downscalefiles)){
      ### testing parms # 9 20-21
      i <- downscalefiles[i]
      
      print(paste("Processing:", i))
      r  <- raster::raster(i)
      dres <- res(r)[1]
      px <- raster::res(r)[1]
      r  <- raster::disaggregate(r, px/template_res)  ## This will through an error if not an integer
      
      if(extent(r)!= extent(template)){
        
        # if extents dont match then reproject
        
        r_proj <- raster::projectRaster(from = r, to= template, method="bilinear")
        
        xx <- raster::stack(template, r_proj)
        
        
      } else{
        
        r_proj <- r
      }
      
      
      raster::writeRaster(r_proj, file.path(out_file , gsub(".tif", paste0("_d",dres,".tif"), basename(i))), overwrite = TRUE)
      
      #xx <- raster::stack(template_raster, r)
      #xx <- raster::stack(list.files(out_file, full.names = TRUE))
      #xxx <- r_proj - template_raster
      #plot(xxx)
     }
}
    # check that stack is correct 
    
#    out_file <- "E:/temp/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/1_map_inputs/covariates/25m/downscale"
#    rasterfiles <- list.files(file.path(out_file), pattern = ".tif$", full.names = TRUE)
#    test_stack <- stack(rasterfiles) 
    