
# setup folder structure 

# creates a serier of folder under the designated study area name to enable easier use of codeing

#' @param aoi is a text string which designates the name of the study area (e.g. "BoundaryTSA", "Deception") 
#' @keywords aoi, folder structure, admin
#' @export
#' @examples

library(plyr)
library(dplyr)

setupfolders <- function(AOI){

# testing line  AOI <- "KootInvCran"
  
  #base directory 
  AOI_dir <- file.path(".", paste0(AOI, "_AOI"))
  raw_dir <- file.path(AOI_dir, "0_raw_inputs")
  derived_dir <- file.path(AOI_dir, "1_map_inputs")
  
  
  # input and data processing directly 
  
  cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
  shape_dir <- file.path(AOI_dir, "0_raw_inputs", "base_layers")
  shape_raw_dir <- file.path(AOI_dir, "0_raw_inputs", "base_layers", "raw")
  dem_dir <- file.path(AOI_dir, "0_raw_inputs", "dem")
  lidar_dir <- file.path(AOI_dir, "0_raw_inputs", "dem", "lidar")
  trim_dir <- file.path(AOI_dir, "0_raw_inputs", "dem", "trim")
  
  # sample filepaths
  out_path <- file.path(AOI_dir, "2_sample_design", "stage1_StudyDesign")
  sampling_raw_folder <- file.path(out_path, "input_raster")
  clhs_outpath <- file.path(out_path, "clhs_sample_plans")
  
  training_data <- file.path(AOI_dir, "1_map_inputs", "trainingData")
  training_data_clean <- file.path(AOI_dir, "1_map_inputs", "trainingData", "clean")
  
  # model building folders
  model_dir <- file.path(AOI_dir, "3_maps_analysis")
  model_data <- file.path(AOI_dir, "3_maps_analysis", "models")
  model_f <- file.path(AOI_dir, "3_maps_analysis", "models", "forest")
  
  
  
  # set up folders if not already exist
  folder_set_up <- c(AOI_dir, raw_dir,shape_raw_dir, derived_dir, cov_dir, shape_dir, dem_dir,lidar_dir, trim_dir, out_path, sampling_raw_folder, clhs_outpath,training_data, training_data_clean, model_dir, model_data, model_f )
  
  for(fold in folder_set_up){
    
    ifelse(!dir.exists(fold), dir.create(fold, recursive = TRUE), FALSE)
    
  }
  
  return(print("all folders successfully created"))  

}
