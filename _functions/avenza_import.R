# Copyright 2020 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# import Avenza data from Deception (both S1 and S2 combined

ls <- c("tidyverse", "raster", "fasterize", "sf", "clhs", "tools", "lwgeom",
        "scales", "foreach", "velox", "ranger")

new_pkg <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new_pkg)) install.packages(new_pkg)

# Check for at least sf version 0.9-0 (required by the make_lines function)
if(package_version("0.9-0") > packageVersion("sf")) install.packages("sf")

lapply(ls, library, character.only = TRUE)
rm(ls, new_pkg)

#remotes::install_github("hunzikp/velox")


#library(corrplot)
library(caret)
library(sf)
library(ranger)
library(tidyverse)
library(fasterize)
library(stringr)
library(dplyr)
library(raster)
library(terra)
library(readxl)
library(stars)
library(stringr)
library(foreach)


AOI <- "Deception"
s2type <- "ensemble"

if (s2type == "ensemble")
{type = "m2"} else { 
    type = "m1"}


res <- 2.5

AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

trans_layout <- file.path(AOI_dir, "2_sample_design", "stage2_StudyDesign", paste0("01_",s2type), "transect_layout")
trans_data <- file.path(AOI_dir, "2_sample_design", "stage2_StudyDesign", paste0("01_",s2type),"transect_data","compiled") 
output_pnts_dir <- file.path(AOI_dir, "2_sample_design", "stage2_StudyDesign", "training_pts")
output_cleaned_dir <- file.path(AOI_dir, "1_map_inputs", "trainingData", "clean")
map.key  <- read.csv(file.path(AOI_dir, "_MapUnitLegend", 
                               paste0(AOI, "_MapUnitLegend.csv")), 
                     stringsAsFactor = FALSE)

res_folder <- paste0(res, "m")

if(!dir.exists(output_pnts_dir)) dir.create(output_pnts_dir, recursive = TRUE)


source(here::here('_functions', 'extend_lines.R'))
source(here::here('_functions', 'make_lines.R'))
source(here::here('_functions', 'transect_sample.R'))
source(here::here('_functions', 'multiline_to_line.R'))


## inport base raster of resolution required 
raster_template <- raster(list.files(file.path(cov_dir, res_folder), pattern = ".tif", full.names = TRUE)[1])


trans <- list.files(trans_layout, pattern = ".gpkg$|.shp$", full.names = TRUE, recursive = TRUE)  
#trans <- list.files(trans_layout, pattern = ".gpkg$", full.names = TRUE, recursive = TRUE)

#trans <- trans[str_detect(trans,"all_tri.shp")]
trans <- trans[str_detect(trans,"all_tri.gpkg")]

transect_layout <- foreach(x = trans, .combine = rbind) %do% {
  clhs_layers <- st_layers(x)
  lines <- which(clhs_layers[["geomtype"]] %in% c("Line String", "Multi Line String"))
  if(length(lines)) {
    transects <- foreach(y = clhs_layers$name[lines], .combine = rbind) %do% {
      transect <- st_read(x, y, quiet = TRUE) %>% 
        rename_all(recode, geom = "geometry") %>% 
        rename_all(.funs = tolower) %>%
        dplyr::select(id) %>% 
        mutate(id = as.character(id)) %>% 
        st_transform(3005)
    }
  } 
} #%>% dplyr::rename(ID = id)

transect_layout_buf <- st_buffer(transect_layout, 10)

#st_write(transect_layout, file.path(output_pnts_dir, paste0("trans_layout_", AOI,".gpkg")), 
#         delete_layer = TRUE)


points <- list.files(trans_data, full.names = TRUE)
points  <- points[grep(".gpkg$", points)]

# read in raw data files: 

all_points <- foreach(x = points[1:4], .combine = rbind) %do% {
   #x <- points[1]
  points_read <- st_read(x, quiet = TRUE) %>%
    st_transform(3005) %>% 
    st_zm() %>% 
    mutate(mapunit1 = X2mapunit, 
           mapunit2 = X4mapunit2,
           point_type = desc, 
           #mapunit2 = !!sym(mapunit2_column),
           #point_type = !!sym(poc_column), 
           file = x) %>%
    st_join(., transect_layout_buf, join = st_intersects) %>%
    rename_all(.funs = tolower) %>%
    dplyr::select(name, mapunit1, mapunit2, name, id, file)
} %>% 
  distinct(., .keep_all = TRUE) 


all_points2 <- foreach(x = points[5:6], .combine = rbind) %do% {
  #x <- points[5]
  points_read <- st_read(x, quiet = TRUE) %>%
    st_transform(3005) %>% 
    st_zm() %>% 
    mutate(#mapunit1 = X2MapUnit, 
           #mapunit2 = X4MapUnit2,
           mapunit1 = X2mapunit, 
           mapunit2 = X4mapunit2,
           point_type = desc, 
           #mapunit2 = !!sym(mapunit2_column),
           #point_type = !!sym(poc_column), 
           file = x) %>%
    st_join(., transect_layout_buf, join = st_intersects) %>%
    rename_all(.funs = tolower) %>%
    dplyr::select(name, mapunit1, mapunit2, name, id, file)
} %>% 
  distinct(., .keep_all = TRUE) 



# add entropy 
all_points <- rbind(all_points, all_points2)

#unique(all_points$id)


# export non_standard points: 
all_points_extras <- all_points %>%
  filter(is.na(id))

if(file.exists(file.path(output_pnts_dir, "sbsmc2_extra_points_S2.gpkg"))) {
  existing_pts <- st_read(file.path(output_pnts_dir, "sbsmc2_extra_points_S2.gpkg"))
  all_points_extras <- rbind(existing_pts, all_points_extras)
  all_points_extras <- distinct(all_points_extras)
  } 
  
st_write(all_points_extras, file.path(output_pnts_dir, "sbsmc2_extra_points_S2.gpkg"), delete_layer = TRUE)

# export standard transect
all_points <- all_points %>% filter(!is.na(id))

st_write(all_points, file.path(output_pnts_dir, paste0("stage2_transect_",type,"_pts_raw.gpkg")), delete_layer = TRUE)






## FOR KARA - start here: 
#all_points <- st_read("filepath to the location of file") %>%
#  st_transform(3005) %>% 
#  st_zm()

# read in the layout file: 
                   


# consolidate tracklog 

# covert to lines 
processed_transects <- make_lines(GPSPoints = all_points, 
                                  #GPSTracks = all_tracks,
                                  Transects = transect_layout, 
                                  method = "pts2lines", #"tracklog",  
                                  tBuffer = 20, PROJ = 3005) %>% 
  mutate(mapunit12 = paste0(mapunit1,"_", mapunit2)) %>%
  mutate(mapunit12 = gsub("_NA","", mapunit12)) %>%
  dplyr::select(-TID, -ID)


st_write(processed_transects,  file.path(output_pnts_dir, paste0("proc_s2_",type, "_transects.shp")), 
         delete_layer = TRUE)


# raster version 

raster_key <- function(lines, mapunit12) { 
  map_key <- lines %>% 
    dplyr::select(mapunit12) %>%
    st_drop_geometry() %>%
    distinct() %>%
    mutate(mapunit_no = seq(1:nrow(.)))
} 

map_key <- raster_key(processed_transects, "mapunit12")

lBuff <- processed_transects %>% 
  dplyr::group_by(mapunit12) %>%
  dplyr::summarise() %>%
  sf::st_buffer(., dist = 2.5, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
  sf::st_cast(.,"MULTIPOLYGON") %>%
  dplyr::mutate(mapunit12 = as.factor(mapunit12)) %>%
  dplyr::left_join(., map_key)

# note need to have a 2.5 m template raster or generating rasters is incomplete 

rastAll <- fasterize(lBuff, raster_template, field = "mapunit_no")

raster_points_xy <- as.data.frame(rasterToPoints(rastAll)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 3005) %>%
  merge(map_key, by.x = names(rastAll), by.y = "mapunit_no") %>% 
  st_join(st_buffer(transect_layout_buf, 10)) %>% 
  cbind(st_coordinates(.)) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename_all(.funs = tolower) %>%
  separate(mapunit12, "_(?=[^_]*$)",
           into = c("mapunit1", "mapunit2"), 
           remove = TRUE) %>% 
  dplyr::select(mapunit1, mapunit2, x, y, id)

# tidy names 
raster_points_xy <- raster_points_xy %>%
  st_transform(3005) %>%
  rename_all(.funs = tolower) %>%
  dplyr::select_if(names(.) %in% c("x", "y", "mapunit1", "mapunit2", "id")) %>%
  #distinct() %>%
  mutate(mapunit2 = gsub("_", "/", mapunit2), 
         mapunit1 = gsub("_", "/", mapunit1)) %>%
  left_join(map.key, by = c("mapunit1" = "FieldCall")) %>%
  dplyr::select(x, y, MapUnit, mapunit2, id) %>%                         
  dplyr::rename(mapunit1 = MapUnit) %>%
  left_join(map.key, by = c("mapunit2" = "FieldCall")) %>%
  dplyr::select(x, y, mapunit1, MapUnit, id) %>% 
  dplyr::rename(mapunit2 = MapUnit) %>%
  mutate(mapunit2 = ifelse(mapunit1 == mapunit2, NA, mapunit2)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 3005) %>%
  drop_na(mapunit1)


writeRaster(rastAll, file.path(output_pnts_dir , paste0("proc_s2_trans_",type,"_raster.tif")), overwrite = TRUE)

st_write(raster_points_xy, file.path(output_pnts_dir, paste0("stage2_transect_",type,"_pts.shp")), delete_layer = TRUE)

st_write(raster_points_xy, file.path(output_cleaned_dir, paste0("stage2_transect_",type,"_pts.shp")), delete_layer = TRUE)



# sample from set distacnt 

for(i in c(5, 30)) {
   i = 5
  SamplePtsXY <- transect_sample(processed_transects, mdist = i) %>% 
    dplyr::select(-one_of("X", "Y", "name")) %>% 
    cbind(st_coordinates(.)) %>% 
    distinct(.keep_all = TRUE) %>% 
    rename_all(.funs = tolower) %>% 
    dplyr::select(mapunit1, mapunit2, x, y, id)
  
  SamplePtsXY  <- SamplePtsXY %>%
    st_transform(3005) %>%
    rename_all(.funs = tolower) %>%
    dplyr::select_if(names(.) %in% c("x", "y", "mapunit1", "mapunit2", "id")) %>%
    st_drop_geometry() %>%
    mutate(mapunit2 = gsub("_", "/", mapunit2), 
           mapunit1 = gsub("_", "/", mapunit1)) %>%
    left_join(map.key, by = c("mapunit1" = "FieldCall")) %>%
    dplyr::select(x, y, MapUnit, mapunit2, id) %>%                         
    dplyr::rename(mapunit1 = MapUnit) %>%
    left_join(map.key, by = c("mapunit2" = "FieldCall")) %>%
    dplyr::select(x, y, mapunit1, MapUnit, id) %>% 
    dplyr::rename(mapunit2 = MapUnit) %>%
    mutate(mapunit2 = ifelse(mapunit1 == mapunit2, NA, mapunit2)) %>% 
    st_as_sf(coords = c("x", "y"), crs = 3005)
  
  st_write(SamplePtsXY, file.path(output_cleaned_dir, paste0("stage2_transect_", type,"_",i, "m_pts.shp")), delete_layer = TRUE)
}

