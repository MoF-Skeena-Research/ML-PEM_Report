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


## Introduction to R Demonstration script. 

# written by genevieve perkins (genevieve.perkins@gov.bc.ca) 
# This script explored wetland data for the ESI project to 
# vizualise the spread of the data and utilise specific packages
# to access data from the BC data catalogue. 


# set up R session --------------------------------------------------------


# read in libraries 
library(bcdata)
library(bcmaps)
library(sf)
library(dplyr)
library(readxl)
library(mapview)
library(stringr)


# set up filepaths

AOI <- "Deception"

# set up file structure
AOI_dir <- file.path(".", paste0(AOI,"_AOI"))
cov.dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
trainpt_dir <- file.path(AOI_dir, "1_map_inputs", "trainingData","raw")


# define aoi 
aoi <- st_read("Deception_AOI/0_raw_inputs/base_layers/AOI.gpkg") 


# 3. Lets read in the wetland and river data 
waterbodies <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = 3005) %>% # lakes
  bcdata::filter(INTERSECTS(aoi)) %>% 
  collect() 

waterbodies <- waterbodies %>%
  select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA)) 

# lets get the centroid of each of the waterbodies and wetlands 
waterbodiesXY <- st_centroid(waterbodies) 

wbpt <- st_coordinates(waterbodiesXY) 
wbpt <- waterbodiesXY %>%
  cbind(wbpt)%>%
  st_drop_geometry() %>%
  mutate(mapunit1 = "LA") %>%
  select(c("X", "Y", "mapunit1"))


# read in the water layer and check against the sentinel layer : see reference Kaplan and Avdan 2017
#https://www.tandfonline.com/doi/pdf/10.1080/22797254.2017.1297540


# mask to nonforest areas
water10 <- raster(file.path(cov.dir, "10m", "sen_mndwi.tif"))
water5 <- raster(file.path(cov.dir, "5m", "sen_mndwi.tif"))

wb10 <- as.data.frame(raster::extract(water10, wbpt[1:2]))
wb5 <- as.data.frame(raster::extract(water5, wbpt[1:2]))

names(wb10) = "wb10"
names(wb5) = "wb5"

wb <- cbind(wbpt, wb10, wb5)

hist(wb$wb10)
hist(wb$wb5)

# remove deep waterbodies # using threshold
#water[water > 0.5] <- NA

write.csv(wb, file = "Deception_AOI/1_map_inputs/trainingData/raw/lake2_train_pts.csv")
write.csv(wbpt, file = "Deception_AOI/1_map_inputs/trainingData/raw/lake_train_pts.csv")

# 
# 
# #rivers <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e", crs = 3005) %>% # rivers
# #  bcdata::filter(INTERSECTS(aoi)) %>% 
# #  collect() 
# 
# #wetlands <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6", crs = 3005) %>% # wetlands
# #  bcdata::filter(INTERSECTS(aoi)) %>% 
# #  collect() 
# 
# #mw = mapview(waterbodies)  
# #ma = mapview(aoi)
# 
# # mapview(rivers) + 
# #mapview(wetlands)
# 
# # lets select only the smallest lakes 
# waterbodies <- waterbodies %>%
#   select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA)) 
# 
# st_write(waterbodies, "Deception_AOI/0_raw_inputs/base_layers/waterbodies.gpkg")
# 
# wetlands <- wetlands %>% # lakes
#   select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA))
# 
# 
# # lets get the centroid of each of the waterbodies and wetlands 
# waterbodiesXY <- st_centroid(waterbodies) 
# wbpt <- st_coordinates(waterbodiesXY) 
# wbpt <- waterbodiesXY %>%
#   cbind(wbpt)%>%
#   st_drop_geometry() %>%
#   mutate(mapunit1 = "LA")
# 
# wetlandsXY <- st_centroid(wetlands)
# wetpt <- st_coordinates(wetlandsXY) 
# wetpt <- wetlandsXY %>%
#   cbind(wetpt) %>%
#   st_drop_geometry()
# 
# waterpt <- bind_rows(wbpt, wetpt ) 
# waterpt <- st_as_sf(waterpt, coords= c("X","Y"), crs = 3005)
# 
# mapview(waterpt)
# 
# 
# 
# # Part 2: Stratified random sampling of wetlands  ---------------------------------------------
# 
# # lets now create a random sample of wetlands 
# # the criteria we want to work with 
# #       - wetlands must be within X distance to a road
# #       - create random points within the bec zones ( ) 
# #       - dont want to sample where we already have data
# 
# 
# # lets create a buffer around the roads and then determine which points fall within the 
# # buffer 
# 
# 
# 
# 
# # we can now eliminate any points that have already been sampled - lets buffer out sites by 500m 
# # to ensure no overlap with new points 
# 
# plot_exclude <- st_buffer(plot, dist = 500)
# 
# overlap_pts <- st_intersection(waterpt, plot_exclude)
# 
# waterpt <- waterpt %>%
#   filter(!WATERBODY_POLY_ID %in% overlap_pts$WATERBODY_POLY_ID)
# 
# 
# # We can now randomly select using the bec types to stratify first let us intersect the 
# # point to add the bec zone name and id
# 
# bec_pts <- st_intersection(waterpt, bec_sf)
# 
# # make a list of unique bec variants 
# bgc.ls <- as.list(unique(bec_pts$MAP_LABEL))
# 
# 
# 
# 
# # write out as csv   
# 
# write.csv(out, file = "demo/wetland_sample.csv")
# 
# 
# # Write out a KML file ----------------------------------------------------
# 
# # Now we can write out a KML file so we can 
# # view this is google earth. First we need to convert to a sp object. 
# 
# out_sp <- as(out, "Spatial") 
# 
# # write out a kml 
# kml(out_sp,
#     file.name    = file.path("demo/wetland_points.kml"),
#     points_names = out_sp$MAP_LABEL,
#     colour    = "#FF0000",
#     alpha     = 0.6,
#     size      = 1,
#     shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")
# 
# 
