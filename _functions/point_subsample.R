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

# Series of functions to prepare data for use in random forest models this includes 
# adding covariate and subset training point data to generate multiple models

# include filtering for 
#       1) forest and non-forest points, 
#       2) per variant 
#       3) pure calls only  (no secondary calls recorded)
#       4) primary calls    (only primary calls, ignoring secondary calls)


# Parameters 
# points : data set with all points
# map1_col : name of column with primary call 
# map2_col : name of column with secondary call 
# map_key : name and location of map key 



# function 1 :
# points - data frame with XY values 
# x, y = column names that define the xy co-ordinates
# cov_dir = filepath to covariate folder 
# res_folder = pixal resolution at which to extract values (default is finest res (2.5m)



# 1: function to assign forest - non-forest group and split BGCs into BGC + non-forest grouping  

library(dplyr)

forest_nonforest_split <- function(points, map.key) {
  
 # points = tpts
 # map.key = mapkey_dir
  
  map.key = read.csv(map.key, stringsAsFactor = FALSE)
  
  fnfpoints <- points %>%
    mutate(match = mapunit1) %>%
    left_join(map.key, by = c(match = "MapUnit")) %>%
    mutate(fnf = case_when(
      For_NonFor == "Wat" ~ "Nfor",
      For_NonFor == "Anthro" ~ "Nfor",
      For_NonFor == "Nveg" ~ "Nfor", 
      TRUE ~ as.character(For_NonFor)), 
       bgc  = ifelse(
        str_detect(mapunit1, "_") == TRUE, 
        #grepl("(?i)_[a-z]", match, perl = TRUE),
        sub("[^[:alpha:]]+$", "", mapunit1),
        "non_forest")) %>%
    dplyr::select(names(points), fnf, bgc)
  
  fnfpoints 
}  

# 2: Primary or pure calls? 













# 3: add covariate points to data set 

# NB : this is now redundant function 

add_covars <- function(points, covar_dir) {
  
  # points = tpts
  #  covar_dir = file.path(cov_dir, paste0(map_res, "m"))
  
  cov_dat <-  stack(grep(".tif", list.files(file.path(covar_dir), 
                                            full.names = TRUE), value = TRUE))
  
  tptsxy <- points %>% 
    rename_all(.funs = tolower) %>%
    dplyr::select(x, y) %>%
    cbind(raster::extract(cov_dat, .))
  
  out_df <-  tptsxy %>%
    left_join(points)  
  
  out_df
  
} 

#tpts <- add_covars(tpts, file.path(cov_dir, paste0(map_res, "m")))
