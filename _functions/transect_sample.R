#' Transect Sample
#'
#' This is a wrapper for st_line_sample.  The PEM transect lines are used to generate a minimum of one point per line segment. Additional sample points are added at a minimum distance of 5 metres.
#'
#' _Note: current version does not include the raster method._
#'
#' @param lines is a sf _LINES_ object.
#' @param mdist Optional with a default of 5m
#' @keywords points, transects, samples, sampling
#' export

transect_sample <- function(lines, mdist = 5) {
  # Original script:
  
  # for (i in 1:nrow(lines)) {
  #   # i <- 1
  #   L <- lines[i,]
  #   print(paste(L$TID, L$id, round(st_length(L),1)))
  #   
  #   sample <- st_line_sample(L,
  #                            density = 1/mdist) %>%  ## 1 sample every 5m
  #     st_cast(., "POINT") %>% ## convert from point to multipoint.
  #     st_sfc(.) %>% st_sf(.)  ## generate geom column and convert to sf object
  #   
  #   sample <- st_join(sample, st_buffer(L, 5))
  #   
  #   ## if first time create samples else append
  #   if (i == 1) {samples <- sample} else {samples <- rbind(samples, sample)}
  # }
  
  
  ##############################################################################
  
  # Parallelized version of the original function below:
  # cl <- parallel::makeCluster(parallel::detectCores() - 1)
  # doParallel::registerDoParallel(cl)
  # samples <- foreach(i = 1:nrow(lines), .combine = rbind, .packages = "sf") %dopar% {
  #   
  #   sample <- st_line_sample(lines[i, ], density = 1/mdist) %>%  ## 1 sample every 5m
  #     st_cast(., "POINT") %>% ## convert from point to multipoint.
  #     st_sfc(.) %>% st_sf(.) %>%   ## generate geom column and convert to sf object
  #     st_join(st_buffer(lines[i, ], 5))
  # }
  # parallel::stopCluster(cl)
  
  
  ###############################################################################

  # Fastest implementation of line sampling, seems to do the same thing as the original:
  # Note: Submitted issue to sf package regarding how short segments are sampled: 
  # https://github.com/r-spatial/sf/issues/1365
  
  long_lines <- lines[st_length(lines) > units::set_units(mdist / 2, m), ]
  short_lines <- lines[st_length(lines) <= units::set_units(mdist / 2, m), ]
  
  # Use density approach for lines longer than half the mdist
  samples_long <- st_line_sample(long_lines, density =  1 / mdist) %>%
    st_cast(., "POINT") %>% ## convert from point to multipoint.
    st_sfc(.) %>% st_sf(.) %>%
    st_join(long_lines, join = st_nearest_feature)
  
  # Sample short lines in the middle of the line
  samples_short <- st_line_sample(short_lines, sample = 0.5) %>% 
    st_cast(., "POINT") %>% ## convert from point to multipoint.
    st_sfc(.) %>% st_sf(.) %>%
    st_join(short_lines, join = st_nearest_feature)
  
  # Merge together
  samples <- rbind(samples_long, samples_short)

  return(samples)
}



# helper function which converts transect id values into transect id + transect number

format_transect <- function(indata){
  
  # testing lines 
  #indata <- raster_points_xy
  
  indata <- indata %>%
    mutate(tid = tolower(gsub("_[[:alpha:]].*","", transect_id))) %>%
    mutate(slice = sub('.*(?=.$)', '',gsub("\\..*","", tid), perl=T))
  
  indata <- indata %>%
    dplyr::select("mapunit1", "mapunit2", "transect_id", "tid", "slice", "data_type", "transition", "observer", "comments", "geometry")
  
  #sort(unique(raw_dat$tid))
  #sort(unique(raw_dat$slice))
  
  print(paste0("the data contains a maximum of ", length(unique(indata$slice))," slices, ", length(unique(indata$transect_id)), " transects, and ",
  length(unique(indata$tid)), " sites"))
  return(indata)
  
}


# helper function for populating the observer column 

# fill the observer value from missing on raw data - matches based on unit transect id
fill_observer <- function(input_data){
  
#input_data <- points_final 
  
observer_key <- input_data %>%
  dplyr::select(transect_id, observer) %>%
  rename("observer_fill" = observer) %>%
  st_drop_geometry() %>%
  distinct()  %>%
  na.omit() %>%
  mutate(observer_fill = trimws(observer_fill, which = "both")) %>%
  filter(observer_fill != "")
  

input_data <- input_data %>%
  left_join(observer_key, by = c("transect_id" = "transect_id")) %>%
  mutate(observer = trimws(observer, which = "both")) %>%
  mutate(observer = ifelse(is.na(observer),observer_fill, observer)) %>%
  mutate(observer = ifelse(observer == "" , observer_fill, observer)) %>%
  #mutate(observer = observer_fill) %>%
  dplyr::select(-observer_fill)

return(input_data)

}


# format mapunits to fix unmatched field calls 
# requires input data and map key. 

format_mapunit_names <- function(indata, map.key) {

#  indata = s1_pts
  
# cross check values with AOI key 
map.key <- map.key %>%
  dplyr::select(c(FieldCall, BaseMapUnit)) 

# outdata1 <- indata %>%
#   left_join(map.key, by = c("mapunit1" = "FieldCall")) %>%
#   group_by(time)%>%
#   summarise(count = n())

outdata <- indata %>%
  left_join(map.key, by = c("mapunit1" = "FieldCall")) %>%
  dplyr::select(-mapunit1) %>%
  dplyr::rename(mapunit1 = BaseMapUnit) %>%
  left_join(map.key, by = c("mapunit2" = "FieldCall")) %>%
  dplyr::select(-mapunit2) %>%
  dplyr::rename(mapunit2 = BaseMapUnit) 

# if replace mapunit1 with mapunit2 if mapunit1 is missing 
outdata <- outdata %>%
  mutate(mapunit1 = ifelse(!is.na(mapunit2) & is.na(mapunit1), mapunit2, mapunit1)) %>%
  mutate(mapunit2 = ifelse((mapunit1 == mapunit2), NA, mapunit2))

return(outdata)
  }
