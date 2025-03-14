

pem_create_maps <- function(sample_points, out_name, centroid_distance = 400){
  
  #sample_points = ptCoords
  #out_name = file.path(prev_points, "ESSFwv_heli.gpkg")
  #centroid_distance = 400
  
  b = unique(ptCoords$subzone)
  #acost <- raster(file.path(out_path, "input_raster", "acost.tif"))
  #sample_cost <- raster(file.path(out_path,"input_raster", "sample_cost.tif"))
  
  g <- sample_points
  name<- "geometry"
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  sample_points <- g
# 
# # read in poly mask 
# mask_poly <- st_read(file.path(out_path, "input_raster",paste0(b, "_exclude_poly.gpkg"))) %>%
#   st_transform(3005)

  # create paired outputs
  sample_points_clhs <- st_as_sf(sample_points) %>% 
    st_transform(3005) #%>%
    # dplyr::select(-final_obj_continuous) %>%
    #mutate(aoi = NA)
  
rotation_angles <- seq(0, 315, 45) # Rotation degrees 

sample_points_rotations <- st_sf(st_sfc()) %>% st_set_crs(3005)

for(i in 1:nrow(sample_points_clhs)){
  #i = 1
  pnt <- sample_points_clhs[i,]
  pGeom <- st_geometry(pnt)
  pGeom <- pGeom + c(0, centroid_distance)
  pnt_feat <- st_set_geometry(pnt, pGeom)
  
  rotated_points <-  st_sf(st_sfc()) %>% st_set_crs(3005)
  
  rotated_points <- foreach(Bear = rotation_angles, .combine = rbind) %do%{
    #Bear = rotation_angles[5]
    Feature_geo <- st_geometry(pnt_feat)
    PivotPoint  <- st_geometry(pnt)
    ## Convert bearing from degrees to radians
    d <- ifelse(Bear > 180, pi * ((Bear -360)/ 180) ,  pi * (Bear / 180))
    rFeature <- (Feature_geo - PivotPoint) * rot(d)   + PivotPoint
    rFeature <- st_set_crs(rFeature, st_crs(pnt_feat))
    pnt_feat$geometry <- st_geometry(rFeature) ## replace the original geometry
    pnt_feat$Rotation <- Bear
    pnt_feat <- pnt_feat %>% st_set_crs(3005)
  }
  
  sample_points_rotations <- rbind(rotated_points, sample_points_rotations)  
}


sample_points_rotations <- st_as_sf(sample_points_rotations, crs = 3005) %>%
  #  mutate(rotation = mapvalues(Rotation, rotation_angles, c("N", "NE", "E", "SE", "S", "SW", #"W", "NW"))) %>%
  mutate(rotation = mapvalues(Rotation, rotation_angles, c("N", "NE", "SE", "W", "E", "NW", "SW", "S"))) %>%
  filter(!is.na(Rotation)) #%>% 
  #st_join(mask_poly, join = st_intersects) %>%
  #st_join(acost, join = st_intersect)
  #mutate(aoi = ifelse(is.na(cost), FALSE, TRUE))  %>%
  #dplyr::select(-cost) %>%
  #cbind(cost = raster::extract(sample_cost, .))

# sample_points_low_cost <- sample_points_rotations %>%
#   group_by(id) %>%
#   filter(aoi == TRUE) %>%
#   slice(which.min(cost)) %>%
#   ungroup() 
# 
# sample_points_low_cost <- sample_points_low_cost %>%
#   dplyr::select(-c(cost, Rotation))

sample_points_clhs$rotation <- "cLHS"

sample_points_rotations <- sample_points_rotations %>%
  dplyr::select(-Rotation) %>%
  filter(!is.na(rotation))

all_points <- rbind(sample_points_clhs, sample_points_rotations)  %>%
  mutate(id = paste(ID, rotation, sep = "_"))# This is all possible paired samples

all_triangles <- st_sf(st_sfc()) %>% st_set_crs(3005)
         
         for(i in 1:nrow(all_points)){
           #i = 1
           poc <- all_points[i, ]
           
           triangle <- Tri_build(id = poc$id, x =  st_coordinates(poc)[1], y =  st_coordinates(poc)[2])
           random_rotation <- runif(1, min = 0, max = 360)
           triangle <- rotFeature(triangle, poc, random_rotation)
            all_triangles <- rbind(all_triangles, triangle)
         }
         
         # check that all triangles fall within mask poly 
         
        # paired_triangles <- all_triangles[all_triangles$id %in% paired_sample$id,]
         
  #####write Transects####################
all_points <- all_points %>% mutate(transect_id = ID) %>% select(-c(ID))         

st_write(all_points, file.path(out_name),
                  layer = paste0(b,"_points_all"), delete_layer = TRUE)
#    
st_write(all_triangles, file.path(out_name), 
                  layer = paste0(b,"_transects_all"), delete_layer = TRUE)
         
         ####write buffer#########################
 triangle_buff <- st_buffer(all_triangles, dist = 10)
         
 st_write(triangle_buff,file.path(out_name), layer = paste0(b,"_transects_all_buffered"), delete_layer = TRUE)
 st_write(sample_points_clhs, file.path(out_name), layer = paste0(b,"_points_clhs"), delete_layer = TRUE)
         
}     