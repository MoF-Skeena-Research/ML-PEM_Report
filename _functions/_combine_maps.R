combine_maps <- lapply(folders, function(f){
  
  #f <- folders[[1]]
  
  rtemp <- raster(file.path(map_dir, f, "mosaic.tif"))
  
  rtemp[is.na(rtemp[])] <- 0 
  
  # filter to only predict over bgc
  bec_filter <- bec_shp %>%
    filter(MAP_LABEL == f) %>%
    dplyr::select(MAP_LABEL) 
  
  rtemp <- raster::mask(rtemp, as(bec_filter,'Spatial'))
  
  subkey <- rkey %>% dplyr::filter(model == f) %>%
    mutate(mosaic = as.numeric(rownames(.)))
  
  # check if the key matches or needs reclassification 
  if (isTRUE(unique(subkey$mosaic == subkey$map.response))) {
    
    print("matching key")
    
  } else {
    
    print("updating key")
    
    m <- subkey %>%
      mutate(to = as.numeric(X), 
             from = as.numeric(X)+1) %>%
      dplyr::select(to, from, map.response) 
    
    reclm <-  as.matrix(m, ncol=3, byrow= TRUE)
    rtemp <-  reclassify(rtemp, reclm, right = FALSE)#, include.lowest=TRUE)
    
  }
  
  rtemp <- reclassify(rtemp, cbind(-Inf, 0, NA), include.lowest=TRUE)
  rtemp
  
})