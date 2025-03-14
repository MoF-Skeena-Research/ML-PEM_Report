##########functions called by Stage1SampleDesign.RMD
###########layout of Stage 1 transects

library(foreach)
library(LearnGeom)

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

## Feature rotation
rotFeature <- function(Feature, PivotPt, Bearing) {
  # where Feature is the Shape to be rotated, eg:  #Feature <- tri
  # Bearing is the compass bearing to rotate to    #PivotPt <- pt.sf
  # PivotPt is the point to rotate around          #Bearing <- 15 

  ## extract the geometry
  Feature_geo <- st_geometry(Feature)
  PivotPoint  <- st_geometry(PivotPt)
  
  ## Convert bearing from degrees to radians
  d <- ifelse(Bearing > 180, pi * ((Bearing-360)/ 180) ,  pi * (Bearing / 180))
  
  rFeature <- (Feature_geo - PivotPoint) * rot(d)   + PivotPoint
  rFeature <- st_set_crs(rFeature, st_crs(Feature))
  
  Feature$geometry <- st_geometry(rFeature) ## replace the original geometry
  return(Feature)
}

pairedPoint <- function(POC, Dist, Rotations){ #Where bearing is the bearing recorded in the transect
  # This function is dependent on other PEM functions: rot, rotFeature
  PROJ <- st_crs(POC)
  pGeom <- st_geometry(POC)
  pGeom <- pGeom + c(0,Dist)
  feat <- st_set_geometry(POC, pGeom)
  pts <- foreach(Bear = Rotations, .combine = rbind) %do%{
    Bear = Rotations
    temp <- rotFeature(feat, POC, Bear)
    temp <- st_set_crs(temp, PROJ)
    temp$Rotation <- Bear
    temp
  }
  return(pts)
}


## Create Triangles -------------------------------------------------
## Build Triangle from a ID, x and y coordinate

Tri_build <- function(id, x, y){
  tris <- CreateRegularPolygon(3, c(as.numeric(paste(x)), 
                                    as.numeric(paste(y))), 145) # number of sides, center pt and length of sides
  
  MoonLineCentre <- data.frame(tris) %>%
    st_as_sf(., coords = c("X", "Y"), crs = 3005) %>%
    mutate(id = id) %>%
    group_by(id) %>%
    dplyr::summarise() %>%
    st_cast("POLYGON") %>%
    st_cast("MULTILINESTRING") #%>%
  #st_set_crs(newproj)
  return(MoonLineCentre)
} 



## Create a series of triangles -------------------------------------
## calls the rotation functions above
## Generate Potential lines /'MoonTransect' around a given POC
tri_set <- function(POC, Shape){  ## Where C is a point feature
  ## Where is Shape is the feature to be rotated
  
  allLines <- Shape ## this will be appended to later
  
  allLines$Rot <- 0  ## no rotation for the first transect
  
  ## Loop through a sequence of bearings
  for(d in seq(15,345, by = 15)){
    
    Shape$Rot <- d  ## Adds an attribute field to indicate the bearing it was rotated to
    tmp <- rotFeature(Shape, POC, d)  ## calls rotFeature function above
    
    allLines <- rbind(allLines, tmp)
  }
  return(allLines)
}

# Function to select the highest values of entropy for feature (triangle set)

max_entropy <- function(Raster, Feature) {
  # Raster <- entropy
  #  Feature <- triSet
  ## create column for the new metrics
  TTs <- Feature %>% mutate(Entropy_Sum = as.numeric(NA))
  
  tmpR <- crop(Raster, extent(TTs)) ## this allows for faster processing in the loop
  
  for(i in 1:nrow(TTs)){
    tmp <- TTs[i,] ## select the transect row
    values <- unlist(extract(tmpR, tmp))  ## extracts the values under the line
    TTs$Entropy_Sum[i] <- sum(values, na.rm = TRUE)
    
  }
  return(TTs)
}

tri_cost <- function(Raster, Feature) {
  # Raster <- entropy
  #  Feature <- triSet
  ## create column for the new metrics
  TTs <- Feature %>% mutate(cost = as.numeric(NA))
  
  tmpR <- crop(Raster, extent(TTs)) ## this allows for faster processing in the loop
  
  for(i in 1:nrow(TTs)){
    tmp <- TTs[i,] ## select the transect row
    values <- unlist(extract(tmpR, tmp))  ## extracts the values under the line
    TTs$cost[i] <- sum(values, na.rm = TRUE)
    
  }
  return(TTs)
}



numSamples <- function(ancDat) {
  tempD <- as.data.frame(ancDat)
  tempD <- tempD[!is.na(tempD[,2]),-length(tempD)]
  for(i in c(1,3)){ ##convert to factor
    tempD[,i] <- as.factor(tempD[,i])
  }
  nVars <- ncol(tempD)
  df <- tempD
  cl <- makeCluster(7)
  registerDoSNOW(cl)
  iterations <- 1
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ###loop through and calculate change in objective function
  cseq <- seq(5,100,5)
  sampSize <- foreach(ss = cseq,.combine = rbind, .packages = c("clhs")) %dopar% {
    res2 <- clhs(ancDat, size = ss, iter = 100, progress = F, simple = FALSE) # , cost = 'layer'
    data.frame(nSample = ss, deltaObj = min(res2$obj)/max(res2$obj))
  }
  
  stopCluster(cl)
  
  ###Now fit data to exponential function
  x <- sampSize$nSample
  y <- sampSize$deltaObj
  
  #Parametise Exponential decay function
  plot(x, y, xlab="sample number",ylab = "Prop Obj-fun Decrease")          # Initial plot of the data
  start <- list(k = 100,b1 = 0.05,b0 = 100)
  fit1 <- nls(y ~ -k*exp(-b1*x) + b0, start = start)
  lines(x, fitted(fit1), col="red")
  
  xx<- seq(1, 200,1)
  jj <- predict(fit1,list(x=xx))
  normalized = (jj-min(jj))/(max(jj)-min(jj))###standardise
  return(approx(x = normalized, y = xx, xout = 0.90)$y)###get 90th quantile
}



# function to run clhs with previous sites 


highcost_buffer <- function(sampling_area, sample_points){

    # testing lines 
    #  sampling_area <- lays_df 
    #  sample_points <-  clhs_sampled_buff
      
      # Check if points overlap buffer and then remove 
      lays_df_SF <-  sampling_area  %>%
        st_as_sf(., coords = c("x","y")) %>%
        st_set_crs(3005)
      
      to_cost <- st_intersection(lays_df_SF, sample_points) %>%
        dplyr::select(geometry) %>%
        mutate(cost_update = 100000) 
      
      # this part is slow and could be sped up..
      lays_df_buffered <- st_join(lays_df_SF, to_cost)  %>%
        mutate(cost = ifelse(!is.na(cost_update), cost_update, cost)) %>%
        select(-cost_update) 
      
      lays_df1 <- cbind(st_coordinates(lays_df_buffered), lays_df_buffered) %>%
        st_drop_geometry() %>%
        dplyr::rename(x = X, y = Y)
      
      
      lays_df1
      
      }


#lay1 <- highcost_buffer(lays_df, clhs_sampled_buff)

















