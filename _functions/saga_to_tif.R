

## writeout option
rasterfiles <- list.files(file.path(saga_files,"sagaTmp"),pattern = ".sdat")
rasterfiles

#for (iii in 9:length(rasterfiles)){
for (iii in 10:11){        
  iii = 22
  
  sagafile <- rasterfiles[iii] # 5
  print (sagafile)
  outfile <- gsub("sdat", "tif", sagafile)
  #w <- file.path(saga_files,"outputs", outfile)
  #r <- raster(sagafile)
  #writeRaster(r, w, overwrite = TRUE)
  #faster version using rgdal
  r <- readGDAL(file.path(saga_files,"sagaTmp",sagafile))
  w <- file.path(saga_files, "outputs", outfile) #, sep = "")
  writeGDAL(r, w)
}


