
# helper function to fix problem with outod data geo. Requires one file with correct 
# crs

r1 <- read_stars(rast_list[1])

epgs <- st_crs(r1)

for(i in 2:length(rast_list)) {
  #i <- 2
  print(i)
  rtemp <- read_stars(rast_list[i])
  
  if (st_crs(rtemp)$input == "unnamed"){
    
    print(paste(i, " needs fixing"))
    st_crs(rtemp) <- epgs
    stars::write_stars(rtemp,
                       rast_list[i]) #tile name
    
    # read_stars(rast_list[c(1,2)])
  } else {
    
    
    print("correctly assigned")
  }
}
