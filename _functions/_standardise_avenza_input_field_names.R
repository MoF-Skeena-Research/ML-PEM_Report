# function to comvert geopackage to shapefile and rename column 
# gen perkins 
# Oct 10 2021
library(sf)
library(dplyr)

split_gpkgs <- function(infile, outfolder ){
  
  #infolder <- "D:/PEM_DATA/BEC_DevExchange_Work/KIC_SE_AOI/2_sample_design/stage1_StudyDesign/Koot_contractor_data_QA/Deliverables"
 #infilename <- "PEM_14July2022.gpkg"
  
  outfile <- outfolder 
  
  layers <- st_layers(infile)
  
  
  for(i in layers$name) {
    
    #i = layers$name[1]
    print(i)
    
    fileoi <- st_read(infile, layer = i )
    
    if(st_geometry_type(fileoi) %in% c("MULTILINESTRING")) {
          
          print ("line")
      
          st_write(fileoi, file.path( outfile, paste0(i, " (1).gpkg")), delete_layer = TRUE)
                 
          } else {
            
      if(st_geometry_type(fileoi) %in% c("POINT")) {
        
        print("point")
        
        points_shp <- fileoi %>%
          st_transform(3005) %>%
          st_zm() %>%
          dplyr::rename("name" = Name, 
                 "desc" = descriptio,
                 "timestamp" = timestamp,
                 "Photos"= pdfmaps_ph,
                 "01_trans" = F01_transec, 
                 "02_observe" = F02_observe,
                 "03_pt_type" = F03_pt_type,
                 "04_mapunit" = F04_mapunit,
                 "05_transit" = F05_transit,
                 "06_mapunit" = F06_mapunit,
                 "07_struc_" = F07_struct_,
                 "08_struct" = F08_struct_,
                 "09_comment" = F09_comment,
                 "10_edatope" = F10_edatope#, 
                 #"geometry" = Shape)
          ) 
      
        st_write(points_shp, file.path( outfile, paste0(i, ".gpkg")), delete_layer = TRUE)
        
              }
      }       
            
  }
  
}
