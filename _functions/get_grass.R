# Search system for an existing GRASS GIS version (grass7x)
# Should find the latest version, so search backwards from version 7999 (not sure
# how grass versions are all named but I assume it's by an increasing number)
# Only good on a Windows machine at the moment

get_grass <- function() {
  for(i in 999:1) {
    if(Sys.which(paste0("grass8", i)) != "") {
      grass_version <- paste0("grass8", i)
      break
    }
  }
  
  # If system does not contain a valid version of grass, download and install one
  if(!exists("grass_version")) {
    osgeo_download <- curl::curl_download(
      "https://download.osgeo.org/osgeo4w/v2/osgeo4w-setup.exe", 
      destfile = file.path(tempdir(), "osgeo4w-setup.exe")
    )
    term_id <- rstudioapi::terminalCreate()
    rstudioapi::terminalSend(term_id, paste(
      osgeo_download, "-q -k -P grass -s http://download.osgeo.org/osgeo4w/x86_64/\r"))
    message("Installing GRASS, please wait")
    Sys.sleep(3)
    while(rstudioapi::terminalBusy(term_id)) Sys.sleep(0.5)
    rstudioapi::terminalKill(term_id)
    
    for(i in 999:1) {
      if(Sys.which(paste0("grass8", i)) != "") {
        grass_version <- paste0("grass8", i)
        break
      }
    }
  }
  return(normalizePath(Sys.which(grass_version)))
}
