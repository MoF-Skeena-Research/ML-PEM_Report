# required libraries
library(terra)
library(data.table)
 library(sf)
#### A bit about this script:
# a conversion to function of KDaust neighbourhood points addition
# data = raster_points_xy
# raster = testrast

### data is a SpatVector of transect points
##raster is a SpatRaster template of the AOI of the appropriate scale
add_neighbours <- function(data,raster) {

    dat_pts <- data
  dat_pts$ptsID <- 1:nrow(dat_pts)
  dat_atts <- as.data.table(st_drop_geometry(dat_pts))
  pts <- vect(dat_pts)
  testrast <- raster
  cellNums <- cells(testrast, pts)
  cell_lookup <- data.table(ID = pts$ptsID,cell = cellNums)
  
  adjCells <- adjacent(testrast,cells = cellNums[,2],directions = "queen",include = T)
  adjCells <- as.data.table(adjCells)
  setnames(adjCells, c("Orig",paste("Adj",1:8,sep = "")))
  adjCells[,ID := 1:nrow(adjCells)]
  adjLong <- melt(adjCells, id.vars = "ID", value.name = "CellNum", variable.name = "Position")
  setorder(adjLong,"ID","Position")
  values(testrast) <- 1:ncell(testrast)
  cellnums <- 1:ncell(testrast)
  testrast[!cellnums %in% adjLong$CellNum] <- NA
  pts <- as.points(testrast,values = T, na.rm = T)
  
 
  pts2 <- st_as_sf(pts)
  pts2 <- as.data.table(pts2)
  setnames(pts2,c("CellNum","geometry"))
  allPts <- merge(pts2, adjLong, by = "CellNum", all = T)
  #allPts[cell_lookup, ptID := i.ID, on = c(CellNum = "cell.cell")]
  allPts <- merge(allPts, dat_atts, by.x = "ID",by.y = "ptsID",all = T)
  allPts <- vect(st_as_sf(allPts))
  return(allPts)
}
