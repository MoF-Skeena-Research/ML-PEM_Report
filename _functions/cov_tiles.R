#' Tiles a set of rasters
#'
#' Uses \code{SpaDES.tools::splitRaster} to generate tiles of a given set of rasters using parallel processing.
#'
#' @param output character, destination of output tiles.  Note that a subdirectory for each raster given will be created within this folder.
#' @param rList a vector of files to process (e.g. \code{list.files("e:/rasters", pattern= "*.tif")})
#' @param pxCount the maximum number of pixels in the x and y direction.  \code{pxCount} squared will be the size of the output raster.  Default of 1000 x 1000 pixels.
#' @param index When \code{TRUE} genearates a shapefile \code{tiles.shp} -- for each set of tiles generated.
#' @keywords raster, tile, index
#' @export
#' @examples
#' r <- raster::raster(ncol=100, nrow=100) ## empty raster
#' r <- raster::setValues(r, rnorm(raster::ncell(r), 1)) ##
#' dir.create("e:/cv_tile")
#' raster::writeRaster(r, "e:/cv_tile/r.tif")
#' rasterList <- list.files("e:/cv_tile", full.names = TRUE) ## make sure this is the rasters only!
#'
#' cv_tile(rList = rasterList, output = "e:/cv_tile",
#'         pxCount = 50, index = TRUE)
#' #> [1] "Tiling: r.tif ... 1 of 1"
#' #> [1] "Generating Tile index: e:/cv_tile/r_tiles/tileindex.gpkg"
#'
#' list.files("e:/cv_tile", recursive = TRUE, full.names = TRUE)
#' #> [1] "e:/cv_tile/r.tif"
#' #> [2] "e:/cv_tile/r_tiles/r_tile1.grd"
#' #>[10] "e:/cv_tile/r_tiles/tileindex.gpkg"

cov_tile <- function(rList, output = "./tiles/", pxCount = 1000, index = TRUE){
  library(sp) ## needed as I could not create a direct call to as(e, "SpatialPolygon")
  library(rgdal) ## needed by raster calls to generate GTiff

  for(i in 1:length(rList)){
    print(paste("Tiling:", basename(rList[i]),"...", i, "of", length(rList)))
    r <- raster::raster(rList[i]) ## load raster to tile


    ## Tiles Out Path
    tilesOutpath <- paste(output, paste0(r@data@names,"_tiles"), sep = "/") ## another subdir by cv name
    if(!dir.exists(output)){
      dir.create(output, recursive = TRUE)  ## create subdir
    }



    ## Begin tiling (in parallel) -- uses SPAdes.tools
    n <- parallel::detectCores()-1
    raster::beginCluster(n)
    cv_tiles <- SpaDES.tools::splitRaster(r,
                                          nx = ceiling(as.numeric(dim(r)[1])/pxCount),  ## keeps the max rows below PxCount
                                          ny = ceiling(as.numeric(dim(r)[2])/pxCount),  ## keeps the max cols below PxCount
                                          path = tilesOutpath,
                                          # rType = "GTiff"
    )
    raster::endCluster()


    ## Create Tile Index
    if(index == TRUE){
      print(paste("Generating Tile index:", paste(tilesOutpath, "tileindex.gpkg", sep = "/")))
      lt <- list.files(tilesOutpath, pattern = "*.grd", full.names = TRUE)
      tiles <- sf::st_sf(sf::st_sfc())  ## create an empty sf object
      tiles$name <- as.character()
      for(i in 1:length(lt)){
        # i <- 2
        # print(i)
        r <- raster::raster(lt[i])
        e <- raster::extent(r)
        e <- as(e, "SpatialPolygons")
        e <- sf::st_as_sf(e)
        e$name <- basename(lt[i])
        tiles[i,] <- e
      }
      sf::st_write(tiles, paste(tilesOutpath, "tileindex.gpkg", sep = "/"), "index", quiet = TRUE)
    }

  }
}



