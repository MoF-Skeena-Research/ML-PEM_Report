
# testing land classification from 
#https://github.com/gianmarcoalberti/GmAMisc/blob/master/R/landfClass.R


landfClass <- function (x, scale = 3, sn=3, ln=7, n.classes="six", add.tpi=FALSE, stand.tpi = FALSE) {
  #define the shape of the moving window used by spatialEco::tpi
  
  # x <- DTM
  # scale = 75 
  # sn = 25
  # ln = 75
  # n.classes = "ten"
  # add.tpi = FALSE
  # stand.tpi = FALSE
  
  # @param x: input DTM (RasterLayer class).
  #' @param scale: size (in terms of cells per side) of the neighborhood (moving window) to be used; it must be an odd integer.
  #' @param sn: if the 10-class classification is selected, this paramenter sets the s(mall) n(eighborhood) to be used.
  #' @param ln: if the 10-class classification is selected, this paramenter sets the l(arge) n(eighborhood) to be used.
  #' @param n.classes: "six" or "ten" for a six- or ten-class landform classification.
  #' @param add.tpi: set to TRUE will return a TPI raster (FALSE is default).
  #' @param stand.tpi: specifies whether the returned TPI raster will be un- or standardized (FALSE is default).
  #' @keywords landform
  #' @export
  #' @examples
  #' data(elev) #load the 'elev' raster from the 'raster' package
  #' landfClass(elev, scale=5, add.tpi=TRUE, stand.tpi=TRUE) #perform the 6-class landform analysis (which is default), and also produce the standardized TPI; a moving window of dimension 5 (in terms of cells per side) is used
  #' landfClass(elev, sn=5, ln=11, n.classes="ten") #perform the 10-class landform analysis, with a s(mall) n(eighborhood) of size 5 and a l(arge) n(eighborhood) of size 11
  #'
 
  
  win = "rectangle"

  #calculate the slope from the input DTM, to be used for either the six or ten class slope position
  slp <- raster::terrain(x, opt="slope", unit="degrees", neighbors=8)
  
  win = "rectangle"
  
  if (n.classes == "six") {
    
    #calculate the tpi using spatialEco::tpi function
    tp <- spatialEco::tpi(x, scale=scale, win=win, normalize=TRUE)
    
    
    #define the six classes on the basis of thresholds of tp and slope
    valley <- (tp <= -1)
    valley[na.omit(valley)] <- 1
    
    lower.slp <- (tp > -1 & tp <= -0.5)
    lower.slp[na.omit(lower.slp)] <- 2
    
    flat.slp <- (tp > -0.5 & tp < 0.5) & (slp <= 5)
    flat.slp[na.omit(flat.slp)] <- 3
    
    middle.slp <- (tp > -0.5 & tp < 0.5) & (slp > 5)
    middle.slp[na.omit(middle.slp)] <- 4
    
    upper.slp <- (tp > 0.5 & tp <= 1)
    upper.slp[na.omit(upper.slp)] <- 5
    
    ridge <- (tp > 1)
    ridge[na.omit(ridge)] <- 6
   
    #plot(valley, main="Valley", sub="TPI <= -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(lower.slp, main="Lower Slope", sub="-1 < TPI <= -0.5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(flat.slp, main="Flat Slope", sub="-0.5 < TPI < 0.5 \nslope <= 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(middle.slp, main="Middle Slope", sub="-0.5 < TPI < 0.5 \nslope > 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(upper.slp, main="Upper Slope", sub="0.5 < TPI <= 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(ridge, main="Ridge", sub="TPI > 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    
    # consolidate into single layer
    tpi_class <- valley + lower.slp +flat.slp + middle.slp+upper.slp + ridge

  } else {
    
    #calculate two standardized tpi, one with small neighbour, one with large neighbour
    
    sn <- spatialEco::tpi(x, scale=sn, win=win, normalize=TRUE)
    ln <- spatialEco::tpi(x, scale=ln, win=win, normalize=TRUE)
    
    #define the ten classes on the basis of thresholds of sn, sl, and slope
    canyons <- (sn <= -1) & (ln <= -1)
    canyons[na.omit(canyons)] <- 1
    
    midslope.dr <- (sn <= -1) & (ln > -1 & ln < 1)
    midslope.dr[na.omit(midslope.dr)] <- 2
    
    upland.dr <-  (sn <= -1) & (ln >= 1)
    upland.dr[na.omit(upland.dr)] <- 3
    
    us.valley <-  (sn > -1 & sn < 1) & (ln <=-1)
    us.valley[na.omit(us.valley)] <- 4
    
    plains <- (sn > -1 & sn < 1) & (ln > -1 & ln < 1) & (slp <= 5)
    plains[na.omit(plains)] <- 5
    
    open.slp <-  (sn > -1 & sn < 1) & (ln > -1 & ln < 1) & (slp > 5)
    open.slp[na.omit(open.slp)] <- 6
    
    upper.slp <- (sn > -1 & sn < 1) & (ln >= 1)
    upper.slp[na.omit(upper.slp)] <- 7
    
    local.rdg <- (sn >= 1) & (ln <= -1)
    local.rdg[na.omit(local.rdg)] <- 8
    
    midslp.rdg <- (sn >= 1) & (ln > -1 & ln < 1)
    midslp.rdg[na.omit(midslp.rdg)] <- 9
    
    mount.top <- (sn >= 1) & (ln >=1)
    mount.top[na.omit(mount.top)] <- 10
    
    #plot(canyons, main="Canyons\nDeeply Incised Streams", sub="SN: TPI <= -1\nLN: TPI <= -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(midslope.dr, main="Midslope Drainage\nShallow Valleys", sub="SN: TPI <= -1\nLN: -1 < TPI < 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(upland.dr, main="Upland Drainages\nHeadwaters", sub="SN: TPI <= -1\nLN: TPI >= 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(us.valley, main="U-shaped Valleys", sub="SN: -1 < TPI < 1\nLN: TPI <= -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(plains, main="Plains", sub="SN: -1 < TPI < 1\nLN: -1 < TPI < 1\nslope <= 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(open.slp, main="Open Slopes", sub="SN: -1 < TPI < 1\nLN: -1 < TPI <  1\nslope > 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(upper.slp, main="Upper Slopes\nMesas", sub="SN: -1 < TPI < 1\nLN: TPI >=  1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(local.rdg, main="Local Ridges\nHills in Valleys", sub="SN: TPI >= 1\nLN: TPI <=  -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(midslp.rdg, main="Midslopes Ridges\nSmall Hills in Plains", sub="SN: TPI >= 1\nLN: -1 < TPI < -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
    #plot(mount.top, main="Mountain Tops\nHigh Ridges", sub="SN: TPI >= 1\nLN: TPI >= 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  
    # consolidate into single layer
    tpi_class <- canyons +  midslope.dr + upland.dr + us.valley + plains + open.slp + upper.slp + local.rdg + midslp.rdg + mount.top
    
    }
  # 
  # if (add.tpi == TRUE) {
  #   if (stand.tpi == FALSE) {
  #     tp <-  spatialEco::tpi(x, scale=scale, win=win, normalize=FALSE)
  #   } else {
  #     tp <- tp
  #   }
  #   plot(tp, main=paste0(ifelse(stand.tpi==TRUE, "Standardized", "Unstandardized"), " Topographic Position Index"), cex.main=0.8)
  # }
  # #tpi_class <- tp
  
  return(tpi_class)
}
