### Kiri Daust
### Find areas on map with similar bins to unsample-able points
### used to check covariates create at landscape scape per BGC 


library(sf)
library(data.table)
library(raster)
library(velox)
library(fasterize)
library(dplyr)
library(ggplot2)

        
# Gen version 

AOI <- "KIC_SE"

AOI_dir <- file.path(".", paste0(AOI, "_AOI"))
shape_dir <- file.path(AOI_dir, "0_raw_inputs", "base_layers")

bec <- st_read(file.path(shape_dir, "bec.gpkg"))
raster_folder <- file.path(paste0(aoi, "_AOI"), "1_map_inputs", "covariates",
                           "25m_trim")

# select resolution of raster 
# if bgc raster does not exist create one: 
rtemplate <- stars::read_stars(file.path(raster_folder, "dah.tif"), proxy = FALSE)
out <- stars::st_rasterize(bec["BGC_LABEL"], template = rtemplate)
st_crs(out) = st_crs(rtemplate)
stars::write_stars(out,file.path(raster_folder, "bgc.tif")) #tile name

fileoi <- c("dah_LS_0.3.tif", "dah_LS_0.2.tif", "mrvbf_LS.tif", "landform_LS.tif", "bgc.tif")

covariates <- list.files(raster_folder,full.names = T)

covariatesoi <- covariates[basename(covariates) %in% fileoi]

ancDat <- raster::stack(covariatesoi)

# find unique combinations 
combinations <- raster::unique(ancDat)
comb.df <- as.data.frame(combinations) 
comb.df <- na.omit(comb.df) # remove NA values 
comb.df$id = seq(1,length(comb.df$X25m_DAH_3Class),1 )


unique(comb.df$X25m_MRVBF_Classified_IS64Low6Up2)


ancDat.df <- as.data.frame(ancDat)
anc_class <- left_join(ancDat.df, comb.df)

# convert to raster
#anc_class <- raster(anc_class, ancDat,"id")
r <- raster(nrow=nrow(ancDat), ncol=ncol(ancDat), ext=extent(ancDat), 
            crs= NA )
values(r) <- anc_class$id

# write out raster
writeRaster(r, file.path("PEM_standards_manuscripts", "outputs_temp","deception_LS_classes.tif"), overwrite = TRUE)


#calculate the average

raster_sum <- anc_class %>%
  group_by(id) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum)) %>%
  rowwise() %>%
  mutate(pc = (sum/total )* 100)

ggplot(raster_sum, aes(x = id, y = pc)) +
  geom_bar(stat = "identity")




new_sample_space(badPoints,ancDat,"Testing_NewSpace_ICHmc1")
