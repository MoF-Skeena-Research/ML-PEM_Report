#' Generate a quick machine learning model for preliminary assesment
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#'
#'
#' @param outDir  Highly recommended to be set as an absolute directory.  This defaults to the project's root directory OR where the RMD script is saved.
#' Additional products generated from the associated `model_gen.Rmd`` markdown script will also be saved to this dir.
#' @param traindat Is a dataframe that contains the model training data.  The reponse variable should be one of the columns.
#' @param target   The name of the response variable in the traindat data frame.
#' @param rseed    Optional random number seed.
#' @keywords machine-learning, model, report
#' @export
#' @examples
#' dat <- read.csv("e:/workspace/2020/PEM/ALRF_PEMv2/dev/modDat.csv",
#'                stringsAsFactors = TRUE)
#'
#'
#' model_gen(traindat = dat,
#'           target = "SiteSeries",
#'           outDir = "e:/tmp/model_gen_test",
#'           rseed = 456)


model_gen_ranger <- function(traindat, target, outDir = ".", mname = "model", rseed = NA) {
  
  library(sf)
  library(ranger)
  
  #############simple ranger model
   ##testing : GP
 #  traindat = fnf
#   target = "fnf"  
#   outDir = paste0("D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/2_sample_design/stage1_Analysis/models/", target)
#   mname = "fnf_1"
#   rseed = 456
 
    ## error testing ----------------
  if (sum(is.na(traindat[,target])) > 0) {
    # print(paste("There are,", sum(is.na(traindat[,target]))  , "NA values in the target:", target))
    stop(paste("There are,", sum(is.na(traindat[,target]))  , "NA values in the target:", target))
  }
  
  ## passing variables to it --------------
  
  BGCmodel <- ranger(target ~ ., data = traindat , #do.trace = 10,
                  num.trees = 501,   importance = "impurity", 
                  splitrule = "extratrees", seed = 12345, 
                  write.forest = TRUE, classification = TRUE)#strata=BGC, sampsize= c(500),
  

}




# Function 2: generate output from ranger quick model ---------------------

model_quick_ranger_outputs <- function(ranger_model, modelout_dir, model_name) {
  
#ranger_model = model_fnf
#model_name = "fnf"
#modelout_dir = out_dir
  
  modelout_dir = file.path(modelout_dir, model_name)

    
    ## create destination folder
  ifelse(!dir.exists(file.path(modelout_dir)),                # if folder does not exist
           dir.create(file.path(modelout_dir)), print("folder already exists"))          # create it
  
  save(ranger_model, file = file.path(modelout_dir, paste0(model_name , "_ranger.rds")))
    

  v <- as.data.frame(ranger_model$variable.importance)
  DF <- v %>% tibble::rownames_to_column() %>% 
    dplyr::rename(w = rowname, v = `ranger_model$variable.importance`)

var_plot = ggplot(DF, aes(x=reorder(w,v), y=v,fill=v))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip() +
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Covariate Importance Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

  ggsave(filename = file.path( modelout_dir, paste0(model_name , "_var_import.png")), 
       plot = var_plot, width = 10, height = 20, units = "cm")

write.csv (ranger_model$variable.importance, file = file.path(modelout_dir, paste0(model_name,"_Importance.csv")))
write.csv (ranger_model$prediction.error, file = file.path(modelout_dir, paste0(model_name,"_Error.csv")))
write.csv (ranger_model$confusion.matrix, file = file.path(modelout_dir, paste0(model_name,"_ConfusionMatrix.csv")))

print("output generated")

}



# Function 3: generate output from ranger quick model using mlr setup (for testing) ---------------------



# install.packages("mlr", dependencies = TRUE)

model_gen_quick <- function(traindat , target, outDir = ".", rseed = NA, map = FALSE) {
  
# testing : GP
# traindat = bgc_pts_i
#  target = "mapunit1"  
#  outDir = paste0("D:/PEM_DATA/BEC_DevExchange_Work/Deception_AOI/2_sample_design/stage1_Analysis/models/",i)
#  rseed = 456
  
  
  library(mlr)
  library(tidyverse)
  
  ## create destination folder
  ifelse(!dir.exists(file.path(outDir)),                # if folder does not exist
         dir.create(file.path(outDir)), FALSE)         # create it
  
  ## Convert to data frame -------------------
  if("sf" %in% class(traindat)) {
    traindat <- as.data.frame(traindat)
    traindat <- traindat[, -length(traindat)]
    print("Data is a sf object -- converted to dataframe for modelling")
  }
  
  ## error testing ----------------
  if (sum(is.na(traindat[,target])) > 0) {
    # print(paste("There are,", sum(is.na(traindat[,target]))  , "NA values in the target:", target))
    stop(paste("There are,", sum(is.na(traindat[,target]))  , "NA values in the target:", target))
  }
  
  ## Begin modeling 
  
  
  ### Define the Task and Learner
  
  ## use or create a random number seed -- this can be used to repeat results in future.
  if (!is.na(rseed)) {
    set.seed(rseed) 
    print(paste("Random number generator seed set to:", rseed))
  } else {
    rseed <- as.integer(Sys.time())
    print(paste("Random number generator seed set to:", rseed))
  } 
  
  
  ## Create task 
  tsk <- makeClassifTask(data = traindat, target = target)
  
  ## Define Learner
  lrn <- makeLearner("classif.ranger",
                     num.trees = 500,                         ## number of trees DEFAULT: 500
                     mtry = round(sqrt(ncol(traindat)-1)),      ## someone showed me to declare mtry this way
                     num.threads = parallel::detectCores()*2,   ## CAUTION HERE: how many threads does your machine have?
                     importance = "impurity",                 ## collect var importance data
                     predict.type = "response")        
  
  ### Train the model
  
  #The model is trained using all the data and then saved. 
  
  mod <- train(lrn, tsk)
  saveRDS(mod, paste(outDir, "model_quick.rds", sep = "/"))
  
  ### Variable importance
  
  var_imp <- as.data.frame(mod$learner.model$variable.importance) %>%
    rownames_to_column()
  names(var_imp) <- c("name", "VaribleImportance")
  
  write.csv (var_imp, file.path(outDir,paste0(i,"_Importance.csv")))
  
  
}










# function 4 Make a quick map using direct rangetr outputs quck function  


predict_landscape_quick <- function(mod, cov, tilesize = 500,
                                    outDir = "./quickmap", 
                                    bgc = FALSE) {
  
  mod =  model 
  cov = rast_list
  tilesize = 500
  outDir = file.path(out_dir,mname,"quick_map")                                                      
  bgc = FALSE 
  
  ## libraries  -----
  
  library(dplyr)
  library(mlr)
  library(terra)
  
  ## Adjust names
  ## This will be used in the loop to rename stars object
  n <- gsub(".tif", "", basename(cov))
  
  # set up the bgc to filter for tiles with particular variant
  bgc.sf = st_read(file.path(shapes_dir, "bec.gpkg")) %>%
    st_transform("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") %>%
    dplyr::select(MAP_LABEL)
  
  # Need to fix the eited BCG as topology errors
  #bgc.sf = st_read(file.path(shapes_dir, "bec_edited.gpkg")) %>%
  #  st_set_precision(1000000) %>% 
  #  st_make_valid()
  
  if (isTRUE(bgc)){
    
    bgc_filter = bgc.sf %>% filter(str_detect(MAP_LABEL, i))
    
  } else {
    
    bgc_filter = bgc.sf %>% st_union() 
    
  }
  
  ## Error handle -- model vs. cov -----------
  ## If names in model features are found in the cov list continue.
  ## ELSE exit with message
  if (length(setdiff(mod$features, n)) != 0) { ## tests if all model features are found in the cov list
    ## On model vs. cov error ---------------
    print("Name mis-match between the model features and the names of the rasters.")
    print("The following raster co-variates are not found in the model features list:")
    print(setdiff(mod$features, n))
  } else {
    
    ifelse(!dir.exists(outDir), dir.create(outDir, recursive = TRUE), print("model folder already exists"))
    
    source(here::here('_functions', 'tile_index.R'))
    
    tiles <- tile_index(cov[1], tilesize)
    
    ## begin loop through tiles ----
    
    for (ii in 1:nrow(tiles)) {    ## testing first 2 tiles       ##nrow(tiles)) {
      
      t <- tiles[4,] # testing line
      
      t <- tiles[ii,]  ## get tile
      print(paste("working on ", ii, "of", nrow(tiles)))
      print("...")
      
      # check against the bgc filter 
      
      bgc_check = st_intersects(t, bgc_filter, sparse = FALSE)
      
      if(all(bgc_check == FALSE)) {
        
        ## * load tile area---------
        print("... tile is not within bgc area of extent and will be ignored...")
        
      } else {
        
        ## * load tile area---------
        print("... loading new data (from rasters)...")
        
        # extract raster for the area of the tile
        
        t <- as(t, 'Spatial')
        r <- raster::crop(stack(cov) , extent(t))
        r <- mask(r, as(bgc_filter,'Spatial'))
        
        
        # create a raster template in stars to convert back to stars object 
        rtemplate <- stars::st_as_stars(r[[1]])
        
        ### chcek this!!!!
        
        
        # grab point values for the raster stack
        rsf <- rasterToPoints(r) 
        
        rsf.xy <- rsf[,c(1,2)] # keep xy to convert back to raster below
        rsf <- rsf[,-c(1:2)] # drop xy values
        colnames(rsf) = tolower(colnames(rsf))
        rsf.df <- as.data.frame(rsf) 
        
        ## * Test if tile is empty  -------
        if(any(sapply(rsf.df, function(x) all(is.na(x))) == TRUE)){
          
          print("some variables with all NA values, skipping tile")
          
        } else {
          
          ## * predict ---------
          ## When some of the values are NA change them to zero
          rsf_bk <- rsf  ## create a backup of rsf -- this will be used to restore NA values
          rsf.df[is.na(rsf.df)] <- 0 ## convert NA to zero as the predict function cannot handle NA
          
          print("... modelling outcomes (predicting)...")
          pred <- predict(mod, newdata = rsf.df)
         # pred <- predict(mod, data = rsf.df)#, type = "response")
          
          
          ## Restore NA values
          pred_dat <- as.data.frame(pred$predictions) ## predicted values extracted then changed
          pred_dat[is.na(rsf_bk[,1]), 1:length(pred_dat)] <- NA ## if originally NA restore NA   
          
          
          ## * geo-link predicted values ---------
          
          r_out <- cbind(rsf.xy, as.data.frame(pred$predictions)) %>%
            dplyr::rename(response = "pred$predictions")
          
          
          keep <- setdiff(names(r_out), names(r))
          
          
          
          
          
          ## Save the names of the model response -----
          ## The levels are in the multiclass 'response'
          
          wkey <- 0
          if (wkey == 0)  {
            respNames <- levels(r_out$response) ## this becomes the dictionary to describe the raster values
            write.csv(respNames, paste(outDir, "response_names.csv",
                                       sep = "/"),
                      row.names = TRUE)
            wkey <- 1 ## Change this value so this small is statement does not execute again.
          }
          
          
          ## change the text values to numeric values.
          r_out$response <- as.numeric(r_out$response)
          r_out <- st_as_sf(r_out, coords = c("x", "y"), crs = 3005)
          
          ## Set up subdirectories for rastertile outputs
          print("... exporting raster tiles...")
          dir.create(paste(outDir, "response", sep = "/"))
          
          
          ## * save tile (each pred item saved) -------
          
          out <- stars::st_rasterize(r_out["response"],
                                     template = rtemplate)
          st_crs(out) = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
          
          stars::write_stars(out,
                             paste0(outDir,"/",
                                    "response", "/",             #sub-directoy
                                    "response", "_", ii, ".tif")) #tile name
        }
        
      } 
      
    } # end of each tile loop 
  }
  #} ## END LOOP -------------
  
  print("All predicted tiles generated")
  
  
  ## Mosaic Tiles ---------------
  
  
  r_tiles <- list.files(paste(outDir, "response", sep = "/"),
                        pattern = ".tif$",
                        full.names = TRUE)
  
  
  #raster(r_tiles[1])
  
  ## mosaic
  gdalUtils::mosaic_rasters(gdalfile = r_tiles, ## list of rasters to mosaic
                            dst_dataset = paste(outDir, "response.tif", sep = "/"),  #output: dir and filename
                            output_Raster = TRUE) ## saves the raster (not just a virtual raster)
  
}  

 


