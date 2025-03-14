# In reparation for running models : 

## Install and load libraries: 

## OPTIONAL : The first time you run this in R : you need install libraries. 
#Install Libraries 
#ls <- c("dplyr","ggplot2","tidyr","stringr", "readxl", "foreign", "lsr", "car",  "moments", "psych",
#         "latex2exp","gtools", "knitr", "rgeos", "maptools", "raster", "sp","rgdal", "mapview", "sf")
# new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages,.libPaths("C:/Program Files/R/R-3.5.1/library"))
# rm(ls, new.packages)

# install.packages("xlsx",dep = TRUE, .libPaths("C:/Program Files/R/R-3.5.1/library"))

# Load Libraries 
x <- c("dplyr","ggplot2","tidyr","stringr","raster","sp","sf","rgdal",'snow',
       "xlsx","rJava","tibble","mapview","gtools","ModelMap","randomForest",
       "RColorBrewer","colorspace","rJava","caret","reshape2","ggcorrplot",
       "UBL","rasterVis","parallel","doParallel","foreach","maptools",
       "readxl",'tmap',"tcltk","viridis")

lapply(x,library, character.only = TRUE) 

rm(x)  # load the required packages

#rm(list=ls())
