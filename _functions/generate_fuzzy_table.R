

## testing - this is a place holder or scripting which ended up using manual methods as 
## Could not automate : Dec 2021, Gen 


#Create an ecotype grid for fuzzy metrics 

library(dplyr)
library(raster)
library(stringr)
library(readxl)
library(tidyverse)

# read in table
indir <- ("C:\\Users\\genperk\\OneDrive - Government of BC\\PEM_DATA\\_data_sharing")

# williams lake
infile <- read_excel(file.path(indir, "Edatopic_grids.xlsx"),sheet = 1) 

inIDF <- infile[c(1:10),] %>%
  mutate(across(starts_with("IDF"), as.numeric))

in_l <- inIDF %>%
  pivot_longer(cols = starts_with("IDF"),
                values_to = "pred",
                names_to = "fVal") 
  
inIDFx <- infile[c(12:20),] %>%
  mutate(across(starts_with("IDF"), as.numeric))








AOI <- "Deception"
AOI_dir <- file.path(paste0("./", AOI,"_AOI"))

bcecodata <- read.csv(file.path("_data_allBC","Edatopic_v12_11.csv")) %>%
  dplyr::select(-Source)



#Assign values to edatopic grid positions ( 1:40)
ed_df = tibble(Edatopic = c(sort(unique(bcecodata$Edatopic)))) %>%
  mutate(order = str_sub(Edatopic, 2,2)) %>%
  arrange(order) %>%
  mutate( val = c(seq(1,length(order)))) %>%
  dplyr::select(-order)

ed_df

# add the raster value number based on edatopic position 
bcecodata <- left_join(bcecodata, ed_df) 

# # Build a base template raster (1-40 values)
r <- raster(ncol = 5, nrow = 8)
values(r) <- 1:ncell(r)

# # set up blank raster
r_blank <- raster(ncol = 5, nrow = 8)

#r_of_interest <- raster(ncol = 5, nrow = 8)
#values(r_of_interest) <- 1


# select the variant of interest
voi <- "SBSmc2"

vdata <- bcecodata %>% filter(BGC == voi) %>%
  dplyr::select (-BGC)

# loop through the site series classes of the variant of interest
# all site series per 
ss <- unique(vdata$SS_NoSpace)



class_name <- ss[1] #loop through the target locations 
#[1] "SBSmc2/01"

# get location/values of site series to map 
class_pos <- vdata %>% 
  dplyr::filter(SS_NoSpace %in% class_name)%>%
  dplyr::select(val) %>%
  pull()

# get inerse of location/values of site series to map 
no_pos <- setdiff(values(r), class_pos)

ss_pos <- values(r) %in% class_pos
ss_no_pos <- values(r) %in% no_pos


# # Built target ss raster 
ss_r <- r_blank
values(ss_r)[ss_pos] <- 1


# loop through all the other units 


#class_pos



# build the adjoining site series 




adj_name <- ss[5] # loop through each of the other mapunits

ss_pos_adj <- vdata %>% filter(SS_NoSpace == adj_name) %>%
  rename("adjacent_class" = "SS_NoSpace") %>%
  dplyr::select(val) %>%
  pull()

# 1 check if there are direct overlaps first 

if(any(is.na(match(ss_pos_adj, class_pos)))){ #= T {
  
  print("no direct overlap")
  
} else {
  
  print(" do something here to remove direct adjact and assignb 0.1 values")
  
}

# check for adjacent cells 

# # Built target ss raster 
adj_r <- ss_r
values(adj_r)[ss_pos_adj ] <- 2

#temp fix
ss_pos_adj <- c(14, 9, 13, 8, 7, 12, 11, 16, 6)

plot(adj_r, add = T)


#https://gis.stackexchange.com/questions/70776/efficiently-check-for-equal-adjacent-values-in-raster-calculator

r <- raster(nrows=10, ncols=10)
xx <-adjacent(r, cells=c(24,19,23, 18, 22, 17), directions=4, pairs=TRUE) 


tb <- table(adj_r[xx[,1]], adj_r[xx[,2]])
tb
tb <- unclass(tb)











plot(raster(tb, xmn=-0.5, xmx=5.5, ymn=-0.5, ymx=5.5))

r[c(1,55,90)] <- 1
r[a] <- 2
plot(r)

# Count the number of times that a cell with a certain value
# occurs next to a cell with a certain value
set.seed(0)
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r)) * 5)
a <- adjacent(r, 1:ncell(r), 4, pairs=TRUE)
tb <- table(r[a[,1]], r[a[,2]])
tb
# make a matrix out of the 'table' object
tb <- unclass(tb)
plot(raster(tb, xmn=-0.5, xmx=5.5, ymn=-0.5, ymx=5.5))
# }


str_split()




r <- raster(nrows=10, ncols=10)
adjacent(r, cells=c(1, 55), directions=8, pairs=TRUE) 

a <- adjacent(r, cell = c(1,55,90), directions=4, sorted=TRUE) 
a

r[c(1,55,90)] <- 1
r[a] <- 2
plot(r)

# same result as above
rook <- matrix(c(NA, 1, NA, 
                 1, 0,  1, 
                 NA, 1, NA), ncol=3, byrow=TRUE)

adjacent(r, cells = c(1,55,90), directions=rook, sorted=TRUE) 


# Count the number of times that a cell with a certain value
# occurs next to a cell with a certain value
set.seed(0)
r <- raster(ncol=10, nrow=10)
values(r) <- round(runif(ncell(r)) * 5)
a <- adjacent(r, 1:ncell(r), 4, pairs=TRUE)
tb <- table(r[a[,1]], r[a[,2]])
tb
# make a matrix out of the 'table' object
tb <- unclass(tb)
plot(raster(tb, xmn=-0.5, xmx=5.5, ymn=-0.5, ymx=5.5))







# select the variant of interest

voi <- "SBSmc2"

vdata <- bcecodata %>% filter(BGC == voi)

# loop through the site series classes of the variant of interest
ss <- unique(vdata$SS_NoSpace)

# test iteration 
    class_name <- ss[1]

    class_pos <- vdata %>% 
      dplyr::filter(SS_NoSpace %in% class_name)%>%
      dplyr::select(val) %>%
      pull()

    no_pos <- setdiff(values(r), class_pos)
 
    ss_pos <- values(r) %in% class_pos
    ss_no_pos <- values(r) %in% no_pos
    values(r)[ss_pos] <- values(r_of_interest)[ss_pos]  
  
# option 2 - use grid
#I updated these values. I will need to present the formal method of calculating
#5% per shared side and 10% per shared grid position.
# We may want to compare against % transition calls in transect data
    
  
    
    
    
    
    # set up raster and assign values based on key 
   r_blank <- raster(ncol = 5, nrow = 8)
     
     # positional raster (top left to bottom right )
     r <- raster(ncol = 5, nrow = 8)
     values(r) <- 1:ncell(r)
     plot(r)
     
     
     # raster with 1
     r_of_interest <- raster(ncol = 5, nrow = 8)
     values(r_of_interest) <- 1
    
