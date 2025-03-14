library(foreach)


## Parallel processing ------------------------------
## https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
library(foreach) ## for parallel processing in loops
#parallel::detectCores()
n.cores <- parallel::detectCores() -1

## Set up processing cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster) ## register it -- so foreach will recognize it
## check set up:
print(my.cluster)             ## list number of nodes
foreach::getDoParRegistered() ## should be TRUE
foreach::getDoParWorkers()    ## should be # of nodes
## --------------------------------------------------


## create a set of system command calls.
las2las <- "c:\\LASTools\\bin\\las2las.exe"
out_dir <- "f:\\out_las"


## Input fileames
lst <- list.files("f:/PEM_2021/IDF_WilliamsLake/additional_bcts_IDF/Chasm_bcalb/", ".laz", full.names = TRUE)
lst <- gsub("/", "\\\\", lst) ## changes slashes ... needed for system call(?)


## Output filenames
out <- substr(lst, 1, nchar(lst)-19)  ## drop reference to BCAlbers
out <- basename(out)                  ## drop input directory
out <- paste0(out, "_epsg3157.laz")   ## add suffix
out <- paste0(out_dir, "\\", out)     ## add out-directory


## create sequence of system calls
mysys <- as.character(NA)
for (i in 1:length(lst)) {
mysys[i] <- paste(las2las, "-i", lst[i],        # input name
               "-epsg 3005",                  # input projection
               "-target_epsg 3157",           # output projection
               "-o", out[i])                 # output


}



### Runs the system calls in parallel
foreach (i = mysys) %dopar% {
  system(i)
  print(i)
}











## stop the cluster

parallel::stopCluster(cl = my.cluster)
