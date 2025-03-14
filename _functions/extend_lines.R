# function : extend_lines
# parameters:
#   sf_lines : An sf dataframe object of LINESTRING geometry type
#   dist_factor : default = 1.01

# function : droplevels.sfc
# parameters:
#   x : Any sf object
#   except : 
#   include : 

extend_lines <- function(sf_lines, dist_factor = 1.01){
  line_start <- st_line_sample(sf_lines, sample = 0)
  line_end <- st_line_sample(sf_lines, sample = 1)
  dist_diff <- line_end - line_start
  new_line_end <- line_start + dist_diff * dist_factor
  st_crs(new_line_end) <- st_crs(sf_lines)
  extended_lines <- st_nearest_points(line_start, new_line_end, pairwise = TRUE)
}

droplevels.sfc = function(x, except, exclude, ...) x