multiline_to_line <- function(sf_multilinestring) {
  
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  lines <- foreach(i = 1:nrow(sf_multilinestring), .packages = c("sf", "tidyverse"), .combine = rbind) %dopar% {
  #for(i in 1:nrow(sf_multilinestring)) {
    temp <- st_geometry(sf_multilinestring[i, "geometry"])[[1]]
    if(st_geometry_type(temp) != "LINESTRING") {
      if(length(temp) > 2) {
        bottoms <- matrix(unlist(lapply(temp, function(x) tail(x, n = 1))), ncol = 2, byrow = TRUE) %>% 
          as.data.frame() %>% 
          mutate(xy = paste(V1, V2), id = row_number())
        
        tops <- matrix(unlist(lapply(temp, function(x) head(x, n = 1))), ncol = 2, byrow = TRUE) %>% 
          as.data.frame() %>% 
          mutate(xy = paste(V1, V2), id = row_number())
        
        # Find the first linestring. Theoretically, the tops of data matrices should
        # be the same as the bottoms of the matrices above them. Therefore, for a 
        # multilinestring containing 3 linestrings, the top data points of 2 of those 
        # linestring matrices should match the bottom data points of 2 linestrings. 
        # The top data points that don't match are the beginning of the line
        start_index <- which(!tops$xy %in% bottoms$xy)
        end_index <- which(!bottoms$xy %in% tops$xy)
        
        # If there was no match, indicates some duplication of start/end points, but
        # we should still be able to work with that by vector matching
        if(!length(start_index) && length(end_index)) {
          # Identify duplicated start points, filter points already in the end index
          coords1 <- which(tops$xy %in% tops$xy[duplicated(tops$xy)])
          start_index <- coords1[!coords1 %in% end_index]
        }
        
        if(length(start_index) && !length(end_index)) {
          coords1 <- which(bottoms$xy %in% bottoms$xy[duplicated(bottoms$xy)])
          end_index <- coords1[!coords1 %in% start_index]
        }
        
        if(length(start_index) > 1) {
          new_start_index <- start_index
          for(j in start_index) {
            if(nrow(temp[[j]]) == 2 && temp[[j]][1, ] == temp[[j]][2, ]) {
              new_start_index <- new_start_index[-j]
            }
          }
          if(length(new_start_index) > 1) {
            coords1 <- which(tops$xy[new_start_index] %in% tops$xy[new_start_index][duplicated(tops$xy)])
            start_index <- coords1[!coords1 %in% end_index]
            if(length(start_index) > 1) start_index <- start_index[1]
            if(length(start_index) == 0) start_index <- new_start_index[1] #### I don't like this as a failsafe
          } else {
            start_index <- new_start_index
          }
        }
        
        if(length(end_index) > 1) {
          new_end_index <- end_index
          for(j in end_index) {
            if(nrow(temp[[j]]) == 2 && temp[[j]][1, ] == temp[[j]][2, ]) {
              new_end_index <- new_end_index[-j]
            }
          }
          if(length(new_end_index) > 1) {
            coords1 <- which(bottoms$xy[new_end_index] %in% bottoms$xy[new_end_index][duplicated(bottoms$xy)])
            end_index <- coords1[!coords1 %in% end_index]
            if(length(end_index) > 1) end_index <- end_index[1]
            if(length(end_index) == 0) end_index <- new_end_index[length(new_end_index)] #### I don't like this as a failsafe
          } else {
            end_index <- new_end_index
          }
        }
        
        #remaining <- temp[-c(start_index, end_index)]
        new_line <- start_index
        new_line_end <- end_index
        
        start_mat <- temp[[start_index]]
        start_bottom <- paste(start_mat[nrow(start_mat), ], collapse = " ")
        start_bot_pt <- st_point(c(start_mat[nrow(start_mat), 1], start_mat[nrow(start_mat), 2]))
        
        end_mat <- temp[[end_index]]
        end_top <- paste(end_mat[1, ], collapse = " ")
        end_top_pt <- st_point(c(end_mat[1, 1], end_mat[1, 2]))
        
        # MAybe try something with this...index matching
        for(j in 1:length(temp)) {
          match_top <- which(tops$xy %in% start_bottom) # ID top(s) of linestring sf dfs
          match_top <- match_top[!match_top %in% c(new_line, new_line_end)]
          
          if(length(match_top) == 1) {
            new_line <- c(new_line, match_top)
            start_bottom <- paste(temp[[match_top]][nrow(temp[[match_top]]), ], collapse = " ")
            
            # if there is more than 1 result, try building up bottom first
          } else if(length(match_top) != 1) {
            match_bottom <- which(bottoms$xy %in% end_top) # ID top(s) of linestring sf dfs
            match_bottom <- match_bottom[!match_bottom %in% c(new_line, new_line_end)]
            
            if(length(match_bottom) == 1) {
              new_line_end <- c(match_bottom, new_line_end)
              end_top <- paste(temp[[match_bottom]][1, ], collapse = " ")
              
            } else if(length(match_top) > 1 && length(match_bottom > 1)) {
              match_top_pt <- st_multipoint(as.matrix(bottoms[match_top, 1:2]))
              match_index <- st_nearest_feature(start_bot_pt, match_top_pt)
              
              new_line <- c(new_line, match_top[match_index])
              start_bottom <- paste(temp[[match_top[match_index]]][nrow(temp[[match_top[match_index]]]), ], collapse = " ")
            
            } else if(!length(match_top) && length(match_bottom > 1)) {
              match_bot_pt <- st_multipoint(as.matrix(tops[match_bottom, 1:2]))
              match_index <- st_nearest_feature(end_top_pt, match_bot_pt)
              
              new_line_end <- c(match_bottom[match_index], new_line_end)
              end_top <- paste(temp[[match_bottom[match_index]]][1, ], collapse = " ")
            }
          }
        }
        
        new_line <- c(new_line, new_line_end)
        new_linestring <- matrix(nrow = 0, ncol = 2)
        for(j in new_line) {
          new_linestring <- rbind(new_linestring, temp[[j]])
        }
        
        new_linestring_1 <- st_linestring(new_linestring) %>% 
          st_geometry() %>% 
          st_set_crs(st_crs(sf_multilinestring))
        st_geometry(sf_multilinestring[i, ]) <- new_linestring_1
        return(sf_multilinestring[i, ])
        
        # If there are only 2 lines, match the top/bottom frames properly (easily coded)
      } else if(length(temp) == 2) {
        if(all(temp[[1]][nrow(temp[[1]]), ] == temp[[2]][1, ])) {
          temp_bind <- st_linestring(rbind(temp[[1]], temp[[2]])) %>% 
            st_geometry() %>% 
            st_set_crs(st_crs(sf_multilinestring))
          st_geometry(sf_multilinestring[i, ]) <- temp_bind
          return(sf_multilinestring[i, ])
          
        } else if(all(temp[[2]][nrow(temp[[2]]), ] == temp[[1]][1, ])) {
          temp_bind <- st_linestring(rbind(temp[[2]], temp[[1]])) %>% 
            st_geometry() %>% 
            st_set_crs(st_crs(sf_multilinestring))
          st_geometry(sf_multilinestring[i, ]) <- temp_bind
          return(sf_multilinestring[i, ])
          
        } else {
          temp_bind <- st_linestring(rbind(temp[[1]], temp[[2]])) %>% 
            st_geometry() %>% 
            st_set_crs(st_crs(sf_multilinestring))
          st_geometry(sf_multilinestring[i, ]) <- temp_bind
          return(sf_multilinestring[i, ])
        }
        
      } else if(length(temp) == 1) {
        st_geometry(sf_multilinestring[i, ]) <- st_set_crs(st_geometry(st_linestring(temp[[1]])), 
                                                           st_crs(sf_multilinestring))
        return(sf_multilinestring[i, ])
      }
    } else return(sf_multilinestring[i, ])
  } %>% st_cast("LINESTRING")
  parallel::stopCluster(cl)
  return(lines)
}
