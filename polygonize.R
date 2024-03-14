#####################File that reads the cluster data and convert them into polygons and points######################

library (tidyverse)
library(data.table)
library(adehabitatHR)
library(concaveman)
library(rgeos)
library(sf)
library(smoothr)
library(maptools)

plot <- 0
source("mapLoad.R")
source("speciesAttr.R")


#speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")
#speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")
species <- read.csv("species.csv") 
species <- species[,1]

dist_thresholds <- c(20, 50, 100)
#dist_thresholds <- c(100)
dist_thresholds <- speciesattr$RESOLUTION %>% unique() %>% na.omit() %>% as.vector() %>% sort()

# Go over all the species.
for (sp in species)
{
  for (dist in dist_thresholds)
  { 
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), 100, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`)  
    if (!is.na(Seasonal$SEASONAL))
    {
      seasons <- strsplit(as.character(Seasonal$SEASONAL),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      
      for(season in seasons)
      {
        tryCatch ({
        if (! ( file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist,"_",season,".rds")) | 
                file.exists(paste0(".\\loctable\\loctable_",sp,"_",season,".rds")))) 
            { 
              print(paste("No file for",sp,dist,season)) 
              next
        }
          
        cluster <- NULL
        if(file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist,"_",season,".rds")))
        {
          cluster <- readRDS(paste0(".\\cluster\\cluster_",sp,"_", dist,"_",season, ".rds"))
        }
        spec_loc_table <- NULL
        if(file.exists(paste0(".\\loctable\\loctable_",sp,"_",season,".rds")))
        {
          spec_loc_table <- readRDS(paste0(".\\loctable\\loctable_",sp,"_",season,".rds"))
        }
        
        clusterPolygonCount <- 0
        if(!is.null(cluster))
        {
          clusters <- cluster$CLUSTER %>% unique()
          
          clusterPolygons <- list()
  
          # Iterate over the clusters and create polygons, make it into a list.
          for (j in clusters)  
          {
            if(!is.na(j) && !is.null(spec_loc_table))
            {
              loc3 <- cluster %>% 
                filter (CLUSTER == j) %>% 
                inner_join(spec_loc_table, by = c("LOCALITY.ID" = "LOCALITY_ID")) %>% 
                mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
                       LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>%
                dplyr::select(LATITUDE, LONGITUDE) %>%
                distinct()
      
                sp::coordinates(loc3) <- ~LONGITUDE+LATITUDE
                CH = gConvexHull(loc3)
              
              if("polygons" %in% slotNames(CH))
              {
                clusterPolygonCount <- clusterPolygonCount + 1
                
                PolygonType <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`POLYGON.TYPE`)
              
                if(PolygonType == "Concave")
                {
                  dat_sf <- st_as_sf(loc3, coords = c("LONGITUDE", "LATITUDE"))
                  concave <- concaveman(dat_sf, concavity = 1.0,  length_threshold = 0.3)
                  CH <- as_Spatial(concave)
                }
                else
                {
                  # Use Convexhull itself
                }
              
                r_poly_smooth <- smooth(CH, method = "chaikin")
      
                # Assigning unique ID
                r_poly_smooth@polygons[[1]]@ID <- as.character(clusterPolygonCount)
                
                clusterPolygons[[clusterPolygonCount]] <- r_poly_smooth
              }
              else
              { # Remove these points from the cluster as they lie in a straight line
                cluster <- cluster %>%
                  mutate(CLUSTER = ifelse(CLUSTER == j, NA, CLUSTER))
              }
            }
          }
        }
        
        clippedMergedPolygons <- NULL
        if (clusterPolygonCount > 0)
        {
        # Do not pipe these code. These code can break as packages are routinely deprecated and would need debuggin
        # Merging overlapping polygons
          rawPolygons <- lapply(clusterPolygons, function(x) x@polygons[[1]])
          spatialPolygons <- SpatialPolygons(rawPolygons)
          mergedPolygons <- gUnaryUnion(spatialPolygons)
    
          # Clipping for country/land boundaries
          mergedPolygons_sf <- st_as_sf(mergedPolygons, coords = c("long", "lat"))
          
          sub_mergedPolygons_sf <- mergedPolygons_sf
          st_crs(sub_mergedPolygons_sf) <- st_crs(ind_sub)
          sub_clippedMergedPolygons_sf <- st_intersection(sub_mergedPolygons_sf, ind_sub)
          
          isl_mergedPolygons_sf <- mergedPolygons_sf
          st_crs(isl_mergedPolygons_sf) <- st_crs(ind_isl)
          isl_clippedMergedPolygons_sf <- st_intersection(isl_mergedPolygons_sf, ind_isl)
          
          clippedMergedPolygons_sf <- rbind (sub_clippedMergedPolygons_sf, isl_clippedMergedPolygons_sf)
    
          # Add clips for all other countries        
          clippedMergedPolygons <- as_Spatial(clippedMergedPolygons_sf)
        }
        
        if(!is.null (clippedMergedPolygons))
        { 
          print(paste("Saving polygons for",sp,dist,season))
          saveRDS(clippedMergedPolygons, paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season, ".rds"))
        }

        ##############Saved all polygons###################################
        
        clusterNA <- cluster %>% 
          filter (is.na(CLUSTER)) %>%
          mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
                 LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>% 
          dplyr::select(LATITUDE, LONGITUDE)
        
        # There are points and polygons, then process them for inclusion by removing the points
        # that are inside the polygons
        if( (nrow(clusterNA) > 0) && (length(clippedMergedPolygons) > 0))
        {
          spClusterNA <- clusterNA
          sp::coordinates(spClusterNA) <- ~LONGITUDE+LATITUDE
          
          spCluster <- SpatialPolygons(clippedMergedPolygons@polygons)
          proj4string(spClusterNA) <- proj4string(spCluster)
          
          points_within_polygons <- over( spClusterNA, spCluster)
          

          clusterNA <- cbind(clusterNA, points_within_polygons)
          
          # All NA from the over function are non-overlapping points to be plotted
          clusterNA <- clusterNA[is.na(clusterNA$points_within_polygons), ]
        }    
        
        clusterNA$points_within_polygons <- NULL
        
        if(!is.null(clusterNA))
        {
          print(paste("Saving points for",sp,dist,season))
          saveRDS(clusterNA, paste0(".\\points\\points_",sp,"_",dist,"_", season, ".rds"))
        }

        #   Save space
        loc3 <- NULL
        cluster <- NULL
        clusterNA <- NULL
        spec_loc_table <- NULL
        clippedMergedPolygons <- NULL
      }, error = function (err) {
            cat("Error occurred:", sp, conditionMessage(err), "\n")
        })
      }
      
      seasonPriority <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASON.PRIORITY`) 
      
      if (!is.na(seasonPriority))
      {
        seasonPriority <- strsplit(as.character(seasonPriority),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      }
      else
      {
        seasonPriority <- c("S", "W", "P")
      }
        
      # Find intersecting polygons, find the difference, remove the lower priority one.
      for(i in length(seasonPriority):2)
      {
        polygons1 <- NULL
        if(file.exists(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", as.character (seasonPriority[i]), ".rds")))
        {
          polygons1 <- readRDS(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", as.character (seasonPriority[i]), ".rds"))
        }
        if(!is.null(polygons1))
        {
          for(j in (i-1) : 1)
          {
            print(paste(i,j))
            polygons2 <- NULL
            if(file.exists(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", as.character (seasonPriority[j]), ".rds")))
            {
              polygons2 <- readRDS(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", as.character (seasonPriority[j]), ".rds"))
            }
            if(!is.null(polygons2))
            {
              print(paste("Removing",seasonPriority[j],"range from",seasonPriority[i]))
              polygons1 <- gDifference(polygons1, polygons2)
            }
          }
        }
        if(!is.null (polygons1))
        {
          print(paste("Saving polygons for",sp,dist,seasonPriority[i]))
          saveRDS(polygons1, paste0(".\\polygons\\polygons_",sp,"_",dist,"_", seasonPriority[i], ".rds"))
        }
      }
      
      # Remove points which overlap with any polygons in any season.
      for(season1 in seasons)
      {
        clusterNA <-NULL
        if(file.exists(paste0(".\\points\\points_",sp,"_",dist,"_", season1, ".rds")))
        {
          clusterNA <- readRDS(paste0(".\\points\\points_",sp,"_",dist,"_", season1, ".rds"))
        }
        
        if(!is.null(clusterNA))
        {
          for(season2 in seasons)
          {
            if (season1 == season2) next;
            tryCatch ({
              clippedMergedPolygons <- NULL
              if(file.exists(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season2, ".rds")))
              {
                clippedMergedPolygons <- readRDS(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season2, ".rds"))
              }
              
              # There are points and polygons, then process them for inclusion by removing the points
              # that are inside the polygons
              if( (nrow(clusterNA) > 0) && !is.null(clippedMergedPolygons) && (length(clippedMergedPolygons) > 0))
              {
                print(paste("Removing points from", season1,"with polygon", season2))
                spClusterNA <- clusterNA
                sp::coordinates(spClusterNA) <- ~LONGITUDE+LATITUDE
                
                spCluster <- SpatialPolygons(clippedMergedPolygons@polygons)
                proj4string(spClusterNA) <- proj4string(spCluster)

                points_within_polygons <- over( spClusterNA, spCluster)
                
                clusterNA <- cbind(clusterNA, points_within_polygons)
                
                # All NA from the over function are non-overlapping points to be plotted
                clusterNA <- clusterNA[is.na(clusterNA$points_within_polygons), ]
              }    
              clusterNA$points_within_polygons <- NULL
  
              clippedMergedPolygons <- NULL
            }, error = function (err) {
              cat("Error occurred:", sp, conditionMessage(err), "\n")
            })
          }
        }
        # Verify if any of the resulting polygons is to small, if yes (a) remove it or (b) merge it. 
        # Before removing and merging, find if there are any points in cluster that is a result of this polygon that is going to be removed
        # Move such points to clusterNA

        for(season1 in seasons)         
        {
          clippedMergedPolygons <- NULL
          if(file.exists(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season1, ".rds")))
          {
            clippedMergedPolygons <- readRDS(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season1, ".rds"))
          }
          
          if( !is.null(clippedMergedPolygons) && (length(clippedMergedPolygons) > 0))
          {
            
          }
            
        }
        
        
        if(!is.null(clusterNA))
        {
          print(paste("Saving points for",sp,dist,season1))
          saveRDS(clusterNA, paste0(".\\points\\points_",sp,"_",dist,"_", season1, ".rds"))
        }
        clusterNA <- NULL
      }
    }
    else
    {
      tryCatch ({
        if (! ( file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist, ".rds")) | 
                file.exists(paste0(".\\loctable\\loctable_",sp,".rds")))) 
        { 
          print(paste("No file for",sp, dist)) 
          next
        }
        
        cluster <- NULL
        if (file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist, ".rds")))  
        {
          cluster <- readRDS(paste0(".\\cluster\\cluster_",sp,"_", dist, ".rds"))
          
        }
        
        spec_loc_table <- NULL
        if (file.exists(paste0(".\\loctable\\loctable_",sp,".rds")))
        {
          spec_loc_table <- readRDS(paste0(".\\loctable\\loctable_",sp,".rds"))
        }
        
        clusterPolygonCount <- 0
        if(!is.null(cluster))
        {
          clusters <- cluster$CLUSTER %>% unique()
          clusterPolygons <- list()
  
          # Iterate over the clusters and create polygons, make it into a list.
          for (j in clusters)  
          {
            if(!is.na(j))
            {
              loc3 <- cluster %>% 
                filter (CLUSTER == j) %>% 
                inner_join(spec_loc_table, by = c("LOCALITY.ID" = "LOCALITY_ID")) %>% 
                mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
                       LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>%
                filter (LATITUDE <= boundsConfig['None','yhigh'] & LATITUDE >= boundsConfig['None','ylow']) %>%
                dplyr::select(LATITUDE, LONGITUDE) %>%
                distinct()
              
              if (nrow(loc3) > 0)
              {
                sp::coordinates(loc3) <- ~LONGITUDE+LATITUDE
                CH = gConvexHull(loc3)
                
                if("polygons" %in% slotNames(CH))
                {
                  clusterPolygonCount <- clusterPolygonCount + 1
                  
                  PolygonType <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`POLYGON.TYPE`)
                  
                  if(PolygonType == "Concave")
                  {
                    dat_sf <- st_as_sf(loc3, coords = c("LONGITUDE", "LATITUDE"))
                    concave <- concaveman(dat_sf, concavity = 1.0,  length_threshold = 0.3)
                    CH <- as_Spatial(concave)
                  }
                  else
                  {
                    # Use Convexhull itself
                  }
                  
                  r_poly_smooth <- smooth(CH, method = "chaikin")
                  buffer_distance <- 0.0001
                  r_poly_smooth <- gBuffer(r_poly_smooth, byid = TRUE, width = buffer_distance)
                  
                  # Check for invalid geometries
                  invalid_geometries <- !gIsValid(r_poly_smooth)
                  
                  # If there are invalid geometries, attempt to fix them
                  if (any(invalid_geometries)) {
                    # Identify and fix invalid geometries
                    print(paste("Invalid geometries for cluster",j))
  #                  r_poly_smooth <- gMakeValid(r_poly_smooth)
                  }                

                  # Assigning unique ID
                  r_poly_smooth@polygons[[1]]@ID <- as.character(clusterPolygonCount)
                  
                  clusterPolygons[[clusterPolygonCount]] <- r_poly_smooth
                }
                else
                { # Remove these points from the cluster as they lie in a straight line
                  cluster <- cluster %>%
                    mutate(CLUSTER = ifelse(CLUSTER == j, NA, CLUSTER))
                }
              }
            }
          }
        }
        
        clippedMergedPolygons <- NULL
        if (clusterPolygonCount > 0)
        {
          # Do not pipe these code. These code can break as packages are routinely deprecated and would need debuggin
          # Merging overlapping polygons
          rawPolygons <- lapply(clusterPolygons, function(x) x@polygons[[1]])
          spatialPolygons <- SpatialPolygons(rawPolygons)
          mergedPolygons <- gUnaryUnion(spatialPolygons)
          
          # Clipping for country/land boundaries
          mergedPolygons_sf <- st_as_sf(mergedPolygons, coords = c("long", "lat"))
          
          sub_mergedPolygons_sf <- mergedPolygons_sf
          st_crs(sub_mergedPolygons_sf) <- st_crs(ind_sub)
          sub_clippedMergedPolygons_sf <- st_intersection(sub_mergedPolygons_sf, ind_sub)
          
          isl_mergedPolygons_sf <- mergedPolygons_sf
          st_crs(isl_mergedPolygons_sf) <- st_crs(ind_isl)
          isl_clippedMergedPolygons_sf <- st_intersection(isl_mergedPolygons_sf, ind_isl)
          
          clippedMergedPolygons_sf <- rbind (sub_clippedMergedPolygons_sf, isl_clippedMergedPolygons_sf)
          
          # Add clips for all other countries        
          clippedMergedPolygons <- as_Spatial(clippedMergedPolygons_sf)
        }
        
        if(!is.null(clippedMergedPolygons))
        {
          print(paste("Saving polygons for",sp,dist))
          saveRDS(clippedMergedPolygons, paste0(".\\polygons\\polygons_",sp,"_",dist,".rds"))
        }
        
        ##############Saved all polygons###################################
        
        clusterNA <- cluster %>% 
          filter (is.na(CLUSTER)) %>%
          mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
                 LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>% 
          dplyr::select(LATITUDE, LONGITUDE)
        
        # There are points and polygons, then process them for inclusion by removing the points
        # that are inside the polygons
        if( (nrow(clusterNA) > 0) && (length(clippedMergedPolygons) > 0))
        {
          spClusterNA <- clusterNA
          sp::coordinates(spClusterNA) <- ~LONGITUDE+LATITUDE
          
          spCluster <- SpatialPolygons(clippedMergedPolygons@polygons)
          proj4string(spClusterNA) <- proj4string(spCluster)
          
          points_within_polygons <- over( spClusterNA, spCluster)
          
          clusterNA <- cbind(clusterNA, points_within_polygons)
          
          # All NA from the over function are non-overlapping points to be plotted
          clusterNA <- clusterNA[is.na(clusterNA$points_within_polygons), ]
        }    
        
        if(!is.null(clusterNA))
        {
          print(paste("Saving points for",sp,dist))
          saveRDS(clusterNA, paste0(".\\points\\points_",sp,"_",dist,".rds"))
        }
        
        #   Save space
        loc3 <- NULL
        cluster <- NULL
        clusterNA <- NULL
        spec_loc_table <- NULL
        clippedMergedPolygons <- NULL
      }, error = function (err) {
        cat("Error occurred:", sp, conditionMessage(err), "\n")
      })
    }
  }
}


calculateArea <- function(region_name) {
  region_coords <- boundsConfig[region_name, ]
  area <- geosphere::areaPolygon(cbind(c(region_coords['xlow'], region_coords['xhigh'], region_coords['xhigh'], region_coords['xlow'], region_coords['xlow']),
                                       c(region_coords['ylow'], region_coords['ylow'], region_coords['yhigh'], region_coords['yhigh'], region_coords['ylow'])))
  return(area / 1e6)  # Convert square meters to square kilometers
}
