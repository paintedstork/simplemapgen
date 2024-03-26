#####################File that reads the cluster data and convert them into polygons and points######################

library (tidyverse)
library(data.table)
library(adehabitatHR)
library(concaveman)
library(rgeos)
library(sf)
library(sp)
library(smoothr)
library(maptools)
library(zeallot)

plot <- 0
source("mapLoad.R")
source("speciesAttr.R")
source("config.R")


#speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")
#speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")
species <- read.csv("species.csv") 
species <- species[,1]

dist_thresholds <- c(20, 50, 100)
#dist_thresholds <- c(100)
dist_thresholds <- speciesattr$RESOLUTION %>% unique() %>% na.omit() %>% as.vector() %>% sort()

# Returns polygons made from the cluster as well as the reprogrammed cluster list
# You need to use the unpacking assignment operator from zeallot to obtain both
getClusterPolygons <- function (sp, dist = 100, season = "")
{
  filesuffix <- NULL
  if(season == "")
  {
    filesuffix <- paste0(".rds")
  }else
  {
    filesuffix <- paste0("_",season,".rds")
  }

  if (! ( file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist, filesuffix))))
  { 
    print(paste("No file for",sp,dist,season)) 
    next
  }
  
  cluster <- NULL
  if(file.exists(paste0(".\\cluster\\cluster_",sp,"_", dist, filesuffix)))
  {
    cluster <- readRDS(paste0(".\\cluster\\cluster_",sp,"_", dist, filesuffix))
  }
  spec_loc_table <- NULL
#  if(file.exists(paste0(".\\loctable\\loctable_",sp, filesuffix)))
#  {
#    spec_loc_table <- readRDS(paste0(".\\loctable\\loctable_",sp, filesuffix))
#  }
  
  clusterPolygonCount <- 0
  if(!is.null(cluster))
  {
    clusters <- cluster$CLUSTER %>% unique()
    
    clusterPolygons <- list()
    
    # Iterate over the clusters and create polygons, make it into a list.
    for (j in clusters)  
    {
      if(!is.na(j)) # && !is.null(spec_loc_table))
      {
        loc3 <- cluster %>% 
          filter (CLUSTER == j) %>% 
#          inner_join(spec_loc_table, by = c("LOCALITY.ID" = "LOCALITY_ID")) %>% 
          mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
                 LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>%
          filter (LATITUDE <= boundsConfig['None','ymax'] & LATITUDE >= boundsConfig['None','ymin']) %>%
          dplyr::select(LATITUDE, LONGITUDE) %>%
          distinct()
        
        if (nrow (loc3) > 0)
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
              
              # Concaveman can return invalid geometries. This need to be fixed by increasing concavity.
              concavity_start <- 1.0
              repeat 
              {
                concave <- concaveman(dat_sf, concavity = concavity_start,  length_threshold = 0.3)
                CH <- as_Spatial(concave)
                
                if (gIsValid(CH) || (concavity_start > 10)) {
                  break;
                }
                else
                {
                  concavity_start <- concavity_start + 0.1 
                }
              }                
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
              print(paste("poly_smooth created Invalid geometries for cluster",j))
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
  return ( list(clippedMergedPolygons, cluster))
}

#Cluster with NA are the points that did not fall in any cluster.
#The function will remove points that fell within the polygons from plotting
#This typically may not happen with concave polygons but can happen with convex polygons

getClusterNA <- function (clippedMergedPolygons, cluster)
{
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
  
  return (clusterNA)
}

# Go over all the species.
for (sp in species)
{
  for (dist in dist_thresholds)
  { 
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), defaultDistanceThreshold, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`)  
    if (!is.na(Seasonal$SEASONAL))
    {
      seasons <- strsplit(as.character(Seasonal$SEASONAL),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      
      for(season in seasons)
      {
        tryCatch ({
          
        c(clippedMergedPolygons, cluster) %<-% getClusterPolygons (sp, dist, season) 
        
        if(!is.null (clippedMergedPolygons))
        { 
          print(paste("Saving polygons for",sp,dist,season))
          valid <- gIsValid(clippedMergedPolygons)
          if (!all(valid)) {
            clippedMergedPolygons <- gBuffer(clippedMergedPolygons, width = 0)
          }
          saveRDS(clippedMergedPolygons, paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season, ".rds"))
        }

        ##############Saved all polygons###################################
        
        clusterNA <- getClusterNA (clippedMergedPolygons, cluster)
        
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
      
      # Till here the code is common for seasonal as well as aseasonal species
      # The only difference is that the code is executed per season
      
      seasonPriority <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASON.PRIORITY`) 
      
      if (!is.na(seasonPriority))
      {
        seasonPriority <- strsplit(as.character(seasonPriority),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      }
      else
      {
        seasonPriority <- defaultSeasonPriority
        seasonPriority <- seasonPriority[seasonPriority %in% unique(seasons)]
      }
        
      # Find intersecting polygons, find the difference, remove the lower priority one.
      for(i in 2:length(seasonPriority))
      {
        print (i)
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
              valid <- gIsValid(polygons1)
              if (!all(valid)) {
                polygons1 <- gBuffer(polygons1, width = 0)
              }
            }
          }
        }
        if(!is.null (polygons1))
        {
          print(paste("Saving polygons for",sp,dist,seasonPriority[i]))
          valid <- gIsValid(polygons1)
          if (!all(valid)) {
            polygons1 <- gBuffer(polygons1, width = 0)
          }
          
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

#        for(season1 in seasons)         
#        {
#          clippedMergedPolygons <- NULL
#          if(file.exists(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season1, ".rds")))
#          {
#            clippedMergedPolygons <- readRDS(paste0(".\\polygons\\polygons_",sp,"_",dist,"_", season1, ".rds"))
#          }
          
#          if( !is.null(clippedMergedPolygons) && (length(clippedMergedPolygons) > 0))
#          {
#            
#          }
#            
#        }
        
        
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
        
        c(clippedMergedPolygons, cluster) %<-% getClusterPolygons (sp, dist) 
        
        if(!is.null(clippedMergedPolygons))
        {
          print(paste("Saving polygons for",sp,dist))
          valid <- gIsValid(clippedMergedPolygons)
          if (!all(valid)) {
            clippedMergedPolygons <- gBuffer(clippedMergedPolygons, width = 0)
          }
          saveRDS(clippedMergedPolygons, paste0(".\\polygons\\polygons_",sp,"_",dist,".rds"))
        }
        
        ##############Saved all polygons###################################
        clusterNA <- getClusterNA (clippedMergedPolygons, cluster)

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
  area <- geosphere::areaPolygon(cbind(c(region_coords['xmin'], region_coords['xmax'], region_coords['xmax'], region_coords['xmin'], region_coords['xmin']),
                                       c(region_coords['ymin'], region_coords['ymax'], region_coords['ymax'], region_coords['ymax'], region_coords['ymin'])))
  return(area / 1e6)  # Convert square meters to square kilometers
}
