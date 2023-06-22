#####################File that reads the cluster data and convert them into polygons and points######################


library (tidyverse)

# Optimize the libraries
library (REdaS)
library (factoextra)
library(rgdal)
library(rgeos)
library(adehabitatHR)
library(rgeos)
library(ggmap)
library(ggplot2)
library(terra)
library(sf)
library(units)
library(smoothr)
library(data.table)

source("mapProcess.R")


speciesattr <- read.csv2(paste0("SoIB_main_09062023.csv"), sep=",") %>% 
  dplyr::select('eBird.Scientific.Name.2022', 'India.Endemic','Clip.Region')

colnames(speciesattr) <- c("SCIENTIFIC.NAME", "ENDEMIC", "CLIP.REGION")

speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")

dist <- 50
# Go over all the species.
for (sp in species[c(78)]) 
{
  #  End create Locality Matrix#######################################  
  cluster <- readRDS(paste0(".\\clusters\\cluster_",sp,"_", dist, ".rds"))
  spec_loc_table <- readRDS(paste0(".\\loc_table\\loc_table_",sp,".rds"))
  
  clusters <- cluster$CLUSTER %>% unique()
  
  clusterPolygons <- list()
  clusterPolygonCount <- 1
  
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
        dplyr::select(LATITUDE, LONGITUDE) %>%
        distinct()
      
      sp::coordinates(loc3) <- ~LONGITUDE+LATITUDE
      
      CH = gConvexHull(loc3)
      if("polygons" %in% slotNames(CH))
      {
        r_poly_smooth <- smooth(CH, method = "chaikin")
      
        # Assigning unique ID
        r_poly_smooth@polygons[[1]]@ID <- as.character(clusterPolygonCount)
        
        clusterPolygons[[clusterPolygonCount]] <- r_poly_smooth
        clusterPolygonCount <- clusterPolygonCount + 1
      }
      else
      { # Remove these points from the cluster as they lie in a straight line
        cluster <- cluster %>%
          mutate(CLUSTER = ifelse(CLUSTER == j, NA, CLUSTER))
      }
    }
  }
  
  # Do not pipe these code. These code can break as packages are routinely deprecated and would need debuggin
  # Merging overlapping polygons
  rawPolygons <- lapply(clusterPolygons, function(x) x@polygons[[1]])
  spatialPolygons <- SpatialPolygons(rawPolygons)
  mergedPolygons <- gUnaryUnion(spatialPolygons)
  
  # Clipping for country/land boundaries
  mergedPolygons_sf <- st_as_sf(mergedPolygons, coords = c("long", "lat"))
  st_crs(mergedPolygons_sf) <- st_crs(ind)
  clippedMergedPolygons_sf <- st_intersection(mergedPolygons_sf, ind)
  # Add clips for all other countries        
  clippedMergedPolygons <- as_Spatial(clippedMergedPolygons_sf)
  
  print(paste("Saving polygons for",sp,dist))
  saveRDS(clippedMergedPolygons, paste0(".\\shapes\\polygons_",sp,"_",dist,".rds"))
  ##############Saved all polygons###################################
  
  clusterNA <- cluster %>% 
    filter (is.na(CLUSTER)) %>%
    mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
           LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>% 
    dplyr::select(LATITUDE, LONGITUDE)
  
  spClusterNA <- clusterNA
  sp::coordinates(spClusterNA) <- ~LONGITUDE+LATITUDE
  
  spCluster <- SpatialPolygons(clippedMergedPolygons@polygons)
  proj4string(spClusterNA) <- proj4string(spCluster)
  
  points_within_polygons <- over( spClusterNA, spCluster)
  
  clusterNA <- cbind(clusterNA, points_within_polygons)
  
  # All NA from the over function are non-overlapping points to be plotted
  clusterNA <- clusterNA[is.na(clusterNA$points_within_polygons), ]
  
  print(paste("Saving points for",sp,dist))
  saveRDS(clusterNA, paste0(".\\shapes\\points_",sp,"_",dist,".rds"))
  
  #   Save space
  loc3 <- NULL
  cluster <- NULL
  clusterNA <- NULL
  spec_loc_table <- NULL
  clippedMergedPolygons <- NULL
}
