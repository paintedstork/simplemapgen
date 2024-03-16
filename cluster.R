#####################Algorithm to cluster locations####################################
library (tidyverse)
library(data.table)

source("speciesAttr.R")
source("config.R")

dist_thresholds <- c(20, 50, 100)
dist_thresholds <- speciesattr$RESOLUTION %>% unique() %>% na.omit() %>% as.vector() %>% sort()

species <- readRDS(".\\data\\species.rds")
#speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")
species <- speciesattr$SCIENTIFIC.NAME
species <- read.csv("species.csv") 
species <- species[,1]

createCluster <- function (sp, dist, season = "")
{
  filesuffix <- NULL
  if(season == "")
  {
    filesuffix <- paste0(".rds")
  }else
  {
    filesuffix <- paste0("_",season,".rds")
  }
 
  print(sp)
  if (!file.exists(paste0(".\\localitymatrix\\localitymatrix_",sp,filesuffix))) 
  { 
    print(paste("No file for",sp)) 
    return (NULL)
  }
  
  locality_matrix <- readRDS(paste0(".\\localitymatrix\\localitymatrix_",sp,filesuffix))
  
  
  #  Begin create Clusters #######################################  
  n <- nrow(locality_matrix)
  
  # Make all NA as large value 
  locality_matrix[is.na(locality_matrix)] <- 1000
  
  
  cluster_numbers <- rep(0, n)  # Initialize cluster numbers
  next_cluster_number <- 1  # Start with cluster number 1

  # Create data frame with locality ID and cluster number
  cluster <- data.frame(
    LOCALITY.ID = rownames(locality_matrix),
    CLUSTER = NA
  )
  
  PassageRange <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`PASSAGE.RANGE`) %>% as.vector()
  
  # Cluster passage range only if configured so
  if( (season !="P") || 
      (season == "P" && !is.na(PassageRange$PASSAGE.RANGE)))
  {
    for (loc in 1:n) {
      if (cluster_numbers[loc] == 0) {
        cluster_numbers[loc] <- next_cluster_number  # Assign cluster number
        
        DotMapOnly <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`DOTMAP.ONLY`) 
        
        if (DotMapOnly != "X")
        {
          # Iterate over locations within the same cluster
          locations_to_process <- loc
          while (length(locations_to_process) > 0) {
            current_location <- locations_to_process[1]
            locations_to_process <- locations_to_process[-1]
            
            # Find nearby locations within the distance threshold
            nearby_locations <- which(locality_matrix[current_location, ] <= dist)
            
            # Assign the same cluster number to nearby locations if not already assigned
            unassigned_nearby <- nearby_locations[cluster_numbers[nearby_locations] == 0]
            cluster_numbers[unassigned_nearby] <- next_cluster_number
            
            # Add unassigned nearby locations to the list for processing
            locations_to_process <- c(locations_to_process, unassigned_nearby)
          }
        }
        
        next_cluster_number <- next_cluster_number + 1  # Increment cluster number
      }
    }
    
    # Create data frame with locality ID and cluster number
    cluster <- data.frame(
      LOCALITY.ID = rownames(locality_matrix),
      CLUSTER = cluster_numbers
    )
    
    # We cant have polygons with two points. Plot them as just points.
    cluster <- cluster %>%
      group_by(CLUSTER) %>%
      mutate(CLUSTER = ifelse(n() < 3, NA, CLUSTER)) %>%
      ungroup()    
  }
  
  cluster$LOCALITY.ID <- as.integer(cluster$LOCALITY.ID)     
  return (cluster)  
}

# Main iteration of clustering algorithm
for (dist in dist_thresholds)
{
  for (sp in species) 
  {
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`)  
    
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), defaultDistanceThreshold, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    if (!is.na(Seasonal$SEASONAL))
    {
      seasons <- strsplit(as.character(Seasonal$SEASONAL),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      
      for(season in seasons)
      {
        cluster <- createCluster (sp, dist, season)

        if(!is.null(cluster))
        {
          print(paste("Saving cluster for",sp,dist,season))
          saveRDS(cluster, paste0(".\\cluster\\cluster_",sp,"_",dist,"_", season,".rds"))
        }
        cluster <- NULL
        locality_matrix <- NULL
      }
      seasons <- NULL
    }
    else
    {
       cluster <- createCluster (sp, dist)
       
       if(!is.null(cluster))
       {
         print(paste("Saving cluster for",sp,dist))
         saveRDS(cluster, paste0(".\\cluster\\cluster_",sp,"_",dist,".rds"))
       }
       cluster <- NULL
       locality_matrix <- NULL
    }
  }
}
