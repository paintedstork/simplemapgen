#####################Algorithm to cluster locations####################################
library (tidyverse)
library(data.table)

dist_thresholds <- c(50, 100, 150, 200)

species <- readRDS(".\\data\\species.rds")



for (sp in species[-c(16,82)]) 
{
  locality_matrix <- readRDS(paste0(".\\locality_matrix\\locality_matrix_",sp,".rds"))
  
#  Begin create Clusters #######################################  
   n <- nrow(locality_matrix)
   
   for (dist in dist_thresholds)
   {
     cluster_numbers <- rep(0, n)  # Initialize cluster numbers
     next_cluster_number <- 1  # Start with cluster number 1
     
     for (loc in 1:n) {
       if (cluster_numbers[loc] == 0) {
         cluster_numbers[loc] <- next_cluster_number  # Assign cluster number
         
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
         
         next_cluster_number <- next_cluster_number + 1  # Increment cluster number
       }
     }
     
     # Create data frame with locality ID and cluster number
     cluster <- data.frame(
       LOCALITY.ID = rownames(locality_matrix),
       CLUSTER = cluster_numbers
     )
     
     colnames(cluster) <- c("LOCALITY.ID", "CLUSTER")
     cluster$LOCALITY.ID <- as.integer(cluster$LOCALITY.ID)     
     
     # We cant have polygons with two points. Plot them as just points.
     cluster <- cluster %>%
       group_by(CLUSTER) %>%
       mutate(CLUSTER = ifelse(n() < 3, NA, CLUSTER)) %>%
       ungroup()    
     
     print(paste("Saving cluster for",sp,dist))
     saveRDS(cluster, paste0(".\\clusters\\cluster_",sp,"_",dist,".rds"))
     cluster <- NULL
   }
   locality_matrix <- NULL
}

