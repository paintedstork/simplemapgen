library (tidyverse)
library (rgdal)
source("speciesAttr.R")

speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

species <- speciesattr$SCIENTIFIC.NAME %>% sort()

dist_thresholds <- c(50, 100)

# Define the output directory and shapefile name
output_dir <- "shapefile"

for (dist in dist_thresholds)
{
  for (sp in species)
  {
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), 100, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`) %>% as.character()
  
    if (Seasonal != "")
    {
      print(paste(sp,"Got a seasonal endemic - skip"))
    }
    else
    {
      if (! ( file.exists(paste0(".\\polygons\\polygons_",sp,"_", dist, ".rds")) | 
              file.exists(paste0(".\\points\\points_",sp,"_", dist, ".rds")))) 
      { 
        print(paste("No file for",sp, dist)) 
        next
      }
      
      polygons <- NULL
      if (file.exists(paste0(".\\polygons\\polygons_",sp,"_", dist, ".rds"))) 
      {
        polygons <- readRDS(paste0(".\\polygons\\polygons_",sp,"_", dist, ".rds"))
        
        # Write the SpatialPolygonsDataFrame to a shapefile
        print(paste0(sp,"_",dist))
        if(inherits(polygons, "Spatial"))
        {
          writeOGR(polygons, dsn=output_dir, layer=paste0(sp,"_",dist), driver="ESRI Shapefile", overwrite_layer = TRUE) 
          zip(zipfile = paste0(output_dir,"shape_",sp,"_",dist,".zip"), 
              files = list.files(path = output_dir, pattern = paste0(sp,"_",dist,".")),
              zip = "C:\\Program Files\\WinZip\\WinZip64")
        }
      }
      
      
      points <- NULL
      if (file.exists(paste0(".\\points\\points_",sp,"_", dist, ".rds")))
      {
        points   <- readRDS(paste0(".\\points\\points_",sp,"_", dist, ".rds"))
      }
    }
  }
}
   

