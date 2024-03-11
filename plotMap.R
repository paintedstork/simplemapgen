library (tidyverse)
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
library(parallel)
library(doParallel)
library(foreach)
library(ggpolypath)
library(sfheaders)

plot <- 1
source("mapLoad.R")
source("speciesAttr.R")



map_colors <- c("W" = "darkblue", "S" = "red", "R" = "darkgreen", "P" = "yellow")

#num_cores <- detectCores()

##cl <- makeCluster(num_cores)
#registerDoParallel(cl)


#speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")

species <- speciesattr$SCIENTIFIC.NAME %>% sort()

#species <- readRDS(".\\data\\species.rds")
#species <- read.csv("species.csv") 
#species <- species[,1]

dist_thresholds <- c(50, 60, 100)

for (dist in dist_thresholds)
{
  # Go over all the species.
  for (sp in species)
  {
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), 100, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    ClipRegion <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`CLIP.REGION`)
    
    bounds <- boundsConfig[ClipRegion$CLIP.REGION,]
    
    
    gdisplay <- ggplot()
    gdisplay <- gdisplay + geom_sf(data=sea, fill = "lightblue")  
    gdisplay <- gdisplay + geom_sf(data=pak, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=nep, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=bhu, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=ban, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=lak, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=mal, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=irn, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=mmr, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=uzb, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=tha, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=chn, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=tjk, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=tkm, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=afg, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=ind, fill = "white")  
    gdisplay <- gdisplay + geom_sf(data=sta, linetype = "dashed", fill = "transparent")
    gdisplay <- gdisplay + coord_sf(xlim = c(bounds['xlow'], bounds['xhigh']), ylim = c(bounds['ylow'], bounds['yhigh'])) 
    gdisplay <- gdisplay + theme_void() +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`) 
    
    if (!is.na(Seasonal$SEASONAL))
    {
      seasons <- strsplit(as.character(Seasonal$SEASONAL),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      
      gdisplaya <- gdisplay
      for(season in seasons)
      { 
        if (! ( file.exists(paste0(".\\polygons\\polygons_",sp,"_", dist,"_", season, ".rds")) | 
                file.exists(paste0(".\\points\\points_",sp,"_", dist,"_", season, ".rds")))) 
        { 
          print(paste("No file for",sp, dist)) 
          next
        }
        
        
        #  End create Locality Matrix#######################################  
        polygons <- NULL
        if (file.exists(paste0(".\\polygons\\polygons_",sp,"_", dist,"_", season, ".rds"))) 
        {
          polygons <- readRDS(paste0(".\\polygons\\polygons_",sp,"_", dist,"_", season, ".rds"))
        }
        
        points   <- NULL
        if (file.exists(paste0(".\\points\\points_",sp,"_", dist,"_", season, ".rds"))) 
        {
          points   <- readRDS(paste0(".\\points\\points_",sp,"_", dist,"_", season, ".rds"))
        }
        
        gdisplays <- gdisplay
        if(!is.null(polygons))
        {
          gdisplaya <- gdisplaya +
            geom_polypath(data= fortify(polygons), aes(x=long, y=lat, group=group), fill = map_colors[season], alpha=0.5, linewidth = 0) 
          gdisplays <- gdisplays +
            geom_polypath(data= fortify(polygons), aes(x=long, y=lat, group=group), fill = map_colors[season], alpha=0.5, linewidth = 0) 
        }
        
        if(!is.null(points))
        {
          gdisplaya <- gdisplaya + 
            geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), colour = map_colors[season], shape = 1, fill = "transparent", size = 3) + 
            geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = 15, color = "black", fill = "black", size = 1) 
          gdisplays <- gdisplays + 
            geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), colour = map_colors[season], shape = 1, fill = "transparent", size = 3) + 
            geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = 15, color = "black", fill = "black", size = 1) 
        }
        
        cname <- paste0 (speciesattr[speciesattr$SCIENTIFIC.NAME == sp,]$ENGLISH.NAME, " (", season,")")
        
        gdisplays <- gdisplays + 
          theme(legend.position = "none") +
          ggtitle(cname) +
          theme(plot.title = element_text(color = "black", size = 12, hjust = 1, vjust = 0.5, family = "Times New Roman"),
                panel.border = element_rect(color = "black", fill = NA, size = 1),
                plot.margin = margin(t = 80, r = 10, b = 10, l = 10))      
        if(!is.null(gdisplays))
        {
          print(paste("Saving map",sp,dist,season))
          ggsave(filename = paste0(".\\map\\map_",sp,"_", dist,"_",season,".jpg"), 
                 plot = gdisplays,
                 width = 7, 
                 height = 7, 
                 units = "in")
          ggsave(filename = paste0(".\\map_svg\\map_",sp,"_", dist,"_",season,".svg"), 
                 plot = gdisplays,
                 width = 7, 
                 height = 7, 
                 units = "in")
        }
        gdisplays <- NULL
      }
      gdisplay <- gdisplaya
      gdisplaya <- NULL
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
      }
        
      points <- NULL
      if (file.exists(paste0(".\\points\\points_",sp,"_", dist, ".rds")))
      {
        points   <- readRDS(paste0(".\\points\\points_",sp,"_", dist, ".rds"))
      }
    

      
      Migration <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`MIGRATION`) %>% as.character()
      
  #    if( (Migration != "Resident") && (Migration != "Winter Migrant")) next; 
          
      fillColour <- ifelse (Migration == "Winter Migrant", "darkblue", "darkgreen")
  
      
      if(!is.null(polygons))
      {
        gdisplay <- gdisplay +
          geom_polygon(data= fortify(polygons), aes(x=long, y=lat, group=group), fill = fillColour, alpha=0.5, linewidth = 0) 
      }
      
      if(!is.null(points))
      {
        gdisplay <- gdisplay + 
          geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), color = "darkgreen", shape = 1, fill = "transparent", size = 3) + 
          geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = 15, color = "black", fill = "black", size = 1) 
      }
    }
    
    cname <- speciesattr[speciesattr$SCIENTIFIC.NAME == sp,]$ENGLISH.NAME
    
    gdisplay <- gdisplay + 
      theme(legend.position = "none") +
      ggtitle(cname) +
      theme(plot.title = element_text(color = "black", size = 12, hjust = 1, vjust = 0.5, family = "Times New Roman"),
            panel.border = element_rect(color = "black", fill = NA, size = 1),
            plot.margin = margin(t = 80, r = 10, b = 10, l = 10))      
    
    if(!is.null(gdisplay))
    {
      print(paste("Saving map",sp))
      ggsave(filename = paste0(".\\map\\map_",sp,"_", dist,".jpg"), 
             plot = gdisplay,
             width = 7, 
             height = 7, 
             units = "in")
#      ggsave(filename = paste0(".\\map_svg\\map_",sp,"_", dist,".svg"), 
#             plot = gdisplay,
#             width = 7, 
#             height = 7, 
#             units = "in")
    }
    
    
    #   Save space
    polygons <- NULL
    points <- NULL
    gdisplay <- NULL
    spec_loc_table <- NULL        
  }
}




  
