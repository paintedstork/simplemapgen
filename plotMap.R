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

source("mapProcess.R")

boundsConfig <- matrix(
  c(61, 97, 20, 37, #North
    61, 97, 0,  23, #South
    86, 97, 20, 37, #Northeast
    61, 77, 20, 37, #Northwest
    89, 95, 6,  15, #Andaman
    72, 78, 7,  14, #Ghats
    61, 97, 0,  37), #None
nrow = 4,
  dimnames = list(
                  c("xlow", "xhigh", "ylow", "yhigh"),
                  c("North", "South", "Northeast", "Northwest", "Andaman", "Ghats", "None"))
) %>% as.data.frame() %>% t()



#num_cores <- detectCores()

##cl <- makeCluster(num_cores)
#registerDoParallel(cl)


speciesattr <- read.csv2(paste0("SoIB_main_09062023.csv"), sep=",") %>% 
  dplyr::select('eBird.Scientific.Name.2022', 'India.Endemic','Clip.Region')

colnames(speciesattr) <- c("SCIENTIFIC.NAME", "ENDEMIC", "CLIP.REGION")

speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")

dist <- 100
ggplot_list <- list()
# Go over all the species.
for (sp in species[c(78)]) 
{
  
  #  End create Locality Matrix#######################################  
  polygons <- readRDS(paste0(".\\shapes\\polygons_",sp,"_", dist, ".rds"))
  points   <- readRDS(paste0(".\\shapes\\points_",sp,"_", dist, ".rds"))

  ClipRegion <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`CLIP.REGION`)

  bounds <- boundsConfig[ClipRegion$CLIP.REGION,]
  

  gdisplay <- ggplot()
  gdisplay <- gdisplay + geom_sf(data=pak, fill = "transparent")  
  gdisplay <- gdisplay + geom_sf(data=nep, fill = "transparent")  
  gdisplay <- gdisplay + geom_sf(data=bhu, fill = "transparent")  
  gdisplay <- gdisplay + geom_sf(data=ban, fill = "transparent")  
  gdisplay <- gdisplay + geom_sf(data=lak, fill = "transparent")  
  gdisplay <- gdisplay + geom_sf(data=mal, fill = "transparent")  
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
  

  gdisplay <- gdisplay +
    geom_polygon(data=fortify(polygons), aes(x=long, y=lat, group=group), fill = "darkgreen", alpha=0.5, linewidth = 0) 

  gdisplay <- gdisplay + 
    geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), shape = 1, color="darkgreen", fill = "transparent", size = 3) + 
    geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = 15, color = "black", fill = "black", size = 1) 
  
  gdisplay <- gdisplay + 
    theme(legend.position = "none")
  
  print(paste("Generating display",sp))
  ggplot_list[[sp]] <- gdisplay

  #   Save space
  gdisplay <- NULL
  spec_loc_table <- NULL
}


for (i in 1: length(ggplot_list))
{
  print(paste("Saving map",names(ggplot_list)[i]))
  ggsave(filename = paste0(".\\maps\\map_", names(ggplot_list)[i],"_", dist,".jpg"), 
         plot = ggplot_list[[i]],
         width = 7, 
         height = 7, 
         units = "in")
}
ggplot_list <- NULL



