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
library(sp)

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

ind <- st_read("India.shp")
pak <- st_read("Pakistan.shp")
nep <- st_read("Nepal.shp")
bhu <- st_read("Bhutan.shp")
ban <- st_read("Bangladesh.shp")
lak <- st_read("SriLanka.shp")
mal <- st_read("Maldives.shp")
sta <- st_read("India States.shp")
cap <- st_read("Capitals.shp")

# Clipping polygon
ind_sub <- st_read("ind_sub_clean_simplified_valid.shp")
ind_sub <- st_make_valid(ind_sub)

speciesattr <- read.csv2(paste0("SoIB_main_09062023.csv"), sep=",") %>% 
  dplyr::select('eBird.Scientific.Name.2022', 'India.Endemic','Clip.Region')

colnames(speciesattr) <- c("SCIENTIFIC.NAME", "ENDEMIC", "CLIP.REGION")

speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")

ggplot_list <- list()



# Go over all the species.
for (sp in species[-c(16,82)]) 
#foreach(i = 1:length(species[1:16]), .packages = c("tidyvers", "rgeos")) %dopar% 
{
  
  #  End create Locality Matrix#######################################  
  cluster <- readRDS(paste0(".\\clusters\\cluster_",sp,"_100.rds"))
  spec_loc_table <- readRDS(paste0(".\\loc_table\\loc_table_",sp,".rds"))
  
  # Create a loc3 with only the latitude and longitude of the localities in the cluster dataframe  
  loc3 <- cluster %>% 
    inner_join(spec_loc_table, by = c("LOCALITY.ID" = "LOCALITY_ID")) %>% 
    mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
           LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>%
    dplyr::select(LATITUDE, LONGITUDE) %>%
    distinct()
  
  if(nrow(loc3) < 5)
  {
    loc3 <- loc3 %>% 
      add_row(LATITUDE = loc3$LATITUDE[1] - 0.1, LONGITUDE = loc3$LONGITUDE[1] - 0.1) %>% 
      add_row(LATITUDE = loc3$LATITUDE[1] - 0.1, LONGITUDE = loc3$LONGITUDE[1] + 0.1) %>% 
      add_row(LATITUDE = loc3$LATITUDE[1] + 0.1, LONGITUDE = loc3$LONGITUDE[1] - 0.1) %>% 
      add_row(LATITUDE = loc3$LATITUDE[1] + 0.1, LONGITUDE = loc3$LONGITUDE[1] + 0.1)
  }
  # Convert it into a spatial dataframe
  sp::coordinates(loc3) <- ~LONGITUDE+LATITUDE
  
  #Calculate MCP
  mcp_species <- mcp(loc3, percent=100, unout="km2")
  
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
  
  clusters <- cluster$CLUSTER %>% unique()
  
  # Iterate over the clusters
  for (j in clusters)  
  {
    if(!is.na(clusters[j]))
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

        # Calculate the total area of the spatial polygon
#        total_area <- gArea(r_poly_smooth)
        
        # Identify areas within the polygon that do not have any points and are larger than 10% of the total area
#        holes <- gDifference(r_poly_smooth, loc3)
#        holes <- holes[gArea(holes) > 0.1 * total_area]
        
        # Subtract the holes from the original polygon
#        new_polygon <- gDifference(r_poly_smooth, holes)
        
        
        # Clipping the polygon to the landmass for land birds
        r_poly_smooth_sf <- st_as_sf(r_poly_smooth, coords = c("long", "lat"))
        st_crs(r_poly_smooth_sf) <- st_crs(ind)
#        clipped_r_poly_smooth <- st_transform(clipped_r_poly_smooth, crs = st_crs(ind))
        clipped_r_poly_smooth <- st_intersection(r_poly_smooth_sf, ind)
        clipped_r_poly_smooth <- as_Spatial(clipped_r_poly_smooth)

        gdisplay <- gdisplay +
          geom_polygon(data=fortify(clipped_r_poly_smooth), aes(x=long, y=lat, group=group), fill = "darkgreen", alpha=0.5, linewidth = 0) 
      }
      else
      {
        cluster <- cluster %>%
                      mutate(CLUSTER = ifelse(CLUSTER == j, NA, CLUSTER))
      }
    }
  }
  
  
  clusterNA <- cluster %>% 
    filter (is.na(CLUSTER)) %>%
    mutate(LATITUDE = (LOCALITY.ID - 10000 * as.integer(LOCALITY.ID/10000))/10,
           LONGITUDE = as.integer(LOCALITY.ID/10000)/10) %>% 
    dplyr::select(LATITUDE, LONGITUDE)
  
  gdisplay <- gdisplay + 
    geom_point(data = fortify(clusterNA), aes(x=LONGITUDE, y=LATITUDE), shape = 1, color="darkgreen", fill = "transparent", size = 3) + 
    geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = 15, color = "black", fill = "black", size = 1) 
  
  
  gdisplay <- gdisplay + 
    theme(legend.position = "none")
  
  
  print(paste("Generating display",sp))
  ggplot_list[[sp]] <- gdisplay
  #    ggsave(filename = paste0(".\\maps\\map_", names(ggplot_list)[1],".jpg"), plot = ggplot_list[[1]])
  
  #   Save space
  gdisplay <- NULL
  loc3 <- NULL
  cluster <- NULL
  spec_loc_table <- NULL
  #   stopCluster(cl)
}


for (i in 1: length(ggplot_list))
{
  print(paste("Saving map",names(ggplot_list)[i]))
  ggsave(filename = paste0(".\\maps\\map_", names(ggplot_list)[i],"_100.jpg"), 
         plot = ggplot_list[[i]],
         width = 7, 
         height = 7, 
         units = "in")
}
ggplot_list <- NULL



