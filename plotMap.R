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
library(ragg)

plot <- 1
source("mapLoad.R")
source("config.R")
source("speciesAttr.R")
source("colorConvert.R")

#num_cores <- detectCores()
##cl <- makeCluster(num_cores)
#registerDoParallel(cl)


#speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")
#speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")
species <- read.csv("species.csv") 
species <- species[,1]

#dist_thresholds <- c(20, 50, 100)
dist_thresholds <- speciesattr$RESOLUTION %>% unique() %>% na.omit() %>% as.vector() %>% sort()

adjustForMercator <- function(bounds, pixelcorrection) {
  # Convert bounds to Mercator projection
  merc_bounds <- data.frame(lon = c(bounds[1], bounds[2]), lat = c(bounds[3], bounds[4])) %>% 
                    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                      st_transform(crs = 3857) %>%
                        st_bbox() %>%
                          unlist()
  
  print(merc_bounds)
  # Adjust bounds for Mercator on the left and right
  merc_bounds[1] <- merc_bounds[1] + pixelcorrection
  merc_bounds[3] <- merc_bounds[3] - pixelcorrection
  print(merc_bounds)
  
  # Convert adjusted bounds back to WGS84
  final_bounds <- data.frame(lon = c(merc_bounds[1], merc_bounds[3]), lat = c(merc_bounds[2], merc_bounds[4])) %>% 
                    st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
                      st_transform(crs = 4326) %>%
                        st_bbox() %>%
                          unlist()
  print(final_bounds)
  # This conversion is needed to fix the black-bars that will appear on the sides due to earth's curvature
  return(final_bounds)
}

if(useCmYk)
{
  map_colors <- rgb_colors
  sea_color <- c(13, 0, 2, 0) %>% CMYK2RGB() %>% rgbToHex()
}

for (sp in species)
{
  for (dist in dist_thresholds)
  {
  # Go over all the species.
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), defaultDistanceThreshold, as.integer(resolution))
    
    if(resolution != dist) next; 
    
    ClipRegion <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`CLIP.REGION`)
    
    bounds          <- boundsConfig[ClipRegion$CLIP.REGION,]
    
    if(showBorder != 1)
    {
      pixelcorrection <- boundsConfig[ClipRegion$CLIP.REGION,'pixelcorrection']
      bounds <- adjustForMercator(bounds, pixelcorrection)
      print(bounds)
    }
    
    
    gdisplay <- ggplot()
    gdisplay <- gdisplay + geom_sf(data=sea, fill = sea_color, linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=irn, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=mmr, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=uzb, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=tha, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=chn, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=tjk, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=tkm, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=afg, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=pak, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=nep, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=bhu, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=ban, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=lak, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=mal, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=ind, fill = "white", linewidth = polygonLineThickness)  
    gdisplay <- gdisplay + geom_sf(data=sta, linetype = "dashed", fill = "transparent", linewidth = polygonLineThickness)
    gdisplay <- gdisplay + coord_sf(xlim = c(bounds['xmin'], bounds['xmax']), ylim = c(bounds['ymin'], bounds['ymax'])) 
    gdisplay <- gdisplay + theme_void() +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    gdisplay <- gdisplay + 
      geom_point(data = cap, aes(x=LONGITUDE, y=LATITUDE), shape = capitalIconShape, color = capitalIconFillColor, fill = capitalIconFillColor, size = capitalIconSize) 
    
    Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`) 

    if (!is.na(Seasonal$SEASONAL))
    {
      seasons <- strsplit(as.character(Seasonal$SEASONAL),":") %>% as.data.frame() %>% unique() %>% as.list() %>% unlist()
      
      gdisplaya <- gdisplay  #Variable holding plots across all seasons
      
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
        
		    gdisplays <- gdisplay  #Variable holding plot for one season
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
            geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), colour = map_colors[season], shape = pointIconShape, fill = pointIconFillColor, size = pointIconSize, stroke = pointIconStroke)  
          gdisplays <- gdisplays + 
            geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), colour = map_colors[season], shape = pointIconShape, fill = pointIconFillColor, size = pointIconSize, stroke = pointIconStroke)  
        }
		
    		gdisplays <- gdisplays + 
    		              theme(legend.position = "none") 
		  
        cname = ""
        if (showSpeciesName)
        {
          cname <- paste0 (speciesattr[speciesattr$SCIENTIFIC.NAME == sp,]$ENGLISH.NAME, " (", season,")")
          gdisplays <- gdisplays + 
                      ggtitle(cname) +
                      theme(plot.title = element_text(color = speciesNameColor, size = speciesNameFontSize, hjust = 1, vjust = 0.5, family = "Times New Roman"))
        }
        
        if(showBorder)
        {
          gdisplays <- gdisplays + 
            theme(panel.border = element_rect(color = panelBorderColor, fill = NA, size = panelBorderSize),
                  plot.margin = margin(t = marginTop, r = marginRight, b = marginBottom, l = marginLeft))      
        }
        else
        {
          gdisplays <- gdisplays + 
            theme(panel.border = NULL,
                  plot.margin = NULL)      
        }
        
        
        if(!is.null(gdisplays) & generateSeasonalMaps)
        {
          if(generateJPG)
          {
            print(paste("Saving map",sp,dist,season, jpgWidth, jpgHeight))
            ggsave(filename = paste0(".\\map\\map_",sp,"_", dist,"_",season,".jpg"), 
                  plot = gdisplays,
                  width = jpgWidth, 
                  height = jpgHeight, 
                  units = jpgUnit,
                  dpi = jpgdpi)

          }
          
          if(generateTIFF)
          {
            file_path <- paste0(".\\map_tiff\\map_", sp, "_", dist,"_",season,".tiff")
            print(file_path)
            
            if (file.exists(file_path)) {
              # Remove the existing file
              file.remove(file_path)
            }
            
            ragg::agg_tiff(file_path,
                           tiffWidth, 
                           tiffHeight,
                           units = tiffUnit,
                           background = "black",
                           res=tiffdpi)  
            print(gdisplay)
            # Close the TIFF device
            dev.off()
            
          }
          
          if(generateSVG)
          {
            ggsave(filename = paste0(".\\map_svg\\map_",sp,"_", dist,"_",season,".svg"), 
                   plot = gdisplays,
                   width = svgWidthinInches, 
                   height = svgHeightinInches, 
                   units = "in")
          }
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
      
      season = switch (Migration, 
                      "Summer Migrant" = 'S', 
                      "Winter Migrant" = 'W',
                      "Passage Migrant" = 'P',
                      "Resident" = 'R',
                      )
      
      if(!is.null(polygons))
      {
        gdisplay <- gdisplay +
          geom_polygon(data= fortify(polygons), aes(x=long, y=lat, group=group), fill = map_colors[season], alpha=0.5, linewidth = 0) 
      }
      
      if(!is.null(points))
      {
        gdisplay <- gdisplay + 
          geom_point(data = fortify(points), aes(x=LONGITUDE, y=LATITUDE), color = map_colors[season], shape = pointIconShape, fill = pointIconFillColor, size = pointIconSize, stroke = pointIconStroke) 
      }
      gdisplay <- gdisplay 
    }

    gdisplay <- gdisplay + 
      theme(legend.position = "none")
      
    cname = ""
    if (showSpeciesName)
    {
      cname <- speciesattr[speciesattr$SCIENTIFIC.NAME == sp,]$ENGLISH.NAME
      gdisplay <- gdisplay + 
        ggtitle(cname) +
        theme(plot.title = element_text(color = speciesNameColor, size = speciesNameFontSize, hjust = 1, vjust = 0.5, family = "Times New Roman"))      
    }

    if(showBorder)
    {
      gdisplay <- gdisplay + 
        theme(panel.border = element_rect(color = panelBorderColor, fill = NA, size = panelBorderSize),
              plot.margin = margin(t = marginTop, r = marginRight, b = marginBottom, l = marginLeft))      
    }
    else
    {
      gdisplay <- gdisplay + 
        theme(panel.border = NULL,
              plot.margin = NULL)      
    }
    
    if(!is.null(gdisplay))
    {
      if(generateJPG)
      {
        print(paste("Saving map",sp, jpgWidth, jpgHeight))
        ggsave(filename = paste0(".\\map\\map_",sp,"_", dist,".jpg"), 
               plot = gdisplay,
               width = jpgWidth, 
               height = jpgHeight, 
               units = jpgUnit,
               dpi = jpgdpi)
      }
      
      if (generateTIFF)
      {
        file_path <- paste0(".\\map_tiff\\map_", sp, "_", dist, ".tiff")
        print(file_path)
        
        if (file.exists(file_path)) {
          # Remove the existing file
          file.remove(file_path)
        }
        
        ragg::agg_tiff(file_path,
                       tiffWidth, 
                       tiffHeight,
                       units = tiffUnit,
                       background = "black",
                       res=tiffdpi)  
        
        print(gdisplay)
        # Close the TIFF device
        dev.off()
      }
      if (generateSVG)
      {
        ggsave(filename = paste0(".\\map_svg\\map_",sp,"_", dist,".svg"), 
               plot = gdisplay,
               width = svgWidthinInches, 
               height = svgHeightinInches, 
               units = "in")
      }
    }
    
    
    #   Save space
    polygons <- NULL
    points <- NULL
    gdisplay <- NULL
    spec_loc_table <- NULL        
  }
}




  
