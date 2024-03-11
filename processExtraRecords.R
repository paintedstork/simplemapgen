library(tidyverse)
library(dplyr)

source("speciesAttr.R")
extradata <- read.csv("Extrarecords.csv")

data1 <- extradata %>% mutate (LATITUDE = 0.1 * round(10 * LATITUDE),
                          LONGITUDE = 0.1 * round (10 * LONGITUDE), 
                          LATITUDE_R = deg2rad (LATITUDE),
                          LONGITUDE_R = deg2rad (LONGITUDE),
                          LOCALITY_ID = LONGITUDE*100000 + LATITUDE*10) %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE, LATITUDE_R, LONGITUDE_R) %>%
  distinct (LOCALITY_ID, .keep_all = TRUE)

loc <- read.csv("locality.csv")
data1 <- anti_join(data1, loc, by = "LOCALITY_ID")

write.csv(data1, paste0("Extralocality",".csv"), row.names=FALSE)

records <- inner_join(extradata, speciesattr, by = c("COMMON.NAME" = "ENGLISH.NAME"))

species <- records$SCIENTIFIC.NAME %>% unique()

species <- read.csv("Extraspecies.csv") 
species <- species[,1]

for (sp in species) 
{
  # Backup original data in orig. Always use orig to combine records. 
  if (!file.exists(paste0(".\\data\\data_orig_", sp,".rds"))) 
  { 
    data <- readRDS(paste0(".\\data\\data_",sp, ".rds"))
    saveRDS(data, paste0(".\\data\\data_orig_",sp, ".rds"))
  }
  else
  {
    if (!file.exists(paste0(".\\data\\data_", sp,".rds"))) 
    {
      print(paste("No file for",sp))
      next;
    }
    data <- readRDS(paste0(".\\data\\data_",sp, ".rds"))
  }
  
  
  records <- records %>% 
                filter (SCIENTIFIC.NAME == sp) %>%  
                     mutate ( 
                              CATEGORY = "species",
                              APPROVED = as.integer(1),
                              OBSERVATION.DATE = paste0(as.Date(paste0(YEAR,"-",MONTH,"-",15)))) %>%
                        dplyr::select (CATEGORY, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, OBSERVATION.DATE, APPROVED)
  
  records <- rbind (data, records) %>% 
              distinct(LATITUDE, LONGITUDE, OBSERVATION.DATE, .keep_all = TRUE)
} 

saveRDS(records, paste0(".\\data\\data_",sp, ".rds"))

