#####################Algorithm to prepare locality matrix####################################
library (tidyverse)
library(data.table)
library(pryr)

highlocalityspecies <- read.csv("bigspecies.csv")

# Pre-processed location data with measurements betweeen eBird locations. This is generated by a julia script.
eloc <- readRDS("loc.RDS")
setDT(eloc)
setkey(eloc, LOCALITY1, LOCALITY2)

source("speciesAttr.R")

#speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")
#speciesattr <-  speciesattr %>% filter (ENDEMIC.REGION == "Sri Lanka")
#speciesattr <-  speciesattr %>% filter (CLIP.REGION == "Pakistan")
#speciesattr <-  speciesattr %>% filter (MIGRATION == "Resident")

species <- speciesattr$SCIENTIFIC.NAME

#species <- readRDS(".\\data\\species.rds")

species <- read.csv("species.csv") 
species <- species[,1]

getLocalityMatrix <- function (spec_loc_table)
{
  # Begin create Locality Matrix#######################################  
  
  localities <- spec_loc_table %>% 
    distinct(LOCALITY_ID)
  
  num_localities <- nrow (localities)
  
  print(mem_used())
  if(num_localities > 4000)
  {
    print(paste("Randomised",sp))
    random_indices <- sample(num_localities, 4000)
    # Select the random rows from the dataframe
    localities <- localities[random_indices, ]    
    num_localities <- nrow (localities)
    random_indices <- NULL
  }

  locality_list = as.integer(localities$LOCALITY_ID) %>% sort()
  localities <- NULL
  
  loc_f <- eloc[LOCALITY1 %in% locality_list & LOCALITY2 %in% locality_list]
  # Note, with the optimization to localities further than 200km, this may be only a subset.
  locality_list <- NULL
  
  loc_f$LOCALITY1 <- as.integer(loc_f$LOCALITY1)
  loc_f$LOCALITY2 <- as.integer(loc_f$LOCALITY2)
  
  # Combine locality values to create row and column names
  useful_localities <- unique(c(loc_f$LOCALITY1, loc_f$LOCALITY2)) %>% sort()
  # Generate complete set of row and column indices
  indices <- expand.grid(LOCALITY1 = useful_localities, LOCALITY2 = useful_localities, stringsAsFactors = FALSE) %>% as.data.table()
  
  setDT(loc_f)
  setkey(loc_f, LOCALITY1, LOCALITY2)
  
  # Aggregate distances for the same row and column indices
  aggregated_loc_f <- loc_f[, .(DISTANCE = sum(DISTANCE)), by = .(LOCALITY1, LOCALITY2)]
  
  loc_f  <- NULL
  # Merge with the aggregated data to fill in missing values
  loc_f  <- merge(indices, aggregated_loc_f, by = c("LOCALITY1", "LOCALITY2"), all.x = TRUE)
  indices <- NULL
  aggregated_loc_f <- NULL
  
  setDT(loc_f)
  setkey(loc_f, LOCALITY1, LOCALITY2)
  
  # Create a locality_matrix
  locality_matrix <- matrix(
    loc_f$DISTANCE,
    nrow = length(useful_localities),  
    ncol = length(useful_localities),        
    byrow = TRUE
  )
  loc_f <- NULL
  rownames(locality_matrix) = colnames(locality_matrix) = useful_localities
  useful_localities <- NULL

  # Make the matrix symmetrical.
  missing_values <- is.na(locality_matrix)
  transposed_values <- t(locality_matrix)
  locality_matrix[missing_values] <- transposed_values[missing_values]
  
  missing_values <- NULL
  transposed_values <- NULL
  
  print(num_localities)
  
  return (locality_matrix)
}


for (sp in species) 
{
  if (!file.exists(paste0(".\\data\\data_", sp,".rds"))) 
  { 
    print(paste("No file for",sp)) 
    next;
  }
  
  if (sp %in% highlocalityspecies[,1])
  {
    print(paste("High Locality Species",sp)) 
    next;
  }
  
  data <- readRDS(paste0(".\\data\\data_", sp,".rds"))
  
  # Make the location data more coarse.
  spec_loc_table <- data %>% 
    mutate (LATITUDE = 0.1 * round(10 * LATITUDE),
            LONGITUDE = 0.1 * round (10 * LONGITUDE),
            LOCALITY_ID = as.integer (LONGITUDE*100000 + LATITUDE*10),
            MONTH = as.integer(format(as.Date(OBSERVATION.DATE),"%m")),
            YEAR = as.integer(format(as.Date(OBSERVATION.DATE),"%Y"))) %>%
    dplyr::select(SCIENTIFIC.NAME, LOCALITY_ID, MONTH) %>% 
    distinct()
  
  data <- NULL
  
  Seasonal <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`SEASONAL`) 
  
  if (!is.na(Seasonal$SEASONAL))
  {
    season <- strsplit(as.character(Seasonal),":") %>% as.data.frame()
    season$MONTH <- rownames(season)
    colnames(season) <- c("SEASON", "MONTH")
    season$MONTH <- as.integer(season$MONTH)
    spec_loc_table <- inner_join (spec_loc_table, season)
    
    split_season <- split(spec_loc_table, spec_loc_table$SEASON)
    
    
    for (season_data in split_season) {
      print(mem_used())
      season <- season_data$SEASON %>% unique()
      print(season)
      
      locality_matrix <- getLocalityMatrix (season_data)

      if(!is.null(locality_matrix))
      {
        print(paste("Saving locality matrix for", sp))
        saveRDS(locality_matrix, paste0(".\\localitymatrix\\localitymatrix_",sp,"_", season,".rds"))
        locality_matrix <- NULL
      }
    }
    split_season <- NULL
  }
  else
  {
    locality_matrix <- getLocalityMatrix (spec_loc_table)
    
    if(!is.null(locality_matrix))
    {
      print(paste("Saving locality matrix for", sp))
      saveRDS(locality_matrix, paste0(".\\localitymatrix\\localitymatrix_",sp,".rds"))
      locality_matrix <- NULL
    }
  }
}  
eloc <- NULL
speciesattr <- NULL