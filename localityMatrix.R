#####################Algorithm to prepare locality matrix####################################
library (tidyverse)
library(data.table)


# Pre-processed location data with measurements betweeen eBird locations. This is generated by a perl/julia script.
eloc <- readRDS("loc.RDS")
setDT(eloc)
setkey(eloc, LOCALITY1, LOCALITY2)


speciesattr <- read.csv2(paste0("..\\soib_v2\\SoIB_main_09062023.csv"), sep=",") %>% 
  dplyr::select('eBird.Scientific.Name.2022', 'India.Endemic')

colnames(speciesattr) <- c("SCIENTIFIC.NAME", "ENDEMIC")

speciesattr <-  speciesattr %>% filter (ENDEMIC == "Yes")

species <- speciesattr$SCIENTIFIC.NAME

species <- readRDS(".\\data\\species.rds")

for (sp in species) 
{
  # For each species, find the unique locations
  data <- readRDS(paste0(".\\data\\", sp,".rds"))
  
  if(nrow(data) > 50000)
  {
    print(paste("Randomised",sp))
    random_indices <- sample(nrow(data), 50000)
    # Select the random rows from the dataframe
    data <- data[random_indices, ]    
  }

  # Make the location data more coarse.
  spec_loc_table <- data %>% 
                      mutate (LATITUDE = 0.1 * round(10 * LATITUDE),
                              LONGITUDE = 0.1 * round (10 * LONGITUDE),
                              LOCALITY_ID = as.integer (LONGITUDE*100000 + LATITUDE*10),
                              MONTH = as.integer(format(as.Date(OBSERVATION.DATE),"%m")),
                              YEAR = as.integer(format(as.Date(OBSERVATION.DATE),"%Y"))) %>%
                        dplyr::select(SCIENTIFIC.NAME, LOCALITY_ID, MONTH, YEAR) %>% 
                          distinct()
  
  data <- NULL
  
  # Begin create Locality Matrix#######################################  

  localities <- spec_loc_table %>% 
                  distinct(LOCALITY_ID) 
  print(paste("Saving species location table for", sp))
  saveRDS(spec_loc_table, paste0(".\\loc_table\\loc_table_",sp,".rds"))
  spec_loc_table <- NULL
  
  
  num_localities <- nrow (localities)
  
  locality_list = as.integer(localities$LOCALITY_ID) %>% sort()
  localities <- NULL
  
  loc_f <- eloc[LOCALITY1 %in% locality_list & LOCALITY2 %in% locality_list]
  
  loc_f$LOCALITY1 <- as.integer(loc_f$LOCALITY1)
  loc_f$LOCALITY2 <- as.integer(loc_f$LOCALITY2)
  
  # Combine locality values to create row and column names
  locations <- unique(c(loc_f$LOCALITY1, loc_f$LOCALITY2))
  # Generate complete set of row and column indices
  indices <- expand.grid(LOCALITY1 = locations, LOCALITY2 = locations, stringsAsFactors = FALSE) %>% as.data.table()
  
  locations <- NULL
  
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
    nrow = num_localities,  
    ncol = num_localities,        
    byrow = TRUE
  )
  loc_f <- NULL
  rownames(locality_matrix) = colnames(locality_matrix) = locality_list
  locality_list <- NULL
  
  # Make the matrix symmetrical.
  missing_values <- is.na(locality_matrix)
  transposed_values <- t(locality_matrix)
  locality_matrix[missing_values] <- transposed_values[missing_values]

  missing_values <- NULL
  transposed_values <- NULL
  
  print(num_localities)
  
  print(paste("Saving locatlity matrix for", sp))
  saveRDS(locality_matrix, paste0(".\\locality_matrix\\locality_matrix_",sp,".rds"))
  locality_matrix <- NULL
  
}  
eloc <- NULL