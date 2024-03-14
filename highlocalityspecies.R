species <- readRDS(".\\data\\species.rds")

for (sp in species)
{
  if (!file.exists(paste0(".\\data\\data_", sp,".rds")))
  {
    next
  }
  
  data <- readRDS(paste0(".\\data\\data_", sp,".rds"))
  spec_loc_table <- data %>% 
    mutate (LATITUDE = 0.1 * round(10 * LATITUDE),
            LONGITUDE = 0.1 * round (10 * LONGITUDE),
            LOCALITY_ID = as.integer (LONGITUDE*100000 + LATITUDE*10),
            MONTH = as.integer(format(as.Date(OBSERVATION.DATE),"%m")),
            YEAR = as.integer(format(as.Date(OBSERVATION.DATE),"%Y"))) %>%
    dplyr::select(SCIENTIFIC.NAME, LOCALITY_ID, MONTH) %>% 
    distinct()
  
  localities <- spec_loc_table %>% 
    distinct(LOCALITY_ID)
  num_localities <- nrow (localities)
  if (num_localities > 4000)
  {
    print (sp)
  }
}