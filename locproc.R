library (tidyverse)
library(pracma)
library(purrr)
library(dplyr)

CurMonth <- 5
CurYear  <- 2023

# List the interested columns
preimp <-  c( 
  "LATITUDE", #For identifying region
  "LONGITUDE" #For identifying region
)

# Do the same for all Indian subcont countries
countries <- c("IN", "PK", "NP", "BT", "BD", "LK", "MV")

for (country in countries)
{
  ebdfile <- paste0("ebd_",country,"_rel",month.abb[CurMonth],"-",CurYear)
  ebdfile <- paste0(".\\ebd\\",ebdfile)
  
  # Read the header plus first row
  nms <- read.delim( paste0 (ebdfile,".txt"),
                     nrows = 1, 
                     sep = '\t', 
                     header = T, 
                     quote = "", 
                     stringsAsFactors = F, 
                     na.strings = c ("", " ",NA)) 
  nms <- names(nms)
  nms [!(nms %in% preimp)] <- "NULL"
  nms [nms %in% preimp] <- NA
  
  
  data <- read.delim(paste0(ebdfile,".txt"),
                     colClasses = nms,
                     #                  nrows = 100000, # For testing, this is useful
                     sep = '\t', 
                     header = T, 
                     quote = "", 
                     stringsAsFactors = F, 
                     na.strings = c ("", " ",NA)) 
  
  data1 <- data %>% mutate (LATITUDE = 0.1 * round(10 * LATITUDE),
                          LONGITUDE = 0.1 * round (10 * LONGITUDE), 
                          LATITUDE_R = deg2rad (LATITUDE),
                          LONGITUDE_R = deg2rad (LONGITUDE),
                          LOCALITY_ID = LONGITUDE*100000 + LATITUDE*10) %>%
                            dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE, LATITUDE_R, LONGITUDE_R) %>%
                              distinct (LOCALITY_ID, .keep_all = TRUE)
  
  write.csv(data1, paste0("locality",country,".csv"), row.names=FALSE)
}


# Read and merge CSV files
merged_data <- countries %>%
  map_df(~ read.csv(paste0("locality", .x, ".csv")), .id = "Country")

# Write the merged data to a single CSV file
write.csv(merged_data[-1], "locality.csv", row.names = FALSE)

#############################################
#Run the distance.jl Julia program to calculate all pairs distances
#Takes locality.csv as input and generates loc.csv as output
#########################################
df <- read.csv("loc_extra.csv")
saveRDS(df, "loc.RDS")
