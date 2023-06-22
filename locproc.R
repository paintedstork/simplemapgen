library (tidyverse)

CurMonth <- 9
CurYear  <- 2022
# Keep the zip file in a direct one level up as data

# TODO: Move to India Data
ebdfile <- paste0("ebd_IN_prv_rel",month.abb[CurMonth],"-",CurYear)

# List the interested columns
preimp <-  c( 
  "LATITUDE", #For identifying region
  "LONGITUDE" #For identifying region
)

ebdfile <- paste0("..\\state-of-indias-birds\\soibiucn\\",ebdfile)
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


write.csv(data1, "locality.csv", row.names=FALSE)

df <- read.csv("loc.csv")
saveRDS(df, "loc.RDS")
