#########################Prepare species data##############################################
library (tidyverse)
library(data.table)

CurMonth <- 5
CurYear  <- 2023
# Keep the zip file in a direct one level up as data

# List the interested columns
preimp <-  c( 
  "SCIENTIFIC.NAME", # For list length calc.
  "CATEGORY", # To remove spuhs
  "OBSERVATION.DATE", # For year and month
  "LATITUDE", #For identifying region
  "LONGITUDE", #For identifying region
  "APPROVED" #Remove unapproved records
)


# Do the same for all Indian subcont countries
countries <- c("PK", "NP", "BT", "BD", "LK", "MV", "IN")
data <- NULL

for (country in countries)
{
  ebdfile <- paste0("ebd_",country,"_rel",month.abb[CurMonth],"-",CurYear)
  ebdfile <- paste0(".\\ebd\\",ebdfile)
  
####################### Preparation of the dataset############################
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

  data1 <- read.delim(paste0(ebdfile,".txt"),
                     colClasses = nms,
                     #                  nrows = 100000, # For testing, this is useful
                     sep = '\t', 
                     header = T, 
                     quote = "", 
                     stringsAsFactors = F, 
                     na.strings = c ("", " ",NA)) 
  
  if(is.null(data))
  { # Dont read IN first. It will create two huge dataframs
    data <- data1
  }
  else
  {
    data <- rbind (data, data1)
  }
  data1 <- NULL
}

setDT(data)
setkey(data, CATEGORY)

categories <- c("species", "issf")

data <- data[CATEGORY %in% categories] 
     
data <- data %>% 
  filter (APPROVED == 1)

totspecies <- species <- data$SCIENTIFIC.NAME %>% unique()

species <- read.csv("species.csv") 
species <- species[,1]

for (sp in species) 
{
  df <- data %>% filter(SCIENTIFIC.NAME == sp)
  print(paste("Saving data for", sp))
  saveRDS(df, paste0(".\\data\\data_",sp, ".rds"))
  df <- NULL
}

saveRDS(totspecies, paste0(".\\data\\species.rds"))
data <- NULL

########################################################Faster vectorised code

#split_df <- split(data, data$SCIENTIFIC.NAME)
#lapply(names(split_df), function(v) {
#          saveRDS(split_df[[v]], file = filename <- paste0(".\\data\\data_",v,".rds"))
#})
split_df <- NULL
data <- NULL