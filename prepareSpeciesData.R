#########################Prepare species data##############################################
library (tidyverse)
library(data.table)

CurMonth <- 9
CurYear  <- 2022
# Keep the zip file in a direct one level up as data

# TODO: Move to India Data
ebdfile <- paste0("ebd_IN_prv_rel",month.abb[CurMonth],"-",CurYear)

# List the interested columns
preimp <-  c( 
  "SCIENTIFIC.NAME", # For list length calc.
  "CATEGORY", # To remove spuhs
  "OBSERVATION.DATE", # For year and month
  "LATITUDE", #For identifying region
  "LONGITUDE", #For identifying region
  "APPROVED" #Remove unapproved records
)


ebdfile <- paste0("..\\state-of-indias-birds\\soibiucn\\",ebdfile)

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

data <- read.delim(paste0(ebdfile,".txt"),
                   colClasses = nms,
                   #                  nrows = 100000, # For testing, this is useful
                   sep = '\t', 
                   header = T, 
                   quote = "", 
                   stringsAsFactors = F, 
                   na.strings = c ("", " ",NA)) 

setDT(data)
setkey(data, CATEGORY)

categories <- c("species", "issf")

data <- data[CATEGORY %in% categories] 
     
data <- data %>% 
  filter (APPROVED == 1)

species <- data$SCIENTIFIC.NAME %>% unique()

for (sp in species) 
{
  df <- data %>% filter(SCIENTIFIC.NAME == sp)
  print(paste("Saving data for", sp))
  saveRDS(df, paste0(".\\data\\",sp, ".rds"))
}

saveRDS(species, paste0(".\\data\\species.rds"))
data <- NULL
df <- NULL
