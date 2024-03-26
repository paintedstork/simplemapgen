# Load required libraries
library(data.table)
library(stringr)
library(tools)
library(tidyverse)

dist_thresholds <- speciesattr$RESOLUTION %>% unique() %>% na.omit() %>% as.vector() %>% sort()
seasons <- c('W', 'S', 'R', 'P')
seasons <- paste0("_", seasons)

dist_thresholds <- paste0("_", dist_thresholds)

threshold_string <- paste(dist_thresholds, collapse = "|")
threshold_string <- paste0("(", threshold_string, ")?")

seasons_string <- paste(seasons, collapse = "|")
seasons_string <- paste0("(", seasons_string, ")?")

my_file_exists <- function (directory, pattern)
{
  #Search for pattern in directories:
  return ( any(grepl(paste(file.path(directory, pattern), collapse = "|"), list.files(directory, full.names = TRUE, recursive = FALSE))))  
}

# Function to get the file names from a directory
get_file_names <- function(directory, dist_suffix, season_suffix) {
  d_suffix <- ifelse(dist_suffix == "Yes", threshold_string, "")
  s_suffix <- ifelse(season_suffix == "Yes", seasons_string, "")
  pattern <- paste0(directory, ".*", d_suffix, s_suffix, "\\.\\w{3}$")
  
  file_names <- list.files(path = directory, pattern = pattern, full.names = TRUE) %>% unique()  
  return(file_names)
}

get_genus_species <- function(file_names) {
  pattern <- "([^_]+)_([^_.]+)[_.]"  
  genus_species <- str_extract(file_names, pattern, 2)
  return(genus_species %>% unique())  # Unlist to have a vector of species names
}


# Function to create the data.table
create_summary_table <- function(directory, dist_suffix, season_suffix, ext) {
  file_names <- get_file_names(directory, dist_suffix, season_suffix)
  present_species <- get_genus_species(file_names)
  
  species_summary <- data.table(Species = present_species)

  species_summary <- inner_join(species_summary, speciesattr, by = c('Species' = 'SCIENTIFIC.NAME')) %>% 
                          dplyr::select ('Species', 'RESOLUTION','SEASONAL') %>% 
                            rowwise() %>%
                              mutate (RESOLUTION = ifelse(is.na(RESOLUTION), 100, RESOLUTION),
                                    filenamestring = ifelse ((season_suffix == "Yes") & !is.na(SEASONAL), 
                                                             paste0(directory,"_", Species,ifelse(dist_suffix == "Yes", paste0("_",RESOLUTION), ""), "_", "[SPWR]", ".", ext),
                                                             paste0(directory,"_", Species,ifelse(dist_suffix == "Yes", paste0("_",RESOLUTION), ""),                ".",ext)
                                                            ))
                                      
  species_summary <- species_summary %>% 
                            rowwise() %>%
                            mutate (!!directory := ifelse(
                                                  is.na(SEASONAL),
                                                  ifelse(file.exists(file.path(directory,filenamestring)), "X",NA),
                                                  ifelse(my_file_exists(directory, filenamestring), "X",NA)
                                                  ))
  
  species_summary <- species_summary %>% dplyr::select('Species', directory)   
  species_summary <- species_summary %>% dplyr::distinct_all()
  
  return(species_summary)
}

# Directories list
directories <- c("map", "points", "polygons", "cluster", "localitymatrix","data") 
dist_suffixes   <- c("Yes", "Yes", "Yes", "Yes", "No", "No")
season_suffixes <- c("No", "Yes", "Yes", "Yes", "Yes", "No")
extension <- c("jpg", "rds", "rds", "rds", "rds", "rds")

# Create the combined summary data.table
combined_summary <- NULL
for (i in 1: length(directories))
{
  print(directories[i])
  summary_table <- create_summary_table(directories[i], dist_suffixes[i], season_suffixes[i], extension [i])
  if (is.null(combined_summary)) {
    combined_summary <- summary_table
  } else {
    combined_summary <- merge(combined_summary, summary_table, by = "Species", all = TRUE)
  }
}

colnames(combined_summary) <- toTitleCase(colnames(combined_summary))


# Write the combined summary data.table to summary_combined.csv
write.csv(combined_summary, "summary_combined.csv", row.names = FALSE)
