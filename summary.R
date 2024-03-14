# Load required libraries
library(data.table)
library(stringr)
library(tools)

# Function to get the file names from a directory
get_file_names <- function(directory, suffix) {
  suffix <- ifelse (suffix == "Yes", "(50|100)", "")
  file_names <- list.files(path = directory, pattern = paste0(directory,"_.*",suffix,"\\.\\w{3}$"), full.names = TRUE)
  return(file_names)
}

# Function to extract GenusName and SpeciesName from file names
get_genus_species <- function(file_names, suffix) {
  pattern <- "([^_]+)\\s([^_]+)"
  pattern <- ifelse(suffix == "Yes", "([^_]+)\\s([^_]+)", "([^_]+)\\s([^\\.]+)")
  genus_species <- str_extract_all(file_names, pattern)
  genus_species <- sapply(genus_species, function(x) paste(x, collapse = "_"))
  return(genus_species)
}

# Function to create the data.table
create_summary_table <- function(directory, suffix) {
  file_names <- get_file_names(directory, suffix)
  present_species <- get_genus_species(file_names, suffix)
  
  species_summary <- data.table(Species = present_species)
  
  # Create a column with "X" if the species is present in the directory, empty otherwise
  species_summary[, (directory) := ifelse(Species %in% present_species, "X", ""), by = .(Species)]
  
  species_summary <- species_summary %>% dplyr::distinct_all()
  
  return(species_summary)
}

# Directories list
directories <- c("map", "points", "polygons", "cluster", "loctable", "localitymatrix","data") 
suffixes <- c("Yes", "Yes", "Yes", "Yes", "No", "No", "No")

# Create the combined summary data.table
combined_summary <- NULL
for (i in 1: length(directories))
{
  summary_table <- create_summary_table(directories[i], suffixes[i])
  if (is.null(combined_summary)) {
    combined_summary <- summary_table
  } else {
    combined_summary <- merge(combined_summary, summary_table, by = "Species", all = TRUE)
  }
}

colnames(combined_summary) <- toTitleCase(colnames(combined_summary))


# Write the combined summary data.table to summary_combined.csv
write.csv(combined_summary, "summary_combined.csv", row.names = FALSE)
