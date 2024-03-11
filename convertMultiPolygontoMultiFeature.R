# Install and load the 'sf' package
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)


# Read the input shapefile
input_sf <- st_read("shapes\\India States.shp")

# Check the structure of the original sf object
print(input_sf)

# Create a new sf object with individual features
split_sf <- st_cast(input_sf, "POLYGON", group_or_split = TRUE)

# Specify the output folder and file name
output_folder <- ".\\shapes\\split_vector"
output_file <- paste0(output_folder, "\\split_vector.shp")

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Write the new shapefile
st_write(split_sf, output_file)
