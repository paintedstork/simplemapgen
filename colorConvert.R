# Function to convert a single RGB value to hexadecimal format
rgbToHex <- function(rgb) {
  return(sprintf("#%02X%02X%02X", as.integer(rgb[1]), as.integer(rgb[2]), as.integer(rgb[3])))
}

# Define CMYK to RGB conversion function
CMYK2RGB <- function(cmyk) {
  c <- cmyk[1] / 100  # Convert from percentage to fraction
  m <- cmyk[2] / 100
  y <- cmyk[3] / 100
  k <- cmyk[4] / 100
  
  # Convert CMYK to RGB
  r <- 255 * (1 - c) * (1 - k)
  g <- 255 * (1 - m) * (1 - k)
  b <- 255 * (1 - y) * (1 - k)
  
  # Clip values to [0, 255]
  r <- pmax(0, pmin(255, r))
  g <- pmax(0, pmin(255, g))
  b <- pmax(0, pmin(255, b))

# Return RGB values as a vector
return(c(r, g, b))
}

# Convert CMYK colors to RGB
rgb_colors <- lapply(cmyk_colors, CMYK2RGB)

# Convert the structure to match map_colors
names(rgb_colors) <- names(cmyk_colors)

# Convert RGB values to hexadecimal format
rgb_colors <- sapply(rgb_colors, rgbToHex)


library(measurements)
