generateSVG = 0
svgWidthinInches = 7
svgHeightinInches = 7

generateTIFF = 0

generateJPG = 1

generateSeasonalMaps = 0


showStates = 1

jpgWidth  = 7
jpgHeight = 7
jpgUnit   = "in"
jpgdpi    = 300

tiffWidth   = 26.5 
tiffHeight  = 29 
tiffUnit    = "mm"
tiffdpi     = 600

showCapitals = 1
capitalIconSize = 1/15
capitalIconSize = 1
capitalIconShape = 15
capitalIconFillColor = "black"

showSpeciesName = 1
speciesNameColor = "black"
speciesNameFontSize = 12

pointIconSize = 3/6
pointIconSize = 3
pointIconShape = 1
pointIconFillColor = "transparent" 
pointIconStroke = 0.5/7
pointIconStroke = 0.5

polygonLineThickness = 0.5/20
polygonLineThickness = 0.5/7

useCmYk = 0
cmyk_colors <- list(
  "W" = c(75, 0, 0, 0),   # Winter
  "S" = c(0, 33, 100, 0), # Summer
  "R" = c(64, 19, 99, 0), # Resident
  "P" = c(33, 47, 8, 0)   # Passage
)

map_colors <- c("W" = "darkblue", "S" = "red", "R" = "darkgreen", "P" = "yellow2")

sea_color <- "lightblue"


#Default distance threshold for cluster formation
defaultDistanceThreshold <- as.integer(100) # In km

#Default season priority
defaultSeasonPriority <- c("R", "S", "W", "P")

showBorder = 1
panelBorderColor = "black"
panelBorderSize = 1

marginTop = 80
marginBottom = 10
marginLeft = 10
marginRight = 10

#marginTop = 0
#marginBottom = 0
#marginLeft = 0
#marginRight = 0

# Try to keep it same aspect ratio 
boundsConfig <- matrix(
  c(70, 85, 22, 36.5, 0000, #North 
    80, 89, 25.5, 33.5, 33000, #Nepal 
    69, 89, 1.25,  22, 20000, #South 
    78, 83, 5,  10, 20000, #Lanka 
    86.5, 97.5, 20.5, 30.5, 50000, #Northeast 
    60.5, 75.5, 22, 36.5, 1000, #Pakistan 
    60.5, 78.5, 19, 36.5, 2000, #Northwest 
    89, 97, 6,  14, 31000, #Andaman 
    72, 78, 8,  14, 18000, #Ghats 
    71, 74, 2,  5,  20000, #Maldives (Pending)
    78, 97, 14, 33, 00000, #East 
    85, 92, 20, 27, 4000, #Sundarbans 
    80, 97, 14, 31, 20000, #EastNorthEast  
    60.5, 98, 1.25, 36,  220000),#None (Fine-tuned)
  nrow = 5,
  dimnames = list(
    c("xmin", "xmax", "ymin", "ymax", "pixelcorrection"),
    c("North", "Nepal", "South", "Lanka", "Northeast", "Pakistan", "Northwest", "Andaman", "Ghats", "Maldives", "East", "Sundarbans", "EastNorthEast", "None"))
) %>% as.data.frame() %>% t()


#adjust_ymin <- function(ymin, ymax, aspectRatio) {
#  new_ymin <- ymin - 0.5 * ( (ymax - ymin) * (aspectRatio - 1)  )
#  return(new_ymin)
#}

#adjust_ymax <- function(ymin, ymax, aspectRatio) {
#  new_ymax <- ymax + 0.5 * ( (ymax - ymin) * (aspectRatio - 1)  )
#  return(new_ymax)
#}


#newboundsConfig <- boundsConfig
# Iterate over each row of boundsConfig and adjust ymin
#for (i in 1:nrow(boundsConfig)) {
#  newboundsConfig[i, "ymin"] <- adjust_ymin(boundsConfig[i, "ymin"], boundsConfig[i, "ymax"], aspectRatio)
#  newboundsConfig[i, "ymax"] <- adjust_ymax(boundsConfig[i, "ymin"], boundsConfig[i, "ymax"], aspectRatio)
#}

#boundsConfig <- newboundsConfig
