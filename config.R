generateSVG = 0
svgWidthinInches = 7
svgHeightinInches = 7

generateSeasonalMaps = 1


showStates = 1

jpgWidthinInches = 7
jpgHeightinInches = 7

showCapitals = 1
capitalIconSize = 1
capitalIconShape = 15
capitalIconFillColor = "black"

showSpeciesName = 1
speciesNameColor = "black"
speciesNameFontSize = 12

pointIconSize = 3
pointIconShape = 1
pointIconFillColor = "transparent" 
map_colors <- c("W" = "darkblue", "S" = "red", "R" = "darkgreen", "P" = "yellow2")

#Default distance threshold for cluster formation
defaultDistanceThreshold <- as.integer(100) # In km

#Default season priority
defaultSeasonPriority <- c("S", "W", "P")

panelBorderColor = "black"
panelBorderSize = 1

marginTop = 80
marginBottom = 10
marginLeft = 10
marginRight = 10

# Try to keep it same aspect ratio 
boundsConfig <- matrix(
  c(70, 85, 21, 36, #North
    79, 88, 25, 34, #Nepal
    69, 89, 2,  22, #South
    78, 83, 5,  10, #Lanka
    86, 97, 20, 31, #Northeast
    61, 75, 22, 36, #Pakistan
    61, 78, 19, 36, #Northwest
    89, 97, 6,  14, #Andaman
    72, 78, 8,  14, #Ghats
    71, 74, 2,  5,  #Maldives
    78, 97, 14, 33, #East
    85, 92, 20, 27, #Sundarbans
    80, 97, 14, 31, #EastNorthEast 
    62, 96, 2,  36),#None
  nrow = 4,
  dimnames = list(
    c("xlow", "xhigh", "ylow", "yhigh"),
    c("North", "Nepal", "South", "Lanka", "Northeast", "Pakistan", "Northwest", "Andaman", "Ghats", "Maldives", "East", "Sundarbans", "EastNorthEast", "None"))
) %>% as.data.frame() %>% t()


