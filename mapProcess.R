library(sp)

ind <- st_read("India.shp")
pak <- st_read("Pakistan.shp")
nep <- st_read("Nepal.shp")
bhu <- st_read("Bhutan.shp")
ban <- st_read("Bangladesh.shp")
lak <- st_read("SriLanka.shp")
mal <- st_read("Maldives.shp")
sta <- st_read("India States.shp")
cap <- st_read("Capitals.shp")

# Clipping polygon
ind_sub <- st_read("ind_sub_clean_simplified_valid.shp")
ind_sub <- st_make_valid(ind_sub)
