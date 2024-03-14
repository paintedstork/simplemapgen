library(sp)
library(sf)

# Moved to a file where the islands near the delta are cleaned up
#ind           <- st_read("shapes\\India.shp")
ind           <- st_read("shapes\\ind_delta_clean.shp")
ind           <- ind[, c("fid", "geometry")]
colnames(ind) <- c("ID", "geometry")
ind_geometry <- st_combine(st_geometry(ind))
ind <- st_sf(ID=1,geometry = ind_geometry)
ind$ID[1] <- 1

# Moved to a file where the islands near the delta are cleaned up
pak           <- st_read("shapes\\pak_delta_clean.shp")
pak           <- pak[, c("ID_0", "geometry")]
colnames(pak) <- c("ID", "geometry")
pak_geometry <- st_combine(st_geometry(pak))
pak <- st_sf(ID=1,geometry = pak_geometry)
pak$ID[1] <- 2

# pak <- st_read("shapes\\Pakistan.shp")
# pak <- pak[, c("ID_0", "geometry")]
# colnames(pak) <- c("ID", "geometry")
# pak$ID[1] <- 2

nep <- st_read("shapes\\Nepal.shp")
nep <- nep[, c("ID_0", "geometry")]
colnames(nep) <- c("ID", "geometry")
nep$ID[1] <- 3

bhu <- st_read("shapes\\Bhutan.shp")
bhu <- bhu[, c("ID_0", "geometry")]
colnames(bhu) <- c("ID", "geometry")
bhu$ID[1] <- 4


# Moved to a file where the islands near the delta are cleaned up
ban           <- st_read("shapes\\ban_delta_clean.shp")
ban           <- ban[, c("ID_0", "geometry")]
colnames(ban) <- c("ID", "geometry")
ban_geometry <- st_combine(st_geometry(ban))
ban <- st_sf(ID=1,geometry = ban_geometry)
ban$ID[1] <- 5

#ban <- st_read("shapes\\Bangladesh.shp")
#ban <- ban[, c("ID_0", "geometry")]
#colnames(ban) <- c("ID", "geometry")
#ban$ID[1] <- 5

lak <- st_read("shapes\\SriLanka.shp")
lak <- lak[, c("ID_0", "geometry")]
colnames(lak) <- c("ID", "geometry")
lak$ID[1] <- 6

mal <- st_read("shapes\\mal_isl_clean.shp")
mal <- mal[, c("ID_0", "geometry")]
colnames(mal) <- c("ID", "geometry")
mal_geometry <- st_combine(st_geometry(mal))
mal <- st_sf(ID=1,geometry = mal_geometry)
mal$ID[1] <- 7

irn <- st_read("shapes\\irn_admbnda_adm0_unhcr_20190514.shp")
irn <- irn[, c("Shape_Leng", "geometry")]
colnames(irn) <- c("ID", "geometry")
irn$ID[1] <- 8

mmr <- st_read("shapes\\mmr_delta_clean.shp")
mmr <- mmr[, c("OBJECTID", "geometry")]
colnames(mmr) <- c("ID", "geometry")
mmr_geometry <- st_combine(st_geometry(mmr))
mmr <- st_sf(ID=1,geometry = mmr_geometry)
mmr$ID[1] <- 9

tjk <- st_read("shapes\\geoBoundaries-TJK-ADM1.geojson")
tjk <- tjk[, c("shapeID", "geometry")]
colnames(tjk) <- c("ID", "geometry")
tjk_geometry <- st_combine(st_geometry(tjk))
tjk <- st_sf(ID=1,geometry = tjk_geometry)
tjk$ID[1] <- 10

chn <- st_read("shapes\\chn_admbnda_adm0_ocha_2020.shp")
chn <- chn[, c("ADM0_EN", "geometry")]
colnames(chn) <- c("ID", "geometry")
chn_geometry <- st_combine(st_geometry(chn))
chn <- st_sf(ID=1,geometry = chn_geometry)
chn$ID[1] <- 11

tha <- st_read("shapes\\tha_admbnda_adm0_rtsd_20220121.shp")
tha <- tha[, c("ADM0_EN", "geometry")]
colnames(tha) <- c("ID", "geometry")
tha_geometry <- st_combine(st_geometry(tha))
tha <- st_sf(ID=1,geometry = tha_geometry)
tha$ID[1] <- 12

tkm <- st_read("shapes\\geoBoundaries-TKM-ADM0.geojson")
tkm <- tkm[, c("shapeID", "geometry")]
colnames(tkm) <- c("ID", "geometry")
tkm_geometry <- st_combine(st_geometry(tkm))
tkm <- st_sf(ID=1,geometry = tkm_geometry)
tkm$ID[1] <- 13

uzb <- st_read("shapes\\uzb_admbnda_adm0_2018b.shp")
uzb <- uzb[, c("ADM0_EN", "geometry")]
colnames(uzb) <- c("ID", "geometry")
uzb_geometry <- st_combine(st_geometry(uzb))
uzb <- st_sf(ID=1,geometry = uzb_geometry)
uzb$ID[1] <- 14

afg <- st_read("shapes\\afghanistan_Afghanistan_Country_Boundary.shp")
afg <- afg[, c("shapeid", "geometry")]
colnames(afg) <- c("ID", "geometry")
afg_geometry <- st_combine(st_geometry(afg))
afg <- st_sf(ID=1,geometry = afg_geometry)
afg$ID[1] <- 15

sta <- st_read("shapes\\ind_states_delta_clean.shp")

cap <- st_read("shapes\\Capitals.shp")

all_geom <- ind$geometry %>% st_union (pak$geometry) %>% st_union(ban$geometry) %>% st_union(nep$geometry) %>% st_union(bhu$geometry) %>% st_union(ban$geometry)
boundary <- st_combine(all_geom)
boundary_sf <- st_sf(ID = 1, geometry = boundary)

boundary_sf <- st_make_valid (boundary_sf)
saveRDS(boundary_sf,"shapes\\ind_sub.rds")


islands <- st_union(lak$geometry, mal$geometry)
islandbounday <- st_combine(islands)
islandboundary_sf <- st_sf(ID = 2, geometry = islandbounday)

islandboundary_sf <- st_make_valid (islandboundary_sf)

saveRDS(islandboundary_sf, "ind_isl.rds")

saveRDS(ind,"shapes\\ind.rds")
saveRDS(pak,"shapes\\pak.rds")
saveRDS(nep,"shapes\\nep.rds")
saveRDS(bhu,"shapes\\bhu.rds")
saveRDS(ban,"shapes\\ban.rds")
saveRDS(lak,"shapes\\lak.rds")
saveRDS(mal,"shapes\\mal.rds")
saveRDS(sta,"shapes\\sta.rds")
saveRDS(cap,"shapes\\cap.rds")
saveRDS(irn,"shapes\\irn.rds")
saveRDS(mmr,"shapes\\mmr.rds")
saveRDS(chn,"shapes\\chn.rds")
saveRDS(uzb,"shapes\\uzb.rds")
saveRDS(tkm,"shapes\\tkm.rds")
saveRDS(tha,"shapes\\tha.rds")
saveRDS(tjk,"shapes\\tjk.rds")
saveRDS(afg,"shapes\\afg.rds")


# Clipping polygon
#ind_sub_clean <- st_read("shapes\\ind_sub_clean2_simplified_valid.shp")
#ind_sub_clean <- st_make_valid(ind_sub_clean)
#saveRDS(ind_sub_clean,"shapes\\ind_sub_clean.rds")


# # Trying out new things
# library(sf)
# 
# # Function to simplify geometries
# simplify_geometries <- function(sf_object) {
#   simplified_sf <- st_simplify(sf_object, preserveTopology = FALSE, dTolerance = 0.05)
#   return(simplified_sf)
# }
# 
# # Read and simplify each shapefile
# ind <- st_read("shapes\\India.shp") %>% simplify_geometries()
# saveRDS(ind, "shapes\\ind.rds")
# 
# pak <- st_read("shapes\\Pakistan.shp") %>% simplify_geometries()
# saveRDS(pak, "shapes\\pak.rds")
# 
# nep <- st_read("shapes\\Nepal.shp") %>% simplify_geometries()
# saveRDS(nep, "shapes\\nep.rds")
# 
# bhu <- st_read("shapes\\Bhutan.shp") %>% simplify_geometries()
# saveRDS(bhu, "shapes\\bhu.rds")
# 
# ban <- st_read("shapes\\Bangladesh.shp") %>% simplify_geometries()
# saveRDS(ban, "shapes\\ban.rds")
# 
# lak <- st_read("shapes\\SriLanka.shp") %>% simplify_geometries()
# saveRDS(lak, "shapes\\lak.rds")
# 
# mal <- st_read("shapes\\Maldives.shp") %>% simplify_geometries()
# saveRDS(mal, "shapes\\mal.rds")
# 
# sta <- st_read("shapes\\India States.shp") %>% simplify_geometries()
# saveRDS(sta, "shapes\\sta.rds")
# 
# cap <- st_read("shapes\\Capitals.shp") %>% simplify_geometries()
# saveRDS(cap, "shapes\\cap.rds")
# 
# # Simplify the unioned geometries
# all_geom <- ind$geometry %>% st_union(pak$geometry) %>% st_union(ban$geometry) %>% st_union(nep$geometry) %>% st_union(bhu$geometry) %>% st_union(ban$geometry)
# boundary <- st_combine(all_geom)
# boundary_sf <- st_sf(ID = 1, geometry = boundary) %>% simplify_geometries()
# saveRDS(boundary_sf, "shapes\\ind_sub.rds")
# 
# # Simplify island boundaries
# islands <- st_union(lak$geometry, mal$geometry)
# islandbounday <- st_combine(islands)
# islandboundary_sf <- st_sf(ID = 2, geometry = islandbounday) %>% simplify_geometries()
# saveRDS(islandboundary_sf, "shapes\\ind_isl.rds")
# 
