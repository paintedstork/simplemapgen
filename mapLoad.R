library (sp)


ind_isl <- readRDS("shapes\\ind_isl.rds")
ind_sub <- readRDS("shapes\\ind_sub.rds")

if (plot)
{
  ind <- readRDS("shapes\\ind.rds")
  pak <- readRDS("shapes\\pak.rds")
  nep <- readRDS("shapes\\nep.rds")
  bhu <- readRDS("shapes\\bhu.rds")
  ban <- readRDS("shapes\\ban.rds")
  lak <- readRDS("shapes\\lak.rds")
  mal <- readRDS("shapes\\mal.rds")
  irn <- readRDS("shapes\\irn.rds")
  mmr <- readRDS("shapes\\mmr.rds")
  chn <- readRDS("shapes\\chn.rds")
  tha <- readRDS("shapes\\tha.rds")
  tkm <- readRDS("shapes\\tkm.rds")
  tjk <- readRDS("shapes\\tjk.rds")
  uzb <- readRDS("shapes\\uzb.rds")
  afg <- readRDS("shapes\\afg.rds")
  
  sta <- readRDS("shapes\\sta.rds")
  cap <- readRDS("shapes\\cap.rds")

  all_land_shapes <- rbind(ind_isl, ind_sub, irn, mmr, chn, tha, uzb, tkm, tjk, afg) %>% st_make_valid()    

  sea <- st_difference(st_as_sfc(st_bbox(all_land_shapes)), all_land_shapes)
}

# Try to keep it same aspect ratio 
boundsConfig <- matrix(
  c(70, 85, 23, 36, #North
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


