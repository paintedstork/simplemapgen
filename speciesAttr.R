speciesattr <- read.csv2(paste0("SoIB_main_09062023.csv"), sep=",") %>% 
  dplyr::select('eBird.English.Name.2022', 
                'eBird.Scientific.Name.2022', 
                "Family.Name",
                "Family.English.Name",
                'India.Endemic',
                'Endemic.Region', 
                'Migratory.Status.Within.India', 
                'Clip.Region', 
                'Resolution', 
                'Polygon.Type', 
                'Pelagic', 
                'Sensitive', 
                'Seasonal',
                'PassageRange',
                'SeasonPriority',
                'DotMapOnly',
                "Live")

colnames(speciesattr) <- c("ENGLISH.NAME", 
                           "SCIENTIFIC.NAME",
                           "FAMILY.NAME",
                           "FAMILY.ENGLISH.NAME",
                           "ENDEMIC", 
                           "ENDEMIC.REGION", 
                           "MIGRATION", 
                           "CLIP.REGION",
                           "RESOLUTION", 
                           "POLYGON.TYPE", 
                           "PELAGIC", 
                           "SENSITIVE", 
                           "SEASONAL",
                           "PASSAGE.RANGE",
                           "SEASON.PRIORITY",
                           "DOTMAP.ONLY",
                           "LIVE")
