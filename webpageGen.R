source("speciesAttr.R")
speciescount <- 0
filecount <- 0
filename <- ""
newFile <- TRUE

speciesattr <-  speciesattr %>% filter (LIVE == "X")

family <- speciesattr$FAMILY.NAME %>% unique()


for (fm in family)
{
  if (newFile)
  { # Newfile flag has been set. Increase filecount, create new file and initialize it.
    filecount <- filecount + 1
    filename <- paste0(".\\webout\\webpage", filecount,".txt")
    write_file("\n", filename, append=FALSE)
    
    write_file("<ul>\n", filename, append = TRUE )
    newFile <- FALSE #Reset the flag
  }
  print(fm)
  species <- speciesattr %>% filter (FAMILY.NAME == fm) %>% dplyr::select(SCIENTIFIC.NAME)
  species <- species$SCIENTIFIC.NAME
  if (length (species) > 0)
  {
    familyEnglishname <- speciesattr %>% filter (FAMILY.NAME == fm) %>% dplyr::select(FAMILY.ENGLISH.NAME)
    
    write_file(paste0("<li><h5><strong>",familyEnglishname$FAMILY.ENGLISH.NAME[1]," (",fm,")</strong></h5>\n"), filename,append=TRUE)
  }
  for (sp in species)
  {
    resolution <- speciesattr %>% filter (`SCIENTIFIC.NAME` == sp) %>% dplyr::select(`RESOLUTION`)
    resolution <- ifelse(is.na(resolution), 100, as.integer(resolution))
    
    if (file.exists(paste0(".\\map\\map_", sp,"_",resolution,".jpg")))
    {
      speciescount <- speciescount + 1
      if (speciescount %% 100 == 0)
      { #If the new 100 species is reached, then set the flag to open new file
        newFile <- TRUE
      }
        
      print(paste(speciescount,sp))
      write_file("<ul>\n", filename, append = TRUE )
      englishName <- speciesattr %>% filter (SCIENTIFIC.NAME == sp) %>% dplyr::select(ENGLISH.NAME)
      englishName <- englishName$ENGLISH.NAME
      
      write_file(paste0("<li>",englishName," (",sp,")"," [sgdg path=\"SpeciesMaps/All Species/",sp,"\"]</li>\n"), filename,append=TRUE)
      write_file("</ul>\n", filename, append = TRUE )

      if (dir.exists(paste0(".\\webout\\", sp))) 
      {
        unlink(paste0(".\\webout\\", sp), recursive = TRUE)
      }
      dir.create(paste0(".\\webout\\", sp))
      
      file.copy(paste0(".\\map\\map_", sp,"_",resolution,".jpg"), paste0(".\\webout\\", sp,"\\map_", sp,"_",resolution,".jpg"))
    }
    else
    {
      print (paste("Missing file", sp))
    }
  }
  if (newFile)
  { #newFile flag has been set. Close the existing file
    write_file("</li>\n", filename, append = TRUE)
    write_file("</ul>\n", filename, append = TRUE )
  }
}
  


