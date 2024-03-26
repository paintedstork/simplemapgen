source("speciesAttr.R")
library(readr)
library(dplyr)
speciescount <- 0
filecount <- 0
filename <- ""
newFile <- TRUE

#speciesattr <-  speciesattr %>% filter (LIVE == "X")
speciesattr <-  speciesattr

family <- speciesattr$FAMILY.NAME %>% unique()

headerText1 <- "A simple species map is quite useful to convey the distribution of a bird quickly. They have been used extensively in printed books and field guides. This page has such maps for <span style=\"text-decoration: underline;\">450</span> <strong>resident species</strong> and 70 migratory species, entirely generated out of eBird data.\n"
#headerText <- paste0(headerText, "<a href=\"https://birdcount.in/speciesmaps/\">Page 1 </a>|<a href=\"https://birdcount.in/speciesmaps2/\">Page 2 </a>|<a href=\"https://birdcount.in/speciesmaps3/\">Page 3 </a>|<a href="https://birdcount.in/speciesmaps4/">Page 4 </a>|<a href="https://birdcount.in/speciesmaps5/">Page 5 </a>|<a href="https://birdcount.in/speciesmaps6/">Page 6 </a>\n"
  
footerText1 <- "Version 1.1: Released on 12 March 2024. Data download till 31 May 2023.\n"
footerText2 <- "Recommended Attribution: <i>Created by Bird Count India from eBird data. Website URL: https://birdcount.in/speciesmaps/ </i> <i>Accessed on 12 March 2024.</i>\m"
footerText3 <- "The contents of this page are made available under a <a href=\"https://creativecommons.org/licenses/by/4.0/\" target=\"_blank\" rel=\"noopener noreferrer\">CC BY license</a></li>\n"
footerText4 <- "</ul>\n"


for (fm in family)
{
  newFile <- TRUE
  if (newFile)
  { # Newfile flag has been set. Increase filecount, create new file and initialize it.
    filecount <- filecount + 1
    filename <- paste0(".\\webout\\webpage", filecount,".txt")
    
    write_file(headerText1, filename, append=FALSE)
    write_file("\n", filename, append=TRUE)
    
    write_file("<ul>\n", filename, append = TRUE )
#    newFile <- FALSE #Reset the flag
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
    
    makeLive <-   speciesattr %>% filter (`SCIENTIFIC.NAME` == sp)  %>% dplyr::select(`LIVE`)
    
    if ( (makeLive$LIVE == "X") & (file.exists(paste0(".\\map\\map_", sp,"_",resolution,".jpg"))))
    {
      speciescount <- speciescount + 1
#      if (speciescount %% 50 == 0)
#      { #If the new 50 species is reached, then set the flag to open new file
#        newFile <- TRUE
#      }
        
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
      
      write_file("<ul>\n", filename, append = TRUE )
      englishName <- speciesattr %>% filter (SCIENTIFIC.NAME == sp) %>% dplyr::select(ENGLISH.NAME)
      englishName <- englishName$ENGLISH.NAME
      
      write_file(paste0("<li>",englishName," (",sp,")"," Map under preparation</li>\n"), filename,append=TRUE)
      write_file("</ul>\n", filename, append = TRUE )
    }
  }
  if (newFile)
  { #newFile flag has been set. Close the existing file
    write_file("</li>\n", filename, append = TRUE)
    write_file("</ul>\n", filename, append = TRUE )
    
    write_file(footerText1, filename, append = TRUE)
    write_file(footerText2, filename, append = TRUE)
    write_file(footerText3, filename, append = TRUE)
    write_file(footerText4, filename, append = TRUE)
  }
}

