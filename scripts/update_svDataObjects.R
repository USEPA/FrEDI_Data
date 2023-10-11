###### Function to update SV data objects
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(devtools)



update_svDataObjects <- function(
    fpath0  = ".",
    sv      = TRUE,
    pop     = TRUE,
    format  = TRUE,
    save    = TRUE,
    return  = TRUE
){
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- fpath0
  ###### Code path
  codeDir     <- projectDir |> file.path("R")
  codeNames   <- codeDir    |> list.files(".R", full.names = T)
  codePaths   <- codeDir    |> file.path(codeNames)
  ###### Data output path and file name
  dataOutDir  <- projectDir |> file.path("data", "sv")
  
  ###### 0. Load Local Functions ###### 
  for(code_i in codePaths){ code_i |> source() }
  
  ###### 1. Create SV Data ###### 
  test_svData <- createSVData(
    projectPath = projectDir,
    outPath     = dataOutDir,
    sv          = sv, 
    pop         = pop,
    format      = format,
    impacts     = F,
    save        = save, 
    return      = return
  )
  return(test_svData)
}



