###### Function to update SV data objects
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(devtools)



update_svImpactLists <- function(
    fpath0  = ".",
    sectors = NULL,
    save    = T,
    return  = F
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
    sv          = F, 
    pop         = F,
    format      = F,
    impacts     = T,
    sectors     - sectors,
    save        = save, 
    return      = return
  )
  return(test_svData)
}



