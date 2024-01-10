###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(devtools)

###### Set Arguments ######
###### Parameters for saving tests
add_svData_to_sysdata <- function(
    projectDir = ".",
    save       = TRUE
){
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir
  
  ###### Data output path and file name
  dataOutDir  <- projectDir |> file.path("data")
  # dataOutName <- "sysdata"  |> paste0(".rda")
  # dataOutPath <- dataOutDir |> file.path(dataOutName)
  
  ###### 0. Local Function ###### 
  projectDir |> load_all()
  
  ###### 1. Add SV Data to sysdata.rda ###### 
  update_sysdata(
    save    = save,
    sv      = TRUE,
    outPath = dataOutDir
  )
  
}





