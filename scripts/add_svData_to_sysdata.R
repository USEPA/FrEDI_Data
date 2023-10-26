###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
# require(devtools)

###### Set Arguments ######
###### Parameters for saving tests
save_test   <- TRUE
return_test <- TRUE

###### Set Paths ######
###### Project path
projectDir  <- "."
###### Code path
codeDir     <- projectDir |> file.path("R")
codeNames   <- codeDir    |> list.files(".R")
codePaths   <- codeDir    |> file.path(codeNames)
###### Data input path and file name
dataInDir   <- projectDir |> file.path("inst", "extdata")
dataInName  <- "FrEDI_config" |> paste0(".xlsx")
dataInPath  <- dataInDir |> file.path(dataInName)
###### Data output path and file name
dataOutDir  <- projectDir |> file.path("data")
dataOutName <- "sysdata"  |> paste0(".rda")
dataOutPath <- dataOutDir |> file.path(dataOutName)

###### 0. Local Function ###### 
for(i in codePaths){ i |> source() }

update_sysdata(
  save    = TRUE,
  sv      = TRUE,
  impacts = FALSE,
  outPath = dataOutDir
)




