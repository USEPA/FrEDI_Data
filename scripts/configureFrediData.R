# ### Save plots as image files
# "." |> devtools::load_all()

###### Script for configuring data for main FrEDI
###### Load Packages
require(tidyverse)
require(openxlsx)
require(FrEDI)
require(devtools)

###### configureFrediData
configureFrediData <- function(
    projectDir = ".",   ### Directory in which to find the FrEDI_Data project/package
    testFiles  = list(
      temp = "temp_0to6_to2300"   |> paste0(".csv"),
      gdp  = "rff_gdp_mean"       |> paste0(".csv"),
      pop  = "rff_state_pop_mean" |> paste0(".csv")
    ), ### End list
    save       = FALSE, ### Whether to save the data
    reshape    = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
    silent     = TRUE , ### Level of messaging
    msg0       = ""
){
  ###### Messaging ######
  ### Messaging
  msgN <- "\n"
  msgN |> paste0(msg0, "Running configureFrediData()...") |> message()
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  
  ###### Set Arguments ######
  ###### Parameters for saving tests
  save0       <- save; rm(save)
  reshape0    <- reshape; rm(reshape)
  msgUser     <- !silent
  
  
  ###### Initialize Return List ######
  # returnList  <- list()
  
  
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir
  ### Path and file name for data inputs
  dataInDir   <- projectDir     |> file.path("inst", "extdata")
  dataInName  <- "FrEDI_config" |> paste0(".", "xlsx")
  dataInPath  <- dataInDir |> file.path(dataInName)
  ### Path and file name for saving outputs
  dataOutDir  <- projectDir     |> file.path("data")
  dataOutName <- "tmp_sysdata"  |> paste0(".", "rda")
  # dataOutName <- "tmp_sysdata"  |> paste0(".", "rda")
  dataOutPath <- dataOutDir     |> file.path(dataOutName)
  ### Path for saving test outputs
  testOutDir  <- projectDir     |> file.path("data_tests")
  testOutFile <- "configTestResults" |> paste0(".", "xlsx")
  testOutPath <- testOutDir     |> file.path(testOutFile)
  
  
  ###### 0. Load FrEDI Data Code ###### 
  ### Load FrEDI Data Code
  projectDir |> devtools::load_all()
  
  ###### 1. Configure Data ######
  list_systemData0 <- configureSystemData(
    fileDir     = dataInDir, 
    configFile  = dataInName, 
    configSheet = "tableNames",
    testFiles   = testFiles,
    extend_all  = T,
    silent      = T,
    save        = T, 
    outPath     = dataOutPath,
    reshape     = reshape0,
    return      = T,
    msg0        = msg0
  ) ### End configureSystemData
  ### Add to return list
  # returnList[["systemDataList"]] <- list_systemData0
  
  
  ###### Return ######
  paste0(msg0, "...Finished running configureFrediData().") |> paste0(msgN) |> message()
  return(list_systemData0)
  # return(returnList)
}

###### End script ######