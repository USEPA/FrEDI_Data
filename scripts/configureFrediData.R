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
  returnList  <- list()
  
  
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
  
  
  ###### 2. Run General Tests on Data ######
  ### Run general tests
  test_general_config <- general_config_test(
    configuredData = list_systemData0,
    save           = save0,
    overwrite      = TRUE,
    xlsxName       = testOutFile,
    msg0        = msg0
  ) ### End general_config_test
  ### Add to return list
  returnList[["generalConfigTests"]] <- test_general_config
  
  
  ###### 5. Create Images of Scaled Impacts ######
  ### Test create results
  testResults  <- list_systemData0 |> get_fredi_sectorOptions_results(); testResults |> glimpse()
  # ### Test region plot info
  # testIter0    <- testResults |> get_region_plotInfo(); 
  # # testIter0$sectorInfo |> glimpse()
  ### Make scaled impact plots 
  testPlots0   <- testResults |> make_scaled_impact_plots()
  ### Add to return list
  returnList[["testPlots0"]] <- testPlots0
  ### Save plots
  testPlots1   <- testPlots0 |> save_scaled_impact_figures(df0=testResults, type0="GCM", fpath=testOutDir)
  testPlots2   <- testPlots0 |> save_scaled_impact_figures(df0=testResults, type0="SLR", fpath=testOutDir)
  
  
  ###### Return ######
  paste0(msg0, "...Finished running configureFrediData().") |> paste0(msgN) |> message()
  return(returnList)
}

###### End script ######