# ### Save plots as image files
# "." |> devtools::load_all()

###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(FrEDI)
require(devtools)

configureFrediData <- function(
    projectDir = ".",   ### Directory in which to find the FrEDI_Data project/package
    save       = FALSE, ### Whether to save the data
    byState    = TRUE,  ### Whether to do test on a state-level...deprecated
    reshape    = TRUE   ### Whether to include reshaped data in outputs (e.g., for testing)
){
  ###### Set Arguments ######
  ###### Parameters for saving tests
  save_test   <- save
  
  
  ###### Initialize Return List ######
  returnList  <- list()
  
  
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir
  ### Path and file name for data inputs
  dataInDir   <- projectDir |> file.path("inst", "extdata")
  dataInName  <- "FrEDI_config" |> paste0(".xlsx")
  dataInPath  <- dataInDir |> file.path(dataInName)
  ### Path and file name for saving outputs
  dataOutDir  <- projectDir |> file.path("data")
  dataOutName <- "sysdata"  |> paste0(".rda")
  dataOutPath <- dataOutDir |> file.path(dataOutName)
  ### Path for saving test outputs
  testOutDir  <- projectDir |> file.path("data_tests")
  
  
  ###### 0. Load FrEDI Data Code ###### 
  ### Load FrEDI Data Code
  projectDir |> devtools::load_all()
  
  
  ###### 1. Configure Data ######
  list_systemData0 <- dataInDir |> configureSystemData(
    fileName   = dataInName, 
    save       = T, 
    silent     = T, 
    outPath    = dataOutDir |> file.path("tmp_sysdata.rda"),
    reshape    = reshape,
    extend_all = T,
    return     = T
  ) ### End configureSystemData
  ### Add to return list
  returnList[["systemDataList"]] <- list_systemData0
  
  
  ###### 2. Run General Tests on Data ######
  ### Run general tests
  test_general_config <- general_config_test(
    configuredData = list_systemData0,
    byState        = byState,
    save           = save,
    overwrite      = TRUE,
    xlsxName       = "generalConfig_testResults.xlsx",
    fredi_config   = fredi_config
  ) ### End general_config_test
  ### Add to return list
  returnList[["generalConfigTests"]] <- test_general_config
  
  
  ###### 5. Create Images of Scaled Impacts ######
  ### Test create results
  testResults  <- list_systemData0 |> get_fredi_sectorOptions_results(); testResults |> glimpse()
  ### Test region plot info
  testIter0    <- testResults |> get_region_plotInfo(); 
  # testIter0$sectorInfo |> glimpse()
  ### Make scaled impact plots 
  testPlots0   <- testResults |> make_scaled_impact_plots()
  ### Add to return list
  returnList[["testPlots0"]] <- testPlots0
  ### Save plots
  testPlots1   <- testPlots0 |> save_scaled_impact_figures(df0=testResults, modelType="GCM", fpath=testOutDir)
  testPlots2   <- testPlots0 |> save_scaled_impact_figures(df0=testResults, modelType="SLR", fpath=testOutDir)
  
  
  ###### Return ######
  return(returnList)
}

###### End script ######