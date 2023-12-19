# ### Save plots as image files
# "." |> devtools::load_all()

###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(FrEDI)
require(devtools)
# require(devtools)

configureFrediData <- function(
    projectDir = ".",
    save       = FALSE,
    byState    = TRUE,
    update_sv  = TRUE,
    reshape    = TRUE
){
  
  ###### Set Arguments ######
  ###### Parameters for saving tests
  save_test   <- save
  
  ###### Return List ######
  returnList  <- list()
  
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir
  ###### Data input path and file name
  dataInDir   <- projectDir |> file.path("inst", "extdata")
  dataInName  <- "FrEDI_config" |> paste0(".xlsx")
  dataInPath  <- dataInDir |> file.path(dataInName)
  ###### Data output path and file name
  dataOutDir  <- projectDir |> file.path("data")
  dataOutName <- "sysdata"  |> paste0(".rda")
  dataOutPath <- dataOutDir |> file.path(dataOutName)
  
  
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
    extend_all = T
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
  )
  
  ### Add to return list
  returnList[["generalConfigTests"]] <- test_general_config
  
  ###### 3. Drop Reshaped Data ######
  rDataList    <- list_systemData0[["rDataList"]]
  drop0        <- c("rsData_reg", "rsData_state")
  inDrop0      <- (rDataList |> names()) %in% drop0
  rDataList    <- rDataList[!inDrop0]
  ### Update list
  list_systemData0[["rDataList"]] <- rDataList
  ### Save list
  sysDataFile  <- dataOutDir |> file.path("tmp_sysdata.rda")
  save(fredi_config, rDataList, file=sysDataFile)
  
  ###### 4. Add SV Data ######
  ### Update system data: Added in another step
  # if(update_sv){
  #   update_sysdata(
  #     save    = TRUE,
  #     outPath = dataOutDir
  #   ) ### End update_sysdata
  # } ### End if(update_sv)
  
  ###### 5. Create Images of Scaled Impacts ######
  ### Test create results
  testResults  <- list_systemData0 |> get_fredi_sectorOptions_results(); testResults |> glimpse()
  
  ### Test region plot info
  testIter0    <- testResults |> get_region_plotInfo(); #testIter0$sectorInfo |> glimpse()
  ### Test make plots
  testPlots0   <- testResults |> make_scaled_impact_plots()
  ### General tests
  returnList[["testPlots0"]] <- testPlots0
  
  ### Test save plots
  testPlots1   <- testPlots0  |> save_scaled_impact_figures(df0=testResults, modelType="GCM", fpath=projectDir |> file.path("data_tests"))
  testPlots2   <- testPlots0  |> save_scaled_impact_figures(df0=testResults, modelType="SLR", fpath=projectDir |> file.path("data_tests"))
  
  # ### Test get plots
  # testPlots    <- list_systemData0 |> get_scaled_impact_plots(save=T)
  # ### Test get plots
  # testPlots    <- list_systemData0 |> get_scaled_impact_plots(save=T)
  # 
  
  ###### Return ######
  return(returnList)
}

###### End script ######