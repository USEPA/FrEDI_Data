# ### Save plots as image files
# "." |> devtools::load_all()

###### Script for configuring data for main FrEDI
###### Load Packages
require(tidyverse)
require(openxlsx)
require(FrEDI)
require(devtools)

###### testFrediData
testFrediData <- function(
    projectDir = "."  , ### Directory in which to find the FrEDI_Data project/package
    dataList   = NULL , ### List with data (e.g., rDataList)
    general    = TRUE , ### Whether to run the general test
    figures    = TRUE , ### Whether to create scaled impact figures
    # save       = TRUE , ### Whether to save the data
    outDir     = "." |> file.path("data_tests"), ### Directory to save test results
    testFile   = "configTestResults" |> paste0(".", "xlsx"), ### Filename for general tests
    silent     = TRUE , ### Level of messaging
    msg0       = ""
){
  ###### Messaging ######
  ### Messaging
  msgN <- "\n"
  msgN |> paste0(msg0, "Running testFrediData()...") |> message()
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  
  ###### Set Arguments ######
  ###### Parameters for saving tests
  # save0       <- save; rm(save)
  # reshape0    <- reshape; rm(reshape)
  save0       <- TRUE
  msgUser     <- !silent
  
  ### Whether to include reshaped data in outputs (e.g., for testing)
  reshape0    <- dataList[["rsData"]] |> length()
  
  ###### Initialize Return List ######
  # returnList  <- list()
  
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir

  ### Path and file name for saving outputs
  # dataOutDir  <- projectDir     |> file.path("data")
  # dataOutName <- "tmp_sysdata"  |> paste0(".", "rda")
  # # dataOutName <- "sysdata"  |> paste0(".", "rda")
  # dataOutPath <- dataOutDir     |> file.path(dataOutName)
  
  ### Path for saving test outputs
  # testOutDir  <- projectDir     |> file.path("data_tests")
  # testOutFile <- "configPlotResults" |> paste0(".", "xlsx")
  # testOutPath <- testOutDir     |> file.path(testOutFile)
  
  
  ###### 0. Load FrEDI Data Code ###### 
  ### Load FrEDI Data Code
  projectDir |> devtools::load_all()
  
  ###### 1. Run General Tests on Data ######
  ### Run general tests
  if(general) {
    testResults <- general_config_test(
      configuredData = dataList,
      save           = save0,
      overwrite      = TRUE,
      # xlsxName       = testOutFile,
      save           = TRUE, 
      outPath        = outDir,
      xlsxName       = testFile,
      msg0           = msg0
    ) |> try(silent=T) ### End general_config_test
    ### Add to return list
    returnList[["generalTestResults"]] <- testResults
  } ### End if(general)
  


  ###### 2. Create Images of Scaled Impacts ######
  ### Create images
  if(figures) {
    ### Initialize figure list
    figureList   <- list()
    
    ### Create info for plot figures
    plotResults  <- list_systemData0 |> 
      get_fredi_sectorOptions_results() |> 
      try(silent=T); plotResults |> glimpse()
    ### Add to figure list
    figureList[["sectorOptions"]] <- plotResults
    
    ### Test region plot info function
    plotInfo     <- plotResults |> 
      get_region_plotInfo() |> 
      try(silent=T); plotInfo$sectorInfo |> glimpse()
    ### Add to figure list
    figureList[["plotInfo"]] <- plotInfo
    
    ### Make scaled impact plots
    plotList     <- plotResults |> 
      make_scaled_impact_plots() |> 
      try(silent=T)
    ### Add to figure list
    figureList[["plotList"]] <- plotList

    ### Save plots
    # modelTypes   <- c("GCM")
    # modelTypes   <- c("SLR")
    modelTypes   <- c("GCM", "SLR")
    for(type_i in modelTypes) {
      savePlots <- plotList |> 
        save_scaled_impact_figures(
          df0   = plotResults, 
          type0 = type_i, 
          fpath = outDir
        ) |> try(silent=T)
    } ### End for(type_i in modelTypes)
    # savePlots1   <- plotList |> save_scaled_impact_figures(df0=plotResults, type0="GCM", fpath=testOutDir) |> try(silent=T)
    # savePlots2   <- plotList |> save_scaled_impact_figures(df0=plotResults, type0="SLR", fpath=testOutDir) |> try(silent=T)
    
    ### Add to return list
    returnList[["figures"]] <- figureList
  } ### End if(figures)
  
  ###### Return ######
  paste0(msg0, "...Finished running testFrediData().") |> paste0(msgN) |> message()
  return(returnList)
}

###### End script ######