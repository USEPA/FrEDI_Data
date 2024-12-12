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
    sectors    = "all", ### Which sector to run
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
  
  ### Sectors
  sectorsLC0  <- sectors |> tolower(); rm(sectors)
  doAll       <- "all" %in% sectors0
  
  ### Whether to include reshaped data in outputs (e.g., for testing)
  reshape0    <- dataList[["rsData"]] |> length()
  
  ###### Initialize Return List ######
  returnList  <- list()
  
  ###### Set Paths ######
  ###### Project path
  # projectDir  <- "."
  projectDir  <- projectDir
  
  
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
      outPath        = outDir,
      xlsxName       = testFile,
      msg0           = msg0
    ) |> try(silent=T) ### End general_config_test
    ### Add to return list
    testResults |> print()
    returnList[["generalTestResults"]] <- testResults
  } ### End if(general)
  


  ###### 2. Create Images of Scaled Impacts ######
  ### Create images
  if(figures) {
    ### Initialize figure list
    figureList   <- list()
    
    ### Create info for plot figures
    plotResults  <- dataList |> 
      get_fredi_sectorOptions_results() |> 
      try(silent=T); plotResults |> glimpse()
    plotResults |> pull(sector) |> unique() |> print()
    ### Add to figure list
    figureList[["sectorOptions"]] <- plotResults
    
    ### Test region plot info function
    plotInfo     <- plotResults |> 
      get_region_plotInfo() |> 
      try(silent=T); plotInfo[["sectorInfo"]] |> glimpse()
    ### Add to figure list
    figureList[["plotInfo"]] <- plotInfo
    
    ### Filter to specific sectors
    if(doAll) {
      plotResults <- plotResults
    } else{
      plotResults <- plotResults |> filter(sector %in% sectorsLC0)
    } ### End if(doAll)
    
    ### Check if results are present
    hasResults   <- plotResults |> length()
    if(hasResults) {
      ### Make scaled impact plots
      plotList     <- plotResults |> 
        make_scaled_impact_plots() |> 
        try(silent=T)
      plotList[[1]][[1]][[1]] |> print()
      ### Add to figure list
      figureList[["plotList"]] <- plotList
      
      ### Save plots
      # modelTypes   <- c("GCM")
      # modelTypes   <- c("SLR")
      # modelTypes   <- c("GCM", "SLR")
      modelTypes   <- plotResults |> pull(modelType) |> unique()
      for(type_i in modelTypes) {
        savePlots <- plotList |> 
          save_scaled_impact_figures(
            df0   = plotResults, 
            type0 = type_i, 
            fpath = outDir
          ) |> try(silent=T)
        outDir |> file.path("images") |> list.files() |> print()
      } ### End for(type_i in modelTypes)
      # savePlots1   <- plotList |> save_scaled_impact_figures(df0=plotResults, type0="GCM", fpath=testOutDir) |> try(silent=T)
      # savePlots2   <- plotList |> save_scaled_impact_figures(df0=plotResults, type0="SLR", fpath=testOutDir) |> try(silent=T)
    } ### End if(hasResults) 
   
    
    ### Add to return list
    returnList[["figures"]] <- figureList
  } ### End if(figures)
  
  ###### Return ######
  paste0(msg0, "...Finished running testFrediData().") |> paste0(msgN) |> message()
  return(returnList)
}

###### End script ######