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
    ### Directories
  projectDir  = ".",   ### Directory in which to find the FrEDI_Data project/package
  dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  configDir   = "fredi"    , ### Module directory relative to dataDir
  scenarioDir = "scenarios", ### Directory to scenario relative to dataDir
  outDir      = "data"     , ### Directory to save outputs relative to projectDir
  ### Info on config file
  configFile  = "FrEDI_config" |> paste0(".", "xlsx"), ### Name of excel file with config information
  configSheet = "tableNames" ,  ### Name of worksheet on which to find tables
  ### Additional scenarios
  testFiles  = list(
    temp = "temp_0to6_to2300"   |> paste0(".csv"),
    gdp  = "rff_gdp_mean"       |> paste0(".csv"),
    pop  = "rff_state_pop_mean" |> paste0(".csv")
  ), ### End list
  ### Conditionals
  reshape     = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
  extend_all  = TRUE , ### Whether to extend all GCM model observations to maximum range
  doScalars   = TRUE , ### Whether or not do format scalars
  doScenarios = TRUE , ### Whether to load scenarios
  ### Info on saving
  outFile     = "tmp_sysdata" |> paste0("."), ### Name of file to which to save results
  save        = TRUE , ### Whether to save the data
  return      = TRUE , ### Whether to return object
  return_type = "db",  ### Whether to return a database or rda object
  ### Info on messaging
  silent      = TRUE , ### Level of messaging
  msg0        = ""
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
  ###### Adjust paths
  # configDir   <- dataDir    |> file.path(configDir)
  # scenarioDir <- dataDir    |> file.path(scenarioDir)
  outDir      <- projectDir |> file.path(outDir)
  outPath     <- outDir     |> file.path(paste0(outFile,return_type))
  # ### Path and file name for data inputs
  # dataInDir   <- projectDir     |> file.path("inst", "extdata")
  # dataInName  <- "FrEDI_config" |> paste0(".", "xlsx")
  # dataInPath  <- dataInDir |> file.path(dataInName)
  # ### Path and file name for saving outputs
  # dataOutDir  <- projectDir     |> file.path("data")
  # dataOutName <- "tmp_sysdata"  |> paste0(".", "rda")
  #dataOutPath <- dataOutDir     |> file.path(dataOutName)

  
  ###### 0. Load FrEDI Data Code ###### 
  ### Load FrEDI Data Code
  projectDir |> devtools::load_all()
  
  ###### 1. Configure Data ######
  list_systemData0 <- configureSystemData(
    ### Directories
    dataDir     = dataDir    , ### Directory in which to find data
    configDir   = configDir  , ### Module directory relative to dataDir
    scenarioDir = scenarioDir, ### Directory to scenario relative to dataDir
    ### Info on config file
    configFile  = configFile , ### Name of config file in configDir
    configSheet = configSheet,
    ### Additional scenarios
    testFiles   = testFiles,
    ### Conditionals
    reshape0    = reshape0   , ### Whether to include reshaped data in outputs (e.g., for testing)
    extend_all  = extend_all , ### Whether to extend all GCM model observations to maximum range
    doScalars   = doScalars  , ### Whether or not do format scalars
    doScenarios = doScenarios, ### Whether to load scenarios
    ### Info on saving
    outPath     = outPath,
    save0       = save0, 
    return0     = return0,
    return_type = return_type,
    ### Info on messaging
    silent      = silent,
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