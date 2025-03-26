### Last updated: 2022.01.10
### 2022.01.10: Added SLR sectors
### This function loads the data from a directory specified by the user. There is a default directory structure it will try to load from.
# gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
# gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
# popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
# ### Other args
# ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
# testFiles   = list(
#   temp = "temp_0to6_to2300"   |> paste0(".csv"),
#   gdp  = "rff_gdp_mean"       |> paste0(".csv"),
#   pop  = "rff_state_pop_mean" |> paste0(".csv")
# ), ### Files to load for test scenarios
loadFrediData <- function(
    dataDir     = "." |> file.path("inst", "extdata", "fredi"),  ### Path to directory containing configuration data
    configFile  = "FrEDI_config.xlsx", ### Name of Excel file with config information, relative to frediDir
    configSheet = "tableNames"       , ### Name of sheet in configFile containing table with info about tables
    controlData = NULL   , ### Output of configureControlTables
    module      = "fredi", ### Module string
    doScalars   = TRUE   , ### Whether to load scalars
    silent      = FALSE  , ### Level of messaging
    msg0        = 0        ### Message index
) {
  ### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msgUser       <- !silent
  if(msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Running loadFrediData...") |> message()
  
  ### File Paths ----------------
  # ### Directories for scalars, GCM scaled impacts, and SLR scaled impacts
  # frediDir      <- dataDir  |> file.path("fredi"    ) ### Path to Excel config file, relative to dataDir
  # scenarioDir   <- dataDir  |> file.path("scenarios") ### Path to scenarios, relative to dataDir
  scalarDir     <- dataDir |> file.path("scalars")
  # impactDir     <- dataDir |> file.path("scalars")

  ### Columns & Values ----------------
  moduleLC      <- module   |> tolower()
  configLStr0   <- moduleLC |> paste0("Data")
  stateLStr0    <- "stateData"
  
  ### Initialize List ----------------
  dataList      <- list()
  stateData     <- list()
  
  ### Load Table of Tables ----------------
  ### Load table with names of data tables
  # frediFile |> print()
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading configuration data and controls...") |> message()
  configList    <- dataDir |> loadFrediConfig(
    # configDir   = dataDir   , ### Path to Excel config file relative to dataDir
    configFile  = configFile , ### Name of Excel file with config information, relative to frediDir
    configSheet = configSheet, ### Name of sheet in configFile containing table with info about tables
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  ### Add to dataList
  dataList[[configLStr0]] <- configList
  rm(configList)
 

  ### Load Default Scenarios & Pop Ratios ----------------
  # if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading scenario data...") |> message()
  # listDefaults  <- loadScenarioData(
  #   scenarioDir = scenarioDir, ### Path to Excel config file relative to dataDir
  #   gcamFile    = gcamFile   , ### File in scenarioDir containing temperature scenarios
  #   gdpFile     = gdpFile    , ### File in scenarioDir containing GDP scenarios
  #   popFile     = popFile    , ### File in scenarioDir containing population scenarios
  #   ratiosFile  = ratiosFile , ### File in scenarioDir containing population ratios
  #   testFiles   = testFiles  , ### Files with scenarios for testing
  #   silent      = silent     , ### Level of messaging
  #   msg0        = msg1
  # ) ### End loadFrediConfig
  # ### Add to dataList
  # dataList[["scenarioData"]] <- listDefaults
  # rm(listDefaults)
  
 
  ### Load Scalars ----------------
  ### Load scalars
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading scalars...") |> message()
  if(doScalars) {
    scalarData <- scalarDir |> loadFrediScalars()
    stateData[["scalarData"]] <- scalarData
    rm(scalarData)
  } ### End if(doScalars)
  
  ### Load Scaled Impacts ----------------
  ### Load GCM scaled impacts
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading scaled impacts...") |> message()
  modTypes0     <- controlData[["co_moduleModTypes"]] |> 
    filter(module %in% moduleLC) |> 
    pull(model_type) |> unique() |> 
    tolower()
  impactData    <- modTypes0 |> map(function(type0, dir0=dataDir){
    if (!silent) msg2 |> get_msgPrefix(newline=F) |> paste0("Loading ", type0, " scaled impacts...") |> message()
    dataDir |> file.path(type0) |> loadFrediImpacts(type=type0)
  }) |> set_names(modTypes0 |> paste0("ImpData"))
  stateData <- stateData |> c(impactData)
  rm(impactData)
  # data_gcmImpacts <- impactDir |> loadFrediImpacts(type="gcm")
  # ### Load SLR scaled impacts
  # if (!silent) pmsg1 |> get_msgPrefix(newline=F) |> paste0("Loading SLR scaled impacts...") |> message()
  # data_slrImpacts <- impactDir |> loadFrediImpacts(type="slr")
  
  # ### Create a list
  # stateData       <- list()
  # stateData[["scalarData"]] <- data_scalars
  # stateData[["gcmImpData"]] <- data_gcmImpacts
  # stateData[["slrImpData"]] <- data_slrImpacts
  # rm(data_scalars, data_gcmImpacts, data_slrImpacts)
  
  ### Add to dataList
  dataList[["stateData"]] <- stateData
  rm(stateData)
  
  ### Return ----------------
  paste0(msg0, "...Finished running loadFrediData().", msgN) |> message()
  return(dataList)
}