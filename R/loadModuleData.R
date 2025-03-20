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
loadModuleData <- function(
    module      = "fredi", ### Module string
    dataDir     = "." |> file.path("inst", "extdata", "fredi"),  ### Path to directory containing configuration data
    configFile  = "FrEDI_config.xlsx", ### Name of Excel file with config information, relative to frediDir
    configSheet = "tableNames"       , ### Name of sheet in configFile containing table with info about tables
    controlData = NULL   , ### Output of configureControlTables
    doScalars   = TRUE   , ### Whether to load scalars
    silent      = FALSE  , ### Level of messaging
    msg0        = 0      , ### Message index
    .testing    = FALSE
) {
  ### Set Up Environment ----------------
  #### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msgUser       <- !silent
  # if(msgUser) 
  msg0 |> get_msgPrefix(newline=F) |> paste0("Loading module data...") |> message()

  #### Columns & Values ----------------
  moduleLC      <- module   |> tolower()
  # configLStr0   <- moduleLC |> paste0("Data")
  configLStr0   <- "configData"
  stateLStr0    <- "stateData"
  
  #### Initialize List ----------------
  dataList      <- list()
  stateData     <- list()
  
  ### Add config data to list
  fredi_config  <- frediConfig()
  dataList[["fredi_config"]] <- fredi_config
  
  ### Load Data ----------------
  #### Load Config Data ----------------
  #### Load table from Excel with names of data tables, and read in tables
  # frediFile |> print()
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading configuration data and controls...") |> message()
  configList    <- dataDir |> loadFrediConfig(
    # configDir   = dataDir   , ### Path to Excel config file relative to dataDir
    configFile  = configFile , ### Name of Excel file with config information, relative to frediDir
    configSheet = configSheet, ### Name of sheet in configFile containing table with info about tables
    silent      = silent     , ### Level of messaging
    msg0        = msg1,
    .testing    = .testing
  ) ### End loadFrediConfig
  ### Add to dataList
  dataList[[configLStr0]] <- configList
  rm(configList)
  
 
  #### Load Scalars ----------------
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading scalars...") |> message()
  if(doScalars) {
    # scalarDir  <- dataDir |> file.path("scalars")
    scalarData <- dataDir |> file.path("scalars") |> loadFrediScalars()
    stateData[["scalarData"]] <- scalarData
    rm(scalarData)
  } ### End if(doScalars)
  
  #### Load Scaled Impacts ----------------
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("Loading scaled impacts...") |> message()
  modTypes0     <- controlData[["co_moduleModTypes"]] |> 
    filter(module %in% moduleLC) |> 
    pull(model_type) |> unique() |> 
    tolower()
  impactNames   <- modTypes0 |> paste0("Data")
  impactData    <- modTypes0 |> map(function(type0){
    if (!silent) msg2 |> get_msgPrefix(newline=F) |> paste0("Loading ", type0, " scaled impacts...") |> message()
    dataDir |> file.path(type0) |> loadFrediImpacts(type=type0)
  }) |> set_names(impactNames)
  stateData <- stateData |> c(impactData)
  rm(impactData)

  
  ### Add stateData to Data List ----------------
  dataList[["stateData"]] <- stateData
  rm(stateData)
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished loading module data.", msgN) |> message()
  return(dataList)
}