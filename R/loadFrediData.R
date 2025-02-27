### Last updated: 2022.01.10
### 2022.01.10: Added SLR sectors
### This function loads the data from a directory specified by the user. There is a default directory structure it will try to load from.
loadFrediData <- function(
    ### Directories
  dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  configDir   = "fredi"    , ### Module directory relative to dataDir
  scenarioDir = "scenarios", ### Directory to scenario relative to dataDir
  ### Config info
  configFile  = "FrEDI_config.xlsx", ### Name of Excel file with config information, relative to frediDir
  configSheet = "tableNames"       , ### Name of sheet in configFile containing table with info about tables
  ### Scenario info
  gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
  gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
  popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
  ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
  testFiles   = list(
    temp = "temp_0to6_to2300"   |> paste0(".csv"),
    gdp  = "rff_gdp_mean"       |> paste0(".csv"),
    pop  = "rff_state_pop_mean" |> paste0(".csv")
  ), ### Files to load for test scenarios
  ### Conditionals
  doScalars   = doScalars  , ### Whether or not do format scalars
  doScenarios = doScenarios, ### Whether to load scenarios
  ### Messaging
  silent      = FALSE, ### Level of messaging
  msg0        = "\t"   ### Message index
) {
  ### Set Up Environment -------------------
  #### Messaging -------------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  paste0(msg0, "Running loadFrediData...") |> message()
  
  #### File Paths -------------------
  ### Directories for scalars, GCM scaled impacts, and SLR scaled impacts
  configDir     <- dataDir   |> file.path(configDir) ### Path to Excel config file, relative to mainDir
  scenarioDir   <- dataDir   |> file.path(scenarioDir) ### Path to scenarios, relative to mainDir
  scalarDir     <- configDir |> file.path("scalars"  )
  impactDirs    <- configDir |> list.files(pattern="gcm|slr")
  
  #### Initialize List -------------------
  dataList      <- list()
  listState     <- list()
  
  ### Load Table of Tables -------------------
  ### Load table with names of data tables
  # frediFile |> print()
  if (!silent) paste0(msg1, "Loading configuration data and controls...") |> message()
  listFrEDI     <- configDir |> loadFrediConfig(
    configFile  = configFile , ### Name of Excel file with config information, relative to frediDir
    configSheet = configSheet, ### Name of sheet in configFile containing table with info about tables
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  ### Add to dataList
  dataList[["frediData"]] <- listFrEDI
  rm(listFrEDI)
  
  
  ### Load Scenarios & Pop Ratios -------------------
  if(doScenarios) {
    if (!silent) paste0(msg1, "Loading scenario data...") |> message()
    listDefaults  <- scenarioDir |> loadScenarioData(
      gcamFile    = gcamFile   , ### File in scenarioDir containing temperature scenarios
      gdpFile     = gdpFile    , ### File in scenarioDir containing GDP scenarios
      popFile     = popFile    , ### File in scenarioDir containing population scenarios
      ratiosFile  = ratiosFile , ### File in scenarioDir containing population ratios
      testFiles   = testFiles  , ### Files with scenarios for testing
      silent      = silent     , ### Level of messaging
      msg0        = msg1
    ) ### End loadFrediConfig
    ### Add to dataList
    dataList[["scenarioData"]] <- listDefaults
    rm(listDefaults)
  } ### End if(doScenarios)
  
  
  ### Load Scalars -------------------
  ### Load scalars
  if(doScalars) {
    if (!silent) paste0(msg1, "Loading scalars...") |> message()
    data_scalars <- scalarDir |> loadFrediScalars()
    listState[["scalarData"]] <- data_scalars
    rm(data_scalars)
  } ### End if(doScalars)
  
  
  ###### Load Scaled Impacts -------------------
  # ### Load GCM scaled impacts
  # if (!silent) paste0(msg1, "Loading GCM scaled impacts...") |> message()
  # data_gcmImpacts <- impactDir |> loadFrediImpacts(type="gcm")
  # listState[["gcmImpData"]] <- data_gcmImpacts
  # rm(data_gcmImpacts)
  # ### Load SLR scaled impacts
  # if (!silent) paste0(msg1, "Loading SLR scaled impacts...") |> message()
  # data_slrImpacts <- impactDir |> loadFrediImpacts(type="slr")
  # listState[["slrImpData"]] <- data_slrImpacts
  # rm(data_slrImpacts)
  ### Iterate over impact list
  listImpacts <- impactDirs |> map(function(type0){
    configDir |> loadFrediImpacts(type=type0)
  }) |> set_names(impactDirs)
  ### Add to list
  listState[["gcmImpData"]] <- listImpacts[["gcm"]]
  listState[["slrImpData"]] <- listImpacts[["slr"]]
  rm(listImpacts)
  
  ### Format Return List -------------------
  dataList[["stateData"]] <- listState
  rm(listState)
  
  ### Return -------------------
  gc()
  paste0(msg0, "...Finished running loadFrediData().", msgN) |> message()
  return(dataList)
}