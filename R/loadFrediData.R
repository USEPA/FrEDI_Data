### Last updated: 2022.01.10
### 2022.01.10: Added SLR sectors
### This function loads the data from a directory specified by the user. There is a default directory structure it will try to load from.
loadFrediData <- function(
    mainDir     = "." |> file.path("inst", "extdata"),  ### Path to directory containing configuration data
    ### Files
    configFile  = "FrEDI_config.xlsx",                  ### Name of Excel file with config information, relative to frediDir
    configSheet = "tableNames",                         ### Name of sheet in configFile containing table with info about tables
    gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
    gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
    popFile     = "ICLUS_%_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
    ### Other args
    ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
    testFiles   = list(
      temp = "temp_0to6_to2300"   |> paste0(".csv"),
      gdp  = "rff_gdp_mean"       |> paste0(".csv"),
      pop  = "rff_%_pop_mean" |> paste0(".csv")
    ), ### Files to load for test scenarios
    silent      = FALSE, ### Level of messaging
    msg0        = "\t"   ### Message index
) {
  ###### Messaging ######
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  paste0(msg0, "Running loadFrediData...") |> message()
  
  ###### File Paths ######
  ### Directories for scalars, GCM scaled impacts, and SLR scaled impacts
  frediDir      <- mainDir  |> file.path("fredi"    ) ### Path to Excel config file, relative to mainDir
  scenarioDir   <- mainDir  |> file.path("scenarios") ### Path to scenarios, relative to mainDir
  scalarDir     <- frediDir |> file.path("scalars"  )
  impactDir     <- frediDir
  ### National Directories for scalars, GCM scaled impacts, and SLR scaled impacts
  nat_mainDir       <- frediDir  |> file.path("national")
  nat_frediDir      <- nat_mainDir  |> file.path("fredi"    ) ### Path to Excel config file, relative to mainDir
  nat_scenarioDir   <- nat_mainDir  |> file.path("scenarios") ### Path to scenarios, relative to mainDir
  nat_scalarDir     <- nat_mainDir  |> file.path("scalars"  )
  nat_impactDir     <- frediDir
  
  ###### Initialize List ######
  dataList      <- list()
  nat_dataList  <- list()
  ###### Load Table of Tables ######
  ### Load table with names of data tables
  # frediFile |> print()
  if (!silent) paste0(msg1, "Loading configuration data and controls...") |> message()
  listFrEDI     <- loadFrediConfig(
    configDir   = frediDir   , ### Path to Excel config file relative to mainDir
    configFile  = configFile , ### Name of Excel file with config information, relative to frediDir
    configSheet = configSheet, ### Name of sheet in configFile containing table with info about tables
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  
  ### Load National table with names of data tables
  listFrEDI_nat    <- loadFrediConfig(
    configDir   = nat_mainDir   , ### Path to Excel config file relative to mainDir
    configFile  = configFile , ### Name of Excel file with config information, relative to frediDir
    configSheet = configSheet, ### Name of sheet in configFile containing table with info about tables
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  )
  
  
  ### Add to dataList
  dataList[["frediData"]] <- listFrEDI
  nat_dataList[["frediData"]] <- listFrEDI_nat
  rm(listFrEDI)
 

  ###### Load Default Scenarios & Pop Ratios ######
  if (!silent) paste0(msg1, "Loading scenario data...") |> message()
  test_state <- testFiles
  test_state$pop <-gsub("%","state",test_state$pop) 
  listDefaults  <- loadScenarioData(
    scenarioDir = scenarioDir, ### Path to Excel config file relative to mainDir
    gcamFile    = gcamFile   , ### File in scenarioDir containing temperature scenarios
    gdpFile     = gdpFile    , ### File in scenarioDir containing GDP scenarios
    popFile     =  gsub("%","State",popFile)  , ### File in scenarioDir containing population scenarios
    ratiosFile  = ratiosFile , ### File in scenarioDir containing population ratios
    testFiles   = test_state , ### Files with scenarios for testing
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  ### Add to dataList
  dataList[["scenarioData"]] <- listDefaults
  rm(listDefaults)
  
  test_nat <- testFiles
  test_nat$pop <-gsub("%","CONUS",test_nat$pop) 
  listDefaults_nat  <- loadScenarioData(
    scenarioDir = scenarioDir, ### Path to Excel config file relative to mainDir
    gcamFile    = gcamFile   , ### File in scenarioDir containing temperature scenarios
    gdpFile     = gdpFile    , ### File in scenarioDir containing GDP scenarios
    popFile     =  gsub("%","CONUS",popFile)  , ### File in scenarioDir containing population scenarios
    ratiosFile  = ratiosFile , ### File in scenarioDir containing population ratios
    testFiles   = test_nat , ### Files with scenarios for testing
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  ### Add to dataList
  nat_dataList[["scenarioData"]] <- listDefaults_nat

 
  ###### Load Scalars & Scaled Impacts ######
  ### Load scalars
  if (!silent) paste0(msg1, "Loading scalars...") |> message()
  data_scalars    <- scalarDir |> loadFrediScalars()
  nat_data_scalars    <- nat_scalarDir |> loadFrediScalars_nat()
  
  ### Load GCM scaled impacts
  if (!silent) paste0(msg1, "Loading GCM scaled impacts...") |> message()
  data_gcmImpacts <- impactDir |> loadFrediImpacts(type="gcm")
  nat_data_gcmImpacts <- nat_impactDir |> loadFrediImpacts_nat(type="gcm")
  
  ### Load SLR scaled impacts
  if (!silent) paste0(msg1, "Loading SLR scaled impacts...") |> message()
  data_slrImpacts <- impactDir |> loadFrediImpacts(type="slr")
  nat_data_slrImpacts <- impactDir |> loadFrediImpacts_nat(type="slr")
  
  ### Create a list
  listState       <- list()
  listState[["scalarData"]] <- data_scalars
  listState[["gcmImpData"]] <- data_gcmImpacts
  listState[["slrImpData"]] <- data_slrImpacts
  
  listNat       <- list()
  listNat[["scalarData"]] <- nat_data_scalars
  listNat[["gcmImpData"]] <- nat_data_gcmImpacts
  listNat[["slrImpData"]] <- nat_data_slrImpacts
  
  
  # rm(data_scalars, data_gcmImpacts, data_slrImpacts)
  rm(data_scalars, data_slrImpacts)
  
  ### Add to dataList
  dataList[["stateData"]] <- listState
  nat_dataList[["natData"]] <- listNat
  
  dataList[["national"]] <- nat_dataList
  rm(listState)
  
  ###### Return ######
  paste0(msg0, "...Finished running loadFrediData().", msgN) |> message()
  return(dataList)
}
