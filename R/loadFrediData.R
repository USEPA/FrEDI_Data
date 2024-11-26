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
    popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
    ### Other args
    ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
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

  ###### Initialize List ######
  dataList      <- list()
  
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
  ### Add to dataList
  dataList[["frediData"]] <- listFrEDI
  rm(listFrEDI)
 

  ###### Load Default Scenarios & Pop Ratios ######
  if (!silent) paste0(msg1, "Loading scenario data...") |> message()
  listDefaults  <- loadScenarioData(
    scenarioDir = scenarioDir, ### Path to Excel config file relative to mainDir
    gcamFile    = gcamFile   , ### File in scenarioDir containing temperature scenarios
    gdpFile     = gdpFile    , ### File in scenarioDir containing GDP scenarios
    popFile     = popFile    , ### File in scenarioDir containing population scenarios
    ratiosFile  = ratiosFile , ### File in scenarioDir containing population ratios
    silent      = silent     , ### Level of messaging
    msg0        = msg1
  ) ### End loadFrediConfig
  ### Add to dataList
  dataList[["scenarioData"]] <- listDefaults
  rm(listDefaults)
  
 
  ###### Load Scalars & Scaled Impacts ######
  ### Load scalars
  if (!silent) paste0(msg1, "Loading scalars...") |> message()
  data_scalars    <- scalarDir |> loadFrediScalars()
  
  ### Load GCM scaled impacts
  if (!silent) paste0(msg1, "Loading GCM scaled impacts...") |> message()
  data_gcmImpacts <- impactDir |> loadFrediImpacts(type="gcm")
  
  ### Load SLR scaled impacts
  if (!silent) paste0(msg1, "Loading SLR scaled impacts...") |> message()
  data_slrImpacts <- impactDir |> loadFrediImpacts(type="slr")
  
  ### Create a list
  listState       <- list()
  listState[["scalarData"]] <- data_scalars
  listState[["gcmImpData"]] <- data_gcmImpacts
  listState[["slrImpData"]] <- data_slrImpacts
  # rm(data_scalars, data_gcmImpacts, data_slrImpacts)
  rm(data_scalars, data_slrImpacts)
  
  ### Add to dataList
  dataList[["stateData"]] <- listState
  rm(listState)
  
  ###### Return ######
  paste0(msg0, "...Finished running loadFrediData().", msgN) |> message()
  return(dataList)
}