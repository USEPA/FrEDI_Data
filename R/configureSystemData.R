configureSystemData <- function(
  # fileDir     = "." |> file.path("inst", "extdata"),          ### Path to project
  ### Directories
  dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  configDir   = "fredi"    , ### Module directory relative to dataDir
  scenarioDir = "scenarios", ### Directory to scenario relative to dataDir
  ### Info on config file
  configFile  = "FrEDI_config.xlsx", ### Name of excel file with config information
  configSheet = "tableNames",        ### Sheet with table info
  ### Additional scenarios
  testFiles   = list(
    temp = "temp_0to6_to2300"   |> paste0(".csv"),
    gdp  = "rff_gdp_mean"       |> paste0(".csv"),
    pop  = "rff_state_pop_mean" |> paste0(".csv")
  ), ### Files in inst/extdata/scenarios to load for testing
  ### Conditionals
  reshape0    = TRUE , ### Whether to include reshaped data items in data list (for testing)
  extend_all  = TRUE , ### Whether to extend all GCM model observations to maximum range
  doScalars   = TRUE , ### Whether or not do format scalars
  doScenarios = TRUE , ### Whether to load scenarios
  ### Info on saving
  outPath     = "." |> file.path("data", "tmp_sysdata.rda"),  ### Where to save data
  save0       = FALSE, ### Whether to save the file
  return0     = TRUE , ### Whether to return the data list
  ### Info on messaging
  silent      = TRUE , ### Level of messaging 
  msg0        = ""     ### Message prefix
){
  ### Messaging
  msgN <- "\n"
  msgN |> paste0(msg0, "Running configureSystemData()...") |> message()
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  
  ###### Create File Paths ######
  ### Output file
  sysDataFile   <- outPath |> basename()
  sysDataPath   <- outPath |> dirname()
  sysDataFile   <- sysDataPath |> file.path(sysDataFile)
  
  ###### Initialize Object List ######
  ### Initialize list of objects to return
  rDataList     <- list()
  rDataList[["frediData"   ]] <- list(name="frediData"   , data=list())
  rDataList[["stateData"   ]] <- list(name="stateData"   , data=list())
  rDataList[["scenarioData"]] <- list(name="scenarioData", data=list())
  
  
  ###### 1. Load Excel Data ######
  ### Load state data
  # if(!silent) 
  paste0(msg1, "Loading data...") |> message()
  loadData0     <- dataDir |> loadFrediData(
    configDir   = configDir  , ### Directory containing config file, model impacts, scalars
    configFile  = configFile , ### Name of excel file with config information
    configSheet = configSheet, ### Sheet with info about tables in config file
    scenarioDir = scenarioDir, ### Directory containing scenarios
    testFiles   = testFiles  , ### Files to load for testing
    doScalars   = doScalars  , ### Whether or not do format scalars
    doScenarios = doScenarios, ### Whether to load scenarios
    silent      = silent,
    msg0        = msg1
  ) ### End loadData
  gc()
  ### Update data in list
  rDataList[["frediData"   ]] <- loadData0[["frediData"   ]]
  rDataList[["stateData"   ]] <- loadData0[["stateData"   ]]
  rDataList[["scenarioData"]] <- loadData0[["scenarioData"]]
  # loadData0[["frediData"   ]] |> names() |> print()
  # loadData0[["stateData"]][["slrImpData"]] |> names() |> print()
  # return(rDataList)
  
  ###### 2. Reshape Loaded Data ######
  ### Reshape state data
  # if(!silent) 
  paste0(msg1, "Reshaping data...") |> message()
  reshapeData0  <- loadData0 |> reshapeFrediData(
    doScalars   = doScalars  , ### Whether or not do format scalars
    doScenarios = doScenarios, ### Whether to load scenarios
    silent      = silent, 
    msg0        = msg1
  ) ### End reshapeFrediData
  gc()
  rm(loadData0)
  ### If reshape0, save the reshaped data separately
  if(reshape0){ 
    rDataList[["rsData"]] <- reshapeData0
  } else{
    rDataList[["rsData"]] <- list(name="rsData", data=list())
  } ### End if(reshape0)
  ### Update data in list
  rDataList[["frediData"   ]] <- reshapeData0[["frediData"   ]]
  rDataList[["stateData"   ]] <- reshapeData0[["stateData"   ]]
  rDataList[["scenarioData"]] <- reshapeData0[["scenarioData"]]
  # return(rDataList)
  
  
  ###### 4. Configure Data ######
  ### Names of objects to combine from reshaped data
  # if(!silent) 
  paste0(msg1, "Configuring data...") |> message()
  sysDataList0  <- reshapeData0 |> createSystemData(
    extend_all  = extend_all , ### Whether to extend values to maximum allowable value
    doScalars   = doScalars  , ### Whether or not do format scalars
    doScenarios = doScenarios, ### Whether to load scenarios
    silent = silent, 
    msg0   = msg1
  ) ### End createSystemData
  gc()
  rm(reshapeData0)
  ### Update data in list
  rDataList[["fredi_config"]] <- sysDataList0[["fredi_config"]]
  rDataList[["frediData"   ]] <- sysDataList0[["frediData"   ]]
  rDataList[["stateData"   ]] <- sysDataList0[["stateData"   ]]
  rDataList[["scenarioData"]] <- sysDataList0[["scenarioData"]]
  
  
  ###### 6. Drop Reshaped Data Objects ######
  if(!reshape0) {
    drop0         <- c("rsData")
    inDrop0       <- (rDataList |> names()) %in% drop0
    rDataList     <- rDataList
    rDataList     <- rDataList[!inDrop0]
  } ### End if(!reshape0) 
  
  ###### Save to File ######
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save0) {
    paste0(msg1, "Saving results to ", sysDataPath, "...") |> message()
    outPathExists <- sysDataPath |> dir.exists()
    fredi_config  <- rDataList[["frediData"]][["fredi_config"]]
    if(outPathExists){ 
      # save(fredi_config, rDataList, file=sysDataFile)
      save(rDataList, file=sysDataFile)
    } else{
      paste0(msg1, "Warning: outPath = ", sysDataPath, "doesn't exist!") |> message()
      paste0(msg2, "Exiting without saving...") |> message()
    } ### End if(outPathExists)
  } ### End if(save)
  
  ###### Return ######
  paste0(msg0, "...Finished running configureSystemData().") |> paste0(msgN) |> message()
  gc()
  if(return0) return(rDataList)
}