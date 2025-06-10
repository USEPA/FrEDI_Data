
reshapeFrediData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent   = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 |> paste("\t")  
  # if (!silent) 
  paste0(msg0, "Running reshapeFrediData...") |> message()
  
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  frediData  <- dataList[["frediData"]]
  stateData  <- dataList[["stateData"]]
  scenarios  <- dataList[["scenarioData"]]
  # stateData |> names() |> print()
  # stateData[["scalars"]] |> glimpse()
  # stateData[["gcmImpacts"]] |> glimpse()
  # stateData[["slrImpacts"]] |> glimpse()
  
  
  ###### FrEDI Data  ######
  ### Control Tables
  frediData  <- frediData |> reshapeConfigData(silent=silent, msg0=msg1)
  dataList[["frediData"]] <- frediData
  # dataList[["frediData"]] |> names() |> print()
  
  ###### Scenarios Data  ######
  ### Reshape scenario data
  scenarios  <- scenarios |> reshapeScenarioData(
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  dataList[["scenarioData"]] <- scenarios
  
  ###### Scalar Data  ######
  ### Reshape scalar data
  # stateData |> names() |> print()
  scalarData <- stateData[["scalarData"]]
  # scalarData |> glimpse()
  scalarData <- scalarData |> reshapeScalarData(
    frediData = frediData, 
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["scalarData"]] <- scalarData
  
  ###### GCM Scaled Impacts  ######
  ### Reshape scalar data
  gcmData <- stateData[["gcmImpData"]]
  gcmData <- gcmData |> reshapeScaledImpacts(
    frediData = frediData, 
    type0     = "gcm",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["gcmImpData"]] <- gcmData
  
  ###### SLR Scaled Impacts  ######
  ### Reshape scalar data
  slrData <- stateData[["slrImpData"]]
  slrData <- slrData |> reshapeScaledImpacts(
    frediData = frediData, 
    type0     = "slr",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["slrImpData"]] <- slrData
  
  ### Update data in list
  dataList[["stateData"]] <- stateData
  # dataList[["frediData"]] |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  # if(!silent) 
  paste0(msg0, "...Finished running reshapeFrediData().", msgN) |> message()
  return(dataList)
}