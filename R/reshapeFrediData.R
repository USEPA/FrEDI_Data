
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
  frediData  <- dataList$frediData
  stateData  <- dataList$stateData
  scenarios  <- dataList$scenarioData
  national   <- dataList$national

  
  ###### FrEDI Data  ######
  ### Control Tables
  frediData     <- reshapeConfigData(
                          dataList = frediData,
                           silent=silent, 
                                      msg0=msg1)
  
  frediData_nat <- national$frediData |> 
                    reshapeConfigData(silent = silent,
                                      msg0 = msg1)
  
  dataList$frediData <- frediData
  dataList$national$frediData <- frediData_nat
  
  # dataList[["frediData"]] |> names() |> print()
  
  ###### Scenarios Data  ######
  ### Reshape scenario data
  scenarios  <- reshapeScenarioData(
    scenarioData = scenarios, ### List with scenario data
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  
  scenarios_nat  <- reshapeScenarioData(
    scenarioData = national$scenarioData, ### List with scenario data
    silent    = silent, 
    msg0      = msg1,
    nat_status = TRUE
  ) ### End reshapeScalarData
  
  
  dataList$scenarioData <- scenarios
  dataList$national$scenarioData <- scenarios_nat

  ###### Scalar Data  ######
  ### Reshape scalar data
  # stateData |> names() |> print()
  # scalarData |> glimpse()
  ######## State Scalar ########
  scalarData_state <- reshapeScalarData(
    scalarData = stateData$scalarData,
    frediData = frediData, 
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["scalarData"]] <- scalarData_state
  
  ######## National Scalars #######
  scalarData_nat <-  reshapeScalarData(
    scalarData = national$natData$scalarData,
    frediData = national$frediData, 
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  dataList$national$natData$scalarData <- scalarData_nat
  
  
  
  ###### GCM Scaled Impacts  ######
  ### Reshape scalar data
  gcmData <- reshapeScaledImpacts(
    impacts    = stateData$gcmImpData,
    frediData = frediData, 
    type0     = "gcm",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["gcmImpData"]] <- gcmData
  
  ### Reshape National scalar data
  gcmData_nat <- national$natData$gcmImpData |>
                 mutate(
                   state = "National",
                   postal = "NAT"
                 )
  gcmData_nat <- reshapeScaledImpacts(
    impacts    = gcmData_nat,
    frediData = national$frediData, 
    type0     = "gcm",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  dataList$national$natData$gcmImpData <- gcmData_nat
  
  
  ###### SLR Scaled Impacts  ######
  ### Reshape scalar data
  slrData <- reshapeScaledImpacts(
    impacts   = stateData$slrImpData,
    frediData = frediData, 
    type0     = "slr",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["slrImpData"]] <- slrData
  
  ### Reshape scalar data
  slrData_nat <- national$natData$slrImpData|>
    mutate(
      state = "National",
      postal = "NAT"
    )
  
  slrData_nat <- reshapeScaledImpacts(
    impacts    = slrData_nat,
    frediData = national$frediData, 
    type0     = "slr",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  dataList$national$natData$slrImpData <- slrData_nat
  
  ### Update data in list
  dataList[["stateData"]] <- stateData
  # dataList[["frediData"]] |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  # if(!silent) 
  paste0(msg0, "...Finished running reshapeFrediData().", msgN) |> message()
  return(dataList)
}