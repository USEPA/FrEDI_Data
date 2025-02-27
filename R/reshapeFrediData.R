#' reshapeFrediData
#'
#' @param dataList Outputs from `loadData`
#' @param silent   Indicate level of messaging
#' @param msg0    Initial messaging prefix
#'
#' @return
#' @export
#'
#' @examples
reshapeFrediData <- function(
    dataList    = NULL , ### List of data (e.g., as returned from FrEDI_Data::loadData())
    doScalars   = TRUE , ### Whether or not do format scalars
    doScenarios = FALSE, ### Whether to load scenarios
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
  if(doScenarios) {
    scenarios  <- scenarios |> reshapeScenarioData(
      silent    = silent, 
      msg0      = msg1
    ) ### End reshapeScalarData
    dataList[["scenarioData"]] <- scenarios
  } ### End if(doScenarios)
  
  ###### Scalar Data  ######
  ### Reshape scalar data
  if(doScalars) {
    # stateData |> names() |> print()
    scalarData <- stateData[["scalarData"]]
    # scalarData |> glimpse()
    scalarData <- scalarData |> reshapeScalarData(
      frediData = frediData, 
      silent    = silent, 
      msg0      = msg1
    ) ### End reshapeScalarData
    stateData[["scalarData"]] <- scalarData
  } ### End if(doScalars)
  
  ###### GCM Scaled Impacts  ######
  ### Reshape scalar data
  mList0  <- list()
  mList0[["mType0"]] <- frediData[["co_modelTypes"]] |> pull(modelType_id) |> unique()
  mList0[["mName0"]] <- mList0[["mType0"]] |> paste0("ImpData")
  mData0  <- mList0 |> map(function(mType0, mName0){
    ### Get data
    df0 <- stateData[[mName0]]
    ### Shape data
    df0 <- df0 |> reshapeScaledImpacts(
      frediData = frediData, 
      type0     = mType0,
      silent    = silent, 
      msg0      = msg1
    ) ### End reshapeScalarData
  }) |> set_names(mList0[["mName0"]])
  # gcmData <- stateData[["gcmImpData"]]
  # doGCM   <- !(gcmData |> is.null())
  # gcmData <- gcmData |> reshapeScaledImpacts(
  #   frediData = frediData, 
  #   type0     = "gcm",
  #   silent    = silent, 
  #   msg0      = msg1
  # ) ### End reshapeScalarData
  # stateData[["gcmImpData"]] <- gcmData
  # 
  # ###### SLR Scaled Impacts  ######
  # ### Reshape scalar data
  # slrData <- stateData[["slrImpData"]]
  # slrData <- slrData |> reshapeScaledImpacts(
  #   frediData = frediData, 
  #   type0     = "slr",
  #   silent    = silent, 
  #   msg0      = msg1
  # ) ### End reshapeScalarData
  # stateData[["slrImpData"]] <- slrData
  
  ### Update data in list
  dataList[["stateData"]] <- stateData
  # dataList[["frediData"]] |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  # if(!silent) 
  paste0(msg0, "...Finished running reshapeFrediData().", msgN) |> message()
  return(dataList)
}