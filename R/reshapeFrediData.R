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
#' update_sectorInfo
reshapeFrediData <- function(
    module      = "fredi",
    dataList    = NULL , ### List of config data for module (outputs of reshapeFrediConfig())
    controlData = NULL , ### Control tables (output of configureControlTables)
    doScalars   = TRUE , ### Whether to load scalars
    silent      = TRUE , ### Level of messaging
    msg0        = 0      ### Prefix for messaging
) {
  ### Set Up Environment ----------------
  #### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msg0 |> get_msgPrefix(newline=F) |> paste0("Running reshapeFrediData()...") |> message()
  
  #### Columns & Values ----------------
  ### Module and list object names
  moduleLC      <- module   |> tolower()
  # listStr0      <- "rDataList"
  # listStr0      <- moduleLC |> paste0("Data")
  # configLStr0   <- moduleLC |> paste0("Data")
  fConfigStr0   <- "fredi_config"
  configLStr0   <- "configData"
  stateLStr0    <- "stateData"
  
  ### Values
  minYr0        <- dataList[[fConfigStr0]][["minYear0"]]
  maxYr0        <- dataList[[fConfigStr0]][["npdYear0"]]
  
  ### Lists
  stateData     <- list()
  
  ### Module info
  modules0      <- controlData[["co_moduleInfo"]] |> pull(module)
  for(moduleX in modules0){moduleX |> paste0("Str0") |> assign(moduleX)}
  doFredi       <- moduleLC |> str_detect(frediStr0   )
  doExtremes    <- moduleLC |> str_detect(extremesStr0)
  doGHG         <- moduleLC |> str_detect(ghgStr0     )
  doSV          <- moduleLC |> str_detect(svStr0      )
  
  
  ### Reshape Config Data ----------------
  configData  <- module |> reshapeConfigData(
    dataList    = dataList[[configLStr0]],
    controlData = controlData,
    minYr0      = minYr0,
    maxYr0      = maxYr0,
    silent      = silent, 
    msg0        = msg1
  ) ### End reshapeConfigData
  dataList[[configLStr0]] <- configData
  # dataList[[configLStr0]] |> names() |> print()
  
  ### Reshape Scalar Data ----------------
  ### Reshape scalar data
  # stateData |> names() |> print()
  # scalarData |> glimpse()
  # dataList[[stateLStr0]] |> glimpse()
  # return()
  scalarData <- dataList[[stateLStr0]][["scalarData"]] |> reshapeScalarData(
    controlData = controlData,
    scalarInfo  = configData[["co_scalarInfo"]],
    minYr0      = minYr0,
    maxYr0      = maxYr0,
    yrCol0      = "year",
    valCol0     = "value",
    natStr0     = "US",
    silent      = silent, 
    msg0        = msg1
  ) ### End reshapeScalarData
  # stateData[["df_scalars"]] <- scalarData
  # dataList[[stateLStr0]][["scalarData"]] <- scalarData
  stateData[["scalarData"]] <- scalarData
  # dataList[[stateLStr0]] <- stateData
  rm(scalarData)
  # return(dataList)
  
  
  ### Reshape Scaled Impacts ----------------
  ### GCM Scaled Impacts 
  # dataList[[stateLStr0]][["gcmData"]] |> glimpse()
  # gcmData <- dataList[[stateLStr0]][["gcmData"]] |> reshapeScaledImpacts(frediData=frediData, type0="gcm", silent=silent, msg0=msg1)
  # dataList[[stateLStr0]][["gcmData"]] <- gcmData
  # rm(gcmData)
  ### SLR Scaled Impacts 
  # dataList[[stateLStr0]][["slrData"]] |> glimpse()
  # slrData <- dataList[[stateLStr0]][["slrData"]] |> reshapeScaledImpacts(frediData=frediData, type0="gcm", silent=silent, msg0=msg1)
  # dataList[[stateLStr0]][["slrData"]] <- slrData
  # rm(slrData)
  if(msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("Reshaping scaled impacts...") |> message()
  modTypes0     <- controlData[["co_moduleModTypes"]] |> 
    filter(module %in% moduleLC) |> 
    pull(model_type) |> unique() |> 
    tolower()
  impactNames   <- modTypes0 |> paste0("Data")
  # dataList[[stateLStr0]] |> glimpse()
  # dataList[[stateLStr0]][impactNames] |> glimpse()
  impactData    <- list(type0=modTypes0, name0=impactNames) |> pmap(function(type0, name0){
    type0 |> reshapeScaledImpacts(
      impacts     = dataList[[stateLStr0]][[name0]], 
      controlData = controlData, 
      xCol0       = case_when(type0 |> str_detect("slr") ~ "year", .default = "modelUnitValue"),
      yCol0       = "scaled_impacts",
      valCol0     = "value",
      idCol0      = "scenario_id",
      modCol0     = "model",
      mTypeCol0  <- "model_type",
      idCols0     = c("sector", "variant", "impactType", "impactYear", "region", "postal"),
      minYr0      = minYr0,
      maxYr0      = maxYr0,
      silent      = silent,
      msg0        = msg2   
    ) ### End reshapeScaledImpacts
  }) |> set_names(impactNames)
  stateData <- stateData |> c(impactData)
  rm(impactData)
  
  ### Update Sectors Info ----------------
  if(msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("Updating sector info...") |> message()
  sectorsInfo   <- modTypes0 |> update_sectorInfo(
    df0      = dataList[[configLStr0]][["co_sectorsInfo"]], ### co_sectorsInfo output from reshapeConfigData
    list0    = stateData[impactNames], ### Impacts list output from reshapeScaledImpacts
    typeCol0 = "model_type",
    idCol0   = "scenario_id",
    valCol0  = "hasScenario",
    nameStr0 = "Data"
  ) ### End update_sectorInfo
  dataList[[configLStr0]][["co_sectorsInfo"]] <- sectorsInfo
  
  ### Update State Data ----------------
  dataList[["stateData"]] <- stateData
  # dataList[["frediData"]] |> names() |> print()
  
  ### Return ----------------
  ### Return the list of dataframes
  # if(!silent) 
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running reshapeFrediData().", msgN) |> message()
  return(dataList)
}