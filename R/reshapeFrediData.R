#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeFrediData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent   = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeFrediData:") |> message()
  
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  frediData <- dataList[["frediData"]]
  stateData <- dataList[["stateData"]][["data"]]
  # stateData |> names() |> print()
  # stateData[["scalars"]] |> glimpse()
  # stateData[["gcmImpacts"]] |> glimpse()
  # stateData[["slrImpacts"]] |> glimpse()
  
  # listNames <- dataList |> names()
  # listNames |> print()
  # for(name_i in listNames) {name_i |> assign(dataList[[name_i]])}
  # dataList |> list2env(envir = environment())
  
  ### Ensure all dataframes are tibbles
  # names0   <- dataList |> names()
  # for(list_i in listNames) { for(name_j in list_i) { name_j |> assign(list_i[[name_j]]); rm(name_j)}; rm(list_i) }
  # # for(name_i in names0) {
  # #   data_i <- dataList[[name_i]]
  # #   isDf_i <- data_i |> is.data.frame()
  # #   if(isDf_i) data_i <- data_i |> as_tibble()
  # #   dataList[[name_i]] <- data_i
  # #   rm(name_i, data_i, isDf_i)
  # # } ### End for(name_i in names0) 
  
  ###### FrEDI Data  ######
  ### Control Tables
  frediData  <- frediData |> reshapeConfigData(silent=silent, msg0=msg1)
  dataList[["frediData"]] <- frediData
  
  ###### Scalar Data  ######
  ### Reshape scalar data
  # stateData |> names() |> print()
  scalarData <- stateData[["scalars"]]
  # scalarData |> glimpse()
  scalarData <- scalarData |> reshapeScalarData(
    frediData = frediData, 
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["scalars"]] <- scalarData
  
  ###### GCM Scaled Impacts  ######
  ### Reshape scalar data
  gcmData <- stateData[["gcmImpacts"]]
  gcmData <- gcmData |> reshapeScaledImpactsData(
    frediData = frediData, 
    type0     = "gcm",
    silent    = silent, 
    msg0      = msg1
  ) ### End reshapeScalarData
  stateData[["gcmImpacts"]] <- gcmData
  
  
  ###### SLR Scaled Impacts  ######
  ### Reshape scalar data
  slrData <- stateData[["slrImpacts"]]
  slrData <- slrData |> reshapeScaledImpactsData(
    frediData = frediData, 
    type0     = "slr",
    silent    = silent
  ) ### End reshapeScalarData
  stateData[["slrImpacts"]] <- slrData
  
  ### Update data in list
  dataList[["stateData"]][["data"]] <- stateData
  
  ###### Return ######
  ### Return the list of dataframes
  return(dataList)
}