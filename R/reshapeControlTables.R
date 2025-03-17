#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeConfigData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent   = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ### Set up Environment ----------------
  #### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "Running reshapeControlTables()...") |> message()
  if (!silent) paste0(msg1, "Reshaping data from control tables file...") |> message()
  
  ### Import Functions from FrEDI ----------------
  # get_matches        <- "get_matches"        |> utils::getFromNamespace("FrEDI")
  # get_co_sectorsInfo <- "get_co_sectorsInfo" |> utils::getFromNamespace("FrEDI")
  
  #### Assign Objects ----------------
  # dataList0 <- dataList
  ### Assign tables in dataList to object in local environment
  # listNames     <- dataList |> names()
  # dataList |> names() |> print()
  for(name_i in dataList |> names()) {name_i |> assign(dataList[[name_i]]); rm(name_i)}
  # dataList |> list2env(envir = environment())
  
  #### Columns ----------------
  ### Region and state level columns to use
  # stateCols0    <- c("state", "postal")
  
  
  ### Modify Tables and Update in List ----------------
  #### 1. Module Model Types ----------------
  ### Gather columns by model type
  co_moduleModTypes <- co_moduleModTypes |> (function(
    df0,
    idCols0  = "module",
    nameCol0 = "model_type",
    valCol0  = "value"
  ){
    df0 |> 
      pivot_longer(-c(idCols0), names_to=nameCol0, values_to=valCol0) |>
      filter_at(c(valCol0), function(x){!(x |> is.na())}) |>
      select(-any_of(valCol0))
  })()
  ### Update values in list, drop intermediate variables
  dataList[["co_moduleModTypes"]] <- co_moduleModTypes
  
  
  #### 2. Default Scenarios ----------------
  co_moduleScenarios <- co_moduleScenarios |> (function(
    df0,
    idCols0  = "inputType",
    nameCol0 = "module",
    valCol0  = "scenarioName"
  ){
    df0 |> 
      pivot_longer(-c(idCols0), names_to=nameCol0, values_to=valCol0) |>
      filter_at(c(valCol0), function(x){!(x |> is.na())})
  })()
  ### Update values in list, drop intermediate variables
  dataList[["co_moduleScenarios"]] <- co_moduleScenarios
  
  #### 3. Module Areas ----------------
  ### Drop damage adjustment names (present in variants)
  co_moduleAreas <- co_moduleAreas |> (function(
    df0,
    idCols0  = "module",
    nameCol0 = "area",
    valCol0  = "value"
  ){
    df0 |> 
      pivot_longer(-c(idCols0), names_to=nameCol0, values_to=valCol0) |>
      filter_at(c(valCol0), function(x){!(x |> is.na())}) |>
      select(-any_of(valCol0))
  })() # ; co_moduleAreas |> glimpse
  dataList[["co_moduleAreas"]] <- co_moduleAreas
  
  
  #### 4. Input Scenarios ----------------
  ### Join info on scenarios with input info
  co_scenarios <- co_scenarios |> (function(
    df0,
    df1   = co_inputInfo,
    join0 = "inputName"
  ){
    df0 |> 
      left_join(df1, by=join0)
  })() # ; co_moduleAreas |> glimpse
  dataList[["co_scenarios"]] <- co_scenarios
  
  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeControlTables().", msgN) |> message()
  return(dataList)
}
