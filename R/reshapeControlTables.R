#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeControlTables <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    minYr0   = 2010,
    maxYr0   = 2300,
    silent   = TRUE,   ### Level of messaging
    msg0     = 0
) {
  ### Set up Environment ----------------
  #### Messaging ----------------
  msgUser     <- !silent
  msgN        <- "\n"
  msg1        <- msg0 + 1
  msg2        <- msg0 + 2
  msg0 |> get_msgPrefix(newline=F) |> paste0("Running reshapeControlTables()...") |> message()
  
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
  ##### Module Scenarios ----------------
  co_moduleScenarios <- co_moduleScenarios |> (function(
    df0,
    df1      = co_inputInfo,
    idCols0  = c("inputName", "idCol0"),
    nameCol0 = "module",
    valCol0  = "scenarioName",
    join0    = "inputName"
  ){
    ### Pivot longer
    df0 <- df0 |> pivot_longer(
        -any_of(idCols0), 
        names_to  = nameCol0, 
        values_to = valCol0
      ) |>
      filter_at(c(valCol0), function(x){!(x |> is.na())}) |>
      arrange_at(c(nameCol0, idCols0))
    # df0 |> glimpse(); df1 |> glimpse()
    
    ### Join with scenario info and input info
    df0 <- df0 |> left_join(df1, by=join0)
    
    ### Return
    return(df0)
  })() |> (function(
    df0,
    df1      = co_scenarios,
    drop1    = c("inputName"),
    join0    = c("scenarioName"),
    sort0    = c("module", "inputName", "scenarioName")
    # join0    = "inputName"
  ){
    ### Drop input name
    df1 <- df1 |> select(-any_of(drop1))
    
    ### Join with scenario info and input info
    df0 <- df0 |> 
      # left_join(df1, by=c(join0, valCol0)) |>
      left_join(df1, by=join0) |>
      arrange_at(c(sort0))
    # df0 |> glimpse()
    
    ### Return
    return(df0)
  })()
  ### Update values in list, drop intermediate variables
  dataList[["co_moduleScenarios"]] <- co_moduleScenarios
  
  ##### Input Scenarios ----------------
  ### Join info on scenarios with input info
  co_scenarios <- co_scenarios |> (function(
    df0,
    df1     = co_inputInfo,
    # select0 = c("scenarioName", "inputName", "inputArgType", "inputArgVal"),
    idCol0  = "inputName",
    join0   = "inputName",
    sort0   = c("inputName", "scenarioName")
  ){
    # df0 |> glimpse(); df1 |> glimpse()
    df0 |> 
      left_join(df1, by=join0) |>
      arrange_at(c(sort0))
    # df0 |> glimpse()
  })() 
  # co_scenarios |> glimpse()
  dataList[["co_scenarios"]] <- co_scenarios
  
  #### 3. Module Areas, States, & Regions ----------------
  ##### Module Areas ----------------
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

  ##### States ----------------
  ### Join info on scenarios with input info
  co_states <- co_states |> (function(
    df0,
    df1   = co_regions,
    df2   = co_areas,
    sort0 = "state_order"
  ){
    ### Join states with regions to get region labels
    join0 <- df0 |> names() |> get_matches(y=df1 |> names())
    df0   <- df0 |> left_join(df1, by=join0)
    ### Join states with areas to get area labels
    join0 <- df0 |> names() |> get_matches(y=df2 |> names())
    df0   <- df0 |> left_join(df2, by=join0)
    ### Sort
    df0   <- df0 |> arrange_at(c(sort0))
    ### Return
    return(df0)
  })() 
  # co_scenarios |> glimpse()
  dataList[["co_states"]] <- co_states
  
  
  #### 4. SLR Scenario Info ----------------
  ### Gather slr_cm columns
  # slr_cm |> glimpse()
  # co_slrCm |> pull(model) |> print()
  slr_cm  <- slr_cm |> reshape_slrCm(
    modLvls0 = co_slrCm |> pull(model),
    xCol0    = "xRef",
    yrCol0   = "year", 
    modCol0  = "model"
  ) |> extend_data(to0 = maxYr0) ### End reshape_slrCm
  # slr_cm |> glimpse()
  dataList[["slr_cm"]] <- slr_cm
  
  ### Reshape/format slr_cm for use with SLR extremes
  slrCmExtremes <- slr_cm |> get_slrCmExtremes(
    xCol0    = "xRef",
    yrCol0   = "year", 
    modCol0  = "model"
  ) ### End reshape_slrCm
  # slr_cm |> glimpse()
  dataList[["slrCmExtremes"]] <- slrCmExtremes
  
  ### Reshape/format slr_cm for use with main SLR interpolation
  slrCmMain <- slr_cm |> get_slrCmMain(
    xCol0    = "xRef",
    yrCol0   = "year",
    modCol0  = "model"
  ) ### End get_slrCmMain
  # slr_cm |> glimpse()
  dataList[["slrCmMain"]] <- slrCmMain
  
  ### Return ----------------
  ### Return the list of dataframes
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running reshapeControlTables().", msgN) |> message()
  msg0 |> get_msgPrefix(newline=T) |> message()
  return(dataList)
}
