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
    module      = "fredi",
    dataList    = NULL, ### List of data (e.g., as returned from FrEDI_Data::loadData())
    controlData = NULL, ### List with configured control tables (outputs of reshapeControlTables)
    minYr0      = 2010,
    maxYr0      = 2300,
    silent      = TRUE, ### Level of messaging
    msg0        = "\t"  ### Prefix for messaging
) {
  ### Set up Environment ----------------
  #### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "Running reshapeConfigData...") |> message()
  if (!silent) paste0(msg1, "Reshaping data from FrEDI config file...") |> message()
  
  ### Import Functions from FrEDI ----------------
  # get_matches        <- "get_matches"        |> utils::getFromNamespace("FrEDI")
  # get_co_sectorsInfo <- "get_co_sectorsInfo" |> utils::getFromNamespace("FrEDI")
  
  ### 0. Load FrEDI Data Code ----------------
  # ### Load FrEDI Data Code
  # # projectDir |> devtools::load_all()
  # configVals0 <- frediConfig()
  # minYr0      <- configVals0[["minYear0"]]
  # maxYr0      <- configVals0[["maxYear0"]]
  
  #### Assign Objects ----------------
  # dataList0 <- dataList
  ### Assign tables in dataList to object in local environment
  # listNames     <- dataList |> names()
  # listNames |> print()
  for(name_i in dataList    |> names()) {name_i |> assign(dataList  [[name_i]]); rm(name_i)}
  for(name_i in controlData |> names()) {name_i |> assign(controlData[[name_i]]); rm(name_i)}
  # dataList |> list2env(envir = environment())
  
  #### Columns * Value ----------------
  ### Region and state level columns to use
  # stateCols0    <- c("state", "postal")
  areas0  <- controlTables$co_moduleAreas |> 
    filter_at(c("module"), function(x, y=module){x %in% y}) |>
    pull(area)
  mTypes0 <- co_moduleModTypes |> 
    filter_at(c("module"), function(x, y=module){x %in% y}) |>
    pull(model_type)
  
  ### Modify Tables and Update in List ----------------
  #### 1. Sectors ----------------
  ### Make a copy of the sectors list to include variants
  dataList[["co_sectorsRef"]] <- co_sectors
  ### Drop other columns
  co_sectors    <- co_sectors |> (function(
    df0,
    drop0 = c("include", "variants", "impactYears", "impactTypes")
  ){
    df0 |> select(-any_of(drop0))
  })()
  ### Update values in list
  dataList[["co_sectors"]] <- co_sectors
  
  #### 2. Misc ----------------
  ### No changes to variants
  ### No changes to input scenario info
  
  #### 3. Impact Types Info ----------------
  ### Drop damage adjustment names (present in variants)
  co_impactTypes <- co_impactTypes |> (function(
    df, 
    drop0=c("damageAdjName")
  ){
    df0 |> select(-any_of(drop0))
  })() # ; co_impactTypes |> glimpse
  dataList[["co_impactTypes"]] <- co_impactTypes
  
  
  #### 4. Impact Years ----------------
  ### Load data and gather the data by impact year levels
  # # lblCol0   = c("impactYear_label"),
  # impYrs0   = c(2010, 2090),
  # naStr0    = c("NA"),
  # naLbl0    = c("N/A")
  co_impactYears <- co_impactYears |> (function(
    df0, 
    df1       = co_impYrLvls,
    col0      = c("impactYear"), 
    idCols0   = c("sector")
  ){
    ### Select Columns
    cols1     <- col0 |> paste0(c("", "_label"))
    df1       <- df1  |> select(all_of(cols1))
    ### Convert to character and pivot longer by impact year levels
    valCol0   <- "value"
    df0       <- df0 |> pivot_longer(
        cols      = -all_of(idCols0),
        names_to  = col0, 
        values_to = valCol0
      ) |> ### End pivot_longer
      filter_all(all_vars(!is.na(.))) |>
      select(-all_of(valCol0)) |>
      left_join(df1, by=col0)
    ### Return
    return(df0)
  })() 
  ### Update/add tables in/to list, drop intermediate values
  dataList[["co_impactYears"]] <- co_impactYears
  

  #### 5. Models & Model Types ----------------
  # ### Update/add tables in/to list, drop intermediate values
  # co_slrCm <- co_slrCm |> (function(
  #   df0, 
  #   modCol0 = "model"
  # ){
  #   df0 |> mutate_at(c(model), factor, df0 |> pull(all_of(modCol0)))
  # })()
  # ### Update in list
  # dataList[["co_slrCm"]] <- co_slrCm
  
  
  #### 6. Regions & States ----------------
  ### Combine and add to data list...also a copy with info on national data
  ### Update in list
  # co_regions <- co_regions |> mutate(region = region_id)
  # dataList[["co_regions"]] <- co_regions
  # co_states  <- co_states |> 
  #   mutate(region = region |> str_replace_all(" ", "")) |> 
  #   mutate(region = region |> str_replace_all("\\.", ""))
  # dataList[["co_states"]] <- co_states
  
  
  #### 7. Scenario Info ----------------
  ### Combine sectors with co_variants, co_impactTypes, co_impactYears to get group options
  # get_co_sectorsInfo <- utils::getFromNamespace("get_co_sectorsInfo", "FrEDI")
  co_sectorsInfo <- co_sectors |> pull(sector) |> get_co_sectorsInfo(
    # addRegions = TRUE, ### Whether to include regions & states
    # addModels  = TRUE, ### Whether to include models
    # addIds     = TRUE, ### Add scenario Ids
    # include    = c("region", "state", "postal", "model"), ### Other columns to include
    # colTypes   = c("ids", "labels", "extra"), 
    # slrStr     = "Interpolation",
    dfSects    = co_sectors,
    dfVars     = co_variants,
    dfITypes   = co_impactTypes,
    dfIYears   = co_impactYears,
    dfMTypes   = co_modelTypes |> filter(model_type %in% mTypes0),
    # dfReg      = co_regions,
    dfStates   = co_states |> filter(area %in% areas0),
    dfModels   = co_models
  ) ### End get_co_sectorsInfo()
  ### Update in list, drop intermediate values
  # co_sectorsInfo |> glimpse()
  dataList[["co_sectorsInfo"]] <- co_sectorsInfo

  
  #### 8. Sector Scalar Info ----------------
  co_sectorScalars <- co_sectorsInfo |> (function(
    df0,
    # df1      = co_scalarTypes,
    idCols0  = c("sector"),
    typeCol0 = c("scalarType"),
    nameCol0 = c("scalarName"),
    nameStr0 = c("Name"),
    types0   = co_scalarTypes |> pull(all_of(typeCol0))
  ){
    ### Select columns
    ### Pivot longer
    nameCols0  <- types0  |> paste0(nameStr0)
    select0    <- idCols0 |> c(nameCols0)
    df0        <- df0 |> 
      select(all_of(select0)) |> distinct() |> 
      pivot_longer(
        -any_of(idCols0), 
        names_to = typeCol0, 
        values_to = nameCol0
      ) |> 
      mutate_at(c(mutate0), str_replace, "Name", "")
    ### Return
    return(df0)
  })()
  ### Update() in list, drop intermediate values
  # co_sectorsInfo |> glimpse()
  dataList[["co_sectorScalars"]] <- co_sectorScalars

  
  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeConfigData().", msgN) |> message()
  return(dataList)
}
