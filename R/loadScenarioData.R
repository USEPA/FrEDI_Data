### Load and format FrEDI default scenarios
# gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
# gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
# popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
# ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
# testFiles   = list(
#   temp = "temp_0to6_to2300"   |> paste0(".csv"),
#   gdp  = "rff_gdp_mean"       |> paste0(".csv"),
#   pop  = "rff_state_pop_mean" |> paste0(".csv")
# ), ### End list
loadScenarioData <- function(
    dfScenarios, 
    dataDir     = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    dataExt     = "csv",
    popRatios   = "state_population_ratios",
    ratiosName  = "popRatios",
    silent      = FALSE, ### Level of messaging
    msg0        = 0   ### Messaging prefix
) {
  ### Set up Environment ----------------
  #### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  # if(msgUser) 
  msg0 |> get_msgPrefix(newline=T) |> paste0("Loading scenario data...") |> message()
  
  #### Columns and values ----------------
  ### Columns
  areaCol0      <- "area"
  regCol0       <- "region"
  postCol0      <- "postal"
  stateCol0     <- "state"
  fipsCol0      <- "fips"
  orderCol0     <- "state_order"
  regCols0      <- c(areaCol0, regCol0, fipsCol0)
  yrCol0        <- "year"
  typeCol0      <- "inputName"
  idCol0        <- "scenarioName"
  tempIdCol0    <- "scenario"
  
  ### Input Scenarios ----------------
  ### Group main input scenarios
  ### - Get distinct values
  select0       <- c(typeCol0, idCol0)
  # dfScenarios |> glimpse()
  dfScenarios   <- dfScenarios |> 
    select(all_of(select0)) |> 
    distinct()
  
  ### Add information for files
  dfScenarios <- dfScenarios |> 
    arrange_at(c(typeCol0, idCol0)) |>
    group_by_at(c(typeCol0)) |>
    rename_at(c(typeCol0, idCol0), ~c("type0", "name0")) |>
    mutate(file0 = name0   |> paste0(".", dataExt)) |>
    mutate(dir0  = dataDir |> file.path(type0)) |>
    mutate(path0 = dir0    |> file.path(file0))
  rm(select0)

  # "got here" |> print()
  ### Iterate over groups, loading scenarios
  types0        <- dfScenarios |> group_keys() |> pull(type0)
  dataList      <- dfScenarios |> group_map(function(.x, .y){
    .x |> loadScenarioData_byType(
      .y       = .y,
      typeCol0 = "type0",
      nameCol0 = "name0",
      pathCol0 = "path0",
      silent   = TRUE,
      msg0     = msg1
    ) ### End loadScenarioData_byType
  }) |> set_names(types0)
  
  
  ### Population Ratios ----------------
  ### Add additional values
  # dataList[["gcam"     ]] <- dataList[["temp"]] |> filter_at(c(idCol0), function(x, y="Hector_v5.3_GCAM"){x %in% y})
  # dataList[["gdp"      ]] <- dataList[["gdp" ]] |> filter_at(c(idCol0), function(x, y="EPPA_v6_GDP"){x %in% y})
  # dataList[["pop"      ]] <- dataList[["pop" ]] |> filter_at(c(idCol0), function(x, y="ICLUS_State_Population"){x %in% y})
  
  ### Population Ratio Data
  # sort0        <- c("state_order") |> c(yrCol0)
  # join0        <- c("state")
  # drop0        <- c("area", "region", "fips")
  popRFile     <- "state_population_ratios" |> paste0(".", dataExt)
  popRPath     <- dataDir  |> file.path(popRFile)
  popRData     <- popRPath |> read.csv()
  ### Add to list
  dataList[[ratiosName]] <- popRData
  
  
  ###### Return ----------------
  # if (msgUser) 
  msg1 |> get_msgPrefix(newline=T) |> paste0("...Finished loading scenario data.") |> message()
  return(dataList)
}