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
reshapeScenarioData <- function(
    dfScenarios,
    scenarioData,
    controlData, ### Output of configureControlTables()
    minYr0      = 2010,
    maxYr0      = 2300,
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
  msg0 |> get_msgPrefix(newline=T) |> paste0("Reshaping scenario data...") |> message()
  
  #### Columns and values ----------------
  ### Columns
  areaCol0      <- "area"
  regCol0       <- "region"
  postCol0      <- "postal"
  stateCol0     <- "state"
  fipsCol0      <- "fips"
  orderCol0     <- "state_order"
  stateCols0    <- c(areaCol0, regCol0, stateCol0, postCol0, fipsCol0, orderCol0)
  yrCol0        <- "year"
  typeCol0      <- "inputName"
  idCol0        <- "scenarioName"
  tempIdCol0    <- "scenario"
  
  #### Data ----------------
  ### States
  co_states     <- controlData[["co_states"]] |> select(any_of(stateCols0))
  
  
  ### Reshape Population Ratios ----------------
  ### Format Population Ratio Data
  popRData     <- scenarioData[[ratiosName]] |> format_popRatioData(
    df1     = co_states,    ### States data
    group0  = c(areaCol0, regCol0, stateCol0, postCol0, fipsCol0, orderCol0), ### Grouping columns
    xCol0   = c(yrCol0), ### X columns
    yCols0  = c("area2nat", "reg2area", "state2reg"), ### Columns to interpolate
    to0     = maxYr0, ### Interpolate values to
    method0 = "linear",
    rule0   = 2, 
    silent  = silent,
    msg0    = msg1
  ) ### End format_popRatioData
  ### Update in list
  scenarioData[["popRatios"]] <- popRData
  
  ### Reshape Input Data ----------------
  ### Scenarios data
  ### Scenarios
  dfScenarios   <- dfScenarios |> 
    # select(all_of(select0)) |> distinct() |> 
    arrange_at(c(typeCol0, idCol0)) |> 
    group_by_at(c(typeCol0))
  types0        <- dfScenarios  |> pull(any_of(typeCol0)) |> unique()
  
  ### Iterate over list
  dataList     <- dfScenarios |> group_map(function(.x, .y){
    .x |> formatScenarioData_byType(
      .y       = .y,
      df1      = co_states,
      typeCol0 = typeCol0,
      idCol0   = idCol0,
      xCol0    = yrCol0,
      valCol0  = "valueCol",
      grpCol0  = "groupCols",
      regCols0 = stateCols0,
      list0    = scenarioData,
      method0  = "linear",
      rule0    = 1,
      silent   = silent,
      msg0     = msg1
    ) ### End reshapeScenarioData_byType
  }) |> set_names(types0)
  ### Update in list
  # scenarioData <- scenarioData[scenarioData |> names() |> get_matches(y=types0, matches=F, type="matches")]
  scenarioData <- dataList |> c(scenarioData["popRatios"])
  rm(dataList)
  # dataList |> glimpse()
  
  ### Return ----------------
  # if (msgUser) 
  msg1 |> get_msgPrefix(newline=T) |> paste0("...Finished reshaping scenario data.") |> message()
  return(scenarioData)
}