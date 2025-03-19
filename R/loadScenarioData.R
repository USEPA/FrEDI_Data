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
    # dfScenarios, ### Data frame with info on scenarios
    dataDir     = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    dataExt     = "csv",
    controlData, ### Output of configureControlTables()
    minYr0      = 2010,
    maxYr0      = 2300,
    # dfScenarios, ### Data frame with info on scenarios
    popRatios   = "state_population_ratios",
    ratiosName  = "popRatios",
    # outDir      = "." |> file.path("data", "scenarios"),
    silent      = FALSE, ### Level of messaging
    msg0        = 0   ### Messaging prefix
) {
  ### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  # msg1          <- msg0 |> paste("\t")  
  # msg2          <- msg1 |> paste("\t")  
  msg1 <- msg0 + 1
  msg2 <- msg0 + 2
  if(msgUser) msg0 |> get_msgPrefix(newline=T) |> paste0("In loadScenarioData():") |> message()
  
  ### Columns and values ----------------
  ### Columns
  stateCol0     <- "state"
  regCols0      <- c("area", "region", "postal")
  # regCols0      <- c("region", "state", "postal")
  yrCol0        <- "year"
  typeCol0      <- "inputName"
  idCol0        <- "scenarioName"
  tempIdCol0    <- "scenario"
  
  ### Values
  dfScenario    <- controlData[["co_scenarios"]]
  types0        <- dfScenario  |> pull(inputName) |> unique()
  # # projectDir |> devtools::load_all()
  # configVals0   <- frediConfig()
  # minYr0        <- configVals0[["minYear0"]]
  # maxYr0        <- configVals0[["maxYear0"]]
  
  ### Tables
  co_states     <- controlData[["co_states"]]
  
  
  ### Population Ratios ----------------
  ### Population Ratio Data
  sort0        <- c("fips") |> c(yrCol0)
  join0        <- c("state")
  drop0        <- c("area", "region", "postal")
  popRatioFile <- "state_population_ratios" |> paste0(".", dataExt)
  popRatioPath <- dataDir |> file.path(popRatioFile)
  popRatioData <- popRatioPath |> read.csv() |> 
    select(-(any_of(drop0))) |> 
    left_join(co_states, by=join0) |>
    extend_data(to0=maxYr0) |>
    arrange_at(c(sort0))
  ### Add to list
  # dataList[["testScenarios"]] <- inputsList
  # dataList[["popRatios"]] <- popRatioData
  
  ### Load Data ----------------
  ### Scenarios list
  # scenarioDir |> list.files() |> print()
  # scenarioNames <- c("gcam", "gdp", "pop", "popRatios")
  dataList  <- types0 |> map(
    loadScenarioData_byType,
    info0    = dfScenario,
    dir0     = dataDir, 
    ext0     = dataExt,
    typeCol0 = typeCol0,
    idCol0   = idCol0,
    silent   = silent,
    msg0     = msg1
  ) |> set_names(types0)
  # dataList |> glimpse()
  
  ### Interpolate Data ----------------
  ### Create list of file names
  # listData      <- list()
  # listFiles     <- list()
  # listFiles[["gcam"     ]] <- gcamFile
  # listFiles[["gdp"      ]] <- gdpFile
  # listFiles[["pop"      ]] <- popFile
  # listFiles[["popRatios"]] <- ratiosFile
  if(msgUser) msg0 |> get_msgPrefix(newline=T) |> paste0("Reshaping scenarios...") |> message()
  dataList  <- types0 |> map(
    reshapeScenarioData_byType,
    list0    = dataList,
    typeCol0 = typeCol0,
    idCol0   = idCol0,
    argCol0  = c("inputArgVal"),
    valCol0  = c("valueCol"),
    yrCol0   = yrCol0,
    method0  = c("linear"),
    rule0    = 1,
    silent   = silent,
    msg0     = msg1
  ) |> set_names(types0)
  # dataList |> glimpse()
  
  ### Add additional values
  # dataList[["gcam"     ]] <- dataList[["temp"]] |> filter_at(c(idCol0), function(x, y="Hector_v5.3_GCAM"){x %in% y})
  # dataList[["gdp"      ]] <- dataList[["gdp" ]] |> filter_at(c(idCol0), function(x, y="EPPA_v6_GDP"){x %in% y})
  # dataList[["pop"      ]] <- dataList[["pop" ]] |> filter_at(c(idCol0), function(x, y="ICLUS_State_Population"){x %in% y})
  dataList[["popRatios"]] <- popRatioData
  
  ###### Return ----------------
  if (msgUser) msg1 |> get_msgPrefix(newline=T) |> paste0("...Finished running loadScenarioData().") |> message()
  return(dataList)
}