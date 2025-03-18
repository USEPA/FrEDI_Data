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
    dfScenarios, ### Data frame with info on scenarios
    scenarioDir = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    scenarioExt = "csv",
    # dfScenarios, ### Data frame with info on scenarios
    popRatios   = "state_population_ratios",
    ratiosName  = "popRatios",
    silent      = FALSE, ### Level of messaging
    msg0        = "\t"   ### Messaging prefix
) {
  ### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  msg2          <- msg1 |> paste("\t")  
  if (!silent) paste0(msg0, "In loadScenarioData:") |> message()
  
  ### Columns and values ----------------
  ### Columns
  regCols0      <- c("region", "state", "postal")
  yrCol0        <- "year"
  typeCol0      <- "inputName"
  idCol0        <- "scenarioName"
  tempIdCol0    <- "scenario"
  
  ### Values
  types0        <- dfScenario  |> pull(inputName) |> unique()
  
  ### Load Data ----------------
  ### Scenarios list
  # scenarioDir |> list.files() |> print()
  # scenarioNames <- c("gcam", "gdp", "pop", "popRatios")
  scenarioData  <- types0 |> map(
    loadScenarioData_byType,
    info0    = dfScenario,
    dir0     = scenarioDir, 
    ext0     = scenarioExt,
    typeCol0 = typeCol0,
    idCol0   = idCol0,
    msg0     = 0
  ) |> set_names(types0)
  
  
  ### Interpolate Data ----------------
  ### Create list of file names
  # listData      <- list()
  # listFiles     <- list()
  # listFiles[["gcam"     ]] <- gcamFile
  # listFiles[["gdp"      ]] <- gdpFile
  # listFiles[["pop"      ]] <- popFile
  # listFiles[["popRatios"]] <- ratiosFile
  scenarioData  <- types0 |> map(
    reshapeScenarioData_byType,
    list0    = scenarioData,
    typeCol0 = typeCol0,
    idCol0   = idCol0,
    argCol0  = c("inputArgVal"),
    valCol0  = c("valueCol"),
    yrCol0   = yrCol0,
    method0  = c("linear")
    rule0    = 1,
    msg0     = 0
  ) |> set_names(types0)
  
  ### Add additional values
  # scenarioData[["gcam"     ]] <- scenarioData[["temp"]] |> filter_at(c(idCol0), function(x, y="Hector_v5.3_GCAM"){x %in% y})
  # scenarioData[["gdp"      ]] <- scenarioData[["gdp" ]] |> filter_at(c(idCol0), function(x, y="EPPA_v6_GDP"){x %in% y})
  # scenarioData[["pop"      ]] <- scenarioData[["pop" ]] |> filter_at(c(idCol0), function(x, y="ICLUS_State_Population"){x %in% y})
  
  ### Interpolate Data ----------------
  ### Population Ratio Data
  sort0        <- "area" |> c(regCols0, yrCol0)
  popRatioFile <- "state_population_ratios" |> paste0(".", fExt0)
  popRatioPath <- scenarioDir |> file.path(popRatioFile)
  popRatioData <- popRatioPath |> read.csv() |> 
    extend_data(to0=2300) |> 
    arrange_at(c(sort0))
  ### Add to list
  # dataList[["testScenarios"]] <- inputsList
  listData[["popRatios"]] <- popRatioData
  
  ###### Return ----------------
  if (!silent) paste0("...Finished running loadScenarioData().") |> message()
  return(dataList)
}