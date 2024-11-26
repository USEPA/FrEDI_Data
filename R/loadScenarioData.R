### Load and format FrEDI default scenarios
loadScenarioData <- function(
    scenarioDir = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
    gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
    popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
    ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
    silent      = FALSE, ### Level of messaging
    msg0        = "\t\t" ### Messaging indentation
) {
  ###### Messaging ######
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In loadScenarioData:") |> message()
  
  ###### File Paths ######
  ### Scenarios list
  # scenarioDir |> list.files() |> print()
  scenarioNames <- c("gcam", "gdp", "pop", "popRatios")
  
  ### Create list of file names
  listFiles     <- list()
  listFiles[["gcam"     ]] <- gcamFile
  listFiles[["gdp"      ]] <- gdpFile
  listFiles[["pop"      ]] <- popFile
  listFiles[["popRatios"]] <- ratiosFile
  
  ### Create list of file paths
  listPaths     <- scenarioNames |> map(function(name_i, file_i=listFiles[[name_i]], dir_i=scenarioDir){
    if (!silent) paste0(msg1, "Loading ", name_i, " data...") |> message()
    dir_i |> file.path(file_i)
  }) |> set_names(scenarioNames)
  # listPaths |> unlist() |> print()
  
  ### Iterate over list to get data
  dataList      <- listPaths |> map(function(file_i){
    file_i |> read.csv() |> as_tibble()
  }) |> set_names(scenarioNames |> paste0("Data"))
  
  
  ###### Return ######
  if (!silent) paste0("\n") |> message()
  return(dataList)
}