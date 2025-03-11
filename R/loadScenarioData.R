### Load and format FrEDI default scenarios
loadScenarioData <- function(
    scenarioDir = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
    gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
    popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
    ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
    testFiles   = list(
      temp = "temp_0to6_to2300"   |> paste0(".csv"),
      gdp  = "rff_gdp_mean"       |> paste0(".csv"),
      pop  = "rff_state_pop_mean" |> paste0(".csv")
    ), ### End list
    silent      = FALSE, ### Level of messaging
    msg0        = "\t"   ### Messaging prefix
) {
  ###### Messaging ######
  msgN          <- "\n"
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
  
  # ### Create list of file paths
  # listPaths     <- scenarioNames |> map(function(name_i, file_i=listFiles[[name_i]], dir_i=scenarioDir){
  #   if (!silent) paste0(msg1, "Loading ", name_i, " data...") |> message()
  #   dir_i |> file.path(file_i)
  # }) |> set_names(scenarioNames)
  # # listPaths |> unlist() |> print()
  # 
  # ### Iterate over list to get data
  # dataList      <- listPaths |> map(function(file_i){
  #   file_i |> read.csv() |> as_tibble()
  # }) |> set_names(scenarioNames |> paste0("Data"))
  
  # ### Create list of file paths
  # listPaths     <- scenarioNames |> map(function(
  #   name_i, 
  #   dir_i  = scenarioDir, 
  #   file_i = listFiles[[name_i]]
  # ){
  #   if (!silent) paste0(msg1, "Loading ", name_i, " data...") |> message()
  #   dir_i |> file.path(file_i)
  # }) |> set_names(scenarioNames)
  # # listPaths |> unlist() |> print()
  
  ### Iterate over list to get data
  dataList      <- scenarioNames |> map(function(
    name_i, 
    dir_i   = scenarioDir, 
    files_i = listFiles
  ){
    if (!silent) paste0(msg1, "Loading ", name_i, " data...") |> message()
    file_i <- files_i[[name_i]]
    path_i <- dir_i  |> file.path(file_i)
    df_i   <- path_i |> read.csv() |> as_tibble()
    return(df_i)
  }) |> set_names(scenarioNames |> paste0("Data"))
  
  ### List of inputs to test
  ### Iterate over list to get data
  inputNames <- testFiles |> names()
  inputsList <- inputNames |> map(function(
    name_i, 
    dir_i   = scenarioDir, 
    files_i = testFiles
  ){
    if (!silent) paste0(msg1, "Loading ", name_i, "scenario data...") |> message()
    file_i <- files_i[[name_i]]
    path_i <- dir_i  |> file.path(file_i)
    df_i   <- path_i |> read.csv() |> as_tibble()
    return(df_i)
  }) |> set_names(inputNames)
  
  ### Add to list
  dataList[["testScenarios"]] <- inputsList
  
  
  ###### Return ######
  if (!silent) paste0("...Finished running loadScenarioData().") |> message()
  return(dataList)
}