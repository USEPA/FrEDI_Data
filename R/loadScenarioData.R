### Load and format FrEDI default scenarios
loadScenarioData <- function(
    scenarioDir = "." |> file.path("inst", "extdata", "scenarios") |> file.path(), ### Path to scenarios
    dfScenarios, ### Data frame with info on scenarios
    popRatios   = "state_population_ratios",
    ratiosName  = "popRatios",
    # gcamFile    = "Hector_v5.3_GCAM" |> paste0(".csv"), ### File in scenarioDir containing temperature scenarios
    # gdpFile     = "EPPA_v6_GDP"      |> paste0(".csv"), ### File in scenarioDir containing GDP scenarios
    # popFile     = "ICLUS_State_Population"  |> paste0(".csv"), ### File in scenarioDir containing population scenarios
    # ratiosFile  = "state_population_ratios" |> paste0(".csv"), ### File in scenarioDir containing population ratios
    # testFiles   = list(
    #   temp = "temp_0to6_to2300"   |> paste0(".csv"),
    #   gdp  = "rff_gdp_mean"       |> paste0(".csv"),
    #   pop  = "rff_state_pop_mean" |> paste0(".csv")
    # ), ### End list
    silent      = FALSE, ### Level of messaging
    msg0        = "\t"   ### Messaging prefix
) {
  ### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  msg2          <- msg1 |> paste("\t")  
  if (!silent) paste0(msg0, "In loadScenarioData:") |> message()
  
  ### Columns and values ----------------
  yrCol0        <- "year"
  regCols0      <- c("region", "state", "postal")
  fExt0         <- "csv"
  fType0        <- "." |> paste0(fExt0)
  
  ### Load Data ----------------
  ### Scenarios list
  # scenarioDir |> list.files() |> print()
  # scenarioNames <- c("gcam", "gdp", "pop", "popRatios")
  types0        <- dfScenario  |> pull(inputName) |> unique()
  listData      <- types0 |> map(function(type0){
    if (!silent) paste0(msg1, "Loading ", type0, " data...") |> message()
    ### Filter data and group by remaining columns
    group0 <- dfScenarios |> names()
    info0  <- dfScenarios |> 
      filter(inputName %in% type0) |> 
      group_by_at(c(dfScenarios |> names()))
    scen0  <- info0 |> pull(scenarioName)
    # keys0  <- info0 |> group_keys()
    data0  <- info0 |> 
      group_by_at(c("scenarioName")) |>
      group_map(function(.x, .y){
        ### File name
        nameX <- .x |> pull(scenarioName)
        fileX <- scenarioDir |> file.path(type0, nameX) |> paste0(".", fExt0)
        ### Read in file
        if (!silent) paste0(msg2, "Loading ", nameX, " data...") |> message()
        dataX <- fileX |> read.csv()
        ### Return data
        return(dataX)
    }) |> set_names(scen0) |> 
      bind_rows(.id="scenarioName") 
  }) |> set_names(types0)
  
  
  ### Interpolate Data ----------------
  ### Create list of file names
  # listData      <- list()
  # listFiles     <- list()
  # listFiles[["gcam"     ]] <- gcamFile
  # listFiles[["gdp"      ]] <- gdpFile
  # listFiles[["pop"      ]] <- popFile
  # listFiles[["popRatios"]] <- ratiosFile
  #### Temperature & SLR Data ----------------
  listData      <- types0 |> map(function(type0){
    if (!silent) paste0(msg1, "Reshaping ", type0, " data...") |> message()
    ### Filter data and group by remaining columns
    data0   <- listData[[type0]]
    info0   <- dfScenarios |> filter(inputName %in% type0)
    ### Unique info
    yCol0   <- info0 |> pull(valueCol) |> unique()
    ### Conditionals
    doTemp0 <- type0 |> str_detect("temp")
    doPop0  <- type0 |> str_detect("pop")
    scen0   <- info0 |> pull(scenarioName)
    ### Data
    # keys0  <- info0 |> group_keys()
    data0   <- info0 |> group_map(function(.x){
      ### Name
      nameX   <- .x |> pull(scenarioName)
      if (!silent) paste0(msg2, "Loading ", nameX, " data...") |> message()
      ### Group values
      yrsX    <- .x |> pull(all_of(yrCol0))
      inArgX  <- .x |> pull(inputInfoType) |> unique()
      regX    <- .x |> pull(regional) |> unique()
      stateX  <- inArgX |> str_detect("state")
      doRegX  <- (1 %in% regX) | (doPop0 & stateX)
      groupX  <- c()
      if(doRegX ) groupX <- groupX |> c(groupX)
      if(doTemp0) groupX <- "scenario" |> c(groupX)
      ### Data
      dataX   <- data0 |> 
        filter(scenarioName %in% nameX) |>
        group_by_at(c(groupX), .add=TRUE)
      ### Iterate over groups in X
      if(doTemp0) {
        dataX <- dataX |> group_map(function(.x1, .y1){
          .x1 <- .x1 |> format_tempData_byScenario( 
            tempType0 = inArgX,
            minYr0    = yrsX |> min(na.rm=T),
            maxYr0    = yrsX |> max(na.rm=T),
            tempCol0  = yCol0,
            yrCol0    = yrCol0,
            rule      = 2
          ) ### End format_tempData_byScenario
          .x1 <- .y1 |> cross_join(.x1)
          return(.x1)
        })
      } else{
        dataX <- dataX |> group_map(function(.x1, .y1){
          .x1 <- .x1 |> pull(all_of(yrCol0)) |> approx(
            y = .x1 |> pull(all_of(yrCol0)),
            xout = yrsX
          ) ### End approx
          .x1 <- .y1 |> cross_join(.x1)
          return(.x1)
        })
      } ### End if(doTemp0)  
      ### Ungroup and bind rows
      sortX <- c("scenarioName") |> c(groupX) |> c(yrCol0)
      dataX <- dataX |> 
        bind_rows() |> ungroup() |>
        arrange_at(c(sortX))
      ### Return data
      return(dataX)
    }) |> set_names(scen0) |> bind_rows(.id="scenarioName") 
  }) |> set_names(types0)
  
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