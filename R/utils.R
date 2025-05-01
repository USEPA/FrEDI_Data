### FrEDI Data Utilities
## Import functions from FrEDI namespace ----------------
# convertTemps       <- "convertTemps"       |> utils::getFromNamespace("FrEDI")
# get_matches        <- "get_matches"        |> utils::getFromNamespace("FrEDI")
# get_msgPrefix      <- "get_msgPrefix"      |> utils::getFromNamespace("FrEDI")
# get_scenario_id    <- "get_scenario_id"    |> utils::getFromNamespace("FrEDI")
# # interpolate_annual <- "interpolate_annual" |> utils::getFromNamespace("FrEDI")
# # match_scalarValues <- "match_scalarValues" |> utils::getFromNamespace("FrEDI")
# temps2slr          <- "temps2slr"          |> utils::getFromNamespace("FrEDI")
# # update_popScalars  <- "update_popScalars"  |> utils::getFromNamespace("FrEDI")

### For SV
calc_countyPop     <- "calc_countyPop" |> utils::getFromNamespace("FrEDI")

## Misc FrEDI Data Utilities  ----------------
### Function to get years from data
### Get a sequence from a set of years
get_years_fromData <- function(years0, by=1){
  min0 <- years0 |> min(na.rm=T)
  max0 <- years0 |> max(na.rm=T)
  yrs0 <- min0 |> seq(max0, by=by)
  return(yrs0)
}

### Function to extend data
extend_data <- function(
    df0,          ### Data frame to extend
    # from0  = 2090, ### Year to extend from
    to0   = 2300, ### Year to extend to
    xCol0 = "year"
){
  ### complete(year = seq(min(year), 2300, 1)) |> fill(-c("year"))
  #### Years
  # df0 |> glimpse()
  from0  <- df0 |> pull(all_of(xCol0)) |> max()
  vals0  <- ((from0 + 1):to0) |> as.numeric()
  dfNew0 <- tibble(xVal = vals0) |> rename_at(c("xVal"), ~xCol0)
  # vals0 |> print(); dfNew0 |> glimpse()
  
  ### Data to extend
  dfNew0 <- df0 |> 
    filter_at(c(xCol0), function(x, y=from0){x %in% y}) |> 
    select(-any_of(xCol0)) |> 
    cross_join(dfNew0)

  ### Bind extended data
  df0    <- df0 |> 
    filter_at(c(xCol0), function(x, y=from0){x <= y}) |> 
    bind_rows(dfNew0)
  
  ### Return
  return(df0)
}

## Scenario Functions ----------------
# ## Function to load data of specific type
loadScenarioData_byType <- function(
    .x, 
    .y,
    typeCol0 = "inputName",
    nameCol0 = "name0",
    pathCol0 = "path0",
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  
  ### Type
  # .y |> glimpse(); .x |> glimpse()
  type0      <- .y |> pull(all_of(typeCol0)) |> unique()
  names0     <- .x |> pull(all_of(nameCol0))
  
  ### Read in files and bind data
  if (msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Loading ", type0, " scenario data...") |> message()
  cols0      <- c(nameCol0, pathCol0)
  list0      <- .x |> select(all_of(cols0)) |> 
    pmap(function(name0, path0){
      # path0 |> print()
      msg1  |> get_msgPrefix(newline=F) |> paste0("Loading ", name0, " scenario data...") |> message()
      path0 |> read.csv()
    }) |> set_names(names0)
  
  ### Return data
  return(list0)
}


### Format population ratio data
format_popRatioData <- function(
    df0,
    df1,    ### States data
    group0  = c("area", "region", "state", "postal", "fips", "state_order"), ### Grouping columns
    xCol0   = c("year"), ### X columns
    yCols0  = c("area2nat", "reg2area", "state2reg"), ### Columns to interpolate
    from0   = NULL,
    to0     = 2300, ### Interpolate values to
    method0 = "linear",
    rule0   = 2, 
    silent  = TRUE,
    msg0    = 0
){
  
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  if (msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Formatting population ratio data...") |> message()
  
  ### Columns
  areaCol0      <- "area"
  regCol0       <- "region"
  postCol0      <- "postal"
  stateCol0     <- "state"
  fipsCol0      <- "fips"
  orderCol0     <- "state_order"
  stateCols0    <- c(areaCol0, regCol0, stateCol0, postCol0, fipsCol0, orderCol0)
  yrCol0        <- "year"
  
  ### Population Ratio Data
  drop0      <- c(areaCol0, regCol0)
  join0      <- c(stateCol0)
  sort0      <- c(orderCol0, xCol0)
  df0        <- df0 |> 
    select(-any_of(drop0)) |> 
    left_join(df1, by=join0) |>
    # extend_data(to0=to0) |>
    arrange_at(c(sort0)) |> 
    group_by_at(c(group0))
  
  ### Get group keys
  df0        <- df0 |> group_map(
    interpolate_byGroup,
    xCol0     = xCol0,
    yCols0    = yCols0,
    method0   = method0,
    rule0     = rule0
  ) |> bind_rows()
  
  ### Return
  return(df0)
}


### Function to format groupCols column
format_groupColsColumn <- function(x0){
  # x0 |> print()
  x0 <- x0 |> trimws()
  x0 <- paste0("'", x0, "'")
  x0 <- x0 |> str_replace(",", "','")
  x0 <- x0 |> str_replace(" |NA", "")
  x0 <- x0 |> str_replace("''", "")
  return(x0)
}

parse_groupColsColumn <- function(x0){
  # x0 |> print()
  x0  <- x0 |> trimws()
  x0  <- paste0("c(", x0, ")")
  x0  <- parse(text=x0) |> eval()
  na0 <- (x0 |> length()) == 0
  if(na0) x0 <- c()
  # x0 |> print()
  return(x0)
}

### Reshape specific scenarios
formatScenarioData_byType <- function(
    .x, 
    .y,
    df1,     ### States info
    list0,   ### scenarioData
    typeCol0 = "inputName",
    idCol0   = "scenarioName",
    xCol0    = "year",
    valCol0  = "valueCol",
    grpCol0  = "groupCols",
    argCol0  = "inputArgVal",
    regCols0 = c("area", "region", "state", "postal", "fips", "state_order"),
    method0  = "linear",
    rule0    = 2, 
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  
  ### Columns & Values
  # "got here2" |> print(); .y |> glimpse(); # .x |> glimpse();  
  type0      <- .y |> pull(all_of(typeCol0)) |> unique() |> tolower()
  yCol0      <- .x |> pull(all_of(valCol0)) |> unique()
  refYr0     <- .x |> pull(refYear ) |> unique()
  minVal0    <- .x |> pull(inputMin) |> unique()
  doReg0     <- .x |> pull(doReg0  ) |> unique()
  doTemp0    <- .x |> pull(doTemp0 ) |> unique()
  
  ### Group columns
  groupCols0 <- .x |> pull(all_of(grpCol0)) |> 
    format_groupColsColumn() |>
    parse_groupColsColumn() |> 
    unique()
  # type0 |> print(); groupCols0 |> print()
  
  ### Group x by remaining names
  list0      <- list0[[type0]]
  group0     <- idCol0
  .x         <- .x |> group_by_at(c(group0))
  names0     <- .x |> group_keys() |> pull(all_of(group0)) |> unique()
  # "got here3" |> print(); .x |> glimpse(); names0 |> print()
  # return(list0)
  
  ### Read in files and bind data
  if (msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Formatting ", type0, " scenario data...") |> message()
  list0      <- .x |> group_map(function(.x1, .y1){
    .x1 |> reshapeScenarioData_byGroup(
      .y       = .y1,
      df1      = df1,
      list0    = list0,
      idCol0   = idCol0,
      xCol0    = xCol0,
      yCol0    = yCol0,
      refYr0   = refYr0,
      minVal0  = minVal0,
      group0   = groupCols0,
      regCols0 = regCols0,
      doReg0   = doReg0,
      doTemp0  = doTemp0,
      silent   = silent,
      msg0     = msg1
    ) |> 
      group_by_at(c(groupCols0)) ### End reshapeScenarioData_byGroup
  }) |> set_names(names0)
  # return(list0)
  
  ### Format data
  # "got here4" |> print(); list0 |> glimpse()
  df0        <- .x |> group_map(function(.x1, .y1){
    .x1 |> interpolateScenarioData_byGroup(
      .y       = .y1,
      list0    = list0,
      idCol0   = idCol0,
      xCol0    = xCol0,
      yCol0    = yCol0,
      argCol0  = argCol0,
      doTemp0  = doTemp0,
      method0  = method0,
      rule0    = rule0,
      silent   = silent,
      msg0     = msg1
    ) ### End reshapeScenarioData_byGroup
  }) |> bind_rows()
  
  ### Return data
  if (msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("Finished formatting ", type0, " scenario data...") |> message()
  return(df0)
}

### Reshape population data
### May need to add steps in the future if not a state population scenario
reshape_regScenario <- function(
    df0,
    df1,     ### States info
    xCol0    = c("year"),
    yCol0    = c("pop"),
    group0   = c(),
    regCols0 = c("area", "region", "state", "postal", "fips", "state_order"),
    join0    = c("state", "postal"),
    popType  = "state",
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  ### Join by state or postal
  names0     <- df0      |> names()
  join0      <- join0    |> get_matches(names0)
  group0     <- group0   |> get_matches(names0) |> get_matches(y=join0, matches=F)
  select0    <- join0    |> c(group0) |> unique() |> c(xCol0, yCol0)
  group0     <- regCols0 |> c(group0) |> unique()
  sort0      <- group0   |> c(xCol0 ) |> unique()
  df0        <- df0      |> 
    select(all_of(select0)) |>
    left_join(df1, by=join0) |>
    arrange_at(c(sort0)) |>
    group_by_at(c(group0))
  ### Return
  return(df0)
}



### Reshape specific scenarios
reshapeScenarioData_byGroup <- function(
    .x, 
    .y,
    df1,     ### State info
    list0,   ### List with data
    idCol0   = "inputName",
    xCol0    = "year",
    yCol0    = "temp_C",
    refYr0   = NA,
    minVal0  = 00,
    group0   = c(),
    regCols0 = c("area", "region", "state", "postal", "fips", "state_order"),
    doReg0   = FALSE,
    doTemp0  = FALSE,
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  
  ### Values
  # list0 |> names() |> print(); idCol0 |> print()
  # .y |> glimpse(); .x |> glimpse(); 
  name0      <- .y |> pull(all_of(idCol0)) |> unique()
  
  ### Data
  if (msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Reshaping ", name0, " scenario...") |> message()
  df0        <- list0[[name0]]
  
  ### If doTemp, add scenario if not present
  # "got here1" |> print(); group0 |> print()
  if(doTemp0) {
    hasGrp0 <- group0 |> get_matches(y=df0 |> names()) |> length()
    if(!hasGrp0) df0[[group0]] <- name0
  } ### End if(doTemp0)
  
  ### Group columns
  groupCols0 <- idCol0 |> c(group0) |> unique()
  # groupCols0 |> print()
  
  ### If doReg, standardize region/state info
  if(doReg0) {
    df0 <- df0 |> reshape_regScenario(
      df1      = df1,     ### States info
      xCol0    = xCol0,
      yCol0    = yCol0,
      # group0   = group0,
      group0   = groupCols0,
      regCols0 = regCols0,
      silent   = TRUE,
      msg0     = 0
    ) ### End reshape_regScenario
  } ### End if(doReg0)
  # "got here2" |> print(); xCol0 |> print()
  
  ### Group columns and filter out missing values
  select0    <- group0 |> c(xCol0, yCol0)
  sort0      <- group0 |> c(xCol0)
  # select0 |> print(); df0 |> glimpse()
  df0        <- df0 |>
    filter_at(c(xCol0, yCol0), function(x){!{x |> is.na()}}) |> 
    ungroup() |>
    select(any_of(select0)) |>
    arrange_at(c(sort0)) |>
    group_by_at(c(group0))
  
  ### If ref year is not missing, add a reference value
  doRef0     <- !(refYr0 |> is.na())
  if(doRef0) {
    df0 <- df0 |> filter_at(c(xCol0), function(x, y=refYr0){x > y})
    df0 <- df0 |> 
      summarize_at(c(xCol0), min) |> 
      mutate_at(c(xCol0), function(x, y=refYr0 ){y}) |> 
      mutate(y = minVal0) |>
      rename_at(c("y"), ~yCol0) |>
      bind_rows(df0) |>
      arrange_at(c(sort0))
  } ### End if(doRef0)

  ### Return data
  if (msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("...Finished reshaping ", name0, " scenario.") |> message()
  return(df0)
}


### Reshape specific scenarios
interpolateScenarioData_byGroup <- function(
    .x, 
    .y,
    list0,   ### List with data
    idCol0   = "inputName",
    xCol0    = "year",
    yCol0    = "temp_C",
    group0   = c("scenario"),
    argCol0  = "inputArgVal",
    doTemp0  = TRUE,
    method0  = "linear",
    rule0    = 2,
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1
  
  ### Values
  # .y |> glimpse(); .x |> glimpse();
  name0      <- .y |> pull(all_of(idCol0 )) |> unique()
  argVal0    <- .x |> pull(all_of(argCol0)) |> unique()
  if (msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Interpolating values for ", name0, " scenario...") |> message()
  
  ### Data
  df0        <- list0[[name0]]
  
  ### If doTemp, use one function, otehrwise, use the other
  if(doTemp0){
    df0 <- df0 |> group_map(
      format_tempData_byGroup,
      xCol0     = xCol0,
      yCol0     = yCol0,
      tempType  = argVal0,
      method0   = method0,
      rule0     = rule0
    ) |> bind_rows()
  } else{
    df0 <- df0 |> group_map(
      interpolate_byGroup,
      xCol0     = xCol0,
      yCols0    = yCol0,
      method0   = method0,
      rule0     = rule0
    ) |> bind_rows()
  } ### End if(doTemp0)  
  
  ### Join data with input col
  # df0 |> glimpse(); 
  df0        <- .y |> 
    select(any_of(idCol0)) |> 
    distinct() |>
    cross_join(df0)
  
  ### Return data
  if (msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("...Interpolating values for ", name0, " scenario.") |> message()
  return(df0)
}


### Format grouped temperature scenarios
# formatGroupedTempData <- function(
#     df0,      ### Original GCAM data
#     tempCol0  = "temp_C_global",
#     typeCol0  = "inputArgVal",
#     # tempType0 = "global",
#     yrCol0    = "year",
#     group0    = c("scenario", "model"),
#     method0   = "linear",
#     rule0     = 1
# ){
#   ### Select columns
#   ### Filter out any missing values
#   sort0   <- group0 |> c(yrCol0)
#   df0     <- df0 |>
#     filter_all(any_vars(!(. |> is.na()))) |>
#     arrange_at(c(sort0)) |>
#     group_by_at(c(group0))
#   
#   ### Calculate temp_C_conus
#   # df0 |> pull(scenario) |> unique() |> print()
#   # groups0 <- df0 |> group_keys()
#   df0     <- df0 |> group_map(
#     format_tempData_byGroup,
#     tempCol0  = tempCol0,
#     typeCol0  = typeCol0,
#     # tempType0 = tempType0,
#     yrCol0    = yrCol0,
#     method0   = method0,
#     rule0     = rule0
#   ) |> bind_rows()
#   # df0 |> glimpse()
#   
#   ### Return
#   return(df0)
# }

### Format grouped temperature scenarios by group
### df0       = Data, filtered to a specific scenario
### scen0     = Scenario name
### scenCol0  = Scenario column
### tempCol0  = Temperature column
### tempType0 = Temperature type
# format_tempData_byGroup <- function(
#     .x,       ### Data, filtered to a scenario
#     .y,       ### Group info
#     xCol0     = "year",
#     yCol0     = "temp_C",
#     tempType0 = "global",
#     # argCol0   = "inputArgVal", ### Column to look for tempType
#     method0   = "linear",
#     rule0     = 1,
#     globalStr = "global",
#     conusStr  = "conus"
# ){
#   # .x |> glimpse(); .y |> glimpse()
#   # ### Sort data
#   # .x        <- .x |> 
#   #   filter_at(c(xCol0, yCol0), function(x){!(x |> is.na())}) |>
#   #   arrange_at(c(xCol0))
#   # 
#   ### Group info
#   # tempType0 <- .y   |> pull(all_of(typeCol0)) |> unique() |> tolower()
#   # xVals0 |> range() |> print()
#   
#   
#   ### Values and columns
#   doGlobal  <- tempType0 |> str_detect(globalStr)
#   tempType1 <- tempType0
#   tempType2 <- case_when(doGlobal ~ conusStr, .default = globalStr)
#   
#   ### Interpolate values
#   .x        <- .x |> interpolate_byGroup(
#     .y        = .y,
#     xCol0     = xCol0,
#     yCols0    = yCol0,
#     method0   = method0,
#     rule0     = rule0
#   ) ### End interpolate_byGroup
#   
#   ### Interpolate values, convert temperatures, calculate slr_cm
#   ### Calculate SLR
#   old0      <- yCol0 |> c("y2")
#   new0      <- c("temp_C_" |> paste0(c(tempType1, tempType2)))
#   .x        <- .x |> 
#     mutate(y2 = .x |> pull(all_of(yCol0)) |> convertTemps(from=tempType0)) |>
#     rename_at(c(old0), ~new0)
#   # .x |> glimpse()
#   
#   ### Slr values
#   dfSlr     <- .x    |> 
#     pull(temp_C_global) |> 
#     temps2slr(years=.x |> pull(all_of(xCol0))) |> 
#     rename_at(c("year"), ~xCol0) |>
#     left_join(.x, by=xCol0) |>
#     filter(!(slr_cm |> is.na())) 
#   # df0 |> glimpse()
# 
#   # ### Add model data
#   # .x        <- .y |> cross_join(.x)
#   
#   ### Return
#   return(.x)
# }




## Reshape/Format SLR Height/Scenario Data ----------------
### Function to get SLR lvls from model column
get_slrLevels <- function(
    df0, 
    modCol0 = "model",
    addZero = T,
    addUnit = T
){
  ### Get unique model values
  # str0  <- " |cm"
  # str1  <- ""
  vals0 <- df0   |> pull(all_of(modCol0)) |> as.character() |> unique() 
  nums0 <- vals0 |> parse_number()
  lvls0 <- nums0 |> sort()
  if(addZero) lvls0 <- 0 |> c(lvls0) |> unique()
  if(addUnit) {
    unit0 <- vals0 |> first() |> str_replace(nums0 |> first() |> as.character(), "")
    lvls0 <- lvls0 |> paste0(unit0)
  } ### End if(addUnit)
  ### Return
  return(lvls0)
}

### Zero SLR values
zeroSlrValues <- function(
    df0, 
    yCol0   = "value",
    modCol0 = "model",
    min0    = 30, 
    new0    = 0
){
  ### Determine if model has already been factored
  hasLvls0 <- df0 |> pull(all_of(modCol0)) |> is.factor()
  ### Factor model if !hasLvls
  if(hasLvls0) {
    lvls0  <- df0 |> pull(all_of(modCol0)) |> levels()
  } else{
    lvls0  <- df0 |> get_slrLevels(modCol0=modCol0, addZero=T, addUnit=T)
    df0    <- df0 |> mutate_at(c(modCol0), factor, lvls0)
  } ### End if(hasLvls0)
  
  ### Get Unit
  lvl0     <- lvls0 |> tail(2) |> first()
  unit0    <- lvl0  |> str_replace(lvl0 |> parse_number() |> as.character(), "")
  minStr0  <- min0  |> paste0(unit0) 
  newStr0  <- new0  |> paste0(unit0) 
  # df0 |> pull(all_of(modCol0)) |> unique() |> print(); lvls0 |> print(); unit0 |> print()
  
  ### Convert to character and filter
  # df0 |> nrow() |> print()
  df0      <- df0 |> mutate_at(c(modCol0), as.character)
  dfMin0   <- df0 |> filter_at(c(modCol0), function(x, y=minStr0){x %in% y})
  df0      <- df0 |> filter_at(c(modCol0), function(x, y=newStr0){!(x %in% y)})
  # "gotHere3" |> print(); dfMin0 |> nrow() |> print();  df0 |> nrow() |> print()
  
  ### Zero out column
  dfMin0   <- dfMin0 |> 
    mutate_at(c(modCol0), function(x, y=newStr0){y}) |>
    mutate_at(c(modCol0), factor, lvls0) |>
    mutate_at(c(yCol0  ), function(x, y=new0){y})
  # "gotHere5" |> print(); dfMin0 |> nrow() |> print();  
  
  ### Bind values
  # df0    <- df0    |> filter_at(c(modCol0), function(x, y=newStr0){!(x %in% y)})
  df0      <- dfMin0 |> bind_rows(df0)
  # "gotHere5" |> print(); df0 |> nrow() |> print()
  
  ### Return
  return(df0)
}


### Function to perform initial formatting/reshaping of slr_cm
### Function to get main SLR heights
reshape_slrCm <- function(
    df0,      ### slr_cm
    modLvls0 = c(0, 30,  50, 100, 150, 200, 250) |> paste0("cm"), ### Model levels to use for factoring...from co_slrCm
    modCol0  = "model",
    xCol0    = "xRef",
    yrCol0   = "year"
){
  ### Gather slr_cm columns
  df0    <- df0 |> pivot_longer(
    cols      = -all_of(yrCol0), 
    names_to  = modCol0,
    values_to = xCol0
  ) ### End pivot_longer
  # df0 |> glimpse()
  # df0 |> pull(all_of(modCol0)) |> unique() |> print()
  
  ### Columns
  sort0 <- c(modCol0, yrCol0, xCol0)
  ### Factor models
  ### Rename columns and organize
  df0   <- df0 |>
    mutate_at  (c(modCol0), as.character) |>
    mutate_at  (c(modCol0), factor, modLvls0) |>
    arrange_at (c(sort0))
  
  ### Return
  return(df0)
}


### Function to get main SLR heights
get_slrCmMain <- function(
    df0,     ### Tibble with formatted SLR scenario heights (output of reshape_slrCm)
    xCol0    = "xRef",
    yrCol0   = "year",
    modCol0  = "model"
){
  ### Sort models by year, xRef, model
  ### Group by year, xRef
  ### Arrange values ascending, get lowest model for each year-xRef combination
  grp0  <- c(yrCol0, xCol0)
  sort0 <- grp0 |> c(modCol0)
  df0   <- df0 |>
    arrange_at (c(sort0)) |>
    group_by_at(c(grp0)) |>
    slice(1)
  ### Return
  return(df0)
}

### Function to iterate over in fun_slrConfigExtremes
get_slrCmExtremes <- function(
    df0,      ### Tibble with formatted SLR scenario heights (output of reshape_slrCm)
    xCol0     = "xRef",
    yrCol0    = "year",
    modCol0   = "model",
    boundCol0 = "bound"
){
  # df0 |> glimpse(); 
  ### Other values
  loStr0  <- "Lo"
  hiStr0  <- "Hi"
  bounds0 <- c(loStr0, hiStr0)
  funs0   <- c("nth", "max")
  xStr0   <- "x"
  ### Arrange values ascending, get lowest model for each year-xRef combination
  ### Note: this is also done in get_slrCmMain, but keeping this here in case we want to change the order
  group0 <- c(yrCol0, xCol0)
  sort0  <- group0 |> c(modCol0)
  df0    <- df0 |> 
    arrange_at (c(sort0 )) |>
    group_by_at(c(group0)) |>
    slice_head(n = 1)
  # df0 |> glimpse()
  
  ### Then, get the two highest xRefs:
  ### - Sort descending, group by year, get highest and second highest xRef values
  ### - Then, join with other data
  # df0 |> pull(all_of(yrCol0)) |> unique() |> length() |> print()
  df1  <- df0 |> 
    arrange_at (c(sort0), desc) |>
    group_by_at(c(yrCol0)) |>
    summarise_at(c(xCol0), list(~. |> max(na.rm=T), ~. |> nth(2))) |>
    rename_at(c(funs0), ~xStr0 |> paste0(bounds0)) |>
    mutate(xLo = case_when(xLo |> is.na() ~ xHi, .default = xLo))
  # df1 |> glimpse()

  ### Join with model info and reduce
  df0    <- bounds0 |> map(function(
    boundX,
    oldX   = c(xCol0, modCol0),
    newX   = c(xStr0, modCol0),
    yrColX = yrCol0
  ){
    ### Columns
    newX   <- newX   |> paste0(boundX)
    xColX  <- newX[1]
    mColX  <- newX[2]
    groupX <- yrColX |> c(xColX)
    sortX  <- groupX |> c(mColX)
    ### Rename data
    dfX   <- df0 |> ungroup() |> rename_at(c(oldX), ~newX)
    ### Join with values
    dfX   <- df1 |> left_join(dfX, by=c(yrColX, xColX))
    # dfX |> nrow() |> print()
    ### Arrange and make sure values are unique
    dfX   <- dfX |>
      arrange_at (c(sortX)) |>
      group_by_at(c(groupX)) |>
      slice_head(n = 1) |>
      ungroup()
    ### Return
    return(dfX)
  }) |> reduce(left_join, by=c(yrCol0, xStr0 |> paste0(bounds0)))
  # df0 |> glimpse()
  
  ### Calculate xRef
  df0    <- df0 |> mutate(xRef = case_when(xHi > xLo ~ xHi, .default=xLo))
  
  ### Return
  return(df0)
}


## Reshape/Format Scaled Impacts ----------------
# ## Standardize scaled impacts
# standardize_scaledImpacts <- function(
#     df0,      ### Tibble of scaled impacts data, e.g.: stateData$slrData
#     df1,      ### Tibble of sector group data, e.g.: frediData$co_sectorsInfo
#     minYr0    = 2010,
#     maxYr0    = 2300,
#     # minYr0    = "minYear0" |> get_frediDataObj("fredi_config", "rDataList"),
#     # maxYr0    = "npdYear0" |> get_frediDataObj("fredi_config", "rDataList"),
#     mType0    = "gcm",
#     xCol0     = c("modelUnitValue"),
#     idCol0    = c("scenario_id"),
#     mainCols0 = c("sector", "variant", "impacType", "impactYear"),
#     regCols0  = c("region", "state", "postal"),
#     mTypeCol0 = c("model_type"),
#     modCol0   = c("model"),
#     yrCol0    = c("year"),
#     dropCols0 = c("fips")
# ){
#   ### Values
#   # df0 |> glimpse()
#   # # cols0    <- c(mainCols0, mTypeCol0, modCol0, yrCol0)
#   # cols0    <- c(mainCols0, regCols0, mTypeCol0, mmodCol0, yrCol0)
#   # mType0   <- df0 |> pull(all_of(mTypeCol0)) |> unique()
#   # # sectors0 <- df0 |> pull(sector) |> unique()
#   # # sectors0 |> print()
#   mType0   <- df0 |> pull(all_of(mTypeCol0)) |> unique()
#   doSlr0   <- (mType0 |> tolower()) %in% "slr"
#   
#   # Add years to tibble of sector group info? Don't need to do this for GCM scaled impacts
#   # years0   <- tibble() |> mutate(year = minYr0:maxYr0)
#   # df1      <- df1 |> cross_join(years0) |> select(-any_of(dropCols0))
#   # cols0    <- c(mainCols0, regCols0, mTypeCol0)
#   cols0    <- c(mainCols0, regCols0)
#   if(!doSlr0) {cols0 <- cols0 |> c(modCol0)}
#   cols0    <- cols0 |> c(idCol0)
#   df1      <- df1   |> 
#     filter_at(c(mTypeCol0), function(x, y=mType0){x %in% y}) |> 
#     select(all_of(select0)) |> 
#     arrange_at(c(cols0))
#   # df1 |> glimpse()
#   
#   ### Extend data
#   df0      <- df0 |> extend_data(to0=maxYr0)
#   
#   ### Get scenario ID and select columns
#   include0 <- cols0 |> get_matches(y=c(mainCols0, mTypeCol0, idCol0), matches=F)
#   # sort0    <- cols0 |> c(xCol0)
#   df0      <- df0   |> 
#     get_scenario_id(include=include0, col0=idCol0) |> 
#     arrange_at(c(cols0)) |>
#     select(-any_of(include0))
#   
#   ### Join with data
#   ### Add scenario value
#   join0    <- c(idCol0)
#   group0   <- cols0 |> get_matches(y=mTypeCol0)
#   df0      <- df1   |> 
#     left_join(df0, by=join0, relationship="many-to-many") |> 
#     arrange_at(c(cols0))
#   # arrange_at(c(cols0)) |> 
#   # group_by_at(c(group0))
#   rm(df1)
#   
#   ### Figure out if there are scenarios present
#   # hasScen0 <- df0 |> filter_if(vars("scaled_impacts"), ~ !(. |> is.na())) |> pull(scenario_id) |> unique()
#   dfHas0  <- df0 |> 
#     filter_at(c(yCol0), function(x){!(x |> is.na())}) |> 
#     select(all_of(idCol0)) |> 
#     distinct() |>
#     mutate(hasScenario = TRUE)
#   
#   ### Add column to data
#   df0      <- df0 |> 
#     left_join(dfHas0, by=idCol0) |>
#     mutate_at(c("hasScenario"), replace_na, FALSE)
#   
#   ### Return
#   return(df0)
# }

### Reshape/Format SLR Impacts
### idCols0: values to include in ID
reshape_modelImpacts <- function(
    df0,      ### Tibble of model scaled impacts
    type0     = "slr", ### Model Type
    xCol0     = c("year"),
    yCol0     = c("scaled_impacts"),
    idCol0    = c("scenario_id"),
    modCol0   = c("model"),
    idCols0   = c("sector", "variant", "impactType", "impactYear", "region", "postal"), 
    # modLvls0  = c(0, 30,  50, 100, 150, 200, 250) |> paste0("cm"), ### Model levels to use for factoring...from co_slrCm
    modStr0   = c("Interpolation")
){
  ### Add scenario_id, sort, group 
  doSlr0   <- type0   |> str_detect("slr")
  doGcm0   <- type0   |> str_detect("gcm")
  type0    <- type0 |> tolower()
  group0   <- idCols0
  if(doSlr0) {
    group0 <- group0 |> c(xCol0) |> unique()
    sort0  <- group0 |> c(yCol0, modCol0) |> unique()
  } else if(doGcm0) {
    group0 <- group0 |> c(modCol0) |> unique()
    sort0  <- group0 |> c(xCol0) |> unique()
  } else{
    sort0  <- group0 |> c(xCol0) |> unique()
  } ### End if(doSlr0)
  
  ### Factor model### Determine if model has already been factored
  hasLvls0 <- df0 |> pull(all_of(modCol0)) |> is.factor()
  ### Factor model if !hasLvls
  if(!hasLvls0 & doSlr0){
    lvls0  <- df0 |> get_slrLevels(modCol0=modCol0, addZero=T, addUnit=T)
    df0    <- df0 |> mutate_at(c(modCol0), factor, lvls0)
  } ### End if(!hasLvls0)
  
  ### Add scenario ID
  tmpCol0  <- "modelTemp"
  typeCol0 <- "model_type"
  # sort0  <- group0 |> c(xCol0, yCol0, modCol0)
  df0      <- df0    |> 
    mutate(model_type = type0) |>
    mutate(modelTemp = case_when(model_type |> str_detect("slr") ~ modStr0, .default=model)) |>
    get_scenario_id(include0=idCols0 |> c(tmpCol0), idCol0="id") |>
    rename_at (c("id"), ~idCol0) |>
    relocate(all_of(idCol0), .before=all_of(xCol0)) |>
    select(-any_of(tmpCol0), -any_of(typeCol0))
  # "gotHere8" |> print(); df0 |> nrow() |> print()
   
  ### Figure out which have no impacts
  cols0    <- idCol0 |> c("hasScenario")
  df0      <- df0    |>
    filter_at(c(yCol0), function(x){!(x |> is.na())}) |>
    mutate(hasScenario = 1)

  ### Arrange and group
  # group0   <- group0 |> c(idCol0)
  df0      <- df0    |>
    arrange_at (c(sort0 )) |>
    group_by_at(c(group0, idCol0))

  ### Return
  return(df0)
}

### Reshape/Format SLR Impacts
format_slrData <- function(
    df0,      ### Tibble of reshaped SLR scaled impacts (output of reshape_modelImpacts)
    xCol0     = c("year"),
    yCol0     = c("scaled_impacts"),
    idCol0    = c("scenario_id"),
    modCol0   = c("model"),
    modStr0   = c("Interpolation")
){
  ### Add scenario_id, sort, group 
  # # group0 <- df0    |> group_cols() |> c(xCol0, yCol0)
  # # group0 <- c(idCol0, xCol0, yCol0)
  # gCols0 <- df0 |> group_cols()
  # group0 <- yCol0
  # sort0  <- gCols0 |> c(yCol0, modCol0)
  # df0    <- df0    |> 
  #   # arrange_at (c(sort0  )) |>
  #   group_by_at(c(yCol0), .add=T) |>
  #   # group_by_at(c(group0), .add=F) |>
  #   slice_head(1)
  df0    <- df0    |> 
    # arrange_at (c(sort0  )) |>
    group_by_at(c(yCol0), .add=T) |>
    # group_by_at(c(group0), .add=F) |>
    slice_head(n=1)
  ### Return
  return(df0)
}


# ### Function to iterate over in fun_slrConfigExtremes
# get_slrExtValues <- function(
#     df0,      ### Tibble of SLR impacts
#     df1,      ### Tibble of SLR max heights (outputs of get_slrMaxHeights)
#     xCol0     = c("year"),
#     yCol0     = c("scaled_impacts"),
#     idCol0    = c("scenario_id"),
#     modCol0   = c("model")
#     # idCol0    = c("scenario_id", "hasScenario"),
#     # modCol0   = c("model"),
#     # group0    = c("sector", "variant", "impactType", "impactYear", "region", "postal")
# ){
#   ### Strings
#   loStr0   <- "Lo"
#   hiStr0   <- "Hi"
#   bounds0  <- c(loStr0, hiStr0)
#   xStr0    <- "x"
#   yStr0    <- "y"
#   xRef0    <- "xRef"
#   
#   ### Select data
#   select0  <- c(idCol0, xCol0, yCol0, modCol0)
#   keys0    <- df0   |> group_keys()
#   keys0 |> glimpse()
#   ids0     <- keys0 |> pull(all_of(idCol0)) |> head(5)
#   group0   <- keys0 |> names()
#   # df0      <- df0   |> ungroup() |> select(all_of(select0)) |> rename_at(c(yCol0), ~yStr0)
#   keys0    <- keys0 |> filter_at(c(idCol0), function(x, y=ids0){x %in% y})
#   df0      <- df0   |> filter_at(c(idCol0), function(x, y=ids0){x %in% y}) |> filter(year %in% 2050)
#   df0 |> glimpse()
#   df0      <- df0  |> group_map(function(.x, .y){
#     .x |> 
#       arrange_at(c(xCol0, yCol0, modCol0)) |>
#       group_by_at(c(xCol0, yCol0)) |>
#       
#   })
#   ### Join data
#   old0     <- c(modCol0)
#   joinLo0  <- c(old0, xStr0) |> paste0(loStr0)
#   joinHi0  <- c(old0, xStr0) |> paste0(hiStr0) |> c(idCol0, xRef0)
#   # df0      <- df1 |> 
#   #   left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(loStr0)), by=joinLo0) |> 
#   #   left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(hiStr0)), by=joinHi0) |> 
#   df1
#   df1      <- df1 |> 
#     filter(year %in% 2050) |>
#     left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(loStr0)), by=joinLo0)
#   df0 |> glimpse()
#   df1 |> glimpse()
#   df0      <- df1 |> 
#     left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(hiStr0)), by=joinHi0) |>
#     relocate(all_of(idCol0))
#   df0 |> glimpse()
#   rm(old0, joinLo0, joinHi0, df1)
#   
#   ### Join group keys, arrange
#   # sort0  <- c(bounds0, group0, xCol0, yCol0, modCol0) |> unique()
#   join0  <- c(idCol0)
#   sort0  <- c(idCol0, xCol0, yCol0, modCol0) |> unique()
#   df0    <- keys0 |> 
#     left_join(df0, by=join0) |> 
#     arrange_at (c(sort0)) |> 
#     group_by_at(c(group0))
#   
#   ### Return
#   return(df0)
# }

### Function to iterate over in fun_slrConfigExtremes
get_slrExtValues <- function(
    df0,      ### Tibble of SLR impacts
    df1,      ### Tibble of SLR max heights (outputs of get_slrMaxHeights)
    xCol0     = c("year"),
    yCol0     = c("scaled_impacts"),
    idCol0    = c("scenario_id"),
    modCol0   = c("model")
    # idCol0    = c("scenario_id", "hasScenario"),
    # modCol0   = c("model"),
    # group0    = c("sector", "variant", "impactType", "impactYear", "region", "postal")
){
  ### Strings
  loStr0   <- "Lo"
  hiStr0   <- "Hi"
  bounds0  <- c(loStr0, hiStr0)
  xStr0    <- "x"
  yStr0    <- "y"
  xRef0    <- "xRef"
  
  ### Select data
  select0  <- c(idCol0, xCol0, yCol0, modCol0)
  keys0    <- df0   |> group_keys()
  # keys0 |> glimpse()
  ids0     <- keys0 |> pull(all_of(idCol0)) |> head(5)
  group0   <- keys0 |> names()
  # keys0    <- keys0 |> filter_at(c(idCol0), function(x, y=ids0){x %in% y})
  # df0      <- df0   |> filter_at(c(idCol0), function(x, y=ids0){x %in% y})
  # keys0    <- keys0 |> filter(year %in% 2050)
  # df0      <- df0   |> filter(year %in% 2050)
  # df1      <- df1   |> filter(year %in% 2050) # |> select(-any_of(xRef0))
  df0      <- df0   |> ungroup() |> select(any_of(select0))
  # keys0 |> glimpse(); 
  # df0 |> glimpse(); df1 |> glimpse()
  
  ### Join data
  # join0    <- c(idCol0, xCol0)
  df0      <- bounds0 |> map(function(
    boundX, 
    modX   = modCol0,
    yColX  = yCol0,
    xColX  = xCol0
  ){
    ### Columns
    oldX  <- c(modX, yColX)
    newX  <- c(modX, yStr0)  |> paste0(boundX)
    colsX <- xColX |> c(c(modX, xStr0)  |> paste0(boundX))

    ### Join values
    # df1 |> select(all_of(colsX)) |> glimpse(); df0 |> rename_at(c(oldX), ~newX) |> glimpse()
    dfX   <- df1  |> 
      select(all_of(colsX)) |> 
      left_join(
        df0  |> rename_at(c(oldX), ~newX), 
        by=xCol0 |> c(modX |> paste0(boundX))
      ) ### End left_join()

    ### Relocate columns
    moveX <- c(idCol0, xColX) |> c(c(xStr0, yStr0, modCol0) |> paste0(boundX))
    dfX   <- dfX |> relocate(any_of(moveX))
    # dfX |> glimpse()
    
    ### Return
    return(dfX)
  }) |> reduce(left_join, by=c(idCol0, xCol0))
  # return(list(keys0=keys0, df1=df1, df0=df0))
  # df0 |> glimpse()
  
  ### Reduce and join with xRef values
  select0  <- c(xCol0, xRef0)
  df1      <- df1 |> select(any_of(select0), any_of(xRef0))
  df0      <- df0 |> 
    left_join(df1, by=xCol0) |>
    relocate(any_of(xRef0), .after=all_of(xCol0))
  # df0 |> glimpse()
  rm(select0, df1)
  
  ### Join group keys, arrange
  join0  <- c(idCol0, xCol0)
  group0 <- group0 |> get_matches(y=c(xCol0, yCol0, modCol0), matches=F)
  # df0 |> glimpse(); keys0 |> glimpse()
  df0    <- keys0 |> 
    left_join(df0, by=join0) |>
    arrange_at (c(join0)) |> 
    group_by_at(c(group0)) |> 
    relocate(any_of(idCol0), .before=all_of(xCol0))
  # df0 |> glimpse()
  
  ### Return
  return(df0)
}

### Function for dealing with SLR values above the maximum
fun_slrConfigExtremes <- function(
    df0, ### Outputs of get_slrExtValues()
    modCol0 = "model"
){
  ### Strings
  loStr0  <- "Lo"
  hiStr0  <- "Hi"
  bounds0 <- c(loStr0, hiStr0)
  xStr0   <- "x"
  yStr0   <- "y"
  
  ### Calculate differences and drop columns
  dCols   <- c(xStr0, yStr0) |> paste0("Delta")
  df0     <- df0 |>
    # ### X reference value
    # mutate(xRef      = case_when(xHi > xLo ~ xHi, .default=xLo)) |> 
    ### Calculate deltas and convert to absolute
    mutate(xDelta = xHi - xLo) |> 
    mutate(yDelta = yHi - yLo) |> 
    mutate_at(c(dCols), abs) |>
    ### Calculate slope & replace zeros
    mutate(slope     = case_when(xHi == xLo ~ 0, .default = yDelta / xDelta)) |> 
    ### Calculate intercept
    mutate(intercept = case_when(yHi > yLo ~ yHi, .default=yLo))
    # mutate(intercept = case_when(yHi < yLo ~ yHi, .default=yLo))
  # df0 |> glimpse()
  
  ### Drop columns
  # drop0   <- c(xStr0, yStr0, modCol0) |> map(paste0, bounds0) |> unlist() |> c(dCols)
  # df0     <- df0 |> select(-any_of(drop0))
  
  ### Return
  return(df0)
}


## Format Scalar data and sector info ----------------
update_sectorInfo <- function(
    types0,  ### Model types
    df0      = NULL, ### co_sectorsInfo output from reshapeConfigData
    list0    = NULL, ### Impacts list output from reshapeScaledImpacts
    typeCol0 = "model_type",
    idCol0   = "scenario_id",
    valCol0  = "hasScenario",
    nameStr0 = "Data"
){
  ### Figure out which values have a scenario present
  select0    <- c(idCol0, valCol0)
  dNames0    <- types0  |> paste0(nameStr0)
  df0        <- list(typeX=types0, dfX1=list0) |> 
    pmap(function(typeX, dfX1){
      ### Ids
      dfX1 <- dfX1 |> ungroup() |> select(all_of(select0)) |> distinct()
      ### Data
      dfX0 <- df0   |> 
        filter_at(c(typeCol0), function(x, y=typeX){(x |> tolower()) %in% y}) |>
        left_join(dfX1, by=idCol0) |>
        mutate_at(c(valCol0), replace_na, 0)
      ### Return
      return(dfX0)
    }) |> bind_rows()
  ### Return
  return(df0)
}

### fun_formatScalars
fun_formatScalars <- function(
    df0, ### rDataList$scalarDataframe
    df1, ### rDataList$co_scalarInfo,
    # yrs0, ### rDataList$list_years
    years0   = 2010:2300,
    yrCol0   = c("year"),
    valCol0  = c("value"),
    idCol0   = c("groupId"),
    # regCols0 = c("region", "state", "postal"),
    regCols0 = c("region", "postal"),
    # scCols0  = c("scalarType", "scalarName"),
    natStr0  = c("US"),
    noneStr0 = c("none"),
    noneVal0 = 1,
    rule0    = 2
){
  ### Columns
  regCol0   <- "region"
  postCol0  <- "postal"
  stateCol0 <- "state"
  ### Method columns
  rCol0    <- "regional"
  dCol0    <- "dynamic"
  mCol0    <- "method"
  # idCol0   <- "group_id"
  typeCol0 <- "scalarType"
  nameCol0 <- "scalarName"
  scCols0  <- c(typeCol0, nameCol0)
  
  ### Select Info
  # df0 |> glimpse(); df1 |> glimpse()
  select0  <- scCols0 |> c(regCols0, yrCol0, valCol0)
  select1  <- scCols0 |> c(rCol0, dCol0)
  df0      <- df0 |> select(any_of(select0))
  df1      <- df1 |> select(any_of(select1))
  # df0 |> glimpse(); df1 |> glimpse()
  rm(select0, select1)
  
  ### Add "none" as scalar types:
  ### - Get data frame with type and scalarName == "none"
  ### - Get national region info
  noneStr0 <- "none"
  noneVal0 <- 1
  ### - Get national region info
  select0  <- c(regCols0)
  dfNone   <- df0 |>
    filter_at(c(postCol0), function(x, y=natStr0){x %in% y}) |>
    select(all_of(select0)) |> distinct() |> 
    mutate(scalarName = noneStr0) |>
    mutate(value = noneVal0) |>
    rename_at(c("value"), ~valCol0) |> 
    cross_join(tibble(year = years0))
  ### Bind dfNone with df0
  df0      <- df0 |> bind_rows(dfNone)
  # df0 |> glimpse(); 
  rm(dfNone)
  
  ### Bind dfNone with df0 and join info
  move0    <- c(scCols0, rCol0, dCol0)
  sort0    <- c(dCol0, rCol0, typeCol0, nameCol0, regCols0, yrCol0)
  df0      <- df0 |> 
    left_join(df1, by=nameCol0) |>
    relocate(all_of(move0)) |>
    arrange_at(c(sort0))
  rm(df1)
  
  ### Arrange and get groups
  ### Add column indicating method
  idCols0  <- c(typeCol0, nameCol0, regCols0)
  group0   <- c(idCols0, idCol0, dCol0, rCol0, mCol0) |> c("regional")
  df0      <- df0 |>
    arrange_at(c(sort0)) |> 
    mutate(group_id = df0 |> select(all_of(idCols0)) |> apply(1, function(x){
      x |> as.vector() |> paste(collapse="_")
    }) |> unlist()) |>
    rename_at(c("group_id"), ~idCol0) |>
    mutate(method = case_when(
      dynamic == 0 ~ "constant", 
      .default="linear"), 
      .before=all_of(yrCol0)
    ) |> group_by_at(c(group0))
  
  ### Iterate over groups
  methods0 <- df0      |> pull(all_of(mCol0)) |> unique() |> sort()
  df0      <- methods0 |> map(function(methodX){
    df0 |>
      filter_at(c(mCol0), function(x, y=methodX){x %in% y}) |>
      group_map(function(.x, .y){
        .x |> interpolate_byGroup(
          .y      = .y,
          xCol0   = yrCol0,
          yCol0   = valCol0,
          xOut0   = years0,
          method0 = methodX,
          rule0   = rule0
        ) ### End interpolate_byGroup
      }) |> bind_rows()
  }) |> bind_rows()

  ### Drop values
  df0      <- df0 |>
    ungroup() |>
    select(-any_of(mCol0))
  
  ### Return
  return(df0)
}

## Format GCM Scaled Impacts ----------------
### Extrapolate Impact Function
### Function to extrapolate an impact function
extrapolate_gcmImpacts_byGroup <- function(
    df0,
    xCol0 = "xIn", ### Which column to use for x (default = temp_C)
    yCol0 = "yIn", ### Which column to use for y
    xMin0 = 0,     ### Minimum value for x
    yMin0 = 0,     ### Value of y at minimum value of x 
    from0 = NULL,  ### Maximum value for model type to extend from, if not missing
    to0   = 30  ,   ### Extend last points for x
    all0  = FALSE  ### Whether to extend all models or just those that go to the max model value
    # all0  = FALSE,  ### Whether to extend all models or just those that go to the max model value
    # unitScale   = 0.5 ,   ### Scale between values,
    # extrapolate = TRUE,
){
  ### Filter to values greater than the reference
  ### Select columns and filter out NA values
  old0   <- c(xCol0, yCol0)
  new0   <- c("xIn", "yIn")
  
  ### Standardize minimum value, then get unique values
  df0    <- df0 |> 
    arrange_at(c(xCol0)) |>
    head(1) |> 
    mutate_at(c(xCol0), function(x, y=xMin0){y}) |> 
    mutate_at(c(yCol0), function(x, y=yMin0){y}) |> 
    bind_rows(df0 |> filter_at(c(xCol0), function(x, y=xMin0){x > xMin0}))
  
  ### Extend from/to
  ### Get max value from which to extend values
  # extendTo  <- to0 |> as.character() |> as.numeric()
  xMax0  <- df0 |> pull(all_of(xCol0)) |> max(na.rm=T)
  tail0  <- df0 |> tail(2) |> rename_at(c(old0), ~new0)
  doExt0 <- xMax0 < to0 | all0
  if(tail0 |> pull(yIn) |> is.na() |> all()) tail0 |> glimpse()
  
  ### If extrapolate:
  if(doExt0){
    ### Get linear trend
    lm0    <- lm(yIn~xIn, data=tail0)
    int0   <- lm0$coefficients[[1]]
    slope0 <- lm0$coefficients[[2]]
    ### Create data
    dfNew  <- tibble() |> 
      tibble(xIn = to0) |>
      mutate(yIn = int0 + xIn * slope0) |> 
      rename_at(c(new0), ~old0)
    ### Extend values, then bind with earlier observations
    df0    <- df0 |> bind_rows(dfNew)
  } ### End if(doExtrap)
  
  ### Return
  return(df0)
}


### Function to extend GCM Impacts for interpolation
extrapolate_gcmImpacts <- function(
    df0,    ### Data frame with scaled impacts data
    idCol0  = "scenario_id"   , ### Which column to look for the scenario column name (default =  )
    xCol0   = "modelUnitValue", ### Which column to use for x (default = temp_C)
    yCol0   = "scaled_impacts", ### Which column to use for y
    xMin0   = 0    , ### Minimum value for x
    yMin0   = 0    , ### Value of y at minimum value of x 
    from0   = NULL , ### Maximum value for model type to extend from, if not missing
    to0     = 30   , ### Extend last points for x
    all0    = FALSE, ### Whether to extend all models or just those that go to the max model value
    method0 = "linear",
    rule0   = 1
    # rule0       = 1,
    # extrapolate = FALSE, ### Whether to extrapolate by default
    # unitScale   = NULL,  ### Scale between values
){
  ### Groups ----------------
  ### - Create groups and get group keys
  ### - Filter out NA values
  ### - Filter to values greater than xMin0
  ### - Rename columns
  cols0  <- c(xCol0, yCol0)
  keys0  <- df0   |> group_keys()
  group0 <- keys0 |> names()
  ids0   <- keys0 |> pull(all_of(idCol0)) |> unique()
  # ids0   <- ids0  |> head(5)
  # keys0  <- keys0 |> filter_at(c(idCol0), function(x, y=ids0){x %in% y})
  # keys0  <- df0   |> filter_at(c(idCol0), function(x, y=ids0){x %in% y})
  # ids0 |> print(); df0 |> glimpse(); keys0 |> glimpse()

  ### Filter and Arrange Data ----------------
  df0    <- df0 |> 
    arrange_at(c(idCol0, xCol0)) |> 
    filter_at (c(idCol0), function(x, y=ids0){x %in% y}) |>
    filter_at (c(yCol0), function(x){!(x |> is.na())}) |>
    filter_at (c(xCol0), function(x){x > xMin0})
  # df0 |> glimpse()
  
  ### Extrapolate Data ----------------
  ### Extrapolate values
  cols0    <- c(xCol0, yCol0)
  df0      <- df0 |> group_map(function(.x, .y){
    .x |> 
      ungroup() |>
      select(all_of(cols0)) |> 
      extrapolate_gcmImpacts_byGroup(
        xCol0 = xCol0, 
        yCol0 = yCol0, 
        xMin0 = xMin0, 
        yMin0 = yMin0, 
        from0 = from0,
        to0   = to0,  
        all0  = all0  
      ) ### extrapolate_impFunction
  }) |> set_names(ids0) |> 
    bind_rows(.id=idCol0)
  
  ### Join Group Info ----------------
  ### Join data and rename columns
  df0      <- keys0 |> 
    filter_at(c(idCol0), function(x, y=ids0){x %in% y}) |>
    left_join(df0, by=idCol0) |>
    arrange_at(c(idCol0, xCol0)) |>
    group_by_at(c(group0))
  
  ### Return ----------------
  gc()
  return(df0)
}

### Approximation function
### Create a piece-wise linear interpolation function using approxfun and defaults
###    rule = 1 (Returns NA for x-values outside range)
###    ties = mean (take the average of multiple values)
get_impactFunctions <- function(
    df0,    ### Tibble with extended/grouped GCM scaled impacts (output of extend_gcmImpacts)
    idCol0  = "scenario_id"   , ### Which column to look for the scenario column name (default =  )
    xCol0   = "modelUnitValue", ### Which column to use for x (default = temp_C)
    yCol0   = "scaled_impacts", ### Which column to use for y
    method0 = "linear",
    rule0   = 1
){
  ### Groups
  df0   <- df0   |> arrange_at (c(idCol0, xCol0))
  keys0 <- df0   |> group_keys()
  ids0  <- keys0 |> pull(all_of(idCol0))
  
  ### Get Impact Functions
  list0 <- df0 |> group_map(function(.x, .y){
    inX  <- .x |> pull(all_of(xCol0))
    inY  <- .x |> pull(all_of(yCol0))
    funX <- approxfun(x=inX, y=inY, method=method0, rule=rule0)
    return(funX)
  }) |> set_names(ids0)
  
  ### Return the list (i.e., list0)
  gc()
  return(list0)
}


### Function to format SLR values
format_slrScaledImpacts <- function(
    df0, 
    dfExt0, ### Extrems data (controlData[["slrCmExtremes"]])
    xCol0    = "year",
    yCol0    = "scaled_impacts",
    idCol0   = "scenario_id", 
    modCol0  = "model",
    group0   = c("sector", "variant", "impactType", "impactYear", "region", "postal"),
    modStr0  = "Interpolation",
    silent   = TRUE,
    msg0     = 0
){
  ### Msg0
  msgUser <- !silent
  msg0    <- msg0 + 1
  
  ### Initialize list
  list0   <- list()
  ### Format impacts
  if(msgUser) msg0 |> get_msgPrefix() |> paste("Formatting SLR impact values...") |> message()
  # df1     <- df0 |> format_slrData(
  #   xCol0    = xCol0,
  #   yCol0    = yCol0,
  #   idCol0   = idCol0,
  #   modCol0  = modCol0,
  #   # group0   = group0,
  #   modStr0  = "Interpolation"
  # ) ### End format_slrImpacts
  # list0[["slrImpacts"]] <- df1
  list0[["slrData"]] <- df0
  # rm(df0)
  
  ### Format extremes
  if(msgUser) msg0 |> get_msgPrefix() |> paste("Creating extreme SLR impact values...") |> message()
  # df2     <- df1 |> get_slrExtValues(
  df2     <- df0 |> get_slrExtValues(
    df1      = dfExt0,
    xCol0    = xCol0,
    yCol0    = yCol0,
    idCol0   = idCol0,
    modCol0  = modCol0
  ) |> fun_slrConfigExtremes() 
  list0[["slrExtremes"]] <- df2
  rm(df2)
  
  ### Return
  return(list0)
}

### Function to format SLR values
format_gcmScaledImpacts <- function(
    df0, 
    xCol0   = "modelUnitValue",
    yCol0   = "scaled_impacts",
    idCol0  = "scenario_id", 
    modCol0 = "model",
    group0  = c("sector", "variant", "impactType", "impactYear", "region", "postal"),
    modStr0 = "Interpolation",
    to0     = 30  , ### Value to extend values to
    all0    = TRUE, ### Whether to extrapolate cold models, too
    method0 = "linear",
    rule0   = 1   , ### Rule for extrapolation
    silent  = TRUE,
    msg0    = 0
){
  ### Msg0
  msgUser <- !silent
  msg0    <- msg0 + 1
  
  ### Initialize list
  list0  <- list()
  ### Format impacts
  ### Extend values
  if(msgUser) msg0 |> get_msgPrefix() |> paste("Extending GCM impact values...") |> message()
  df0    <- df0 |> extrapolate_gcmImpacts(
    xCol0   = xCol0, 
    yCol0   = yCol0, 
    idCol0  = idCol0,
    to0     = to0,
    all0    = all0,
    method0 = method0,
    rule0   = rule0
  ) ### End extrapolate_gcmImpacts
  list0[["gcmData"]] <- df0
  
  ### Get impact functions
  if(msgUser) msg0 |> get_msgPrefix() |> paste("Creating GCM impact functions...") |> message()
  funs0  <- df0 |> get_impactFunctions(
    idCol0  = idCol0,
    xCol0   = xCol0,
    yCol0   = yCol0,
    method0 = method0,
    rule0   = rule0
  ) ### get_impactFunctions
  list0[["gcmFuns"]] <- funs0
  rm(df0)
  
  ### Return
  return(list0)
}

### SAve Data ----------------
### Save Objects to sys data
fun_saveSysData <- function(
  dataDir     = "." |> file.path("data"),
  controlFile = "controlData",
  scenarioDir = "scenarios",
  outFile     = "tmp_sysData",
  modules     = c("fredi", "ghg", "sv"),
  extStrs     = c("rda", "rds")
){
  ### Extensions
  strSep0    <- "|"
  dotStr0    <- "\\."
  extStr0    <- extStrs |> paste(collapse=strSep0)
  extStr1    <- dotStr0 |> paste0(extStrs) |> paste(collapse=strSep0)
  
  ### Load scenario files
  scenDir    <- dataDir   |> file.path(scenarioDir)
  scenFiles  <- scenDir   |> list.files(pattern=extStr0, full.names=F)
  scenNames  <- scenFiles |> str_replace(pattern=extStr1, "")
  ### - Module Files
  modFiles   <- dataDir |> file.path(modules) |> 
    map(list.files, pattern=extStr0, full.names=F) |> 
    set_names(modules)
  modNames   <- modFiles |>
    map(str_replace, pattern=extStr1, "") |> 
    unlist()
  # modFiles |> print()
  # modNames |> unlist() |> print()
  
  ### Data names
  dataNames  <- c(controlFile, scenNames) |> c(modNames |> unlist())
  
  ### Load data
  ### - Load control data
  dataDir |> list.files() |> print()
  ctrlPath   <- dataDir |> list.files(pattern=controlFile, full.names=T)
  ctrlPath |> load()
  # ctrlPath |> print()
  
  ### - Scenarios
  for(file_j in scenFiles){
    scenDir |> file.path(file_j) |> load()
  }
  
  
  ### - Module data
  for(mod_i in modules){
    files_i <- modFiles[[mod_i]]
    for(file_j in files_i){
      dataDir |> file.path(mod_i, file_j) |> load()
    }
  }

  ### Save data
  outPath <- dataDir |> file.path(outFile) |> paste0(".", "rda")
  save(list=c(dataNames), file=outPath)

  ### Return
  return("Success")
}
