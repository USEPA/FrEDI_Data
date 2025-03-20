### FrEDI Data Utilities
## Import functions from FrEDI namespace ----------------
convertTemps       <- "convertTemps"       |> utils::getFromNamespace("FrEDI")
get_matches        <- "get_matches"        |> utils::getFromNamespace("FrEDI")
get_msgPrefix      <- "get_msgPrefix"      |> utils::getFromNamespace("FrEDI")
get_scenario_id    <- "get_scenario_id"    |> utils::getFromNamespace("FrEDI")
# interpolate_annual <- "interpolate_annual" |> utils::getFromNamespace("FrEDI")
# match_scalarValues <- "match_scalarValues" |> utils::getFromNamespace("FrEDI")
temps2slr          <- "temps2slr"          |> utils::getFromNamespace("FrEDI")
# update_popScalars  <- "update_popScalars"  |> utils::getFromNamespace("FrEDI")

### For SV
calc_countyPop     <- "calc_countyPop" |> utils::getFromNamespace("FrEDI")

## Misc FrEDI Data Utilities  ----------------
###### Function to get years from data
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
  dfNew0 <- tibble(xVal = vals0) |> rename_at(c(xVal), ~xCol0)
  # vals0 |> print(); dfNew0 |> glimpse()
  
  ### Data to extend
  dfNew0 <- df0 |> 
    filter_at(c(xCol0), function(x, y=from0){x %in% y)) |> 
    select(-any_of(xCol0)) |> 
    cross_join(dfNew0)

  ### Bind extended data
  df0    <- df0 |> 
    filter_at(c(xCol0), function(x, y=from0){x <= y)) |> 
    bind_rows(dfNew0)
  
  ### Return
  return(df0)
}

## Scenario Functions ----------------
## Function to load data of specific type
loadScenarioData_byType <- function(
    type0    = "temp",
    info0,   ### co_scenarios from configureScenarioData
    dir0     = "." |> file.path("inst", "extdata", "scenarios"), 
    ext0     = "csv",
    typeCol0 = "inputName",
    yrCol0   = c("year"),
    idCol0   = c("scenarioName"),
    silent   = TRUE,
    msg0     = 0
){
  ### Filter to data
  names0 <- info0 |> names()
  info0  <- info0 |> 
    filter_at(c(typeCol0), function(x, y=type0){x %in% y}) |>
    group_by_at(c(names0))
  ids0   <- info0 |> pull(all_of(idCol0))
  # dir0 |> print()
  ### Read in files and bind data
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Loading ", type0, "scenario data...") |> message()
  data0 <- info0 |> group_map(function(.x, .y){
    loadScenarioData_byGroup(
      .x=.x, 
      .y=.y,
      dir0     = dir0, 
      ext0     = ext0,
      typeCol0 = typeCol0,
      idCol0   = idCol0,
      silent   = silent,
      msg0     = msg0 + 1
    ) ### End loadScenarioData_byGroup
  }) |> bind_rows() |>
    group_by_at(c(names0))
  # data0 <- info0 |> group_map(
  #   loadScenarioData_byGroup,
  #   dir0     = dir0, 
  #   ext0     = ext0,
  #   typeCol0 = typeCol0,
  #   idCol0   = idCol0,
  #   msg0     = msg0 + 1
  # ) |> bind_rows()
  ### Return data
  return(data0)
}

### Load scenario by scenarioName
## .x = group data
## .y = group key data
loadScenarioData_byGroup <- function(
    .x, 
    .y,
    # type0    = "temp",
    info0,   ### co_scenarios from configureScenarioData
    dir0     = "." |> file.path("inst", "extdata", "scenarios"), 
    ext0     = "csv",
    typeCol0 = "inputName",
    idCol0   = c("scenarioName"),
    silent   = TRUE,
    msg0     = 0
){
  ### File paths
  # dir0 |> print()
  # .x |> glimpse(); .y |> glimpse()
  type0 <- .y |> pull(all_of(typeCol0)) |> unique()
  file0 <- .y |> pull(all_of(idCol0  )) |> unique()
  path0 <- dir0 |> file.path(type0, file0) |> paste0(".", ext0)
  # path0 |> glimpse()
  ### Read in file
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Loading data from ", file0 |> paste0(".", ext0), "...") |> message()
  data0 <- path0 |> read.csv()
  data0 <- .y |> 
    cross_join(data0) |> 
    filter_all(any_vars(!(. |> is.na())))
  ### Return data
  return(data0)
}

### Reshape specific scenarios
reshapeScenarioData_byType <- function(
    type0    = "temp",
    list0, ### List of data with types
    typeCol0 = c("inputName"),
    idCol0   = c("scenarioName"),
    argCol0  = c("inputArgVal"),
    valCol0  = c("valueCol"),
    yrCol0   = c("year"),
    method0  = "linear",
    silent   = TRUE,
    rule0    = 1,
    msg0     = 0
){ 
  ### Filter to data
  # doTemp0 <- type0 |> str_detect("temp")
  data0   <- list0[[type0]]
  # cols0   <- data |> group_cols()
  # if(doTemp0) cols0 <- cols0 |> c("scenario")
  
  ### Read in files and bind data
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Reshaping ", type0, "scenario data...") |> message()
  data0 <- data0 |> 
    filter_all(any_vars(!(. |> is.na()))) |>
    # group_by_at(c(cols0)) |>
    group_map(
      reshapeScenarioData_byGroup,
      typeCol0 = typeCol0,
      argCol0  = argCol0,
      valCol0  = valCol0,
      yrCol0   = yrCol0,
      idCol0   = idCol0,
      method0  = method0,
      rule0    = rule0,
      silent   = silent,
      msg0     = msg0 + 1
    ) |> bind_rows()
  ### Return data
  return(data0)
} ### End if(doTemp0)  


### Reshape specific scenarios
reshapeScenarioData_byGroup <- function(
    .x, 
    .y,
    typeCol0 = c("inputName"),
    idCol0   = c("scenarioName"),
    argCol0  = c("inputArgVal"),
    valCol0  = c("valueCol"),
    yrCol0   = c("year"),
    method0  = "linear",
    rule0    = 1,
    silent   = TRUE,
    msg0     = 0
){
  ### File paths
  # .x |> glimpse(); 
  # .y |> glimpse()
  gCols0 <- .y |> names()
  type0  <- .y |> pull(all_of(typeCol0)) |> unique()
  name0  <- .y |> pull(all_of(idCol0  )) |> unique()
  yCol0  <- .y |> pull(all_of(valCol0 )) |> unique() 
  arg0   <- .y |> pull(all_of(argCol0 )) |> unique() 
  group0 <- .x |> names() |> get_matches(y=c(yrCol0, yCol0), matches=F)
  # .x |> names() |> print(); group0 |> print()
  # doTemp <- type0 |> str_detect("temp")
  sort0  <- group0 |> c(yrCol0)
  df0    <- .x |> 
    filter_all(any_vars(!(. |> is.na()))) |>
    arrange_at(c(sort0)) |> 
    group_by_at(c(group0))
  # df0 |> glimpse()
  ### Interpolate data
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Reshaping ", name0, "...") |> message()
  
  ### Conditionals
  doTemp <- type0 |> str_detect("temp")
  if(doTemp){
    df0 <- df0 |> group_map(
      format_tempData_byGroup,
      tempCol0  = yCol0,
      typeCol0  = "tempType", 
      yrCol0    = yrCol0,
      method0   = method0,
      rule0     = rule0
    ) |> bind_rows()
  } else{
    df0 <- df0 |> group_map(
      interpolate_byGroup,
      xCol0     = yrCol0,
      yCol0     = yCol0,
      method0   = method0,
      rule0     = rule0
    ) |> bind_rows()
  } ### End if(doTemp0)  
  
  ### Join data
  df0    <- .y |> cross_join(df0)
  
  ### Return data
  return(df0)
}


### Format grouped temperature scenarios
formatGroupedTempData <- function(
    df0,      ### Original GCAM data
    tempCol0  = "temp_C_global",
    typeCol0  = "inputArgVal",
    # tempType0 = "global",
    yrCol0    = "year",
    group0    = c("scenario", "model"),
    method0   = "linear",
    rule0     = 1
){
  ### Select columns
  ### Filter out any missing values
  sort0   <- group0 |> c(yrCol0)
  df0     <- df0 |>
    filter_all(any_vars(!(. |> is.na()))) |>
    arrange_at(c(sort0)) |>
    group_by_at(c(group0))
  
  ### Calculate temp_C_conus
  # df0 |> pull(scenario) |> unique() |> print()
  # groups0 <- df0 |> group_keys()
  df0     <- df0 |> group_map(
    format_tempData_byGroup,
    tempCol0  = tempCol0,
    typeCol0  = typeCol0,
    # tempType0 = tempType0,
    yrCol0    = yrCol0,
    method0   = method0,
    rule0     = rule0
  ) |> bind_rows()
  # df0 |> glimpse()
  
  ### Return
  return(df0)
}

### Format grouped temperature scenarios by group
### df0       = Data, filtered to a specific scenario
### scen0     = Scenario name
### scenCol0  = Scenario column
### tempCol0  = Temperature column
### tempType0 = Temperature type
format_tempData_byGroup <- function(
    .x,       ### Data, filtered to a scenario
    .y,       ### Group info
    tempCol0  = "temp_C_global",
    typeCol0  = "inputArgVal", ### Column to look for tempType
    # tempType0 = "global",
    yrCol0    = "year",
    method0   = "linear",
    rule0     = 1,
    globalStr = "global",
    conusStr  = "conus"
){
  # .x |> glimpse(); .y |> glimpse()
  ### Group info
  tempType0 <- .y   |> pull(all_of(typeCol0)) |> unique() |> tolower()
  yrs0      <- .x   |> pull(all_of(yrCol0  )) |> unique()
  minYr0    <- yrs0 |> min(na.rm=T)
  maxYr0    <- yrs0 |> max(na.rm=T)
  # yrs0 |> range() |> print()
  ### Sort data
  .x        <- .x |> 
    filter_all(any_vars(!(. |> is.na()))) |>
    arrange_at(c(yrCol0))
  
  ### Values and columns
  doGlobal  <- tempType0 |> str_detect(globalStr)
  tempType1 <- tempType0
  tempType2 <- case_when(doGlobal ~ conusStr, .default = globalStr)
  ### Interpolate values, convert temperatures, calculate slr_cm
  # .y |> glimpse()
  x0        <- .x |> pull(all_of(yrCol0  ))
  y0        <- .x |> pull(all_of(tempCol0))
  xVals0    <- minYr0:maxYr0
  old0      <- c("x", "y", "y2")
  new0      <- yrCol0 |> c("temp_C_" |> paste0(c(tempType1, tempType2)))
  df0       <- x0 |> 
    approx(y=y0, xout=xVals0, method=method0, rule=rule0) |> 
    as.data.frame() |> 
    as_tibble() |> 
    mutate(y2 = y |> convertTemps(from=tempType0)) |>
    rename_at(c(old0), ~new0)
  # df0 |> glimpse()
  ### Calculate SLR
  slrVals0  <- df0 |> pull(temp_C_global) |> temps2slr(years=xVals0)
  df0       <- df0 |> mutate(slr_cm = slrVals0)
  ### Add model data
  df0       <- .y |> cross_join(df0)
  ### Return
  return(df0)
}




## Reshape/Format SLR Height/Scenario Data ----------------
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

### Zero SLR values
zeroSlrValues <- function(
    df0, 
    # yCol0   = "value"
    yCol0   = "value",
    modCol0 = "model",
    unit0   = "cm",
    min0    = 30, 
    new0    = 0 
){
  ### Zero out column
  dfMin0 <- df0 |> 
    filter_at(c(modCol0), function(x, y=min0 |> paste0(unit0)){x %in% y}) |>
    mutate_at(c(modCol0), function(x, y=new0 |> paste0(unit0)){x %in% y}) |>
    mutate_at(c(yCol0  ), function(x, y=new0){y})
  ### Bind values
  df0    <- df0 |> 
    filter_at(c(modCol0), function(x, y=new0 |> paste0(unit0)){!(x %in% y)}) |>
    bind_rows(dfMin0)
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
    modLvls0  = c(0, 30,  50, 100, 150, 200, 250) |> paste0("cm"), ### Model levels to use for factoring...from co_slrCm
    modStr0   = c("Interpolation")
){
  ### Add scenario_id, sort, group 
  doSlr0 <- type0   |> str_detect("slr")
  doGcm0 <- type0   |> str_detect("gcm")
  group0 <- idCols0
  if(doSlr0) {
    group0 <- group0 |> c(xCol0) |> unique()
    sort0  <- group0 |> c(yCol0, modCol0) |> unique()
  } else if(doGcm0) {
    group0 <- group0 |> c(modCol0) |> unique()
    sort0  <- group0 |> c(xCol0) |> unique()
  } else{
    sort0  <- group0 |> c(xCol0) |> unique()
  } ### End if(doSlr0)
  # sort0  <- group0 |> c(xCol0, yCol0, modCol0)
  df0    <- df0    |> 
    # get_scenario_id(include=c("region", "postal"), col0="id") |>
    get_scenario_id(include=idCols0, col0="id") |>
    mutate    (id = id |> paste0("_", case_when(
      doSlr0 ~ modStr0, 
      .default=df0 |> pull(all_of(modCol0))
    )), .before=all_of(xCol0)) |>
    rename_at (c("id"), ~idCol0)
   
  ### Figure out which have no impacts
  cols0  <- idCol0 |> c("hasScenario")
  dfNA   <- df0    |> 
    filter_at(c(yCol0), function(x){!(x |> is.na())}) |>
    mutate(hasScenario = 1) |>
    select(all_of(cols0))
  ### Add scenario info
  df0    <- df0    |> 
    left_join(dfNA, by=idCol0) |>
    mutate(hasScenario = hasScenario |> replace_na(0), .after=all_of(idCol0)) |>
    filter(hasScenario == 1)
  rm(dfNA)
  ### Factor model
  if(doSlr) {
    df0   <- df0   |> mutate_at (c(modCol0), function(x, y=modLvls0){x |> factor(y)})
  } ### End if(doSlr)
  ### Arrange and group
  group0 <- group0 |> c(idCol0)
  df0    <- df0    |> 
    arrange_at (c(sort0 )) |>
    group_by_at(c(group0))
  ### Return
  return(df0)
}

### Reshape/Format SLR Impacts
format_slrImpacts <- function(
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
    slice_head(1)
  ### Return
  return(df0)
}


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
  group0   <- df0 |> group_cols()
  keys0    <- df0 |> group_keys()
  df0      <- df0 |> ungroup() |> select(all_of(select0)) |> rename_at(c(yCol0), ~yStr0)
  ids0     <- keys0 |> pull(all_of(idCol0))
  
  ### Join data
  old0     <- c(modCol0, yStr0)
  joinLo0  <- c(modCol0, xStr0) |> paste0(loStr0)
  joinHi0  <- c(modCol0, xStr0) |> paste0(hiStr0) |> c(idCol0, xRef0)
  df1      <- df1 |> 
    left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(loStr0)), by=joinLo0) |> 
    left_join(df0 |> rename_at(c(old0), ~old0 |> paste0(hiStr0)), by=joinHi0) |> 
    relocate(all_of(idCol0))
  rm(old0, joinLo0, joinHi0, df1)
  
  ### Join group keys, arrange
  # sort0  <- c(bounds0, group0, xCol0, yCol0, modCol0) |> unique()
  join0  <- c(idCol0)
  sort0  <- c(idCol0, xCol0, yCol0, modCol0) |> unique()
  df0    <- keys0 |> 
    left_join(df0, by=join0) |> 
    arrange_at (c(sort0)) |> 
    group_by_at(c(group0))
  
  ### Return
  return(df0)
}

### Function for dealing with SLR values above the maximum
fun_slrConfigExtremes <- function(
    df0 ### Outputs of get_slrExtValues()
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
    mutate_at(c(delCols), abs) |>
    ### Calculate slope & replace zeros
    mutate(slope     = case_when(xHi == xLo ~ 0, .default = yDelta / xDelta)) |> 
    ### Calculate intercept
    mutate(intercept = case_when(yHi > yLo ~ yHi, .default=yLo))
  # df0 |> glimpse()
  
  ### Drop columns
  drop0   <- c(modCol0, xStr0, yStr0) |> map(paste, bounds0) |> c(dCols)
  df0     <- df0 |> select(-any_of(drop0))
  
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
        mutate_at(c(valCol0), na_replace, 0)
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
  rCol0    <- "regional"
  dCol0    <- "dynamic"
  # idCol0   <- "group_id"
  typeCol0 <- "scalarType"
  nameCol0 <- "scalarName"
  scCols0  <- c(typeCol0, nameCol0)
  
  ### Select Info
  # df0 |> glimpse(); df1 |> glimpse()
  select0  <- scCols0 |> c(regCols0, yrCol0, valCol0)
  select1  <- scCols0 |> c(rCol0, dCol0)
  df0      <- df0 |> select(any_of(select0))
  df1      <- df1 |> select(any_of(idCols0))
  rm(join0, select0, select1)
  
  ### Add "none" as scalar types:
  ### - Get data frame with type and scalarName == "none"
  ### - Get national region info
  noneStr0 <- "none"
  noneVal0 <- 1
  dfNone0  <- df1 |> 
    select(all_of(scCols0)) |>
    filter_at(c(nameCol0), function(x, y=noneStr0){x %in% y}) |>
    distinct()
  ### - Get national region info
  select0  <- c(regCols0, valCol0)
  dfNat0   <- df0 |> 
    filter(postal %in% natStr0) |>
    mutate_at(c(valCol0), function(x, y=noneVal0){y}) |>
    select(all_of(select0)) |>
    distinct()
  ### - Cross-join info
  dfNone0  <- dfNone0 |> 
    cross_join(dfNat0) |> 
    cross_join(tibble(year = years0)) |>
    relocate(all_of(yrCol0), .before=all_of(valCol0))
  rm(dfNat0)
  
  ### Bind dfNone with df0 and join info
  df0      <- df0 |> bind_rows(dfNone)
  df0      <- df0 |> left_join(df1, by=scCols0)
  rm(dfNone, df1)
  
  ### Arrange and get groups
  ### Add column indicating method
  # idCols0  <- c(typeCol0, rCol0, nameCol0, regCols0)
  # group0   <- c(typeCol0, rCol0, idCols0)
  idCols0  <- c(nameCol0, regCols0)
  group0   <- idCols0
  sort0    <- group0  |> c(yrCol0)
  df0      <- df0 |>
    arrange_at(c(sort0)) |> 
    mutate(group_id = df0 |> select(all_of(idCols0)) |> apply(1, function(x){
      x |> as.vector() |> paste(collapse=sep0)
    }) |> unlist()) |>
    rename_at(c("group_id"), ~idCol0) |>
    group_by_at(c(group0, idCol0)) |> 
    mutate(method0 = case_when(dynamic == 0 ~ "constant", .default="linear"))
  
  ### Iterate over groups
  # select0  <- c(idCol0, yrCol0, valCol0)
  # dfKeys0  <- df0     |> group_keys()
  # groups0  <- dfKeys0 |> pull(all_of(idCol0))
  # df0       <- df0 |> ungroup() |> select(all_of(select0))
  ### Get unique names & types
  df0      <- df0 |> group_map( 
    interpolate_byGroup,
    xCol0     = yrCol0,
    yCol0     = valCol0,
    xOut0     = years0,
    method0   = .y |> pull(method0) |> unique(),
    rule0     = rule0
  ) |>
    # bind_rows(.id=idCol0) |>
    bind_rows() |>
    ungroup() |> 
    # select(all_of(select0)) |>
    group_by_at(c(nameCol0, idCol0))
  # rm(select0)
  # df0 |> glimpse()
  # 
  # df0      <- dfKeys |> 
  #   left_join(df0, by=join0) |>
  #   arrange_at(c(join0))
  
  ### Return
  return(df0)
}


## Format GCM Scaled Impacts ----------------
### Extrapolate Impact Function
### Function to extrapolate an impact function
extrapolate_gcmImpacts_byGroup <- function(
    df0,
    xCol        = "xIn", ### Which column to use for x (default = temp_C)
    yCol        = "yIn", ### Which column to use for y
    xMin        = 0,     ### Minimum value for x
    yMin        = 0,     ### Value of y at minimum value of x 
    # extrapolate = TRUE,
    extend_from = NULL,  ### Maximum value for model type to extend from, if not missing
    extend_to   = 30  ,   ### Extend last points for x
    unitScale   = 0.5 ,   ### Scale between values,
    extend_all  = FALSE  ### Whether to extend all models or just those that go to the max model value
){
  ### Filter to values greater than the reference
  ### Select columns and filter out NA values
  cols0     <- c(xCol, yCol)
  
  ### Standardize minimum value, then get unique values
  df0       <- df0 |>
    head(1) |> 
    mutate_at(c(xCol), function(x, y=xMin){y}) |> 
    mutate_at(c(yCol), function(x, y=yMin){y}) |> 
    bind_rows(df0)
  
  ### Extend from/to
  ### Get max value from which to extend values
  extendTo  <- extend_to |> as.character() |> as.numeric()
  tail0     <- df0 |> tail(2)
  
  ### Arrange by x values
  ### - Get max x value for which is there a non-missing y-value
  ### - Get maximum value
  df0       <- df0 |> arrange_at(vars("xIn"))
  xInMax0   <- tail0 |> pull(all_of(xIn)) |> last()
  # xOut_min  <- xIn_max + unitScale
  xOut_max  <- extendTo
  xOut      <- xOut_max
  
  ### If extrapolate:
  doExtrap  <- xInMax0 < extendTo | extend_all
  if(doExtrap){
    ### Get linear trend
    lm_ex    <- lm(yIn~xIn, data=tail0)
    slope0   <- lm_ex$coefficients[[2]]
    int0     <- lm_ex$coefficients[[1]]
    ### Create data
    dfExt    <- tibble() |> 
      tibble(xIn = extendTo) |>
      mutate(yIn = xIn * slope0 + int0)
    ### Extend values, then bind with earlier observations
    df0      <- df0 |> bind_rows(dfExt)
  } ### End if(doExtrap)
  
  ### Return
  return(df0)
}


### Function to extend GCM Impacts for interpolation
extrapolate_gcmImpacts <- function(
    df0,        ### Data frame with scaled impacts data
    groupCol    = "scenario_id"   , ### Which column to look for the scenario column name (default =  )
    xCol        = "modelUnitValue", ### Which column to use for x (default = temp_C)
    yCol        = "scaled_impacts", ### Which column to use for y
    xMin        = 0,    ### Minimum value for x
    yMin        = 0,    ### Value of y at minimum value of x 
    # extrapolate = FALSE, ### Whether to extrapolate by default
    # unitScale   = NULL,  ### Scale between values
    extend_from = NULL ,  ### Maximum value for model type to extend from, if not missing
    extend_to   = 30,  ### Extend last points for x
    extend_all  = FALSE,  ### Whether to extend all models or just those that go to the max model value
    method0     = "linear",
    rule0       = 1
){
  ### Groups ----------------
  ### - Create groups and get group keys
  ### - Filter out NA values
  ### - Filter to values greater than xMin
  ### - Rename columns
  old0      <- c(xCol, yCol)
  new0      <- c("x", "y") |> paste0("In")
  # df0 |> glimpse(); c(groupCol, xCol, yCol) |> print()
  df0      <- df0 |> 
    arrange_at (c(groupCol, xCol)) |> 
    group_by_at(c(groupCol)) |> 
    filter_all(all_vars(!(. |> is.na()))) |>
    filter_at(c(xCol0), function(x){x > xMin}) |>
    rename_at(c(old0), ~new0)
  
  ### Extrapolate Data ----------------
  ### Extrapolate values
  dfKeys0  <- df0     |> group_keys()
  groups0  <- dfKeys0 |> pull(all_of(groupCol))
  df0      <- df0     |> group_map(function(.x, .y){
    .x |> extrapolate_gcmImpacts_byGroup(
      xCol        = "xIn", 
      yCol        = "yIn", 
      xMin        = xMin, 
      yMin        = yMin, 
      extend_from = extend_from,
      extend_to   = extend_to,  
      extend_all  = extend_all  
    ) ### extrapolate_impFunction
  }) |> set_names(groups0) |> bind_rows(.id=groupCol)
  
  ### Join Data ----------------
  ### Join data and rename columns
  df0      <- dfKeys0 |> 
    left_join(df0, by=idCol0) |> 
    rename_at(c(new0), ~old0)
  
  ### Return ----------------
  gc()
  return(df0)
}

### Approximation function
### Create a piece-wise linear interpolation function using approxfun and defaults
###    rule = 1 (Returns NA for x-values outside range)
###    ties = mean (take the average of multiple values)
get_impactFunctions <- function(
    df0,        ### Tibble with extended/grouped GCM scaled impacts (output of extend_gcmImpacts)
    groupCol    = "scenario_id"   , ### Which column to look for the scenario column name (default =  )
    xCol        = "modelUnitValue", ### Which column to use for x (default = temp_C)
    yCol        = "scaled_impacts", ### Which column to use for y
    method0     = "linear",
    rule0       = 1
){
  ### Groups
  df0      <- df0 |> arrange_at (c(groupCol, xCol))
  groups0  <- df0 |> group_keys() |> pull(all_of(groupCol))
  
  ### Get Impact Functions
  list0    <- df0 |> group_map(function(.x, .y){
    inX  <- .x |> pull(all_of(xCol))
    inY  <- .y |> pull(all_of(yCol))
    funX <- approxfun(x=inX, y=inY, method=method0, rule=rule0)
    return(funX)
  }) |> set_names(groups0)
  
  ### Return the list (i.e., list0)
  gc()
  return(list0)
}


mapFormatScaledImpacts <- function(
    type0,
    dataList    = NULL, ### List of state data, output of reshapeModuleData
    controlData = NULL, 
    xCol0       = "year",
    yCol0       = "scaled_impacts",
    idCol0      = "scenario_id",
    idCols0     = c("sector", "variant", "impactType", "impactYear", "region", "postal"),
    modCol0     = "model",
    oldStr0     = "Data",
    newStr0     = "Impacts",
    msg0        = 0
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  ### Level of messaging (default is to message the user) and save behavior
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  
  msg0 |> get_msgPrefix() |> paste("Formatting ", type0 |> toupper(), " impacts...") |> message()
  
  #### Columns & Values ----------------
  
  ### Values
  type0  <- type0 |> tolower()
  doSlr0 <- type0 |> str_detect(slrStr0)
  doGcm0 <- type0 |> str_detect(gcmStr0)
  
  ### Object names
  name0  <- type0 |> paste0(oldStr0)
  name1  <- type0 |> paste0(newStr0)
  name2  <- type0 |> paste0(case_when(doSlr0 ~ "Extremes", .default = "Funs"))
  
  #### Data ----------------
  data0  <- dataList[[name0]]
  nRow0  <- data |> nrow() 
  ### If there is no data, return
  if(!nRow0) {return(NULL)}
  
  ### Format Data ----------------
  ### Otherwise, do model types
  list0  <- list()
  if(doSlr0) {
    ### Format impacts
    if(msgUser) msg1 |> get_msgPrefix() |> paste("...Formatting SLR impact values...") |> message()
    dfX1    <- data0 |> format_slrImpacts(
      modLvls0 = controlData[["co_slrCm"]] |> pull(all_of(modCol0)), 
      xCol0    = xCol0,
      yCol0    = yCol0,
      idCol0   = idCol0,
      modCol0  = modCol0,
      group0   = idCols0,
      modStr0  = "Interpolation"
    ) ### End format_slrImpacts
    list0[["slrImpacts"]] <- dfX1
    
    ### Format extremes
    if(msgUser) msg1 |> get_msgPrefix() |> paste("...Creating extreme SLR impact values...") |> message()
    dfX2    <- dfX1 |> get_slrExtValues(
      df1      = controlData[["slrCmExtremes"]],
      xCol0    = yrCol0,
      yCol0    = impactsCol0,
      idCol0   = idCol0,
      modCol0  = modCol0
    ) |> fun_slrConfigExtremes() 
    list0[["slrExtremes"]] <- dfX2
    rm(dfX1, dfX2)
  } else if(doGcm0) {
    ### Extrapolate values
    ### - Value to extend to
    extend0 <- controlData[["co_modelTypes"]] |> 
      filter(model_type %in% gcmStr0) |> 
      pull(driverMaxOutput) |> 
      unique()
    ### Extend values
    data0   <- data0 |> extrapolate_gcmImpacts(
      groupCol   = idCol0, 
      xCol       = unitCol0, 
      yCol       = impactsCol0, 
      extend_to  = extend0,
      extend_all = extend_all,
      method0    = "linear",
      rule0      = 1
    ) ### End extrapolate_gcmImpacts
    list0[["gcmImpacts"]] <- data0
    
    ### Get impact functions
    funs0   <- data0 |> get_impactFunctions(
      groupCol   = idCol0,
      xCol       = unitCol0,
      yCol       = impactsCol0,
      method0    = "linear",
      rule0      = 1
    ) ### get_impactFunctions
    # list0[["gcmImpFuncs" ]] <- funs0
    list0[["gcmFunctions"]] <- funs0
    rm(data0, funs0)
  } ### End if(doSlr0)
  ### Return
  msg0 |> get_msgPrefix() |> paste("...Finished formatting ", type0 |> toupper(), " impacts...") |> message()
  return(list0)
}

