### FrEDI Data Utilities
## Import functions from FrEDI namespace ----------------
get_matches        <- "get_matches"        |> utils::getFromNamespace("FrEDI")
interpolate_annual <- "interpolate_annual" |> utils::getFromNamespace("FrEDI")
get_scenario_id    <- "get_scenario_id"    |> utils::getFromNamespace("FrEDI")
convertTemps       <- "convertTemps"       |> utils::getFromNamespace("FrEDI")
temps2slr          <- "temps2slr"          |> utils::getFromNamespace("FrEDI")
interpolate_annual <- "interpolate_annual" |> utils::getFromNamespace("FrEDI")
match_scalarValues <- "match_scalarValues" |> utils::getFromNamespace("FrEDI")
update_popScalars  <- "update_popScalars"  |> utils::getFromNamespace("FrEDI")
get_scenario_id    <- "get_scenario_id"    |> utils::getFromNamespace("FrEDI")

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



## Format GCAM scenarios ----------------
format_gcamData <- function(
    df0, ### Original GCAM data
    yrCol0    = "year",
    scenCol0  = "scenario",
    modCol0   = "model",
    tempCol0  = "temp_C_global",
    tempType0 = "global",
    group0    = c("scenario", "model"),
    rule      = 1
){
  ### Select columns
  ### Filter out any missing values
  select0 <- c(yrCol0, scenCol0, modCol0, tempCol0)
  sort0   <- group0 |> c(yrCol0)
  df0     <- df0 |> 
    select(any_of(select0)) |>
    arrange_at(c(sort0)) |> 
    filter_all(any_vars(!(. |> is.na()))) |>
    group_by_at(c(group0))
  rm(select0)
  
  ### Calculate temp_C_conus
  # df0 |> pull(scenario) |> unique() |> print()
  # groups0 <- df0 |> group_keys()
  df0     <- group_map(format_gcamData_byScenario, 
    tempType0 = tempType0,
    minYr0    = minYr0,
    maxYr0    = maxYr0,
    scenCol0  = scenCol0,
    tempCol0  = tempCol0,
    yrCol0    = yrCol0,
    rule      = rule
  ) |> bind_rows()
  # df0 |> glimpse()
  
  ### Return
  return(df0)
}

###### Format GCAM scenario
### df0       = Data, filtered to a specific scenario
### scen0     = Scenario name
### scenCol0  = Scenario column
### tempCol0  = Temperature column
### tempType0 = Temperature type
format_gcamData_byScenario <- function(
    .x,       ### Data, filtered to a scenario
    .y,       ### Group info
    tempType0 = "global",
    minYr0    = "minYear0" |> get_frediDataObj("fredi_config", "rDataList"),
    maxYr0    = "npdYear0" |> get_frediDataObj("fredi_config", "rDataList"),
    scenCol0  = "scenario",
    tempCol0  = "temp_C_global",
    yrCol0    = "year",
    rule      = 1
){
  ### Import Functions to Namespace
  convertTemps       <- utils::getFromNamespace("convertTemps"      , "FrEDI")
  temps2slr          <- utils::getFromNamespace("temps2slr"         , "FrEDI")
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  ### Sort data
  df0       <- df0 |> arrange_at(c(yrCol))
  ### Values and columns
  globalStr <- "global"
  conusStr  <- "conus"
  doGlobal  <- tempType0 |> tolower() |> str_detect(globalStr)
  tempType1 <- tempType0 |> tolower()
  tempType2 <- case_when(doGlobal ~ conusStr, .default = globalStr)
  select0   <- c(yrCol0) |> c(tempColX |> paste0(c(conusStr0, globalStr0)))
  ### Interpolate values, convert temperatures, calculate slr_cm
  # scenario0 <- .y |> pull(all_of(scenCol0)) |> unique()
  .y |> glimpse()
  x0        <- .x |> pull(all_of(yrCol0  ))
  y0        <- .x |> pull(all_of(tempCol0))
  xVals0    <- minYr0:maxYr0
  yVals0    <- x0 |> approx(y=y0, xout=xVals0, method="linear", rule=rule)
  df0       <- tibble(xVal=xVals0, yVal1=yVals0) |> 
    mutate(yVal2 = yVal1 |> convertTemps(from=tempType0)) |>
    rename_at(c(xVal, yVal1, yVal2), ~c(yrCol0, tempCol1, tempCol2)) |>
    select(all_of(select0))
  ### Calculate SLR
  df0       <- df0 |> mutate(slr_cm = df0 |> pull(temp_C_global) |> temps2slr(years=xVals0))
  ### Add model data
  df0       <- df0 |> cross_join(.y)
  ### Return
  return(df0)
}

## Standardize Scaled Impacts ----------------
## Standardize scaled impacts
standardize_scaledImpacts <- function(
    df0,      ### Tibble of scaled impacts data, e.g.: stateData$slrImpData
    df1,      ### Tibble of sector group data, e.g.: frediData$co_sectorsInfo
    minYr0    = 2010,
    maxYr0    = 2300,
    # minYr0    = "minYear0" |> get_frediDataObj("fredi_config", "rDataList"),
    # maxYr0    = "npdYear0" |> get_frediDataObj("fredi_config", "rDataList"),
    mType0    = "gcm",
    xCol0     = c("modelUnitValue"),
    idCol0    = c("scenario_id"),
    mainCols0 = c("sector", "variant", "impacType", "impactYear"),
    regCols0  = c("region", "state", "postal"),
    mTypeCol0 = c("model_type"),
    modCol0   = c("model"),
    yrCol0    = c("year"),
    dropCols0 = c("fips")
){
  ### Values
  # df0 |> glimpse()
  # # cols0    <- c(mainCols0, mTypeCol0, modCol0, yrCol0)
  # cols0    <- c(mainCols0, regCols0, mTypeCol0, mmodCol0, yrCol0)
  # mType0   <- df0 |> pull(all_of(mTypeCol0)) |> unique()
  # # sectors0 <- df0 |> pull(sector) |> unique()
  # # sectors0 |> print()
  mType0   <- df0 |> pull(all_of(mTypeCol0)) |> unique()
  doSlr0   <- (mType0 |> tolower()) %in% "slr"
  
  # Add years to tibble of sector group info? Don't need to do this for GCM scaled impacts
  # years0   <- tibble() |> mutate(year = minYr0:maxYr0)
  # df1      <- df1 |> cross_join(years0) |> select(-any_of(dropCols0))
  # cols0    <- c(mainCols0, regCols0, mTypeCol0)
  cols0    <- c(mainCols0, regCols0)
  if(!doSlr0) {cols0 <- cols0 |> c(modCol0)}
  cols0    <- cols0 |> c(idCol0)
  df1      <- df1   |> 
    filter_at(c(mTypeCol0), function(x, y=mType0){x %in% y}) |> 
    select(all_of(select0)) |> 
    arrange_at(c(cols0))
  # df1 |> glimpse()
  
  ### Extend data
  df0      <- df0 |> extend_data(to0=maxYr0)
  
  ### Get scenario ID and select columns
  include0 <- cols0 |> get_matches(y=c(mainCols0, mTypeCol0, idCol0), matches=F)
  # sort0    <- cols0 |> c(xCol0)
  df0      <- df0   |> 
    get_scenario_id(include=include0, col0=idCol0) |> 
    arrange_at(c(cols0)) |>
    select(-any_of(include0))
  
  ### Join with data
  ### Add scenario value
  join0    <- c(idCol0)
  group0   <- cols0 |> get_matches(y=mTypeCol0)
  df0      <- df1   |> 
    left_join(df0, by=join0, relationship="many-to-many") |> 
    arrange_at(c(cols0))
    # arrange_at(c(cols0)) |> 
    # group_by_at(c(group0))
  rm(df1)
  
  ### Figure out if there are scenarios present
  # hasScen0 <- df0 |> filter_if(vars("scaled_impacts"), ~ !(. |> is.na())) |> pull(scenario_id) |> unique()
  dfHas0  <- df0 |> 
    filter_at(c(yCol0), function(x){!(x |> is.na())}) |> 
    select(all_of(idCol0)) |> 
    distinct() |>
    mutate(hasScenario = TRUE)
  
  ### Add column to data
  df0      <- df0 |> 
    left_join(dfHas0, by=idCol0) |>
    mutate_at(c("hasScenario"), replace_na, FALSE)
  
  ### Return
  return(df0)
}

## Extend Data ----------------
### Function to extend data
extend_data <- function(
    df0,          ### Data frame to extend
    # from0  = 2090, ### Year to extend from
    to0    = 2300, ### Year to extend to
    yrCol0 = "year"
){
  ### complete(year = seq(min(year), 2300, 1)) |> fill(-c("year"))
  #### Years
  from0  <- df0 |> pull(all_of(yrCol0)) |> max()
  years0 <- (from0 + 1):to0
  dfYrs0 <- tibble()
  dfYrs0[[yrCol0]] <- years0
  #### Data to extend
  dfMax  <- df0 |> 
    filter(year == from0) |> select(-any_of(yrCol0)) |> 
    cross_join(dfYrs0)
  ### Bind back in
  df0    <- df0 |> 
    filter(year <= from0) |> 
    bind_rows(dfMax)
  ### Return
  return(df0)
}

## Reshape/Format SLR Data ----------------
### Function to perform initial formatting/reshaping of slr_cm
### Function to get main SLR heights
reshape_slrCm <- function(
    df0,      ### slr_cm
    modLvls0, ### Model levels to use for factoring...from co_slrCm
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
  
  ### Columns
  sort0 <- c(modCol0, yrCol0, xCol0)
  ### Factor models
  ### Rename columns and organize
  df0   <- df0 |>
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
  xStr0   <- "x"
  ### Arrange values ascending, get lowest model for each year-xRef combination
  ### Note: this is also done in get_slrCmMain, but keeping this here in case we want to change the order
  group0 <- c(yrCol0, xCol0)
  sort0  <- group0 |> c(modCol0)
  df0    <- df0 |> 
    arrange_at (c(sort0 )) |>
    group_by_at(c(group0)) |>
    slice_head(n = 1)
  rm(group)
  ### Then, get the two highest xRefs:
  ### - Sort descending, group by year, get highest and second highest xRef values
  ### - Then, join with other data
  df0    <- df0 |> 
    arrange_at (c(sort0), desc) |>
    group_by_at(c(yrCol0)) |>
    summarize(Hi = xCol0 |> max(), Lo = xCol0 |> nth(2, order_by=xCol0), .groups="drop") |>
    pivot_longer(cols=c(loStr0, hiStr0), names_to=boundCol0, values_to=xCol0) |> 
    left_join(df0, by=sort0) |>
    arrange_at(c(sort0))
  ### Join data with model info
  # df0        <- vals0 |> left_join(df0, by=c(yrCol0, xCol0))
  # rm(vals0)
  ### Flatten data
  old0   <- c(modCol0, xCol0)
  new0   <- c(modCol0, xStr0)
  df0    <- bounds0 |> (function(boundX, oldX=old0, newX=new0){
    df0 |> 
      filter_at(c(boundCol0), function(x, y=boundX){x %in% y}) |> 
      rename_at(c(oldX), ~newX |> paste0(boundX)) |> 
      arrange_at(c(yrCol0))
  }) |> reduce(left_join, by=yrCol0)
  ### Calculate xRef
  df0    <- df0 |> mutate(xRef = case_when(xHi > xLo ~ xHi, .default=xLo))
  ### Return
  return(df0)
}

### Reshape/Format SLR Impacts
format_slrImpacts <- function(
    df0,      ### Tibble of SLR max heights (outputs of get_slrMaxHeights)
    modLvls0, ### Model levels to use for factoring...from co_slrCm
    yCol0     = c("scaled_impacts"),
    group0    = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal"),
    yrCol0    = c("year"),
    modCol0   = c("model"),
    idCol0    = c("scenario_id"),
    modStr0   = c("Interpolation")
){
  ### Add 
  sort0  <- group0 |> c(yrCol0, yCol0, modCol0)
  group0 <- group0 |> c(idCol0)
  df0    <- df0    |> 
    get_scenario_id(include=c("region", "state", "postal"), col0=idCol0) |>
    mutate_at(c(scenario_id), paste0, "_" |> paste0(modStr0)) |>
    mutate_at (c(modCol0), function(x, y=modLvls0){x |> factor(y)}) |>
    arrange_at(c(sort0)) |>
    group_by_at(c(group0))
  ### Return
  return(df0)
}

### Function to iterate over in fun_slrConfigExtremes
# df0    <- bounds0 |> map(function(boundX){
#   df0 |> 
#     filter_at(c(boundCol0), function(x){x %in% boundX}) |>
#     left_join(df1, by=join0) |> 
#     relocate(all_of(names1)) |> 
#     rename_at(c(old0), ~new0 |> paste0(boundX)) |>
#     select(-any_of(boundCol0))
# }) |> reduce(left_join, by=join0)
# df0    <- df0 |> 
#   arrange_at(c(sort0), desc) |>
#   group_by_at(c(group0)) |>
#   mutate(id = cur_group_id()) |>
#   # group_by_at(c("id"), .add=T) |>
#   arrange_at(c(group0)) |>
#   slice_head(1)
# rm(names1, join0, old0, new0, df1)

get_slrExtValues <- function(
    df0,      ### Tibble of SLR max heights (outputs of get_slrMaxHeights)
    df1,      ### Tibble of SLR impacts
    yCol0     = c("scaled_impacts"),
    yrCol0    = c("year"),
    modCol0   = c("model"),
    idCol0    = c("scenario_id", "hasScenario")
    # idCol0    = c("scenario_id")
    # xCol0     = c("xRef"),
    # idCol0    = c("scenario_id"),
    # group0    = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal"),
    # boundCol0 = c("bound")
){
  ### Strings
  loStr0   <- "Lo"
  hiStr0   <- "Hi"
  bounds0  <- c(loStr0, hiStr0)
  xStr0    <- "x"
  yStr0    <- "y"
  xRef0    <- "xRef"
  
  ### Select data
  select0  <- c(idCol0, yrCol0, modCol0, yCol0)
  group0   <- df1 |> group_cols()
  dfKeys   <- df1 |> group_keys()
  df1      <- df1 |> ungroup() |> select(all_of(select0)) |> rename_at(c(yCol0), ~yStr0)
  
  ### Join data
  # names1   <- df1 |> names()C
  # join0   <- names1 |> get_matches(y=df0 |> names())
  # new0    <- c(modCol0, xCol0, yCol0)
  # df0    <- df0 |> eft_join(df1, by=join0) |> relocate(all_of(names1))
  old0     <- c(modCol0, yStr0)
  joinLo0  <- c(modCol0, xStr0) |> paste0(loStr0)
  joinHi0  <- c(modCol0, xStr0) |> paste0(hiStr0) |> c(idCol0, xRef0)
  df0      <- df0 |> 
    left_join(df1 |> rename_at(c(old0), ~old0 |> paste0(loStr0)), by=joinLo0) |> 
    left_join(df1 |> rename_at(c(old0), ~old0 |> paste0(hiStr0)), by=joinHi0) |> 
    relocate(all_of(idCol0))
  rm(ld0, joinLo0, joinHi0, df1)
  
  ### Join group keys, arrange
  # sort0  <- c(bounds0, group0, yrCol0, yCol0, modCol0) |> unique()
  join0  <- c(idCol0)
  sort0  <- c(idCol0, yrCol0, yCol0, modCol0) |> unique()
  df0    <- dfKeys |> left_join(df0, by=join0) |> 
    arrange_at(c(sort0)) |> 
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
    mutate(slope = case_when(xHi == xLo ~ 0, .default = yDelta / xDelta)) |> 
    ### Calculate intercept
    mutate(intercept = case_when(yHi > yLo ~ yHi, .default=yLo))
  # df0 |> glimpse()
  
  ### Drop columns
  drop0   <- c(modCol0, xStr0, yStr0) |> map(paste, bounds0) |> c(dCols)
  df0     <- df0 |> select(-any_of(drop0))
  
  ### Return
  return(df0)
}


## Reshape/Format Scalar Data ----------------
### fun_formatScalars
### Function to format scalars in createSystemData
fun_formatScalars <- function(
    df0, ### rDataList$scalarDataframe
    df1, ### rDataList$co_scalarInfo,
    # yrs0, ### rDataList$list_years
    years0   = 2010:2300,
    yrCol0   = c("year"),
    valCol0  = c("value"),
    # regCols0 = c("region", "state", "postal"),
    regCols0 = c("region", "postal"),
    scCols0  = c("scalarType", "scalarName"),
    natStr0  = c("US"),
    rule0    = 2
){
  ### Columns
  rCol0    <- "regional"
  dCol0    <- "dynamic"
  idCol0   <- "group_id"
  
  ### Select Info
  # df0 |> glimpse(); df1 |> glimpse()
  select0  <- c(scCols0, regCols0, yrCol0, valCol0)
  select1  <- c(scCols0, rCol0, dCol0)
  df0      <- df0 |> select(all_of(select0))
  df1      <- df1 |> select(all_of(idCols0))
  rm(join0)
  
  ### Add "none" as scalar types:
  ### - Get data frame with type and scalarName == "none"
  ### - Get national region info
  dfTypes0 <- df1 |> 
    select(all_of(scCols0)) |>
    filter(scalarName %in% "none") |>
    distinct()
  ### - Get national region info
  dfNat0   <- df0 |> 
    select(all_of(regCols0)) |>
    filter(postal %in% natStr0) |>
    distinct()
  ### - Cross-join info
  dfNone0  <- dfTypes0 |> 
    cross_join(dfNat0) |> 
    mutate(value = 1) |> 
    cross_join(tibble(year = years0)) |>
    relocate(all_of(yrCol0), .before=all_of(valCol0))
  rm(dfTypes0, dfNat0)
  
  ### Bind dfNone with df0 and join info
  df0      <- df0 |> bind_rows(dfNone)
  df0      <- df0 |> left_join(df1, by=scCols0)
  rm(dfNone, df1)
  
  ### Arrange and get groups
  ### Add column indicating method
  group0   <- c(dCol0, rCol0, scCols0, regCols0)
  df0      <- df0 |>
    arrange_at(c(group0, yrCol0)) |> 
    mutate(group_id = df0 |> select(all_of(group0)) |> apply(1, function(x){
      x |> as.vector() |> paste(collapse=sep0)
    }) |> unlist()) |>
    group_by_at(c(group0, idCol0)) |> 
    mutate(method0 = case_when(dynamic == 0 ~ "constant", .default="linear"))
  
  ### Iterate over groups
  select0  <- c(idCol0, yrCol0, valCol0)
  dfKeys0  <- df0 |> group_keys()
  groups0  <- dfKeys0 |> pull(all_of(idCol0))
  # df0       <- df0 |> ungroup() |> select(all_of(select0))
  ### Get unique names & types
  # group0  <- select0 |> c("byState")
  # group0  <- c(scCols0, rCol0, dCol0, regCols0)
  df0      <- df0 |>
    group_map(function(.x, .y){
      typeX  <- .x |> pull(method0)
      inX    <- .x |> pull(all_of(yrCol0 ))
      inY    <- .x |> pull(all_of(valCol0))
      valsX  <- approx(x=inX, y=inY, xout=years0, rule=rule0, method=typeX) 
      dfX    <- tibble() |>
        mutate(year  = years0) |>
        mutate(value = valsX)
      return(dfX)
    }) |> set_names(groups0) |>
    bind_rows(.id=idCol0) |>
    ungroup() |>
    select(all_of(select0))
  rm(select0)
  # df0 |> glimpse()
  
  ### Add "none" as scalar types
  # select0  <- 
  dfNone   <- df0 |> 
    
  
  ### Join with group keys and drop ID col
  join0    <- c(idCol0, yrCol0)
  df0      <- dfKeys |> 
    left_join(df0, by=join0) |>
    arrange_at(c(join0))
  
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
    extend_to   = NULL,  ### Extend last points for x
    unitScale   = 0.5,   ### Scale between values,
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
    extend_to   = NULL ,  ### Extend last points for x
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




