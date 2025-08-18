####### FrEDI Data Utilities 
###### Function to get years from data
### Get a sequence from a set of years
get_years_fromData <- function(years0, by=1){
  min0 <- years0 |> min(na.rm=T)
  max0 <- years0 |> max(na.rm=T)
  yrs0 <- min0 |> seq(max0, by=by)
  return(yrs0)
}


###### Interpolate GDP scenario
### df0 is a data frame with columns c("year", "gdp_usd")
interpolate_gdp <- function(df0){
  ### Import Functions to Namespace
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  ### Select columns
  select0 <- c("year", "gdp_usd")
  df0     <- df0 |> select(all_of(select0))
  ### Get years
  years0  <- df0 |> pull(year) |> get_years_fromData()
  ### Add region="NationalTotal"
  df0     <- df0 |> mutate(region = "NationalTotal")
  ### Interpolate annual
  sum0    <- c("gdp_usd")
  df0     <- df0 |> interpolate_annual(years=years0, column=sum0, rule=1, byState=F)
  ### Drop region
  drop0   <- c("region")
  df0     <- df0 |> select(-any_of(drop0))
  ### Return
  return(df0)
}


###### Interpolate population
### df0 is a data frame with columns c("region", "state", "postal", "year", "pop")
interpolate_pop <- function(df0){
  ### Import Functions to Namespace
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  ### Select column
  select0 <- c("region", "state", "postal", "year", "pop")
  df0     <- df0 |> select(all_of(select0))
  ### Get years
  years0  <- df0 |> pull(year) |> get_years_fromData()
  ### Interpolate annual
  sum0    <- c("pop")
  df0     <- df0 |> interpolate_annual(years=years0, column=sum0, rule=1, byState=T)
  ### Return
  return(df0)
}

### Calculate national population
### df0 is a data frame with columns c("region", "state", "postal", "year", "pop")
calc_nationalPop <- function(df0){
  ### Select columns
  select0  <- c("region", "state", "postal", "year", "pop")
  df0      <- df0 |> select(all_of(select0))
  ### Summarize population over states and regions
  group0   <- c("year")
  sum0     <- c("pop")
  df0      <- df0 |>
    group_by_at (vars(group0)) |>
    summarize_at(vars(sum0), sum, na.rm=T) |> ungroup()
  ### Rename values
  renameAt <- sum0
  renameTo <- "national_pop"
  df0      <- df0 |> rename_at(c(renameAt), ~c(renameTo))
  ### Return
  return(df0)
}


### Create national scenario from national population and GDP information
### gdp0 is a data frame with columns c("year", "gdp_usd")
### pop0 is a data frame with columns c("region", "state", "postal", "year", "pop")
create_nationalScenario <- function(gdp0, pop0, natPop0=NULL){
  ### If national population is NULL, calculate national population
  nullNpop <- natPop0 |> is.null()
  if(nullNpop) natPop0 <- pop0 |> calc_nationalPop()
  ### Select columns
  colsG0   <- c("year", "gdp_usd")
  colsP0   <- c("region", "state", "postal", "year", "pop")
  colsN0   <- c("year", "national_pop")
  gdp0     <- gdp0 |> select(all_of(colsG0))
  pop0     <- pop0 |> select(all_of(colsP0))
  ### Join GDP and national population by year
  join0    <- c("year")
  nat0     <- gdp0 |> left_join(natPop0, by=c(join0))
  ### Calculate GDP per capita
  nat0     <- nat0 |> mutate(gdp_percap = gdp_usd / national_pop)
  ### Join nat0 with state population by year
  nat0     <- nat0 |> left_join(pop0, by=c(join0), relationship="many-to-many")
  ### Arrange by colsP0
  arrange0 <- colsP0 |> (function(x, y="year"){x[!(x %in% y)]})()
  ### Return
  return(nat0)
}


###### Format GCAM scenarios
format_gcamData <- function(
    df0, ### Original GCAM data
    conn
){
  ### Select values
  select0 <- c("year", "temp_C_global", "scenario", "model")
  df0     <- df0 |> select(all_of(select0))
  rm(select0)
  
  ### Calculate
  # df0 |> glimpse()
  df0     <- df0 |> filter_all(any_vars(!(. |> is.na())))
  # df0 |> glimpse()
  
  ### Calculate temp_C_conus
  scen0   <- df0 |> pull(scenario) |> unique()
  # scen0 |> print()
  list0   <- list(scen_i=scen0, df0_i=scen0 |> map(function(scen_i){df0 |> filter(scenario == scen_i)}))
  df0     <- list0 |> pmap(function(scen_i, df0_i){
    df0_i |> format_gcamData_byScenario(conn = conn)
  }) |> bind_rows()
  rm(list0)
  # df0 |> glimpse()
  
  ### Select values
  select0 <- c("year", "temp_C_global", "temp_C_conus", "slr_cm", "scenario", "model")
  df0     <- df0 |> select(all_of(select0))
  rm(select0)
  
  ### Return
  return(df0)
}

###### Format GCAM scenario
format_gcamData_byScenario <- function(
    df0, ### Original GCAM data, filtered to a specific scenario
    conn
){
  ###### Import Functions to Namespace
  convertTemps       <- utils::getFromNamespace("convertTemps"      , "FrEDI")
  temps2slr          <- utils::getFromNamespace("temps2slr"         , "FrEDI")
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  
  ### Years
  years0 <- df0 |> pull(year) |> get_years_fromData()
  
  ### Interpolate temperatures by year
  df0    <- df0 |> mutate(region = "NationalTotal") 
  df0    <- df0 |> interpolate_annual(years=years0, column="temp_C_global", rule=1)
  df0    <- df0 |> select(-c("region"))
  # df0_i |> glimpse()

  ### Calculate CONUS temperatures and move before global temps
  df0    <- df0 |> mutate(temp_C_conus = temp_C_global |> convertTemps(from="global"))
  df0    <- df0 |> relocate(c("temp_C_conus"), .before="temp_C_global")
  
  ### Then, calculate SLR heights
  df1    <- FrEDI::temps2slr(temps=df0 |> pull(temp_C_global), years=df0 |> pull(year),conn = conn)
  df0    <- df0 |> left_join(df1, by=c("year"))
  # df0 |> glimpse()

  ### Select values  
  df0   <- df0 |> select(c("year", "temp_C_conus", "temp_C_global", "slr_cm", "scenario", "model"))
  
  ### Return
  return(df0)
}


standardize_scaledImpacts <- function(
    df0, ### Tibble of scaled impacts data, e.g.: stateData$slrImpData
    df1, ### Tibble of sector group data, e.g.: frediData$co_sectorsInfo
    xCol = "year"
){
  ### Load functions from FrEDI
  get_scenario_id <- utils::getFromNamespace("get_scenario_id", "FrEDI")
  
  ### Values
  # df0 |> glimpse()
  sectors0 <- df0 |> pull(sector) |> unique()
  cols0    <- c("sector", "variant", "impactType", "impactYear", "modelType", "model") |> c(xCol)
  
  ### Filter to options
  select0  <- c("modelType") |> c(xCol)
  df2      <- df0 |> select(all_of(select0)) |> distinct()
  rm(select0)
  
  ### Drop fips
  select0  <- c("fips")
  # sectors0 |> print(); df1 |> pull(sector) |> unique() |> print()
  df1      <- df1 |> filter(sector %in% sectors0)
  df1      <- df1 |> select(-any_of(select0))
  names1   <- df1 |> names()
  rm(select0)
  # df1 |> glimpse()
  
  ### Get values to join
  join0    <- c("modelType")
  df2      <- df1 |> left_join(df2, by=c(join0), relationship="many-to-many")
  rm(join0)
  # df2 |> glimpse()
  # return(df2)
  
  ### Join with data
  # join0    <- cols0 |> c(names1)
  join0    <- c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "modelType", "model") |> c(xCol)
  df0      <- df2 |> left_join(df0, by=join0, relationship="many-to-many")
  df0      <- df0 |> arrange_at(c(join0))
  rm(join0)
  
  ### Add scenario value
  include0 <- c("region") |> c("state", "postal") |> c("model")
  df0      <- df0 |> get_scenario_id(include=include0) |> ungroup()
  
  ### Figure out if there are scenarios present
  # hasScen0 <- df0 |> filter_if(vars("scaled_impacts"), ~ !(. |> is.na())) |> pull(scenario_id) |> unique()
  hasScen0 <- df0 |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
  df0      <- df0 |> mutate(hasScenario = (scenario_id %in% hasScen0) |> as.numeric())
  
  ### Return
  return(df0)
}


####### extend_data 
extend_data <- function(
    df0,          ### Data frame to extend
    from0 = 2090, ### Year to extend from
    to0   = 2300, ### Year to extend to
    by0   = 1     ###
){
  ### complete(year = seq(min(year), 2300, 1)) |> fill(-c("year"))
  #### Years
  years0 <- tibble(year = seq(from0 + 1, to0, by=by0))
  #### Data to extend
  df1    <- df0 |> filter(year==from0) |> select(-c("year"))
  df1    <- df1 |> cross_join(years0)
  ### Bind back in
  df0    <- df0 |> filter(year < from0)
  df0    <- df0 |> rbind(df1)
  ### Return
  return(df0)
}

####### extend_slr 
### Function to extend SLR scenarios in createSystemData
extend_slr   <- function(
    x,
    newMax_x  = 2300,
    arrange_x = c("model", "year")
){
  ### Values
  maxYear_x <- x$year |> max()
  newYears  <- (maxYear_x + 1):newMax_x
  ### Format Dataframes
  x_nu      <- tibble(year = newYears) |> mutate(joinCol = 1)
  x_up      <- x |> filter(year == maxYear_x) |> mutate(joinCol = 1) |> select(-c("year"))
  x_lo      <- x |> filter(year <= maxYear_x)
  rm(x)
  ### Join data
  join0     <- c("joinCol")
  x_up      <- x_up |> left_join(x_nu, by=c(join0)) |> select(-all_of(join0))
  x         <- x_lo |> rbind(x_up)
  rm(x_nu, x_up, x_lo)
  # ### Arrange and standardize model type
  # x  <- x |> arrange_at(c(arrange_x)) |> mutate(modelType = "slr")
  ### Return
  return(x)
} ### End function


###### SLR Extremes
### Function to iterate over in fun_slrConfigExtremes
get_slrMaxValues <- function(
    year_i,
    data_x, ### Tibble of SLR heights
    data_y  ### Tibble of SLR impacts
){
  ### Columns
  # data_x |> glimpse(); data_y |> glimpse()
  yearCol0   <- c("year")
  modCols0   <- c("model", "modelType")
  arrange0   <- c("driverValue")  |> c(modCols0) |> c(yearCol0)
  impCols0   <- data_y |> names() |> (function(x){x[!(x %in% arrange0)]})()
  impCols0   <- impCols0 |> c(yearCol0)
  ### Other values
  bounds0    <- c("lower", "upper")
  
  ### Filter data & drop columns
  drop0      <- modCols0
  dfx_i      <- data_x |> filter(year==year_i) |> select(-any_of(drop0))
  dfy_i      <- data_y |> filter(year==year_i) |> select(-any_of(drop0))
  rm(drop0)
  
  ### Driver values:
  ### - Get driver values and then unique driver values
  ### - Figure out which the last values belong to
  vals_i    <- dfx_i |> pull(driverValue) |> unique() |> sort(decreasing=TRUE)
  ### Add value
  addVal_i  <- vals_i |> length() == 1
  if(addVal_i){vals_i <- vals_i |> rep(2)}
  ref_i     <- tibble(driverValue = vals_i [1:2])
  ref_i     <- ref_i |> mutate(valueType = bounds0[2:1]) ### End tibble
  ref_i     <- ref_i |> mutate(year      = year_i)
  
  ### Filter dfx_i to driver values %in% first_i and add an order
  join0     <- c(yearCol0) |> c("driverValue")
  dfx_i     <- dfx_i |> left_join(ref_i, by=c(join0))
  dfx_i     <- dfx_i |> filter(!(valueType |> is.na()))
  
  ### Join with driver values
  join0     <- c(yearCol0) |> c("model_cm")
  dfy_i     <- dfy_i |> left_join(dfx_i, by=c(join0))
  dfy_i     <- dfy_i |> filter(!(valueType |> is.na()))
  rm(join0)
  
  ### Get maximum impacts
  sum0      <- c("scaled_impacts")
  sum1      <- c("model_cm")
  group0    <- impCols0 |> c("driverValue", "valueType") |> unique()
  group0    <- group0 |> (function(x){x[!(x %in% c(sum0, sum1))]})()
  join0     <- group0 |> c(sum0)
  df_i      <- dfy_i |>
    group_by_at(c(group0)) |>
    summarize_at(c(sum0), max, na.rm=T) |> ungroup()
  
  ### Join max values
  df_i       <- df_i |> left_join(dfy_i, by=c(join0))
  
  ### Filter to maximum impacts and get associated model
  df_i       <- df_i |>
    group_by_at(c(join0)) |>
    summarize_at(c(sum1), max, na.rm=T) |> ungroup()
  
  ### Return
  return(df_i)
}

### Function for dealing with SLR values above the maximum
fun_slrConfigExtremes <- function(
    slr_x, ### rDataList$slr_cm
    imp_x  ### rDataList$slrImpacts
){
  # slr_x |> glimpse(); imp_x |> glimpse()
  ### Columns
  yearCol0 <- c("year")
  modCols0 <- c("model", "modelType")
  # exCols0  <- modCols0 |> c("scaled_impacts", "model_cm") |> c(yearCol0)
  exCols0  <- modCols0 |> c("scaled_impacts", "model_cm") |> c(yearCol0)
  impCols0 <- imp_x    |> names() |> (function(x){x[!(x %in% exCols0)]})()
  impCols0 <- impCols0 |> c(yearCol0)
  # slr_x |> glimpse(); imp_x |> glimpse()
  ### Prepare data
  ### SLR Heights: slr_df; SLR Impacts: imp_df
  slr_df   <- slr_x  |> mutate(model_cm = model |> fun_slrModel2Height(include="values"))
  imp_df   <- imp_x  |> mutate(model_cm = model |> fun_slrModel2Height(include="values"))
  rm(slr_x, imp_x)
  # slr_df |> head() |> glimpse(); imp_df |> head() |> glimpse()
  
  ### Get upper and lower for each year
  slrYears <- slr_df   |> pull(year) |> unique() |> sort()
  slr_extr <- slrYears |> map(~.x |> get_slrMaxValues(data_x=slr_df, data_y=imp_df))
  slr_extr <- slr_extr |> bind_rows()
  
  ###### Select and arrange ######
  # slr_extr |> names() |> print()
  arrange0 <- impCols0 |> c("valueType", "driverValue", "scaled_impacts")
  select0  <- arrange0 |> c("model_cm")
  # slr_extr |> glimpse()
  slr_extr <- slr_extr |> select(all_of(select0))
  slr_extr <- slr_extr |> arrange_at(c(arrange0))
  rm(select0)
  
  ###### Spread Lower & Upper ######
  ### Spread lower and upper values and join them together
  ### - Separate into lower and upper values and join data
  drop0    <- c("valueType")
  slr_lo   <- slr_extr  |> filter(valueType=="lower") |> select(-all_of(drop0))
  slr_up   <- slr_extr  |> filter(valueType!="lower") |> select(-all_of(drop0))
  rm(drop0)
  
  ### - Rename columns
  renameAt <- c("driverValue", "scaled_impacts", "model_cm")
  suffix0  <- c("1", "2")
  slr_lo   <- slr_lo |> rename_at(c(renameAt), ~renameAt |> paste0("1"))
  slr_up   <- slr_up |> rename_at(c(renameAt), ~renameAt |> paste0("2"))
  
  ### - Join upper and lower values
  join0    <- impCols0
  slr_extr <- slr_up |> left_join(slr_lo, by = c(join0))
  rm(slr_up, slr_lo); rm(renameAt)
  # slr_extr |> glimpse()
  
  ###### Calculate differences ######
  slr_extr <- slr_extr |> mutate(delta_impacts     = scaled_impacts2 - scaled_impacts1)
  slr_extr <- slr_extr |> mutate(delta_driverValue = driverValue2    - driverValue1)
  ### Calculate absolute values
  slr_extr <- slr_extr |> mutate(delta_impacts     = delta_impacts     |> abs())
  slr_extr <- slr_extr |> mutate(delta_driverValue = delta_driverValue |> abs())
  ### Calculate intercept
  slr_extr <- slr_extr |> mutate(driverValue_ref   = (driverValue2 > driverValue1) |> ifelse(driverValue2, driverValue1))
  slr_extr <- slr_extr |> mutate(impacts_intercept = (scaled_impacts2 > scaled_impacts1) |> ifelse(scaled_impacts2, scaled_impacts1))
  ### Calculate slope & replace zeros
  slr_extr <- slr_extr |> mutate(impacts_slope = delta_impacts / delta_driverValue)
  slr_extr <- slr_extr |> mutate(impacts_slope = (delta_driverValue == 0) |> ifelse(0, delta_driverValue))
  
  ###### Drop some columns ######
  # slr_extr |> glimpse()
  drop0    <- c("delta_impacts", "delta_driverValue")
  drop0    <- "scaled_impacts" |> paste0(suffix0) |> c(drop0)
  drop0    <- "driverValue"    |> paste0(suffix0) |> c(drop0)
  drop0    <- "model_cm"       |> paste0(suffix0) |> c(drop0)
  # drop0    <- c("driverValue", "valueType") |> c(drop0)
  slr_extr <- slr_extr |> select(-any_of(drop0))
  # ### Arrange values
  # slr_extr |> glimpse(); drop0 |> print(); arrange0 |> print()
  # arrange0 <- arrange0 |> (function(x){x[!(x %in% drop0)]})()
  # slr_extr <- slr_extr |> arrange_at(c(arrange0))
  
  ###### Return ######
  return(slr_extr)
}


###### fun_formatScalars
### Function to format scalars in createSystemData
fun_formatScalars <- function(
    data_x,  ### rDataList$scalarDataframe
    info_x,  ### rDataList$co_scalarInfo
    years_x  ### rDataList$list_years
){
  ### Load functions from FrEDI
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  
  ### State columns
  stateCols0 <- c("state", "postal")
  # data_x |> glimpse(); info_x |> glimpse()
  ### Join info
  join0    <- c("scalarName", "scalarType")
  select0  <- join0  |> c("national_or_regional", "constant_or_dynamic")
  select1  <- join0  |> c("region") |> c(stateCols0) |> c("year", "value")
  info_x   <- info_x |> select(all_of(select0))
  data_x   <- data_x |> select(all_of(select1))
  data_x   <- data_x |> left_join(info_x, by=c(join0))
  
  ### Get unique names & types
  # group0  <- select0 |> c("byState")
  group0  <- select0
  dfNames <- data_x  |>
    group_by_at(c(group0)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  # data_x |> glimpse(); dfNames |> glimpse()
  rm(group0)
  
  ### Iteration list
  listPm  <- list()
  listPm[["name_i"]] <- dfNames |> pull(scalarName)
  listPm[["type_i"]] <- dfNames |> pull(scalarType)
  listPm[["con_i" ]] <- dfNames |> pull(constant_or_dynamic)
  listPm[["nat_i" ]] <- dfNames |> pull(national_or_regional)
  # listPm |> print()
  
  ### Iterate over list and interpolate annual values
  # data_x |> glimpse()
  data_x   <- listPm |> pmap(function(name_i, type_i, con_i, nat_i){
    ### Filter to appropriate name and type
    data_i    <- data_x |> filter(scalarName==name_i)
    data_i    <- data_i |> filter(scalarType==type_i)
    data_i    <- data_i |> filter(national_or_regional==nat_i)
    ### Info about method
    method_i  <- (con_i=="constant") |> ifelse(con_i, "linear")
    # if(byState){states_i <- data_i[["state"]] |> unique()} else{states_i <- "N/A"}
    # # byState_i <- !("N/A" %in% states_i) & byState
    # byState_i <- !("N/A" %in% states_i)
    # name_i |> print(); data_i |> glimpse()
    ### Interpolate data
    data_i    <- data_i |> interpolate_annual(
      years   = years_x,
      column  = "value",
      rule    = 2:2,
      method  = method_i,
      byState = TRUE
    ) ### End interpolate_annual
    ### Return
    return(data_i)
  }) |> bind_rows()
  
  # ### Bind rows
  # data_x |> glimpse()
  ### Arrange
  arrange0 <- select1 |> c("national_or_regional") |> unique()
  data_x   <- data_x  |> select(all_of(arrange0))
  data_x   <- data_x  |> arrange_at(c(arrange0))
  ### Return
  return(data_x)
}


###### Extrapolate Impact Function
### Function to extrapolate an impact function
# extrapolate_impFunction <- function(
#     df0,
#     xCol        = "xIn", ### Which column to use for x (default = temp_C)
#     yCol        = "yIn", ### Which column to use for y
#     xMin        = 0,     ### Minimum value for x
#     yMin        = 0,     ### Value of y at minimum value of x 
#     # extrapolate = TRUE,
#     extend_from = NULL,  ### Maximum value for model type to extend from, if not missing
#     extend_to   = NULL,  ### Extend last points for x
#     unitScale   = 0.5,   ### Scale between values,
#     extend_all  = FALSE  ### Whether to extend all models or just those that go to the max model value
# ){
#   ### Filter out NA values
#   select0   <- c(xCol, yCol)
#   df0       <- df0 |> select(all_of(select0))
#   df0       <- df0 |> filter_all(all_vars(!is.na(.)))
#   
#   ### Rename values
#   renameAt  <- c(xCol, yCol)
#   renameTo  <- c("xIn", "yIn")
#   df0       <- df0 |> rename_at(c(renameAt), ~renameTo)
#   
#   ### Standardize minimum value, then get unique values
#   df0_min   <- tibble(xIn=xMin, yIn=yMin)
#   df0       <- df0_min |> rbind(df0) 
#   df0       <- df0     |> unique()
#   rm(df0_min)
#   
#   ### Extend from/to
#   ### Make sure values are numeric
#   extendAt  <- extend_from |> as.character() |> as.numeric()
#   extendTo  <- extend_to   |> as.character() |> as.numeric()
#   
#   ### Arrange by x values
#   ### - Get maximum value
#   df0       <- df0 |> arrange_at(vars("xIn"))
#   xIn_max   <- df0 |> pull(xIn) |> max()
#   # xOut_min  <- xIn_max + unitScale
#   xOut_max  <- extendTo
#   xOut      <- xOut_max
#   # xOut      <- xOut_min:xOut_max
#   
#   ### If extrapolate:
#   extrap0   <- (xIn_max == extendAt) & (extendAt != extendTo)
#   extrap0   <- extrap0 | extend_all
#   if(extrap0){
#     ### - Filter to the last two observations
#     # df0 |> tail(2) |> print()
#     ### Get linear trend
#     lm_ex    <- lm(yIn~xIn, data=df0 |> tail(2))
#     slope0   <- lm_ex$coefficients[[2]]
#     inter0   <- lm_ex$coefficients[[1]]
#     ### Extend values, then bind with earlier observations
#     df_new   <- tibble(xIn = xOut) |> mutate(yIn = inter0 + xIn * slope0)
#     df0      <- df0 |> rbind(df_new)
#     # ### Sort and get new y value to extend to
#     # which0   <- df0$xIn == extend_to
#     # yMaxNew  <- df0$yIn[df0$xIn == extend_to]
#   } ### End if(extrapolate)
#   
#   # ### Then, interpolate over the range
#   # xIn0      <- df0  |> pull(xIn)
#   # yIn0      <- df0  |> pull(yIn)
#   # range0    <- xIn0 |> range()
#   # xOut0     <- range0[1] |> seq(range0[2], by=unitScale)
#   # # xIn0 |> print(); yIn0 |> print(); xOut0 |> print()
#   # df0       <- approx(x=xIn0, y=yIn0, xout=xOut0, rule=1)
#   # df0       <- df0 |> as_tibble()
#   # 
#   # renameAt  <- c("x", "y")
#   # ### Rename columns and bind
#   ### Rename columns and bind
#   renameAt  <- c("xIn", "yIn")
#   renameTo  <- c(xCol, yCol)
#   df0       <- df0 |> rename_at(c(renameAt), ~renameTo)
#   
#   ### Return
#   return(df0)
# }
###### Extrapolate Impact Function
### Function to extrapolate an impact function
extrapolate_impFunction <- function(
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
  ### Filter out NA values
  select0   <- c(xCol, yCol)
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> filter_all(all_vars(!(. |> is.na())))
  browser()
  ### Rename values
  renameAt  <- c(xCol, yCol)
  renameTo  <- c("xIn", "yIn")
  df0       <- df0 |> rename_at(c(renameAt), ~renameTo)
  
  ### Standardize minimum value, then get unique values
  df0_min   <- tibble(xIn=xMin, yIn=yMin)
  df0       <- df0_min |> rbind(df0) 
  df0       <- df0     |> unique()
  rm(df0_min)
  
  ### Sort values
  ### - Get max x value for which is there a non-missing y-value
  sort0     <- c("xIn")
  df0       <- df0    |> arrange_at(c(sort0))
  extendAt  <- df0    |> pull(all_of(sort0)) |> max()
  rm(sort0)
  
  ### Extend from/to
  ### Make sure values are numeric
  # extendAt  <- extend_from |> as.character() |> as.numeric()
  extendTo  <- extend_to   |> as.character() |> as.numeric()
  
  ### Arrange by x values
  ### - Get max x value for which is there a non-missing y-value
  ### - Get maximum value
  df0       <- df0 |> arrange_at(vars("xIn"))
  xIn_max   <- df0 |> pull(xIn) |> max()
  # xOut_min  <- xIn_max + unitScale
  xOut_max  <- extendTo
  xOut      <- xOut_max
  # xOut      <- xOut_min:xOut_max
  
  ### If extrapolate:
  extrap0   <- (xIn_max == extendAt) & (extendAt != extendTo)
  extrap0   <- extrap0 | extend_all
  if(extrap0){
    ### - Filter to the last two observations
    # df0 |> tail(2) |> print()
    ### Get linear trend
    lm_ex    <- lm(yIn~xIn, data=df0 |> tail(2))
    slope0   <- lm_ex$coefficients[[2]]
    inter0   <- lm_ex$coefficients[[1]]
    ### Extend values, then bind with earlier observations
    df_new   <- tibble(xIn = xOut) |> mutate(yIn = inter0 + xIn * slope0)
    df0      <- df0 |> rbind(df_new)
    # ### Sort and get new y value to extend to
    # which0   <- df0$xIn == extend_to
    # yMaxNew  <- df0$yIn[df0$xIn == extend_to]
  } ### End if(extrapolate)
  
  # ### Then, interpolate over the range
  # xIn0      <- df0  |> pull(xIn)
  # yIn0      <- df0  |> pull(yIn)
  # range0    <- xIn0 |> range()
  # xOut0     <- range0[1] |> seq(range0[2], by=unitScale)
  # # xIn0 |> print(); yIn0 |> print(); xOut0 |> print()
  # df0       <- approx(x=xIn0, y=yIn0, xout=xOut0, rule=1)
  # df0       <- df0 |> as_tibble()
  # 
  # renameAt  <- c("x", "y")
  # ### Rename columns and bind
  ### Rename columns and bind
  renameAt  <- c("xIn", "yIn")
  renameTo  <- c(xCol, yCol)
  df0       <- df0 |> rename_at(c(renameAt), ~renameTo)
  
  ### Return
  return(df0)
}


get_impactFunctions <- function(
    df0         = NULL, ### Data frame with scaled impacts data
    groupCol    = NULL, ### Which column to look for the scenario column name (default = temp_impact_scenario )
    xCol        = NULL, ### Which column to use for x (default = temp_C)
    yCol        = NULL, ### Which column to use for y
    xMin        = 0,    ### Minimum value for x
    yMin        = 0,    ### Value of y at minimum value of x 
    # extrapolate = FALSE, ### Whether to extrapolate by default
    # unitScale   = NULL,  ### Scale between values
    extend_from = NULL,  ### Maximum value for model type to extend from, if not missing
    extend_to   = NULL,  ### Extend last points for x
    extend_all  = FALSE  ### Whether to extend all models or just those that go to the max model value
){
  ###### Groups ######
  ### Create groups and get group keys
  # df0 |> glimpse(); c(groupCol, xCol, yCol) |> print()
  df0      <- df0 |> group_by_at(vars(groupCol))
  df0      <- df0 |> arrange_at (vars(groupCol))
  groups0  <- df0 |> group_keys() |> pull(all_of(groupCol))
  nGroups0 <- groups0 |> length()
  # groups0 |> head() |> print()
  browser()
  ###### Extrapolate Data ######
  df0      <- df0 |> group_map(function(.x, .y){
    ### Unique group
    # .x |> glimpse(); .y |> pull(all_of(groupCol)) |> print()
    group_i  <- .y |> pull(all_of(groupCol)) |> unique()
    
    ### Extrapolate values
    df_i     <- .x |> extrapolate_impFunction(
      xCol        = xCol, 
      yCol        = yCol, 
      xMin        = xMin, 
      yMin        = yMin, 
      extend_from = extend_from,
      extend_to   = extend_to,  
      extend_all  = extend_all  
    ) ### extrapolate_impFunction
    
    ### Add group and ename columns in df_i
    renameAt <- c("group_id")
    renameTo <- c(groupCol)
    df_i     <- df_i |> mutate(group_id = group_i)
    df_i     <- df_i |> rename_at(vars(renameAt), ~renameTo)
    
    ### Return
    return(df_i)
  }) |> bind_rows()
  
  ###### Get Impact Functions ######
  df0      <- df0 |> ungroup()
  df0      <- df0 |> group_by_at(vars(groupCol))
  df0      <- df0 |> arrange_at (vars(groupCol))
  groups0  <- df0 |> group_keys() |> pull(all_of(groupCol))
  list0    <- df0 |> group_map(function(.x, .y){
    ### Unique group
    # .x |> glimpse(); .y |> pull(all_of(groupCol)) |> print()
    group_i  <- .y |> pull(all_of(groupCol)) |> unique()
    
    ### Approximation function
    ### Create a piece-wise linear interpolation function using approxfun and defaults
    ###    rule = 1 (Returns NA for x-values outside range)
    ###    ties = mean (take the average of multiple values)
    fun_i    <- approxfun(x=.x |> pull(all_of(xCol)), y=.x |> pull(all_of(yCol)), method="linear", rule=1)
    
    ### Return
    return(fun_i)
  }) |> set_names(groups0)
  
  ###### Return ######
  ### Create a list with the impact functions and data
  list0   <- list(df0=df0, funs0=list0)
  gc()
  return(list0)
}




###### fun_slrModel2Height
### Helper function to convert SLR model to height in cm
fun_slrModel2Height <- function(
    col_x,    ### column "model"
    include   = c("factor", "values"),
    valType   = c("numeric", "character", "factor"),
    labelType = c("numeric", "character") ### Used for factor or label
){
  ### Checks
  do_factor <- "factor" %in% include
  do_values <- "values" %in% include
  do_both   <- do_factor & do_values
  ### Value types and priority
  valTypes <- c("numeric", "character", "factor")
  valType0 <- valType
  valType0 <- valTypes |> (function(y, types_y=valTypes){
    ls1 <- ls0 <- types_y
    c0  <- ls0[1] %in% y
    c1  <- ls0[2] %in% y
    c3  <- ls0[2] %in% y
    if     (c0) {ls1 <- ls0[1]}
    else if(c1) {ls1 <- ls0[2]}
    else        {ls1 <- ls0[3]}
    return(ls1)
  })()
  do_numb  <- "numeric"   %in% valType
  do_char  <- "character" %in% valType
  do_fact  <- "factor"    %in% valType
  # valType |> print(); labelType |> print()
  ### Label types and priority
  labTypes <- c("numeric", "character")
  label_x0 <- labelType |> (function(y, types_y=labTypes){
    ls1 <- ls0 <- types_y
    c0  <- do_numb | do_char
    c1  <- ls0[1] %in% y
    if     (c0) {ls1 <- ls0[1]}
    else if(c1) {ls1 <- ls0[1]}
    else        {ls1 <- ls0[2]}
    return(ls1)
  })()
  # label_x0 |> print()
  labChar       <- "character" %in% label_x0
  # label_x0 |> print(); labChar |> print()
  ### Original labels
  lvl_x0        <- col_x |> unique()
  df_x0         <- tibble(levels=lvl_x0)
  ### Standardize
  df_x0$labels  <- df_x0 |> pull(levels) |> str_replace("_" , "")
  df_x0$numbers <- df_x0 |> pull(labels) |> str_replace("cm", "")
  df_x0$values  <- df_x0$numbers |> as.character() |> as.numeric()
  ### Sprt
  df_x0         <- df_x0 |> arrange_at(vars("values"))
  ### Create factor list
  list_x        <- list(factors=df_x0)
  ### Adjust values
  vals_x        <- NULL
  if(do_values){
    if(labChar){labels_x <- df_x0 |> pull(labels)}
    else       {labels_x <- df_x0 |> pull(values)}
    vals_x <- col_x  |> factor(levels=df_x0$levels, labels=labels_x)
    if(do_char){vals_x <- vals_x |> as.character()}
    if(do_numb){vals_x <- vals_x |> as.numeric()}
    list_x[["values"]] <- vals_x
  }
  ### Return list
  if     (do_both  ) {return_x <- list_x}
  else if(do_factor) {return_x <- list_x$factors}
  else               {return_x <- list_x$values}
  ### Return
  gc()
  return(return_x)
}