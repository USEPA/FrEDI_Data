####### FrEDI Data Utilities 
###### Function to get years from data
### Get a sequence from a set of years
get_years_fromData <- function(years0, by=1){
  min0 <- years0 |> min(na.rm=T)
  max0 <- years0 |> max(na.rm=T)
  yrs0 <- min0 |> seq(max0, by=by)
  return(yrs0)
}

###### Format GCAM scenarios
format_gcamData <- function(
    df0 ### Original GCAM data
){
  ### Select values
  select0 <- c("year", "temp_C_global", "scenario", "model")
  df0     <- df0 |> select(all_of(df0))
  rm(select0)
  
  ### Calculate
  # df0 |> glimpse()
  df0     <- df0 |> filter_all(any_vars(!(. |> is.na())))
  # df0 |> glimpse()
  
  ### Calculate temp_C_conus
  scen0   <- df0 |> pull(scenario) |> unique()
  # scen0 |> print()
  df0     <- list(scen_i=scen0, df0_i=scen0 |> map(function(scen_i){df0 |> filter(scenario == scen_i)})) |>
    scen0 |> map(function(scen_i, df0_i){
    df0_i |> format_gcamData_byScenario()
  }) |> bind_rows()
  # df0 |> glimpse()
  
  ### Select values
  select0 <- c("year", "temp_C_global", "temp_C_global", "slr_cm", "scenario", "model")
  df0     <- df0 |> select(all_of(select0))
  rm(select0)
  
  ### Return
  return(df0)
}

###### Format GCAM scenario
format_gcamData_byScenario <- function(
    df0 ### Original GCAM data, filtered to a specific scenario
){
  ### Calculate
  # df0 |> glimpse()
  
  ### Years
  years0 <- df0 |> pull(year) |> get_years_fromData()
  
  ### Interpolate temperatures by year
  df0    <- df0 |> mutate(region = "NationalTotal") 
  df0    <- df0 |> interpolate_annual(years=list_years, column="temp_C_global", rule=2:2)
  df0    <- df0 |> select(-c("region"))
  # df0_i |> glimpse()

  ### Calculate CONUS temperatures and move before global temps
  df0    <- df0 |> mutate(temp_C_conus = temp_C_global |> convertTemps(from="global"))
  df0    <- df0 |> relocate(c("temp_C_conus"), .before("temp_C_global"))
  
  ### Then, calculate SLR heights
  df1    <- FrEDI::temps2slr(temps=df0 |> pull(temp_C_global), years=df0 |> pull(year))
  df0    <- df0 |> left_join(df1_i, by=c("year"))
  # df0 |> glimpse()

  ### Select values  
  df0   <- df0 |> select(c("year", "temp_C_conus", "temp_C_global", "slr_cm", "scenario", "model"))
  
  ### Return
  return(df0)
}

####### extend_data 
extend_data <- function(
    df0, ### Data frame to extend
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
  df0    <- df0 |> filter(year< from0)
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
  x_nu <- tibble(year = newYears) |> mutate(joinCol = 1)
  x_up <- x |> filter(year == maxYear_x) |> mutate(joinCol = 1) |> select(-c("year"))
  x_lo <- x |> filter(year <= maxYear_x)
  rm("x")
  ### Join data
  x_up <- x_up |> left_join(x_nu, by = c("joinCol")) |> select(-c("joinCol"))
  x    <- x_lo |> rbind(x_up)
  rm("x_nu", "x_up", "x_lo")
  # ### Arrange and standardize model type
  # x  <- x |> arrange_at(c(arrange_x)) |> mutate(model_type = "slr")
  ### Return
  return(x)
} ### End function


###### SLR Extremes #######
### Function to iterate over in fun_slrConfigExtremes
get_slrMaxValues <- function(
    year_i,
    data_x, ### Tibble of SLR heights
    data_y  ### Tibble of SLR impacts
){
  ### Columns
  yearCol0   <- c("year")
  modCols0   <- c("model", "model_dot", "model_type")
  arrange0   <- c("driverValue")  |> c(modCols0) |> c(yearCol0)
  impCols0   <- data_y |> names() |> (function(x){x[!(x %in% arrange0)]})()
  impCols0   <- impCols0 |> c(yearCol0)
  ### Other values
  bounds0    <- c("lower", "upper")
  
  ### Filter data & drop columns
  drop0      <- modCols0
  dfx_i      <- data_x |> filter(year==year_i) |> select(-all_of(drop0))
  dfy_i      <- data_y |> filter(year==year_i) |> select(-all_of(drop0))
  rm(drop0)
  
  ### Driver values:
  ### - Get driver values and then unique driver values
  ### - Figure out which the last values belong to
  vals_i    <- dfx_i[["driverValue"]] |> unique() |> sort(decreasing=TRUE)
  ### Add value
  addVal_i  <- vals_i |> length() == 1
  if(addVal_i){vals_i <- vals_i |> rep(2)}
  ref_i     <- tibble(driverValue = vals_i [1:2])
  ref_i     <- ref_i |> mutate(valueType   = bounds0[2:1]) ### End tibble
  ref_i     <- ref_i |> mutate(year = year_i)
  
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
  # dfy_i |> glimpse(); group0 |> print()
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
  ### Columns
  yearCol0 <- c("year")
  modCols0 <- c("model", "model_dot", "model_type")
  # exCols0  <- modCols0 |> c("scaled_impacts", "model_cm") |> c(yearCol0)
  exCols0  <- modCols0 |> c("scaled_impacts", "model_cm") |> c(yearCol0)
  impCols0 <- imp_x    |> names() |> (function(x){x[!(x %in% exCols0)]})()
  impCols0 <- impCols0 |> c(yearCol0)
  
  ### Prepare data
  ### SLR Heights: slr_df; SLR Impacts: imp_df
  slr_df   <- slr_x  |> mutate(model_cm = model_dot |> fun_slrModel2Height(include="values"))
  imp_df   <- imp_x  |> mutate(model_cm = model_dot |> fun_slrModel2Height(include="values"))
  rm(slr_x, imp_x)
  # slr_df |> head() |> glimpse(); imp_df |> head() |> glimpse()
  
  ### Get upper and lower for each year
  slrYears <- slr_df[["year"]] |> unique() |> sort()
  slr_extr <- slrYears |> map(~.x |> get_slrMaxValues(data_x=slr_df, data_y=imp_df))
  slr_extr <- slr_extr |> bind_rows()
  
  ###### Select and arrange ######
  # slr_extr |> names() |> print()
  arrange0 <- impCols0 |> c("valueType", "driverValue", "scaled_impacts")
  select0  <- arrange0 |> c("model_cm")
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
  rename0  <- c("driverValue", "scaled_impacts", "model_cm")
  suffix0  <- c("1", "2")
  slr_lo   <- slr_lo |> rename_at(c(rename0), ~rename0 |> paste0("1"))
  slr_up   <- slr_up |> rename_at(c(rename0), ~rename0 |> paste0("2"))
  
  ### - Join upper and lower values
  join0    <- impCols0
  slr_extr <- slr_up |> left_join(slr_lo, by = c(join0))
  rm(slr_up, slr_lo); rm(rename0)
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


###### fun_formatScalars #######
### Function to format scalars in createSystemData
fun_formatScalars <- function(
    data_x,  ### rDataList$scalarDataframe
    info_x,  ### rDataList$co_scalarInfo
    years_x  ### rDataList$list_years
){
  ### By state
  stateCols0 <- c("state", "postal")
  
  ### Join info
  join0    <- c("scalarName", "scalarType")
  select0  <- join0  |> c("national_or_regional", "constant_or_dynamic")
  select1  <- join0  |> c("region") |> c(stateCols0) |> c("year", "value")
  info_x   <- info_x |> select(all_of(select0))
  data_x   <- data_x |> select(all_of(select1))
  data_x   <- data_x |> left_join(info_x, by=c(join0))
  
  ### Get unique names & types
  group0  <- select0 |> c("byState")
  dfNames <- data_x  |>
    group_by_at(c(group0)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  # data_x |> glimpse(); dfNames |> glimpse()
  rm(group0)
  
  ### Iteration list
  listPm  <- list()
  listPm[["name_i"]] <- dfNames[["scalarName"]] |> as.list()
  listPm[["type_i"]] <- dfNames[["scalarType"]] |> as.list()
  listPm[["con_i" ]] <- dfNames[["constant_or_dynamic" ]] |> as.list()
  listPm[["nat_i" ]] <- dfNames[["national_or_regional"]] |> as.list()
  # listPm |> print()
  
  ### Iterate over list and interpolate annual values
  # "got here4" |> print()
  data_x   <- listPm %>% pmap(function(name_i, type_i, con_i, nat_i){
    ### Filter to appropriate name and type
    data_i    <- data_x |> filter(scalarName==name_i)
    data_i    <- data_i |> filter(scalarType==type_i)
    data_i    <- data_i |> filter(national_or_regional==nat_i)
    ### Info about method
    method_i  <- (con_i=="constant") |> ifelse(con_i, "linear")
    if(byState){states_i <- data_i[["state"]] |> unique()} else{states_i <- "N/A"}
    byState_i <- !("N/A" %in% states_i) & byState
    ### Interpolate data
    interpolate_annual    <- utils::getFromNamespace("interpolate_annual"   , "FrEDI")
    data_i    <- data_i |> interpolate_annual(
      years   = years_x,
      column  = "value",
      rule    = 1:2,
      method  = method_i,
      byState = byState_i
    ) ### End interpolate_annual
    ### Return
    return(data_i)
  }) |> bind_rows()
  
  # ### Bind rows
  # "got here5" |> print(); data_x |> glimpse()
  ### Arrange
  arrange0 <- select1 |> c("national_or_regional") |> unique()
  data_x   <- data_x |> select(all_of(arrange0))
  data_x   <- data_x |> arrange_at(c(arrange0))
  ### Return
  return(data_x)
}



###### get_impactFunctions #######
### Last updated 2021.02.05
### Get Impact Functions (createSystemData)
### This is used by createSystemData (see inst/extdata/createSystemData.R) to generate impact functions
### This function can be run separately and its outputs saved as an R data object to facilitate computation time.
get_impactFunctions <- function(
    x         = NULL, ### Data frame with scaled impacts data
    groupCol  = NULL, ### Which column to look for the scenario column name (default = temp_impact_scenario )
    xCol      = NULL, ### Which column to use for x (default = temp_C)
    yCol      = NULL, ### Which column to use for y
    # extrapolate = FALSE, ### Whether to extrapolate by default
    extend_from = NULL, ### Maximum value for model type to extend from, if not missing
    extend_to   = NULL, ### Extend last points for x
    extend_all  = FALSE, ### Whether to extend all models or just those that go to the max model value
    unitScale   = NULL ### Scale between values
){
  ###### Defaults ######
  unitScale   <- unitScale |> ifelse(1, unitScale)
  # extend_to      <- ifelse(is.null(extend_to     ),        1, unitScale)
  ###### Group data ######
  x$group_id  <- x[[groupCol]]
  x$xIn       <- x[[xCol    ]]
  x$yIn       <- x[[yCol    ]]
  ###### Extend from/to ######
  ### Make sure they are numeric
  extend_from <- extend_from |> as.character() |> as.numeric()
  extend_to   <- extend_to   |> as.character() |> as.numeric()
  
  ###### Groups ######
  ### Create groups and get group keys
  x        <-  x |> group_by(group_id)
  groups_x <- (x |> group_keys())$group_id |> unique()
  
  ### Initialize data
  xIn_min  <- 0
  yIn_min  <- 0
  df_0     <- tibble(xIn = xIn_min, yIn = yIn_min)
  
  ###### Generate list of impact functions ######
  ### Iterate over the groups
  list_x   <- x |> group_map(function(.x, .y, .keep=T){
    group_i     <- .x[["groupCol"]] |> unique()
    
    ###### Subset values ######
    ### Subset data to scenario name and exclude NA values, then add a zero value
    df_i        <- .x   |> select(xIn, yIn) |> filter(!is.na(yIn))
    df_i        <- df_0 |> rbind(df_i)
    
    ###### Information about Extrapolation values ######
    ### Length of df_i
    len_i       <- df_i |> nrow()
    # ### Extend values out to 10 degrees of warming
    xIn_max     <- df_i$xIn[len_i]
    yIn_max     <- df_i$yIn[len_i]
    yMaxNew     <- NA
    
    # extrapolate |> print(())
    ### Whether to extend values
    ### Extend values out to the specified value
    ### - Find linear relationship between last two points
    ### - Interpolate the last few values
    # extrapolate <- TRUE
    extrapolate <- (xIn_max == extend_from) & (extend_from!=extend_to)
    extrapolate <- extend_all
    # extrapolate |> print()
    if(extrapolate){
      df_ref_i <- df_i[len_i + -1:0,]
      # df_ref_i |> print()
      ### Get linear trend
      lm_i     <- lm(yIn~xIn, data=df_ref_i)
      ### Extend values
      # df_new_i <- tibble(xIn = seq(xIn_max + unitScale, extend_to, unitScale))
      df_new_i <- tibble(xIn = c(xIn_max + unitScale, extend_to))
      df_new_i <- df_new_i |> mutate(yIn = xIn * lm_i$coefficients[2] + lm_i$coefficients[1])
      ### Bind the new observations with the other observations
      df_i     <- df_i |> rbind(df_new_i)
      ### Sort and get new y value to extend to
      which_i <- df_i$xIn == extend_to
      yMaxNew <- df_i$yIn[which_i]
    } ### End if(extrapolate)
    
    ###### Linear Interpolation ######
    ### Create a piece-wise linear interpolation function using approxfun and defaults
    ###    rule = 1 (Returns NA for x-values outside range)
    ###    ties = mean (take the average of multiple values)
    # fun_i <- approxfun(x = df_i$xIn, y = df_i$yIn, method = "linear", rule = 1)
    fun_i <- approxfun(
      x = df_i$xIn,
      y = df_i$yIn,
      method = "linear",
      yleft  = yIn_min,
      yright = yMaxNew
    ) ### End approxfun
    ### Return fun_i
    return(fun_i)
  }) ### End group map
  
  ##### Add names to the list
  list_x <- list_x |> set_names(groups_x)
  
  ###### Return Object ######
  return(list_x)
}

###### fun_slrModel2Height ######
### Helper function to convert SLR model to height in cm
fun_slrModel2Height <- function(
    col_x, ### column "model_dot"
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
    if(c0) {ls1 <- ls0[1]}
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
  label_x0 <- labelType |>
    (function(y, types_y=labTypes){
      ls1 <- ls0 <- types_y
      c0  <- do_numb | do_char
      c1  <- ls0[1] %in% y
      if(c0) {ls1 <- ls0[1]}
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
  df_x0$labels  <- gsub("_" , "", df_x0$levels)
  df_x0$numbers <- gsub("cm", "", df_x0$labels)
  df_x0$values  <- df_x0$numbers |> as.character() |> as.numeric()
  ### Sprt
  df_x0         <- df_x0 |> arrange_at(.vars=c("values"))
  ### Create factor list
  list_x        <- list(factors=df_x0)
  ### Adjust values
  vals_x        <- NULL
  if(do_values){
    if(labChar){labels_x <- df_x0$labels}
    else       {labels_x <- df_x0$values}
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
  return(return_x)
}