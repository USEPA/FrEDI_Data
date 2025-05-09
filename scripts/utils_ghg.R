ghg_groupMap <- function(
    .x, 
    .y,
    yCols0, ### Columns to sum
    xCol0 = "year", ### X column
    xOut0,  ### New or x values
    rule0 = 2
){
  ### Iterate over yCols, interpolate, and reduce
  .x <- yCols0 |> 
    map(ghg_mapApprox, df0=.x, xCol0=xCol0, xOut0=xOut0, rule0=rule0) |> 
    reduce(left_join, by=xCol0)
  ### Cross join with group info
  .x <- .x |> cross_join(.y)
  ### Return
  return(.x)
}

ghg_mapApprox <- function(
    yCol0,
    xCol0 = "year", 
    xOut0,
    df0,
    rule0 = 2
){
  ### Check if there are values
  df0  <- df0 |> filter_at(c(yCol0), function(x){!(x |> is.na())})
  do0  <- df0 |> nrow()
  ### If there are values, pull values and interpolate
  if(do0) {
    ### Input values
    xIn  <- df0 |> pull(all_of(xCol0))
    yIn  <- df0 |> pull(all_of(yCol0))
    ### Approximate new y values
    df0  <- xIn |> approx(y=yIn, xout=xOut0, rule=rule0) |> bind_cols()
  } else{
    df0  <- tibble(x = xOut0, y = NA)
  } ### End if(doX)
  ### Rename values
  df0 <- df0 |> rename_at(c("x", "y"), ~c(xCol0, yCol0))  
  ### Return
  return(df0)
}

format_ghgAsthmaAffectedPop <- function(
    df0, 
    dfS, ### co_states
    dfA, ### co_ageTypes
    maxYr0 = 2100 ### configList$coefficients$maxYear0
){
  ### Rename values
  from0   <- c("State_FIPS", "newAgeRange", "Population", "Pct.State.Pop", "Year")
  to0     <- c("fips", "ageRange", "statePopRef", "ageRangePct", "year")
  sort0   <- c("ageRange", "fips", "year")
  lvls0   <- co_ageTypes |> pull(ageRange) |> levels()
  df0     <- df0 |> 
    select(all_of(from0)) |>
    rename_at(c(from0), ~to0) |> 
    mutate_at(c("ageRange"), factor, levels=lvls0) |>
    arrange_at(c(sort0))
  
  ### Create tibble of years
  # dfYrs0  <- df0  |> select(year) |> unique() |> arrange_at(c("year"))
  # newYrs0 <- df0  |> pull(year) |> (function(x){x |> min() |> seq(x |> max())})()
  yrs0    <- df0  |> pull(year) |> unique() |> sort()
  newYrs0 <- yrs0 |> min() |> seq(maxYr0, by=1)
  dfYrs0  <- tibble(year = newYrs0)
  
  ### Group and interpolate
  sum0    <- c("statePopRef", "ageRangePct")
  group0  <- c("ageRange", "fips")
  # names0  <- df0 |> names()
  # group0  <- names0 |> get_matches(y=c(sum0, "year"), matches=F)
  df0     <- df0 |> group_by_at(c(group0))
  df0     <- df0 |> group_map(function(.x, .y){
    .x |> ghg_groupMap(
      .y     = .y,
      yCols0 = sum0, ### Columns to sum
      xCol0  = "year", ### X column
      xOut0  = newYrs0 ### New or x values
    ) ### End ghg_groupMap
  }) |> bind_rows() |> ungroup()
  # df0 |> glimpse()
  rm(sum0)
  
  ### Format states, age ranges, years to join
  join0   <- c("ageRange", "fips", "year")
  ages0   <- df0  |> pull(ageRange) |> unique()
  dfA     <- dfA  |> filter(ageRange %in% ages0)
  dfJoin0 <- dfS  |> 
    cross_join(dfA) |>
    cross_join(dfYrs0) |> 
    arrange_at(c(sort0)) |> 
    group_by_at(c(group0))
  # dfJoin0 |> glimpse()
  
  ### Join state, age range, and year info with df0 to standardize
  ### Then calculate values
  df0       <- dfJoin0 |> 
    left_join(df0, by=join0) |> 
    arrange_at(c(sort0)) |> 
    group_by_at(c(group0)) |>
    mutate(affectedPop = ageRangePct * statePopRef)
  rm(dfJoin0)
  # df0 |> glimpse()
  
  ### Prepare reference data
  ### 102
  # dfRef0  <- dfJoin0 |> slice(1) |> select(all_of(join0))
  select0 <- group0 |> c("affectedPop")
  dfRef0  <- df0    |> 
    slice(1) |> 
    select(all_of(select0)) |>
    rename_at(c("affectedPop"), ~"affectedPopBase")
  # dfRef0 |> glimpse()
  df0     <- df0    |> 
    left_join(dfRef0, by=group0) |>
    ungroup()
  rm(dfRef0)
  
  ### Return
  return(df0)
}


format_ghgAsthmaExcessCases <- function(
    df0, 
    df1, ### asthmaAgePcts
    dfM, ### co_models
    dfT, ### Impact types
    maxYr0 = 2100 ### configList$coefficients$maxYear0
){
  ### Rename values
  ### Filter out model = "MMM" (multi-model mean)
  from0     <- c("Endpoint", "Start_Age", "End_Age", "State_FIPS", "Model", "ModelYear", "State_Results")
  to0       <- c("endpoint", "startAge", "endAge", "fips", "model", "year", "excessAsthma")
  sort0     <- to0 |> get_matches("excessAsthma", matches=F)
  lvls0     <- df1 |> pull(ageRange) |> levels()
  ages0     <- lvls0 |> unique()
  df0       <- df0 |> 
    rename_at(c(from0), ~to0) |> 
    filter_at(c("model"), function(x, y="MMM"){!x %in% y}) |>
    relocate(any_of(to0)) |>
    mutate(ageRange = startAge |> paste0("TO", endAge) |> factor(levels=lvls0)) |>
    arrange_at(c(sort0))
  # df0 |> glimpse()
  
  ### Get standard IDs and standardize endpoints data
  # dfEnds    <- df0 |> select(c("endpoint", "ageRange")) |> unique()
  dfEnds    <- df0 |> select(c("endpoint", "startAge", "endAge", "ageRange", "model")) |> unique()
  dfFips    <- df1 |> select(c("ageRange", "fips")) |> unique()
  dfIds     <- dfFips |> 
    left_join(dfEnds, by="ageRange", relationship="many-to-many") |> 
    arrange_at(c("endpoint", "ageRange", "fips"))
  ### Join data with IDs to standardize
  join0     <- c("endpoint", "startAge", "endAge", "ageRange", "fips", "model")
  sort0     <- c("endpoint", "ageRange", "fips", "model", "year")
  df0       <- dfIds |> 
    left_join(df0, by=join0) |>
    arrange_at(c(sort0))
  
  ### Create tibble of years
  # dfYrs0  <- df0  |> select(year) |> unique() |> arrange_at(c("year"))
  # newYrs0 <- df0  |> pull(year) |> (function(x){x |> min() |> seq(x |> max())})()
  yrs0    <- df1  |> pull(year) |> unique() |> sort()
  newYrs0 <- yrs0 |> min() |> seq(maxYr0, by=1)
  dfYrs0  <- tibble(year = newYrs0)
  
  ### Group and interpolate
  sum0    <- c("excessAsthma")
  group0  <- c("endpoint", "ageRange", "fips", "model")
  # names0  <- df0 |> names()
  # group0  <- names0 |> get_matches(y=c(sum0, "year"), matches=F)
  df0     <- df0 |> group_by_at(c(group0))
  df0     <- df0 |> group_map(function(.x, .y){
    .x |> ghg_groupMap(
      .y     = .y,
      yCols0 = sum0, ### Columns to sum
      xCol0  = "year", ### X column
      xOut0  = newYrs0 ### New or x values
    ) ### End ghg_groupMap
  }) |> bind_rows() |> ungroup()
  # df0 |> glimpse()
  rm(sum0)
  
  ### Add in base values as placeholders
  df0       <- df0
  
  ### Join with age info
  ### Join with model info
  ### Join with impact type info
  # dfMods0   <- df0 |> select(model) |> unique()
  join0     <- c("ageRange", "fips", "year")
  joinT     <- c("endpoint", "ageRange", "ageType")
  joinM     <- c("model_str")
  # df0 |> nrow() |> print()
  df0       <- df0 |> left_join(df1, by=join0)
  # df0 |> nrow() |> print()
  df0       <- df0 |> left_join(dfT, by=joinT)
  # df0 |> nrow() |> print()
  df0       <- df0 |>
    rename_at(c("model"), ~"model_str") |> 
    left_join(dfM, by=joinM)
  df0 |> nrow() |> print()
  rm(join0, df1, joinT, dfT, joinM, dfM)
  
  ### Relocate and arrange
  sort0     <- c("sector", "impactType", "endpoint", "ageRange", "fips", "model", "year")
  sum0      <- c("year", "statePopRef", "ageRangePct", "affectedPop", "affectedPopBase", "excessAsthma")
  names0    <- df0 |> names() |> get_matches(sum0, matches=F)
  df0       <- df0 |> 
    mutate(baseAsthmaNumer = 1) |>
    mutate(baseAsthmaDenom = 1) |> 
    relocate(any_of(sort0)) |> 
    relocate(any_of(sum0), .after=any_of(names0)) |>
    arrange_at(c(sort0))
  
  ### Return
  return(df0)
}