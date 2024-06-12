###### Overview ######
### This file contains helper functions for FrEDI's state-level functionality.

###### loadStateImpacts ######
### Load state scaled impact data from a specified directory.
loadStateImpacts <- function(
    fpath   = "." |> file.path("inst", "extdata", "state"), ### file path to directory with state-level slr and gcm scaled impact data
    type    = "gcm" ### Or slr
){
  ### Do GCM
  type          <- type |> tolower()
  do_gcm        <- "gcm" %in% type
  ### Column names
  mainCols      <- c("sector", "variant", "impactType", "impactYear", "state", "postal", "model")
  gcm_cols      <- c("modelUnitValue", "value")
  slr_cols      <- c("year", "value")
  if(do_gcm){sumCols <- gcm_cols} else{sumCols <- slr_cols}
  ### Get files:
  fpath         <- fpath |> file.path(type)
  fPaths        <- fpath |> list.files(full.names=T)
  ### Impacts data
  df_impacts    <- fPaths |> map(function(
    file_i, 
    cols0 = mainCols, 
    sum0  = sumCols
  ){
    ### Modify data
    df0     <- file_i |> read.csv() |> as_tibble()
    df0     <- df0 |> mutate_at(c(cols0), as.character)
    df0     <- df0 |> mutate_at(c(sum0), as.numeric)
    ### Select columns
    select0 <- c(cols0, sum0)
    df0     <- df0 |> select(all_of(select0))
    ### Return
    return(df0)
  }) |> bind_rows()
  
  ### Return 
  return(df_impacts)
}


###### loadStateScalars ######
### Load state scalar data from a specified directory.
loadStateScalars <- function(
    fpath = "." |> file.path("inst", "extdata", "state", "scalars") ### File path to directory with state-level scalar data
){
  ### File names
  fnames     <- fpath |> file.path("scalars") |> list.files(full.names=T)
  scalarData <- fnames |> map(function(file_i){
    cols0 <- c("state", "postal", "scalarName", "year", "value")
    df0   <- file_i |> read.csv() |> as_tibble()
    df0   <- df0    |> select(all_of(cols0))
    return(df0)
  }) |> bind_rows()
  ### Return
  return(scalarData)
}


###### loadStateData ######
### Load state scalar and scaled impacts data from a specified directory.
loadStateData <- function(
    fpath         = "." |> file.path("inst", "extdata", "state"), ### File path to directory with containing other state-level data
    popDir        = "scenarios",                  ### Directory name in fpath containing population info
    popFile       = "State ICLUS Population.csv", ### File name of file with state-level population scenarios
    popRatiosFile = "state_population_ratios.csv" ### File name of file with state-level population ratios
){
  ### Load scalars
  scalars        <- fpath |> loadStateScalars()
  ### Load population, population ratios
  popPath        <- fpath |> file.path(popDir, popFile)
  ratiosPath     <- fpath |> file.path(popDir, popRatiosFile)
  state_pop      <- popPath    |> read.csv() |> as_tibble()
  state_ratios   <- ratiosPath |> read.csv() |> as_tibble()
  ### Load impacts
  gcm_impacts    <- fpath |> loadStateImpacts(type="gcm")
  slr_impacts    <- fpath |> loadStateImpacts(type="slr")
  ### Add to list
  state_data     <- list()
  state_data[["df_stateScalars"   ]] <- scalars
  state_data[["df_gcmStateImpacts"]] <- gcm_impacts
  state_data[["df_slrStateImpacts"]] <- slr_impacts
  state_data[["df_statePop"       ]] <- state_pop
  state_data[["df_popRatios"      ]] <- state_pop
    
  ### Return
  return(state_data)
}

###### updateStateScalars ######
### Add national level scalars from regional data
updateStateScalars <- function(
    stateList0, ### List of loaded & reshaped state  data
    regList0    ### List of loaded & reshaped region data
){
  ### - Format regional scalars
  scalars0      <- stateList0[["scalarDataframe"]]
  scalars1      <- regList0  [["scalarDataframe"]]
  ### - Format data
  scalars1      <- scalars1 |> filter(region=="National.Total")
  scalars1      <- scalars1 |> mutate(state="All", postal="US")
  scalars0      <- scalars1 |> rbind(scalars0)
  stateList0[["scalarDataframe"]] <- scalars0
  ### Return
  return(stateList0)
}

###### Combine Reshaped Lists ######
### Combine region & state level reshaped data
combineReshapedLists <- function(
    regList0,  ### List of loaded & reshaped region data
    stateList0 ### List of loaded & reshaped state  data
){
  ### Names of objects to combine from reshaped data
  colsCombine0 <- c("scalarDataframe", "data_scaledImpacts", "slrImpacts")
  ### List of items in common, region list, state list
  listOther0   <- stateList0 |> (function(x){x[!(names(x) %in% colsCombine0)]})()
  regList0     <- regList0   |> (function(x){x[  names(x) %in% colsCombine0 ]})()
  stateList0   <- stateList0 |> (function(x){x[  names(x) %in% colsCombine0 ]})()
  ### Initialize list to combine
  listCombine0 <- list()
  listCombine0[[".region"]] <- regList0  
  listCombine0[[".state" ]] <- stateList0
  ### Iterate over list, combining items
  listCombine0 <- colsCombine0 |> map(function(.x){
    ### Tibbles
    df_region <- regList0  [[.x]]
    df_state  <- stateList0[[.x]]
    ### Check which sector is in which
    doScalars <- .x == "scalarDataframe"
    if(doScalars){
      ### Get list of scalars in state
      group0    <- c("scalarType", "scalarName")
      scalars0  <- df_state |> 
        group_by_at(c(group0)) |> 
        summarize(byState=n(), .groups="keep") |> ungroup() |>
        mutate(byState = 1)
      ### Join with region data & filter to scalars not in state
      df_region <- df_region |> left_join(scalars0, by=c(group0))
      df_region <- df_region |> filter(byState |> is.na())
      df_region <- df_region |> select(-c("byState"))
    } else{
      # df_region |> glimpse(); df_state |> glimpse()
      sectors0  <- df_state [["sector"]] |> unique()
      df_region <- df_region |> filter(!(sector %in% sectors0))
      rm(sectors0)
    } ### End else
    ### Modify region and combine
    df_region <- df_region |> mutate(state="N/A", postal="N/A", byState=0)
    df_state  <- df_state  |> mutate(byState=1) 
    df_state  <- df_state  |> rbind(df_region)
    ### Return
    return(df_state)
  }) |> set_names(colsCombine0)
  ### - Format regional scalars
  listCombine0  <- listCombine0 |> c(listOther0)
  ### Return
  return(listCombine0)
}
