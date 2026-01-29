###### Overview ######
### This file contains helper functions for FrEDI's state-level functionality.

###### loadFrediImpacts ######
### Load state scaled impact data from a specified directory.
loadFrediImpacts_nat <- function(
    fpath   = "." |> file.path("inst", "extdata", "fredi","national"), ### file path to directory with state-level slr and gcm scaled impact data
    type    = "gcm" ### Or slr
){
  ### Do GCM
  type       <- type |> tolower()
  do_gcm     <- "gcm" %in% type
  ### Column names modelUnitValue
  valCol     <- do_gcm |> ifelse("modelUnitValue", "year")
  sumCols    <- c(valCol) |> c("value")
  mainCols   <- c("sector", "variant", "impactType", "impactYear", "model")
  mainCols   <- mainCols |> c(sumCols)
  
  ### Get files:
  fpath_type      <- fpath |> file.path(type)
  fPaths     <- fpath_type |> list.files(full.names=T)
  # fPaths |> head() |> print() 
  # fPaths |> file.exists() |> head() |> print()
  ### Impacts data
  df_impacts <- fPaths |> map(function(
    file_i, 
    cols0 = mainCols, 
    sum0  = sumCols
  ){
    ### Modify data
    df0     <- file_i |> read.csv() |> as_tibble()
    df0     <- df0 |> mutate_at(c(cols0), as.character)
    df0     <- df0 |> mutate_at(c(sum0 ), as.numeric)
    ### Select columns
    select0 <- c(cols0, sum0)
    df0     <- df0 |> select(all_of(select0))
    ### Return
    return(df0)
  }) |> bind_rows()
  
  ### Return 
  return(df_impacts)
}


###### loadFrediScalars ######
### Load state scalar data from a specified directory.
loadFrediScalars_nat <- function(
    fpath = "." |> file.path("inst", "extdata", "fredi","national", "scalars") ### File path to directory with state-level scalar data
){
  ### File names
  fnames     <- fpath |> list.files(full.names=T)
  # fnames |> head() |> print() 
  # fnames |> file.exists() |> head() |> print()
  scalarData <- fnames |> map(function(file_i){
    cols0 <- c( "postal","state", "scalarName", "year", "value")
    df0   <- file_i |> read.csv() |> as_tibble() #|> mutate( value = as.double(value))
    df0   <- df0    |> select(all_of(cols0))
    return(df0)
  }) |> bind_rows()
  ### Return
  return(scalarData)
}


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
