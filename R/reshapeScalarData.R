#' reshapeScalarData
#'
#' @param scalarData Tibble with scalars data (as output from `loadFrediScalars`)
#' @param frediData  List of FrEDI configuration data (as output from `loadFrediData`)
#' @param silent     Indicate level of messaging
#' @param msg0       Prefix for messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeScalarData <- function(
    scalarData  = NULL, ### Tibble with scalars data
    controlData = NULL, ### Output of configControlTables
    scalarInfo  = NULL , ### List of FrEDI configuration data
    minYr0      = 2010,
    maxYr0      = 2300,
    yrCol0      = "year",
    valCol0     = "value",
    natStr0     = "US",
    # dropCols0   = "state", ### Drop state
    silent      = TRUE , ### Level of messaging
    msg0        = 0      ### Prefix for messaging
) {
  ### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Reshaping scalar data...") |> message()
  
  ### Assign Objects ----------------
  ### Assign tables in dataList to object in local environment
  # frediData |> names() |> print()
  # regCols0   <- c("region", "postal")
  # co_states  <- controlData[["co_states"]] |> select(all_of(regCols0))
  # scalarInfo <- configData [["co_scalarInfo"]]
  # co_states  |> glimpse(); scalarInfo |> glimpse()

  ### Columns & Values ----------------
  areaCol0   <- "area"
  regCol0    <- "region"
  stateCol0  <- "state"
  postCol0   <- "postal"
  fipsCol0   <- "fips"
  yrCol0     <- "year"
  regCols0   <- c(regCol0, postCol0)
  
  typeCol0   <- "scalarType"
  nameCol0   <- "scalarName"
  
  ### Join with State/Region Info ----------------
  ### Add region to scalar data frame
  # "got here0" |> print()
  
  
  # naStr0     <- "US"
  # select0    <- c("area", "region", "state", "postal", "fips")
  # "got here1" |> print()
  # select0    <- c("region", "state", "postal", "fips")
  co_states  <- controlData[["co_states"]] |> select(all_of(regCols0))
  co_states |> glimpse()
  # scalarData |> glimpse()
  
  # drop0      <- c(areaCol0, regCol0)
  drop0      <- c(areaCol0, regCol0, stateCol0, fipsCol0)
  join0      <- co_states  |> names() |> get_matches(y=scalarData |> names())
  move0      <- nameCol0 |> c(regCols0, yrCol0)
  scalarData <- scalarData |> 
    select(-any_of(drop0)) |>
    mutate_at(c(postCol0), replace_na, natStr0) |>
    left_join(co_states, by=postCol0) |> 
    relocate(any_of(move0))
  # "got here2" |> print()
  # scalarData |> glimpse()
  
  ### Format scalars
  drop0      <- c("groupId")
  scalarData <- scalarData |> fun_formatScalars(
    df1      = scalarInfo, 
    years0   = minYr0:maxYr0,
    natStr0  = natStr0,
    rule0    = 2
  ) |> select(-any_of(drop0)) ### End fun_formatScalars
  # "got here3" |> print()
  
  
  ### Standardize values by state/region
  # co_states  <- co_states  |> select(all_of(postCol0))
  scNames0   <- scalarData  |> pull(all_of(nameCol0)) |> unique()
  scalarData |> glimpse()
  scalarData <- scNames0 |> map(function(nameX){
    ### Get unique values by year
    dropX <- "groupID"
    dfX   <- scalarData |> 
      select(-any_of(dropX)) |>
      filter_at(c(nameCol0), function(x, y=nameX){x %in% y})
    
    ### Whether do regional
    doRegX <- dfX |> pull(regional) |> unique() |> as.logical()
    if(!doRegX) return(dfX)
    
    ### Unique values
    ### Cross Join, standardizing values
    colsY <- c(typeCol0, nameCol0) |> c("dynamic", "regional") |> c(yrCol0)
    moveY <- colsY     |> get_matches(y=yrCol0, matches=F)
    dfY   <- dfX       |> select(all_of(colsY)) |> distinct()
    dfZ   <- co_states |> cross_join(dfY) |> relocate(all_of(moveY))
    # dfZ |> glimpse(); dfX |> glimpse()
    
    ### Join, standardizing values
    joinZ <- c(colsY) |> c(regCols0)
    dfZ   <- dfZ       |> left_join(dfX, by=joinZ, relationship="many-to-many")
    ### return
    return(dfZ)
  }) |> bind_rows() |> 
    arrange_at(c(typeCol0, nameCol0, postCol0, yrCol0))
  # scalarData |> glimpse()

  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("...Finished reshaping scalar data.", msgN) |> message()
  return(scalarData)
}
