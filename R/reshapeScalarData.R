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

  ### Join with State/Region Info ----------------
  ### Add region to scalar data frame
  # "got here0" |> print()
  regCol0    <- "region"
  postCol0   <- "postal"
  stateCol0  <- "state"
  regCols0   <- c(regCol0, postCol0)
  # naStr0     <- "US"
  # select0    <- c("area", "region", "state", "postal", "fips")
  # "got here1" |> print()
  # select0    <- c("region", "state", "postal", "fips")
  co_states  <- controlData[["co_states"]] |> select(all_of(regCols0))
  # co_states |> glimpse()
  # scalarData |> glimpse()
  join0      <- co_states  |> names() |> get_matches(y=scalarData |> names())
  move0      <- c("scalarName") |> c(regCols0, yrCol0)
  scalarData <- scalarData |> 
    select(-any_of(regCol0), any_of(stateCol0)) |>
    mutate_at(c(postCol0), replace_na, natStr0) |>
    left_join(co_states, by=postCol0) |> 
    relocate(any_of(move0))
  # "got here2" |> print()
  # scalarData |> glimpse()
  
  ### Format scalars
  scalarData <- scalarData |> fun_formatScalars(
    df1      = scalarInfo, 
    years0   = minYr0:maxYr0,
    natStr0  = natStr0,
    rule0    = 2
  ) ### End fun_formatScalars
  # "got here3" |> print()
  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("...Finished reshaping scalar data.", msgN) |> message()
  return(scalarData)
}
