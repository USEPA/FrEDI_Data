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
    scalarData = NULL , ### Tibble with scalars data
    frediData  = NULL , ### List of FrEDI configuration data
    minYr0     = 2010,
    maxYr0     = 2300,
    yrCol0     = "year",
    valCol0    = "value",
    natStr0    = "US",
    # dropCols0  = "state", ### Drop state
    silent     = TRUE , ### Level of messaging
    msg0       = "\t" ,  ### Prefix for messaging
) {
  ### Messaging ----------------
  msgN       <- "\n"
  msg1       <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScalarData:"   ) |> message()
  if (!silent) paste0(msg1, "Reshaping scalar data...") |> message()
  
  ### Assign Objects ----------------
  ### Assign tables in dataList to object in local environment
  # frediData |> names() |> print()
  co_states  <- frediData[["co_states"    ]]
  scalarInfo <- frediData[["co_scalarInfo"]]
  # co_states  |> glimpse(); scalarInfo |> glimpse()

  ### Join with State/Region Info ----------------
  ### Add region to scalar data frame
  # scalarDataframe |> glimpse(); scalarData |> glimpse()
  # regCols0   <- c("region", "state", "postal")
  # regCols0   <- c("region", "postal")
  # join0      <- scalarData |> names() |> get_matches(y=regCols0)
  # stateCol0  <- c("state")
  postCol0   <- "postal"
  stateCol0  <- "state"
  # naStr0     <- "US"
  co_states  <- co_states  |> select(-any_of(stateCol0)) 
  scalarData <- scalarData |> 
    select(-any_of(dropCols0)) |>
    mutate_at(c(mutate0), replace_na, naStr0) |>
    left_join(by=join0)
  # rm(join0)
  # 
  # ### Replace data with NA values
  # # naStr0     = c("")
  # scalarData <- scalarData |> 
  #   mutate(region = region |> str_replace_all(" |\\.", "")) |> 
  #   # mutate(state  = state  |> na_if("National Total")) |> 
  #   mutate(region = region |> replace_na("National")) |> 
  #   mutate(state  = state  |> replace_na("N/A")) |> 
  #   mutate(postal = postal |> replace_na("N/A"))
  # 
  # ### Join scalar data frame with scalar info
  # ### Select columns and arrange
  # # select0    <- c("scalarType", "scalarName", "scalarLabel")
  # # select1    <- select0 |> c(regCols0, yrCol0, valCol0)
  # # sort0      <- select0 |> get_matches(y=valCol0, matches=F)
  # # join0      <- scalarData |> names() |> get_matches(y=select0)
  # # scalarData <- scalarData |> 
  # #   left_join(scalarInfo |> select(all_of(select0)), by=join0) |>
  # #   select(all_of(select1)) |>
  # #   arrange_at(c(sort0))
  # # rm(select0, join0)
  # # scalarData |> names() |> print()
  
  ### Format scalars
  scalarData <- scalarData |> fun_formatScalars(
    df1      = scalarInfo, ### rDataList$co_scalarInfo,
    years0   = minYr0:maxYr0,
    natStr0  = natStr0,
    rule0    = 2
  ) ### End fun_formatScalars
  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeScalarData().", msgN) |> message()
  return(scalarData)
}
