#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeScalarData <- function(
    scalarData = NULL,   ### Tibble with scalars data
    frediData  = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent     = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScalarData:") |> message()
  
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  # frediData |> names() |> print()
  co_states  <- frediData[["co_states"    ]]
  scalarInfo <- frediData[["co_scalarInfo"]]
  # co_states  |> glimpse(); scalarInfo |> glimpse()

  
  ###### Join with State/Region Info ######
  ### Add region to scalar data frame
  # scalarDataframe |> glimpse()
  # scalarData |> glimpse()
  stateCols0 <- c("state", "postal")
  select0    <- c("region") |> c(stateCols0)
  join0      <- stateCols0
  scalarData <- scalarData |> left_join(co_states |> select(all_of(select0)), by=c(join0))
  rm(select0, join0)
  
  ### Join scalar data frame with scalar info
  select0    <- c("scalarName", "scalarLabel", "scalarType")
  join0      <- c("scalarName")
  scalarData <- scalarData |> left_join(scalarInfo |> select(all_of(select0)), by=c(join0))
  rm(select0, join0)
  
  
  ### Standardize region name, then select columns
  select0    <- c("scalarType", "scalarLabel", "scalarName", "region") |> c(stateCols0) |> c("year", "value")
  scalarData <- scalarData |> mutate(region = region |> str_replace(" ", ""))
  scalarData <- scalarData |> select(all_of(select0))
  rm(select0)
  
  ###### Return ######
  ### Return the list of dataframes
  if (!silent) paste0("\n") |> message()
  return(scalarData)
}