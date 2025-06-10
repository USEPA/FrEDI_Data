reshapeScalarData <- function(
    scalarData = NULL , ### Tibble with scalars data
    frediData  = NULL , ### List of FrEDI configuration data
    silent     = TRUE , ### Level of messaging
    msg0       = "\t"   ### Prefix for messaging
) {
  ###### Messaging ######
  msgN       <- "\n"
  msg1       <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScalarData:"   ) |> message()
  if (!silent) paste0(msg1, "Reshaping scalar data...") |> message()
  
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
  
  ### Replace data with NA values
  scalarData <- scalarData |> mutate(region = region |> str_replace_all("\\.", ""))
  scalarData <- scalarData |> mutate(region = region |> str_replace_all(" ", ""))
  scalarData <- scalarData |> mutate(state  = state  |> na_if("National Total"))
  scalarData <- scalarData |> mutate(region = region |> replace_na("National"))
  scalarData <- scalarData |> mutate(state  = state  |> replace_na("N/A"))
  scalarData <- scalarData |> mutate(postal = postal |> replace_na("N/A"))
  
  
  ### Join scalar data frame with scalar info
  select0    <- c("scalarName", "scalarLabel", "scalarType")
  join0      <- c("scalarName")
  scalarData <- scalarData |> left_join(scalarInfo |> select(all_of(select0)), by=c(join0))
  rm(select0, join0)
  
  
  ### Standardize region name, then select columns
  select0    <- c("scalarType", "scalarLabel", "scalarName", "region") |> c(stateCols0) |> c("year", "value")
  scalarData <- scalarData |> select(all_of(select0))
  rm(select0)
  # scalarData |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeScalarData().", msgN) |> message()
  return(scalarData)
}
