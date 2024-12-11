#' reshapeScenariosData
#'
#' @param scenarioData List with scenario data (as output from `loadScenarioData`)
#' @param silent Indicate level of messaging
#' @param msg0 Prefix for messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeScenarioData <- function(
    scenarioData = NULL, ### List with scenario data
    silent       = TRUE, ### Level of messaging
    msg0         = "\t"  ### Prefix for messaging
) {
  ###### Messaging ######
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScenarioData:"   ) |> message()
  if (!silent) paste0(msg1, "Reshaping scenario data...") |> message()
  
  ###### Get Unique Values for Data ######
  ### Add region to scalar data frame
  gcamData   <- scenarioData[["gcamData"     ]]
  gdpData    <- scenarioData[["gdpData"      ]]
  popData    <- scenarioData[["popData"      ]]
  ratiosData <- scenarioData[["popRatiosData"]]
  # ratiosData |> glimpse()
  
  ### Replace data with NA values
  gcamData   <- gcamData   |> distinct()
  gdpData    <- gdpData    |> distinct()
  popData    <- popData    |> distinct()
  ratiosData <- ratiosData |> distinct()
  # ratiosData |> glimpse()
  
  ### Format data
  popData    <- popData    |> mutate(region = region |> str_replace("\\.", " "))
  ratiosData <- ratiosData |> mutate(region = region |> str_replace("\\.", " "))
  
  ### Update in List
  scenarioData[["gcamData"     ]] <- gcamData
  scenarioData[["gdpData"      ]] <- gdpData
  scenarioData[["popData"      ]] <- popData
  scenarioData[["popRatiosData"]] <- ratiosData
  # scenarioData[["popRatiosData"]] |> dim() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeScenarioData.", msgN) |> message()
  return(scenarioData)
}