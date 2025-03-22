#' reshapeScaledImpacts
#'
#' @param impacts   Tibble of scaled impacts data (as output from `loadFrediImpacts`)
#' @param frediData List of FrEDI data (as output from `loadFrediData`)
#' @param type0     Model type (e.g., "gcm" or "slr")
#' @param silent    Indicate level of messaging
#' @param msg0      Prefix for messaging
#'
#' @return
#' @export
#'
#' @examples
#' 
reshapeScaledImpacts <- function(
    type0       = "gcm", ### Model type
    impacts     = NULL , ### Tibble with scaled impacts data for type
    # dataList    = NULL , ### List of config data for module (outputs of reshapeFrediConfig())
    controlData = NULL , ### Control tables (output of configureControlTables)
    xCol0       = "modelUnitValue",
    yCol0       = "scaled_impacts",
    valCol0     = "value",
    idCol0      = "scenario_id",
    modCol0     = "model",
    mTypeCol0   = "model_type",
    idCols0     = c("sector", "variant", "impactType", "impactYear", "region", "postal"),
    minYr0      = 2010 , ### Minimum year
    maxYr0      = 2300 , ### Maximum year   
    silent      = TRUE , ### Level of messaging
    msg0        = 0      ### Prefix for messaging
) {
  ### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  
  ### Message user
  # if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Running reshapeScaledImpacts...") |> message()
  if (!silent) msg0 |> get_msgPrefix(newline=F) |> paste0("Reshaping ", type0 |> toupper(), " scaled impacts...") |> message()
  
  ### Values & Columns ----------------
  ### Model Types
  naRepl0    <- "NA|N/A"
  naStr0     <- "NA"
  pattern0   <- " |\\.|\\-"
  replace0   <- ""
  
  ### Types
  type0      <- type0 |> tolower()
  doGcm      <- "gcm" %in% type0
  doSlr      <- "slr" %in% type0
  
  ### Assign Objects ----------------
  ### Assign tables in dataList to object in local environment
  modLvls0   <- controlData[["co_slrCm"]] |> pull(all_of(modCol0))
  
  ### Standardize region
  # co_states  <- co_states |> mutate_at(c(mutate0), function(x){x |> str_replace_all(pattern0, replace0)})
  # co_states  <- co_states |> select(-any_of(stateCol0))
  regCols0   <- c("region", "postal")
  co_states  <- controlData[["co_states"]] |> select(all_of(regCols0))
  

  ### Standardize Region ----------------
  ### Join with state info & relocate columns
  ### Mutate special characters in model
  # select0    <- mainCols0 |> c(regCols0, mTypeCol0, modCol0, xCol0, yCol0)
  drop0      <- c("region", "state")
  # impacts |> glimpse()
  impacts    <- impacts |> select(-any_of(drop0))
  
  ### Join data with states
  names0     <- impacts |> names()
  mutate0    <- c(idCols0, modCol0, mTypeCol0) |> get_matches(y=names0)
  # mutate0    <- c(idCols0, modCol0, mTypeCol0) |> get_matches(y=names0)
  join0      <- regCols0 |> get_matches(y=names0)
  # "gotHere1" |> print(); impacts |> nrow() |> print()
  impacts    <- impacts  |> 
    # mutate_at(c(mutate0), as.character) |>
    mutate_at(c(idCols0  |> get_matches(y=names0)), function(x, y=naRepl0, z=naStr0){
      case_when(x |> str_detect(y) ~ NA, .default=x) |> replace_na(z)
    }) |> 
    mutate_at(c(modCol0), str_replace_all, pattern0, replace0) |>
    left_join(co_states, by=join0) |>
    relocate(all_of(valCol0), .after=all_of(xCol0)) |> 
    rename_at(c(valCol0), ~yCol0)
  rm(join0)
  # "got here" |> print()
  # "gotHere2" |> print(); impacts |> nrow() |> print()
  
  ### Format SLR Values ----------------
  if(doSlr) {
    ### Zero out values 
    impacts  <- impacts |> 
      # mutate_at(c(modCol0), factor, modLvls0) |>
      zeroSlrValues(
        modCol0 = modCol0,
        yCol0   = yCol0,
        min0    = 30, 
        new0    = 0 
      ) |> extend_data(
        to0   = maxYr0, ### Year to extend to
        xCol0 = xCol0
      ) ### End zeroSlrValues
  } ### End if doSlr
  # "gotHere6" |> print(); impacts |> nrow() |> print()
  
  ### Standardize Data ----------------
  impacts    <- impacts |> reshape_modelImpacts(
    type0     = type0, ### Model Type
    xCol0     = xCol0,
    yCol0     = yCol0,
    idCol0    = idCol0,
    modCol0   = modCol0,
    idCols0   = idCols0,
    modStr0   = "Interpolation"
  ) ### End reshape_modelImpacts
  
  
  ### Return ----------------
  ### Return the list of dataframes
  if (!silent) msg1 |> get_msgPrefix(newline=F) |> paste0("...Finished reshaping ", type0 |> toupper(), " scaled impacts...", msgN) |> message()
  return(impacts)
}
