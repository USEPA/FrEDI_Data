#' reshapeData
#'
#' @param impacts Tibble of scaled impacts data (as output from `loadFrediImpacts`)
#' @param frediData List of FrEDI data (as output from `loadFrediData`)
#' @param type0 Model type (e.g., "gcm" or "slr")
#' @param silent Indicate level of messaging
#' @param msg0 Prefix for messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeScaledImpacts <- function(
    impacts    = NULL,   ### Tibble with scalars data
    frediData  = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    type0      = "gcm",  ### Model type
    silent     = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScaledImpacts:") |> message()
  if (!silent) paste0(msg1, "Reshaping ", type0 |> toupper(), " scaled impacts...") |> message()
  
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  co_sectors <- frediData[["co_sectors"]] |> filter((modelType |> tolower()) == type0)
  co_models  <- frediData[["co_models" ]] |> filter((modelType |> tolower()) == type0)
  co_states  <- frediData[["co_states" ]]
  
  # impacts |> names() |> print()
  # co_sectors |> names() |> print()
  # co_models |> names() |> print()
  
  ###### Values ######
  type0      <- type0 |> tolower()
  do_gcm     <- "gcm" %in% type0
  do_slr     <- "slr" %in% type0

  ###### Standardize Region ######
  ### Join with state info & relocate columns
  stateCols0 <- c("state", "postal")
  select0 <- "region" |> c(stateCols0)
  after0  <- c("value")
  join0   <- stateCols0
  impacts <- impacts |> left_join(co_states |> select(all_of(select0)), by=c(join0))
  impacts <- impacts |> relocate(all_of(after0), .after=all_of(select0))
  rm(select0, after0, join0)
  
  ### Standardize region name & rename column
  mutate0 <- c("region")
  rename0 <- c("region", "value")
  # rename1 <- c("region_id", "scaled_impacts")
  rename1 <- c("region", "scaled_impacts")
  impacts <- impacts |> mutate_at(c(mutate0), function(x){gsub(" ", "", x)})
  impacts <- impacts |> rename_at(c(rename0), ~rename1)
  rm(mutate0, rename0, rename1)
  
  ###### Filter to Models & Sectors ######
  ### Filter to specific models and sectors
  filter0 <- co_models  |> pull(model_id ) |> unique()
  filter1 <- co_sectors |> pull(sector_id) |> unique()
  # impacts |> glimpse()
  # filter0 |> print(); impacts |> pull(model) |> unique() |> print()
  impacts <- impacts |> filter(model  %in% filter0)
  impacts <- impacts |> filter(sector %in% filter1)
  rm(filter0, filter1)
  
  ###### Zero Out SLR Values ######
  if(do_slr) {
    ### Zero out values
    impacts0 <- impacts  |> filter(model == "30cm")
    impacts0 <- impacts0 |> mutate(model =  "0cm" )
    impacts0 <- impacts0 |> mutate(scaled_impacts = 0)
    
    impacts  <- impacts  |> filter(model != "0cm")
    impacts  <- impacts0 |> rbind(impacts)
  } ### End if do_slr
  
  ###### Rename column ######
  # if(do_gcm) {
  #   rename0  <- c("modelType")
  #   renameTo <- c("model_type")
  #   impacts  <- impacts   |> rename_at(c(rename0), ~renameTo)
  # }
 
  ###### Adjust values ######
  ### Replace NA values in impactYear, impactType
  mutate0 <- c("variant", "impactType", "impactYear", "model")
  impacts <- impacts |> mutate_at(c(mutate0), as.character)
  impacts <- impacts |> mutate(impactType = impactType |> as.character() |> replace_na("NA"))
  impacts <- impacts |> mutate(impactYear = impactYear |> as.character() |> replace_na("NA"))
  
  ### Standardize models
  if(do_gcm) {
    levels0 <- co_models |> pull(model_label) |> unique()
    labels0 <- co_models |> pull(model_id   ) |> unique()
    impacts <- impacts   |> mutate(model_id = model |> factor(levels0, labels0) |> as.character())
  } else if(do_slr) {
    levels0 <- co_models |> pull(model_id   ) |> unique()
    labels0 <- co_models |> pull(model_label) |> unique()
    impacts <- impacts   |> mutate(model_id = model)
    impacts <- impacts   |> mutate(model    = model |> factor(levels0, labels0) |> as.character())
  } ### End if(do_gcm)
  rm(levels0, labels0)
  
  ### Add model type
  drop0   <- c("model_type")
  impacts <- impacts |> select(-any_of(drop0))
  impacts <- impacts |> mutate(modelType = type0 |> as.character())
  
  
  ##### Select columns ######
  col0    <- do_gcm |> ifelse("modelUnitValue", "year")
  cols0   <- c("sector", "variant", "impactType", "impactYear", "modelType", "model", "model_id")
  cols0   <- cols0 |> c("region") |> c(stateCols0) |> c(col0)
  select0 <- cols0 |> c("scaled_impacts")
  impacts <- impacts |> select(all_of(select0))
  impacts <- impacts |> arrange_at(vars(cols0))
  
  ###### Return ######
  ### Return the list of dataframes
  # if (!silent) paste0("\n") |> message()
  return(impacts)
}