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
reshapeScaledImpacts <- function(
    impacts    = NULL , ### Tibble with scalars data
    frediData  = NULL , ### List of data (e.g., as returned from FrEDI_Data::loadData())
    type0      = "gcm", ### Model type
    minYr0     = 2090 , ### Minimum year
    maxYr0     = 2300 , ### Maximum year   
    silent     = TRUE , ### Level of messaging
    msg0       = ""     ### Prefix for messaging
) {
  ###### Messaging ######
  msgN       <- "\n"
  msg1       <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "Running reshapeScaledImpacts...") |> message()
  if (!silent) paste0(msg1, "Reshaping ", type0 |> toupper(), " scaled impacts...") |> message()
  
  ###### Values & Columns ######
  ### Model Types
  naStr0     <- "NA"
  pattern0   <- " |\\.|\\-"
  replace0   <- ""
  type0      <- type0 |> tolower()
  doGcm      <- "gcm" %in% type0
  doSlr      <- "slr" %in% type0
  
  ### Columns
  mainCols0  <- c("sector", "variant", "impactType", "impactYear")
  # regCols0   <- c("region", "state", "postal")
  regCols0   <- c("region", "postal")
  mTypeCol0  <- "model_type"
  modCol0    <- "model"
  yrCol0     <- "year"
  xCol0      <- case_when(doGcm ~ "modelUnitValue", .default=yrCol0)
  yCol0      <- "scaled_impacts"
  valCol0    <- "value"
  idCol0     <- "scenario_id"
  
  
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  dfSectInfo <- frediData[["co_sectorsInfo"]] |> filter_at(c(mTypeCol0), function(x, y=type0){(x |> tolower()) %in% y})
  co_sectors <- frediData[["co_sectors"]] |> filter_at(c(mTypeCol0), function(x, y=type0){(x |> tolower()) %in% y})
  co_models  <- frediData[["co_models" ]] |> filter_at(c(mTypeCol0), function(x, y=type0){(x |> tolower()) %in% y})
  co_states  <- frediData[["co_states" ]]
  # impacts |> pull(model) |> unique() |> print()
  # impacts |> glimpse()
  # dfSectInfo |> glimpse()
  # co_sectors |> glimpse()
  # co_models |> glimpse()
  
  ### Standardize region
  co_states  <- co_states |> 
    # mutate_at(c(mutate0), function(x){x |> str_replace_all(" ", "")})|> 
    # mutate_at(c(mutate0), function(x){x |> str_replace_all("\\.", "")})
    mutate_at(c(mutate0), function(x){x |> str_replace_all(pattern0, replace0)})
  

  ###### Standardize Region ######
  ### Join with state info & relocate columns
  ### Mutate special characters in model
  # select0    <- mainCols0 |> c(regCols0, mTypeCol0, modCol0, xCol0, yCol0)
  join0      <- impacts |> names() |> get_matches(y=regCols0)
  impacts    <- impacts |> 
    mutate_at(c(mTypeCol0), function(x, y=type0){y})
    # mutate_at(c(modCol0), function(x){x |> str_replace_all(" ", "")}) |> 
    # mutate_at(c(modCol0), function(x){x |> str_replace_all("\\.|\\-", "")}) |> 
    mutate_at(c(mutate0), function(x){x |> str_replace_all(pattern0, replace0)}) |>
    mutate_at(c(mainCols0, modCol0, mTypeCol0), as.character) |> 
    mutate_at(c(mainCols0), replace_na, naStr0) |> 
    left_join(co_states |> select(all_of(regCols0)), by=join0) |> 
    rename_at(c(valCol0), ~yCol0) |>
    relocate(all_of(valCol0), .after=all_of(regCols0))
  rm(join0)
  
  
  ###### Filter to Models & Sectors ######
  # ### Filter to unique models and sectors
  # models0   <- co_models  |> pull(model ) |> unique()
  # sectors0  <- co_sectors |> pull(sector) |> unique()
  # # impacts |> glimpse()
  # # impacts$model |> unique() |> sort() |> print(); filter0 |> sort() |> print()
  # # filter0 |> print(); impacts |> pull(model) |> unique() |> print()
  # impacts    <- impacts |> filter(model  %in% models0)
  # impacts    <- impacts |> filter(sector %in% sectors0)
  # rm(filter0, filter1)
  
  ###### Format SLR Values ######
  if(doSlr) {
    ### Zero out values 
    impacts  <- impacts |> (function(dfX, minX=30, newX=0, unitX="cm"){
      dfX0 <- dfX |> 
        filter_at(c(modCol0), function(x, y=minX |> paste0(unitX)){x %in% y}) |>
        mutate_at(c(modCol0), function(x, y=newX |> paste0(unitX)){x %in% y}) |>
        mutate_at(c(yCol0  ), function(x, y=newX){y})
      dfX  <- dfX |> 
        filter_at(c(modCol0), function(x, y=minX |> paste0(unitX)){!(x %in% y)}) |>
        bind_rows(dfX0)
      return(dfX)
    })
  } ### End if doSlr
  
  ###### Select & Arrange ######
  ### Replace NA values in impactYear, impactType
  select0    <- mainCols0 |> c(regCols0, mTypeCol0, modCol0, xCol0, yCol0)
  sort0      <- select0   |> get_matches(y=select0, matches=F)
  impacts    <- impacts |>
    select(all_of(select0)) |>
    arrange_at(vars(sort0))
  
  ##### Select columns ######
  # col0       <- doGcm  |> ifelse("modelUnitValue", "year")
  # cols0      <- c("sector", "variant", "impactType", "impactYear", "modelType", "model")
  # cols0      <- cols0   |> c("region") |> c(stateCols0) |> c(col0)
  # select0    <- cols0   |> c("scaled_impacts")
  # impacts    <- impacts |> 
  #   select(all_of(select0))
  # impacts    <- impacts |> arrange_at(vars(cols0))
  
  ###### Standardize Data ######
  impacts    <- impacts |> standardize_scaledImpacts(
    df1    = dfSectInfo,
    maxYr0 = maxYr0, 
    mType0 = mType0, 
    xCol0  = xCol0,
    yCol0  = yCol0,
    idCol0 = idCol0
  ) ### End standardize_scaledImpacts
  
  # ### Add model levels to SLR
  # if(doSlr) {
  #   ### Arrange and group
  #   impacts    <- impacts |> format_slrImpacts(
  #     modLvls0 = co_slrCm |> pull(all_of(modCol0)), 
  #     yCol0    = yCol0,
  #     group0   = mainCols0 |> c(regCols0),
  #     yrCol0   = yrCol0,
  #     modCol0  = modCol0,
  #     idCol0   = idCol0,
  #     modStr0  = "Interpolation"
  #   ) ### End format_slrImpacts
  # } ### End if(doSlr)
  
  
  ###### Return ######
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeScaledImpacts().", msgN) |> message()
  return(impacts)
}
