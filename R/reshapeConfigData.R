#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeConfigData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent   = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeConfigData:") |> message()
  
  
  ###### Assign Objects ######
  # dataList0 <- dataList
  ### Assign tables in dataList to object in local environment
  listNames <- dataList |> names()
  # dataList |> names() |> print()
  for(name_i in listNames) {name_i |> assign(dataList[[name_i]]); rm(name_i)}
  # dataList |> list2env(envir = environment())
  

  ###### Columns  ######
  ### Region and state level columns to use
  stateCols0 <- c("state", "postal")
  
  
  ###### Modify Tables and Update in List ######
  ###### ** 1. Sectors     ######
  ### Filter to those tables to include
  ### Make a copy of the sectors list to include variants
  drop0           <- c("include", "variants", "impactYears", "impactTypes")
  co_sectorsRef   <- co_sectors
  co_sectors      <- co_sectors |> select(-all_of(drop0))
  ### Update values in list, drop intermediate variables
  dataList[["co_sectorsRef"]] <- co_sectorsRef
  dataList[["co_sectors"   ]] <- co_sectors
  rm(drop0)
  
  
  ###### ** 2. Misc ######
  ### No changes to variants
  ### No changes to model types
  ### No changes to input scenario info
  
  
  
  ###### ** 3. Impact Types Info ######
  ### Drop damage adjustment names (present in variants)
  drop0          <- c("damageAdjName")
  co_impactTypes <- co_impactTypes |> select(-all_of(drop0)) # ; co_impactTypes |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_impactTypes"]] <- co_impactTypes
  rm(drop0)
  
  
  
  ###### ** 4. Impact Years ######
  ### Load data and gather the data by impact year levels
  idCols0     <- c("sector_id")
  impYearLvls <- co_impactYears |> select(-all_of(idCols0)) |> names()
  co_impactYears <- co_impactYears |> (function(df0, mutate0=impYearLvls, cols0=idCols0){
    ### Convert to character
    df0     <- df0 |> mutate_at(c(mutate0), as.character)
    rm(mutate0)
    
    ### Reshape impact years: gather the data by impact year levels
    df0     <- df0 |> pivot_longer(
      cols      = -all_of(cols0),
      names_to  = "impactYear_id", 
      values_to = "impactYear"
    ) ### End pivot_longer
    
    ### Mutate values
    mutate0 <- c("impactYear_id") 
    df0     <- df0 |> mutate_at(c(mutate0), na_if     , "X3")
    df0     <- df0 |> mutate_at(c(mutate0), replace_na, "NA")
    
    ### Drop values with missing `impactYear` and then drop column
    drop0 <- "impactYear"
    df0   <- df0 |> filter(!(impactYear |> is.na()))
    df0   <- df0 |> select(-all_of(drop0))
    
    ### Get impact year label
    df0   <- df0 |> mutate(impactYear_label = impactYear_id |> str_replace("NA", "N/A"))
    
    ### Return
    return(df0)
  })() 
  
  
  ### Create info on impact year levels
  select0 <- c("impactYear_id", "impactYear_label")
  co_impYearLvls <- co_impactYears |> (function(df0, cols0=select0){
    df0 <- df0 |> select(all_of(cols0))
    df0 <- df0 |> unique()
    return(df0)
  })() 
  
  ### Update/add tables in/to list, drop intermediate values
  dataList[["co_impactYears"     ]] <- co_impactYears
  dataList[["co_impYearLvls"]] <- co_impYearLvls
  rm(idCols0, impYearLvls)
  
  

  ###### ** 5. co_models ######
  ### Combine with model types and update data list
  join0          <- c("modelType")
  co_models      <- co_models     |> left_join(co_modelTypes, by = join_by(modelType == modelType_id)) 
  # co_models |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_models"]] <- co_models
  rm(join0)
  
  
  
  ###### ** 6. co_regions ######
  ### Combine and add to data list...also a copy with info on national data
  ### Update in list
  co_regions <- co_regions |> mutate(region = region_id)
  dataList[["co_regions"]] <- co_regions
  
  
  
  ###### ** 7. SLR Scenario Info ######
  ### Gather slr_cm columns
  # slr_cm |> names() |> print()
  idCols0 <- c("year")
  slr_cm  <- slr_cm |> (function(df0, df1=co_models, cols0=idCols0){
    ### Gather slr_cm columns
    df0    <- df0 |> pivot_longer(
      cols      = -all_of(cols0), 
      names_to  = "model",
      values_to = "driverValue"
    ) ### End pivot_longer
    
    ### Add model type
    df0    <- df0 |> mutate(model_type = "slr")
    
    ### Zero out values
    df0_0cm <- df0     |> filter(model == "30cm")
    df0_0cm <- df0_0cm |> mutate(model   = "0cm")
    df0_0cm <- df0_0cm |> mutate(driverValue = 0)
    df0_0cm <- df0_0cm |> rbind(df0)
    
    ### Return
    return(df0)
  })()
  
  
  ### Update in data list, drop intermediate values
  # slr_cm |> names() |> print()
  dataList[["slr_cm"]] <- slr_cm
  rm(idCols0)
  # dataList |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  # dataList0[["frediData"]] <- dataList
  # return(dataList0)
  if (!silent) paste0("\n") |> message()
  return(dataList)
}