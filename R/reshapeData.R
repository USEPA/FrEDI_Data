#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    byState  = FALSE,  ### Whether to run for state-level data (deprecated)
    silent   = TRUE    ### Level of messaging
) {
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  # listNames <- dataList |> names()
  # listNames |> print()
  # for(name_i in listNames) {name_i |> assign(dataList[[name_i]])}
  # dataList |> list2env(envir = environment())
  
  ### Ensure all dataframes are tibbles
  names0   <- dataList |> names()
  for(list_i in listNames) { for(name_j in list_i) { name_j |> assign(list_i[[name_j]]); rm(name_j)}; rm(list_i) }
  # for(name_i in names0) {
  #   data_i <- dataList[[name_i]]
  #   isDf_i <- data_i |> is.data.frame()
  #   if(isDf_i) data_i <- data_i |> as_tibble()
  #   dataList[[name_i]] <- data_i
  #   rm(name_i, data_i, isDf_i)
  # } ### End for(name_i in names0) 
  
  ###### By State  ######
  ### Region and state level columns to use
  # if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  stateCols0 <- c("state", "postal")
  
  
  ###### Modify Tables and Update in List ######
  ###### ** 1. Sectors     ######
  ### Filter to those tables to include
  ### Make a copy of the sectors list to include variants
  drop0           <- c("include", "variants", "impactYears", "impactTypes")
  co_sectorsRef   <- co_sectors
  co_sectors      <- co_sectors |> select(-all_of(drop0))
  ### Update values in list, drop intermediate variables
  dataList[["co_sectors"     ]] <- co_sectors
  dataList[["co_sectorsRef"  ]] <- co_sectorsRef
  # co_stateSectors <- co_sectors |> filter(byState == 1)
  # dataList[["co_stateSectors"]] <- co_stateSectors
  rm(drop0)
  
  
  ###### ** 2. Misc ######
  ### No changes to variants
  ### No changes to model types
  ### No changes to input scenario info
  
  
  
  ###### ** 3. Impact Years ######
  ### Load data and gather the data by impact year levels
  ### Levels for factoring are impact year levels: `N/A`, `2010`, `2090`, Interpolate
  ### Replace special characters in impact year levels
  idCols0 <- c("sector_id")
  mutate0 <- co_impactYears |> select(-all_of(idCols0)) |> names()
  
  ### Create info on impact year levels
  co_impactYearLevels <- tibble(impactYear_label = mutate0)
  co_impactYearLevels <- co_impactYearLevels |> mutate(impactYear_id    = gsub("/", "", impactYear_label))
  co_impactYearLevels <- co_impactYearLevels |> mutate(impactYear_excel = gsub("NA", "", impactYear_id))
  co_impactYears      <- co_impactYears |> mutate_at(c(mutate0), as.character)
  
  ### Reshape impact years: gather the data by impact year levels
  co_impactYears <- co_impactYears |> pivot_longer(
    cols      = -all_of(idCols0),
    names_to  = "impactYear_label", 
    values_to = "impactYear2"
  ) ### End pivot_longer
  
  ### Drop values with missing `impactYear2`
  co_impactYears <- co_impactYears |> filter(!(impactYear2 |> is.na()))
  
  ### Join impact year info and impact year levels
  drop0   <- c("impactYear2")
  join0   <- c("impactYear_label")
  co_impactYears <- co_impactYears |> select(-all_of(drop0))
  co_impactYears <- co_impactYears |> left_join(co_impactYearLevels, by=c(join0))
  # co_impactYears |> glimpse()
  
  ### Update/add tables in/to list, drop intermediate values
  dataList[["co_impactYears"     ]] <- co_impactYears
  dataList[["co_impactYearLevels"]] <- co_impactYearLevels
  rm(drop0, join0, idCols0, mutate0)
  
  
  ###### ** 4. Impact Types Info ######
  ### Drop damage adjustment names (present in variants)
  drop0          <- c("damageAdjName")
  co_impactTypes <- co_impactTypes |> select(-all_of(drop0)) # ; co_impactTypes |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_impactTypes"]] <- co_impactTypes
  rm(drop0)
  
  
  ###### ** 5. co_models ######
  ### Combine with model types and update data list
  join0          <- c("modelType")
  co_modelTypes2 <- co_modelTypes |> rename(modelType = modelType_id)
  co_models      <- co_models     |> left_join(co_modelTypes2, by = c(join0)) # ; co_models |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_models"]] <- co_models
  rm(join0, co_modelTypes2)
  
  
  ###### ** 6. co_regions ######
  ### Combine and add to data list...also a copy with info on national data
  co_regions <- co_regions |> mutate(region = region_dot)
  ### Update in list
  dataList[["co_regions"]] <- co_regions
  
  
  ###### ** 7. df_sectorsInfo ######
  ### Join with co_variants, co_impactTypes, co_impactYears
  join0   <- c("sector_id")
  drop0   <- c("variant_label")
  drop1   <- c("impactType_label", "impactType_description")
  drop2   <- c("impactYear_label")
  df_sectorsInfo <- co_sectors     |> left_join(co_variants    |> select(-all_of(drop0)), by=c(join0))
  df_sectorsInfo <- df_sectorsInfo |> left_join(co_impactTypes |> select(-all_of(drop1)), by=c(join0), relationship = "many-to-many")
  df_sectorsInfo <- df_sectorsInfo |> left_join(co_impactYears |> select(-all_of(drop2)), by=c(join0), relationship = "many-to-many")
  ### Rename values
  rename0 <- c("sector", "variant", "impactYear", "impactType") |> paste("_id")
  rename1 <- gsub("_id", "", c(rename0))
  df_sectorsInfo <- df_sectorsInfo |> rename_at(c(rename0), ~rename1) 
  ### Update in list, drop intermediate values
  # df_sectorsInfo |> glimpse()
  dataList[["df_sectorsInfo"]] <- df_sectorsInfo
  rm(join0, drop0, drop1, drop2, rename0, rename1)
  
  
  ###### ** 8. Temperature Scenario ######
  ### ** Initialize Temperature Scenario
  ### Add temp_C_conus
  co_defaultTemps <- co_defaultTemps |> mutate(temp_C_conus = temp_C_global |> FrEDI::convertTemps(from="global"))
  ### Update in list
  dataList[["co_defaultTemps"]] <- co_defaultTemps
  
  
  ###### ** 9. Socioeconomic Scenario ######
  ### Gather the columns from the default scenario and update data list
  if (byState) {
    ### Rename and gather
    select0   <- stateCols0 |> c("fips", "proportion_2100")
    co_defaultScenario <- co_defaultScenario |> pivot_longer(
      cols      = -all_of(select0), 
      names_to  = "year", 
      values_to = "state_pop"
    ) ### End pivot_longer
    ### Mutate names to years
    mutate0   <- c("year")
    str0      <- "proj_"
    co_defaultScenario <- co_defaultScenario |> mutate_at(c(mutate0), function(y){gsub(str0, "", y)})
    co_defaultScenario <- co_defaultScenario |> mutate_at(c(mutate0), as.numeric)
    ### - Join with state info & standardize region name
    join0     <- stateCols0 |> c("fips")
    mutate1   <- c("region")
    co_defaultScenario <- co_defaultScenario |> left_join(co_states, by=c(join0))
    co_defaultScenario <- co_defaultScenario |> mutate_at(c(mutate1), function(x){gsub(" ", ".", x)})
    # ### Save a copy of state proportions & then select columns
    # select1            <- "region" |> c(select0)
    # co_statePopRatios  <- co_defaultScenario |> select(all_of(select1))
    # ### Update in list, drop intermediate values
    # dataList[["co_statePopRatios"]] <- co_statePopRatios
    rm(mutate0, str0, join0, mutate1)
    
    ### Join with GDP default scenario & relocate names
    join0              <- c("year")
    select2            <- "region" |> c(stateCols0) |> c("state_pop") |> c(join0)
    co_defaultScenario <- co_defaultScenario |> select(all_of(select2))
    co_defaultScenario <- co_defaultScenario |> left_join(gdp_default, by=c(join0))
    co_defaultScenario <- co_defaultScenario |> relocate(c("gdp_usd"), .after="state_pop")
    rm(join0, select2)
  } else{
    ### Pivot longer
    co_defaultScenario <- co_defaultScenario |> pivot_longer(
      cols      = -c("year", "gdp_usd"),
      names_to  = "region", 
      values_to = "reg_pop"
    ) ### End pivot_longer
    ### Standardize region name
    str0               <- "pop_"
    mutate0            <- "region"
    co_defaultScenario <- co_defaultScenario |> mutate_at(c(mutate0), function(x){gsub(str0, "", x)})
    rm(str0, mutate0)
  } ### End else
  ### Update in list
  dataList[["co_defaultScenario"]] <- co_defaultScenario
  
  
  ###### ** 10. Scalar Info ######
  ### state scalars
  if (byState) {
    ### Add region to scalar data frame
    # scalarDataframe |> glimpse()
    select0         <- c("region") |> c(stateCols0)
    join0           <- stateCols0
    scalarDataframe <- scalarDataframe |> left_join(co_states |> select(all_of(select0)), by=c(join0))
    ### Join scalar data frame with scalar info
    select1         <- c("scalarName", "scalarLabel", "scalarType")
    join1           <- c("scalarName")
    scalarDataframe <- scalarDataframe |> left_join(co_scalarInfo |> select(all_of(select1)), by=c(join1))
    rm(select0, join0); rm(select1, join1)
  } ### End if (byState)
  
  ### Standardize region name, then select columns
  select0 <- c("scalarType", "scalarLabel", "scalarName", "region") |> c(stateCols0) |> c("year", "value")
  mutate0 <- "region"
  scalarDataframe <- scalarDataframe |> mutate_at(c(mutate0), function(x){gsub(" ", ".", x)})
  scalarDataframe <- scalarDataframe |> select(all_of(select0))
  ### Update in list, drop intermediate values
  dataList[["scalarDataframe"]] <- scalarDataframe
  rm(select0, mutate0)

  
  ###### ** 11. GCM Scaled Impacts ######
  if (byState) {
    ### Join with state info & relocate columns
    select0 <- "region" |> c(stateCols0)
    after0  <- c("value")
    join0   <- stateCols0
    
    ### - Join with state info
    data_scaledImpacts <- data_scaledImpacts |> left_join(co_states |> select(all_of(select0)), by=c(join0))
    data_scaledImpacts <- data_scaledImpacts |> relocate(all_of(after0), .after=all_of(select0))
    rm(select0, after0, join0)
    
    ### Standardize region name
    mutate0 <- "region"
    data_scaledImpacts <- data_scaledImpacts |> mutate_at(c(mutate0), function(x){gsub(" ", ".", x)})
    rm(mutate0)
    
    ### Standardize region name & rename column
    mutate0 <- c("region")
    rename0 <- c("region", "value")
    rename1 <- c("region_dot", "scaledImpact")
    data_scaledImpacts <- data_scaledImpacts |> mutate_at(c(mutate0), function(x){gsub(" ", ".", x)})
    data_scaledImpacts <- data_scaledImpacts |> rename_at(c(rename0), ~rename1)
    rm(mutate0, rename0, rename1)
  } else{
    ### Reshape data_scaledImpacts (move columns with regional values to rows)
    gather0 <- co_regions[["region_dot"]]
    data_scaledImpacts <- data_scaledImpacts |> pivot_longer(
      cols      = all_of(gather0),
      names_to  = "region_dot", 
      values_to = "scaledImpact"
    ) ### End pivot_longer()
    rm(gather0)
  } ### End else(byState)
  
  ### Remove empty rows and GCM and SLR Averages
  ### Update in list, remove intermediate variables
  filter0 <- co_models [["model_id" ]] |> unique()
  filter1 <- co_sectors[["sector_id"]] |> unique()
  # data_scaledImpacts |> glimpse()
  # filter0 |> print(); data_scaledImpacts$model |> unique() |> print()
  data_scaledImpacts <- data_scaledImpacts |> filter(model  %in% filter0)
  data_scaledImpacts <- data_scaledImpacts |> filter(sector %in% filter1)
  ### Update in list, drop intermediate values
  dataList[["data_scaledImpacts"]] <- data_scaledImpacts
  rm(filter0, filter1)
  
  
  ###### ** 12. SLR Scenario Info ######
  ### Gather slr_cm columns
  # slr_cm |> names() |> print()
  idCols0 <- c("year")
  slr_cm  <- slr_cm |> pivot_longer(
    cols = -all_of(idCols0), 
    names_to  = "model",
    values_to = "driverValue"
  ) ### End pivot_longer
  
  ### Standardize model names
  levels0 <- co_models[["model_dot"]]
  labels0 <- co_models[["modelType"]]
  slr_cm  <- slr_cm |> mutate(model = gsub("\\_", "", model))
  slr_cm  <- slr_cm |> mutate(model_dot  = model)
  slr_cm  <- slr_cm |> mutate(model_type = model_dot  |> factor(levels0, labels0))
  slr_cm  <- slr_cm |> mutate(model_type = model_type |> as.character())
  
  ### Zero out values
  slr_0cm <- slr_cm  |> filter(model == "30cm")
  slr_0cm <- slr_0cm |> 
    mutate(model   = "0cm") |> 
    mutate(driverValue = 0) |> 
    rbind(slr_cm)
  
  ### Update in data list, drop intermediate values
  # slr_cm |> names() |> print()
  dataList[["slr_cm"]] <- slr_cm
  rm(idCols0, levels0, labels0)
  
  
  ###### ** 13. SLR Impacts ######
  if (byState) {
    ### State SLR scaledImpacts
    ### Join with state info & relocate columns
    join0      <- stateCols0
    select0    <- "region" |> c(stateCols0)
    before0    <- c("value")
    
    ### Join with state info
    slrImpacts <- slrImpacts |> left_join(co_states |> select(all_of(select0)), by=c(join0))
    slrImpacts <- slrImpacts |> relocate(all_of(select0), .before=all_of(before0))
    
    ### Standardize region info
    mutate0 <- "region"
    slrImpacts <- slrImpacts |> mutate_at(c(mutate0), function(x){gsub(" ", ".", x)})
    
    ### Standardize model info
    rename0    <- c("value")
    rename1    <- c("scaled_impacts")
    slrImpacts <- slrImpacts |> mutate(model_type = "gcm")
    slrImpacts <- slrImpacts |> rename_at(c(rename0), ~rename1)
    rm(join0, select0, before0, mutate0, rename0, rename1)
  } else {
    # slrImpacts |> names() |> print()
    ### Remove special characters from region, model
    idCols0    <- c("sector", "variant", "impactType", "impactYear", "year")
    select0    <- c("region_slr")
    slrImpacts <- slrImpacts |> pivot_longer(
      cols      = -all_of(idCols0), 
      names_to  = "region_slr",
      values_to = "scaled_impacts"
    ) ### End pivot_longer
    rm(idCols0, select0)
    
    ### Get region & model info
    mutate0    <- "region_slr"
    mutate1    <- c("region", "model")
    strRegion0 <- "^([[:graph:]]*)_"
    strModel0  <- "_([[:graph:]]*)$"
    slrImpacts <- slrImpacts |> mutate_at(c(mutate0), trimws)
    slrImpacts <- slrImpacts |> mutate(region = (region_slr |> str_match(strRegion0))[,2])
    slrImpacts <- slrImpacts |> mutate(model  = (region_slr |> str_match(strModel0 ))[,2])
    # slrImpacts$region_slr |> unique() |> sort() |> print()
    # slrImpacts$region |> unique() |> sort() |> print()
    # slrImpacts$model |> unique() |> sort() |> print()
    slrImpacts <- slrImpacts |> mutate_at(c(mutate1), function(x){gsub("\\_", "", x)})
    slrImpacts <- slrImpacts |> mutate(model  = model |> paste0("cm"))
    rm(mutate0, mutate1, strRegion0, strModel0)
    ### Drop intermediate values (update in list further down)
  } ### End else
  
  ### Zero out values
  slrImpacts <- slrImpacts |> filter(model != "0cm")
  slrImpacts <- slrImpacts |> 
    filter(model == "30cm") |> 
    mutate(model =  "0cm" ) |> 
    mutate(scaled_impacts = 0) |> 
    rbind(slrImpacts)
  
  ### Update in data list
  dataList[["slrImpacts"]] <- slrImpacts
  # slrImpacts |> glimpse()
  
  
  ###### ** 14. Format Scaled Impacts ######
  ### Create a list
  list_scaledImpacts <- list()
  list_scaledImpacts[["gcm"]] <- list(name="gcm", data=data_scaledImpacts)
  list_scaledImpacts[["slr"]] <- list(name="slr", data=slrImpacts)
  # list_scaledImpacts <- list(data_scaledImpacts = data_scaledImpacts, slrImpacts = slrImpacts)
  # rm("data_scaledImpacts", "slrImpacts")
  
  list_scaledImpacts <- list_scaledImpacts |> map(function(list_i) {
    ## Data frame
    name_i  <- list_i[["name"]]
    df_i    <- list_i[["data"]]
    do_gcm  <- name_i == "gcm"
    doDot0  <- name_i == "data_scaledImpacts"
    
    ### Replace NA values in impactYear, impactType
    mutate0 <- c("impactType", "impactYear")
    df_i    <- df_i |> mutate_at(c(mutate0), as.character)
    df_i    <- df_i |> mutate(impactType = impactType |> as.character() |> replace_na("NA"))
    df_i    <- df_i |> mutate(impactYear = impactYear |> as.character() |> replace_na("N/A"))
    
      ### Refactor variants (by sector, variant)
      levels0 <- co_variants[["sector_id" ]] |> paste(co_variants[["variant_id_excel"]], sep = "_")
      labels0 <- co_variants[["variant_id"]]
      select0 <- c("sector_variant")
      df_i    <- df_i |> mutate(sector_variant = sector |> paste(variant, sep = "_"))
      df_i    <- df_i |> mutate(sector_variant = sector_variant |> factor(levels0, labels0))
      df_i    <- df_i |> mutate(variant        = sector_variant)
      df_i    <- df_i |> select(-all_of(select0))
      rm(levels0, labels0, select0)
      
      ### Refactor impact years
      levels0 <- co_impactYearLevels[["impactYear_label"]]
      labels0 <- co_impactYearLevels[["impactYear_id"   ]]
      df_i    <- df_i |> mutate(impactYear = impactYear |> factor(levels0, labels0))
      rm(levels0, labels0)
      
      ### Refactor impact types
      levels0 <- co_impactTypes[["sector_id"    ]] |> paste(co_impactTypes[["impactType_id_excel"]], sep = "_")
      labels0 <- co_impactTypes[["impactType_id"]]
      select0 <- c("sector_impactType")
      df_i    <- df_i |> mutate(sector_impactType = sector |> paste(impactType, sep = "_"))
      df_i    <- df_i |> mutate(sector_impactType = sector_impactType |> factor(levels0, labels0))
      df_i    <- df_i |> mutate(impactType        = sector_impactType)
      df_i    <- df_i |> select(-all_of(select0))
      rm(levels0, labels0, select0)
      
      ### Refactor model types/models
      ### Factor model only if name_i=="data_scaledImpacts"
      levels0 <- co_models[["model_id" ]]
      if(do_gcm){labels0 <- co_models[["model_label"]]} else{labels0 <- co_models[["model_dot"]]}
      levels1 <- labels0
      labels1 <- co_models[["modelType"]]
      # levels0 |> print(); labels0 |> print(); levels1 |> print(); labels1 |> print(); df_i$model
      if (doDot0) {df_i <- df_i |> mutate(model_dot = model |> factor(levels0, labels0))}
      else        {df_i <- df_i |> mutate(model_dot = model)}
      df_i    <- df_i |> mutate(model_type = model_dot |> factor(levels1, labels1))
      rm(levels0, levels1, labels1)
      
      ### Convert to character
      mutate0 <- c("variant", "impactType", "impactYear", "model_type", "model_dot")
      df_i    <- df_i |> mutate_at(c(mutate0), as.character)
      rm(mutate0)
      
      ### Join with sector info
      select0 <- c("sector_id", "byState")
      df_i    <- df_i |> left_join(co_sectors |> select(all_of(select0)), by=c("sector"="sector_id"))
      rm(select0)
      
      ### Select columns
      boths0  <- c("sector", "variant", "impactType", "impactYear", "model_type", "model", "model_dot")
      gcm0    <- boths0 |> c("region_dot") |> c(stateCols0) |> c("modelUnitValue", "scaledImpact"  )
      slr0    <- boths0 |> c("region"    ) |> c(stateCols0) |> c("year"          , "scaled_impacts")
      if(do_gcm){useCols0 <- gcm0} else{useCols0 <- slr0}
      df_i      <- df_i |> select(all_of(useCols0))
      ### Return
      return(df_i)
    }) |> set_names(list_scaledImpacts |> names())

  
  ### Update scaled impacts in list
  dataList[["data_scaledImpacts"]] <- list_scaledImpacts[["gcm"]]
  dataList[["slrImpacts"        ]] <- list_scaledImpacts[["slr"]]
  
  ###### Return ######
  ### Return the list of dataframes
  return(dataList)
}