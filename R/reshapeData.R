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
    dataList = NULL,
    byState  = FALSE,
    silent   = TRUE
) {
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  listNames <- dataList |> names()
  # listNames |> print
  for(name_i in listNames) {name_i |> assign(dataList[[name_i]],envir = .GlobalEnv)}
  
  ###### Modify Tables and Update in List  ######
  ###### ** Sectors     ######
  ### Filter to those tables to include
  ### Make a copy of the sectors list to include variants
  ### Drop variant column from `co_sectors`
  drop0           <- c("include", "variants", "impactYears", "impactTypes", "byState")
  co_sectors      <- co_sectors |> filter(include == 1)
  co_stateSectors <- co_sectors |> filter(byState == 1)
  co_sectorsRef   <- co_sectors
  co_sectors      <- co_sectors |> select(-c(all_of(drop0)))
  ### Update values in list
  dataList[["co_sectors"   ]]   <- co_sectors
  dataList[["co_sectorsRef"]]   <- co_sectorsRef
  dataList[["co_stateSectors"]] <- co_stateSectors
  ### Remove intermediate variables
  rm("drop0")
  
  ###### ** Misc ######
  ### No changes to variants
  ### No changes to model types
  ### No changes to input scenario info
  
  ###### ** Impact Years ######
  ### Load data and gather the data by impact year levels
  ### Levels for factoring are impact year levels: `N/A`, `2010`, `2090`, Interpolate
  ### Replace special characters in impact year levels
  drop0 <- c("sector_id")
  drop1 <- c("impactYear2")
  join0 <- c("impactYear_label")
  c_impactYearLevels  <- co_impactYears |> select(-c(all_of(drop0))) |> names()
  co_impactYearLevels <- data.frame(impactYear_label = c_impactYearLevels)
  co_impactYearLevels <- co_impactYearLevels |> mutate(impactYear_id = gsub("/", "", impactYear_label))
  co_impactYearLevels <- co_impactYearLevels |> mutate(impactYear_excel = gsub("NA", "", impactYear_id))
  
  ### Reshape impact years: gather the data by impact year levels
  ### Drop values with missing `impactYear2`
  co_impactYears <- co_impactYears |> gather(key = "impactYear_label", value = "impactYear2", -c(all_of(drop0)))
  co_impactYears <- co_impactYears |> filter(!is.na(impactYear2))
  co_impactYears <- co_impactYears |> select(-c(all_of(drop1)))
  co_impactYears <- co_impactYears |> left_join(co_impactYearLevels, by = c(join0))
  # co_impactYears |> glimpse
  ### Update/add tables in list
  dataList[["co_impactYears"     ]] <- co_impactYears
  dataList[["co_impactYearLevels"]] <- co_impactYearLevels
  ### Remove intermediate variables
  rm("drop0", "drop1", "join0")
  
  ###### ** Impact Types Info ######
  ### Drop damage adjustment names (present in variants)
  ### Update in list
  ### Remove intermediate variables
  drop0          <- c("damageAdjName")
  co_impactTypes <- co_impactTypes |> select(-c(all_of(drop0))) # ; co_impactTypes |> glimpse
  dataList[["co_impactTypes"]] <- co_impactTypes
  rm("drop0")
  
  ###### ** co_models ######
  ### Combine with model types and update data list
  ### Update in list
  ### Remove intermediate variables
  join0          <- c("modelType")
  co_modelTypes2 <- co_modelTypes |> rename(modelType = modelType_id)
  co_models      <- co_models     |> left_join(co_modelTypes2, by = c(join0)) # ; co_models |> glimpse
  dataList[["co_models"]] <- co_models
  rm("join0", "co_modelTypes2")
  
  ###### ** co_regions ######
  ### Combine and add to data list...also a copy with info on national data
  ### Update in list
  co_regions <- co_regions |> mutate(region = region_dot)
  dataList[["co_regions"]] <- co_regions
  
  ###### ** df_sectorsInfo ######
  ### Join sector and other info
  ### Add additional sector info and add to data list
  drop0   <- c("variant_id_excel", "variant_label")
  drop1   <- c("impactYear_label", "impactYear_excel")
  drop2   <- c("impactType_id_excel", "impactType_label", "impactType_description")
  join0   <- c("sector_id")
  rename0 <- c("sector", "variant", "impactYear", "impactType") |> paste("id", sep = "_")
  rename1 <- gsub("_id", "", c(rename0))
  ### Join with co_impactYears, co_impactTypes
  df_sectorsInfo <- co_sectors     |> left_join(co_variants    |> select(-c(all_of(drop0))), by = c(join0))
  df_sectorsInfo <- df_sectorsInfo |> left_join(co_impactYears |> select(-c(all_of(drop1))), by = c(join0), relationship = "many-to-many")
  df_sectorsInfo <- df_sectorsInfo |> left_join(co_impactTypes |> select(-c(all_of(drop2))), by = c(join0), relationship = "many-to-many")
  ### Rename
  df_sectorsInfo <- df_sectorsInfo |> rename_at(.vars = c(rename0), ~rename1) # ; df_sectorsInfo |> glimpse
  ### Update in list
  dataList[["df_sectorsInfo"]] <- df_sectorsInfo
  ### Remove intermediate variables
  rm("drop0", "drop1", "drop2", "join0", "rename0", "rename1")
  
  ###### ** Temperature Scenario ######
  ### ** Initialize Temperature Scenario
  ### Add temp_C_conus
  ### Update in list
  co_defaultTemps <- co_defaultTemps |> mutate(temp_C_conus = temp_C_global %>% (function(x){FrEDI::convertTemps(x, from="global")}))
  dataList[["co_defaultTemps"]] <- co_defaultTemps #|> select(-c("temp_C_conus"))
  
  ###### ** Socioeconomic Scenario ######
  ### Gather the columns from the default scenario and update data list
  if (!byState) {
    idCols0 <- c("year", "gdp_usd")
    nChar0  <- nchar("pop_") + 1 ### 5
    co_defaultScenario <- co_defaultScenario |> gather(key="region", value="reg_pop", -c(all_of(idCols0)))
    co_defaultScenario <- co_defaultScenario |> mutate(region = region |> substr(nChar0, nchar(region)))
    # gather_defaultScenario |> glimpse
    ### Update in list
    ### Remove intermediate variables
    dataList[["co_defaultScenario"]] <- co_defaultScenario
    rm("idCols0", "nChar0")
  } else {
    ### state default population scenario
    str0      <- "proj_"
    select0   <- c("state", "postal")
    select1   <- select0 |> c("proportion_2100", "fips")
    ### Get names
    names0    <- co_defaultScenario |> names()
    names1    <- names0[!(names0 %in% select1)]
    names2    <- gsub(str0, "", names1)
    ### Rename and gather
    # year_cols <- "proj" |> grep(names0)
    co_defaultScenario <- co_defaultScenario |> pivot_longer(
      cols      = all_of(names1), 
      names_to  = "year", 
      values_to = "state_pop"
    )
    ### - Join with state info
    co_defaultScenario <- co_defaultScenario |> left_join(co_states, by = c("state", "postal", "fips"))
    ### Mutate names to years
    co_defaultScenario <- co_defaultScenario |> mutate_at(.vars=c("year"), function(y){gsub(str0, "", y)})
    co_defaultScenario <- co_defaultScenario |> mutate_at(.vars=c("year"), as.numeric)
    ### Save a copy of state proportions
    co_statePopRatios  <- co_defaultScenario |> select(c(all_of(select1)))
    ### Drop columns
    co_defaultScenario <- co_defaultScenario |> select(c(all_of(select0), "region", "year", "state_pop"))
    ### Join with GDP default scenario
    co_defaultScenario <- co_defaultScenario |> left_join(gdp_default, by = c("year"))
    ### Join with region info
    ### Remove values
    rm(str0, names0, names1, names2, select0, select1)
    ### Update in list
    dataList[["co_defaultScenario"]] <- co_defaultScenario
    dataList[["co_statePopRatios" ]] <- co_statePopRatios
  }
  
  # ###### ** Scalar Info ######
  # if (byState){
  #   ### Need to add scalar info
  #   select0 <- c("scalarType", "scalarLabel", "scalarName")
  #   scalarDataframe <- co_scalarInfo |> 
  #     select(c(all_of(select0))) |>
  #     left_join(scalarDataframe, by=c("scalarName"))
  #   rm(select0)
  # } ### End if byState for Scalar Info

  ###### ** GCM Scaled Impacts ######
  if (byState) {
    ### State GCM scaledImpacts:
    ### Reorganize columns
    select0 <- c("state", "postal")
    before0 <- c("value")
    data_scaledImpacts <- data_scaledImpacts |> relocate(c(all_of(select0)), .before=c(all_of(before0)))
    rm(select0, before0)
    ### - Join with state info
    data_scaledImpacts <- data_scaledImpacts |> left_join(co_states, by = c("state", "postal"))
    ### - Mutate model info
    data_scaledImpacts <- data_scaledImpacts |> mutate(model_type = "gcm")
    data_scaledImpacts <- data_scaledImpacts |> mutate(model_dot  = gsub("[-_]", ".", model))
    data_scaledImpacts <- data_scaledImpacts |> mutate(region_dot = gsub(" ", ".", region))
    ### Rename and drop cols
    data_scaledImpacts <- data_scaledImpacts |> rename_at(.vars=c("value"), ~"scaledImpact")
    data_scaledImpacts <- data_scaledImpacts |> select(-c("fips"))
    ### Update in list
    dataList[["data_scaledImpacts"]] <- data_scaledImpacts
  } ### End if(byState)

  ### Remove empty rows and GCM and SLR Averages
  ### Update in list, remove intermediate variables
  filter0 <- co_models [["model_id" ]] |> unique()
  filter1 <- co_sectors[["sector_id"]] |> unique()
  data_scaledImpacts <- data_scaledImpacts |> filter(model  %in% filter0) ### All models
  data_scaledImpacts <- data_scaledImpacts |> filter(sector %in% filter1)
  rm("filter0", "filter1")

  ###### ** SLR Scenario Info ######
  ### Gather slr_cm columns
  ### Substitute special characters in model name
  ### Make a copy of model called model_dot
  ### Factor model dot by model type
  # slr_cm |> names |> print
  idCols0 <- c("year")
  levels0 <- co_models[["model_dot"]]
  labels0 <- co_models[["modelType"]]
  slr_cm  <- slr_cm |> gather(value = "driverValue", key = "model", -c(all_of(idCols0)))
  slr_cm  <- slr_cm |> mutate(model = gsub("\\_", "", model))
  slr_cm  <- slr_cm |> mutate(model_dot  = model)
  slr_cm  <- slr_cm |> mutate(model_type = model_dot |> factor(levels0, labels0))
  # slr_cm |> names |> print
  ### Update list
  ### Remove intermediate variables
  dataList[["slr_cm"]] <- slr_cm
  rm("idCols0", "levels0", "labels0")

  ###### ** SLR Impacts ######
  if (!byState) {
    # slrImpacts |> names |> print
    ### Remove special characters from region, model
    idCols0 <- c("year", "sector", "variant", "impactType", "impactYear")
    select0 <- c("region_slr")
    names0  <- c("region", "model")
    join0   <- c("row_id")
    slrImpacts <- slrImpacts |> gather(value = "scaled_impacts", key = "region_slr", -c(all_of(idCols0)))
    ### Make dataframe with region_slr, region, model and join with slrImpacts
    ### Mutate model
    c_regSlr <- slrImpacts[[select0]] |> str_split("_") %>% (function(i) {do.call(rbind, i)})
    # c_regSlr |> head |> print
    c_regSlr <- c_regSlr %>% (function(x) {colnames(x) <- names0; return(x)})
    c_regSlr <- c_regSlr |> as_tibble()
    c_regSlr <- c_regSlr |> mutate(model = gsub("\\.", "_", model) |> paste0("cm"))
    ### Add row numbers and join
    c_regSlr   <- c_regSlr   |> mutate(row_id = row_number())
    slrImpacts <- slrImpacts |> mutate(row_id = row_number())
    ### Check impacts
    #"nrow(c_regSlr) == nrow(slrImpacts)" |> paste0(": ", nrow(c_regSlr) == nrow(slrImpacts)) |> print()
    ### Join and drop join columns
    slrImpacts <- slrImpacts |> left_join(c_regSlr, by = c(join0))
    slrImpacts <- slrImpacts |> select(-c(all_of(join0), all_of(select0)))
    ### Drop intermediate values (update in list further down)
    rm("idCols0", "select0", "names0", "join0", "c_regSlr")
  } else {
    ### State SLR scaledImpacts
    ### - Reorganize columns
    select0    <- c("state", "postal")
    before0    <- c("value")
    slrImpacts <- slrImpacts |> relocate(c(all_of(select0)), .before = c(all_of(before0)))
    rm(select0, before0)
    ### Join with state info
    slrImpacts <- slrImpacts |> left_join(co_states, by = c("state", "postal"))
    ### Join with model info
    slrImpacts <- slrImpacts |> left_join(co_models |> select(c("model_id", "model_dot")), by = c("model" = "model_id"))
    ### Add model info
    slrImpacts <- slrImpacts |> mutate(model_type = "gcm")
    slrImpacts <- slrImpacts |> mutate(region = gsub(" ", ".", region))
    ### Rename & select columns
    slrImpacts <- slrImpacts |> rename_at(.vars=c("value"), ~"scaled_impacts")
    slrImpacts <- slrImpacts |> select(-c("fips"))
    ### Update data list
    dataList[["slrImpacts"]] <- slrImpacts
  }

  ###### ** Format Scaled Impacts ######
  if (!byState) {
    ### Create a list
    list_scaledImpacts <- list(data_scaledImpacts = data_scaledImpacts, slrImpacts = slrImpacts)
    rm("data_scaledImpacts", "slrImpacts")

    list_scaledImpacts <- list_scaledImpacts |>
      names() %>%
      map(function(name_i, list_x = list_scaledImpacts) {
        ## Data frame
        df_i    <- list_x[[name_i]]
        ### Replace NA values in impactYear, impactType
        df_i    <- df_i |> mutate(impactYear = impactYear |> as.character() |> replace_na("N/A"))
        df_i    <- df_i |> mutate(impactType = impactType |> as.character() |> replace_na("NA"))
        ### Refactor variants, impact estimate years, and impact types
        ### Refactor variants (by sector, variant)
        levels0 <- co_variants[["sector_id" ]] |> paste(co_variants[["variant_id_excel"]], sep = "_")
        labels0 <- co_variants[["variant_id"]]
        select0 <- c("sector_variant")
        df_i    <- df_i |> mutate(sector_variant = sector |> paste(variant, sep = "_"))
        df_i    <- df_i |> mutate(sector_variant = sector_variant |> factor(levels0, labels0))
        df_i    <- df_i |> mutate(variant = sector_variant)
        df_i    <- df_i |> select(-c(all_of(select0)))
        rm("levels0", "labels0", "select0")
        ### Refactor impact years
        levels0 <- co_impactYearLevels[["impactYear_label"]]
        labels0 <- co_impactYearLevels[["impactYear_id"   ]]
        df_i    <- df_i |> mutate(impactYear = impactYear |> factor(levels0, labels0))
        rm("levels0", "labels0")
        ### Refactor impact types
        levels0 <- co_impactTypes[["sector_id"    ]] |> paste(co_impactTypes[["impactType_id_excel"]], sep = "_")
        labels0 <- co_impactTypes[["impactType_id"]]
        select0 <- c("sector_impactType")
        df_i    <- df_i |> mutate(sector_impactType = sector |> paste(impactType, sep = "_"))
        df_i    <- df_i |> mutate(sector_impactType = sector_impactType |> factor(levels0, labels0))
        df_i    <- df_i |> mutate(impactType        = sector_impactType)
        df_i    <- df_i |> select(-c(all_of(select0)))
        rm("levels0", "labels0", "select0")
        ### Refactor model types
        levels0 <- co_models[["model_id" ]]
        labels0 <- co_models[["model_dot"]]
        levels1 <- co_models[["model_dot"]]
        labels1 <- co_models[["modelType"]]
        ### Factor model only if name_i=="data_scaledImpacts"
        ### Factor model_type
        ### Then convert to character
        doDot0  <- name_i == "data_scaledImpacts"
        if (doDot0) {df_i <- df_i |> mutate(model_dot = model |> factor(levels0, labels0))}
        else        {df_i <- df_i |> mutate(model_dot = model)}
        df_i    <- df_i |> mutate(model_type = model_dot |> factor(levels1, labels1))
        ### Convert to character
        mutate0 <- c("variant", "impactYear", "impactType", "model_dot")
        df_i    <- df_i |> mutate_at(.vars = c(mutate0), as.character)
        # rm("levels0", "labels0", "doDot0", "mutate0")
        return(df_i)
      }) %>%
      (function(list_x, names_x = names(list_scaledImpacts)) {
        names(list_x) <- names_x; return(list_x)
      })

    ### Update objects in environment
    for (name_i in names(list_scaledImpacts)) {assign(name_i, list_scaledImpacts[[name_i]])}

    ### Reshape data_scaledImpacts (move columns with regional values to rows)
    valueCols0         <- co_regions[["region_dot"]]
    data_scaledImpacts <- data_scaledImpacts |> gather(key = "region_dot", value = "scaledImpact", c(all_of(valueCols0)))
    rm("valueCols0")
  }

  ### Update scaled impacts in list
  dataList[["data_scaledImpacts"]] <- data_scaledImpacts
  dataList[["slrImpacts"        ]] <- slrImpacts

  ###### ** Reshape Scalars ######
  if (!byState) {
    ### refactor region
    ### Update in list
    ### Remove intermediate objects
    levels0 <- co_regions[["region_label"]] |> c("National Total")
    labels0 <- co_regions[["region_dot"  ]] |> c("National.Total")
    scalarDataframe <- scalarDataframe |> mutate(region = region |> factor(levels0, labels0) |> as.character())
    dataList[["scalarDataframe"]] <- scalarDataframe
    rm("levels0", "labels0")
  } else {
    ### state scalars
    select0         <- c("scalarName", "scalarLabel", "scalarType")
    scalarDataframe <- scalarDataframe |> left_join(co_scalarInfo |> select(c(all_of(select0))), by = c("scalarName"))
    dataList[["scalarDataframe"]] <- scalarDataframe
    rm(select0)
  }

  ### Return the list of dataframes
  return(dataList)
}