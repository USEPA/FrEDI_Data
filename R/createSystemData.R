### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
    dataList    = list(), ### List of data created by reshapeData
    outPath     = "." |> file.path("data", "sysdata.rda"),
    # configPath  = "." |> file.path("R"   , "fredi_config.R"), ### Path to config file
    # byState     = FALSE,  ### Whether to run the function for state-level data (deprecated...remove in future updates)
    extend_all  = FALSE,  ### Whether to extend all GCM model observations to maximum range
    silent      = FALSE,  ### Level of messaging 
    return      = TRUE,   ### Whether to return the data list
    save        = FALSE   ### Whether to save the file
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser    <- !silent
  msg0       <- function(lvl0=1){"\t" |> rep(lvl0) |> paste(collapse="")}
  if(msgUser) {msg0() |> paste0("Running createSystemData()...") |> message()}
  
  
  ###### Initialize Data List ######
  ### Initialize list of data to save
  rDataList  <- list()
  
  ###### Configuration Data ######
  ### Add config to data list
  if(msgUser) {msg0(1) |> paste0("Loading config info from '", configFile, "'...") |> message()}
  rDataList[["fredi_config"]] <- fredi_config()
  fredi_config <- fredi_config()
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}
  # fredi_config |> list2env(envir = environment())
  
  
  ###### Import Functions to Namespace ######
  get_scenario_id    <- utils::getFromNamespace("get_scenario_id"   , "FrEDI")
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  match_scalarValues <- utils::getFromNamespace("match_scalarValues", "FrEDI")
  convertTemps       <- utils::getFromNamespace("convertTemps"      , "FrEDI")
  temps2slr          <- utils::getFromNamespace("temps2slr"         , "FrEDI")
  
  
  ###### By State ######
  popCols0    <- c("state", "postal")
  popCol0     <- c("pop")
  national0   <- c("NationalTotal")
  
  
  ###### Assign Data Objects ###### 
  ###### This section reads in data from the data file and returns a list of tables
  ### Add tables to data list
  # if(msgUser) {msg0(1) |> paste0(messages_data[["loadInputs"]][["success"]]) |> message()}
  if(msgUser) {msg0(1) |> paste0("Configuring data...") |> message()}
  rDataList   <- rDataList |> c(dataList)
  for(name_i in loadDataList |> names()) {name_i |> assign(loadDataList[[name_i]]); rm(name_i)}
  # loadDataList |> list2env(envir = environment())
  
  
  ###### Sector Info ######
  ### Exclude some sectors, get the number of sectors and sector info
  ### Sector info with additional sector info: df_sectorsInfo
  ### Sector info with models: df_sectorsModels
  sector_ids   <- co_sectors[["sector_id"]]
  sector_ids   <- sector_ids |> unique()
  num_sectors  <- sector_ids |> length()
  
  
  ###### Default Driver Scenarios ######
  ### Get reference years and add to fredi_config
  if(msgUser) {msg0(2) |> paste0("Creating driver scenarios...") |> message()}
  refYear_temp   <- co_modelTypes |> filter(inputName == "gcm") |> pull(modelRefYear)
  refYear_slr    <- co_modelTypes |> filter(inputName == "slr") |> pull(modelRefYear)
  
  ### Default temperature scenario
  ### Columns, years for interpolation
  scenarios_gcam <- gcamData      |> format_gcamData()
  gcam_default   <- gcamScenarios |> filter(year >= refYear_temp) |> filter(scenario == "ECS_3.0_REF")
  ### Add to list, remove intermediate values
  rDataList[["scenarios_gcam"]] <- scenarios_gcam
  rDataList[["gcam_default"  ]] <- gcam_default
  
  
  ###### Default Socioeconomic Scenario ######
  if(msgUser) {msg0(2) |> paste0("Creating socioeconomic scenario...") |> message()}
  
  ### Interpolate annual values for GDP:
  ### Filter to first unique region
  ### Select GDP columns and add national total
  # list_years |> print()
  select0     <- c("year", "gdp_usd")
  drop0       <- c("region")
  years0      <- gdpData |> pull(year) |> get_years_fromData()
  gdp_default <- gdpData     |> select(all_of(select0))
  gdp_default <- gdp_default |> mutate(region = national0)
  gdp_default <- gdp_default |> interpolate_annual(years=years0, column="gdp_usd", rule=2:2)
  gdp_default <- gdp_default |> select(-any_of(drop0))
  # gdp_default$year |> unique() |> print()
  rDataList[["gdp_default"]] <- gdp_default
  rm(select0, drop0, years0)
  
  ### Interpolate annual values for population and add to data list
  select0     <- c("region", "state", "postal", "year", "pop")
  drop0       <- c("region")
  years0      <- popData     |> pull(year) |> get_years_fromData()
  pop_default <- popData     |> select(all_of(select0))
  pop_default <- pop_default |> mutate(region = national0)
  pop_default <- pop_default |> interpolate_annual(years=years0, column=popCol0, rule=2:2, byState=T)
  pop_default <- pop_default |> select(-any_of(drop0))
  rDataList[["pop_default"]] <- pop_default
  rm(select0, years0)
  # pop_default |> names() |> print()
  
  ### Calculate national population and add to data list
  group0      <- c("year")
  drop0       <- c("region")
  df_natPop   <- pop_default |> 
    group_by_at (c(group0)) |> 
    summarize_at(c(popCol0), sum, na.rm=T) |> ungroup() |> 
    rename_at(c(popCol0), ~c("national_pop"))
  # df_natPop |> names() |> print()

  ### Default socioeconomic scenario: 
  ### - Join national GDP with national population by year
  ### - Join national values with regional population by year
  ### - Calculate GDP per capita
  join0       <- c("year")
  df_national <- gdp_default |> left_join(df_natPop, by=c(join0))
  df_national <- df_national |> left_join(pop_default, by=c(join0), relationship="many-to-many")
  df_national <- df_national |> mutate(gdp_percap = gdp_usd / national_pop)
  ### Add to list, remove intermediate values
  # df_national |> names() |> print()
  rm(join0, gdp_default, pop_default)
  

  ###### Extreme SLR Scenarios ######
  ### replace NA values and convert to character
  # slr_cm |> names() |> print(); slrImpacts |> names() |> print(); 
  mutate0     <- c("model_type")
  string0     <- c("slr")
  ### Replace NA values
  slr_cm      <- slr_cm     |> mutate_at(vars(mutate0), replace_na, string0)
  slrImpacts  <- slrImpacts |> mutate_at(vars(mutate0), replace_na, string0)
  ### Convert to character
  slr_cm      <- slr_cm     |> mutate_at(vars(mutate0), as.character)
  slrImpacts  <- slrImpacts |> mutate_at(vars(mutate0), as.character)
  ### Create data for extreme values above 250cm
  if(msgUser) {msg0(2) |> paste0("Creating extreme SLR impact values...") |> message()}
  slrExtremes <- fun_slrConfigExtremes(
    slr_x = slr_cm,    ### rDataList$slr_cm
    imp_x = slrImpacts ### rDataList$slrImpacts
  ) ### End fun_slrConfigExtremes
  # return(rDataList)
  
  
  ###### Interpolate SLR Scenarios ######
  ### Extend SLR Heights, Impacts, and Extremes
  if(msgUser) {msg0(2) |> paste0("Extending SLR values...") |> message()}
  # c_cm         <- c("model", "year")
  # c_imp        <- c("sector", "variant", "impactType", "impactYear", "region") |> c(popCols0) |> c("year")
  slr_cm       <- slr_cm      |> extend_slr()
  slrImpacts   <- slrImpacts  |> extend_slr()
  slrExtremes  <- slrExtremes |> extend_slr()

  ### Update in data list and remove objects
  rDataList[["slr_cm"     ]] <- slr_cm
  rDataList[["slrImpacts" ]] <- slrImpacts
  rDataList[["slrExtremes"]] <- slrExtremes
  rm(slr_cm, slrImpacts, slrExtremes); 
  # rm(c_cm, c_imp)
  # return(rDataList)
  
  ###### Format Scalar Tables ######
  ### Interpolate values to annual levels
  if(msgUser) {msg0(2) |> paste0("Formatting scalars...") |> message()}
  # scalarDataframe |> names() |> print()
  years0         <- minYear0:npdYear0
  df_mainScalars <- fun_formatScalars(
    data_x  = scalarDataframe, ### rDataList$scalarDataframe
    info_x  = co_scalarInfo,   ### rDataList$co_scalarInfo
    years_x = years0           ### rDataList$list_years
  ) ### End fun_formatScalars
  ### Add other info
  update_popScalars <- utils::getFromNamespace("update_popScalars", "FrEDI")
  df_mainScalars    <- df_mainScalars |> update_popScalars(df_national)
  ### Update list
  rDataList[["df_mainScalars"]] <- df_mainScalars
  # return(rDataList)
  
  
  ###### Get Scenario Info for Scaled Impacts  ######
  if(msgUser) {msg0(2) |> paste0("Getting scenario IDs...") |> message()}
  ### Add a column with a scenario id
  # data_scaledImpacts |> glimpse()
  includeCols        <- c("region_dot") |> c(stateCols0) |> c("model_dot")
  data_scaledImpacts <- data_scaledImpacts |> get_scenario_id(include=includeCols)
  ### Get list of scenarios for scenarios with at least some non-NA values
  c_scenariosList    <- data_scaledImpacts |> filter(!(scaledImpact |> is.na()))
  get_uniqueValues   <- utils::getFromNamespace("get_uniqueValues", "FrEDI")
  c_scenariosList    <- c_scenariosList    |> get_uniqueValues(column="scenario_id")
  ### Add information on non-missing scenarios to scaled impacts data
  data_scaledImpacts <- data_scaledImpacts |> mutate(hasScenario = (scenario_id %in% c_scenariosList))
  # rm(c_scenariosList)
  ### Update in data list
  rDataList[["data_scaledImpacts"]] <- data_scaledImpacts
  # return(rDataList)
  
  
  ###### Get Interpolation Functions for Scenarios ######
  ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
  ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
  if(msgUser) {msg0(2) |> paste0("Creating list of impact functions...") |> message()}
  df_hasScenario <- data_scaledImpacts |> filter(hasScenario)
  df_hasScenario <- df_hasScenario     |> ungroup()
  c_modelTypes   <- c("gcm")
  
  ### Max output value, maximum extrapolation value, unit scale, extend type
  df_gcm         <- (co_modelTypes |> filter(modelType_id==c_modelTypes))
  maxOutput_gcm  <- df_gcm[["modelMaxOutput"]][1]
  maxExtrap_gcm  <- df_gcm[["modelMaxExtrap"]][1]
  unitScale_gcm  <- df_gcm[["modelUnitScale"]][1]
  
  ### Get functions
  functions_gcm  <- df_hasScenario |> get_impactFunctions(
    groupCol    = "scenario_id",
    xCol        = "modelUnitValue",
    yCol        = "scaledImpact",
    extend_from = maxOutput_gcm,
    extend_to   = maxExtrap_gcm,
    extend_all  = extend_all,
    unitScale   = unitScale_gcm
  ) ### get_impactFunctions
  
  ### Add values to list and remove intermediate values
  rDataList[["list_impactFunctions"]] <- functions_gcm
  rm(c_modelTypes, maxOutput_gcm, maxExtrap_gcm, unitScale_gcm)
  rm(df_gcm, df_hasScenario, functions_gcm)
  ### Message the user
  if(msgUser) {msg0(1) |> paste0(messages_data[["interpFuns"]]$success) |> message()}
  
  
  ###### Return ######
  if(msgUser){msg0(1) |> paste0("...Finished running createSystemData()", ".") |> message()}
  return(rDataList)
} ### End function



