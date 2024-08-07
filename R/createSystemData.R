### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
    dataList    = list(), ### List of data created by reshapeData
    outPath     = "." |> file.path("data", "sysdata.rda"),
    extend_all  = FALSE,  ### Whether to extend all GCM model observations to maximum range
    silent      = FALSE,  ### Level of messaging 
    return      = TRUE,   ### Whether to return the data list
    save        = FALSE,  ### Whether to save the file
    msg0        = "\t"    ### Prefix for messaging
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser    <- !silent
  lvl0       <- msg0 |> str_count(pattern="\\t")
  msg0       <- function(lvl0=1){"\t" |> rep(lvl0) |> paste(collapse="")}
  if(msgUser) {msg0(lvl0=lvl0 + 1) |> paste0("Running createSystemData()...") |> message()}
  
  # dataList |> names() |> print()
  # dataList[["frediData"   ]] |> names() |> print()
  
  ###### Import Functions to Namespace ######
  convertTemps       <- utils::getFromNamespace("convertTemps"      , "FrEDI")
  temps2slr          <- utils::getFromNamespace("temps2slr"         , "FrEDI")
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  match_scalarValues <- utils::getFromNamespace("match_scalarValues", "FrEDI")
  # update_popScalars  <- utils::getFromNamespace("update_popScalars" , "FrEDI")
  get_scenario_id    <- utils::getFromNamespace("get_scenario_id"   , "FrEDI")
  # get_uniqueValues   <- utils::getFromNamespace("get_uniqueValues"  , "FrEDI")
  
  
  ###### Initialize Data List ######
  ### Initialize list of data to save
  rDataList  <- list()
  rDataList  <- rDataList |> c(dataList)
  
  ###### Configuration Data ######
  ### Get config info, add data to list, assign objects to environment
  if(msgUser) {msg0(lvl0 + 2) |> paste0("Loading configuration info...") |> message()}
  fredi_config <- frediConfig()
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}
  rDataList[["fredi_config"]] <- fredi_config
  # fredi_config |> list2env(envir = environment())
  
  
  ###### Assign Data Objects ###### 
  ###### This section reads in data from the data file and returns a list of tables
  ### Add tables to data list
  # if(msgUser) {msg0(1) |> paste0(messages_data[["loadInputs"]][["success"]]) |> message()}
  if(msgUser) msg0(lvl0 + 2) |> paste0("Configuring data...") |> message()
  # dataList |> list2env(envir = environment())
  # dataList |> names() |> print()
  frediData     <- dataList[["frediData"   ]]
  scenarios     <- dataList[["scenarioData"]]
  stateData     <- dataList[["stateData"   ]]
  ### Data names
  frediNames    <- frediData  |> names()
  scenarioNames <- scenarios  |> names()
  stateNames    <- stateData  |> names()
  # frediNames |> print(); scenarioNames |> print(); stateNames |> print()
  ### Assign objects
  for(name_i in frediNames   ){ name_i |> assign(frediData   [[name_i]]); rm(name_i) }

  
  
  ###### State Columns ######
  stateCols0  <- c("state", "postal")
  popCol0     <- c("pop")
  national0   <- c("NationalTotal")

  
  ###### Sector Info ######
  ### Exclude some sectors, get the number of sectors and sector info
  ### Sector info with additional sector info: df_sectorsInfo
  ### Sector info with models: df_sectorsModels
  # sector_ids   <- co_sectors |> pull() |> unique()
  
  
  ###### GCAM Scenarios ######
  ### Get reference years and add to fredi_config
  if(msgUser) {msg0(lvl0 + 3) |> paste0("Formatting GCAM scenarios...") |> message()}
  # co_modelTypes |> glimpse()
  refYear0       <- co_modelTypes |> pull(modelRefYear) |> min()
  ### Default temperature scenario
  ### Columns, years for interpolation
  gcamData       <- scenarios[["gcamData"]]
  # gcamData |> glimpse()
  gcam_scenarios <- gcamData       |> format_gcamData()
  gcam_default   <- gcam_scenarios |> filter(year >= refYear0) |> filter(scenario == "ECS_3.0_REF")
  ### Add to list, remove intermediate values
  scenarios[["gcam_scenarios"]] <- gcam_scenarios
  scenarios[["gcam_default"  ]] <- gcam_default
  rm(refYear0, gcamData)
  
  
  ###### Socioeconomic Scenario ######
  if(msgUser) {msg0(lvl0 + 3) |> paste0("Creating socioeconomic scenario...") |> message()}
  
  ### Interpolate annual values for GDP:
  ### Filter to first unique region
  ### Select GDP columns and add national total
  gdpData     <- scenarios[["gdpData"]]
  gdp_default <- gdpData |> interpolate_gdp()
  scenarios[["gdp_default"]] <- gdp_default
  rm(gdpData)
  
  ### Interpolate annual values for population and add to data list
  popData     <- scenarios[["popData"]]
  pop_default <- popData |> interpolate_pop()
  scenarios[["pop_default"]] <- pop_default
  rm(popData)
  # pop_default |> names() |> print()

  ### Default socioeconomic scenario:
  ### Use to assess default scalars but don't add to list
  pop_default <- pop_default |> mutate(region = region |> str_replace(" ", ""))
  df_national <- gdp_default |> create_nationalScenario(pop0=pop_default)
  # df_national |> names() |> print()
  rm(gdp_default, pop_default)
  
  
  
  ###### Format Scalars ######
  ### Interpolate values to annual levels
  if(msgUser) {msg0(lvl0 + 3) |> paste0("Formatting scalars...") |> message()}
  # scalarDataframe |> names() |> print()
  ### Get data
  scalars    <- stateData[["scalarData"]]
  ### df_mainScalars
  df_scalars <- fun_formatScalars(
    data_x  = scalars,          ### rDataList$scalarDataframe
    info_x  = co_scalarInfo,    ### rDataList$co_scalarInfo
    years_x = minYear0:npdYear0 ### rDataList$list_years
  ) ### End fun_formatScalars
  ### Add other info
  df_scalars <- df_scalars |> update_popScalars(df_national, popCol=popCol0)
  # df_scalars <- df_scalars |> update_popScalars(df_national |> rename_at(vars("pop"), ~"state_pop"), popCol="state_pop")
  ### Update list and remove objects
  stateData[["df_scalars"]] <- df_scalars
  rm(df_scalars, df_national)
  # return(stateData)
  

  
  ###### Format SLR Data ######
  if(msgUser) {msg0(lvl0 + 3) |> paste0("Formatting SLR data...") |> message()}
  ### Standardize SLR scenario info
  if(msgUser) {msg0(lvl0 + 4) |> paste0("Standardizing SLR scaled impacts...") |> message()}
  slrImpData  <- stateData[["slrImpData"]]
  slrImpData  <- slrImpData |> standardize_scaledImpacts(
    df1  = co_sectorsInfo, 
    xCol = "year"
  ) ### End standardize_scaledImpacts
  ### Update list
  slrImpData  <- stateData[["slrImpData"]]
  
  ### Create data for extreme values above 250cm
  if(msgUser) {msg0(lvl0 + 4) |> paste0("Creating extreme SLR impact values...") |> message()}
  slrExtremes <- fun_slrConfigExtremes(slr_x=slr_cm, imp_x=slrImpData)
  # return(rDataList)
  
  ### Extend/Interpolate SLR Heights, Impacts, and Extremes
  if(msgUser) {msg0(lvl0 + 4) |> paste0("Extending SLR values...") |> message()}
  slr_cm       <- slr_cm      |> extend_slr()
  slrImpacts   <- slrImpData  |> extend_slr()
  slrExtremes  <- slrExtremes |> extend_slr()
  
  ### Add other values back into slrImpacts
  include0     <- c("region") |> c(stateCols0) |> c("model")
  slrImpacts   <- slrImpacts  |> get_scenario_id(include=include0) |> ungroup()
  # slrExtremes  <- slrExtremes |> get_scenario_id(include=include0) |> ungroup()
  
  groups0      <- slrImpacts  |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
  slrImpacts   <- slrImpacts  |> mutate(hasScenario = scenario_id %in% groups0)
  # slrExtremes  <- slrExtremes |> mutate(hasScenario = scenario_id %in% groups0)
  rm(include0, groups0)
  
  ### Update in data list and remove objects
  frediData[["slr_cm"     ]] <- slr_cm
  stateData[["slrImpacts" ]] <- slrImpacts
  stateData[["slrExtremes"]] <- slrExtremes
  rm(slr_cm, slrImpacts, slrExtremes)
  # return(list(frediData=frediData, stateData=stateData))
  
  
  
  ###### Format GCM Data  ######
  if(msgUser) {msg0(lvl0 + 3) |> paste0("Formatting GCM data...") |> message()}
  ### Standardize GCM scaled impacts data
  if(msgUser) {msg0(lvl0 + 4) |> paste0("Standardizing GCM scaled impacts...") |> message()}
  gcmImpData  <- stateData[["gcmImpData"]]
  gcmImpData  <- gcmImpData |> standardize_scaledImpacts(
    df1  = co_sectorsInfo, 
    xCol = "modelUnitValue"
  ) ### End standardize_scaledImpacts
  ### Update in data list
  stateData[["gcmImpData"]] <- gcmImpData
  # if(msgUser) {msg0(lvl0 + 3) |> paste0("Getting scenario IDs...") |> message()}
  # ### Add a column with a scenario id
  # # data_scaledImpacts |> glimpse()
  # # gcmImpacts |> glimpse()
  # gcmImpData   <- stateData[["gcmImpData"]]
  # includeCols  <- c("region") |> c(stateCols0) |> c("model")
  # gcmImpData   <- gcmImpData |> get_scenario_id(include=includeCols) |> ungroup()
  # ### Get list of scenarios for scenarios with at least some non-NA values
  # gcmGroupList <- gcmImpData |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
  # # c_scenariosList
  # ### Add information on non-missing scenarios to scaled impacts data
  # gcmImpData   <- gcmImpData |> mutate(hasScenario = (scenario_id %in% gcmGroupList))
  # ### Update in data list
  # stateData[["gcmImpData"]] <- gcmImpData
  # rm(gcmGroupList)
  # # return(rDataList)
  
  
  
  ### Get Interpolation Functions for Scenarios
  ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
  ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
  if(msgUser) {msg0(lvl0 + 4) |> paste0("Creating list of impact functions...") |> message()}
  # gcmImpData |> pull(region) |> unique() |> print()
  # gcmImpData |> glimpse()
  gcmNoImpacts  <- gcmImpData |> filter(hasScenario != 1)
  gcmImpacts    <- gcmImpData |> filter(hasScenario == 1)
  gcmImpacts    <- gcmImpacts |> filter(!(scaled_impacts |> is.na())) 
  rm(gcmImpData)
  
  ### Max output value, maximum extrapolation value, unit scale, extend type
  df_gcm        <- co_modelTypes |> filter(modelType_id=="gcm")
  maxOutput_gcm <- df_gcm |> pull(modelMaxOutput)
  maxExtrap_gcm <- df_gcm |> pull(modelMaxExtrap)
  # unitScale_gcm  <- df_gcm |> pull(modelUnitScale)
  
  ### Get functions
  gcmImpFuncs   <- gcmImpacts |> get_impactFunctions(
    groupCol    = "scenario_id",
    xCol        = "modelUnitValue",
    yCol        = "scaled_impacts",
    extend_all  = extend_all,
    # unitScale   = unitScale_gcm,
    extend_from = maxOutput_gcm,
    extend_to   = maxExtrap_gcm
  ) ### get_impactFunctions
  
  ### Separate out impacts and bind with gcmNoImpacts
  gcmImpacts    <- gcmImpFuncs[["df0"  ]]
  gcmImpFuncs   <- gcmImpFuncs[["funs0"]]
  
  ### Add values to list and remove intermediate values
  ### list_impactFunctions
  stateData[["gcmImpacts" ]] <- gcmImpacts
  stateData[["gcmImpFuncs"]] <- gcmImpFuncs
  rm(df_gcm, gcmImpacts, gcmImpFuncs)
  ### Message the user
  if(msgUser) {msg0(4) |> paste0(messages_data[["interpFuns"]]$success) |> message()}
  
  ###### Drop Data  ######
  ### Drop data sets that are no longer needed
  # drop0         <- c("scalarData", "gcmImpData", "slrImpData")
  # stateData     <- stateData |> (function(list0, x=drop0){list0[!((list0 |> names()) %in% x)]})()
  
  ###### Update Data List ######
  if(msgUser) {msg0(lvl0 + 2) |> paste0("Updating data in lists...") |> message()}
  rDataList[["frediData"   ]] <- frediData
  rDataList[["stateData"   ]] <- stateData
  rDataList[["scenarioData"]] <- scenarios
  # scenarios |> names() |> print()
  
  ###### Return ######
  if(msgUser){paste0(msg0(lvl0 + 1), "...Finished running createSystemData()", ".", "\n") |> message()}
  return(rDataList)
} ### End function



