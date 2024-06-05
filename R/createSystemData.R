### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
    dataList    = list(), ### List of data created by reshapeData
    outPath     = "." |> file.path("data", "sysdata.rda"),
    configPath  = "." |> file.path("R"   , "fredi_config.R"), ### Path to config file
    extend_all  = FALSE, 
    save        = FALSE,
    silent      = FALSE,  ### Whether to message the user
    return      = TRUE,
    byState     = FALSE
){
  
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser     <- !silent
  msg0        <- function(lvl0=1){"\t" |> rep(lvl0) |> paste(collapse="")}
  if(msgUser) {msg0() |> paste0("Running createSystemData()...") |> message()}
  
  ###### Initialize Data List ######
  ### Initialize list of data to save
  rDataList   <- list()
  
  ###### Create File Paths ######
  ### Config file
  configPath <- if (configPath |> is.null()) {"." |> file.path("R", "fredi_config.R")} else {configPath}
  configFile <- configPath |> basename()
  configPath <- configPath |> dirname()
  configFile <- configPath |> file.path(configFile)
  # configFile |> print
  
  ###### Configuration Data ######
  ### Read in configuration data and assign data tables to objects in the list
  if(msgUser) {msg0(1) |> paste0("Loading config info from '", configFile, "'...") |> message()}
  configFile |> source()
  for(name_i in names(fredi_config)) {assign(name_i, fredi_config[[name_i]]); rm("name_i")}
  
  ###### Import Functions to Namespace ######
  get_scenario_id       <- utils::getFromNamespace("get_scenario_id"      , "FrEDI")
  interpolate_annual    <- utils::getFromNamespace("interpolate_annual"   , "FrEDI")
  match_scalarValues    <- utils::getFromNamespace("match_scalarValues"   , "FrEDI")
  convertTemps          <- utils::getFromNamespace("convertTemps"         , "FrEDI")
  temps2slr             <- utils::getFromNamespace("temps2slr"            , "FrEDI")
  
  ###### By State ######
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  
  ###### Assign Data Objects ###### 
  ###### This section reads in data from the data file and returns a list of tables
  ###### Table names physScalars_table1, econScalars_table1, outputs_table1, co_sectors
  ### Add loaded tables to data list
  ### Load data from file
  loadDataList <- dataList
  rDataList    <- rDataList |> c(loadDataList)
  if(msgUser) {msg0(1) |> paste0(messages_data[["loadInputs"]]$success) |> message()}
  for(name_i in names(loadDataList)) {assign(name_i, loadDataList[[name_i]]); rm("name_i")}
  
  ###### Sector Info ######
  ### Exclude some sectors, get the number of sectors and sector info
  ### Sector info with additional sector info: df_sectorsInfo
  ### Sector info with models: df_sectorsModels
  if(msgUser) {msg0(1) |> paste0("Configuring data...") |> message()}
  sector_ids   <- if (byState) {co_stateSectors[["sector_id"]]} else {co_sectors[["sector_id"]]}
  sector_ids   <- sector_ids |> unique()
  num_sectors  <- sector_ids |> length()
  
  ###### Default Driver Scenarios ######
  ### Get reference years and add to fredi_config
  if(msgUser) {msg0(2) |> paste0("Creating driver scenarios...") |> message()}
  refYear_temp <- (co_modelTypes |> filter(modelUnitType=="temperature"))$modelRefYear |> unique()
  refYear_slr  <- (co_modelTypes |> filter(modelUnitType=="slr"        ))$modelRefYear |> unique()
  fredi_config[["refYear_temp"]] <- refYear_temp
  fredi_config[["refYear_slr" ]] <- refYear_slr
  
  ### Default temperature scenario
  ### Columns, years for interpolation
  tempCols     <- c("year", "temp_C_conus", "temp_C_global")
  drop0        <- c("region")
  years0       <- refYear_temp:maxYear
  ### Zero out CONUS values for temperature at reference year and interpolate annual values
  df_refTemp   <- tibble(year = refYear_temp, temp_C_conus = 0)
  temp_default <- co_defaultTemps |> filter(year > refYear_temp) |> select(c("year", "temp_C_conus"))
  temp_default <- df_refTemp      |> rbind(temp_default)
  # rDataList[["temp_default"]] <- temp_default
  ### Interpolate annual
  temp_default <- temp_default    |> interpolate_annual(years=years0, column="temp_C_conus", rule=1:2)
  ### Calculate global temperatures and update in list
  temp_default <- temp_default    |> mutate(temp_C_global = temp_C_conus |> convertTemps(from="conus"))
  temp_default <- temp_default    |> select(-all_of(drop0))
  rDataList[["temp_default"]] <- temp_default
  # temp_default |> glimpse(); temp_default$year |> length() |> print()
  ### Calculate annual values for SLR from global temperatures and add to list
  ### Ref year not needed since temps2slr will zero out values
  df_refSlr    <- tibble(year=refYear_slr, slr_cm=0)
  slr_default  <- temps2slr(temps=temp_default[["temp_C_global"]], years=temp_default[["year"]])
  rDataList[["slr_default"]] <- slr_default
  # slr_default |> names |> print
  ### Remove intermediate values
  rm(tempCols, drop0); rm(temp_default, slr_default)
  
  ###### Default Socioeconomic Scenario ######
  if(msgUser) {msg0(2) |> paste0("Creating socioeconomic scenario...") |> message()}
  ### Columns
  nationalDot <- c("National.Total")
  gdpCols     <- c("year", "gdp_usd")
  group0      <- c("year")
  drop0       <- c("region")
  ### Population columns
  popColName  <- byState |> ifelse("state_pop", "reg_pop")
  popCols     <- c("year", "region") |> c(stateCols0) |> c(popColName)
  
  ### Interpolate annual values for GDP:
  ### Filter to first unique region
  ### Select GDP columns and add national total
  # list_years |> print()
  gdp_default <- gdp_default |> select(all_of(gdpCols)) |> mutate(region = nationalDot)
  gdp_default <- gdp_default |> interpolate_annual(years=list_years, column = "gdp_usd", rule=1:2)
  gdp_default <- gdp_default |> select(-any_of(drop0))
  # gdp_default$year |> unique() |> print()
  rDataList[["gdp_default"]] <- gdp_default
  
  ### Interpolate annual values for population and add to data list
  pop_default <- co_defaultScenario |> select(all_of(popCols))
  pop_default <- pop_default        |> interpolate_annual(years=list_years, column=popColName, rule=1:2, byState=byState)
  rDataList[["pop_default"]] <- pop_default
  # pop_default |> names |> print
  
  ### Calculate region population totals
  if(byState){
    df_regPop   <- pop_default |> 
      group_by_at (c("year", "region")) |> 
      summarize_at(c(popColName), sum, na.rm=T) |> ungroup()
    df_regPop   <- df_regPop |> rename_at(c(popColName), ~c("reg_pop"))
    rDataList[["df_regPop"]] <- df_regPop
  }
  
  ### Calculate national population and add to data list
  df_national <- pop_default |> 
    group_by_at (c(group0)) |> 
    summarize_at(c(popColName), sum, na.rm=T) |> ungroup()
  df_national <- df_national |> rename_at(c(popColName), ~c("national_pop"))
  rDataList[["national_pop_default"]] <- df_national
  # df_national |> names |> print
  
  ### Calculate population ratios for nation to region and region to state
  df_popRatios <- pop_default |>
    left_join(df_regPop, by = c("region", "year")) |>
    left_join(df_national, by = "year") |>
    mutate(region_to_state = state_pop / reg_pop,
           conus_to_region = reg_pop / national_pop) |>
    select(-c(state_pop, reg_pop, national_pop))
  rDataList[["df_popRatios"]] <- df_popRatios

  ### Default scenario: Join national GDP with national population by year
  ### Default scenario: Join national values with regional population by year
  ### Calculate GDP per capita and add to list
  df_national <- gdp_default |> left_join(df_national, by=c(group0))
  df_national <- df_national |> left_join(pop_default, by=c(group0))
  df_national <- df_national |> mutate(gdp_percap = gdp_usd / national_pop)
  rDataList[["df_defaultScenario"]] <- df_national
  # df_defaultScenario |> names |> print
  ### Drop intermediate values
  rm(nationalDot, gdpCols, group0, drop0)
  rm(gdp_default, pop_default)
  # return(rDataList)

  ###### Extreme SLR Scenarios ######
  ### replace NA values and convert to character
  # slr_cm |> names |> print; slrImpacts |> names |> print; 
  mutate0     <- c("model_type")
  string0     <- c("slr")
  ### Replace NA values
  slr_cm      <- slr_cm     |> mutate_at(.vars = c(mutate0), replace_na, string0)
  # slrImpacts |> glimpse()
  slrImpacts  <- slrImpacts |> mutate_at(.vars = c(mutate0), replace_na, string0)
  ### Convert to character
  slr_cm      <- slr_cm     |> mutate_at(.vars = c(mutate0), as.character)
  slrImpacts  <- slrImpacts |> mutate_at(.vars = c(mutate0), as.character)
  ### Create data for extreme values above 250cm
  if(msgUser) {msg0(2) |> paste0("Creating extreme SLR impact values...") |> message()}
  slrExtremes <- fun_slrConfigExtremes(
    slr_x = slr_cm,    ### rDataList$slr_cm
    imp_x = slrImpacts ### rDataList$slrImpacts
  ) ### End fun_slrConfigExtremes
  # return(rDataList)
  
  ###### Interpolate SLR Scenarios ######
  ### Extend SLR Heights, Impacts, and Extremes
  c_cm         <- c("model", "year")
  c_imp        <- c("sector", "variant", "impactType", "impactYear", "region")
  if(byState){c_imp <- c_imp |>  c("state", "postal", "year")} else{ c_imp <- c_imp |> c("year")}
  ### Extend values 
  if(msgUser) {msg0(2) |> paste0("Extending SLR values...") |> message()}
  slr_cm       <- slr_cm      |> extend_slr()
  slrImpacts   <- slrImpacts  |> extend_slr()
  slrExtremes  <- slrExtremes |> extend_slr()

  ### Update in data list and remove objects
  rDataList[["slr_cm"     ]] <- slr_cm
  rDataList[["slrImpacts" ]] <- slrImpacts
  rDataList[["slrExtremes"]] <- slrExtremes
  rm(slr_cm, slrImpacts, slrExtremes); rm(c_cm, c_imp)
  # return(rDataList)
  
  ###### Format Scalar Tables ######
  ### Interpolate values to annual levels
  if(msgUser) {msg0(2) |> paste0("Formatting scalars...") |> message()}
  # scalarDataframe |> names |> print
  df_mainScalars <- fun_formatScalars(
    data_x  = scalarDataframe, ### rDataList$scalarDataframe
    info_x  = co_scalarInfo,   ### rDataList$co_scalarInfo
    years_x = list_years,      ### rDataList$list_years
    byState = byState
  ) ### End fun_formatScalars
  ### Add other info
  update_popScalars   <- utils::getFromNamespace("update_popScalars", "FrEDI")
  df_mainScalars <- df_mainScalars |> update_popScalars(df_national)
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
  # rm("c_scenariosList")
  ### Update in data list
  rDataList[["data_scaledImpacts"]] <- data_scaledImpacts
  # return(rDataList)
  
  ###### Get Interpolation Functions for Scenarios ######
  ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
  ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
  if(msgUser) {msg0(2) |> paste0("Creating list of impact functions...") |> message()}
  # if(msgUser) {paste0("\t", messages_data[["interpFuns"]]$try) |> message()}
  df_hasScenario       <- data_scaledImpacts |> filter(hasScenario)
  df_hasScenario       <- df_hasScenario     |> ungroup() #|> as.data.frame()
  c_modelTypes         <- c("gcm")
  ### Max output value, maximum extrapolation value, unit scale,   extend type
  df_gcm         <- (co_modelTypes |> filter(modelType_id==c_modelTypes))
  maxOutput_gcm  <- df_gcm[["modelMaxOutput"]][1]
  maxExtrap_gcm  <- df_gcm[["modelMaxExtrap"]][1]
  unitScale_gcm  <- df_gcm[["modelUnitScale"]][1]
  # "got here1" |> print(); #return(rDataList)
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
  # "got here2" |> print(); return(rDataList)
  ### Add values to list and remove intermediate values
  rDataList[["list_impactFunctions"]] <- functions_gcm
  rm("c_modelTypes", "maxOutput_gcm", "maxExtrap_gcm", "unitScale_gcm")
  rm("df_gcm", "df_hasScenario", "functions_gcm")
  ### Message the user
  if(msgUser) {msg0(1) |> paste0(messages_data[["interpFuns"]]$success) |> message()}
  
  ###### Add Config List ######
  ### Add config to data list
  rDataList[["fredi_config"]] <- fredi_config
  
  ###### Return object ######
  if(msgUser){msg0(1) |> paste0("...Finished running createSystemData()", ".") |> message()}
  return(rDataList)
} ### End function



