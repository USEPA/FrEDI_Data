### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
    dataList    = list(), ### List of data created by reshapeData
    extend_all  = FALSE,  ### Whether to extend all GCM model observations to maximum range
    silent      = FALSE,  ### Level of messaging 
    msg0        = ""      ### Prefix for messaging
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  ### Level of messaging (default is to message the user) and save behavior
  msgUser    <- !silent
  msgN       <- "\n"
  paste0(msg0, "Running createSystemData()...") |> message()
  msg1       <- msg0 |> paste0("\t")
  msg2       <- msg1 |> paste0("\t")
  msg3       <- msg2 |> paste0("\t")
  msg4       <- msg3 |> paste0("\t")
  # lvl0       <- msg0 |> str_count(pattern="\\t")
  # msg0       <- function(lvl0=1){"\t" |> rep(lvl0) |> paste(collapse="")}
  if(msgUser) msg0 |> paste0("Running createSystemData()...") |> message()
  
  # dataList |> names() |> print()
  # dataList[["frediData"   ]] |> names() |> print()
  
  
  #### Initialize Data List ----------------
  ### Initialize list of data to save
  rDataList  <- list()
  rDataList  <- rDataList |> c(dataList)
  
  #### Configuration Data ----------------
  ### Get config info, add data to list, assign objects to environment
  if(msgUser) msg1 |> paste0("Loading configuration info...") |> message()
  fredi_config <- frediConfig()
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}
  rDataList[["fredi_config"]] <- fredi_config
  # fredi_config |> list2env(envir = environment())
  
  
  #### Assign Data Objects ----------------
  ###### This section reads in data from the data file and returns a list of tables
  ### Add tables to data list
  # # if(msgUser) {msg1 |> paste0(messages_data[["loadInputs"]][["success"]]) |> message()}
  # # dataList |> list2env(envir = environment())
  # # dataList |> names() |> print()
  # if(msgUser) 
  msg1 |> paste0("Configuring scenarios...") |> message()
  frediData     <- dataList[["frediData"   ]]
  scenarios     <- dataList[["scenarioData"]]
  stateData     <- dataList[["stateData"   ]]
  ### Data names
  frediNames    <- frediData  |> names()
  scenarioNames <- scenarios  |> names()
  stateNames    <- stateData  |> names()
  # frediNames |> print(); scenarioNames |> print(); stateNames |> print()
  ### Assign objects
  for(name_i in frediNames){ name_i |> assign(frediData[[name_i]]); rm(name_i) }
  
  
  #### Columns & Values ----------------
  #### Columns
  # stateCols0    <- c("state", "postal")
  mainCols0     <- c("sector", "variant", "impactType", "impactYear")
  regCols0      <- c("region", "state", "postal")
  mTypeCol0     <- c("model_type")
  modCol0       <- c("model")
  popCol0       <- c("pop")
  yrCol0        <- c("year")
  unitCol0      <- c("modelUnitValue")
  impactsCol0   <- c("scaled_impacts")
  idCol0        <- c("scenario_id")

  #### Values
  # sectors0   <- co_sectors |> pull() |> unique()
  national0     <- c("NationalTotal")
  slrStr0       <- c("slr")
  gcmStr0       <- c("gcm")

  
  ### Scenarios ----------------
  #### GCAM Scenarios ----------------
  # ### Get reference years and add to fredi_config
  # if(msgUser) msg2 |> paste0("Formatting GCAM scenarios...") |> message()
  # # co_modelTypes |> glimpse()
  # refYear0       <- co_modelTypes |> pull(modelRefYear) |> min()
  # ### Default temperature scenario
  # ### Columns, years for interpolation
  # gcamData       <- scenarios[["gcamData"]]
  # # gcamData |> glimpse()
  # gcam_scenarios <- gcamData       |> format_gcamData()
  # gcam_default   <- gcam_scenarios |> filter(year >= refYear0) |> filter(scenario == "ECS_3.0_REF")
  # ### Add to list, remove intermediate values
  # scenarios[["gcam_scenarios"]] <- gcam_scenarios
  # scenarios[["gcam_default"  ]] <- gcam_default
  # rm(refYear0, gcamData)


  #### Socioeconomic Scenario ----------------
  # # if(msgUser) msg2 |> paste0("Creating socioeconomic scenario...") |> message()
  # # 
  # # ### Interpolate annual values for GDP:
  # # ### Filter to first unique region
  # # ### Select GDP columns and add national total
  # # gdpData     <- scenarios[["gdpData"]]
  # # gdp_default <- gdpData |> interpolate_gdp()
  # # scenarios[["gdp_default"]] <- gdp_default
  # # rm(gdpData)
  # # 
  # # ### Interpolate annual values for population and add to data list
  # # popData     <- scenarios[["popData"]]
  # # pop_default <- popData |> interpolate_pop()
  # # scenarios[["pop_default"]] <- pop_default
  # # rm(popData)
  # # # pop_default |> names() |> print()
  # # 
  # # ### Default socioeconomic scenario:
  # # ### Use to assess default scalars but don't add to list
  # # pop_default <- pop_default |> mutate(region = region |> str_replace_all(" ", ""))
  # # df_national <- gdp_default |> create_nationalScenario(pop0=pop_default)
  # # # df_national |> names() |> print()
  # # rm(gdp_default, pop_default)
  # 
  
  
  ### Format Scalars ----------------
  # ### Interpolate values to annual levels
  # # if(msgUser) 
  # msg1 |> paste0("Formatting scalars...") |> message()
  # # scalarDataframe |> names() |> print()
  # ### Get data
  # scalars     <- stateData[["scalarData"]]
  # ### df_mainScalars
  # df_scalars  <- fun_formatScalars(
  #   data_x  = scalars,          ### rDataList$scalarDataframe
  #   info_x  = co_scalarInfo,    ### rDataList$co_scalarInfo
  
  
  
  
  ### Format Scaled Impacts ----------------
  #### Format SLR Data ----------------
  # if(msgUser) 
  msg1 |> paste0("Formatting SLR data...") |> message()
  slrImpData  <- stateData[["slrImpData"]]
  doSlr       <- "slrImpData" |> nrow()
  if(doSlr) {
    # ### Standardize SLR scenario info and update in list
    # if(msgUser) msg2 |> paste0("...Standardizing SLR scaled impacts...") |> message()
    # slrImpData  <- stateData[["slrImpData"]] |> standardize_scaledImpacts(df1=co_sectorsInfo, xCol=yrCol0)
    # slrImpData  <- stateData[["slrImpData"]]
    ### Add model levels to SLR
    ### Arrange and group
    slrImpacts  <- slrImpData |> format_slrImpacts(
      modLvls0 = co_slrCm |> pull(all_of(modCol0)), 
      yCol0    = yCol0,
      group0   = mainCols0 |> c(regCols0),
      yrCol0   = yrCol0,
      modCol0  = modCol0,
      idCol0   = idCol0,
      modStr0  = "Interpolation"
    ) ### End format_slrImpacts
    
    
    ### Create data for extreme values above 250cm
    if(msgUser) msg2 |> paste0("...Creating extreme SLR impact values...") |> message()
    # slrExtremes <- fun_slrConfigExtremes(slr_x=slr_cm, imp_x=slrImpData)
    slrExtremes <- slrCmExtremes |> 
      get_slrExtValues(df1=slrImpData) |> 
      fun_slrConfigExtremes()
    ### Add to list
    stateData[["slrExtremes"]] <- slrExtremes
    # return(rDataList)
    
    # ### Extend/Interpolate SLR Heights, Impacts, and Extremes
    # ### Add grouping/sorting columns
    # if(msgUser) msg2 |> paste0("...Extending SLR values...") |> message()
    # slr_cm      <- slr_cm      |> extend_slr()
    # slrImpacts  <- slrImpData  |> extend_slr()
    # slrExtremes <- slrExtremes |> extend_slr()
    
    # ### Add other values back into slrImpacts
    # include0    <- c("region") |> c(stateCols0) # |> c("model")
    # slrImpacts  <- slrImpacts  |> get_scenario_id(include=include0) |> paste0()
    # slrExtremes <- slrExtremes |> get_scenario_id(include=include0) |> ungroup()
    # rm(include0)
    # ### Mutate column
    # slrImpacts  <- slrImpacts  |> mutate(scenario_id = scenario_id |> paste0("_", "Interpolation"))
    # slrExtremes <- slrExtremes |> mutate(scenario_id = scenario_id |> paste0("_", "Interpolation"))
    # ### Get groups
    # idsImpacts  <- slrImpacts  |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
    # idsExtremes <- slrExtremes |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
    # ### Create column with valid values
    # slrImpacts  <- slrImpacts  |> mutate(hasScenario = scenario_id %in% idsImpacts)
    # slrExtremes <- slrExtremes |> mutate(hasScenario = scenario_id %in% idsExtremes)
    # rm(idsImpacts, idsExtremes)
    # 
    # ### Update in data list and remove objects
    # frediData[["slr_cm"     ]] <- slr_cm
    # stateData[["slrImpacts" ]] <- slrImpacts
    # stateData[["slrExtremes"]] <- slrExtremes
    # rm(slr_cm, slrExtremes, slrImpData)
    # # return(list(frediData=frediData, stateData=stateData))
  } ### End if(doSlr)
  
  
  
  
  
  
  
  
  #### Format GCM Data ----------------
  # if(msgUser) 
  gcmImpData  <- stateData[["gcmImpData"]]
  doGcm       <- "gcmImpData" |> nrow()
  if(doGcm) {
    msg1 |> paste0("Formatting GCM data...") |> message()
    # ### Standardize GCM scaled impacts data
    # if(msgUser) msg2 |> paste0("...Standardizing GCM scaled impacts...") |> message()
    # gcmImpData  <- stateData[["gcmImpData"]] |> standardize_scaledImpacts(df1=co_sectorsInfo, xCol=idCol0)
    # stateData[["gcmImpData"]] <- gcmImpData
    # co_gcm      <- co_models  |> (function(df0, cols0=c("model", "driverDataMax", "driverMaxOutput")){
    #   df0 |> 
    #     filter(model_type %in% gcmStr0) |>
    #     select(all_of(cols0))
    # })
    # gcmImpData  <- gcmImpData |> left_join(co_gcm)
    
    
    ### Separate GCM impact data into values with and without scenarios
    if(msgUser) msg2 |> paste0("...Creating list of groups with non-missing values...") |> message()
    # gcmImpData |> pull(region) |> unique() |> print()
    # gcmImpData |> glimpse()
    # gcmNone    <- gcmImpData |> filter(hasScenario != 1)
    gcmData    <- gcmImpData |> filter(hasScenario == 1)
    rm(gcmImpData)
    
    ### Get Interpolation Functions for Scenarios
    ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
    ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
    ### - Max output value, maximum extrapolation value, unit scale, extend type
    if(msgUser) msg2 |> paste0("...Creating list of impact functions for GCM groups...") |> message()
    # df_gcm        <- co_modelTypes |> filter(modelType_id=="gcm")
    # maxOutput_gcm <- df_gcm |> pull(modelMaxOutput)
    # maxExtrap_gcm <- df_gcm |> pull(modelMaxExtrap)
    # # unitScale_gcm  <- df_gcm |> pull(modelUnitScale)
    
    ### - Extend data
    gcmData   <- gcmData |> extrapolate_gcmImpacts(
      groupCol    = idCol0, 
      xCol        = unitCol0, 
      yCol        = impactsCol0, 
      extend_to   = co_modelTypes |> filter(model_type %in% gcmStr0) |> pull(driverMaxOutput) |> unique(),
      extend_all  = extend_all,
      method0     = "linear",
      rule0       = 1
    ) ### End extrapolate_gcmImpacts
    ### Update in list
    stateData[["gcmImpacts" ]] <- gcmData
    # gcmImpData <- gcmNone |> bind_rows(gcmData)
    # stateData[["gcmImpData"]] <- gcmImpData
    # rm(gcmImpData)
    
    ### - Get impact functions
    gcmFuns   <- gcmData |> get_impactFunctions(
      groupCol    = idCol0,
      xCol        = unitCol0,
      yCol        = impactsCol0,
      method0     = "linear",
      rule0       = 1
    ) ### get_impactFunctions
    ### Update in list
    stateData[["gcmImpFuncs" ]] <- gcmData
    
    # ### Add values to list and remove intermediate values
    # ### list_impactFunctions
    # stateData[["gcmImpacts" ]] <- gcmImpacts
    # stateData[["gcmImpFuncs"]] <- gcmImpFuncs
    # rm(df_gcm, gcmImpacts, gcmImpFuncs)
    ### Message the user
    if(msgUser) msg4 |> paste0(messages_data[["interpFuns"]]$success) |> message()
  } ### End if (doGcm)
  
  ### Format Drop Data ----------------
  #### Drop some data from list
  ### Drop data sets that are no longer needed
  # drop0         <- c("scalarData", "gcmImpData", "slrImpData")
  # stateData     <- stateData |> (function(list0, x=drop0){list0[!((list0 |> names()) %in% x)]})()
  
  #### Update Data List
  # if(msgUser) 
  msg1 |> paste0("Updating data in lists...") |> message()
  rDataList[["frediData"   ]] <- frediData
  rDataList[["stateData"   ]] <- stateData
  rDataList[["scenarioData"]] <- scenarios
  # scenarios |> names() |> print()
  
  ### Return ----------------
  paste0(msg0, "...Finished running createSystemData().", msgN) |> message()
  gc()
  return(rDataList)
} ### End function



