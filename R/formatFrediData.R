### This function formats FrEDI/extremes Impact Data
formatFrediData <- function(
    dataList    = list(), ### List of data created by reshapeData
    controlData = list(),
    configList  = frediConfig(),
    extend_all  = FALSE,  ### Whether to extend all GCM model observations to maximum range
    silent      = FALSE,  ### Level of messaging 
    msg0        = 0      ### Prefix for messaging
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  ### Level of messaging (default is to message the user) and save behavior
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msg0 |> get_msgPrefix(newline=F) |> paste0("Running formatFrediData()...") |> message()
  
  
  #### Configuration Data ----------------
  ### Assign configuration data to environment and add to list
  if(msgUser) msg1 |> get_msgPrefix() |> paste0("Loading configuration info...") |> message()
  for(name_i in configList |> names()) {name_i |> assign(configList[[name_i]]); rm(name_i)}
  dataList[["fredi_config"]] <- configList
  # fredi_config |> list2env(envir = environment())
  
  
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
  # national0     <- c("NationalTotal")
  slrStr0       <- c("slr")
  gcmStr0       <- c("gcm")

  #### Model types
  modTypes0     <- controlData[["co_moduleModTypes"]] |> 
    filter(module %in% moduleLC) |> 
    pull(model_type) |> unique() |> 
    tolower()
 
  
  ### Format Scaled Impacts ----------------
  listImpacts <- modTypes0 |> map(function(type0){
    msg1 |> get_msgPrefix() |> paste("Formatting ", type0 |> toupper(), " impacts...") |> message()
    name0  <- type0 |> paste0("Data")
    name
    data0  <- dataList[["stateData"]][[name0]]
    nRow0  <- data |> nrow() 
    ### If there is no data, return
    if(!nRow0) {return(list())}
    
    ### Otherwise, do model types
    list0  <- list()
    doSlr0 <- type0 |> str_detect(slrStr0)
    doGcm0 <- type0 |> str_detect(gcmStr0)
    if(doSlr0) {
      ### Format impacts
      if(msgUser) msg2 |> get_msgPrefix() |> paste("...Formatting SLR impact values...") |> message()
      dfX1    <- data0 |> format_slrImpacts(
        modLvls0 = controlData[["co_slrCm"]] |> pull(all_of(modCol0)), 
        yrCol0   = yrCol0,
        yCol0    = impactsCol0,
        idCol0   = idCol0,
        modCol0  = modCol0,
        group0   = c(mainCols0, regCols0) |> get_matches(y="state", matches=F),
        modStr0  = "Interpolation"
      ) ### End format_slrImpacts
      list0[["slrImpacts"]] <- dfX1
      
      ### Format extremes
      if(msgUser) msg2 |> get_msgPrefix() |> paste("...Creating extreme SLR impact values...") |> message()
      dfX2    <- dfX1 |> get_slrExtValues(
        df1      = controlData[["slrCmExtremes"]],
        xCol0    = yrCol0,
        yCol0    = impactsCol0,
        idCol0   = idCol0,
        modCol0  = modCol0
      ) |> fun_slrConfigExtremes() 
      list0[["slrExtremes"]] <- dfX2
      rm(dfX1, dfX2)
    } else if(doGcm0) {
      ### Extrapolate values
      ### - Value to extend to
      extend0 <- controlData[["co_modelTypes"]] |> 
        filter(model_type %in% gcmStr0) |> 
        pull(driverMaxOutput) |> 
        unique()
      ### Extend values
      data0   <- data0 |> extrapolate_gcmImpacts(
        groupCol   = idCol0, 
        xCol       = unitCol0, 
        yCol       = impactsCol0, 
        extend_to  = extend0,
        extend_all = extend_all,
        method0    = "linear",
        rule0      = 1
      ) ### End extrapolate_gcmImpacts
      list0[["gcmImpacts"]] <- data0
      
      ### Get impact functions
      funs0   <- data0 |> get_impactFunctions(
        groupCol   = idCol0,
        xCol       = unitCol0,
        yCol       = impactsCol0,
        method0    = "linear",
        rule0      = 1
      ) ### get_impactFunctions
      # list0[["gcmImpFuncs" ]] <- funs0
      list0[["gcmFunctions"]] <- funs0
      rm(data0, funs0)
    } ### End if(doSlr0)
    ### Return
    msg1 |> get_msgPrefix() |> paste("...Finished formatting ", type0 |> toupper(), " impacts...") |> message()
    return(list0)
  })
  
  
  
  ### Format Lists ----------------
  #### Drop some data from list
  ### Drop data sets that are no longer needed
  # drop0         <- c("scalarData", "gcmData", "slrData")
  # stateData     <- stateData |> (function(list0, x=drop0){list0[!((list0 |> names()) %in% x)]})()
  namesState <- dataList[["stateData"]] |> names()
  dataList[["stateData"]] <- dataList[["stateData"]][!(namesState %in% (modTypes0 |> paste0("Name")))]
  dataList[["stateData"]] <- dataList[["stateData"]] |> c(listImpacts)
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running formatFrediData().", msgN) |> message()
  gc()
  return(dataList)
} ### End function



