### This function formats FrEDI/extremes Impact Data
formatFrediData <- function(
    module      = "fredi",
    dataList    = list(), ### List of data created by reshapeData
    controlData = list(),
    extend_all  = TRUE, ### Whether to extend all GCM model observations to maximum range
    # dropImpData = TRUE , ### Whether to drop original impact data
    silent      = FALSE, ### Level of messaging 
    msg0        = 0      ### Prefix for messaging
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  ### Level of messaging (default is to message the user) and save behavior
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msg3          <- msg0 + 3
  msg0 |> get_msgPrefix(newline=F) |> paste0("Running formatFrediData()...") |> message()
  
  
  #### Columns & Values ----------------
  ### Module and list object names
  moduleLC      <- module   |> tolower()
  # listStr0      <- "rDataList"
  # listStr0      <- moduleLC |> paste0("Data")
  # configLStr0   <- moduleLC |> paste0("Data")
  fConfigStr0   <- "fredi_config"
  configLStr0   <- "configData"
  stateLStr0    <- "stateData"
  
  #### Columns
  # stateCols0    <- c("state", "postal")
  mainCols0     <- c("sector", "variant", "impactType", "impactYear")
  regCols0      <- c("region", "postal")
  groupCols0    <- mainCols0 |> c(regCols0)
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
    pull(all_of(mTypeCol0)) |> unique() |> 
    tolower()
  
  
  #### Configuration Data ----------------
  ### Assign configuration data to environment and add to list
  # if(msgUser) msg1 |> get_msgPrefix() |> paste0("Assigning configuration info...") |> message()
  # configList <- dataList[[fConfigStr0]]
  # for(name_i in configList |> names()) {name_i |> assign(configList); rm(name_i)}
  # dataList[["fredi_config"]] <- configList
  
  
  ### Format Scaled Impacts ----------------
  # dataList |> glimpse()
  msg1 |> get_msgPrefix() |> paste0("Formatting scaled impacts...") |> message()
  stateData   <- dataList[[stateLStr0]]
  dataList    <- dataList[dataList |> names() |> get_matches(y=stateLStr0, matches=F)]
  modNames0   <- modTypes0 |> map(function(x, y=stateData |> names()){
    y[y |> str_detect(x)]
  }) |> unlist()
  listImpacts <- list(type0=modTypes0, name0=modNames0) |> pmap(function(type0, name0){
    msg2 |> get_msgPrefix() |> paste0("Formatting ", type0 |> toupper(), " impacts...") |> message()
    ### Values
    doSlr0 <- type0 |> str_detect(slrStr0)
    # name1  <- type0 |> paste0("Impacts")
    # name1  <- type0 |> paste0(case_when(doSlr0 ~ "Impacts", .default="Data"))
    name1  <- type0 |> paste0("Data")
    name2  <- type0 |> paste0(case_when(doSlr0 ~ "Extremes", .default="Funs"))
    
    ### Initialize data
    list0  <- list()
    df0    <- stateData[[name0]]
    nRow0  <- df0 |> nrow() 
    ### If there is no data, return
    if(!nRow0) {
      list0[[name1]] <- df0
      list0[[name2]] <- df0
      return(list0)
    } ### End if(!nRow0)
    
    ### Otherwise, do model types
    if(doSlr0) {
      ### Format impacts
      # if(msgUser) msg2 |> get_msgPrefix() |> paste("Formatting SLR impact values...") |> message()
      list0 <- df0 |> format_slrScaledImpacts(
        dfExt0   = controlData[["slrCmExtremes"]],
        xCol0    = yrCol0,
        yCol0    = impactsCol0,
        idCol0   = idCol0,
        modCol0  = modCol0,
        group0   = groupCols0,
        modStr0  = "Interpolation",
        silent   = silent,
        msg0     = msg2
      ) |> set_names(c(name1, name2)) ### End format_slrImpacts
      rm(df0)
    } else {
      ### Extrapolate values
      ### - Value to extend to
      # extend_all |> print()
      extend0 <- controlData[["co_modelTypes"]] |> 
        filter(model_type %in% type0) |> 
        pull(driverMaxOutput) |> 
        unique() |> as.numeric()
      ### Extend values
      list0   <- df0 |> format_gcmScaledImpacts(
        xCol0   = unitCol0, 
        yCol0   = impactsCol0, 
        idCol0  = idCol0, 
        to0     = extend0, ### Value to extend values to
        all0    = extend_all, ### Whether to extrapolate cold models, too
        method0 = "linear",
        rule0   = 1,
        silent   = silent,
        msg0     = msg2
      )|> set_names(c(name1, name2)) ### End extrapolate_gcmImpacts
    } ### End if(doSlr0)
    ### Return
    msg3 |> get_msgPrefix() |> paste0("...Finished formatting ", type0 |> toupper(), " impacts...") |> message()
    return(list0)
  }) |> unlist(recursive=F)
  
  
  ### Format Lists ----------------
  #### Drop some data from list
  ### Drop data sets that are no longer needed
  # drop0         <- c("scalarData", "gcmData", "slrData")
  # if(dropImpData) stateData <- stateData[stateData |> names() |> get_matches(y=modNames0, matches=F, type="matches")]
  namesImpacts <- listImpacts |> names()
  namesState   <- stateData   |> names()
  stateData    <- stateData[namesState |> get_matches(namesImpacts, matches=F)] |> c(listImpacts)
  rm(listImpacts)
  dataList[[stateLStr0]] <- stateData
  rm(stateData)
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running formatFrediData().", msgN) |> message()
  gc()
  return(dataList)
} ### End function


# listImpacts <- list(type0=modTypes0, name0=modNames0) |> pmap(function(type0, name0){
#   msg2 |> get_msgPrefix() |> paste("Formatting ", type0 |> toupper(), " impacts...") |> message()
#   ### Values
#   doSlr0 <- type0 |> str_detect(slrStr0)
#   # name1  <- type0 |> paste0("Impacts")
#   name1  <- type0 |> paste0(case_when(doSlr0 ~ "Data", .default="Funs"))
#   name2  <- type0 |> paste0(case_when(doSlr0 ~ "Extremes", .default="Funs"))
#   
#   ### Initialize data
#   list0  <- list()
#   df0    <- stateData[[name0]]
#   nRow0  <- df0 |> nrow() 
#   ### If there is no data, return
#   if(!nRow0) {
#     list0[[name1]] <- df0
#     list0[[name2]] <- df0
#     return(list0)
#   } ### End if(!nRow0)
#   
#   ### Otherwise, do model types
#   if(doSlr0) {
#     ### Format impacts
#     # if(msgUser) msg2 |> get_msgPrefix() |> paste("Formatting SLR impact values...") |> message()
#     dfX1    <- df0 |> format_slrImpacts(
#       yrCol0   = yrCol0,
#       yCol0    = impactsCol0,
#       idCol0   = idCol0,
#       modCol0  = modCol0,
#       group0   = groupCols0,
#       modStr0  = "Interpolation"
#     ) ### End format_slrImpacts
#     rm(df0)
#     list0[[name1]] <- dfX1
#     
#     ### Format extremes
#     # if(msgUser) msg2 |> get_msgPrefix() |> paste("Creating extreme SLR impact values...") |> message()
#     dfX2    <- dfX1 |> get_slrExtValues(
#       df1      = controlData[["slrCmExtremes"]],
#       xCol0    = yrCol0,
#       yCol0    = impactsCol0,
#       idCol0   = idCol0,
#       modCol0  = modCol0
#     ) |> fun_slrConfigExtremes() 
#     list0[[name2]] <- dfX2
#     rm(dfX1, dfX2)
#   } else {
#     ### Extrapolate values
#     ### - Value to extend to
#     extend0 <- controlData[["co_modelTypes"]] |> 
#       filter(model_type %in% type0) |> 
#       pull(driverMaxOutput) |> 
#       unique()
#     ### Extend values
#     df0     <- df0 |> extrapolate_gcmImpacts(
#       idCol0     = idCol0, 
#       xCol0      = unitCol0, 
#       yCol0      = impactsCol0, 
#       extend_to  = extend0,
#       extend_all = extend_all,
#       method0    = "linear",
#       rule0      = 1
#     ) ### End extrapolate_gcmImpacts
#     list0[[name1]] <- data0
#     
#     ### Get impact functions
#     funs0   <- df0 |> get_impactFunctions(
#       idCol0  = idCol0,
#       xCol0   = unitCol0,
#       yCol0   = impactsCol0,
#       method0 = "linear",
#       rule0   = 1
#     ) ### get_impactFunctions
#     # list0[["gcmImpFuncs" ]] <- funs0
#     list0[[name2]] <- funs0
#     rm(df0, funs0)
#   } ### End if(doSlr0)
#   ### Return
#   msg2 |> get_msgPrefix() |> paste("...Finished formatting ", type0 |> toupper(), " impacts...") |> message()
#   return(list0)
# })
