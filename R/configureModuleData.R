configureModuleData <- function(
    module      = "fredi", ### Module string
    dataDir     = "." |> file.path("inst", "extdata", "fredi"), ### Path to project
    configFile  = "FrEDI_config.xlsx", ### Name of excel file with config information
    configSheet = "tableNames",        ### Sheet with table info
    outDir      = "." |> file.path("data", "fredi"),
    outExt      = "rda",
    controlData = NULL , ### Output of configureControlTables
    configList  = frediConfig(),
    doScalars   = TRUE ,  ### Whether to load scalars
    extend_all  = TRUE ,  ### Whether to extend all GCM model observations to maximum range
    reshape     = TRUE ,  ### Whether to include reshaped data items in data list (for testing)
    silent      = TRUE ,  ### Level of messaging 
    return      = TRUE ,  ### Whether to return the data list
    save        = TRUE ,  ### Whether to save the file
    msg0        = 0    ,  ### Message prefix
    .testing    = FALSE
){
  ### Set Up Environment ----------------
  #### Messaging ----------------
  msgUser       <- !silent
  msgN          <- "\n"
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  msg0 |> get_msgPrefix(newline=F) |> paste0("Running configureModuleData()...") |> message()
  
  #### File Paths ----------------
  ### Output file
  # sysDataFile   <- outPath |> basename()
  # sysDataPath   <- outPath |> dirname()
  # sysDataFile   <- sysDataPath |> file.path(sysDataFile)
  
  
  #### Columns & Values ----------------
  ### Rename values
  reshape0    <- reshape
  save0       <- save
  return0     <- return
  msgUser     <- !silent
  rm(reshape, save, return)
  
  ### Module and list object names
  moduleLC      <- module   |> tolower()
  # listStr0      <- "rDataList"
  # configLStr0   <- moduleLC |> paste0("Data")
  listStr0      <- moduleLC |> paste0("Data")
  configLStr0   <- "configData"
  stateLStr0    <- "stateData"
  rsLStr0       <- "rsData"
  
  ### Module info
  modules0      <- controlData[["co_moduleInfo"]] |> pull(module)
  for(moduleX in modules0){moduleX |> paste0("Str0") |> assign(moduleX)}
  doFredi       <- moduleLC |> str_detect(frediStr0   )
  doExtremes    <- moduleLC |> str_detect(extremesStr0)
  doGHG         <- moduleLC |> str_detect(ghgStr0     )
  doSV          <- moduleLC |> str_detect(svStr0      )
  
  #### Initialize List ----------------
  ### Initialize list of objects to return
  dataList      <- list()
  dataList[[configLStr0]] <- list()
  dataList[[stateLStr0 ]] <- list()

  
  ### 1. Load Excel Data ----------------
  ### Load state data
  # if(!silent) 
  # msg1 |> get_msgPrefix(newline=F) |> paste0("Loading data...") |> message()
  dataList      <- module |> loadModuleData(
    dataDir     = dataDir,
    configFile  = configFile,  ### Name of excel file with config information
    configSheet = configSheet, ### Sheet with info about tables in config file
    controlData = controlData,
    doScalars   = doScalars,
    silent      = silent,
    msg0        = msg1,
    .testing    = .testing
  ) ### End loadData
  # gc()
  return(dataList)
  
  ### 2. Reshape Data ----------------
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0("Reshaping data...") |> message()
  if(doFredi | doExtremes) {
    dataList  <- module |> reshapeFrediData(
      dataList    = dataList,
      controlData = controlData,
      doScalars   = doScalars,
      silent      = silent, 
      msg0        = msg2
    ) ### End reshapeFrediData
  } else if(doGHG) {
    dataList  <- dataList |> reshapeGhgData  (silent=silent, msg0=msg1)
  } ### End if(doFredi | doExtremes)
  gc()
  
  ### If reshape, save a copy of the reshaped data
  if(reshape) {reshapeData0 <- dataList} 
  else        {reshapeData0 <- NULL  }
  
  
  ### 4. Configure Data ----------------
  ### Names of objects to combine from reshaped data
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0("Configuring data...") |> message()
  if(doFredi | doExtremes) {
    dataList  <- dataList |> formatFrediData(
      controlData = controlData,
      configList  = configList,
      extend_all  = extend_all, 
      silent      = silent, 
      msg0        = msg1
    ) ### End formatFrediData
  } else if(doGHG) {
    dataList  <- dataList |> formatGhgData  (silent=silent, msg0=msg1)
  } ### End if(doFredi | doExtremes)
  
  
  ### 6. Add Reshaped Data ----------------
  if(reshape) {
    dataList[[rsLStr0]] <- reshapeData0
  } ### End if(!reshape) 
  
  ### Save to File ----------------
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Saving results to ", outDir, "...") |> message()
    pathExists <- outDir |> dir.exists()
    if(!pathExists) {createDir <- outDir |> dir.create(recursive=T)}
    listStr0 |> assign(dataList)
    save(list=c(listStr0), file=outPath)
  } else {
    if(msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("Exiting without saving...") |> message()
  } ### End if(save)
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running configureModuleData().") |> paste0(msgN) |> message()
  gc()
  return(dataList)
}