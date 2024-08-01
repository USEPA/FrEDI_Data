configureSystemData <- function(
    fileDir     = "." |> file.path("inst", "extdata"),         ### Path to project
    configFile  = "FrEDI_config.xlsx",                         ### Name of excel file with config information
    configSheet = "tableNames",                                ### Sheet with table info
    # configPath  = "." |> file.path("R", "fredi_config.R"),     ### Path to config file
    outPath     = "." |> file.path("data", "tmp_sysdata.rda"), ### Where to save data
    extend_all  = FALSE,  ### Whether to extend all GCM model observations to maximum range
    reshape     = TRUE,   ### Whether to include reshapeData items in data list (for testing)
    silent      = TRUE,   ### Level of messaging 
    return      = TRUE,   ### Whether to return the data list
    save        = FALSE   ### Whether to save the file
){
  ### Messages
  "Running configureSystemData()..." |> message()
  msg0 <- "\t"
  msg1 <- msg0 |> rep(2) |> paste0(collapse="")
  msg2 <- msg0 |> rep(3) |> paste0(collapse="")
  
  ###### Create File Paths ######
  ### Output file
  sysDataFile   <- outPath |> basename()
  sysDataPath   <- outPath |> dirname()
  sysDataFile   <- sysDataPath |> file.path(sysDataFile)
  
  ###### Initialize Object List ######
  ### Initialize list of objects to return
  rDataList     <- list()
  rDataList[["frediData"]] <- list(name="frediData", data=list())
  rDataList[["stateData"]] <- list(name="stateData", data=list())
  
  if(reshape) { 
    # rDataList[["rsData_reg"  ]] <- list(name="rsData_reg"  , data=list())
    rDataList[["rsData_state"]] <- list(name="rsData_state", data=list())
  } ### End if(reshape)
  
  ###### Message User ######
  # (!silent) |> paste0(msg0, "Creating FrEDI data...") |> message()
  
  ###### 1. Load Excel Data ######
  # ### Load region data
  # (!silent) |> ifelse("\n", "") |> paste0(msg1, "Loading region-level data...") |> message()
  # loadReg0      <- loadData(
  #   fileDir   = fileDir,   ### Path to project
  #   fileName  = fileName,  ### Name of excel file with config information
  #   sheetName = sheetName, ### Sheet with info about tables in config file
  #   byState   = FALSE,
  #   silent    = silent
  # ) ### End loadData
  
  ### Load state data
  if(!silent) paste0(msg0, "Loading data...") |> message()
  loadState0    <- fileDir |> loadFrediData(
    configFile  = configFile,  ### Name of excel file with config information
    configSheet = configSheet, ### Sheet with info about tables in config file
    silent      = silent,
    msg0        = msg1
  ) ### End loadData
  
  
  ###### 2. Reshape Loaded Data ######
  # ### Reshape region data
  # (!silent) |> ifelse("\n", "") |> paste0(msg1, "Reshaping region-level data...") |> message()
  # reshapeReg0   <- loadReg0|> reshapeData(byState=F, silent=silent)
  # if(reshape){
  #   rDataList[["rsData_reg"]] <- reshapeReg0
  # } ### End if(reshape)
  # rm(loadReg0)
  
  ### Reshape state data
  if(!silent) paste0(msg0, "Reshaping data...") |> message()
  # reshapeState0 <- loadState0 |> reshapeData(byState=T, silent=silent)
  reshapeState0 <- loadState0 |> reshapeFrediData(silent=silent, msg0=msg1)
  if(reshape){ 
    rDataList[["rsData_state"]] <- reshapeState0
  } ### End if(reshape)
  rm(loadState0)
  
  ###### 3. Combine Reshaped Data ######
  # ### Names of objects to combine from reshaped data
  # (!silent) |> ifelse("\n", "") |> paste0(msg1, "Combining region- & state-level data...") |> message()
  # colsReshape0  <- c("scalarDataframe", "data_scaledImpacts", "slrImpacts")
  # listReshape0  <- reshapeReg0 |> combineReshapedLists(stateList0=reshapeState0)
  # rm(reshapeReg0, reshapeState0)
  
  ###### 4. Configure Data ######
  # ### Names of objects to combine from reshaped data
  # (!silent) |> paste0(msg0, "Configuring data...") |> message()
  # sysDataList0  <- listReshape0 |> createSystemData(byState=T, extend_all=extend_all, save=F, silent=silent)
  # rm(listReshape0)
  
  ###### 5. FrEDI Data  ######
  ### List of data that is the same for both state & region and not modified in configuration steps
  # frediNames0   <- c("fredi_config") |>
  #   c("co_sectors", "co_sectorsRef") |> 
  #   c("co_variants", "co_impactTypes", "co_impactYears", "co_impactYearLevels") |> 
  #   c("co_regions", "co_states", "co_models", "co_modelTypes", "co_inputInfo") |>
  #   c("co_econMultipliers", "co_scalarInfo", "co_slrScalars") |>
  #   c("slr_cm") |> 
  #   c("testDev")
  # ### Update objects
  # # sysDataList0 |> names() |> print()
  # frediList0    <- sysDataList0 |> (function(x){x[  names(x) %in% frediNames0 ]})()
  # sysDataList0  <- sysDataList0 |> (function(x){x[!(names(x) %in% frediNames0)]})()
  # ### Update in list
  # rDataList[["frediData"]][["data"]] <- frediList0
  # rDataList[["stateData"]][["data"]] <- sysDataList0
  # rm(frediNames0, frediList0, sysDataList0)
  
  ###### Drop Reshaped Data Objects ######
  if(!reshape) {
    drop0         <- c("rsData_reg", "rsData_state")
    inDrop0       <- (rDataList |> names()) %in% drop0
    returnList    <- rDataList
    rDataList     <- rDataList[!inDrop0]
  } ### End if(!reshape) 
  
  ###### Save to File ######
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save) {
    msg0 |> paste0("Saving results to ", sysDataPath, "...") |> message()
    outPathExists <- sysDataPath |> dir.exists()
    fredi_config  <- rDataList[["frediData"]][["data"]][["fredi_config"]]
    if(outPathExists){ 
      save(fredi_config, rDataList, file=sysDataFile)
    } else{
      paste0(msg0, "Warning: outPath = ", sysDataPath, "doesn't exist!") |> message()
      paste0(msg0, msg0, "Exiting without saving...") |> message()
    } ### End if(outPathExists)
  } ### End if(save)
  
  ###### Return ######
  "...Finished running configureSystemData()." |> message()
  return(returnList)
}