configureSystemData <- function(
    fileDir    = "." |> file.path("inst", "extdata"), ### Path to project
    fileName   = "FrEDI_config.xlsx", ### name of excel file with config information
    sheetName  = "tableNames",
    configPath = "." |> file.path("R"   , "fredi_config.R"), ### Path to config file
    extend_all = FALSE,
    save       = FALSE,  ### Whether to message the user
    outPath    = "." |> file.path("data", "tmp_sysdata.rda"),
    return     = TRUE,
    silent     = TRUE,
    reshape = TRUE  # Return reshapeData
){
  "Running configureSystemData()..." |> message()
  msg0 <- "\t"
  msg1 <- "\t" |> rep(2) |> paste0(collapse="")
  ###### Create File Paths ######
  ### Output file
  outPath       <- if (is.null(outPath)) {"." |> file.path("data", "tmp_sysdata.rda")} else {outPath}
  sysDataFile   <- outPath |> basename()
  sysDataPath   <- outPath |> dirname()
  sysDataFile   <- sysDataPath |> file.path(sysDataFile)
  
  ###### Initialize Object List ######
  ### Initialize list of objects to return
  rDataList     <- list()
  rDataList[["frediData" ]] <- list(name="frediData" , data=list())
  # rDataList[["regionData"]] <- list(name="regionData", data=list())
  rDataList[["stateData" ]] <- list(name="stateData" , data=list())
  
  if(reshape){ 
    rDataList[["rsData_reg" ]] <- list(name="rsData_reg" , data=list())
    rDataList[["rsData_state" ]] <- list(name="rsData_state" , data=list())
  }
  
  ###### Message User ######
  (!silent) |> ifelse("\n", "") |> paste0(msg0, "Creating FrEDI data...") |> message()
  
  ###### 1. Load Excel Data ######
  ### Load region data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Loading region-level data...") |> message()
  loadReg0      <- loadData( 
    fileDir   = fileDir,    ### Path to project
    fileName  = fileName,   ### name of excel file with config information
    sheetName = sheetName, ### Sheet with info about tables in config file
    byState   = FALSE,
    silent    = silent
  )
  
  ### Load state data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Loading state-level data...") |> message()
  
  loadState0    <- loadData(
    fileDir   = fileDir,    ### Path to project
    fileName  = fileName,   ### name of excel file with config information
    sheetName = sheetName, ### Sheet with info about tables in config file
    byState   = TRUE,
    silent    = silent
  )
  
  
  ###### 2. Reshape Loaded Data ######
  ### Reshape region data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Reshaping region-level data...") |> message()
  reshapeReg0   <- loadReg0   |> reshapeData(byState=F, silent=silent)
  if(reshape){ 
    rDataList[["rsData_reg" ]] <- reshapeReg0
  }
  rm(loadReg0)
  
  ### Reshape state data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Reshaping state-level data...") |> message()
  reshapeState0 <- loadState0 |> reshapeData(byState=T, silent=silent)
  if(reshape){ 
    rDataList[["rsData_state" ]] <- reshapeState0
  }
  rm(loadState0)
  # "got here1" |> print(); return(reshapeState0)
  
  ###### 3. Combine Reshaped Data ######
  ### Names of objects to combine from reshaped data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Combining region- & state-level data...") |> message()
  colsReshape0  <- c("scalarDataframe", "data_scaledImpacts", "slrImpacts")
  listReshape0  <- reshapeReg0 |> combineReshapedLists(stateList0=reshapeState0)
  rm(reshapeReg0, reshapeState0)
  # return(listReshape0)
  # listReshape0 |> names() |> print()
  # "got here2" |> print(); return(listReshape0)
  
  ###### 4. Configure Data ######
  ### Names of objects to combine from reshaped data
  (!silent) |> ifelse("\n", "") |> paste0(msg1, "Configuring data...") |> message()
  sysDataList0  <- listReshape0 |> createSystemData(byState=T, extend_all=extend_all, save=F, silent=silent)
  rm(listReshape0)
  # "got here3" |> print(); return(sysDataList0)
  
  ###### 5. FrEDI Data  ######
  ### List of data that is the same for both state & region and not modified in confiuration steps
  ### List of data that is the same for both state & region and not modified in confiuration steps
  frediNames0   <- c("fredi_config") |>
    c("co_sectors", "co_sectorsRef", "co_stateSectors") |> 
    c("co_variants", "co_impactTypes") |> 
    c("co_impactYears", "co_impactYearLevels") |> 
    c("co_regions", "co_states") |>
    c("co_models", "co_modelTypes") |>
    c("co_econMultipliers", "co_scalarInfo") |>
    c("co_slrScalars") |>
    c("co_defaultTemps", "temp_default") |> 
    c("slr_cm", "slr_default") |>
    c("gdp_default") |> 
    c("co_inputScenarioInfo", "testDev") |>
    c("co_defaultScenario", "co_statePopRatios")
  ### Update objects
  # sysDataList0 |> names() |> print()
  frediList0    <- sysDataList0 |> (function(x){x[  names(x) %in% frediNames0 ]})()
  sysDataList0  <- sysDataList0 |> (function(x){x[!(names(x) %in% frediNames0)]})()
  ### Update in list
  rDataList[["frediData" ]][["data"]] <- frediList0
  rDataList[["stateData" ]][["data"]] <- sysDataList0
  rm(frediNames0, frediList0, sysDataList0)
  
  ###### Drop Reshaped Data Objects ######
  drop0        <- c("rsData_reg", "rsData_state")
  inDrop0      <- (rDataList |> names()) %in% drop0
  returnList <- rDataList
  rDataList    <- rDataList[!inDrop0]
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
    }
    # expr0         <- try(save(rDataList[["fredi_config"]], rDataList, file=sysDataFile), silent=T) |> expression()
    # if(outPathExists) {trySave <- expr0 |> eval()} ### End if outPathExists
  } ### End if save
  # rDataList |> names() |> print()
  
  ###### Return ######
  "...Finished running configureSystemData()." |> message()
  return(returnList)
}