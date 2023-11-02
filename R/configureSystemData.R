configureSystemData <- function(
    fileDir    = "." |> file.path("inst", "extdata"), ### Path to project
    fileName   = "FrEDI_config.xlsx", ### name of excel file with config information
    sheetName  = "tableNames",
    configPath = "." |> file.path("R"   , "fredi_config.R"), ### Path to config file
    save       = FALSE,  ### Whether to message the user
    outPath    = "." |> file.path("data", "tmp_sysdata.rda"),
    return     = TRUE,
    silent     = TRUE,
    reshape = TRUE  # Return reshapeData
){
  "Running configureSystemData()..." |> message()
  msg0 <- "\t"
  ###### Create File Paths ######
  ### Output file
  outPath     <- if (is.null(outPath)) {"." |> file.path("data", "tmp_sysdata.rda")} else {outPath}
  sysDataFile <- outPath |> basename()
  sysDataPath <- outPath |> dirname()
  sysDataFile <- sysDataPath |> file.path(sysDataFile)
  
  ###### Configuration List ######
  ### Names of objects that are the same for region- and state-level FrEDI
  objects0 <- c("fredi_config") |>
    c("co_sectors", "co_sectorsRef", "co_stateSectors") |> 
    c("co_variants", "co_impactTypes") |> 
    c("co_impactYears", "co_impactYearLevels") |> 
    c("co_regions", "co_states") |>
    c("co_models", "co_modelTypes") |>
    c("co_econMultipliers", "co_scalarInfo") |>
    c("co_defaultTemps", "temp_default", "slr_default")
  
  ###### Initialize Object List ######
  ### Initialize list of objects to return
  rDataList  <- list()
  rDataList[["frediData" ]] <- list(name="frediData" , data=list())
  rDataList[["regionData"]] <- list(name="region"   , data=list())
  rDataList[["stateData" ]] <- list(name="stateData" , data=list())
  if(reshape){ 
    rDataList[["rsData_reg" ]] <- list(name="rsData_reg" , data=list())
    rDataList[["rsData_state" ]] <- list(name="rsData_state" , data=list())
  }
  ###### Regional Data ######
  "\n" |> paste0(msg0, "Configuring region-level data...") |> message()
  ###### 1. Load Excel Data ######
  ### list_loadData
  byState0    <- FALSE
  loadList0   <- loadData( 
    fileDir   = fileDir,    ### Path to project
    fileName  = fileName,   ### name of excel file with config information
    sheetName = sheetName, ### Sheet with info about tables in config file
    byState   = byState0,
    silent    = silent
  )
  
  ###### 2. Reshape Loaded Data ######
  ### list_reshapeData
  reshapeList0 <- loadList0 |> reshapeData(byState=byState0, silent=silent)
  ## Add reshape region data if argument = TRUE
  if(reshape){
  names0     <- reshapeList0|> names()
  inList0    <- (names0 %in% objects0)
  which0     <- inList0 |> which()
  which1     <- (!inList0) |> which()
  rDataList[["rsData_reg" ]] <- reshapeList0[which1] 
  }
  
  ###### 3. Configure Data ######
  sysDataList0 <- reshapeList0 |> createSystemData(byState=byState0, save=save, silent=silent)
  # list_systemData0 <- list_reshapeData |> createSystemData(save=T, silent=T, outPath= dataOutDir |> file.path("tmp_sysdata.rda"))
  
  ###### 4. Update Data List ######
  ### Names
  names0     <- sysDataList0 |> names()
  inList0    <- (names0 %in% objects0)
  which0     <- inList0 |> which()
  which1     <- (!inList0) |> which()
  ### Update objects
  rDataList[["frediData" ]][["data"]] <- sysDataList0[which0]
  rDataList[["regionData"]][["data"]] <- sysDataList0[which1]
  ### Remove objects
  rm(names0, inList0, which0, which1)
  rm(loadList0, reshapeList0, sysDataList0)
  
  ###### State Data ######
  "\n" |> paste0(msg0, "Configuring state-level data...") |> message()
  ###### 1. Load Excel Data ######
  ### list_loadData
  byState0    <- TRUE
  loadList0   <- loadData( 
    fileDir   = fileDir,    ### Path to project
    fileName  = fileName,   ### name of excel file with config information
    sheetName = sheetName, ### Sheet with info about tables in config file
    byState   = byState0,
    silent    = silent
  )
  
  ###### 2. Reshape Loaded Data ######
  ### list_reshapeData
  reshapeList0 <- loadList0 |> reshapeData(byState=byState0, silent=silent)
  ### Update object of reshape data to be returned
  if(reshape){
    names0     <- reshapeList0|> names()
    inList0    <- (names0 %in% objects0)
    which0     <- inList0 |> which()
    which1     <- (!inList0) |> which()
    rDataList[["rsData_state" ]] <- reshapeList0[which1] 
  }
  ###### 3. Configure Data ######
  sysDataList0 <- reshapeList0 |> createSystemData(byState=byState0, save=save, silent=silent)
  # list_systemData0 <- list_reshapeData |> createSystemData(save=T, silent=T, outPath= dataOutDir |> file.path("tmp_sysdata.rda"))
  ### Names
  names0     <- sysDataList0 |> names()
  inList0    <- (names0 %in% objects0)
  which1     <- (!inList0) |> which()
  ### Update objects
  rDataList[["stateData" ]][["data"]] <- sysDataList0[which1]
  ### Remove objects
  rm(names0, inList0, which1)
  rm(loadList0, reshapeList0, sysDataList0)
  
  ###### Save to File ######
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save) {
    paste0("\n", msg0, "Saving results to ", sysDataPath, "...") |> message()
    outPathExists <- sysDataPath |> dir.exists()
    fredi_config <- rDataList[["frediData"]][["data"]][["fredi_config"]]
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
  return(rDataList)
}