# ### Save plots as image files
# "." |> devtools::load_all()

## Script for configuring data for main FrEDI
# ###### Load Packages
# require(tidyverse)
# require(openxlsx)
# require(FrEDI)
# require(devtools)

## configureControlTables
configureControlTables <- function(
    ### Directories
  projectDir  = ".",   ### Directory in which to find the FrEDI_Data project/package
  dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  configDir   = "."    , ### Module directory relative to dataDir
  ### Info on config file
  configFile  = "controlTables" |> paste0(".", "xlsx"), ### Name of excel file with config information
  configSheet = "tableNames" ,  ### Name of worksheet on which to find tables
  ### Conditionals
  reshape     = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
  ### Saving
  outDir      = "data", ### Directory to save data, relative to dataDir
  outFile     = "controlTables" |> paste0(".", "rda" ), ### Name of file to which to save results
  save        = TRUE , ### Whether to save the data
  return      = TRUE , ### Whether to return object
  ### Info on messaging
  silent      = TRUE , ### Level of messaging
  msg0        = ""
){
  ### Set Up Environment ----------------
  #### Messaging ----------------
  # msgN <- "\n"
  # msgN |> paste0(msg0, "Running configureControlTables()...") |> message()
  # msg1 <- msg0 |> paste0("\t")
  # msg2 <- msg1 |> paste0("\t")
  msg0 <- msg0 |> str_count("t")
  msg1 <- msg0 + 1
  msg2 <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Running configureControlData()...") |> message()
  
  #### Values ----------------
  ### Parameters for saving tests
  reshape0    <- reshape
  save0       <- save
  return0     <- return
  msgUser     <- !silent
  rm(reshape, save, return)
  
  #### Set Paths ----------------
  ### Adjust paths
  outDir      <- projectDir |> file.path(outDir)
  outPath     <- outDir     |> file.path(outFile)
  
  
  ### 0. Load FrEDI Data Code ----------------
  ### Load FrEDI Data Code
  # projectDir |> devtools::load_all()
  configVals0 <- frediConfig()
  minYr0      <- configVals0[["minYear0"]]
  maxYr0      <- configVals0[["maxYear0"]]
  
  ### 1. Read in Tables from Excel ----------------
  ### Initialize list of objects to return
  controlTables     <- list()
  
  ### 2. Load Excel Data ----------------
  ### Load state data
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0(msg1, "Loading control tables...") |> message()
  # controlTables <- list()
  controlData <- fileDir |> loadFrediConfig(
    configFile  = configFile,  ### Name of excel file with config information
    configSheet = configSheet, ### Sheet with info about tables in config file
    silent      = silent,
    msg0        = msg1
  ) ### End loadData
  # gc()
  # return(controlTables)
  
  ### 3. Reshape Loaded Data ----------------
  ### Reshape state data
  # if(!silent) 
  paste0(msg1, "Reshaping control tables...") |> message()
  controlData <- controlData |> reshapeConfigData(
    minYr0 = minYr0,
    maxYr0 = maxYr0,
    silent = silent, 
    msg0   = msg0 |> get_msgPrefix(newline=F)
  ) ### End reshapeConfigData
  
  ### 4. Save Data to File ----------------
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save) {
    paste0(msg1, "Saving results to ", outPath, "...") |> message()
    pathExists <- outPath |> dir.exists()
    if(!pathExists) {createDir <- outPath |> dir.create(recursive=T)}
    save(list=c("controlData"))
  } ### End if(save)
  
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running configureControlData().") |> message()
  msg0 |> get_msgPrefix(newline=T) |> message()
  if(return0) {return(controlTables)}
  # return(returnList)
}

## End script ----------------