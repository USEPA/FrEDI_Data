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
  # projectDir  = ".",   ### Directory in which to find the FrEDI_Data project/package
  dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  ### Info on config file
  configFile  = "controlTables" |> paste0(".", "xlsx"), ### Name of excel file with config information
  configSheet = "tableNames" ,  ### Name of worksheet on which to find tables
  ### Conditionals
  # reshape     = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
  ### Saving
  outDir      = "." |> file.path("data"), ### Directory to save data, relative to dataDir
  outFile     = "controlTables" |> paste0(".", "rda" ), ### Name of file to which to save results
  save        = TRUE , ### Whether to save the data
  return      = TRUE , ### Whether to return object
  ### Info on messaging
  silent      = TRUE , ### Level of messaging
  msg0        = 0
){
  ### Set Up Environment ----------------
  #### Messaging ----------------
  msgN <- "\n"
  # msgN |> paste0(msg0, "Running configureControlTables()...") |> message()
  # msg1 <- msg0 |> paste0("\t")
  # msg2 <- msg1 |> paste0("\t")
  # msg0 <- msg0 |> str_count("t")
  msg0 |> print()
  msg1 <- msg0 + 1
  msg2 <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Running configureControlTables()...") |> message()
  
  #### Values ----------------
  ### Parameters for saving tests
  save0       <- save
  return0     <- return
  msgUser     <- !silent
  rm(save, return)
  
  #### Set Paths ----------------
  ### Adjust paths
  # outDir      <- projectDir |> file.path(outDir)
  outPath     <- outDir |> file.path(outFile)
  
  
  ### 0. Load FrEDI Data Code ----------------
  ### Load FrEDI Data Code
  # projectDir |> devtools::load_all()
  configVals0 <- frediConfig()
  minYr0      <- configVals0[["minYear0"]]
  maxYr0      <- configVals0[["npdYear0"]]
  
  ### 1. Read in Tables from Excel ----------------
  ### Initialize list of objects to return
  # controlData <- list()
  
  ### 2. Load Excel Data ----------------
  ### Load state data
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0("Loading control tables...") |> message()
  # controlTables <- list()
  controlData <- dataDir |> loadFrediConfig(
    configFile  = configFile,  ### Name of excel file with config information
    configSheet = configSheet, ### Sheet with info about tables in config file
    silent      = silent,
    msg0        = msg1
  ) ### End loadData
  # gc()
  # return(controlData)
  # "got here1" |> print()
  
  ### 3. Reshape Loaded Data ----------------
  ### Reshape state data
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0("Reshaping control tables...") |> message()
  controlData <- controlData |> reshapeControlTables(
    minYr0 = minYr0,
    maxYr0 = maxYr0,
    silent = silent, 
    msg0   = msg1
  ) ### End reshapeConfigData
  # controlData[["co_scenarios"]] |> glimpse()
  # "got here2" |> print()
  
  ### 4. Save Data to File ----------------
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Saving results to ", outDir, "...") |> message()
    pathExists <- outDir |> dir.exists()
    if(!pathExists) {createDir <- outDir |> dir.create(recursive=T)}
    save(list=c("controlData"), file=outPath)
  } ### End if(save)
  
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running configureControlTables().") |> message()
  msg0 |> get_msgPrefix(newline=T) |> message()
  if(return0) {return(controlData)}
  # return(returnList)
}

## End script ----------------