# ### Save plots as image files
# "." |> devtools::load_all()

## Script for configuring data for main FrEDI
# ###### Load Packages
# require(tidyverse)
# require(openxlsx)
# require(FrEDI)
# require(devtools)

## configureScenarios
configureScenarios <- function(
    ### Directories
  # projectDir  = ".",   ### Directory in which to find the FrEDI_Data project/package
  # dataDir     = "." |> file.path("inst", "extdata"), ### Path to projectDir
  # scenarioDir = "scenarios" , ### Module directory relative to dataDir
  dataDir     = "." |> file.path("inst", "extdata", "scenarios"), ### Path to projectDir
  controlData = NULL, ### Output of configureControlTables
  # ### Conditionals
  # reshape     = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
  ### Saving
  outDir      = "data", ### Directory to save data, relative to dataDir
  save        = TRUE , ### Whether to save the data
  return      = TRUE , ### Whether to return object
  ### Info on messaging
  silent      = TRUE , ### Level of messaging
  msg0        = ""
){
  ### Set Up Environment ----------------
  #### Messaging ----------------
  # msgN <- "\n"
  # msgN |> paste0(msg0, "Running configureScenarios()...") |> message()
  # msg1 <- msg0 |> paste0("\t")
  # msg2 <- msg1 |> paste0("\t")
  msg0 <- msg0 |> str_count("t")
  msg1 <- msg0 + 1
  msg2 <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Running configureScenarios()...") |> message()
  
  #### Values ----------------
  ### Parameters for saving tests
  reshape0    <- reshape
  save0       <- save
  return0     <- return
  msgUser     <- !silent
  rm(reshape, save, return)
  
  #### Set Paths ----------------
  ### Adjust paths
  outDir      <- projectDir |> file.path(outDir, "scenarios")
  
  ### 0. Load FrEDI Data Code ----------------
  ### Load FrEDI Data Code
  # projectDir |> devtools::load_all()
  configVals0 <- frediConfig()
  minYr0      <- configVals0[["minYear0"]]
  maxYr0      <- configVals0[["maxYear0"]]
  
  ### 1. Read in Tables from Excel ----------------
  ### Initialize list of objects to return
  # controlTables     <- list()
  
  ### 2. Load Excel Data ----------------
  ### Load state data
  # if(!silent) 
  msg1 |> get_msgPrefix(newline=F) |> paste0(msg1, "Loading scenario data...") |> message()
  # controlTables <- list()
  scenarioData <- fileDir |> loadScenarios(
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
  # paste0(msg1, "Reshaping control tables...") |> message()
  # scenarioData <- scenarioData |> reshapeScenarios(
  #   minYr0 = minYr0,
  #   maxYr0 = maxYr0,
  #   silent = silent, 
  #   msg0   = msg0 |> get_msgPrefix(newline=F)
  # ) ### End reshapeConfigData
  
  ### 4. Save Data to File ----------------
  ### Save R Data objects
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Saving results to ", outDir, "...") |> message()
    pathExists <- outDir |> dir.exists()
    if(!pathExists) {createDir <- outDir |> dir.create(recursive=T)}
    save(list=c("controlData"), file=outPath)
  } ### End if(save)
  
  
  ### Return ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("...Finished running configureScenarios().") |> message()
  msg0 |> get_msgPrefix(newline=T) |> message()
  if(return0) {return(controlTables)}
  # return(returnList)
}

## End script ----------------