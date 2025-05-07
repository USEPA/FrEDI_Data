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
  save0       = TRUE , ### Whether to save the data
  return0     = TRUE , ### Whether to return object
  ### Info on messaging
  silent      = TRUE , ### Level of messaging
  msg0        = 0,
  .testing    = FALSE
){
  ### Set Up Environment ----------------
  #### Messaging ----------------
  msgUser     <- !silent
  msgN        <- "\n"
  msg1        <- msg0 + 1
  msg2        <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Configuring control tables...") |> message()
  
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
    msg0        = msg1,
    .testing    = .testing
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
    msg0   = msg2
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
    if(!pathExists) {
      createDir <- outDir |> dir.create(recursive=T)
    } ### End if(!pathExists)
    save(list=c("controlData"), file=outPath)
  } ### End if(save)

  
  ### Return ----------------
  msg1 |> get_msgPrefix(newline=F) |> paste0("...Finished configuring control tables.") |> message()
  msg1 |> get_msgPrefix(newline=T) |> message()
  if(return0) {
    return(controlData)
  } ### End if(return0)
  # return(returnList)
}

## End script ----------------