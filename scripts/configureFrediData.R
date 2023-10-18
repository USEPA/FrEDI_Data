# ### Save plots as image files
# "." |> devtools::load_all()

###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(FrEDI)
require(devtools)
# require(devtools)

configureFrediData <- function(
    projectDir = ".",
    save       = FALSE
){

###### Set Arguments ######
###### Parameters for saving tests
save_test   <- save

###### Return List ######
returnList  <- list()

###### Set Paths ######
###### Project path
# projectDir  <- "."
projectDir  <- projectDir
###### Data input path and file name
dataInDir   <- projectDir |> file.path("inst", "extdata")
dataInName  <- "FrEDI_config" |> paste0(".xlsx")
dataInPath  <- dataInDir |> file.path(dataInName)
###### Data output path and file name
dataOutDir  <- projectDir |> file.path("data")
dataOutName <- "sysdata"  |> paste0(".rda")
dataOutPath <- dataOutDir |> file.path(dataOutName)

###### 0. Load FrEDI Data Code ###### 
### Load FrEDI Data Code
projectDir |> devtools::load_all()

###### 1. Load Excel Data ######
### list_loadData
list_loadData <- loadData( 
  fileDir   = dataInDir,    ### Path to project
  fileName  = dataInName,   ### name of excel file with config information
  sheetName = "tableNames", ### Sheet with info about tables in config file
  # byState   = TRUE,
  silent    = NULL
)
### Add to list
returnList[["loadedDataList"  ]] <- list_loadData

###### 2. Reshape Loaded Data ######
### list_reshapeData
list_reshapeData <- list_loadData |> reshapeData(silent=T)
### Add to list
returnList[["reshapedDataList"]] <- list_reshapeData

###### 3. Configure Data ######
list_systemData0 <- list_reshapeData |> createSystemData(save=save, silent=T, outPath=dataOutDir |> file.path("tmp_sysdata.rda"))
### Add to list
returnList[["systemDataList"]] <- list_systemData0

###### 4. Run General Tests on Data ######
test_general_config <- general_config_test(
  reshapedData   = list_reshapeData,
  configuredData = list_systemData0,
  save           = save,
  overwrite      = TRUE,
  xlsxName       = "generalConfig_testResults.xlsx",
  fredi_config   = fredi_config
)
### General tests
returnList[["generalConfigTests"]] <- test_general_config

###### 5. Load Reference Data ######
### Load ref data
newEnv_ref  <- new.env()
dataOutPath |> load(verbose = F, envir=newEnv_ref)
# ls(envir=newEnv_ref) |> print()
refDataList <- "rDataList" |> get(envir=newEnv_ref, inherits = F)
# ls() |> print(); refDataList |> names() |> print()
rm("newEnv_ref")
# dataOutPath |> source(encoding="utf-8")
# source(dataOutPath)
# rDataList |> names() %>% print()
# refDataList <- rDataList

###### 6. Run New Sector Tests on Data ######
###### Determine if tests need to be run
df_sectors_new <- list_systemData0[["co_sectors"]][["sector_id"]]
df_sectors_ref <- refDataList[["co_sectors"]][["sector_id"]]
df_sectors_new |> glimpse(); df_sectors_ref |> glimpse()
c_sectors_new  <- df_sectors_new |> unique()
c_sectors_ref  <- df_sectors_ref |> unique()
hasNewSectors  <- (c_sectors_new %in% c_sectors_ref) |> all()
doNewTest      <- !hasNewSectors
###### Run test if there are new sectors
if(doNewTest){
  test_newSectors_config <- newSectors_config_test(
    newData     = list_systemData0,
    refDataFile = dataOutPath,
    xslxName    = "newSectorsConfig_testResults.xlsx",
    save        = save_test,
    return      = return_test
  )
}
### New sector tests
returnList[["newSectorConfigTests"]] <- test_general_config

# ###### 7. Create Images of scaled impacts ######
# ### Test create results
# testResults  <- list_systemData0 |> get_fredi_sectorOptions_results(); testResults |> glimpse()
# ### Test region plot info
# testIter0    <- testResults |> filter(sector=="Extreme Temperature") |> get_region_plotInfo(); testIter0$sectorInfo |> glimpse()
# ### Test make plots
# testPlots0   <- testResults      |> 
#   # filter(sector=="Extreme Temperature") |> 
#   make_scaled_impact_plots()
# testPlots0$GCM$`Extreme Temperature_2010`$All
# ### Test save plots
# testPlots1   <- testPlots0       |> save_scaled_impact_figures(df0=testResults)
# ### Test get plots
# testPlots    <- list_systemData0 |> get_scaled_impact_plots(save=T)

###### Return ######
return(returnList)

}

###### End script ######