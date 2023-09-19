###### Script for configuring data for main FrEDI
###### Load Packages ######
require(tidyverse)
require(openxlsx)
require(devtools)

###### Set Arguments ######
###### Parameters for saving tests
save_test   <- TRUE
return_test <- TRUE

###### Set Paths ######
###### Project path
projectDir  <- "."
###### Code path
codeDir     <- projectDir %>% file.path("R")
codeNames   <- codeDir    %>% list.files(".R")
codePaths   <- codeDir    %>% file.path(codeNames)
###### Data input path and file name
dataInDir   <- projectDir %>% file.path("inst", "extdata")
dataInName  <- "FrEDI_config" %>% paste0(".xlsx")
dataInPath  <- dataInDir %>% file.path(dataInName)
###### Data output path and file name
dataOutDir  <- projectDir %>% file.path("data")
dataOutName <- "sysdata"  %>% paste0(".rda")
dataOutPath <- dataOutDir %>% file.path(dataOutName)

###### 0. Local Function ###### 
for(i in codePaths){ i %>% source() }

###### 1. Load Excel Data ######
### list_loadData
list_loadData <- loadData( 
  fileDir   = dataInDir,    ### Path to project
  fileName  = dataInName,   ### name of excel file with config information
  sheetName = "tableNames", ### Sheet with info about tables in config file
  silent    = NULL
)

###### 2. Reshape Loaded Data ######
### list_reshapeData
list_reshapeData <- list_loadData %>% reshapeData(silent=T)

###### 3. Configure Data ######
list_systemData0 <- list_reshapeData %>% createSystemData(save=F, silent=T)

###### 4. Run Individual Tests on Data ######
# ###### Test the reshaped data:
# test_reshapeData <- list_reshapeData %>% dataInfo_test(
#   csvName = "reshapedData_testResults",
#   save    = save_test,
#   return  = return_test
# )
# 
# ###### Run the general config tests for configured data:
# test_systemData0 <- list_systemData0 %>% dataInfo_test(
#   csvName = "configuredData_testResults",
#   save    = save_test,
#   return  = return_test
# )


###### 5. Run General Tests on Data ######
test_general_config <- general_config_test(
  reshapedData   = list_reshapeData,
  configuredData = list_systemData0,
  overwrite      = TRUE,
  xlsxName       = "generalConfig_testResults.xlsx",
  fredi_config   = fredi_config
)

###### 6. Load Reference Data ######
### Load ref data
newEnv_ref  <- new.env()
dataOutPath %>% load(verbose = F, envir=newEnv_ref)
# ls(envir=newEnv_ref) %>% print
refDataList <- "rDataList" %>% get(envir=newEnv_ref, inherits = F)
# ls() %>% print; refDataList %>% names %>% print
rm("newEnv_ref")

###### 6. Run New Sector Tests on Data ######
###### Determine if tests need to be run
c_sectors_new <- list_systemData0[["co_sectors"]][["sector_id"]] %>% as.matrix %>% as.vector %>% unique
c_sectors_ref <- refDataList[["co_sectors"]][["sector_id"]]  %>% as.matrix %>% as.vector %>% unique
hasNewSectors <- (c_sectors_new %in% c_sectors_ref) %>% all 
doNewTest     <- !hasNewSectors
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

###### 7. Update Saved Data ######
# list_systemData <- list_reshapeData %>% createSystemData(save=T, silent=T)








