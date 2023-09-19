# Script


## Load Packages
require(tidyverse)
require(openxlsx)
require(devtools)


mostRecentDate <- "20230503"# Sys.Date()


projectPath      <- "."
today            <- format(Sys.Date(), "%Y%m%d")


### Project configuration
configPath       <- projectPath %>% file.path("config")
# configFiles      <- configPath %>% list.files(".R")
configFiles      <- "frediConfig.R"
### Code path
codePath         <- projectPath %>% file.path("R")
codeNames        <- codePath %>% list.files(".R")
configLogPath    <- codePath %>% file.path("configLog")

### Names for archive and use versions
configFileName   <- "fredi_config"
configArchName   <- configLogPath %>% file.path(configFileName) %>% paste(today, sep="_") %>% paste0(".rda")
configUseName    <- codePath %>% file.path(configFileName) %>% paste0(".rda")

### Output path
dataOutPath      <- projectPath %>% file.path("data")
dataOutLog       <- dataOutPath %>% file.path("dataLog")
sysFileName      <- "sysdata"
sysArchName      <- dataOutLog %>% file.path(sysFileName) %>% paste(today, sep="_") %>% paste0(".rda")
excelFileName    <- mostRecentDate %>% paste("FrEDI_config.xlsx", sep="_")
sysUseName       <- dataOutPath %>% file.path(sysFileName) %>% paste0(".rda")

### Name of most recent sysdata.rda file in archive
dataOutLogFiles  <- dataOutLog %>% list.files %>% sort(decreasing = T); dataOutLogFiles %>% head
sysFileRef       <- dataOutLog %>% file.path(dataOutLogFiles[1]); sysFileRef
# sysFileRef       <- dataOutLog %>% file.path("sysdata_20221021.rda"); sysFileRef


# Configuration Files -->
##Load all configuration functions


### Load all functions

for(i in configFiles){ source(paste(configPath, i, sep="/")) }



#List configuration files

configLogPath %>% list.files("rda") %>% sort(decreasing=FALSE) %>% head


#Update archive:

frediConfig(outPath = configArchName)


#Confirm copy was saved:
  
basename(configArchName) %in% (configLogPath %>% list.files("rda"))

## Create Tool Configuration Data

### 1. Load Data

#Load data from Excel configuration file:
  
### Load all functions
# codeNames; 
for(i in codeNames){ source(paste(codePath, i, sep="/")) }
### list_loadData
list_loadData <- loadData( 
  fileDir   = "." %>% file.path( "inst", "extdata"), ### Path to project
  fileName  = excelFileName, ### name of excel file with config information
  sheetName = "tableNames",
  silent = NULL
)


#<!-- Check scaled impacts -->
  
  test_impacts0  <- list_loadData$data_scaledImpacts 
  names_impacts0 <- test_impacts0 %>% names 
  check_impacts0 <- test_impacts0 %>% group_by_at(.vars=c(all_of(names_impacts0))) %>% summarize(n=n(), .groups="keep") %>% ungroup 
  #test_impacts0 %>% glimpse -->
  #check_impacts0 %>% glimpse -->
  #check_impacts0 %>% filter(n>1) %>% glimpse -->

  
  ### 2. Reshape Loaded Data
  
#Reshape loaded Excel data

### Load all functions
# codeNames; 
### list_reshapeData
list_reshapeData <- list_loadData %>% reshapeData(silent=T)



  test_impacts0  <- list_reshapeData$data_scaledImpacts 
  names_impacts0 <- test_impacts0 %>% names 
  check_impacts0 <- test_impacts0 %>% group_by_at(.vars=c(all_of(names_impacts0))) %>% summarize(n=n(), .groups="keep") %>% ungroup 


  
  
  ### 3. Configure Data
  
#Test `createSystemData()` function (configure reshaped data)
    
# frediPath %>% load_all
### Uncomment following two lines to create and save data and check the outputs
list_systemData0 <- list_reshapeData %>% createSystemData(save=F, silent=T)


### 4. Tests 

#### Parameters

#Parameters for saving tests

save_test   <- TRUE
return_test <- TRUE



#### Reshaped Data Test
#Try different methods -->
  
 test_types <- c("data.frame", "tibble") 
test_list  <- list(a=test_types, b=c("list"))
 test_list1 <- test_list %>% names %>% map(~case_when(("data.frame" %in% test_list[[.]])~1, ("list" %in% test_list[[.]])~2, TRUE~0)) 
 test_list2 <- test_list %>% names %>% map(~case_when(("data.frame" %in% test_list[[.]])~"data.frame", ("list" %in% test_list[[.]])~"list", TRUE~"N/A"))

  
 # Try the test for the reshaped data:
  
test_reshapeData   <- list_reshapeData %>% dataInfo_test(
  csvName = "reshapedData_testResults",
  save    = save_test,
  return  = return_test
)
#Examine reshaped data test results: check for any tables that didnt pass



#### General Config Tests

#Try the general tests for configured data:

test_systemData0 <- list_systemData0 %>% dataInfo_test(
  csvName = "configuredData_testResults",
  save    = save_test,
  return  = return_test
)


#Examine configured data test results: check for any tables that didnt pass



#### General Tests

#Wrap the general tests into a single wrapper function:
  
test_general_config <- general_config_test(
  reshapedData   = list_reshapeData,
  configuredData = list_systemData0,
  overwrite      = TRUE,
  fredi_config = fredi_config
)


#### New Sector Tests

# Run `createSystemData()` tests:
  
test_newSectors_config <- newSectors_config_test(
  newData     = list_systemData0,
  refDataFile = sysFileRef,
  save      = save_test,
  return    = return_test
)


## Update Data


# If above passed tests, overwrite previous config

# codeNames; for(i in codeNames){ source(paste(codePath, i, sep="/")) }
### Uncomment following two lines to create and save data and check the outputs
list_systemData <- list_reshapeData %>% createSystemData(save=T, silent=T)
#list_systemData <- list_reshapeData %>% createSystemData(save=T, silent=T)








