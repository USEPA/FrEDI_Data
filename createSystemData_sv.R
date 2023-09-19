####################### CREATE SV DATA ############################################

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
for(i in codeNames){ source(paste(codePath, i, sep="/")) }



#List configuration files

configLogPath %>% list.files("rda") %>% sort(decreasing=FALSE) %>% head

c_sv_sectors     <- try(get_sv_sectorInfo())
if("try-error" %in% class(c_sv_sectors)){c_sv_sectors <- NULL}


### Screening


list_screening <- list(
  save     = TRUE,  ### Save SV
  sv       = TRUE,  ### Update SV demographic data
  pop      = TRUE,  ### Update SV population data
  format   = TRUE,  ### Update formats
  impacts  = FALSE, ### Update impact lists
  sectors  = c_sv_sectors
)



## Uncomment following six lines to create and save data and check the outputs

test_svData <- createSVData(
  save    = list_screening$save, 
  sv      = list_screening$sv, 
  pop     = list_screening$pop,
  format  = list_screening$format, 
  impacts = list_screening$impacts, 
  sectors = list_screening$sectors,
  return  = TRUE
)


# Use the following code to update the FrEDI system data with the SV configuration data.
for(name_i in codeNames){ source(paste(codePath, name_i, sep="/")) }; codeNames
time3 <- Sys.time()
update_sysdata(
  save    = TRUE,
  sv      = TRUE,
  impacts = FALSE,
  outPath = dataOutPath
)




