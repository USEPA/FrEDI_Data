####################### CREATE SV DATA ############################################

## Load Packages
require(tidyverse)
require(openxlsx)
require(devtools)


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




