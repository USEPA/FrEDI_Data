## Set Up Environment ----------------
### Packages ----------------
require(tidyverse)
require(devtools)
require(zoo)
require(DBI)
# require(FrEDI)

### File Paths ----------------
### Project directory
projDir   <- "."
"." |> load_all()
### Load utils
frediDir  <- projDir |> file.path("..", "FrEDI")
frUtils   <- frediDir |> file.path("R", "utils.R")
frUtils |> source()



### Data dir
### Get file paths: 9 files, including county results
codeDir   <- projDir   |> file.path("scripts")
dataDir   <- projDir   |> file.path("inst", "extdata", "ghg")
dataPaths <- dataDir   |> list.files(full.names = T, pattern = "\\.csv")
dataFiles <- dataPaths |> basename(); dataFiles
### File to save results
oPath0    <- projDir   |> file.path("data")


### Connect to FreDI DB
con <- load_fredi_db(fredi_db_path = file.path(oPath0,"fredi"))
  
### Exclude county results: 6 files
loadFiles <- dataFiles |> (function(x, str0="County"){x[!(x |> str_detect(str0))]})(); loadFiles
loadCode  <- function(path0=codeDir, files0="utils_ghg.R"){for(file_i in files0){path0 |> file.path(files0) |> source()}}
loadCode()

### Examine data
### Separate files
ch4Files  <- loadFiles |> (function(x, str0="ch4"  ){x[x |> tolower() |> str_detect(str0)]})(); ch4Files
o3Files   <- loadFiles |> (function(x, str0="ozone"){x[x |> tolower() |> str_detect(str0)]})(); o3Files
mortFiles <- loadFiles |> (function(x, str0="mort" ){x[x |> tolower() |> str_detect(str0)]})(); mortFiles
asthFiles <- loadFiles |> (function(x, str0="asth" ){x[x |> tolower() |> str_detect(str0)]})(); asthFiles
popFiles  <- loadFiles |> (function(x, str0="pop"  ){x[x |> tolower() |> str_detect(str0)]})(); popFiles

loadTypes <- c("ch4", "o3", "mort", "asth", "pop")
# dfLoad    <- tibble(prefix0 = loadTypes) |> mutate(str0 = c("pop", "o3", "mort","ch4"))
dfLoad    <- tibble(prefix0 = loadTypes) |> mutate(str0 = c("ch4", "ozone", "mort", "asth", "pop"))

### Load Lists ----------------
#### List of File Names ----------------
listLoad0  <- dfLoad |> pull(str0) |> map(function(str0, list0=loadFiles){
  list0 <- list0 |> (function(x, strx=str0){x[x |> tolower() |> str_detect(strx)]})(); 
  list0 |> print()
  return(list0)
}) |> set_names(dfLoad |> pull(prefix0))
listLoad0 |> names()
# listLoad0$pop

#### Order File Names ----------------
listLoad1 <- listLoad0 |> 
  (function(list0){
    list(type0 = list0 |> names(), files0=list0)
  })() |>
  pmap(function(type0, files0){
    ### Conditionals
    doPop    <- "pop"  %in% type0
    doO3     <- "o3"   %in% type0
    doMort   <- "mort" %in% type0
    doAsth   <- "asth" %in% type0
    doch4    <- "ch4"  %in% type0
    ### Initialize list
    type0 |> print(); files0 |> print()
    list0    <- list()
    if        (doPop ) {
      # list0[[type0]] <- files0
      rff0      <- files0 |> (function(x, strx="RFF"  ){x[x |> str_detect(strx)]})()
      all0      <- files0 |> (function(x, strx="all"  ){x[x |> str_detect(strx)]})()
      age0      <- files0 |> (function(x, strx="Age"  ){x[x |> str_detect(strx)]})()
      base0     <- files0 |> (function(x, strx="State"){x[x |> str_detect(strx)]})() |> get_matches(age0, matches=F)
      hasBase0  <- base0  |> length()
      hasRff0   <- rff0   |> length()
      hasAll0   <- all0   |> length()
      hasAge0   <- age0   |> length()
      if(hasBase0) {list0[["base"]] <- base0}
      if(hasRff0 ) {list0[["rff" ]] <- rff0}
      if(hasAll0 ) {list0[["all" ]] <- all0}
      if(hasAge0 ) {list0[["age" ]] <- age0}
    } else if (doO3  ) {
      nat0      <- files0 |> (function(x, strx="Nation|Country"){x[x |> str_detect(strx)]})()
      state0    <- files0 |> (function(x, strx="State"         ){x[x |> str_detect(strx)]})()
      hasNat0   <- nat0   |> length()
      hasState0 <- state0 |> length()
      if(hasNat0   ) {list0[["nat"  ]] <- nat0}
      if(hasState0 ) {list0[["state"]] <- state0}
    } else if (doMort) {
      base0      <- files0  |> (function(x, strx="Country"  ){x[x |> str_detect(strx)]})()
      stateAdj0  <- files0  |> (function(x, strx="State_Base"){x[x |> str_detect(strx)]})()
      xm0        <- files0  |> (function(x, strx="Excess"){x[x |> str_detect(strx)]})() 
      scalar0    <- files0  |> (function(x, strx="Scalar"){x[x |> str_detect(strx)]})() 
      hasBase0   <- base0     |> length()
      hasAdj0    <- stateAdj0 |> length()
      hasXm0     <- xm0       |> length()
      hasScalar0 <- scalar0   |> length()
      if(hasBase0  ) {list0[["base"  ]] <- base0  }
      if(hasAdj0   ) {list0[["state" ]] <- stateAdj0}
      if(hasXm0    ) {list0[["xm"    ]] <- xm0    }
      if(hasScalar0) {list0[["scalar"]] <- scalar0}
    } else if (doch4) { 
      ssp2450      <- files0  |> (function(x, strx="ssp245"  ){x[x |> str_detect(strx)]})()
      hasssp2450   <- ssp2450   |> length()
      if(hasssp2450) {list0[["ssp245"  ]] <- ssp2450  }
    }  else if (doAsth) { 
      excess0     <- files0  |> (function(x, strx="Excess"){x[x |> str_detect(strx)]})()
      child0      <- files0  |> (function(x, strx="Child"){x[x |> str_detect(strx)]})()
      hasExcess   <- excess0 |> length()
      hasChild    <- child0  |> length()
      if(hasChild ) {list0[["base"]] <- child0}
      if(hasExcess) {list0[["excess"]] <- excess0}
    } ### End if (doPop )### End if (doPop )
    return(list0)
  }) |> set_names(loadTypes)
listLoad1 |> glimpse()
# listLoad1$pop |> glimpse()

#### Load Files ----------------
listLoad  <- listLoad1 |> 
  (function(list0){
    list(type0 = list0 |> names(), lists0=list0)
  })() |> pmap(function(type0, lists0, path0=dataDir){
    ### Initialize list
    list0  <- list()
    files0 <- lists0 |> unlist()
    files0 |> print()
    ### Add list names
    # names0 <- lists0 |> names()
    dfNew0 <- tibble(type = type0, objName = lists0 |> names())
    # dfNew0 |> glimpse()
    dfNew0 <- dfNew0 |> 
      # mutate(dfName = (type==objName) |> ifelse(type, type |> paste0(objName |> str_to_title()))) |>
      # mutate(dfName = type |> paste0(objName |> str_to_title())) |>
      mutate(dfName = objName |> str_to_title() |> (function(x, y=type){y |> paste0(x)})()) |>
      mutate(fName  = files0)
    dfNew0 |> glimpse()
    ### Update info in list
    list0[["info"]] <- dfNew0
    
    ### Read in files
    names0     <- dfNew0 |> pull(dfName)
    list0[["data"]] <- dfNew0 |> pull(fName) |> map(function(fName_i, fpath=path0){
      ### Read in data
      path_i   <- fpath  |> file.path(fName_i)
      df_i     <- path_i |> read.csv()
      ### Adjust FIPS data
      names_i  <- df_i |> names()
      doFips_i <- "State_FIPS" %in% names_i
      if(doFips_i) df_i <- df_i |> filter_at(c("State_FIPS"), negate(is.na))
      ### Return
      return(df_i)
    }) |> set_names(names0)
    
    ### Return
    return(list0)
  }) |> set_names(loadTypes)

# listLoad$pop$data$popAll |> group_by(state, year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()
# listLoad$pop$data |> names()
# for(name_i in listLoad |> names()) { for(name_j in listLoad[[name_i]][["data"]] |> names()) {
#   name_j |> assign(listLoad[[name_i]][["data"]][[name_j]])
# }} ### End for loop


## Initialize Lists ----------------
# listSave    <- list()
ghgData       <- list()
### List of config data and list of state data
configList    <- list()
stateData     <- list()
mortData      <- list()
morbData      <- list()
scenariosList <- list()
# listData    <- list()

### ScenarioData
scenarioData    <- DBI::dbReadTable(con,"scenarioData")
scenarioData    <- unserialize(scenarioData$value |> unlist())


### Coffiecients ----------------
### Lists of coefficients
list_coefficients <- list() |> (function(
    list0
){
  #### Initial List ----------------
  list0   <- list()
  
  
  #### Other ----------------
  listOth <- list()
  listOth[["minYear0"]] <- 2020
  listOth[["maxYear0"]] <- 2100
  listOth[["vsl_adj0"]] <- scenarioData$pop_default |> (function(
    df0, 
    df1   = scenarioData$gdp_default,
    year0 = 2010
  ){
    # df0 |> glimpse(); df1 |> glimpse()
    ### Filter data
    df0   <- df0 |> filter(!(year |> str_detect("Nation")))
    df0   <- df0 |> filter(year %in% year0)
    df1   <- df1 |> filter(year %in% year0)
    
    ### Get national totals
    df0   <- df0 |> group_by_at(c("year")) |> summarize(national_pop = pop |> sum(na.rm=T), .groups="drop")
    
    ### Join data
    join0 <- c("year")
    df0   <- df0 |> left_join(df1, by=c(join0))
    
    ### Calculate adjustment
    df0   <- df0 |> mutate(gdp_percap = gdp_usd / national_pop)
    
    ### Return
    return(df0)
  })()
  ### Update in list
  list0 <- list0 |> c(listOth)
  
  #### Mortality ----------------
  ### Initialize list
  listM   <- list()
  listM[["fun0"]] <- NULL
  ### Update in list
  list0[["Mortality"]] <- listM
  
  #### Methane ----------------
  ### CH4, in pptv 
  ### Initialize list
  listCH4   <- list()
  ### Add coefficients
  listCH4[["base0"]] <- 100
  ### Update in list
  list0[["CH4"]] <- listCH4
  
  #### NOx ----------------
  ### Initialize list
  listNOx   <- list()
  ### NOx coefficients
  listNOx[["base0"     ]] <- 10.528
  listNOx[["slope0"    ]] <- -0.49
  listNOx[["intercept0"]] <- -1.12
  listNOx[["adj0"      ]] <- 1e3/556
  ### Function of NOx in Mt per year
  calc_NOx_factor <- function(
    nox0, 
    slope0     = -0.49, 
    intercept0 = -1.12,
    adj0       = 1e3/556
  ){
    nox0 <- nox0 |> log() 
    nox0 <- nox0 * slope0 + intercept0
    nox0 <- nox0 * adj0
    return(nox0)
  }
  ### Add to list
  listNOx[["fun0"]] <- calc_NOx_factor
  ### Get NOx factor
  listNOx[["NOxFactor0"]] <- listNOx[["base0"]] |> calc_NOx_factor()
  ### Update in list
  list0[["NOx"]] <- listNOx
  
  #### Return ----------------
  return(list0)
})()
### add data to list
# ghgData [["original"]] <- listLoad
configList[["coefficients"]] <- list_coefficients
list_coefficients |> glimpse()

## Config Tables ----------------
### Sector Info ----------------
#### Sectors ----------------
co_sectors  <- tibble(sector = c("mortality", "morbidity")) |> 
  mutate(sector_label = "Health - " |> paste0(sector |> str_to_title()))
co_sectors |> glimpse()
configList[["co_sectors"]] <- co_sectors

#### Age Types ----------------
### Main impact info and add age ranges
co_ageTypes  <- tibble(ageType = c("all", "child", "adult")) |> 
  mutate(ageRange = ageType |> factor(c("all", "child", "adult"), c("all", "0TO17", "18TO99")))
co_ageTypes |> glimpse()
configList[["co_ageTypes"]] <- co_ageTypes

#### Impact Types ----------------
### Main impact info and add age ranges
co_impactTypes  <- tibble(
  sector="mortality", 
  impactType="excessMort", 
  impactType_label="Excess Mortality"
) |> bind_rows(tibble(
  sector="morbidity", 
  impactType="childAsthma" |> c("asthmaER" |> paste0("_", c("child", "adult"))), 
  impactType_label="New Childhood Asthma" |> c("Asthma ER Visits" |> paste0(" (", c("Children", "Adult"), ")"))
)) |> 
  mutate(
    endpoint = case_when(
      sector %in% "mortality" ~ "NA",
      impactType |> str_detect("childAsthma") ~ "Incidence, Asthma", 
      .default="Emergency Room Visits, Asthma")
  ) |> 
  mutate(ageType  = impactType |> str_extract("child|adult") |> replace_na("all")) |>
  left_join(co_ageTypes, by="ageType") |>
  mutate(econScalarName     = c("vsl_usd", "aq_newAsthmacost", "aq_EDcostpervisit", "aq_EDcostpervisit")) |> 
  mutate(econMultiplierName = c("gdp_percap", "gdp_percap", "none", "none")) |> 
  mutate(c0 = 0) |>
  mutate(c1 = 1) |>
  mutate(exp0  = c(0.4, 0.06, 1, 1)) |>
  mutate(econAdjValue0 = case_when(
    econMultiplierName %in% "gdp_percap" ~ list_coefficients$vsl_adj0 |> pull(gdp_percap),
    econMultiplierName %in% "none"    ~ 1, 
    .default = NA
  )); co_impactTypes |> glimpse()
configList[["co_impactTypes"]] <- co_impactTypes

#### Sector Info ----------------
co_sectorInfo  <- co_sectors |> left_join(co_impactTypes, by="sector")
co_sectorInfo |> glimpse()
configList[["co_sectorInfo"]] <- co_sectorInfo

#### Scalar Info ----------------
stateData    <- DBI::dbReadTable(con,"stateData")
stateData    <- unserialize(stateData$value |> unlist())
stateData$scalarData |> filter(scalarName %in% co_impactTypes$econScalarName) |> glimpse()


df_scalars <- co_impactTypes |> (function(
    dfT, 
    dfS = stateData$scalarData 
){
  ### Get distinct scalar names
  select0 <- c("econScalarName", "econMultiplierName")
  vals0   <- dfT |> select(all_of(select0)) |> unlist() |> unique() 
  ### Filter to scalar values
  drop0   <- c("region", "state", "postal")
  from0   <- c("year")
  to0     <- c("year0")
  dfS     <- dfS |> 
    filter(scalarName %in% vals0) |> 
    select(-any_of(drop0)) |> 
    rename_at(c(from0), ~to0)
  ### Return
  return(dfS)
})(); df_scalars |> glimpse()
configList[["df_scalars"]] <- df_scalars

### Regions & States ----------------
### Get new regions
co_regions    <- DBI::dbReadTable(con,"co_regions")
co_regions  <- co_regions |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Select region_label and add "us_area"
  select0   <- c("region_label")
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> mutate(us_area = "CONUS")
  rm(select0)
  
  ### Add observations for Alaska and Hawaii, then rowbind into other data
  df1       <- tibble(region_label=c("Alaska", "Hawaii")) |> mutate(us_area = region_label)
  df0       <- df0 |> rbind(df1)
  
  ### Mutate region ID and move before us_area
  df0       <- df0 |> mutate(region = region_label |> str_replace_all(" ", ""))
  df0       <- df0 |> relocate(c("region"))
  
  ### Return
  return(df0)
})(); co_regions |> glimpse()
configList[["co_regions"]] <- co_regions


### Get new states
co_states    <- DBI::dbReadTable(con,"co_states")
co_states |> glimpse()
co_states   <- co_states |> (function(df0){
  ### Add US area and region
  ### Rename values
  df0       <- df0 |> mutate(us_area = "CONUS")
  
  ### Add new values
  df1       <- tibble(
    state  = c("Alaska", "Hawaii"), 
    postal = c("AK", "HI"), 
    fips   = c(2, 15)
  ) |> ### End tibble
    mutate(us_area = state) |> 
    mutate(region  = state)
  
  ### Bind values and arrange
  arrange0  <- c("us_area", "region", "state")
  df0       <- df0 |> rbind(df1)
  df0       <- df0 |> relocate(c("region"))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); co_states |> glimpse()
configList[["co_states"]] <- co_states

### Check that all states and regions are present in population data
listLoad$pop$data$popBase |> pull(State_FIPS) |> unique() |> length()
((listLoad$pop$data$popBase |> pull(State_FIPS) |> unique()) %in% (co_states$fips |> unique())) |> all()
((co_states$fips |> unique()) %in% (listLoad$pop$data$popBase |> pull(State_FIPS) |> unique())) |> all()




### Model Types and Models ----------------
#### Model Types ----------------
### Model Types
co_modelTypes    <- DBI::dbReadTable(con,"co_modelTypes")
co_modelTypes |> glimpse()
co_modelTypes <- co_modelTypes |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Create new values to bind
  df1       <- tibble(modelType_id = "gcm") |> 
    mutate(modelType_label = "GCM") |> 
    mutate(inputName       = "methane") |> 
    mutate(modelUnitDesc   = "Ozone Response") |> 
    mutate(modelUnit_id    = "pptv/pptb") |> 
    mutate(modelUnit_label = "pptv/pptb")
  
  ### Filter to no values
  ### Bind new values to other values
  select0   <- df1 |> names()
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> filter(inputName %in% "GCM")
  df0       <- df0 |> rbind(df1)
  
  ### Return
  return(df0)
})(); co_modelTypes |> glimpse()
configList[["co_modelTypes"]] <- co_modelTypes

#### Models ----------------
### Get new models and model types
co_models    <- DBI::dbReadTable(con,"co_models")
co_models |> glimpse()
co_models   <- co_models |> (function(
    df0,
    df1,
    mod_str0 = listLoad$o3$data$o3Nat$Model |> unique() |> paste(collapse="|")
){
  ### Filter to GCM values
  df0       <- df0 |> filter(modelType %in% "gcm")
  
  ### Rename columns
  to0   <- c("model", "modelUnit")
  from0 <- c(to0  ) |> paste0("_id")
  df0       <- df0 |> rename_at(c(from0 |> c("maxUnitValue")), ~to0   |> c("gcmMaxTemp"))
  rm(from0, to0  )
  
  ### Select columns
  select0   <- c("model", "model_label", "modelType", "gcmMaxTemp")
  df0       <- df0 |> select(all_of(select0))
  rm(select0)
  
  ### Get model string to match
  ### Match values
  ### Filter to matches
  df0       <- df0 |> mutate(model_match = model       |> str_replace("Can", "C"))
  df0       <- df0 |> mutate(model_str   = model_match |> str_match(pattern=mod_str0) |> as.list() |> unlist())
  df0       <- df0 |> filter(!(model_str |> is.na()))
  
  ### Return
  return(df0)
})(); co_models |> glimpse()
configList[["co_models"]] <- co_models



### Input Info ----------------
### Input Info
co_inputInfo    <- DBI::dbReadTable(con,"co_inputInfo")
co_inputInfo |> glimpse()
co_inputInfo   <- co_inputInfo |> (function(df0){
  ### Create new values to bind
  df1       <- tibble(inputName = c("ch4", "nox", "o3")) |>
    mutate(inputType       = c("methane", "nox", "ozone")) |>
    mutate(inputDesc       = c("Methane", "NOx", "Ozone")) |>
    mutate(inputUnit       = c("ppbv"   , "Mt" , "ppbv")) |>
    mutate(inputMin        = NA) |>
    mutate(inputMax        = NA) |>
    mutate(valueCol        = c("CH4_ppbv", "NOx_Mt", "O3_ppbv")) |>
    mutate(region          = c(0, 0, 1))

  ### Filter to no values
  ### Bind new values to other values
  filter0   <- c("gdp", "pop")
  df0       <- df0 |> filter(inputName %in% c(filter0))
  df0       <- df0 |> rbind(df1)

  ### Return
  return(df0)
})(); co_inputInfo |> glimpse()
configList[["co_inputInfo"]] <- co_inputInfo







## Reshape Base Data ----------------
### Ozone Response ----------------
### Droped actions
# df0       <- df0   |> mutate(base_Nat_respMrate = mRate0)
### National O3 reshaping
listLoad$o3$data$o3Nat |> glimpse()
### Use units pptv to ppbv
nat_o3  <- listLoad$o3$data$o3Nat |> (function(
    df0, 
    dfM   = co_models, 
    ch4_0 = configList$coefficients$CH4$base0, ### pptbv (100)
    nox_0 = configList$coefficients$NOx$base0  ### Mt (10.528)
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  drop0     <- c("OzoneResponse.ppb.ppb.")
  from0     <- c("Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  to0       <- c("model_str", "nat_o3response_pptv_per_ppbv", "base_nat_deltaO3_pptv")
  df0       <- df0 |> 
    select(-any_of(drop0)) |> 
    rename_at(c(from0), ~to0)
  rm(drop0, from0, to0)
  
  ### Join values with models
  select0   <- c("model", "model_label", "model_str")
  join0     <- c("model_str")
  dfM       <- dfM |> select(all_of(select0))
  df0       <- df0 |> 
    left_join(dfM, by=join0) |> 
    # filter(!(model |> is.na())) |> 
    relocate(any_of(select0)) |>
    arrange_at(c("model"))
  rm(select0, join0, dfM)
  
  ### Base CH4 and NOx concentrations
  df0       <- df0 |> 
    mutate(base_CH4_ppbv = ch4_0) |> 
    mutate(base_NOx_Mt   = nox_0)
  
  
  ### Return
  return(df0)
})(); nat_o3 |> glimpse()
configList[["nat_o3"]] <- nat_o3

### State Ozone Reshaping
### - Drop Ozone PPT/PPB
### - Rename State_FIPS to fips, Model to model_str, Ozone response to o3response_ppb2ppb, DeltaOzone to deltaO3
### - Standardize model by:
###   - Mutate co_models: add column "model_str" by using mutate(model_str = model_id |> str_match(pattern=o3State$Model |> unique() |> paste(collapse="|")) |> as.list() |> unlist())
###   - Joining with co_models by "model_str"

# 5*51
listLoad$o3$data$o3State |> glimpse()
state_o3 <- listLoad$o3$data$o3State |> (function(
    df0, 
    dfS = co_states, 
    dfN = nat_o3 
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  drop0     <- c("OzoneResponse.ppb.ppb.")
  from0     <- c("State_FIPS", "Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  to0       <- c("fips", "model_str", "state_o3response_ppbv_per_ppbv", "base_state_deltaO3_ppbv")
  df0       <- df0 |> 
    select(-any_of(drop0)) |> 
    rename_at(c(from0), ~to0)
  rm(drop0, from0, to0)
  # df0 |> glimpse()
  
  ### Join states with national values
  join0     <- c("fips", "model_str")
  move0     <- c("model", "model_label", "model_str")
  sort0     <- c("region", "state", "model")
  dfJoin0   <- dfS     |> cross_join(dfN)
  df0       <- dfJoin0 |> 
    left_join(df0, by=join0) |> 
    # filter(!(model |> is.na())) |> 
    relocate(any_of(move0), .after=c("fips")) |> 
    arrange_at(c(sort0))
  rm(dfS, dfN, join0, move0, sort0)
  
  ### Return
  return(df0)
})(); state_o3 |> glimpse()
state_o3$model |> unique()
stateData[["state_o3"]] <- state_o3






### Population ----------------
#### Default Population ----------------
### Reshape default population data (from BenMAP runs):
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario

### Glimpse data
listLoad$pop$data$popAll |> filter_all(all_vars((. |> is.na()))) |> glimpse()
listLoad$pop$data$popAll |> glimpse()
listLoad$pop$data$popAll$region |> unique() |> sort()
listLoad$pop$data$popAll$state |> unique() |> sort()
listLoad$pop$data$popAll$state |> unique() |> length()
listLoad$pop$data$popAll$year |> range()

co_states |> glimpse()
co_states$region |> unique() |> sort()
co_states$state |> unique() |> sort()
# listLoad$pop$data$popAll |> group_by(state, year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()

def_state_pop <- listLoad$pop$data$popAll |> (function(
    df0, 
    # dfS    = co_states,
    minYr0 = configList$coefficients$minYear0, ### 2020
    maxYr0 = 2300
    # maxYr0 = configList$coefficients$maxYear0  ### 2100
){
  ### Filter values
  ### Select and rename columns
  ### Join with states
  select0   <- c("region", "state", "postal", "fips", "year", "state_pop")
  from0     <- c("state_pop")
  to0       <- c("pop")
  df0       <- df0 |> 
    # filter(year >= minYr0, year <= maxYr0) |> 
    filter(year <= maxYr0) |> 
    select(all_of(select0)) |> 
    rename_at(c(from0), ~to0) |>
    mutate(region_id = region |> str_replace_all(" ", ""))
  df0$region_id |> unique() |> print()
  rm(select0)
  
  ### Standardize years and states
  # drop0     <- c("us_area", "region", "state", "postal")
  drop0     <- c("us_area")
  from0     <- c("region")
  to0       <- c("region_id")
  yrs0      <- df0 |> pull(year) |> unique() |> sort()
  dfYrs0    <- tibble(year = yrs0)
  dfJoin0   <- co_states |> 
    cross_join(dfYrs0) |> 
    rename_at(c(from0), ~to0) |>
    select(-any_of(drop0))
  
  ### Join standardized years and states with data
  join0     <- c("region_id", "state", "postal", "fips", "year")
  df0       <- dfJoin0 |> left_join(df0, by=join0)
  
  ### Filter to years
  # df0       <- df0 |> filter(year <= maxYr0)
  
  ### Return
  return(df0)
})(); def_state_pop |> glimpse()
### Update in list
stateData[["def_state_pop"]] <- def_state_pop

def_state_pop |> group_by_at(c("state")) |> summarize(n=n()) |> filter(n < 91)
def_state_pop |> filter_all(all_vars((. |> is.na()))) |> glimpse()
def_state_pop$region |> unique() |> sort()
def_state_pop$state |> unique() |> sort()
def_state_pop$year |> range(); def_state_pop |> pull(postal) |> unique() |> length()
# def_state_pop$region |> unique()


#### Base Population ----------------
### Reshape base population data (from BenMAP runs):
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario

### View data
listLoad$pop$data$popBase |> glimpse()
listLoad$pop$data$popBase$Year |> unique()
mortBasePopState <- listLoad$pop$data$popBase |> (function(
    df0, 
    dfS = co_states
){
  ### Rename values
  from0     <- c("State_FIPS", "Year", "Population")
  to0       <- c("fips", "base_year", "basePopState")
  df0       <- df0 |> 
    rename_at(c(from0), ~to0) |>
    relocate(any_of(to0))
  rm(from0, to0)
  
  ### Select
  join0     <- c("fips")
  sort0     <- c("region", "state")
  df0       <- dfS |>
    left_join(df0, by=join0) |>
    # select(-any_of(drop0)) |> 
    arrange_at(c(sort0))
  
  ### Return
  return(df0)
})(); mortBasePopState |> glimpse()
stateData[["mortBasePopState"]] <- mortBasePopState
mortBasePopState$base_year |> range(); mortBasePopState |> pull(postal) |> unique() |> length()




### Morbidity ----------------
#### Affected Population by Age Range ----------------
### For morbidity
### - Change column names
### - Standardize with states
### - Split the string for age range into new columns, convert to numeric
### Interpolate over years

### Glimpse
listLoad$pop$data$popAge |> glimpse()
listLoad$pop$data$popAge$Year |> range()
listLoad$pop$data$popAge$newAgeRange |> unique()
# 51 * 2 * (81 + 200) # = 28662
# 51*2*81
loadCode()
asthmaAgePcts <- listLoad$pop$data$popAge|> format_ghgAsthmaAffectedPop(
  dfS    = co_states, 
  dfA    = co_ageTypes, 
  minYr0 = configList$coefficients$minYear0,
  maxYr0 = 2300
  # maxYr0 = configList$coefficients$maxYear0
); asthmaAgePcts |> glimpse()
stateData[["asthmaAgePcts"]] <- asthmaAgePcts



#### Asthma Incidence/ER Visits ----------------
### Rename columns
### Standardize with states
### Drop multi-model mean

### Endpoints c("Incidence, Asthma", "Emergency Room Visits, Asthma")
listLoad$asth$data$asthExcess |> glimpse()
listLoad$asth$data$asthExcess$Endpoint |> unique()
listLoad$asth$data$asthExcess$Start_Age |> unique()
listLoad$asth$data$asthExcess$End_Age |> unique()
listLoad$asth$data$asthExcess$State_FIPS |> unique()

listLoad$asth$data$asthExcess$Model |> unique()
listLoad$asth$data$asthExcess$ModelYear |> range()

listLoad$asth$data$asthExcess$ModelYear |> unique()
# listLoad$asth$data$asthma  |> glimpse()

# 51 * 3 * 5 * (81 + 200) # = 214965
loadCode()
df_asthmaImpacts <- listLoad$asth$data$asthExcess |> format_ghgAsthmaExcessCases( 
  df1 = asthmaAgePcts,
  dfM = co_models,
  dfT = co_impactTypes, 
  minYr0 = configList$coefficients$minYear0,
  maxYr0 = 2300
  # maxYr0 = configList$coefficients$maxYear0
); df_asthmaImpacts |> glimpse()
# 61965*2
df_asthmaImpacts |> filter(model_label |> is.na()) |> glimpse()
stateData[["df_asthmaImpacts"]] <- df_asthmaImpacts


### Mortality ----------------
#### IF Mortality Rate Scalar ----------------
### Form IF Mortality Rate Scalar and join with RFF data
listLoad$mort$data$mortScalar |> glimpse(); 
listLoad$mort$data$mortScalar$Years |> unique() |> head()
listLoad$mort$data$mortScalar$Years |> range()
# nat_ifScalar <- listLoad$mort$data$mortScalar |> (function(
#     df0,
#     minYr0 = configList$coefficients$minYear0,
#     maxYr0 = 2300
# ){
#   ### Glimpse data
#   df0 |> glimpse()
#   idCols0   <- c("year", "fipsNat")
#   sumFrom0  <- c("DeathCounts", "MortRate") |> 
#     map(function(str0, vals0=c("AllCause", "Resp")){
#       vals0 |> paste0(str0)
#     }) |> unlist() |> c("Population", "RespScalar")
#   sumTo0    <- sumFrom0 |> 
#     str_replace("ulation", "") |> 
#     str_replace("AllCause", "All") |> 
#     str_replace("DeathCounts", "Mort") |> 
#     str_replace("MortRate", "Mrate") |>
#     (function(val0, str0="if"){str0 |> paste0(val0)})()
#   from0     <- c("Years", "Region") |> c(sumFrom0)
#   # to0       <- c("year", "nation_fips") |> c("if" |> paste0(sumCols1))
#   to0       <- idCols0 |> c(sumTo0)
#   sumCols0  <- to0     |> get_matches(idCols0, matches=F)
#   from0 |> print(); to0 |> print(); sumCols0 |> print()
#   
#   ### Rename and relocate columns
#   df0       <- df0 |> 
#     rename_at(c(from0), ~to0) |> 
#     relocate(c("ifPop"), .after="year")
#   
#   ### Get years
#   newYrs0 <- minYr0:maxYr0
#   group0  <- c("group")
#   sum0    <- sumCols0
#   df0     <- df0 |> 
#     mutate(group = "group") |>
#     group_by_at(c(group0)) |> 
#     group_map(function(.x, .y){
#       .x |> ghg_groupMap(
#         .y     = .y,
#         yCols0 = sum0  , ### Columns to sum
#         xCol0  = "year", ### X column
#         xOut0  = newYrs0 ### New or x values
#       ) ### End ghg_groupMap
#     }) |> bind_rows() |> 
#     ungroup() |>
#     select(-any_of(group0))
#   
#   ### Return
#   return(df0)
# })(); nat_ifScalar |> glimpse()
# # listData[["nat_ifScalar"]] <- nat_ifScalar
# configList[["nat_ifScalar"]] <- nat_ifScalar

#### RFF Population & Mortality ----------------
# ### Previous process
# years0    <- df0 |> pull(year)
# minYr0    <- years0 |> min()
# maxYr0    <- years0 |> max()
# years     <- minYr0 |> seq(maxYr0, by=1)
# df0       <- tibble(year = years) |> left_join(df0, by=join0)
# df0       <- df0 |> mutate_all(zoo::na.approx, na.rm=F)
# ### Get years for IF data corresponding to RFF data, and linearly extend values
# join0     <- c("year")
# fill0     <- df1 |> names() |> (function(x, y=join0){x[!(x %in% y)]})()
# df1       <- df1 |> filter(year %in% years)
# df1       <- tibble(year = years) |> left_join(df1, by=join0)
# df1       <- df1 |> fill(all_of(fill0), .direction="down")
# 
# ### Join df0 and df1
# join0     <- c("year")
# df0       <- df1 |> left_join(df0, by=join0)

### Reshape population & mortality data (from RFF runs):
### Then join with IF Scalar Data
listLoad$pop$data$popRff |> glimpse()
listLoad$pop$data$popRff$Year |> range()

listLoad$mort$data$mortScalar |> glimpse(); 
listLoad$mort$data$mortScalar$Years |> unique() |> head()
listLoad$mort$data$mortScalar$Years |> range()

# rff_nat_pop
natMRateInfo <- listLoad$pop$data$popRff |> (function(
    df0, 
    # dfN    = nat_ifScalar,
    dfN    = listLoad$mort$data$mortScalar,
    minYr0 = configList$coefficients$minYear0,
    maxYr0 = 2300
    # maxYr0 = configList$coefficients$maxYear0
){
  ### Rename values
  idCols0   <- "year"
  sumCols0  <- "rff" |> paste0(c("Pop", "Mort", "Mrate", "Mrate_intercept", "Mrate_slope"))
  from0     <- c("Year", "pop", "mortality", "mort_rate", "mort_rate_intercept", "mort_rate_slope")
  to0       <- idCols0 |> c(sumCols0)
  df0       <- df0     |> rename_at(c(from0), ~to0)
  
  ### Get years
  newYrs0   <- minYr0:maxYr0
  group0    <- c("group")
  sum0      <- sumCols0
  df0       <- df0 |> 
    mutate(group = "group") |>
    group_by_at(c(group0)) |> 
    group_map(function(.x, .y){
    .x |> ghg_groupMap(
      .y     = .y,
      yCols0 = sum0  , ### Columns to sum
      xCol0  = "year", ### X column
      xOut0  = newYrs0 ### New or x values
    ) ### End ghg_groupMap
  }) |> bind_rows() |> 
    ungroup() |>
    select(-any_of(group0))
  
  ### Glimpse data
  dfN |> glimpse()
  idCols0   <- c("year", "fipsNat")
  sumFrom0  <- c("DeathCounts", "MortRate") |> 
    map(function(str0, vals0=c("AllCause", "Resp")){
      vals0 |> paste0(str0)
    }) |> unlist() |> c("Population", "RespScalar")
  sumTo0    <- sumFrom0 |> 
    str_replace("ulation", "") |> 
    str_replace("AllCause", "All") |> 
    str_replace("DeathCounts", "Mort") |> 
    str_replace("MortRate", "Mrate") |>
    (function(val0, str0="if"){str0 |> paste0(val0)})()
  from0     <- c("Years", "Region") |> c(sumFrom0)
  # to0       <- c("year", "nation_fips") |> c("if" |> paste0(sumCols1))
  to0       <- idCols0 |> c(sumTo0)
  sumCols0  <- to0     |> get_matches(idCols0, matches=F)
  from0 |> print(); to0 |> print(); sumCols0 |> print()
  
  ### Rename and relocate columns
  dfN       <- dfN |> 
    rename_at(c(from0), ~to0) |> 
    relocate(c("ifPop"), .after="year")
  
  ### Get years
  newYrs0 <- minYr0:maxYr0
  group0  <- c("group")
  sum0    <- sumCols0
  dfN     <- dfN |> 
    mutate(group = "group") |>
    group_by_at(c(group0)) |> 
    group_map(function(.x, .y){
      .x |> ghg_groupMap(
        .y     = .y,
        yCols0 = sum0  , ### Columns to sum
        xCol0  = "year", ### X column
        xOut0  = newYrs0 ### New or x values
      ) ### End ghg_groupMap
    }) |> bind_rows() |> 
    ungroup() |>
    select(-any_of(group0))

  ### Join df0 and dfN
  join0     <- c("year")
  names1    <- dfN |> names()
  df0       <- dfN |> left_join(df0, by=join0)
  
  ### Return
  return(df0)
})(); natMRateInfo |> glimpse()
configList[["natMRateInfo"]] <- natMRateInfo



### Update coefficients
### Function of mortality as a function of population
# calc_mortality <- function(
#     df0,     ### Tibble with population and years
#     df1      = rff_nat_pop, ### Tibble with columns for mortality rate slope and mortality rate intercept
#     pCol0    = "national_pop"      , ### Column with national population
#     sCol0    = "rffMrate_slope"    , ### Column with mortality rate slope,
#     iCol0    = "rffMrate_intercept", ### Column with mortality rate intercept,
#     joinCols = c("year") ### Column to join df0 and df1
# ){
#   ### Join df0 and df1
#   # join0 <- c("year")
#   join0 <- joinCols
#   df0   <- df0 |> left_join(df1, by=join0)
#   rm(df1)
#   ### Calculate intermediate populations
#   # df0   <- df0 |> mutate(delta_rffPop = national_pop - rffPop)
#   # df0   <- df0 |> mutate(rffFactor    = delta_rffPop * rffMrate_slope + rffMrate_intercept)
#   df0   <- df0 |> mutate(logPop       = (!!sym(pCol0)) |> log())
#   df0   <- df0 |> mutate(rffFactor    = logPop       * !!sym(sCol0) + !!sym(iCol0))
#   df0   <- df0 |> mutate(respMrate    = rffFactor    * ifRespScalar)
#   ### Return data
#   return(df0)
# }
# ### Update in list
# # listData$coefficients[["Mortality"]][["fun0"]] <- calc_mortality
# configList$coefficients[["Mortality"]][["fun0"]] <- calc_mortality


#### Baseline Mortality Rate ----------------
##### National ----------------
listLoad$mort$data$mortBase  |> glimpse()
baseMortNat <- listLoad$mort$data$mortBase |> (function(
    df0
){
  ### Rename values
  from0 <- c("MortalityIncidence")
  to0   <- c("baseMrateNat0")
  df0   <- df0 |> rename_at(c(from0), ~to0)
  # df0 |> glimpse()
  ### Return
  return(df0)
})(); baseMortNat |> glimpse()
stateData[["baseMortNat"]] <- baseMortNat

##### State ----------------
### Expand grid, na.approx
# 51 * 281 # = 14331
listLoad$mort$data$mortBase  |> glimpse()
listLoad$mort$data$mortState |> glimpse()
loadCode()
baseMortState <- listLoad$mort$data$mortState |> (function(
    df0,
    dfS    = co_states,
    # dfX    = listLoad$mort$data$mortXm, ### By state
    dfR    = baseMortNat,
    minYr0 = configList$coefficients$minYear0,
    maxYr0 = 2300
    # maxYr0 = configList$coefficients$maxYear0
){
  ### Rename values
  from0     <- c("Column", "Year")
  to0       <- c("fips", "year")
  df0       <- df0 |> 
    rename_at(c(from0), ~to0) |>
    arrange_at(c(to0))
  # df0 |> glimpse()
  
  ### Mutate excess mortality data
  
  ### New years
  yrs0      <- df0 |> pull(year) |> unique() |> sort()
  dfYrs0    <- tibble(year = yrs0)
  newYrs0   <- minYr0:maxYr0
  # newYrs0   <- yrs0 |> min() |> seq(maxYr0)
  
  ### Expand values to additional fips
  join0     <- c("fips", "year")
  dfJoin0   <- dfS |> 
    cross_join(dfYrs0) |>
    arrange_at(c(join0))
  
  ### Join and arrange
  # "gothere1" |> print(); df0 |> glimpse(); dfYrs0 |> glimpse()
  # dfJoin0   <- co_states |> cross_join(dfYrs0) |> arrange_at(c(join0))
  sort0     <- c("fips", "year")
  group0    <- c("us_area", "region", "state", "postal", "fips")
  df0       <- dfJoin0 |> 
    left_join(df0, by=join0) |> 
    mutate(StateMortRatio = StateMortRatio |> replace_na(1)) |>
    arrange_at(c(join0)) |>
    group_by_at(c(group0))
  rm(dfJoin0)
  
  ### Extend values to additional years
  sum0      <- c("StateMortRatio")
  df0       <- df0 |> group_map(function(.x, .y){
    .x |> ghg_groupMap(
      .y     = .y,
      yCols0 = sum0  ,  ### Columns to sum
      xCol0  = "year",  ### X column
      xOut0  = newYrs0, ### New or x values
      rule0  = 2
    ) ### End ghg_groupMap
  }) |> bind_rows() |> ungroup() |>
    relocate(any_of(group0)) |>
    relocate(c("year"), .after="fips")
    
  ### Calculate base mortality rate and join with states
  df0       <- df0 |>
    cross_join(dfR) |>
    mutate(baseMrateState = baseMrateNat0 * StateMortRatio)
    
  ### Return
  return(df0)
})(); baseMortState |> glimpse()
stateData[["baseMortState"]] <- baseMortState
baseMortState$baseMrateNat0 |> range()
baseMortState$baseMrateState |> range()

#ghgData$stateData$baseMortState |> glimpse()



### State Excess Mortality ----------------
### State Excess Mortality reshaping
### Model	ModelYear	State_FIPS	State_Results |> rename to: c(model_str, refYear, fips, excess_mortality)
# ghgData$ghgData$co_states
#co_states <- ghgData$ghgData$co_states
#co_models <- ghgData$ghgData$co_models
listLoad$mort$data$mortXm |> glimpse(); 
listLoad$mort$data$mortXm$ModelYear |> range()
baseMortState |> glimpse()
state_xMort <- listLoad$mort$data$mortXm |> (function(
    xm0, 
    dfS = co_states, 
    dfM = co_models, 
    dfB = baseMortState,
    baseYr0 = 2020
){
  ### Glimpse data
  # xm0 |> glimpse()
  
  ### Rename values
  from0     <- c("State_FIPS", "Model", "ModelYear", "State_Results")
  to0       <- c("fips", "model_str", "base_year", "exMortStateBase0")
  xm0       <- xm0 |> 
    rename_at(c(from0), ~to0) |> 
    relocate(any_of(to0))
  rm(from0, to0)
  
  ### Mutate base values
  from0     <- c("year")
  to0       <- c("base_year")
  drop0     <- c("us_area", "region", "state", "postal")
  dfB       <- dfB |>
    rename_at(c(from0), ~to0) |> 
    # filter(year == "2020") |> 
    select(-any_of(drop0))
  rm(from0, to0, drop0)
  
  ### Standardize states and models
  dropM     <- c("modelType", "model_match", "gcmMaxTemp")
  join0     <- c("fips", "model_str")
  joinB     <- c("fips", "base_year")
  dfM       <- dfM |> select(-any_of(dropM))
  dfJoin0   <- dfS |> cross_join(dfM)
  dfJoin0   <- dfJoin0 |> left_join(xm0, by=join0)
  dfJoin0   <- dfJoin0 |> left_join(dfB, by=joinB)
  dfJoin0 |> filter((model |> is.na())) |> glimpse()
  rm(dfS, dfM, xm0, dfB, dropM)
  
  ### Adjust state Values
  ### Select and arrange
  sort0     <- c("region", "state", "model")
  from0     <- c("StateMortRatio", "baseMrateState") 
  to0       <- from0 |> paste0("0")
  dfJoin0   <- dfJoin0 |>
    rename_at(c(from0), ~to0) |>
    mutate(exMortState0 = exMortStateBase0 * StateMortRatio0) |> 
    # select(-any_of(drop0)) |> 
    arrange_at(c(sort0))
  
  ### Return
  return(dfJoin0)
})(); state_xMort |> glimpse()
# state_xMort <- state_xMort |> rename_at(c("exMortState"), ~c("exMortState0"))
# state_xMort |> glimpse()
stateData[["state_xMort"]] <- state_xMort


### Calculate RR Scalar ----------------

#"mortBasePopState" <- ghgData$stateData[["mortBasePopState"]]
#"baseMortState" <- ghgData$stateData[["baseMortState"]]
#"state_o3" <- ghgData$stateData[["state_o3"]]
# "state_xMort" <- ghgData$stateData[["state_xMort"]]

### Base year for: mortBasePopState
mortBasePopState |> glimpse(); 
state_o3 |> glimpse();
state_xMort |> glimpse(); 
baseMortState |> glimpse()
state_rrScalar <- mortBasePopState |> (function(
    pop0, 
    o3_0 = state_o3, 
    xm_0 = state_xMort,
    dfT  = co_impactTypes, ### Impact types
    dfB  = baseMortState # Get Base Year baseline mortality adjustment
){
  #### Join o3 and excess mortality data
  drop0     <- c("region", "state", "postal", "us_area") |> 
    c("model_label", "modelType", "model_match", "model_str")
  join0     <- c("fips", "model") 
  joinP     <- c("fips", "base_year")
  xm_0      <- xm_0 |> select(-any_of(drop0))
  pop0      <- pop0 |> select(-any_of(drop0))
  df0       <- o3_0 |> left_join(xm_0, by=join0)
  # df0       <- xm_0
  # df0 |> glimpse()
  df0       <- df0  |> left_join(pop0, by=joinP)
  # df0 |> glimpse()
  rm(drop0, join0, o3_0, xm_0, pop0)
  
  #### Calculate rr Scalar
  # from0     <- c("baseMrateState", "StateMortRatio")
  # to0       <- from0 |> paste0("0")
  df0       <- df0 |> 
    # rename_at(c(from0), ~to0) |>
    mutate(state_rrScalar   = basePopState * baseMrateState0 * base_state_deltaO3_ppbv) |> 
    mutate(state_mortScalar = exMortState0 / state_rrScalar)
  
  # ### Add sector and impact type
  # df0    |> glimpse()
  sort0     <- c("sector", "impactType", "impactType_label", "endpoint", "ageType", "ageRange", "fips", "model")
  join0     <- c("sector")
  namesT    <- dfT |> names()
  df0       <- df0 |> mutate(sector = "mortality")
  df0       <- df0 |> 
    left_join(dfT, by=join0) |>
    arrange_at(c(sort0)) |>
    relocate(any_of(namesT))
  
  ### Return
  return(df0)
})(); state_rrScalar |> glimpse()
stateData[["state_rrScalar"]] <- state_rrScalar
state_rrScalar$model |> unique()


## Default Scenarios ----------------
### GDP ----------------
### Default GDP scenario = Default FrEDI scenario
scenariosList[["gdp_default"]] <- scenarioData$gdp_default

### Population ----------------
#### Extend pop ratios ----------------
dfPopRatios <- 
  scenarioData$popRatiosData |>
  # "popRatiosData" |> get_frediDataObj("scenarioData") |> 
    (function(df0){
    cols0  <- c("area", "area2nat", "year")
    group0 <- c("year")
    sum0   <- c("area2nat")
    df1    <- df0 |>
      select(all_of(cols0)) |>
      distinct() |>
      filter(area |> tolower() |> str_detect("conus")) |>
      select(-c("area")) |>
      rename_at(c("area2nat"), ~"area2nat_conus")
    # df1 |> glimpse()
    df0    <- df0 |>
      left_join(df1, by="year") |>
      mutate(area2conus = area2nat / area2nat_conus, .after="area2nat") |>
      select(-c("area2nat_conus"))
    return(df0)
  })() |>
  FrEDI:::fun_extendVals(
    from0 = 2100,
    to0   = 2300,
    sort0 = c("area", "region", "state", "year")
  )

### Default population scenario = make scenario from FrEDI data
scenarioData$popData$region |> unique()
pop_default <- def_state_pop |> (function(
    df0,
    df1 = dfPopRatios
){
  drop0 <- c("fips", "area", "us_area")
  df0   <- df0 |> select(-any_of(drop0))
  df0   <- df0 |> filter(year >= 2020)
})(); pop_default |> glimpse()
scenariosList[["pop_default"]] <- pop_default

### Methane ----------------
### Default methane scenario
listLoad$ch4$data$ch4Ssp245  |> glimpse()
ch4_default <- listLoad$ch4$data$ch4Ssp245 |> (function(
    df0,
    minYr0 = list_coefficients$minYear0,
    maxYr0 = list_coefficients$maxYear0
){
  ### Years 
  yrs0      <- minYr0:maxYr0
  df1       <- tibble(year=yrs0)
  ### Rename columns
  # df0       <- df0 |> rename("CH4_ppbv" =  ch4_ppb)
  from0     <- c("ch4_ppb")
  to0       <- c("CH4_ppbv")
  df0       <- df0 |> rename_at(c(from0), ~to0  )
  ### Filter to 2100
  join0     <- c("year")
  df0       <- df1 |> left_join(df0, by=join0)
  ### Return
  return(df0)
})(); ch4_default |> glimpse()
scenariosList[["ch4_default"]] <- ch4_default

### NOx ----------------
### Default NOx scenario
nox_default <- list_coefficients$minYear0:list_coefficients$maxYear0 |> (function(
    years0, 
    nox_0  = list_coefficients$NOx$base0
){
  ### Make tibble, add value, return
  df0 <- tibble(year = years0)
  df0 <- df0 |> mutate(NOx_Mt = nox_0)
  return(df0)
})(); nox_default |> glimpse()
scenariosList[["nox_default"]] <- nox_default

### O3 ----------------
### Default O3 scenario
state_o3 |> glimpse()
o3_default <- ch4_default |> (function(
    df0,
    df1     = nox_default,
    df2     = state_o3,
    df3     = co_regions,
    noxAdj0 = list_coefficients$NOx$NOxFactor0,
    fun0    = list_coefficients$NOx$fun0
){
  ### Join methane and NOx
  join0     <- c("year")
  df0       <- df0 |> left_join(df1, by=c(join0))
  rm(join0)
  
  ### Join with state O3 response
  drop0     <- c("model_str")
  join0     <- c("modelType")
  move0     <- c("region", "state", "postal", "model", "model_label")
  df0       <- df0 |> mutate(modelType = "gcm")
  df2       <- df2 |> mutate(modelType = "gcm")
  df2       <- df2 |> select(-any_of(drop0))
  df0       <- df0 |> left_join(df2, by=c(join0), relationship="many-to-many")
  df0       <- df0 |> relocate(all_of(move0))
  df0       <- df0 |> select(-any_of(join0))
  rm(join0)

  ### Get information on region
  ### - Join with region and standardize region
  join0     <- c("region")
  # df3 |> glimpse(); df0 |> glimpse()
  df3$region |> unique() |> print(); df0$region |> unique() |> print()
  df0       <- df3 |> left_join(df0, by=join0)
  # df0       <- df0 |> mutate()

  ### Drop region, rename values
  # from0 <- c(join0)  |> paste0("_label")
  # to0   <- c(join0)
  to0       <- c("region", "model")
  from0     <- c(to0  )  |> paste0("_label")
  drop0     <- c(join0) |> c(to0  ) |> c("us_area", "fips")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> rename_at(c(from0), ~to0  )
  df0       <- df0 |> relocate(c("model"), .before=c("year"))

  ### Calculate NOx ratio, then O3 response
  ### calc_NOx_factor
  # df0 |> glimpse()
  df0       <- df0 |> mutate(nox_factor0 = noxAdj0)
  df0       <- df0 |> mutate(nox_factor  = NOx_Mt |> fun0())
  df0       <- df0 |> mutate(nox_ratio   = nox_factor / nox_factor0)
  df0       <- df0 |> mutate(O3_pptv     = CH4_ppbv * nox_ratio * state_o3response_ppbv_per_ppbv)
  
  ### Arrange
  arrange0  <- c("region", "state", "model", "year")
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); o3_default |> glimpse()
o3_default$region |> unique(); o3_default$model |> unique()
scenariosList[["o3_default"]] <- o3_default


## Save Data ----------------
### Update GHG List Data ----------------
# configList$co_impactTypes
# configList |> glimpse()
# stateData |> glimpse()
ghgData[["scenarioData"]] <- scenariosList
ghgData[["ghgData"     ]] <- configList
ghgData[["stateData"   ]] <- stateData

### Save GHG Data ----------------
### Update Data in List & Save List
# oPath0 |> file.path("ghg", "ghgData") |> paste0(".", "rda") |> load()
saveFile   <- oPath0 |> file.path("ghg", "ghgData") |> paste0(".", "rda")
save(ghgData, file=saveFile)

dbDisconnect(con)
gc()
### Update System Data ----------------
# projDir |> file.path("R", "fun_saveSysData.R") |> source()
oPath0 |> fun_saveSysData(
  modules = c("ghg", "sv"),
  outFile = "sysData",
  extStrs = c("rda", "rds")
) ### End fun_saveSysData



## End File ----------------
