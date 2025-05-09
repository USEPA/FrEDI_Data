## Set Up Environment ----------------
### Packages ----------------
require(tidyverse)
require(devtools)
require(zoo)
# require(FrEDI)

### File Paths ----------------
### Project directory
projDir   <- "."
"." |> load_all()
### Data dir
### Get file paths: 9 files, including county results
dataDir   <- projDir   |> file.path("inst", "extdata", "ghg")
dataPaths <- dataDir   |> list.files(full.names = T, pattern = "\\.csv")
dataFiles <- dataPaths |> basename(); dataFiles
### File to save results
oPath0    <- projDir   |> file.path("data")

### Exclude county results: 6 files
loadFiles <- dataFiles |> (function(x, str0="County"){x[!(x |> str_detect(str0))]})(); loadFiles

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
      if(hasAdj0   ) {list0[["state" ]] <- stateAdj0  }
      if(hasXm0    ) {list0[["xm"    ]] <- xm0    }
      if(hasScalar0) {list0[["scalar"]] <- scalar0}
    } else if (doch4) { 
      ssp2450      <- files0  |> (function(x, strx="ssp245"  ){x[x |> str_detect(strx)]})()
      hasssp2450   <- ssp2450   |> length()
      if(hasssp2450) {list0[["ssp245"  ]] <- ssp2450  }
    }  else if (doAsth) { 
      asthma      <- files0  |> (function(x, strx="Asthma"){x[x |> str_detect(strx)]})()
      hasAsthma   <- asthma  |> length()
      if(hasAsthma) {list0[["asthma"]] <- asthma  }
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
  listOth[["vsl_adj0"]] <- rDataList$scenarioData$pop_default |> (function(
    df0, 
    df1   = rDataList$scenarioData$gdp_default,
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
### Update list
# listData[["coefficients"]] <- list_coefficients
### add data to list
# ghgData [["original"]] <- listLoad
# listData[["coefficients"]] <- list()
configList[["coefficients"]] <- list_coefficients


## Reshape ID/Crosswalk Tables ----------------
### Sector Info ----------------
#### Sectors ----------------
co_sectors  <- tibble(sector = c("mortality", "morbidity")) |> 
  mutate(sector_label = "Health - " |> paste0(sector |> str_to_title()))
co_sectors |> glimpse()
configList[["co_sectors"]] <- co_sectors

#### Impact Types ----------------
co_impactTypes  <- tibble(
  sector="mortality", 
  impactType="Excess Mortality", 
  impactType_label="N/A"
) |> bind_rows(tibble(
  sector="morbidity", 
  impactType="childAsthma" |> c("asthmaER" |> paste0("_", c("child", "adult"))), 
  impactType_label="New Childhood Asthma" |> c("Asthma ER Visits" |> paste0(" (", c("Children", "Adult"), ")"))
))
co_impactTypes |> glimpse()
configList[["co_impactTypes"]] <- co_impactTypes

#### Sector Info ----------------
co_sectorInfo  <- co_sectors |> left_join(co_impactTypes, by="sector")
co_sectorInfo |> glimpse()
configList[["co_sectorInfo"]] <- co_sectorInfo



### Regions & States ----------------
### Get new regions
co_regions  <- rDataList$frediData$co_regions |> (function(df0){
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
# listData[["co_regions"]] <- co_regions
configList[["co_regions"]] <- co_regions


### Get new states
rDataList$frediData$co_states |> glimpse()
co_states   <- rDataList$frediData$co_states |> (function(df0){
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
# listData[["co_states"]] <- co_states
configList[["co_states"]] <- co_states

### Check that all states and regions are present in population data
listLoad$pop$data$popBase |> pull(State_FIPS) |> unique() |> length()
((listLoad$pop$data$popBase |> pull(State_FIPS) |> unique()) %in% (co_states$fips |> unique())) |> all()
((co_states$fips |> unique()) %in% (listLoad$pop$data$popBase |> pull(State_FIPS) |> unique())) |> all()




### Model Types and Models ----------------
#### Model Types ----------------
### Model Types
rDataList$frediData$co_modelTypes |> glimpse()
co_modelTypes   <- rDataList$frediData$co_modelTypes |> (function(df0){
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
# listData[["co_modelTypes"]] <- co_modelTypes
configList[["co_modelTypes"]] <- co_modelTypes

#### Models ----------------
### Get new models and model types
rDataList$frediData$co_models |> glimpse()
co_models   <- rDataList$frediData$co_models |> (function(
    df0,
    df1,
    mod_str0 = listLoad$o3$data$o3Nat$Model |> unique() |> paste(collapse="|")
){
  ### Filter to GCM values
  df0       <- df0 |> filter(modelType %in% "gcm")
  
  ### Rename columns
  renameTo0 <- c("model", "modelUnit")
  renameAt0 <- c(renameTo0) |> paste0("_id")
  df0       <- df0 |> rename_at(c(renameAt0 |> c("maxUnitValue")), ~renameTo0 |> c("gcmMaxTemp"))
  rm(renameAt0, renameTo0)
  
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
# listData[["co_models"]] <- co_models
configList[["co_models"]] <- co_models



### Input Info ----------------
### Input Info
rDataList$frediData$co_inputInfo |> glimpse()
co_inputInfo   <- rDataList$frediData$co_inputInfo |> (function(df0){
  ### Create new values to bind
  df1       <- tibble(inputName = c("ch4", "nox", "o3")) |>
    mutate(inputType       = c("methane", "nox", "ozone")) |>
    mutate(inputDesc       = c("Methane", "NOx", "Ozone")) |>
    mutate(inputUnit       = c("ppbv"   , "Mt" , "pptv")) |>
    mutate(inputMin        = NA) |>
    mutate(inputMax        = NA) |>
    mutate(valueCol        = c("CH4_ppbv", "NOx_Mt", "O3_pptv")) |>
    mutate(region          = c(0, 0, 1))

  ### Filter to no values
  ### Bind new values to other values
  filter0   <- c("gdp", "pop")
  df0       <- df0 |> filter(inputName %in% c(filter0))
  df0       <- df0 |> rbind(df1)

  ### Return
  return(df0)
})(); co_inputInfo |> glimpse()
# listData[["co_inputInfo"]] <- co_inputInfo
configList[["co_inputInfo"]] <- co_inputInfo







## Reshape Base Data ----------------
### Population ----------------
#### Default Population ----------------
### Reshape default population data (from BenMAP runs):
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario
listLoad$pop$data$popAll |> glimpse()
# listLoad$pop$data$popAll |> group_by(state, year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()
def_state_pop     <- listLoad$pop$data$popAll|> (function(df0, df1=co_states){
  ### Join with states
  select0   <- c("region", "state", "postal", "fips", "year", "state_pop")
  df0       <- df0 |> select(all_of(select0))
  rm(select0)
  
  ### Rename values
  renameAt0 <- c("state_pop")
  renameTo0 <- c("pop")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  
  ### Filter to years
  df0       <- df0 |> filter(year <= 2100)
  
  ### Return
  return(df0)
})(); def_state_pop |> glimpse()
# listData[["def_state_pop"]] <- def_state_pop
stateData[["def_state_pop"]] <- def_state_pop
def_state_pop$year |> range(); def_state_pop |> pull(postal) |> unique() |> length()
# def_state_pop$region |> unique()


#### Base Population ----------------
### Reshape base population data (from BenMAP runs):
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario
listLoad$pop$data$popBase |> glimpse()

base_state_pop     <- listLoad$pop$data$popBase|> (function(df0, df1=co_states){
    ### Rename values
    renameAt0 <- c("Year", "State_FIPS", "Population")
    renameTo0 <- c("base_year", "fips", "base_state_pop")
    df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
    
    ### Join with states
    drop0     <- c("us_area")
    join0     <- c("fips")
    df1       <- df1 |> select(-any_of(drop0))
    df0       <- df1 |> left_join(df0, by=join0)
    rm(drop0, join0)
    
    ### Select
    drop0     <- c("fips")
    arrange0  <- c("region", "state")
    df0       <- df0 |> select(-any_of(drop0))
    df0       <- df0 |> arrange_at(c(arrange0))
    
    ### Return
    return(df0)
  })(); base_state_pop |> glimpse()
# listData[["base_state_pop"]] <- base_state_pop
stateData[["base_state_pop"]] <- base_state_pop
base_state_pop$base_year |> range(); base_state_pop |> pull(postal) |> unique() |> length()



### Mortality ----------------
#### IF Mortality Rate Scalar ----------------
### Form IF Mortality Rate Scalar and join with RFF data
listLoad$mort$data$mortScalar |> glimpse(); listLoad$mort$data$mortScalar$Years |> range()
nat_ifScalar <- listLoad$mort$data$mortScalar |> (function(df0){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  idCols0   <- c("Years", "Region")
  sumCols0  <- c("AllCauseDeathCounts", "RespDeathCounts", "Population", "AllCauseMortRate", "RespMortRate", "RespScalar")
  sumCols1  <- c("AllCauseMort", "RespMort", "Pop", "AllCauseMrate", "RespMrate", "RespScalar")
  renameAt0 <- c(idCols0) |> c(sumCols0)
  # renameTo0 <- c("year", "nation_fips") |> c("nat_if" |> paste0(sumCols0))
  renameTo0 <- c("year", "nation_fips") |> c("if" |> paste0(sumCols1))
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # df0       <- df0 |> relocate(c("year"), .after=c("nation_fips"))
  rm(renameAt0, renameTo0)
  
  ### Return
  return(df0)
})(); nat_ifScalar |> glimpse()
# listData[["nat_ifScalar"]] <- nat_ifScalar
configList[["nat_ifScalar"]] <- nat_ifScalar

#### RFF Population & Mortality ----------------
### Reshape population & mortality data (from RFF runs):
### Then join with IF Scalar Data
listLoad$pop$data$popRff  |> glimpse()
rff_nat_pop     <- listLoad$pop$data$popRff |> (function(df0, df1=nat_ifScalar){
  ### Rename values
  idCols0   <- c("Year")
  sumCols0  <- c("pop", "mortality", "mort_rate", "mort_rate_intercept", "mort_rate_slope")
  sumCols1  <- c("Pop", "Mort", "Mrate", "Mrate_intercept", "Mrate_slope")
  renameAt0 <- c(idCols0) |> c(sumCols0)
  # renameTo0 <- c("base_year", "base_rff_pop", "base_rff_mort", "base_rff_mort_rate", "mort_intercept", "mort_slope")
  renameTo0 <- c("year") |> c("rff" |> paste0(sumCols1))
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  df0       <- df0 |> relocate(all_of(renameTo0))
  
  ### Get tibble with years, join, and linearly interpolate
  join0     <- c("year")
  years0    <- df0 |> pull(year)
  minYr0    <- years0 |> min()
  maxYr0    <- years0 |> max()
  years     <- minYr0 |> seq(maxYr0, by=1)
  df0       <- tibble(year = years) |> left_join(df0, by=join0)
  df0       <- df0 |> mutate_all(zoo::na.approx, na.rm=F)
  # df0       <- df0 |> mutate(across(df0 |> names(), \(x) na.approx(x,na.rm =FALSE)))
  rm(join0, years0, minYr0, maxYr0)

  ### Get years for IF data corresponding to RFF data, and linearly extend values
  join0     <- c("year")
  fill0     <- df1 |> names() |> (function(x, y=join0){x[!(x %in% y)]})()
  df1       <- df1 |> filter(year %in% years)
  df1       <- tibble(year = years) |> left_join(df1, by=join0)
  df1       <- df1 |> fill(all_of(fill0), .direction="down")

  ### Join df0 and df1
  join0     <- c("year")
  df0       <- df1 |> left_join(df0, by=join0)
  
  ### Return
  return(df0)
})(); rff_nat_pop |> glimpse()
# listData[["rff_nat_pop"]] <- rff_nat_pop
configList[["rff_nat_pop"]] <- rff_nat_pop



### Update coefficients
### Function of mortality as a function of population
calc_mortality <- function(
    df0, ### Tibble with population and years
    df1      = rff_nat_pop, ### Tibble with columns for mortality rate slope and mortality rate intercept
    pCol0    = "national_pop"      , ### Column with national population
    sCol0    = "rffMrate_slope"    , ### Column with mortality rate slope,
    iCol0    = "rffMrate_intercept", ### Column with mortality rate intercept,
    joinCols = c("year") ### Column to join df0 and df1
){
  ### Join df0 and df1
  # join0 <- c("year")
  join0 <- joinCols
  df0   <- df0 |> left_join(df1, by=join0)
  rm(df1)
  ### Calculate intermediate populations
  # df0   <- df0 |> mutate(delta_rffPop = national_pop - rffPop)
  # df0   <- df0 |> mutate(rffFactor    = delta_rffPop * rffMrate_slope + rffMrate_intercept)
  df0   <- df0 |> mutate(logPop       = (!!sym(pCol0)) |> log())
  df0   <- df0 |> mutate(rffFactor    = logPop       * !!sym(sCol0) + !!sym(iCol0))
  df0   <- df0 |> mutate(respMrate    = rffFactor    * ifRespScalar)
  ### Return data
  return(df0)
}
### Update in list
# listData$coefficients[["Mortality"]][["fun0"]] <- calc_mortality
configList$coefficients[["Mortality"]][["fun0"]] <- calc_mortality


#### Baseline Mortality Rate ----------------
listLoad$mort$data$mortBase  |> glimpse()
listLoad$mort$data$mortState |> glimpse()

baseline_mort <- listLoad$mort$data$mortState |> (function(
    df0,
    mrate = listLoad$mort$data$mortBase |> pull(MortalityIncidence)
){
  ### Rename values
  renameAt0 <- c("Column", "Year")
  renameTo0 <- c("fips", "year")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # df0 |> glimpse()
  ### Expand values to additional fips
  join0     <- c("fips", "year")
  newVals0  <- c(2, 15)
  fipsVals0 <- df0 |> pull(fips) |> unique() |> c(newVals0) |> sort()
  # yrVals0   <- 2000:2060
  dfYrs0    <- df0 |> select(year) |> unique() |> arrange_at(c("year"))
  # "gothere1" |> print(); df0 |> glimpse(); dfYrs0 |> glimpse()
  dfJoin0   <- co_states |> cross_join(dfYrs0) |> arrange_at(c(join0))
  df0       <- dfJoin0   |> left_join(df0, by=join0)
  rm(dfJoin0)
  # dfYrs0    <- expand_grid(fips = fipsVals0, year = yrVals0)
  # df0       <- dfYrs0 |> left_join(df0, by=join0)
  ### Mutate values
  df0       <- df0 |> mutate(
    StateMortRatio = case_when(
      fips %in% newVals0 ~ 1, 
      .default = StateMortRatio
    ) |> na.approx()
  ) ### End mutate
  ### Join with states info
  
  ### Extend values to additional years
  maxYr0    <- df0 |> pull(year) |> max()
  dfYrs0    <- tibble(year = (maxYr0 + 1):2100)
  # df1       <- expand_grid(fips = fipsVals0, year = 2060:2100)
  # df0       <- df0 |> 
  #   bind_rows(df1) |> 
  #   group_by_at(c("fips")) |> 
  #   fill(StateMortRatio) |> 
  #   ungroup()
  df1       <- df0 |> 
    filter(year %in% maxYr0) |> 
    select(-c("year")) |> 
    cross_join(dfYrs0)
  
  ### Arrange values
  df0       <- df0 |>
    bind_rows(df1) |> 
    arrange_at(c(join0))
    
  ### Calculate base mortality rate and join with states
  df0       <- df0 |>
    mutate(nat_respMrate  = mrate) |>
    mutate(base_respMrate = StateMortRatio * nat_respMrate)
    
  ### Return
  return(df0)
})(); baseline_mort |> glimpse()
# listData[["baseline_state_mort"]] <- baseline_mort
stateData[["baseline_state_mort"]] <- baseline_mort


### Morbidity ----------------
#### Age Range percentages ----------------
### For morbidity
### - Change column names
### - Standardize with states
### - Split the string for age range into new columns, convert to numeric
listLoad$pop$data$popAge |> glimpse()
listLoad$pop$data$popAge$Year |> range()
listLoad$pop$data$popAge$newAgeRange |> unique()
asthmaAgePcts <- listLoad$pop$data$popAge|> (function(
    df0
){
  ### Rename values
  renameAt0 <- c("Year", "State_FIPS", "Population", "Pct.State.Pop")
  renameTo0 <- c("year", "fips", "ageRefPop", "agePctStatePop")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  ### Join with states and years
  sort0     <- c("fips", "newAgeRange", "year")
  join0     <- "fips" |> c("year")
  yrs0      <- df0 |> pull(year) |> unique() |> sort()
  dfYrs0    <- tibble(year = yrs0)
  minYr0    <- yrs0 |> min()
  maxYr0    <- yrs0 |> max()
  newYrs0   <- minYr0:maxYr0
  dfJoin0   <- co_states |> cross_join(dfYrs0)
  df0       <- dfJoin0 |> 
    left_join(df0, by=join0) |> 
    arrange_at(c(sort0))
  # df0 |> glimpse()
  rm(dfJoin0)
  ### Group and interpolate
  sum0      <- c("ageRefPop", "agePctStatePop")
  names0    <- df0 |> names()
  group0    <- names0 |> get_matches(y=c(sum0, "year"), matches=F)
  df0       <- df0 |> group_by_at(c(group0))
  df0       <- df0 |> group_map(function(.x, .y){
    .x <- sum0 |> map(function(colX, dfX=.x){
      xIn  <- .x |> pull(year)
      yIn  <- .x |> pull(all_of(colX))
      out0 <- xIn |> approx(y=yIn, xout=newYrs0) |>
        bind_cols() |>
        rename_at(c("x", "y"), ~c("year", colX))
      return(out0)
    }) |> reduce(left_join, by="year")
    .x <- .x |> cross_join(.y)
    return(.x)
  }) |> bind_rows()
  ### Relocate and arrange
  df0       <- df0 |> 
    relocate(any_of(names0)) |> 
    arrange_at(c(sort0))
  ### Interpolate over years
  # "got here1" |> print()
  # ### Split age range into new columns
  # # c("0TO17", "18TO99", "0TO17") |> 
  # df1       <- df0 |> 
  #   pull(newAgeRange) |> 
  #   map(str_split, "TO") |> 
  #   map(unlist) |> map(as.list) |> 
  #   map(set_names, c("startAge", "endAge")) |> 
  #   map(as.data.frame) |>
  #   bind_rows()
  # # df1 |> glimpse()
  # ### Bind values with data
  # df0       <- df0 |> 
  #   bind_cols(df1) |> 
  #   mutate_at(c("startAge", "endAge"), as.numeric) |>
  #   relocate(c("startAge", "endAge"), .after=newAgeRange)
  # rm(df1)
  
  ### Return
  return(df0)
})(); asthmaAgePcts |> glimpse()
# listData[["asthmaAgeRangePcts"]] <- asthmaAgeRangePcts
stateData[["asthmaAgePcts"]] <- asthmaAgePcts
asthmaAgePcts$fips |> range()


#### Asthma Incidence/ER Visits ----------------
### Rename columns
### Standardize with states
### Drop multi-model mean
listLoad$asth$data$asthAsthma |> glimpse()
listLoad$asth$data$asthAsthma$State_FIPS |> unique()
listLoad$asth$data$asthAsthma$Endpoint |> unique()
listLoad$asth$data$asthAsthma$Model |> unique()
listLoad$asth$data$asthAsthma$ModelYear |> range()
listLoad$asth$data$asthAsthma$ModelYear |> unique()
# listLoad$asth$data$asthma  |> glimpse()

df_asthmaImpacts <- listLoad$asth$data$asthAsthma |> (function(
    df0, 
    df1 = asthmaAgePcts
){
  ### Rename values
  renameAt0 <- c("State_FIPS", "Endpoint", "Model", "ModelYear", "Start_Age", "End_Age", "State_Results")
  renameTo0 <- c("fips", "endpoint", "model", "year", "startAge", "endAge", "scaled_impacts")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # df0 |> glimpse()
  ### Expand values to additional fips
  sort0     <- c("endpoint", "startAge", "endAge", "fips", "model", "year")
  select0   <- df0 |> names() |> get_matches(y=c("fips", "scaled_impacts"), matches=F)
  # dfMods0   <- df0 |> select(model) |> unique()
  dfUnique0 <- df0 |> select(all_of(select0)) |> unique()
  # dfUnique0 |> glimpse()
  dfJoin0   <- co_states |> cross_join(dfUnique0)
  ### Join with years
  yrs0      <- df0 |> pull(year) |> unique() |> sort()
  dfYrs0    <- tibble(year = yrs0)
  minYr0    <- yrs0 |> min()
  maxYr0    <- yrs0 |> max()
  newYrs0   <- minYr0:maxYr0
  # dfJoin0   <- dfJoin0 |> cross_join(dfYrs0)
  # dfJoin0 |> glimpse()
  # join0     <- c("fips", "year")
  join0     <- select0 |> c("fips")
  df0       <- dfJoin0 |> 
    left_join(df0, by=join0) |> 
    relocate(any_of(sort0)) |>
    arrange_at(c(sort0))
  rm(dfUnique0, dfJoin0)
  
  ### Filter out model = "MMM" (multi-model mean)
  ### Join with model info
  # df0 |> glimpse()
  df0       <- df0 |> 
    filter_at(c("model"), function(x, y="MMM"){!x %in% y}) |>
    rename_at(c("model"), ~"model_str") |>
    left_join(co_models, by="model_str")
  
  ### Add sector and impact type
  df0       <- df0 |> 
    mutate(sector = "morbidity", .before="endpoint") |>
    mutate(impactType = case_when(
      endpoint |> str_detect("Emergency Room") ~ case_when(
        startAge == 0 ~ "asthmaER_child",
        .default = "asthmaER_adult"
      ),
      .default = "childAsthma"
    ), .after="sector")
  
  ### Group and interpolate
  sum0      <- c("scaled_impacts")
  names0    <- df0 |> names()
  group0    <- names0 |> get_matches(y=c(sum0, "year"), matches=F)
  df0       <- df0 |> group_by_at(c(group0))
  df0       <- df0 |> group_map(function(.x, .y){
    .x <- sum0 |> map(function(colX, dfX=.x){
      dfX  <- dfX |> filter_at(c(colX), function(x){!(x |> is.na())})
      doX  <- dfX |> nrow()
      if(doX) {
        xIn  <- .x |> pull(year)
        yIn  <- .x |> pull(all_of(colX))
        
        out0 <- xIn |> approx(y=yIn, xout=newYrs0) |> bind_cols()
      } else{
        out0 <- tibble(x = newYrs0, y = NA)
      } ### End if(doX)
      out0 <- out0 |> rename_at(c("x", "y"), ~c("year", colX))  
      return(out0)
    }) |> reduce(left_join, by="year")
    .x <- .x |> cross_join(.y)
    return(.x)
  }) |> bind_rows()
  ### Relocate and arrange
  df0       <- df0 |> 
    relocate(any_of(names0)) |> 
    arrange_at(c(sort0))
  
  ### Join with age ranges
  join0     <- df0 |> names() |> get_matches(df1 |> names())
  df0       <- df0 |> left_join(df1, by=join0, relationship="many-to-many")
  
  ### Return
  return(df0)
})(); df_asthmaImpacts |> glimpse()
# 61965*2
df_asthmaImpacts |> filter(model_label |> is.na()) |> glimpse()
# listData[["df_asthmaImpacts"]] <- df_asthmaImpacts
stateData[["df_asthmaImpacts"]] <- df_asthmaImpacts


### Ozone Response ----------------
### National O3 reshaping
listLoad$o3$data$o3Nat |> glimpse()
### Use units pptv to ppbv
nat_o3  <- listLoad$o3$data$o3Nat |> (function(
    df0, 
    df1    = co_models, 
    mRate0 = listLoad$mort$data$mortBase |> pull(MortalityIncidence),
    ch4_0  = listData$coefficients$CH4$base0, ### pptbv
    nox_0  = listData$coefficients$NOx$base0  ### Mt
    # ch4_0  = 100, ### pptbv
    # nox_0  = 10.528
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  renameTo0 <- c("model_str", "nat_o3response_pptv_per_ppbv", "base_nat_deltaO3_pptv")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Join values with models
  select0   <- c("model", "model_label", "model_str")
  join0     <- c("model_str")
  df0       <- df0 |> left_join(df1 |> select(all_of(select0)), by=join0)
  df0       <- df0 |> relocate(c("model", "model_label"), .before=c("model_str"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  ### Base CH4 and NOx concentrations
  df0       <- df0   |> mutate(base_CH4_ppbv = ch4_0)
  df0       <- df0   |> mutate(base_NOx_Mt   = nox_0)
  
  ### Base mortality incidence
  #browser()
  df0       <- df0   |> mutate(base_Nat_respMrate = mRate0)
  
  ### Select and arrange
  drop0     <- c("OzoneResponse.ppb.ppb.")
  arrange0  <- c("model")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); nat_o3 |> glimpse()
# listData[["nat_o3"]] <- nat_o3
configList[["nat_o3"]] <- nat_o3

### State Ozone Reshaping
### - Drop Ozone PPT/PPB
### - Rename State_FIPS to fips, Model to model_str, Ozone response to o3response_ppb2ppb, DeltaOzone to deltaO3
### - Standardize model by:
###   - Mutate co_models: add column "model_str" by using mutate(model_str = model_id |> str_match(pattern=o3State$Model |> unique() |> paste(collapse="|")) |> as.list() |> unlist())
###   - Joining with co_models by "model_str"
listLoad$o3$data$o3State |> glimpse()
state_o3    <- listLoad$o3$data$o3State |> (function(df0, df1=co_states, df2=nat_o3 ){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  renameTo0 <- c("fips", "model_str", "state_o3response_pptv_per_ppbv", "base_state_deltaO3_pptv")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Join states with values
  drop0     <- "us_area"
  join0     <- "fips"
  df1       <- df1 |> select(-any_of(drop0))
  df0       <- df1 |> left_join(df0, by=join0)
  # df0 |> glimpse()
  rm(drop0, join0)
  
  ### Join values with national values
  join0     <- c("model_str")
  move0     <- c("model", "model_label", "model_str")
  df0       <- df0 |> left_join(df2, by=join0)
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(join0)
  
  ### Select and arrange
  drop0     <- c("OzoneResponse.ppb.ppb.", "fips")
  arrange0  <- c("region", "state") |> c("model")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> arrange_at(c(arrange0))

  ### Return
  return(df0)
})(); state_o3 |> glimpse()
state_o3$model |> unique()
# listData[["state_o3"]] <- state_o3
stateData[["state_o3"]] <- state_o3




### State Excess Mortality ----------------
### State Excess Mortality reshaping
### Model	ModelYear	State_FIPS	State_Results |> rename to: c(model_str, refYear, fips, excess_mortality)
listLoad$mort$data$mortXm |> glimpse(); listLoad$mort$data$mortXm$ModelYear |> range()
state_xMort    <- listLoad$mort$data$mortXm |> (function(
    df0, 
    df1 = co_states, 
    df2 = co_models, 
    df3 = baseline_mort |> filter(year == "2020") |> select(-year,-base_respMrate)
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "State_Results", "ModelYear")
  renameTo0 <- c("fips", "model_str", "base_state_exMort0", "base_year")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  df0       <- df0 |> relocate(c("base_year"), .after=c("model_str"))
  rm(renameAt0, renameTo0)
  
  ### Join states with values
  drop0     <- "us_area"
  join0     <- "fips"
  join1     <- c("region","state","fips","postal")
  df1       <- df1 |> select(-any_of(drop0))
  df0       <- df1 |> left_join(df0, by=join0)
  df0       <- df0 |> left_join(df3, by=join1)|> select(-any_of(drop0))
  rm(drop0, join0, join1)
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  move0     <- c("model", "model_str")
  df2       <- df2 |> select(all_of(select0))
  df0       <- df0 |> left_join(df2, by=join0)
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  ### Adjust state Values
  df0       <- df0 |> mutate(base_state_exMort = base_state_exMort0 * StateMortRatio)
  ### Select and arrange
  drop0     <- c("model_str", "fips","base_state_exMort0","StateMortRatio")
  arrange0  <- c("region", "state") |> c("model")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); state_xMort |> glimpse()
# listData[["state_xMort"]] <- state_xMort
stateData[["state_xMort"]] <- state_xMort




### Calculate RR Scalar ----------------
base_state_pop |> glimpse(); state_o3 |> glimpse(); state_xMort |> glimpse(); 
state_rrScalar <- base_state_pop |> (function(
    df0, 
    o3_0 = state_o3, 
    xm_0 = state_xMort,
    base_mort = baseline_mort |> filter(year == "2020") |> select(-year,-base_respMrate) # Get Base Year baseline mortality adjustment
){
  ### Glimpse data
  # df0 |> glimpse()
  ### Join state O3 response and state excess mortality data
  drop0     <- c("model_str")
  join0     <- c("region", "state", "postal") |> c("model") 
  o3_0      <- o3_0 |> select(-any_of(drop0))
  o3_0      <- o3_0 |> left_join(xm_0, by=join0)
  rm(join0, xm_0)
  
  ### Join population data with O3 response and excess mortality data
  join0     <- c("region", "state", "postal") |> c("base_year")
  joinMort0 <- df0 |> names() |> get_matches(y=base_mort |> names())
  move0     <- c("model", "model_label")
  df0       <- df0 |> left_join(o3_0, by=join0, relationship="many-to-many")
  df0       <- df0 |> left_join(base_mort, by=joinMort0)
  df0       <- df0 |> relocate(any_of(move0), .after=c("postal"))
  rm(join0, move0, o3_0)
  
  ### Calculate rr Scalar
  df0       <- df0 |> mutate(state_rrScalar   = base_state_pop * (base_Nat_respMrate * StateMortRatio)  * base_state_deltaO3_pptv)
  df0       <- df0 |> mutate(state_mortScalar = base_state_exMort / state_rrScalar)
  
  ### Add sector and impact type
  df0       <- df0 |> mutate(sector = "mortality", impactType = "NA", .before="region")
  
  ### Return
  return(df0)
})(); state_rrScalar |> glimpse()
state_rrScalar$model |> unique()
# listData[["state_rrScalar"]] <- state_rrScalar
stateData[["state_rrScalar"]] <- state_rrScalar



## Default Scenarios ----------------
# listData <- listData |> (function(list0, names0=c("ch4_default", "nox_default", "o3_default")){list0[!((list0 |> names()) %in% names0)]})()
# scenariosList <- list()

### GDP ----------------
### Default GDP scenario = Default FrEDI scenario
scenariosList[["gdp_default"]] <- rDataList$scenarioData$gdp_default

### Population ----------------
### Default population scenario = make scenario from FrEDI data
rDataList$scenarioData$popData$region |> unique()
pop_default <- def_state_pop |> (function(df0){
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
    # minYr0 = listData$coefficients$minYear0,
    # maxYr0 = listData$coefficients$maxYear0
){
  ### Years 
  yrs0      <- minYr0:maxYr0
  df1       <- tibble(year=yrs0)
  ### Rename columns
  # df0       <- df0 |> rename("CH4_ppbv" =  ch4_ppb)
  renameAt0 <- c("ch4_ppb")
  renameTo0 <- c("CH4_ppbv")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  ### Filter to 2100
  join0     <- c("year")
  df0       <- df1 |> left_join(df0, by=join0)
  ### Return
  return(df0)
})(); ch4_default |> glimpse()
# listScenarios[["ch4_default"]] <- ch4_default
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
# listScenarios[["nox_default"]] <- nox_default
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
  # renameAt0 <- c(join0)  |> paste0("_label")
  # renameTo0 <- c(join0)
  renameTo0 <- c("region", "model")
  renameAt0 <- c(renameTo0)  |> paste0("_label")
  drop0     <- c(join0) |> c(renameTo0) |> c("us_area", "fips")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  df0       <- df0 |> relocate(c("model"), .before=c("year"))

  ### Calculate NOx ratio, then O3 response
  ### calc_NOx_factor
  # df0 |> glimpse()
  df0       <- df0 |> mutate(nox_factor0 = noxAdj0)
  df0       <- df0 |> mutate(nox_factor  = NOx_Mt |> fun0())
  df0       <- df0 |> mutate(nox_ratio   = nox_factor / nox_factor0)
  df0       <- df0 |> mutate(O3_pptv     = CH4_ppbv * nox_ratio * state_o3response_pptv_per_ppbv )
  
  ### Arrange
  arrange0  <- c("region", "state", "model", "year")
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); o3_default |> glimpse()
o3_default$region |> unique(); o3_default$model |> unique()
# listScenarios[["o3_default"]] <- o3_default
scenariosList[["o3_default"]] <- o3_default


## Save Data ----------------
### Update Data in List ----------------
# ghgData[["package"     ]] <- listData
# ghgData[["scenarioData"]] <- listScenarios
ghgData[["ghgData"     ]] <- configList
ghgData[["stateData"   ]] <- stateData
ghgData[["scenarioData"]] <- scenariosList
# ghgData[["scenarioData"]] <- listScenarios


### Update Data in List & Save List
# saveFile   <- projDir |> file.path("data", "ghgData.rda")
saveFile   <- oPath0 |> file.path("ghg", "ghgData") |> paste0(".", "rda")
save(ghgData, file=saveFile)
ghgData0 <- ghgData
# ghgData |> glimpse()
### Update System Data ----------------
rDataList  <- rDataList |> (function(list0, names0="ghgData"){list0[!((list0 |> names()) %in% names0)]})()
# rDataList[["ghgData"]] <- ghgData
save(rDataList, file=oPath0 |> file.path("sysdata.rda"))
rDataList |> names()
projDir |> devtools::load_all()
# tmpData     <- projDir |> update_sysdata(
#     dataPath  = oPath0,
#     mainFile  = "sysdata.rda",
#     sv        = TRUE ,
#     svPath    = oPath0 |> file.path("sv"),
#     ghgPath   = oPath0 |> file.path("ghg"),
#     svExt     = "rda",
#     save      = TRUE ,
#     return    = FALSE
#   ) ### End update_sysdata
projDir |> devtools::load_all()
oPath0 |> fun_saveSysData(
  # controlFile = "controlData",
  # scenarioDir = "scenarios",
  modules     = c("fredi", "ghg", "sv"),
  outFile     = "sysData",
  extStrs     = c("rda", "rds")
) ### End fun_saveSysData
# dPath0 |> list.files()

# rDataList  <- rDataList |> (function(list0, names0="ghgData"){list0[!((list0 |> names()) %in% names0)]})()
# saveFile   <- projDir |> file.path("data", "sysdata.rda")
# save(rDataList, ghgData, svDataList, svPopList, format_styles, file=saveFile)


## End File ----------------
