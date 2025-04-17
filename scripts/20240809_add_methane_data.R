###### Packages ######
require(tidyverse)
require(devtools)
require(zoo)
# require(FrEDI)

###### File Paths ######
### Project directory
projDir   <- "."
"." |> load_all()
### Data dir
### Get file paths: 9 files, including county results
dataDir   <- projDir   |> file.path("inst", "extdata", "methane")
dataPaths <- dataDir   |> list.files(full.names = T, pattern = "\\.csv")
dataFiles <- dataPaths |> basename(); dataFiles

### Exclude county results: 6 files
loadFiles <- dataFiles |> (function(x, str0="County"){x[!(x |> str_detect(str0))]})(); loadFiles


### Examine data
### Separate files
popFiles  <- loadFiles |> (function(x, str0="pop"  ){x[x |> tolower() |> str_detect(str0)]})(); popFiles
o3Files   <- loadFiles |> (function(x, str0="ozone"){x[x |> tolower() |> str_detect(str0)]})(); o3Files
mortFiles <- loadFiles |> (function(x, str0="mort" ){x[x |> tolower() |> str_detect(str0)]})(); mortFiles
ch4Files  <- loadFiles |> (function(x, str0="ch4"  ){x[x |> tolower() |> str_detect(str0)]})(); ch4Files

loadTypes <- c("pop", "o3", "mort","ch4")
dfLoad    <- tibble(prefix0 = loadTypes) |> mutate(str0 = c("pop", "oz", "mort","ch4"))

###### Load Lists ######
###### ** List of File Names ######
listLoad0  <- dfLoad |> pull(str0) |> map(function(str0, list0=loadFiles){
  list0 <- list0 |> (function(x, strx=str0){x[x |> tolower() |> str_detect(strx)]})(); 
  list0 |> print()
  return(list0)
}) |> set_names(dfLoad |> pull(prefix0))
# listLoad0$pop

###### ** Order File Names ######
listLoad1 <- listLoad0 |> 
  (function(list0){
    list(type0 = list0 |> names(), files0=list0)
  })() |>
  pmap(function(type0, files0){
    ### Conditionals
    doPop    <- "pop"  %in% type0
    doO3     <- "o3"   %in% type0
    doMort   <- "mort" %in% type0
    doch4    <- "ch4"  %in% type0
    ### Initialize list
    type0 |> print(); files0 |> print()
    list0    <- list()
    if        (doPop ) {
      # list0[[type0]] <- files0
      base0     <- files0 |> (function(x, strx="State"){x[x |> str_detect(strx)]})()
      rff0      <- files0 |> (function(x, strx="RFF"  ){x[x |> str_detect(strx)]})()
      all0      <- files0 |> (function(x, strx="all"  ){x[x |> str_detect(strx)]})()
      hasBase0  <- base0  |> length()
      hasRff0   <- rff0   |> length()
      hasAll0   <- all0   |> length()
      if(hasBase0) {list0[["base"]] <- base0}
      if(hasRff0 ) {list0[["rff" ]] <- rff0}
      if(hasAll0 ) {list0[["all" ]] <- all0}
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
      hasBase0   <- base0   |> length()
      hasAdj0    <- stateAdj0 |> length()
      hasXm0     <- xm0     |> length()
      hasScalar0 <- scalar0 |> length()
      if(hasBase0  ) {list0[["base"  ]] <- base0  }
      if(hasAdj0  ) {list0[["state"  ]] <- stateAdj0  }
      if(hasXm0    ) {list0[["xm"    ]] <- xm0    }
      if(hasScalar0) {list0[["scalar"]] <- scalar0}
    } else if (doch4) { 
      ssp2450      <- files0  |> (function(x, strx="ssp245"  ){x[x |> str_detect(strx)]})()
      hasssp2450   <- ssp2450   |> length()
      if(hasssp2450  ) {list0[["ssp245"  ]] <- ssp2450  }
      }### End if (doPop )
    return(list0)
  }) |> set_names(dfLoad |> pull(prefix0))
# listLoad1$pop |> names()

###### ** Load Files ######
listLoad  <- listLoad1 |> 
  (function(list0){
    list(type0 = list0 |> names(), lists0=list0)
  })() |> pmap(function(type0, lists0, path0=dataDir){
    ### Initialize list
    list0  <- list()
    ### Add list names
    # names0 <- lists0 |> names()
    dfNew0 <- tibble(type = type0, objName = lists0 |> names()) |> 
      # mutate(dfName = (type==objName) |> ifelse(type, type |> paste0(objName |> str_to_title()))) |>
      mutate(dfName = type |> paste0(objName |> str_to_title())) |>
      mutate(fName  = lists0 |> unlist())
    list0[["info"]] <- dfNew0
    
    ### Read in files
    list0[["data"]] <- dfNew0 |> pull(fName) |> map(function(fName_i, fpath=path0){
      ### Read in data
      df_i <- fpath |> file.path(fName_i) |> read.csv()
      ### Adjust FIPS data
      doFips <- "State_FIPS" %in% (df_i |> names())
      if(doFips) df_i <- df_i |> filter(!(State_FIPS |> is.na()))
      ### Return
      return(df_i)
    }) |> set_names(dfNew0 |> pull(dfName))
    
    ### Return
    return(list0)
  }) |> set_names(listLoad1 |> names())

listLoad$pop$data$popAll |> group_by(state, year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()
# listLoad$pop$data |> names()
# for(name_i in listLoad |> names()) { for(name_j in listLoad[[name_i]][["data"]] |> names()) {
#   name_j |> assign(listLoad[[name_i]][["data"]][[name_j]])
# }} ### End for loop


###### Initialize Lists ######
# listSave    <- list()
listMethane <- list()
listData    <- list()

### add data to list
listMethane[["original"]] <- listLoad
listData[["coefficients"]] <- list()


###### Reshape ID/Crosswalk Tables ######
###### ** Regions & States ######
### Get new regions
rDataList$frediData$co_regions |> glimpse()
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
listData[["co_regions"]] <- co_regions

### Get new states
rDataList$frediData$co_states |> glimpse()
co_states   <- rDataList$frediData$co_states |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
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
listData[["co_states"]] <- co_states

### Check that all states and regions are present in population data
listLoad$pop$data$popBase |> pull(State_FIPS) |> unique() |> length()
((listLoad$pop$data$popBase |> pull(State_FIPS) |> unique()) %in% (co_states$fips |> unique())) |> all()
((co_states$fips |> unique()) %in% (listLoad$pop$data$popBase |> pull(State_FIPS) |> unique())) |> all()




###### ** Model Types and Models ######
### Model Types
rDataList$frediData$co_modelTypes |> glimpse()
co_modelTypes   <- rDataList$frediData$co_modelTypes |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Create new values to bind
  df1       <- tibble(modelType_id = "gcm") |> 
    mutate(inputName       = "methane") |> 
    mutate(modelType_label = "GCM") |> 
    mutate(modelUnitDesc   = "Ozone Response") |> 
    mutate(modelUnit_id    = "pptv/pptb") |> 
    mutate(modelUnit_label = "pptv/pptb") |> 
    mutate(modelType_id    = "gcm")
  
  ### Filter to no values
  ### Bind new values to other values
  select0   <- df1 |> names()
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> filter(inputName %in% "GCM")
  df0       <- df0 |> rbind(df1)
  
  ### Return
  return(df0)
})(); co_modelTypes |> glimpse()
listData[["co_modelTypes"]] <- co_modelTypes

### Get new models and model types
rDataList$frediData$co_models |> glimpse()
co_models   <- rDataList$frediData$co_models |> (function(
    df0,
    df1,
    mod_str0 = listLoad$o3$data$o3Nat$Model |> unique() |> paste(collapse="|")
){
  ### Glimpse
  # df0 |> glimpse()
  
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
listData[["co_models"]] <- co_models




###### ** Input Info ######
### Input Info
rDataList$frediData$co_inputInfo |> glimpse()
co_inputInfo   <- rDataList$frediData$co_inputInfo |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()

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
listData[["co_inputInfo"]] <- co_inputInfo



###### ** Sectors, Variants, Impact Types ######
# ### Get new sectors, variants, impact types, impact years
# rDataList$frediData$co_sectors |> glimpse()
# co_sectors   <- rDataList$frediData$co_sectors |> (function(df0){
#   ### Glimpse
#   # df0 |> glimpse()
#   
#   ### Create new values to bind
#   df1       <- tibble(sector_id = "Methane") |> 
#     mutate(sector_label = sector_id) |>
#     mutate(modelType    = "gcm")
#   
#   ### Filter to no values
#   ### Bind new values to other values
#   df0       <- df0 |> filter(modelType %in% "GCM")
#   df0       <- df0 |> rbind(df1)
#   
#   ### Return
#   return(df0)
# })(); co_sectors |> glimpse()
# listData[["co_sectors"]] <- co_sectors
# 
# # co_sectors   <- rDataList$frediData$co_sectorsInfo |> (function(df0){
# #   ### Glimpse
# #   # df0 |> glimpse()
# #   
# #   ### Create new values to bind
# #   df1       <- tibble(sector = "Methane") |> 
# #     mutate(variant      = "NA") |>
# #     mutate(impactType   = "NA") |>
# #     mutate(impactYear   = "NA") |>
# #     mutate(modelTyp     = "gcm") |>
# #     mutate(sector_label = "NA")
# #   
# #   ### Filter to no values
# #   ### Bind new values to other values
# #   df0       <- df0 |> filter(modelType %in% "GCM")
# #   df0       <- df0 |> rbind(df1)
# #   
# #   ### Return
# #   return(df0)
# # })(); co_sectors |> glimpse()
# # listData[["co_sectors"]] <- co_sectors
# 
# ### Variants
# rDataList$frediData$co_variants |> glimpse()
# co_variants   <- rDataList$frediData$co_variants |> (function(df0){
#   ### Glimpse
#   # df0 |> glimpse()
#   
#   ### Create new values to bind
#   df1       <- tibble(sector_id = "Methane") |> 
#     mutate(variant_label = sector_id) |>
#     mutate(variant_id    = "Methane") |> 
#     # mutate(sectorprimary = 1) |> 
#     # mutate(includeaggregate = 1) |> 
#     mutate(damageAdjName = "none")
#   
#   ### Filter to no values
#   ### Bind new values to other values
#   df0       <- df0 |> filter(sector_id %in% "GCM")
#   df0       <- df0 |> rbind(df1)
#   
#   ### Return
#   return(df0)
# })(); co_variants |> glimpse()
# listData[["co_variants"]] <- co_variants
# 
# 
# ### Impact Types
# rDataList$frediData$co_impactTypes |> glimpse()
# co_impactTypes   <- rDataList$frediData$co_impactTypes |> (function(df0){
#   ### Glimpse
#   # df0 |> glimpse()
#   
#   ### Create new values to bind
#   df1       <- tibble(sector_id = "Methane") |> 
#     mutate(impactType_label = "Excess Mortality") |>
#     mutate(impactType_id    = "xMort") |> 
#     mutate(impactType_description = "Excess Mortality") |>
#     mutate(physicalmeasure = "Excess Mortality") |>
#     mutate(physScalarName  = "none") |>
#     mutate(physAdjName     = "none")
#   
#   ### Get Valuation info and bind to df1
#   join0      <- c("sector_id")
#   select0    <- join0 |> c("econScalarName", "econMultiplierName", "c0", "c1", "exp0", "year0")
#   df2        <- df0 |> 
#     mutate(sector_id = "Methane") |>
#     mutate(year0     = "2020") |>
#     filter(econScalarName      == "vsl_usd"   ) |>
#     filter(econMultiplierName  == "gdp_percap") |>
#     select(all_of(select0)) |>
#     distinct()
#   df1        <- df1 |> left_join(df2, by=c(join0))
#   rm(select0, join0)
#   
#   
#   ### Filter to no values
#   ### Bind new values to other values
#   df0       <- df0 |> filter(sector_id %in% "GCM")
#   df0       <- df0 |> rbind(df1)
#   
#   ### Return
#   return(df0)
# })(); co_impactTypes |> glimpse()
# listData[["co_impactTypes"]] <- co_impactTypes
# 
# 
# ### Impact Types
# rDataList$frediData$co_impactYears |> glimpse()
# co_impactYears   <- rDataList$frediData$co_impactYears |> (function(df0){
#   ### Glimpse
#   # df0 |> glimpse()
#   
#   ### Create new values to bind
#   df1       <- tibble(sector_id = "Methane") |> 
#     mutate(impactYear_id    = "NA") |> 
#     mutate(impactYear_label = "N/A")
#   
#   ### Filter to no values
#   ### Bind new values to other values
#   df0       <- df0 |> filter(sector_id %in% "GCM")
#   df0       <- df0 |> rbind(df1)
#   
#   ### Return
#   return(df0)
# })(); co_impactYears |> glimpse()
# listData[["co_impactYears"]] <- co_impactYears
# 
# 
# 
# ###### ** Sectors Info
# ### Get new regions
# rDataList$frediData$co_sectorsInfo |> glimpse()




###### Coffiecients ######
### Lists of coefficients
list_coefficients <- list() |> (function(
    list0
){
  ###### Initial List ######
  list0   <- list()
  
  ###### ** Other ######
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
    
    # ### Reshape
    # drop0 <- c("gdp_usd", "national_pop")
    # df0   <- df0 |> select(-any_of(drop0))
    # df0   <- df0 |> pivot_longer(c("gdp_percap"), names_to="econAdjName", values_to="econAdjValue0")
    
    ### Return
    return(df0)
  })()
  ### Update in list
  list0 <- list0 |> c(listOth)
  
  ###### ** Mortality ######
  ### Initialize list
  listM   <- list()
  # ### Mortality coefficients (from RFF)... may be a time dependent function
  # listM[["intercept0"]] <- 0
  # listM[["slope0"    ]] <- 1
  # ### Function of mortality as a function of population
  # calc_mortality <- function(
    #   pop0, ### Column with population values
  #   slope0     = listM[["slope0"    ]], 
  #   intercept0 = listM[["intercept0"]]
  # ){
  #   pop0 <- pop0 * slope0 + intercept0
  #   return(pop0)
  # }; listM[["fun0"]] <- calc_mortality
  # ### Function of mortality as a function of population
  # calc_mortality <- function(
  #   df0, ### Tibble with population and years
  #   df1, ### Tibble with columns for mortality rate slope and mortality rate intercept
  #   # sCol0    = "rff_mrate_slope"    , ### Column with mortality rate slope, 
  #   # iCol0    = "rff_mrate_intercept", ### Column with mortality rate intercept, 
  #   joinCols = c("year") ### Column to join df0 and df1
  # ){
  #   ### Join df0 and df1
  #   # join0 <- c("year")
  #   join0 <- joinCols
  #   df0   <- df0 |> left_join(df1, by=join0)
  #   rm(df1)
  #   ### Calculate intermediate populations
  #   df0   <- df0 |> mutate(delta_rff_pop = national_pop - rff_pop)
  #   df0   <- df0 |> mutate(rff_factor    = delta_rff_pop * rff_mrate_slope + rff_mrate_intercept)
  #   df0   <- df0 |> mutate(resp_mrate    = rff_factor    * nat_ifRespScalar)
  #   ### Return data
  #   return(df0)
  # }; 
  listM[["fun0"]] <- NULL
  ### Update in list
  list0[["Mortality"]] <- listM
  
  ###### ** Methane ######
  ### CH4, in pptv 
  ### Initialize list
  listCH4   <- list()
  ### Add coefficients
  listCH4[["base0"]] <- 100
  ### Update in list
  list0[["CH4"]] <- listCH4
  
  ###### ** NOx ######
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
  
  ###### Return ######
  return(list0)
})()
### Update list
listData[["coefficients"]] <- list_coefficients






###### Reshape Base Data ######
###### ** Base Population ######
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
listData[["base_state_pop"]] <- base_state_pop
base_state_pop$base_year |> range(); base_state_pop |> pull(postal) |> unique() |> length()



###### ** IF Mortality Rate Scalar ######
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
listData[["nat_ifScalar"]] <- nat_ifScalar



###### ** Default Population ######
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
listData[["def_state_pop"]] <- def_state_pop
def_state_pop$year |> range(); def_state_pop |> pull(postal) |> unique() |> length()
# def_state_pop$region |> unique()



###### ** RFF Population & Mortality ######
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
listData[["rff_nat_pop"]] <- rff_nat_pop



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
listData$coefficients[["Mortality"]][["fun0"]] <- calc_mortality


###### ** Baseline Mortality Rate #####
listLoad$mort$data$mortBase |> glimpse()
listLoad$mort$data$mortState |> glimpse()

baseline_mort <- listLoad$mort$data$mortState |> (function(
    statemortDf,
    mrate = listLoad$mort$data$mortBase |> pull(MortalityIncidence)
){
  renameAt0 <- c("Column", "Year")
  renameTo0 <- c("fips", "year")
  
  df0       <- expand_grid(
    Column = statemortDf |> pull(Column) |> unique() |> c(2,15),
    Year = seq(2000, 2060, by = 1)
  ) |>
    left_join(statemortDf) |>
    group_by(Column) |> 
    mutate(
      StateMortRatio = case_when( Column %in% c(2,15) ~ 1, .default = StateMortRatio),
      StateMortRatio =  na.approx(StateMortRatio)) |>
    bind_rows(expand_grid(
      Column = statemortDf |> pull(Column) |> unique() |> c(2,15),
      Year = seq(2061, 2100, by = 1)
    )) |>
    group_by(Column) |>
    fill(StateMortRatio) |>
    mutate(base_respMrate = StateMortRatio * mrate) |>
    rename_at(c(renameAt0), ~renameTo0)
    
  df1 <- df0 |>
        full_join(co_states) 
  
  return(df1)
  
})(); baseline_mort |> glimpse()
listData[["baseline_state_mort"]] <- baseline_mort

###### ** Ozone Response ######
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
listData[["nat_o3"]] <- nat_o3

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
listData[["state_o3"]] <- state_o3




###### ** State Excess Mortality ######
### State Excess Mortality reshaping
### Model	ModelYear	State_FIPS	State_Results |> rename to: c(model_str, refYear, fips, excess_mortality)
listLoad$mort$data$mortXm |> glimpse(); listLoad$mort$data$mortXm$ModelYear |> range()
state_xMort    <- listLoad$mort$data$mortXm |> (function(df0, df1=co_states, 
                                                         df2=co_models, 
                                                         df3 = baseline_mort |> filter(year == "2020") |> select(-year,-base_respMrate)){
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
listData[["state_xMort"]] <- state_xMort




###### ** Calculate RR Scalar ######
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
  o3_0      <- o3_0 |> left_join(xm_0, by=c(join0))
  rm(join0, xm_0)
  
  ### Join population data with O3 response and excess mortality data
  join0     <- c("region", "state", "postal") |> c("base_year")
  move0     <- c("model", "model_label")
  df0       <- df0 |> left_join(o3_0, by=c(join0), relationship="many-to-many")
  df0       <- df0 |> left_join(base_mort)
  df0       <- df0 |> relocate(all_of(move0), .after=c("postal"))
  rm(join0, move0, o3_0)
  
  ### Calculate rr Scalar
  df0       <- df0 |> mutate(state_rrScalar   = base_state_pop * (base_Nat_respMrate * StateMortRatio)  * base_state_deltaO3_pptv)
  df0       <- df0 |> mutate(state_mortScalar = base_state_exMort / state_rrScalar)
  
  ### Return
  return(df0)
})(); state_rrScalar |> glimpse()
state_rrScalar$model |> unique()
listData[["state_rrScalar"]] <- state_rrScalar




###### Default Scenarios ######
# listData <- listData |> (function(list0, names0=c("ch4_default", "nox_default", "o3_default")){list0[!((list0 |> names()) %in% names0)]})()
listScenarios <- list()

###### ** GDP ######
### Default GDP scenario = Default FrEDI scenario
listScenarios[["gdp_default"]] <- rDataList$scenarioData$gdp_default

###### ** Population ######
### Default population scenario = make scenario from FrEDI data
rDataList$scenarioData$popData$region |> unique()
pop_default <- def_state_pop |> (function(df0){
  drop0 <- c("fips", "area", "us_area")
  df0   <- df0 |> select(-any_of(drop0))
  df0   <- df0 |> filter(year >= 2020)
})(); pop_default |> glimpse()
listScenarios[["pop_default"]] <- pop_default

###### ** Methane ######
### Default methane scenario
listLoad$ch4$data$ch4Ssp245  |> glimpse()
ch4_default <- listLoad$ch4$data$ch4Ssp245 |> (function(
    df0,
    minYr0 = listData$coefficients$minYear0,
    maxYr0 = listData$coefficients$maxYear0
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
listScenarios[["ch4_default"]] <- ch4_default

###### ** NOx ######
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
listScenarios[["nox_default"]] <- nox_default

###### ** O3 ######
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
listScenarios[["o3_default"]] <- o3_default



###### Update Data in List & Save List ######
listMethane[["package"     ]] <- listData
listMethane[["scenarioData"]] <- listScenarios
rDataList  <- rDataList |> (function(list0, names0="listMethane"){list0[!((list0 |> names()) %in% names0)]})()


###### Update Data in List & Save List ######
# saveFile   <- projDir |> file.path("data", "listMethane.rda")
saveFile   <- projDir |> file.path("data", "methane", "listMethane.rda")
save(listMethane, file=saveFile)

# saveFile   <- projDir |> file.path("data", "sysdata.rda")
# save(rDataList, listMethane, svDataList, svPopList, format_styles, file=saveFile)


###### End File ######
