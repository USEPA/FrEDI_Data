###### Packages ######
require(tidyverse)
require(devtools)
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
popFiles  <- loadFiles |> (function(x, str0="Pop"   ){x[x |> str_detect(str0)]})(); popFiles
o3Files   <- loadFiles |> (function(x, str0="Ozone" ){x[x |> str_detect(str0)]})(); o3Files
mortFiles <- loadFiles |> (function(x, str0="Mort"  ){x[x |> str_detect(str0)]})(); mortFiles

loadTypes <- c("pop", "o3", "mort")
dfLoad    <- tibble(prefix0 = loadTypes) |> mutate(str0 = c("Pop", "Oz", "Mort"))

###### Load Lists ######
###### ** List of File Names ######
listLoad0  <- dfLoad |> pull(str0) |> map(function(str0, list0=loadFiles){
  list0 <- list0 |> (function(x, strx=str0){x[x |> str_detect(strx)]})(); 
  list0 |> print()
  return(list0)
}) |> set_names(dfLoad |> pull(prefix0))

listLoad1 <- listLoad0 |> 
  (function(list0){
    list(type0 = list0 |> names(), files0=list0)
  })() |>
  pmap(function(type0, files0){
    ### Conditionals
    doPop    <- "pop"  %in% type0
    doO3     <- "o3"   %in% type0
    doMort   <- "mort" %in% type0
    ### Initialize list
    type0 |> print(); files0 |> print()
    list0    <- list()
    if        (doPop ) {
      # list0[[type0]] <- files0
      base0     <- files0 |> (function(x, strx="State"){x[x |> str_detect(strx)]})()
      rff       <- files0 |> (function(x, strx="RFF"  ){x[x |> str_detect(strx)]})()
      hasBase0  <- base0  |> length()
      hasRff0   <- rff0   |> length()
      if(hasBase0) {list0[["base"]] <- base0}
      if(hasRff0 ) {list0[["rff" ]] <- rff0}
    } else if (doO3  ) {
      nat0      <- files0 |> (function(x, strx="Nation|Country"){x[x |> str_detect(strx)]})()
      state0    <- files0 |> (function(x, strx="State"         ){x[x |> str_detect(strx)]})()
      hasNat0   <- nat0   |> length()
      hasState0 <- state0 |> length()
      if(hasNat0   ) {list0[["nat"  ]] <- nat0}
      if(hasState0 ) {list0[["state"]] <- state0}
    } else if (doMort) {
      base0      <- files0  |> (function(x, strx="Base"  ){x[x |> str_detect(strx)]})()
      xm0        <- files0  |> (function(x, strx="Excess"){x[x |> str_detect(strx)]})() 
      scalar0    <- files0  |> (function(x, strx="Scalar"){x[x |> str_detect(strx)]})() 
      hasBase0   <- base0   |> length()
      hasXm0     <- xm0     |> length()
      hasScalar0 <- scalar0 |> length()
      if(hasBase0  ) {list0[["base"  ]] <- base0  }
      if(hasXm0    ) {list0[["xm"    ]] <- xm0    }
      if(hasScalar0) {list0[["scalar"]] <- scalar0}
    } ### End if (doPop )
    return(list0)
  }) |> set_names(dfLoad |> pull(prefix0))

listLoad  <- listLoad1 |> 
  (function(list0){
    list(type0 = list0 |> names(), lists0=list0)
  })() |> pmap(function(type0, lists0, path0=dataDir){
    ### Initialize list
    list0  <- list()
    ### Add list names
    # names0 <- lists0 |> names()
    dfNew0 <- tibble(type = type0, objName = lists0 |> names()) |> 
      mutate(dfName = (type==objName) |> ifelse(type, type |> paste0(objName |> str_to_title()))) |>
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

listLoad |> names()
# # listLoad$pop$data |> names()
# for(name_i in listLoad |> names()) { for(name_j in listLoad[[name_i]][["data"]] |> names()) {
#   name_j |> assign(listLoad[[name_i]][["data"]][[name_j]])
# }} ### End for loop


###### Initialize Lists to Save ######
# listSave    <- list()
listMethane <- list()
listData    <- list()

### add data to list
listMethane[["original"]] <- listLoad() 


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
  
  ###### ** Mortality ######
  ### Initialize list
  listM   <- list()
  ### Mortality coefficients (from RFF)... may be a time dependent function
  listM[["intercept0"]] <- 0
  listM[["slope0"    ]] <- 1
  ### Function of mortality as a function of population
  calc_mortality <- function(
    pop0, 
    slope0     = listM[["slope0"    ]], 
    intercept0 = listM[["intercept0"]]
  ){
    pop0 <- pop0 * slope0 + intercept0
    return(pop0)
  }; listM[["fun0"]] <- calc_mortality
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


###### Reshape ID/Crosswalk Tables ######
###### ** Sectors, Variants, Impact Types ######
### Get new sectors, variants, impact types, impact years
# rDataList$frediData$data$co_sectors |> glimpse()
# me_sectors   <- rDataList$frediData$data$co_sectors |> (function(df0){
#   ### Glimpse
#   # df0 |> glimpse()
#   
#   ### Filter to GCM values
#   df0       <- df0 |> filter("gcm" %in% modelType)
#   
#   ### Drop columns
#   drop0     <- c("model_id", "model_dot", "model_underscore")
#   df0       <- df0 |> select(-any_of(drop0))
#   drop0
#   
#   ### Mutate model ID and relocate
#   df0       <- df0 |> mutate(model = model_label |> str_replace_all("\\-", ""))
#   df0       <- df0 |> relocate(c("model", "model_label"))
#   
#   ### Return
#   return(df0)
# })(); co_sectors |> glimpse()
# listData[["co_sectors"]] <- co_sectors




###### ** Regions & States ######
### Get new regions
me_regions  <- rDataList$frediData$co_regions |> (function(df0){
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
})(); me_regions |> glimpse()
listData[["me_regions"]] <- me_regions

### Get new states
me_states   <- rDataList$frediData$co_states |> (function(df0){
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
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); me_states |> glimpse()
listData[["me_states"]] <- me_states

### Check that all states and regions are present in population data
listLoad$pop$data$pop |> pull(State_FIPS) |> unique() |> length()
((listLoad$pop$data$pop |> pull(State_FIPS) |> unique()) %in% (me_states$fips |> unique())) |> all()
((me_states$fips |> unique()) %in% (listLoad$pop$data$pop |> pull(State_FIPS) |> unique())) |> all()

# ### Reshape other population data
# df_ratios   <- rDataList$stateData$data$df_popRatios |> 
#   (function(df0){
#     ### Glimpse
#     df0 |> glimpse()
#     ### Rename values
#     renameAt0 <- c("Year", "State_FIPS", "Population")
#     renameTo0 <- c("year", "fips", "pop0")
#     df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
#   })(); df_ratios |> glimpse()
# 
# defPop      <- rDataList$stateData$data$pop_default |> filter(year <= 2100) 



###### ** Models ######
### Get new models and model types
me_models   <- rDataList$frediData$co_models |> (function(
    df0,
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
  select0   <- c("model", "modelType", "gcmMaxTemp")
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
})(); me_models |> glimpse()
listData[["me_models"]] <- me_models


###### Reshape Base Data ######
###### ** Base Population ######
### Reshape base population data (from BenMAP runs):
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario
listLoad$pop$data$base |> glimpse()
base_state_pop     <- listLoad$pop$data$base |> (function(df0, df1=me_states){
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
    arrange0  <- c("region", "state", "fips")
    df0       <- df0 |> arrange_at(c(arrange0))
    
    ### Return
    return(df0)
  })(); base_state_pop |> glimpse()
listData[["base_state_pop"]] <- base_state_pop
base_state_pop$base_year |> range(); base_state_pop |> pull(fips) |> unique() |> length()



###### ** RFF Population & Mortality ######
### Reshape population & mortality data (from RFF runs):
listLoad$pop$data$rff |> glimpse()
rff_nat_pop     <- listLoad$pop$data$rff |> (function(df0, df1=me_states){
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
  arrange0  <- c("region", "state", "fips")
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); rff_nat_pop |> glimpse()
listData[["rff_nat_pop"]] <- rff_nat_pop




###### ** Ozone Response ######
### National O3 reshaping
listLoad$o3$data$o3Nat |> glimpse()

### Use units pptv to ppbv
base_nat_o3  <- listLoad$o3$data$o3Nat |> (function(
    df0, 
    df1    = me_models, 
    mRate0 = listLoad$mort$data$mortBase |> pull(MortalityIncidence),
    ch4_0  = 100, ### pptv2ppbv
    nox_0  = 10.528
    
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  renameTo0 <- c("model_str", "base_nat_o3response", "base_nat_deltaO3")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  df0       <- df0 |> left_join(df1 |> select(all_of(select0)), by=join0)
  df0       <- df0 |> relocate(c("model"), .before=c("model_str"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  ### Base CH4 and NOx concentrations
  df0       <- df0   |> mutate(base_nat_CH4 = ch4_0)
  df0       <- df0   |> mutate(base_nat_NOX = nox_0)
  
  ### Base mortality incidence
  df0       <- df0   |> mutate(base_nat_rmRate = mRate0)
  
  ### Select and arrange
  drop0     <- c("OzoneResponse.ppb.ppb.")
  arrange0  <- c("model", "model_str")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); base_nat_o3 |> glimpse()
listData[["base_nat_o3"]] <- base_nat_o3

### State Ozone Reshaping
### - Drop Ozone PPT/PPB
### - Rename State_FIPS to fips, Model to model_str, Ozone response to o3response_ppb2ppb, DeltaOzone to deltaO3
### - Standardize model by:
###   - Mutate co_models: add column "model_str" by using mutate(model_str = model_id |> str_match(pattern=o3State$Model |> unique() |> paste(collapse="|")) |> as.list() |> unlist())
###   - Joining with co_models by "model_str"
listLoad$o3$data$o3State |> glimpse()

base_state_o3    <- listLoad$o3$data$o3State |> (function(df0, df1=me_states, df2=base_nat_o3){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "OzoneResponse.ppt.ppb.", "DeltaOzone")
  renameTo0 <- c("fips", "model_str", "base_state_o3response", "base_state_deltaO3")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Join states with values
  drop0     <- "us_area"
  join0     <- "fips"
  df1       <- df1 |> select(-any_of(drop0))
  df0       <- df1 |> left_join(df0, by=join0)
  rm(drop0, join0)
  
  ### Join values with national values
  join0     <- c("model_str")
  move0     <- c("model", "model_str")
  df0       <- df0 |> left_join(df2, by=join0)
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(join0)
  
  ### Select and arrange
  drop0     <- c("OzoneResponse.ppb.ppb.")
  arrange0  <- c("region", "state", "fips") |> c("model", "model_str")
  df0       <- df0 |> select(-any_of(drop0))
  df0       <- df0 |> arrange_at(c(arrange0))

  ### Return
  return(df0)
})(); base_state_o3 |> glimpse()
listData[["base_state_o3"]] <- base_state_o3




###### ** State Excess Mortality ######
### State Excess Mortality reshaping
### Model	ModelYear	State_FIPS	State_Results |> rename to: c(model_str, refYear, fips, excess_mortality)
listLoad$mort$data$mortXm |> glimpse(); listLoad$mort$data$mortXm$ModelYear |> range()
base_state_xMort    <- listLoad$mort$data$mortXm |> (function(df0, df1=me_states, df2=me_models){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "State_Results", "ModelYear")
  renameTo0 <- c("fips", "model_str", "base_exMortality", "base_year")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  df0       <- df0 |> relocate(c("base_year"), .after=c("model_str"))
  rm(renameAt0, renameTo0)
  
  ### Join states with values
  drop0     <- "us_area"
  join0     <- "fips"
  df1       <- df1 |> select(-any_of(drop0))
  df0       <- df1 |> left_join(df0, by=join0)
  rm(drop0, join0)
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  move0     <- c("model", "model_str")
  df2       <- df2 |> select(all_of(select0))
  df0       <- df0 |> left_join(df2, by=join0)
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  
  
  ### Select and arrange
  arrange0  <- c("region", "state", "fips") |> c("model", "model_str")
  # select0   <- arrange0 |> c("exMortality0")
  # df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); base_state_xMort |> glimpse()
listData[["base_state_xMort"]] <- base_state_xMort




###### ** IF Mortality Rate Scalar ######
listLoad$mort$data$mortScalar |> glimpse(); listLoad$mort$data$mortScalar$Years |> range()
nat_ifScalar <- listLoad$mort$data$mortScalar |> (function(df0){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  idCols0   <- c("Years", "Region")
  sumCols0  <- c("AllCauseDeathCounts", "RespDeathCounts", "Population", "AllCauseMortRate", "RespMortRate", "RespScalar")
  renameAt0 <- c(idCols0) |> c(sumCols0)
  renameTo0 <- c("year", "nation_fips") |> c("nat_IF_" |> paste0(sumCols0))
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # df0       <- df0 |> relocate(c("year"), .after=c("nation_fips"))
  rm(renameAt0, renameTo0)
  
  ### Return
  return(df0)
})(); nat_ifScalar |> glimpse()
listData[["nat_ifScalar"]] <- nat_ifScalar




###### Calculate RR Scalar ######
base_state_pop |> glimpse()
base_state_o3 |> glimpse()
base_state_xMort |> glimpse()
nat_ifScalar |> glimpse()
state_rrScalar <- base_state_pop |> (function(
    df0, 
    o3_0 = base_state_o3, 
    xm_0 = base_state_xMort,
    if_0 = nat_ifScalar
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  # renameAt0 <- c("Years", "Region", "RespScalar")
  # renameTo0 <- c("year", "nation_fips", "ifScalar")
  # df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # # df0       <- df0 |> relocate(c("year"), .after=c("nation_fips"))
  # rm(renameAt0, renameTo0)
  
  ### Join O3 response and excess mortality data
  join0     <- c("region", "state", "postal", "fips") |> c("model", "model_str")  
  o3_0      <- o3_0 |> left_join(xm_0, by=c(join0))
  rm(join0)
  
  ### Join population data with O3 response and excess mortality data
  join0     <- c("region", "state", "postal", "fips") |> c("base_year")
  move0     <- c("model", "model_str")
  df0       <- df0 |> left_join(o3_0, by=c(join0), relationship="many-to-many")
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  rm(join0, move0)
  
  ### Calculate rr Scalar
  df0       <- df0 |> mutate(base_state_rrScalar = base_state_pop * base_nat_rmRate * base_state_deltaO3)
  
  ### Return
  return(df0)
})(); state_rrScalar |> glimpse()
listData[["state_rrScalar"]] <- state_rrScalar








###### Update Data in List ######
# listMethane[["package"]] <- listData
# rDataList[["methane"]] <- listMethane
saveFile   <- projDir |> file.path("data", "methane.rda")
save(listMethane, file=saveFile)



###### Calculate refRespMortRate ######
### Test calculation of respiratory mortality rate
# respMortRate0 <- df_pop0 |> (function(
#     df0, 
#     df1    = respMortScalar, 
#     df2    = xMort0, 
#     df3    = o3State0, 
#     slope0 = listLoad$mort$data$mortBase
# ){
#   ### Glimpse data
#   # df0 |> glimpse(); df1 |> glimpse(); df2 |> glimpse()
#   
#   ### Get ref year
#   refYr0    <- df0 |> pull(year0) |> unique()
#   # refYr0 |> print()
#   
#   ### Filter data to ref year and rename column
#   renameAt0 <- c("year", "RespScalar")
#   renameTo0 <- c("year0", "ifScalar")
#   df1       <- df1 |> rename_at(c(renameAt0), ~renameTo0)
#   df1       <- df1 |> filter(year0 %in% refYr0)
#   rm(renameAt0, renameTo0)
#   
#   ### Join with IFF data
#   join0     <- c("year0")
#   df0       <- df0 |> left_join(df1, by=join0)
#   rm(join0)
#   
#   ### Join with excess mortality data
#   join0     <- c("region", "state", "fips") |> c("year0")
#   move0     <- c("nation_fips", "model", "model_str", "year0")
#   df0       <- df0 |> left_join(df2, by=join0)
#   # df0       <- df0 |> relocate(c("us_area"), .before=c("region"))
#   df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
#   rm(join0)
#   
#   ### Join with O3 response data
#   join0     <- c("us_area", "region", "state", "fips", "model", "model_str")
#   df0       <- df0 |> left_join(df3, by=join0)
#   df0       <- df0 |> relocate(c("us_area"), .before=c("region"))
#   rm(join0)
#   
#   ### Add slope
#   df0       <- df0 |> mutate(mortSlope0 = rMortRate0)
#   
#   ### Calculate respMortRate0 
#   df0       <- df0 |> mutate(respMortRate0 = (ref_pop - rffPop) * slope0 + RespDeathCounts)
#   df0       <- df0 |> mutate(respMortRate0 = ref_pop       - rffPop)
#   df0       <- df0 |> mutate(respMortRate0 = respMortRate0 * mortSlope0)
#   df0       <- df0 |> mutate(respMortRate0 = respMortRate0 + RespDeathCounts)
#   df0       <- df0 |> mutate(respMortRate0 = respMortRate0 * RespScalar)
#   
#   ### Calculate rrScalar0
#   df0       <- df0 |> mutate(rrScalar0  = ref_pop * respMortRate0 * delta_o3_state)
#   
#   ### Return
#   return(df0)
# })(); respMortRate0 |> glimpse()
# listData[["respMortRate0"]] <- respMortRate0











