###### Start ######
require(tidyverse)
require(devtools)
# require(FrEDI)

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
      list0[[type0]] <- files0
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
    list0[["data"]]  <- dfNew0 |> pull(fName) |> map(function(fName_i, fpath=path0){
      df_i <- fpath |> file.path(fName_i) |> read.csv()
      return(df_i)
    }) |> set_names(dfNew0 |> pull(dfName))
    
    ### Return
    return(list0)
  }) |> set_names(listLoad1 |> names())

listLoad |> names()
# listLoad$pop$data |> names()
for(name_i in listLoad |> names()) { for(name_j in listLoad[[name_i]][["data"]] |> names()) {
  name_j |> assign(listLoad[[name_i]][["data"]][[name_j]])
}} ### End for loop


######## Initialize List to Save ######
listSave    <- list()
listMethane <- list()
listData    <- list()

### add data to list
listMethane[["original"]] <- listLoad() 

######## Reshape States ########
### Get new states
me_states   <- rDataList$frediData$data$co_states |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Add US area and region
  ### Rename values
  df0       <- df0 |> mutate(us_area = "CONUS") |> relocate(c("us_area"))
  
  ### Add new values
  df1       <- tibble(
    state  = c("Alaska", "Hawaii"), 
    postal = c("AK", "HI"), 
    fips   = c(2, 15)
  ) |> ### End tibble
    mutate(us_area = state) |> 
    mutate(region  = state) |> 
    relocate(c("us_area", "region"))
  
  ### Bind values and arrange
  arrange0  <- c("us_area", "region", "state")
  df0       <- df0 |> rbind(df1)
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); me_states |> glimpse()
listData[["me_states"]] <- me_states

######## Reshape Regions ########
### Get new regions
me_regions  <- rDataList$frediData$data$co_regions |> (function(df0, df1 = me_states){
  ### Glimpse
  # df0 |> glimpse()
  select0   <- c("region")
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> mutate(region_id = region |> str_replace_all(" ", ""))
  df0       <- df0 |> mutate(us_area = "CONUS")
  rm(select0)
  
  ### Glimpse
  # df1 |> glimpse()
  select0   <- c("us_area", "region")
  df2       <- df1 |> filter(!(us_area %in% c("CONUS")))
  df2       <- df2 |> select(all_of(select0))
  df2       <- df2 |> mutate(region_id = region)
  rm(select0)
  
  ### Bind with co_regions
  ### Join regions with region info
  # df0 |> glimpse(); df2 |> glimpse()
  df0       <- df0 |> rbind(df2)
  
  # ### Arrange
  # arrange0  <- c("us_area", "region")
  # df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); me_regions |> glimpse()
listData[["me_regions"]] <- me_regions

######## Reshape Ratios ########
# ### Reshape other data
# df_ratios   <- rDataList$stateData$data$df_popRatios |> 
#   (function(df0){
#     ### Glimpse
#     df0 |> glimpse()
#     ### Rename values
#     renameAt0 <- c("Year", "State_FIPS", "Population")
#     renameTo0 <- c("year", "fips", "pop0")
#     df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
#   })(); df_ratios |> glimpse()

defPop      <- rDataList$stateData$data$pop_default |> filter(year <= 2100) 

######## Reshape Ref Pop ########
### Reshape pop dataframe:
### - Change names c("Year", "State_FIPS", "Population") to c("year", "fips", "pop")
### - Join with co_states by "fips":
###   - Note: may need to first edit co_states and co_regions to add in Alaska and HI...
###   - Refer to rDataList$scenarioData$popRatioData to get regions, states, and FIPS
### Check that pop dataframe has the same values as the FrEDI default scenario
df_pop0     <- listLoad$pop$data$pop |> (function(df0, df1=me_states){
    ### Rename values
    renameAt0 <- c("Year", "State_FIPS", "Population")
    renameTo0 <- c("year0", "fips", "ref_pop")
    df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
    
    ### Join with states
    join0     <- "fips"
    df0       <- df1 |> left_join(df0, by=join0)
    
    ### Select
    select0   <- c("region", "state", "fips", "year0", "ref_pop")
    df0       <- df0 |> select(all_of(select0))
    
    ### Return
    return(df0)
  })(); df_pop0 |> glimpse()
listData[["df_pop0"]] <- df_pop0

df_pop0$year |> range()

######## Reshape IDS ########
### Get new sectors, variants, impact types, impact years
rDataList$frediData$data$co_sectors |> glimpse()
me_sectors   <- rDataList$frediData$data$co_sectors |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Filter to GCM values
  df0       <- df0 |> filter("gcm" %in% modelType)
  
  ### Drop columns
  drop0     <- c("model_id", "model_dot", "model_underscore")
  df0       <- df0 |> select(-any_of(drop0))
  drop0
  
  ### Mutate model ID and relocate
  df0       <- df0 |> mutate(model = model_label |> str_replace_all("\\-", ""))
  df0       <- df0 |> relocate(c("model", "model_label"))
  
  ### Return
  return(df0)
})(); me_models |> glimpse()
listData[["me_models"]] <- me_models

######## Reshape Models ########
### Get new models and model types
me_models   <- rDataList$frediData$data$co_models |> (function(df0){
  ### Glimpse
  # df0 |> glimpse()
  
  ### Filter to GCM values
  df0       <- df0 |> filter("gcm" %in% modelType)
  
  ### Drop columns
  drop0     <- c("model_id", "model_dot", "model_underscore")
  df0       <- df0 |> select(-any_of(drop0))
  drop0
  
  ### Mutate model ID and relocate
  df0       <- df0 |> mutate(model = model_label |> str_replace_all("\\-", ""))
  df0       <- df0 |> relocate(c("model", "model_label"))
  
  ### Return
  return(df0)
})(); me_models |> glimpse()
listData[["me_models"]] <- me_models



######## Reshape Ozone Response ########
### National O3 reshaping
o3Nat |> glimpse()

o3Nat0  <- listLoad$o3$data$o3Nat |> (function(
    df0, 
    df1    = me_models, 
    slope0 = listLoad$mort$data$mortBase
){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("Model", "OzoneResponse.ppb.ppb.", "DeltaOzone")
  renameTo0 <- c("model_str", "o3response_ppb2ppb_nat", "deltaO3_nat")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Get string matches
  modStr0   <- df0 |> pull(model_str) |> unique() |> paste(collapse="|")
  # df1 |> pull(model) |> str_match(pattern=modStr0) |> as.list() |> unlist() |> print()
  df1       <- df1 |> mutate(model_str = model |> str_match(pattern=modStr0) |> as.list() |> unlist())
  df1       <- df1 |> mutate(model_str = (model_str |> is.na()) |> ifelse(model, model_str))
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  df0       <- df0 |> left_join(df1 |> select(all_of(select0)), by=join0)
  df0       <- df0 |> filter(!(model |> is.na()))
  df0       <- df0 |> relocate(c("model"), .before=c("model_str"))
  rm(select0, join0)
  
  ### Base mortality incidence
  slope0    <- slope0 |> pull(MortalityIncidence)
  df0       <- df0 |> mutate(baseMortInc0  = slope0)
  
  ### Select and arrange
  arrange0  <- c() |> c("model", "model_str")
  select0   <- arrange0 |> c("o3response_ppb2ppb_nat", "deltaO3_nat", "baseMortInc0")
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); o3Nat0 |> glimpse()
listData[["o3Nat0"]] <- o3Nat0

### State Ozone Reshaping
### - Drop Ozone PPT/PPB
### - Rename State_FIPS to fips, Model to model_str, Ozone response to o3response_ppb2ppb, DeltaOzone to deltaO3
### - Standardize model by:
###   - Mutate co_models: add column "model_str" by using mutate(model_str = model_id |> str_match(pattern=o3State$Model |> unique() |> paste(collapse="|")) |> as.list() |> unlist())
###   - Joining with co_models by "model_str"
o3State |> glimpse()

o3State$Model |> unique() |> paste(collapse="|")
o3State$Model[1] |> str_match(pattern=o3State$Model |> unique() |> paste(collapse="|")) |> as.list() |> unlist()
o3State0    <- listLoad$o3$data$o3State |> (function(df0, df1=me_models, df2=me_states, df3=o3Nat0){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "OzoneResponse.ppb.ppb.", "DeltaOzone")
  renameTo0 <- c("fips", "model_str", "o3response_ppb2ppb_state", "deltaO3_state")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Get string matches
  modStr0   <- df0 |> pull(model_str) |> unique() |> paste(collapse="|")
  # df1 |> pull(model) |> str_match(pattern=modStr0) |> as.list() |> unlist() |> print()
  df1       <- df1 |> mutate(model_str = model |> str_match(pattern=modStr0) |> as.list() |> unlist())
  df1       <- df1 |> mutate(model_str = (model_str |> is.na()) |> ifelse(model, model_str))
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  df0       <- df0 |> left_join(df1 |> select(all_of(select0)), by=join0)
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  ### Join states with values
  join0     <- "fips"
  df0       <- df2 |> left_join(df0, by=join0)
  rm(join0)
  
  ### Join values with national values
  join0     <- c("model", "model_str")
  df0       <- df0 |> left_join(df3, by=join0)
  rm(join0)
  
  ### Select and arrange
  arrange0  <- c("us_area", "region", "state", "fips") |> c("model", "model_str")
  select0   <- arrange0 |> c("o3response_ppb2ppb_nat", "o3response_ppb2ppb_state") |> 
    c("deltaO3_nat", "deltaO3_state") |> 
    c("baseMortInc0")
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> arrange_at(c(arrange0))

  ### Return
  return(df0)
})(); o3State0 |> glimpse()
listData[["o3State0"]] <- o3State0




######## Reshape State Excess Mortality ########
### State Excess Mortality reshaping
### Model	ModelYear	State_FIPS	State_Results |> rename to: c(model_str, refYear, fips, excess_mortality)
mortXm |> glimpse()
mortXm$ModelYear |> range()
xMort0    <- listLoad$mort$data$mortXm |> (function(df0, df1=me_models, df2=me_states){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("State_FIPS", "Model", "State_Results", "ModelYear")
  renameTo0 <- c("fips", "model_str", "excess_mortality", "year0")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  
  ### Get string matches
  modStr0   <- df0 |> pull(model_str) |> unique() |> paste(collapse="|")
  # df1 |> pull(model) |> str_match(pattern=modStr0) |> as.list() |> unlist() |> print()
  df1       <- df1 |> mutate(model_str = model |> str_match(pattern=modStr0) |> as.list() |> unlist())
  df1       <- df1 |> mutate(model_str = (model_str |> is.na()) |> ifelse(model, model_str))
  
  ### Join values with models
  select0   <- c("model", "model_str")
  join0     <- c("model_str")
  df0       <- df0 |> left_join(df1 |> select(all_of(select0)), by=join0)
  df0       <- df0 |> filter(!(model |> is.na()))
  rm(select0, join0)
  
  ### Join states with values
  join0     <- "fips"
  df0       <- df2 |> left_join(df0, by=join0)
  rm(join0)
  
  ### Select and arrange
  arrange0  <- c("us_area", "region", "state", "fips") |> c("model", "model_str", "year0")
  select0   <- arrange0 |> c("excess_mortality")
  df0       <- df0 |> select(all_of(select0))
  df0       <- df0 |> arrange_at(c(arrange0))
  
  ### Return
  return(df0)
})(); xMort0 |> glimpse()
listData[["xMort0"]] <- xMort0




######## Reshape IF Mortality Rate Scalar ########
mortScalar |> glimpse()
mortScalar$Years |> range()
respMortScalar <- listLoad$mort$data$mortScalar |> (function(df0){
  ### Glimpse data
  # df0 |> glimpse()
  
  ### Rename values
  renameAt0 <- c("Years", "Region", "Population")
  renameTo0 <- c("year", "nation_fips", "rffPop")
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  # df0       <- df0 |> relocate(c("year"), .after=c("nation_fips"))
  
  ### Rename column and keep old column
  df0       <- df0 |> mutate(rffMort = RespDeathCounts)
  rm(renameAt0, renameTo0)
  
  ### Return
  return(df0)
})(); respMortScalar |> glimpse()
listData[["respMortScalar"]] <- respMortScalar


######## Calculate refRespMortRate ########
respMortRate0 <- df_pop0 |> (function(
    df0, 
    df1    = respMortScalar, 
    df2    = xMort0, 
    df3    = o3State0, 
    slope0 = listLoad$mort$data$mortBase
){
  ### Glimpse data
  # df0 |> glimpse(); df1 |> glimpse(); df2 |> glimpse()
  
  ### Get ref year
  refYr0    <- df0 |> pull(year0) |> unique()
  # refYr0 |> print()
  
  ### Filter data to ref year and rename column
  renameAt0 <- c("year")
  renameTo0 <- c("year0")
  df1       <- df1 |> rename_at(c(renameAt0), ~renameTo0)
  df1       <- df1 |> filter(year0 %in% refYr0)
  rm(renameAt0, renameTo0)
  
  ### Join with IFF data
  join0     <- c("year0")
  df0       <- df0 |> left_join(df1, by=join0)
  rm(join0)
  
  ### Join with excess mortality data
  join0     <- c("region", "state", "fips") |> c("year0")
  move0     <- c("nation_fips", "model", "model_str", "year0")
  df0       <- df0 |> left_join(df2, by=join0)
  # df0       <- df0 |> relocate(c("us_area"), .before=c("region"))
  df0       <- df0 |> relocate(all_of(move0), .after=c("fips"))
  rm(join0)
  
  ### Join with O3 response data
  join0     <- c("us_area", "region", "state", "fips", "model", "model_str")
  df0       <- df0 |> left_join(df3, by=join0)
  df0       <- df0 |> relocate(c("us_area"), .before=c("region"))
  rm(join0)
  
  ### Add slope
  df0       <- df0 |> mutate(mortSlope0 = baseMortInc0)
  
  ### Calculate respMortRate0 
  df0       <- df0 |> mutate(respMortRate0 = (ref_pop - rffPop) * slope0 + RespDeathCounts)
  df0       <- df0 |> mutate(respMortRate0 = ref_pop       - rffPop)
  df0       <- df0 |> mutate(respMortRate0 = respMortRate0 * mortSlope0)
  df0       <- df0 |> mutate(respMortRate0 = respMortRate0 + RespDeathCounts)
  df0       <- df0 |> mutate(respMortRate0 = respMortRate0 * RespScalar)
  
  ### Calculate rrScalar0
  df0       <- df0 |> mutate(rrScalar0  = ref_pop * respMortRate0 * delta_o3_state)
  
  ### Return
  return(df0)
})(); respMortRate0 |> glimpse()
listData[["respMortRate0"]] <- respMortRate0



######## Coffiecients ########
### Lists of coefficients
list_coefficients <- list()
### Mortality coefficients
list_coefficients[["Mortality"]][["intercept0"]] <- 0
list_coefficients[["Mortality"]][["slope0"    ]] <- 1
### Function of NOX in Mt per year
list_coefficients[["Mortality"]][["fun0"]] <- function(
    mort0, 
    slope0     = 1, 
    intercept0 = 0
){
  nox0 <- nox0 |> log() 
  nox0 <- slope0 * nox0 + intercept0
  return(nox0)
}
### NOx
list_coefficients[["NOX"]][["slope0"    ]] <- -0.49
list_coefficients[["NOX"]][["intercept0"]] <- -1.12
list_coefficients[["NOX"]][["adj0"      ]] <- 1e3/556
### Function of NOX in Mt per year
list_coefficients[["NOX"]][["fun0"]] <- function(
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
listData[["coefficients"]] <- list_coefficients


######## Update Data in List ########
listMethane[["package"]] <- listData




