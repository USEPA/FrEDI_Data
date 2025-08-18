reshapeConfigData <- function(
    dataList = NULL,   ### List of data (e.g., as returned from FrEDI_Data::loadData())
    silent   = TRUE,   ### Level of messaging
    msg0     = "\t"    ### Prefix for messaging
) {
  ###### Messaging ######
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "Running reshapeConfigData...") |> message()
  if (!silent) paste0(msg1, "Reshaping data from FrEDI config file...") |> message()
  
  ###### Assign Objects ######
  # dataList0 <- dataList
  ### Assign tables in dataList to object in local environment
  listNames     <- dataList |> names()
  # listNames |> print()
  for(name_i in listNames) {name_i |> assign(dataList[[name_i]]); rm(name_i)}
  # dataList |> list2env(envir = environment())
  
 

  ###### Columns  ######
  ### Region and state level columns to use
  stateCols0    <- c("state", "postal")
  
  
  ###### Modify Tables and Update in List ######
  ###### ** 1. Sectors     ######
  ### Filter to those tables to include
  ### Make a copy of the sectors list to include variants
  drop0         <- c("include", "variants", "impactYears", "impactTypes")
  co_sectorsRef <- co_sectors
  co_sectors    <- co_sectors |> select(-all_of(drop0))
  ### Update values in list, drop intermediate variables
  dataList[["co_sectorsRef"]] <- co_sectorsRef
  dataList[["co_sectors"   ]] <- co_sectors
  rm(drop0)
  
  
  ###### ** 2. Misc ######
  ### No changes to variants
  ### No changes to model types
  ### No changes to input scenario info
  
  
  ###### ** 3. Variants ######
  # ### Change NA to "NA"
  # mutate0     <- c(variant)
  # co_variants <- co_variants |> mutate_at(c(mutate0), replace_na, "NA")
  # ### Update in list, drop intermediate values
  # dataList[["co_variants"]] <- co_variants
  
  
  ###### ** 3. Impact Types Info ######
  ### Drop damage adjustment names (present in variants)
  #drop0          <- c("damageAdjName")
  #co_impactTypes <- co_impactTypes |> select(-all_of(drop0)) # ; co_impactTypes |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_impactTypes"]] <- co_impactTypes
  #rm(drop0)
  
  
  
  ###### ** 4. Impact Years ######
  ### Load data and gather the data by impact year levels
  idCols0     <- c("sector_id")
  impYearLvls <- co_impactYears |> select(-all_of(idCols0)) |> names()
  co_impactYears <- co_impactYears |> (function(df0, mutate0=impYearLvls, cols0=idCols0){
    ### Convert to character
    df0     <- df0 |> mutate_at(c(mutate0), as.character)
    rm(mutate0)
    
    ### Reshape impact years: gather the data by impact year levels
    df0     <- df0 |> pivot_longer(
      cols      = -all_of(cols0),
      names_to  = "impactYear_id", 
      values_to = "impactYear"
    ) ### End pivot_longer
    
    ### Mutate values
    mutate0 <- c("impactYear_id") 
    df0     <- df0 |> mutate_at(c(mutate0), na_if     , "X3")
    df0     <- df0 |> mutate_at(c(mutate0), replace_na, "NA")
    
    ### Drop values with missing `impactYear` and then drop column
    drop0 <- "impactYear"
    df0   <- df0 |> filter(!(impactYear |> is.na()))
    df0   <- df0 |> select(-all_of(drop0))
    
    ### Get impact year label
    df0   <- df0 |> mutate(impactYear_label = impactYear_id |> str_replace("NA", "N/A"))
    
    ### Return
    return(df0)
  })() 
  
  
  ### Create info on impact year levels
  select0 <- c("impactYear_id", "impactYear_label")
  co_impYearLvls <- co_impactYears |> (function(df0, cols0=select0){
    df0 <- df0 |> select(all_of(cols0))
    df0 <- df0 |> unique()
    return(df0)
  })() 
  
  ### Update/add tables in/to list, drop intermediate values
  dataList[["co_impactYears"     ]] <- co_impactYears
  dataList[["co_impYearLvls"]] <- co_impYearLvls
  rm(idCols0, impYearLvls)
  
  

  ###### ** 5. Models & Model Types ######
  ### Combine with model types and update data list
  join0          <- c("modelType")
  co_models      <- co_models |> left_join(co_modelTypes, by = join_by(modelType == modelType_id)) 
  # co_models |> glimpse
  ### Update in list, drop intermediate values
  dataList[["co_models"]] <- co_models
  rm(join0)
  
  
  
  ###### ** 6. Regions & States ######
  ### Combine and add to data list...also a copy with info on national data
  ### Update in list
  # co_regions <- co_regions |> mutate(region = region_id)
  # dataList[["co_regions"]] <- co_regions
  co_states  <- co_states |> mutate(region = region |> str_replace_all(" ", ""))
  co_states  <- co_states |> mutate(region = region |> str_replace_all("\\.", ""))
  dataList[["co_states"]] <- co_states
  
  ###### ** 7. co_sectorsInfo ######
  ### Combine sectors with co_variants, co_impactTypes, co_impactYears to get group options
  co_sectorsInfo <- co_sectors |> pull(sector_id) |> (function(
    sectors0,  ### Sector IDs
    addRegions = TRUE, ### Whether to include regions & states
    addModels  = TRUE, ### Whether to include models
    colTypes   = c("ids", "labels", "extra") ### Types of columns to include: IDs, labels, or extra. If only labels, will return labels without the "_label"
  ){
    ### Get functions from FrEDI
    
    get_matches <- utils::getFromNamespace("get_matches", "FrEDI")
    
    ### Conditionals
    colTypes    <- colTypes |> tolower()
    doIds       <- "ids"    %in% colTypes
    doLabs      <- "labels" %in% colTypes
    doExtra     <- "extra"  %in% colTypes
    onlyLabs    <- !doIds
    
    ### Initialize some values as empty vectors
    ### Adjust values in vectors depending on conditionals
    colsReg0    <- c()
    colsMod0    <- c()
    if(addRegions) colsReg0 <- c("region", "state", "postal")
    if(addModels ) colsMod0 <- c("model")
    
    ### Column names
    colsData0   <- c("sector", "variant", "impactType", "impactYear") |> c(colsReg0) |> c("modelType") |> c(colsMod0)
    colsIds0    <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE) |> paste0("_id")
    colsLabs0   <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE) |> paste0("_label")
    # colsIds0    <- colsData0[!(colsData0 %in% c("modelType", "state", "postal"))] |> paste0("_id")
    # colsLabs0   <- colsData0[!(colsData0 %in% c("modelType", "state", "postal"))] |> paste0("_label")
    #colsVars    <- c("sectorprimary", "includeaggregate", "damageAdjName")
    colsVars    <- c("sectorprimary", "includeaggregate")
    
    colsTypes   <- c("impactType_description", "physicalmeasure") |>
      c(c("physScalar", "physAdj", "econScalar", "econMultiplier") |> paste0("Name")) |>
      c("c0", "c1", "exp0", "year0")
    colsMods0   <- c("maxUnitValue", "inputName") |>
      c("model" |> paste0(c("UnitDesc", "Unit_id", "Unit_label"))) |>
      c("model" |> paste0(c("UnitScale", "RefYear", "MaxOutput", "MaxExtrap")))
    colsMods0   <- colsMods0 |> get_matches(y=c("model" |> paste0(c("UnitScale", "RefYear", "MaxOutput", "MaxExtrap"))), matches=F)
    # colsMods0   <- colsMods0[!(colsMods0 %in% c("model" |> paste0(c("UnitScale", "RefYear", "MaxOutput", "MaxExtrap"))))]
    colsOth0    <- c()
    
    ### Add additional columns
    if(doExtra) {
      colsOth0 <- c(colsVars, colsTypes)
      if(addModels) colsOth0 <- colsOth0 |> c("maxUnitValue","damageAdjName")
    } ### if(doAll)
    
    
    ### Filter data
    hasSectors  <- sectors0 |> length()
    if(hasSectors) co_sectors <- co_sectors |> filter(sector_id %in% sectors0)
    
    ### Rename columns
    renameAt0   <- c("modelType")
    co_modTypes <- co_modelTypes |> rename_at(c(renameAt0 |> paste0("_id")), ~c(renameAt0))
    rm(renameAt0)
    
    ### Join with co_variants, co_impactTypes, co_impactYears
    join0   <- c("sector_id")
    join1   <- c("modelType")
    df0     <- co_sectors |> left_join(co_variants   , by=c(join0))
    df0     <- df0        |> full_join(co_impactTypes, by=c(join0), relationship="many-to-many")
    ### Cleanup dmgAdj issue
    df0     <- df0 |>
               mutate(
                 damageAdjName = case_when(
                   is.na(damageAdjName.x) ~ damageAdjName.y,
                   damageAdjName.y == "byVariant" ~ damageAdjName.x,
                   .default = damageAdjName.y
               )
               ) |>
               select(-damageAdjName.x,-damageAdjName.y)
    
    df0     <- df0        |> left_join(co_impactYears, by=c(join0), relationship="many-to-many")
    df0     <- df0        |> left_join(co_modTypes   , by=c(join1), relationship="many-to-many")
    rm(join0, join1)
    
    ### Join with co_regions and co_states if addStates
    if(addRegions) {
      ### Rename column in states
      ### Join states with regions
      ### Join data with states
      join0     <- c("region_id")
      join1     <- c("joinCol")
      renameAt0 <- c("region")
      co_states <- co_states |> rename_at(c(renameAt0), ~c(renameAt0 |> paste0("_id")))
      co_states <- co_states |> left_join(co_regions, by=c(join0))
      co_states <- co_states |> mutate(joinCol = 1)
      df0       <- df0       |> mutate(joinCol = 1)
      df0       <- df0       |> left_join(co_states, by=c(join1), relationship="many-to-many")
      df0       <- df0       |> select(-all_of(join1))
      rm(renameAt0, join0, join1)
    } ### End if(addModels)
    
    ### Join with co_models if addModels
    if(addModels) {
      # join0     <- c("modelType")
      join0     <- df0 |> names() |> get_matches(y=co_models |> names())
      # join0     <- (df0 |> names())[(df0 |> names()) %in% (co_models |> names())]
      df0       <- df0 |> left_join(co_models, by=c(join0), relationship="many-to-many")
      rm(join0)
    } ### End if(addModels)
    
    ### Rename values
    # df0 |> glimpse()
    # renameTo <- c("sector", "variant", "impactYear", "impactType") |> c(colsReg0) |> c(colsMod0)
    renameTo0 <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE)
    # renameTo0 <- colsData0[!(colsData0 %in% c("modelType", "state", "postal"))]
    renameAt0 <- renameTo0 |> paste0("_id")
    df0       <- df0       |> rename_at(c(renameAt0), ~renameTo0)
    
    ### Select values
    select0   <- c()
    names0    <- df0 |> names()
    if(doIds  ) select0 <- select0 |> c(colsData0) |> unique()
    if(doLabs ) select0 <- select0 |> c(colsLabs0) |> unique()
    if(doExtra) select0 <- select0 |> c(colsOth0 ) |> unique()
    df0       <- df0 |> select(all_of(select0))
    
    ### Arrange values
    arrange0  <- c()
    if     (doIds ) arrange0 <- c(colsData0)
    else if(doLabs) arrange0 <- c(colsLabs0)
    df0       <- df0 |> arrange_at(c(arrange0))
    
    ### Rename columns
    if(onlyLabs) {
      renameAt0 <- colsLabs0
      renameTo0 <- colsData0
      df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
      rm(rename0, renameAt, renameTo)
    } ### End if(onlyLabs)
    
    ### Return
    return(df0)
  })()
  ### Update in list, drop intermediate values
  # co_sectorsInfo |> glimpse()
  dataList[["co_sectorsInfo"]] <- co_sectorsInfo
  # rm(rename0, renameAt, renameTo)
  
  
  
  ###### ** 8. SLR Scenario Info ######
  ### Gather slr_cm columns
  # slr_cm |> names() |> print()
  slr_cm  <- slr_cm |> (function(df0, df1=co_models, cols0=c("year")){
    ### Gather slr_cm columns
    df0    <- df0 |> pivot_longer(
      cols      = -all_of(cols0), 
      names_to  = "model",
      values_to = "driverValue"
    ) ### End pivot_longer
    
    ### Add model type
    df0    <- df0 |> mutate(model_type = "slr")
    
    ### Zero out values and bind with other values
    df0_0cm <- df0     |> filter(model == "30cm")
    df0_0cm <- df0_0cm |> mutate(model = "0cm")
    df0_0cm <- df0_0cm |> mutate(driverValue = 0)
    df0     <- df0     |> filter(model != "0cm")
    df0     <- df0_0cm |> rbind(df0)
    rm(df0_0cm)
    
    ### Add model type
    drop0   <- c("model_type")
    df0     <- df0 |> select(-any_of(drop0))
    df0     <- df0 |> mutate(modelType = "slr" |> as.character())
    
    ### Arrange
    cols0   <- c("model", "year")
    df0     <- df0 |> arrange_at(vars(cols0))
    
    ### Return
    return(df0)
  })()
  ### Update in data list, drop intermediate values
  # slr_cm |> names() |> print()
  dataList[["slr_cm"]] <- slr_cm
  # dataList |> names() |> print()
  
  ###### Return ######
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeConfigData().", msgN) |> message()
  return(dataList)
}
