###### Get a tibble with power of 10 labels
get_p10Tibble <- function(
    max0=21
){
  ### Tibble to match to
  c_p10 <- seq(0, max0)
  df0   <- tibble(p10=c_p10)
  df0   <- df0 |> mutate(p1000 = p10 %/% 3)
  df0   <- df0 |> mutate(p1000 = p10 %/% 3)
  df0   <- df0 |> mutate(label = case_when(
    p1000 >= 0 & p1000 < 1 ~ "",
    p1000 >= 1 & p1000 < 2 ~ "Thousands",
    p1000 >= 2 & p1000 < 3 ~ "Millions",
    p1000 >= 3 & p1000 < 4 ~ "Billions",
    p1000 >= 4 & p1000 < 5 ~ "Trillions",
    p1000 >= 5 & p1000 < 6 ~ "Quadillions",
    p1000 >= 6 & p1000 < 7 ~ "Quintillions",
    .default = ""
  )) ### End case_when/mutate
  return(df0)
}

###### get_p10Labels
### Get power of 10 labels
get_p10Labels <- function(
    values,
    type0  = c("p10"), ### Power 10, power 1000, "p1000"
    p10max = 21        ### Max power
){
  ### Values
  vals0   <- values
  rm(values)
  
  ### Get tibble of values
  dfx     <- tibble(value=vals0)
  hasVals <- dfx |> nrow()
  if(!hasVals) {
    dfx   <- dfx |> mutate(value=1)
  } ### End if(!hasVals)
  
  ### Tibble with power10 labels
  df0   <- p10max |> get_p10Tibble()
  
  ### Rename columns
  ### Join data with power10 info
  dfx   <- dfx |> rename_at(c("value"), ~c(type0))
  dfx   <- dfx |> left_join(df0, by=c(type0))
  
  ### Return
  return(dfx)
}

###### fun_getScale
### This function creates a set of breaks for a particular column
### It returns a list of breaks, the power of 10, and the limits
fun_getScale <- function(
    data,
    scaleCol = "driverValue",
    nTicks   = 5
){
  ###### Check Values ######
  data    <- data
  xVals   <- data  |> pull(all_of(scaleCol))
  naVals  <- xVals |> is.na()
  nVals   <- xVals[!naVals] |> length()
  hasVals <- nVals
  
  ###### Min/Max Values ######
  if(hasVals) {
    ### Calculate values
    xMin      <- xVals |> min(na.rm=T)
    xMax      <- xVals |> max(na.rm=T)
    limits    <- c(xMin, xMax)
    # c(xMin, xMax)|> print()
    
    ### Set minimum to zero unless the minimum is less than zero
    ### If the values are all negative, set the maximum to zero.
    if(xMin > 0) xMin <- 0
    if(xMax < 0) xMax <- 0
    limits    <- c(xMin, xMax)
    
    ### Bounds
    bound_min <- xMin |> floor  ()
    bound_max <- xMax |> ceiling()
    
    ### Min/Max Tibble
    ### Limit names, values, bounds, etc
    df_minMax <- tibble(
      name      = c("min", "max"),
      value     = c(xMin, xMax),
      bound     = c(bound_min, bound_max),
      boundType = c("floor", "ceiling")
    ) |> mutate(hasVal = !(value |> is.na()))
    
    ###### Absolute Bounds
    df_minMax <- df_minMax |> mutate(bound_abs = bound |> abs())
    
    ###### Log10 Values
    ### Absolute value, Power of 10 and y-scale info
    df_minMax <- df_minMax |> mutate(log10 = bound_abs)
    
    ### Calculate log 10 and replace values of infinity with 0
    df_minMax <- df_minMax |> mutate(log10 = bound_abs |> log10())
    df_minMax <- df_minMax |> mutate(log10 = log10 |> abs())
    df_minMax <- df_minMax |> mutate(log10 = log10 |> na_if(Inf))
    df_minMax <- df_minMax |> mutate(log10 = log10 |> replace_na(0))
    
    ###### Power-10 Values
    ### Then get the floor of the log10 value
    df_minMax <- df_minMax |> mutate(p10   = log10 |> floor())
    df_minMax <- df_minMax |> mutate(p1000 = log10 |> floor())
    
   
    ### Get maximum power of 10, then scale to zero for negative numbers
    ### Integer division of power of 10 by 3 to get number of thousands
    ### Then get the modulus of the thousands
    # x_p10Max  <- df_minMax |> pull(p10) |> max(na.rm=T)
    x_p10Max  <- hasVals |> ifelse(df_minMax |> pull(p10) |> max(na.rm=T), NA)
    x_p1000   <- x_p10Max  %/% 3
    x_mod1000 <- x_p10Max  %% 3
    # df_minMax |> print(); x_p1000 |> print()
    # df_minMax$value|> print()
    # df_minMax$bound_abs|> print()
    # x_p1000|> print()
    
    ### Rounded bounded values (round to 1 essentially)
    divideByPower         <- x_p10Max - 1
    minMax_scaled         <- (df_minMax |> pull(value)) / 10**divideByPower
    bound_scaled_min      <- minMax_scaled[1] |> floor()
    bound_scaled_max      <- minMax_scaled[2] |> ceiling()
    bounds_scaled_rounded <- c(bound_scaled_min, bound_scaled_max)
    bounds_rounded        <- bounds_scaled_rounded * 10**divideByPower
    # minMax_scaled|> print()
    # bounds_rounded|> print()
    
    ###### Range
    x_range      <- bounds_rounded
    x_range_p10  <- x_range / 10**x_p10Max
    # x_range_dif  <- x_range_p10[2] - x_range_p10[1]
    x_p10_min    <- x_range_p10[1] |> floor()
    x_p10_max    <- x_range_p10[1] |> ceiling()
    x_p10_dif    <- x_p10_max - x_p10_min
    # x_range_p10 |> print(); x_p10_dif |> print()
    
    ### Determine unit of breaks in power of 10
    # x_range_p10|> print()
    x_unit_p10   <- 0.5
    ### X breaks with power of 10
    x_breaks_p10 <- x_range_p10[1] |> seq(x_range_p10[2], by=x_unit_p10)
    n_Ticks      <- x_breaks_p10   |> length()
    
    ### Check if number of ticks greater than threshold number
    cond1          <- n_Ticks > nTicks
    if(cond1) {
      x_unit_p10   <- 1
      x_breaks_p10 <- x_range_p10[1] |> seq(x_range_p10[2], by=x_unit_p10)
      n_Ticks      <- x_breaks_p10   |> length()
    } ### End if(cond1)
    
    ### Check again
    cond2          <- n_Ticks > nTicks
    if(cond2) {
      x_unit_p10   <- 2
      x_breaks_p10 <- x_range_p10[1] |> seq(x_range_p10[2], by=x_unit_p10)
      n_Ticks      <- x_breaks_p10 |> length()
    } ### End if(cond2)
    
    ### Number of breaks
    x_breaks       <- x_breaks_p10 * 10**x_p10Max
    # return(x_breaks)
  } else{
    df_minMax      <- NULL
    x_breaks       <- NULL
    limits         <- NULL
    bounds_rounded <- NULL
    x_p10Max       <- 0
    x_p1000        <- 0
    x_mod1000      <- 0
    # x_p10Max       <- NULL
    # x_p1000        <- NULL
    # x_mod1000      <- NULL
  } ### End if(hasData)
  
  ###### Return List ######
  ### Create list to return
  return_list <- list(
    breaks  = x_breaks,
    limits  = limits,
    # limits  = df_minMax |> pull(value),
    bounds  = bounds_rounded,
    p10     = x_p10Max,
    p1000   = x_p1000,
    mod1000 = x_mod1000
  ) ### End list
  
  ###### Return ######
  return(return_list)
} ### End fun_getScale()



###### get_colScale
### Get X Scale Values
get_colScale <- function(
    df0,   ### Data frame
    col0   = "xCol", ### Column to use
    nTicks = 5       ### Number of ticks
){
  ###### Initialize list
  list0    <- list()
  ###### X Scales
  x_info   <- df0 |> fun_getScale(scaleCol=col0, nTicks=nTicks)
  limits0  <- x_info[["limits"]]
  hasVals  <- !(limits0 |> is.null())
  
  ### If values present:
  ### - Modify values
  ### - Get labels
  if(hasVals) {
    ### Modify values
    x_p1000  <- x_info[["p1000"]]
    x_p10    <- x_p1000 * 3
    x_denom  <- 10**x_p10
    # x_info[["breaks"]] |> print()
    x_breaks <- x_info[["breaks"]] / x_denom
    x_limits <- x_info[["limits"]] / x_denom
    
    ### Add labels
    # x_lab    <- x_p1000
    x_lab    <- x_p1000 |> get_p10Labels(type="p1000")
    x_lab    <- x_lab[["label"]][1]
    # x_p10 |> print(); x_p1000 |> print(); x_lab |> print()
  } else{
    ### Modify values
    x_p1000  <- 0
    x_p10    <- 0
    x_denom  <- 10**x_p10
    # x_info[["breaks"]] |> print()
    x_breaks <- x_info[["breaks"]] / x_denom
    x_limits <- x_info[["limits"]] / x_denom
    
    ### Add labels
    x_lab    <- ""
  } ### End if(hasVals)
 

  ###### Update list
  list0[["scale" ]] <- x_info
  list0[["p10"   ]] <- x_p10
  list0[["p1000" ]] <- x_p1000
  list0[["denom" ]] <- x_denom
  list0[["breaks"]] <- x_breaks
  list0[["limits"]] <- x_limits
  list0[["label" ]] <- x_lab
  
  ###### Return
  return(list0)
} ### End get_colScale()


###### fun_limitsByGroup
### This function summarizes data for a particular group
### It returns a dataframe of results with the groups, a column called "summary_type", and the summarized value "summary_value"
fun_limitsByGroup <- function(
    data,
    groupCols = c("sector"),
    sumCols   = c("annual_impacts"),
    type      = c("min", "max"),
    silent    = FALSE,
    msg       = ""
){
  ###### Defaults ######
  ### Whether to message
  print_msg  <- !silent
  msg0       <- msg
  msg1       <- "\t" |> paste0(msg0)
  if(print_msg){ msg0 |> paste0("Running fun_limitsByGroup()...") |> message()}
  ###### Data ######
  names0     <- data |> names()
  sumCols0   <- sumCols
  groupCols0 <- groupCols
  ###### Check for Summary Columns ######
  ### Check if the summary columns are present
  hasSumCols   <- sumCols %in% names0
  sumCols      <- sumCols[ hasSumCols]
  naSumCols    <- sumCols[!hasSumCols]
  nSumCols     <- hasSumCols |> length()
  ### Conditions
  anySumCols   <- hasSumCols |> any()
  multSumCols  <- nSumCols > 1
  hasNaSumCols <- length(naSumCols) > 0
  ### Messaging
  msgSumCols   <-
    ### If no summary columns
    if     (!anySumCols ){
      if(print_msg){msg1 |> paste0("Summary columns ", paste(sumCols0, collapse=", "), " not present in data...") |> message()}
      if(print_msg){msg1 |> paste0("Exiting...") |> message()}
      return()
    } ### End if(!hasSumCols)
  else if(hasNaSumCols){
    if(print_msg){msg1 |> paste0("Summary columns ", paste(naSumCols, collapse=", "), " not present in data...") |> message()}
  }
  ### IF multiple columns
  # if(multSumCols){
  #   if(print_msg){ "\t" |> paste0("More than one summary column provided. Summarizing the first column only...") |> message()}
  #   sumCols <- sumCols[1]
  # }

  ###### Check for Group Columns ######
  ### Check if the group columns is present
  hasGroupCols   <- (groupCols %in% names0)
  groupCols      <- groupCols[ hasGroupCols]
  naGroupCols    <- groupCols[!hasGroupCols]
  nGroupCols     <- hasGroupCols |> length()
  ### Conditions
  anyGroupCols   <- hasGroupCols |> any()
  multGroupCols  <- nGroupCols > 1
  hasNaGroupCols <- length(naGroupCols) > 0
  ### If no grouping columns
  if     (!anyGroupCols ){
    if(print_msg){msg1 |> paste0("Group columns ", paste(groupCols0, collapse=", "), " not present in data...") |> message()}
    if(print_msg){msg1 |> paste0("Exiting...") |> message()}
    return()
  } ### End if(!hasGroupCols)
  else if(hasNaGroupCols){
    if(print_msg){msg1 |> paste0("Group columns ", paste(naGroupCols, collapse=", "), " not present in data...") |> message()}
  }

  ###### Summarize Values ######
  ### Message the user
  if(print_msg){msg1 |> paste0("Summarizing values for columns ", paste(sumCols, collapse=", "), "...") |> message()}
  if(print_msg){msg1 |> paste0("Grouping by columns ", paste(groupCols, collapse=", "), "...") |> message()}
  ### Summarize values by sector
  select0      <- c(groupCols, sumCols)
  lim_bySector <- data |> select(all_of(select0)) |> unique()
  naVals       <- data[,sumCols] |> is.na() |> rowSums() |> as.logical()
  nVals        <- (!naVals) |> which() |> length()
  ### Group and summarize if there are non-missing values
  ### No need to summarize if all values are missing
  if(nVals) {
    lim_bySector <- lim_bySector |> 
      group_by_at(c(groupCols)) |>
      summarize_at(c(sumCols), .funs=c(type), na.rm=T)
  } ### End if(nVals)
  ###### Pivot longer ######
  lim_bySector <- lim_bySector |>
    pivot_longer(
      cols      = -all_of(groupCols), 
      names_to  = "summary_type", 
      values_to = "summary_value"
    ) ### #nd pivot_longer
  # df_limBySector |> print()

  ###### Spread ######
  ### Spread & calculate the spread
  lim_bySector <- lim_bySector |> pivot_wider(
    names_from  = "summary_type", 
    values_from = "summary_value"
  ) ### End pivot_wider
  ### Calculate spread
  lim_bySector <- lim_bySector |> mutate(spread= max - min)

  ###### Arrange ######
  ### Arrange the sectors & get their order
  arrange0     <- c("max", "spread")
  lim_bySector <- lim_bySector |> arrange_at(c(arrange0), desc)
  rm(arrange0)
  # lim_bySector |> print()

  ###### Get Group Orders ######
  for(group_i in groupCols) {
    ### Get unique values in arranged order
    val_i <- lim_bySector[[group_i]] |> unique()
    ### Factor values & get order
    ord_i <- "order" |> paste0("_", group_i)
    fac_i <- group_i |> paste0("_", "factor")
    lim_bySector[[fac_i]] <- lim_bySector[[group_i]] |> factor(levels=val_i)
    lim_bySector[[ord_i]] <- lim_bySector[[fac_i  ]] |> as.numeric()
    rm(group_i, val_i, ord_i, fac_i)
  } ### End for(group_i in groupCols)

  ###### Arrange Again ######
  ### Arrange the sectors & get their order
  # arrange0     <- groupCols    |> c("max", "spread")
  arrange0     <- "order" |> paste0("_", groupCols)
  lim_bySector <- lim_bySector |> arrange_at(c(arrange0))
  lim_bySector <- lim_bySector |> mutate(order = row_number())
  # lim_bySector |> print()

  ### Return value
  if(print_msg) msg1 |> paste0("...Finished.") |> message()
  return(lim_bySector)
}


###### get_sector_plotInfo
### Get sector plot info
get_sector_plotInfo <- function(
    df0,      ### Data
    yCol      = "yCol",
    byType    = FALSE,
    groupCols = c("sector"),
    nCol      = 4,
    silent    = TRUE
){
  ###### Initialize Return List ######
  list0  <- list()

  ###### Grouping Columns ######
  if(byType) {
    group0    <- c("impactYear", "variant", "impactType")
    groupCols <- groupCols[!(groupCols %in% group0)]
    groupCols <- groupCols |> c(group0)
  } ### End if(byType)

  ###### Get Value Ranges ######
  # df0 |> glimpse()
  df_sectorInfo <- df0 |> fun_limitsByGroup(
    sumCols   = yCol,
    groupCols = groupCols,
    silent    = silent
  ) ### End fun_limitsByGroup
  # df_sectorInfo |> print()

  ###### Get Value Lists & Lengths ######
  ### Get number of sectors and calculate columns
  doSector   <- "sector"     %in% groupCols
  doYears    <- "impactYear" %in% groupCols
  doVariants <- "variant"    %in% groupCols
  doTypes    <- "impactType" %in% groupCols
  ### Sectors
  if(doSector) {
    c_sectors <- df_sectorInfo[["sector_factor"]] |> levels() |> as.character()
    n_sectors <- c_sectors |> length()
    list0[["cSectors" ]] <- c_sectors |> as.character()
    list0[["nSectors" ]] <- n_sectors
  } ### End if(doSector)
  ### Impact Years
  if(doYears) {
    c_impYears <- df_sectorInfo[["impactYear_factor"]] |> levels() |> as.character()
    n_impYears <- c_impYears |> length()
    list0[["cImpYears"]] <- c_impYears |> as.character()
    list0[["nImpYears"]] <- n_impYears
  } ### End if(doYears)
  ### Variants
  if(doVariants) {
    c_variants <- df_sectorInfo[["variant_factor"]] |> levels() |> as.character()
    n_variants <- c_variants |> length()
    list0[["cVariants"]] <- c_variants |> as.character()
    list0[["nVariants"]] <- n_variants
  } ### End if(doVariants)
  ### Impact Types
  if(doTypes) {
    c_impTypes <- df_sectorInfo[["impactType_factor"]] |> levels() |> as.character()
    n_impTypes <- c_impTypes |> length()
    list0[["cImpTypes"]] <- c_impTypes |> as.character()
    list0[["nImpTypes"]] <- n_impTypes
  } ### End if(doTypes)

  ###### Number of Rows & Columns ######
  ### Initialize rows & columns
  nCol      <- nCol
  nRow      <- n_sectors %/% nCol
  nRow      <- (nRow == 0) |> ifelse(1, nRow)
  ### Get Number of Rows & Columns
  if(byType) nCol <- n_variants
  if(byType) nRow <- n_impTypes
  ### Add to list
  list0[["nCol"]] <- nCol
  list0[["nRow"]] <- nRow

  ###### Min/Max Info ######
  ### Also figure out sector positions in the list of plots
  if(byType) {
    df_sectorInfo <- df_sectorInfo |> mutate(plotCol = order_variant   )
    df_sectorInfo <- df_sectorInfo |> mutate(plotRow = order_impactType)
  } else{
    df_sectorInfo <- df_sectorInfo |> mutate(plotRow = ((order - 1) %/% nCol) + 1)
    df_sectorInfo <- df_sectorInfo |> mutate(plotCol = ((order - 1) %%  nCol) + 1)
  } ### End if(byType)

  # df_sectorInfo |> print()

  ### Get maximum and minimum values by plot row and combine
  df_minMax   <- df_sectorInfo |>
    group_by_at(c("plotRow")) |>
    summarize(min = min(min), max = max(max)) |> ungroup()
  ### Gather values
  df_minMax   <- df_minMax |> pivot_longer(
    cols      = -c("plotRow"),
    names_to  = "summary_type", 
    values_to = "summary_value"
  ) ### End pivot_longer
  # df_minMax[["summary_value"]] |> print()

  ###### Return List ######
  ### Return list
  list0 <- list(minMax     = df_minMax    ) |> c(list0)
  list0 <- list(sectorInfo = df_sectorInfo) |> c(list0)
  ### Return
  return(list0)
}


### Get region plot info (for scaled impact plots)
get_region_plotInfo <- function(
    df0,      ### Data
    yCol      = "scaled_impacts",
    # groupCols = c("sector", "variant", "impactType", "impactYear", "region", "model"),
    groupCols = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "maxUnitValue"),
    nCol      = 4, ### Number of columns
    silent    = TRUE
){
  ###### Initialize Return List ######
  list0  <- list()
  
  ###### Get from FrEDI Namespace ######
  # fun_limitsByGroup <- utils::getFromNamespace("fun_limitsByGroup", "FrEDI")
  
  ###### Get Value Ranges ######
  # df0 |> glimpse()
  df_sectorInfo <- df0 |> fun_limitsByGroup(
    sumCols   = yCol,
    groupCols = groupCols,
    silent    = silent
  ) ### End fun_limitsByGroup()
  # df_sectorInfo |> print()
  
  ###### Number of NA Values ######
  ### Get number of observations in a group
  group0        <- groupCols[!(groupCols %in% c("state", "postal"))]
  df_na         <- df0   |> mutate_at(c(yCol), is.na)
  df_na         <- df_na |> rename_at(c(yCol), ~c("nNA"))
  df_na         <- df_na |> mutate(nObs = 1)
  df_na         <- df_na |>
    group_by_at (c(group0)) |> 
    summarize_at(c("nObs", "nNA"), sum) |> ungroup()
  
  ### Join with df_sectorInfo
  df_sectorInfo <- df_sectorInfo |> left_join(df_na, by=c(group0))
  
  ### Drop values for which nObs == nNA
  df_sectorInfo <- df_sectorInfo |> mutate(naThresh = df_sectorInfo[["nObs"]] |> max())
  df_sectorInfo <- df_sectorInfo |> filter(!(nObs == nNA))
  ### Remove values
  rm(df_na, group0)
  
  ###### Get Value Lists & Lengths ######
  ### Get number of sectors and calculate columns
  cCols      <- groupCols[!(groupCols %in% c("postal"))]
  df_iter    <- tibble(column = cCols)
  df_iter    <- df_iter |> mutate(colSuffix = case_when(
    column == "impactType" ~ "ImpTypes",
    column == "impactYear" ~ "ImpYears",
    .default = column |> str_to_title() |> paste0("s")
  )) ### End mutate
  df_iter    <- df_iter |> mutate(cName = "c" |> paste0(colSuffix))
  df_iter    <- df_iter |> mutate(nName = "n" |> paste0(colSuffix))
  df_iter    <- df_iter |> mutate(factorCol = column  |> paste0("_factor"))
  df_iter    <- df_iter |> mutate(orderCol  = "order_" |> paste0(column))
  ### Iterate over iteration column
  for(i in df_iter |> row_number()){
    fCol_i  <- df_iter[["factorCol"]][i]
    cName_i <- df_iter[["cName"    ]][i]
    nName_i <- df_iter[["nName"    ]][i]
    # fCol_i |> print(); cName_i |> print(); nName_i |> print()
    list0[[cName_i]] <- df_sectorInfo |> pull(all_of(fCol_i)) |> levels() |> as.character()
    list0[[nName_i]] <- list0[[cName_i]] |> length()
    # list0[[cName_i]] |> print()
    rm(i, fCol_i, cName_i, nName_i)
  } ### End for(i in test0 |> row_number())
  # list0 |> names() |> print()
  
  ###### Number of Rows & Columns ######
  colCol    <- "state"
  rowCol    <- "impactType"
  ### Initialize rows & columns
  col_nCol  <- df_iter |> filter(column == colCol) |> pull(nName) |> unique()
  row_nCol  <- df_iter |> filter(column == rowCol) |> pull(nName) |> unique()
  # col_nCol |> c(row_nCol) |> print(); list0 |> names() |> print()
  nCol      <- list0[[col_nCol]]
  nRow      <- list0[[row_nCol]]
  # nCol |> c(nRow) |> print()
  ### Correct for zeros
  nCol      <- (nCol == 0) |> ifelse(1, nCol)
  nRow      <- (nRow == 0) |> ifelse(1, nRow)
  ### Add to list
  list0[["nCol"]] <- nCol
  list0[["nRow"]] <- nRow
  
  ###### Min/Max Info ######
  ### Also figure out sector positions in the list of plots
  col_oCol  <- df_iter |> filter(column == colCol) |> pull(orderCol) |> unique()
  row_oCol  <- df_iter |> filter(column == rowCol) |> pull(orderCol) |> unique()
  # col_oCol |> c(row_oCol) |> print(); df_sectorInfo[[col_oCol]] |> print(); df_sectorInfo[[row_oCol]] |> print()
  df_sectorInfo[["plotCol"]] <- df_sectorInfo[[col_oCol]]
  df_sectorInfo[["plotRow"]] <- df_sectorInfo[[row_oCol]]
  
  ### Get maximum and minimum values by plot row and combine
  group0      <- c("plotRow")
  group0      <- c("sector", "variant", "impactType", "impactYear", "region") |> c(group0)
  df_minMax   <- df_sectorInfo |>
    group_by_at(c(group0)) |>
    summarize(min = min(min), max = max(max)) |> ungroup()
  ### Gather values
  df_minMax   <- df_minMax |> pivot_longer(
    cols      = -c(group0),
    names_to  = "summary_type", 
    values_to = "summary_value"
  ) ### End pivot_longer
  # df_minMax[["summary_value"]] |> print()
  
  ###### Return List ######
  ### Return list
  list0[["df_iter"   ]] <- df_iter
  list0[["minMax"    ]] <- df_minMax
  list0[["sectorInfo"]] <- df_sectorInfo
  ### Return
  return(list0)
}

###### End Script ######
