### For iterating over list names
### Calculating number of columns
fun_nCol <- function(
    z, ### Object name
    a, ### Object class
    b  ### List with objects
){
  # z |> print()
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame
  do_df0   <- "data.frame" %in% class0
  ### Get numbers of columns or length of object
  nCol0    <- obj0 |> ncol() |> (function(x){(x |> is.null()) |> ifelse(NA, x)})()
  nCol0    <- case_when(do_df0 ~ nCol0, .default=obj0 |> length())
  return(nCol0)
} ### End fun_nCol

### Calculating number of rows
fun_nRow <- function(z, a, b){
  # z |> print()
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  ### Get numbers of rows or length of object
  nRow0    <- obj0 |> nrow() |> (function(x){(x |> is.null()) |> ifelse(NA, x)})()
  nRow0    <- case_when(do_df0 ~ nRow0, .default=obj0 |> length())
  return(nRow0)
} ### End fun_nRow

### Calculating distinct rows or values
fun_nUnq <- function(z, a, b){
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  do_list0 <- "list"       %in% class0
  ### What to do for data frames
  if     (do_df0  ) {nUnq0 <- obj0 |> distinct() |> nrow()}
  else if(do_list0) {nUnq0 <- obj0 |> names   () |> unique() |> length()}
  else              {nUnq0 <- obj0 |> unique  () |> length()}
  return(nUnq0)
} ### End fun_nUnq

### Calculating columns with all NA vales
fun_allNA <- function(z, a, b){
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  # name0 |> paste(class0, sep=", ") |> print()
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  ### What to do for data frames
  if(do_df0) {nNna0 <- obj0 |> has_allNA_values_df()  }
  else       {nNna0 <- obj0 |> has_allNA_values_misc(a=class0)}
  return(nNna0)
} ### End fun_nNna


### Function to check if column has at least one non NA value
has_allNA_values_df <- function(df0) {
  ### Calculate number of rows
  nRow   <- df0 |> nrow()
  ### Check whether values in x are NA
  df0    <- df0 |> is.na()
  ### Number of NA values
  numNA  <- df0 |> colSums() |> nrow() |> (function(x){(x |> is.null()) |> ifelse(1, x)})()
  ### Whether all results are missing
  allNA  <- nRow == numNA
  ### Get number of rows %>%
  allNA  <- allNA |> which() |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_df

### Function to check non NA values for other types
has_allNA_values_list <- function(list0) {
  names0    <- list0 |> names()
  hasNames0 <- !(names0 |> is.null())
  if(hasNames0) {y <- names0 |> map(~ list0[[.]] |> class()) |> unlist()}
  else          {y <- list0  |> map(~ . |> class()) |> unlist()}
  skip_y <- ("function" %in% y) | ("list" %in% y)
  # skip_y |> print()
  ### If y has functions
  if(skip_y) {
    allNA  <- FALSE
  } else  {
    isNA0 <- list0 |> map(~ . |> unlist() |> is.na()) |> unlist()
    # allNA |> head |> print()
    allNA <- isNA0    |> all(na.rm=TRUE)
    len0  <- list0 |> map(~ .y |> length()) |> unlist()
    allNA <- allNA & (len0 > 1)
  } ### End if(skip_y)
  ### Count NA values
  allNA   <- y |> which() |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_misc

### Function to check non NA values for other types
has_allNA_values_misc <- function(b, a) {
  ### Get values
  class0   <- a
  obj0     <- b
  ### Check if list
  allNA    <- obj0 |> is.na() |> all(na.rm=TRUE)
  ### Which observations
  which0   <- allNA |> which()
  ### Count NA values
  allNA    <- which0 |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_misc




### Get region plot info (for scaled impact plots)
get_region_plotInfo <- function(
    df0,      ### Data
    yCol      = "scaled_impacts",
    # groupCols = c("sector", "variant", "impactType", "impactYear", "region", "model"),
    groupCols = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "maxUnitValue"),
    nCol      = 4,
    silent    = TRUE
){
  ###### Initialize Return List ######
  list0  <- list()
  
  ###### Get from FrEDI Namespace ######
  # fun_limitsByGroup <- utils::getFromNamespace("fun_limitsByGroup", "FrEDI")
  # get_column_values <- utils::getFromNamespace("get_column_values", "FrEDI")
  
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
  # ### Get number of NA values
  # df0 |> nrow() |> print(); df0[[yCol]] |> length() |> print()
  # df_na         <- df0 |> 
  #   group_by_at(.vars=c(groupCols)) |> 
  #   mutate(isNA = df0[[yCol]] |> is.na()) |>
  #   summarize(nNA=isNA |> sum(), .groups="keep") |> ungroup()
  # ### Join observations
  # df_na         <- df_obs |> left_join(df_na, by=c(group0))
  
  
  ### Join with df_sectorInfo
  df_sectorInfo <- df_sectorInfo |> left_join(df_na, by=c(group0))
  ### Drop values for which nObs == nNA
  # naThresh0     <- 
  df_sectorInfo <- df_sectorInfo |> mutate(naThresh = df_sectorInfo[["nObs"]] |> max())
  # return(df_sectorInfo)
  # df_sectorInfo <- df_sectorInfo |> filter(!(nObs < naThresh))
  df_sectorInfo <- df_sectorInfo |> filter(!(nObs == nNA))
  ### Remove values
  rm(df_na, group0)
  
  ###### Get Value Lists & Lengths ######
  ### Get number of sectors and calculate columns
  # doSectors  <- "sector"     %in% groupCols
  # doVariants <- "variant"    %in% groupCols
  # doTypes    <- "impactType" %in% groupCols
  # doYears    <- "impactYear" %in% groupCols
  # doRegions  <- "region"     %in% groupCols
  # doStates   <- "state"      %in% groupCols
  # doModels   <- "model"      %in% groupCols
  # cCols      <- c("sector", "variant", "impactType", "impactYear", "region")
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
    # fCol_i |> print(); cName_i |> print(); nName_i |> print();
    list0[[cName_i]] <- df_sectorInfo |> pull(all_of(fCol_i)) |> levels() |> as.character()
    list0[[nName_i]] <- list0[[cName_i]] |> length()
    # list0[[cName_i]] |> print()
    rm(i, fCol_i, cName_i, nName_i)
  } ### End for(i in test0 |> row_number())
  # list0 |> names() |> print
  
  ###### Number of Rows & Columns ######
  # "got here" |> print()
  # colCol    <- byState |> ifelse(list0[["state"]], list0[["region"]])
  # colCol    <- byState |> ifelse("state", "region")
  colCol    <- "state"
  rowCol    <- "impactType"
  ### Initialize rows & columns
  col_nCol  <- df_iter |> filter(column == colCol) |> get_column_values(col0 = "nName", unique0 = T)
  row_nCol  <- df_iter |> filter(column == rowCol) |> get_column_values(col0 = "nName", unique0 = T)
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
  col_oCol  <- df_iter |> filter(column == colCol) |> get_column_values(col0 = "orderCol", unique0 = T)
  row_oCol  <- df_iter |> filter(column == rowCol) |> get_column_values(col0 = "orderCol", unique0 = T)
  # col_oCol |> c(row_oCol) |> print(); df_sectorInfo[[col_oCol]] |> print(); df_sectorInfo[[row_oCol]] |> print()
  df_sectorInfo[["plotCol"]] <- df_sectorInfo[[col_oCol]]
  df_sectorInfo[["plotRow"]] <- df_sectorInfo[[row_oCol]]
  
  ### Get maximum and minimum values by plot row and combine
  df_minMax   <- df_sectorInfo |>
    group_by_at(.vars=c("plotRow")) |>
    summarize(min = min(min), max=max(max)) |>
    ungroup()
  ### Gather values
  df_minMax   <- df_minMax |> pivot_longer(
    cols      = -c("plotRow"),
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








