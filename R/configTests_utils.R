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
  # # val2     <- val2 |> is.null() |> ifelse(val3, val2)
  # # val3     <- 0    |> as.integer()
  # val3     <- NA
  # # val1 |> print(); val2 |> print(); val3 |> print()
  # ### Value to return
  # y        <- do_df0 |> if_else(true=val2, false=val3)
  # return(y)
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
  nRow0    <- obj0 |> ncol() |> (function(x){(x |> is.null()) |> ifelse(NA, x)})()
  nRow0    <- case_when(do_df0 ~ nRow0, .default=obj0 |> length())
  return(nRow0)
} ### End fun_nRow

# fun_nRow <- function(z, a, b){
#   # z |> print()
#   ### Check if data frame
#   do_df0 <- "data.frame" %in% a[[z]]
#   val2 <- b[[z]] |> nrow()
#   val3 <- b[[z]] |> length()
#   val2 <- val2   |> is.null() |> ifelse(val3, val2)
#   # val1 |> print(); val2 |> print(); val3 |> print()
#   y    <- do_df0   |> if_else(true=val2, false=val3)
#   return(y)
# } ### End fun_nRow

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
# fun_nUnq <- function(z, a, b){
#   ### Objects
#   class_z <- a[[z]]
#   obj_z   <- b[[z]]
#   do_df0  <- "data.frame" %in% class_z
#   ### Values
#   val1    <- do_df0
#   ### What to do for data frames
#   if(do_df0) {val2 <- obj_z |> distinct() |> nrow()}
#   else       {val2 <- obj_z |> unique() |> length()}
#   # val1 |> print(); val2 |> print(); val3 |> print()
#   val3    <- val2
#   y       <- if_else(val1, true=val2, false=val3)
#   return(y)
# } ### End fun_nUnq

### Calculating columns with all NA vales
# fun_nNna <- function(z, a, b){
#   ### Values
#   val1 <- "data.frame" %in% a[[z]]
#   ### What to do for data frames
#   if(val1) {val2 <- b[[z]] |> has_nonNA_values_df()  }
#   else     {val2 <- b[[z]] |> has_nonNA_values_misc()}
#   ### Other values
#   val3 <- val2
#   y    <- if_else(val1, val2, val3)
#   return(y)
# } ### End fun_nNna
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
# has_nonNA_values_df <- function(df0) {
#   ### Calculate number of rows
#   df1    <- tibble(numRows = df0 |> nrow())
#   ### Check whether values in x are NA
#   df0    <- df0 |> is.na()
#   ### Number of NA values
#   df1    <- df1 |> mutate(numNA = df0 |> colSums() |> nrow() |> is.null() |> if_else(0, 1))
#   ### Whether all results are missing
#   df1    <- df1 |> mutate(allNA = (numRows == numNA))
#   ### Filter to values with allNA
#   df1    <- df1 |> filter(allNA)
#   ### Get number of rows %>%
#   nNonNA <- df1 |> nrow()
#   ### Return
#   return( )
# } ### End has_nonNA_values_df
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
# has_nonNA_values_misc <- function(b, a) {
#   ### Get values
#   name0    <- z
#   class0   <- a
#   obj0     <- b
#   ### Check if list
#   len0     <- obj0 |> length()
#   if(isList0) {
#     names_x    <- x |> names()
#     hasNames_x <- !(names_x |> is.null())
#     if(hasNames_x) {y <- names_x |> map(~ x[[.]] |> class()) |> unlist()}
#     else           {y <- x       |> map(~ . |> class()) |> unlist()}
#     skip_y <- ("function" %in% y) | ("list" %in% y)
#     # skip_y |> print()
#     ### If y has functions
#     if(skip_y) {y <- FALSE}
#     else  {
#       y0 <- x |> map(~ . |> unlist() |> is.na()) |> unlist()
#       # y0 |> head |> print()
#       y0 <- y0 |> all(na.rm=TRUE)
#       y1 <- x |> map(~ .y |> length()) |> unlist()
#       y  <- y0 & (y1 > 1)
#     } ### End else(skip_y)
#   } ### End if(isList0) 
#   else        {y <- x |> is.na() |> all(na.rm=TRUE)}
#   ### Which observations
#   which_x <- y |> which()
#   ### Count NA values
#   z       <- y |> which() |> length()
#   ### Return
#   return(z)
# } ### End has_nonNA_values_misc




### Get region plot info (for scaled impact plots)
get_region_plotInfo <- function(
    df0, ### Data
    yCol      = "scaled_impacts",
    # byState   = FALSE,
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
  
  ###### Grouping Columns ######
  ### Add state to grouping
  # if(byState){
  #   group0    <- c("state", "postal", "model")
  #   groupCols <- groupCols |> (function(x){x[!(x %in% group0)]})
  #   groupCols <- groupCols |> c(group0)
  #   rm(group0)
  # } ### End if(byType)
  # groupCols |> print()
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
  doSectors  <- "sector"     %in% groupCols
  doVariants <- "variant"    %in% groupCols
  doTypes    <- "impactType" %in% groupCols
  doYears    <- "impactYear" %in% groupCols
  doRegions  <- "region"     %in% groupCols
  doStates   <- "state"      %in% groupCols
  # cCols      <- c("sector", "variant", "impactType", "impactYear", "region")
  cCols      <- groupCols[!(groupCols %in% c("postal"))]
  df_iter    <- tibble(column = cCols)
  df_iter    <- df_iter |>
    mutate(colSuffix = case_when(
      column == "impactType" ~ "ImpTypes",
      column == "impactYear" ~ "ImpYears",
      .default = column |> str_to_title() |> paste0("s")
    ))
  df_iter    <- df_iter |> mutate(cName = "c" |> paste0(colSuffix))
  df_iter    <- df_iter |> mutate(nName = "n" |> paste0(colSuffix))
  df_iter    <- df_iter |> mutate(factorCol = column  |> paste0("_factor"))
  df_iter    <- df_iter |> mutate(orderCol  = "order_" |> paste0(column))
  ### Iterate over iteration column
  for(i in df_iter |> row_number()){
    fCol_i  <- df_iter[["factorCol"]][i]
    cName_i <- df_iter[["cName"]][i]
    nName_i <- df_iter[["nName"]][i]
    # fCol_i |> print(); cName_i |> print(); nName_i |> print();
    list0[[cName_i]] <- df_sectorInfo[[fCol_i]] |> levels() |> as.character()
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
  list0 <- list(df_iter    = df_iter      ) |> c(list0)
  list0 <- list(minMax     = df_minMax    ) |> c(list0)
  list0 <- list(sectorInfo = df_sectorInfo) |> c(list0)
  ### Return
  return(list0)
}


### This function plots degrees of warming by sector, variant, impact year, and type
create_scaledImpact_plot <- function(
    data,
    sector0,
    impType0,
    impYear0,
    region0,   ### Region or state
    # byState  = FALSE,
    infoList0, ### Dataframe with sector info...output from get_region_plotInfo
    xCol     = "driverValue", ### X-Column,
    yCol     = "scaled_impacts", ### Y-Column,
    colorCol = "model",
    xInfo   = NULL , ### xScale...outputs of get_colScale
    refPlot = FALSE, ### Whether to do a ref plot
    nTicks  = 5,
    silent  = TRUE,
    options = list(
      # title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Scaled Impacts",
      lgdTitle   = "Model",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Messaging ######
  print_msg <- !silent
  if(print_msg){ "Running create_scaledImpact_plot()..." |> message()}
  
  ###### Get from FrEDI Namespace ######
  # get_colScale <- utils::getFromNamespace("get_colScale", "FrEDI")
  
  ###### Data ######
  df0        <- data |> filter(sector     == sector0)
  df0        <- df0  |> filter(impactType == impType0)
  df0        <- df0  |> filter(impactYear == impYear0)
  type0      <- df0[["model_type"]] |> unique()
  # df0 |> glimpse()
  
  ###### By State ######
  byState    <- df0[["state"]] |> unique() |> (function(x){!("N/A" %in% x)})()
  stateCols  <- c("state", "postal")
  if(byState){df0 <- df0 |> filter(region == region0)} 
  
  ###### Model Types ######
  # type0 %>% print
  do_gcm     <- "gcm" %in% (type0 |> tolower())
  do_slr     <- "slr" %in% (type0 |> tolower())
  
  ###### Sector Info ######
  info0      <- infoList0[["sectorInfo"]] |> filter(sector==sector0)
  index0     <- info0[["sector_order"]][1]
  row0       <- info0[["plotRow"     ]][1]
  col0       <- info0[["plotCol"     ]][1]
  
  ###### Breaks info ######
  ###### ** X Breaks ######
  do_xInfo   <- is.null(xInfo)
  if(do_xInfo){
    if(xCol == "year"){
      x_limits   <- c(2000, 2100)
      x_breaks <- seq(x_limits[1] - 10, x_limits[2] + 10, by = 20)
      x_denom  <- 1
    } ### End if(xCol == "year")
    else              {
      x_limits <- c(-1, 11)
      x_breaks <- seq(0, 10, by=2)
      x_denom  <- 1
    } ### End else(xCol == "year")
  } ### End if(do_xInfo)
  else{
    x_scale    <- xInfo[["scale" ]]
    x_p10      <- xInfo[["p10"   ]]
    x_denom    <- xInfo[["denom" ]]
    x_breaks   <- xInfo[["breaks"]]
    x_limits   <- xInfo[["limits"]]
  }
  ###### ** Y-Breaks ######
  y_info     <- infoList0[["minMax"]] |> filter(plotRow == row0)
  y_info     <- y_info |> mutate(sector=sector0)
  y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
  ### Additional info
  y_scale    <- y_info[["scale" ]]
  # y_scale |> names() |> print()
  y_p10      <- y_info[["p10"   ]]
  y_denom    <- y_info[["denom" ]]
  y_breaks   <- y_info[["breaks"]]
  y_limits   <- y_info[["limits"]]
  y_label    <- y_info[["label" ]]
  ### Labeling
  y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  y_label    <- (y_label=="") |> ifelse("Scaled Impacts", paste0("(", y_label, ")"))
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()
  
  ###### Mutate Data ######
  # "got here" |> print(); x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom
  
  
  # ###### Plot Options ######
  ###### Defaults ######
  ### Defaults
  # def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by GMSL (cm)")
  # def_xTitles <- list(GCM=expression("Degrees of Warming (°C)") , SLR="GMSL (cm)")
  # def_lgdLbls <- list(GCM="Region", SLR="Year")
  def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by Year")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)") , SLR="Year")
  def_lgdLbls <- list(GCM="Region", SLR="Region")
  def_margins <- list(GCM=c(0, 0, .15, 0), SLR=c(0, .2, .15, 0))
  ### Values
  title0      <- options[["title"     ]]
  xTitle      <- options[["xTitle"    ]]
  yTitle      <- options[["yTitle"    ]]
  lgdLbl      <- options[["lgdTitle"  ]]
  lgdPos      <- options[["lgdPos"    ]]
  heights     <- options[["heights"   ]]
  margins     <- options[["margins"   ]]
  mUnit       <- options[["marginUnit"]]
  theme0      <- options[["theme"     ]]
  ### Plot options
  hasTitle    <- !(is.null(title0  ))
  hasXTitle   <- !(is.null(xTitle  ))
  hasYTitle   <- !(is.null(yTitle  ))
  hasLgdLbl   <- !(is.null(lgdLbl  ))
  hasLgdPos   <- !(is.null(lgdPos  ))
  hasHeights  <- !(is.null(heights ))
  hasMargins  <- !(is.null(margins ))
  hasMUnits   <- !(is.null(mUnit   ))
  hasTheme    <- !(is.null(theme0  ))
  ### Defaults: Default Heights Below
  def_title   <- do_gcm |> ifelse(def_titles [["GCM"]], def_titles [["SLR"]])
  def_xTitle  <- do_gcm |> ifelse(def_xTitles[["GCM"]], def_xTitles[["SLR"]])
  def_margin  <- do_gcm |> ifelse(def_margins[["GCM"]], def_margins[["SLR"]])
  def_lgdLbl  <- do_gcm |> ifelse(def_lgdLbls[["GCM"]], def_lgdLbls[["SLR"]])
  def_lgdPos  <- "top"
  def_yTitle  <- "Scaled Impacts"
  def_mUnit   <- "cm"
  def_theme   <- NULL
  ### Values: Height Values Below
  if(!hasTitle  ){title0  <- def_title }
  if(!hasXTitle ){xTitle  <- def_xTitle}
  if(!hasYTitle ){yTitle  <- def_yTitle}
  if(!hasLgdLbl ){lgdLbl  <- def_lgdLbl}
  if(!hasMargins){margins <- def_margin}
  if(!hasMUnits ){mUnit   <- def_mUnit }
  if(!hasTheme  ){theme0  <- def_theme }
  # xTitle |> print()
  ###### Standardize column names ######
  # title0 <- byState |> ifelse(state0, region0)
  title0 <- "Region: " |> paste0(region0)
  
  ###### Create the plot ######
  # colorCol |> print(); def_lgdLbl |> print()
  # plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[colorCol]]))
  ### Group values
  groups0  <- c("sector", "variant", "impactType", "impactYear", "region", "model", "maxUnitValue", xCol)
  facetCol <- byState |> ifelse("state", "region") 
  if(byState){groups0 <- groups0 |> c("state")} 
  df0    <- df0 |> group_by_at(c(groups0))
  rm(groups0)
  
  ### Points dataframe
  if(do_slr){df_points0 <- df0 |> filter(year %in% x_breaks)}
  else      {df_points0 <- df0}
  
  ###### ** Initialize plot
  ### Initialize plot
  plot0  <- ggplot()
  
  ### Check if the plot needs to be made
  allNA  <- df0[[yCol]] |> is.na() |> all()
  doPlot <- !allNA
  if(doPlot){
    ### Determine the columns to use
    if(byState) {regCol0   <- c("state" ); stateCol0 <- c("state")} 
    else        {regCol0   <- c("region"); stateCol0 <- c()}
    group0 <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCol0) |> c("model")
    
    ###### ** Add geoms
    if(do_slr){
      ### Factor model
      # lvls0  <- df0[["driverValue"]] |> unique() |> sort(decreasing = T)
      # lvls0  <- lvls0 |> paste("cm")
      # df0    <- df0 |> mutate(model = model |> factor(levels = lvls0))
      # rm(lvls0)
      ### Points data
      plot0  <- df0 |> ggplot()
      plot0  <- plot0 + geom_line(
        data  = df0, 
        aes(
          x     = .data[[xCol]], 
          y     = .data[[yCol]], 
          color = .data[[regCol0]], 
          group = interaction(!!!syms(group0)), 
          linetype = .data[["variant"]]
        ), ### End aes
        alpha = 0.65
      ) ### End geom_line
      
      ### Add points
      plot0  <- plot0 + geom_point(
        data  = df_points0, 
        aes(
          x     = .data[[xCol]], 
          y     = .data[[yCol]], 
          color = .data[[regCol0]], 
          # group = interaction(!!!syms(group0))
          group = interaction(!!!syms(group0)),
          shape = .data[["variant"]]
        ), ### End aes
        alpha = 0.65
      ) ### End geom_point
      # rm(df0_2)
    } else{
      ### Separate GCM values
      ### Plot these values as lines
      df0_1 <- df0 |> filter((maxUnitValue < 6 & driverValue <= maxUnitValue) | maxUnitValue >=6) 
      ### Plot these values as points
      df0_2 <- df0 |> filter((maxUnitValue < 6 & driverValue >= maxUnitValue))
      ### Plot values as lines
      plot0  <- plot0 + geom_line(
        data  = df0_1,
        aes(
          x        = .data[[xCol]], 
          y        = .data[[yCol]], 
          color    = .data[[regCol0]], 
          group    = interaction(!!!syms(group0)), 
          linetype = .data[["variant"]]
        ), ### End aes
        alpha = 0.65 
      ) ### End geom_line
      ### Plot values as points
      plot0  <- plot0 + geom_point(
        data = df0_2,
        aes(
          x        = .data[[xCol]], 
          y        = .data[[yCol]], 
          color    = .data[[regCol0]], 
          # group    = interaction(!!!syms(group0))
          group    = interaction(!!!syms(group0)),
          shape   = .data[["variant"]]
        ), ### End aes
        alpha=0.65 
      ) ### End geom_line
    }
    
    ###### * Add geoms
    # plot0  <- plot0 + geom_line(aes(linetype = .data[["variant"]]), alpha=0.5)
    
    ###### ** Add facet_grid
    plot0  <- plot0 + facet_grid(model~.data[[regCol0]])
    # plot0 |> print()
    
    ###### ** Adjust legend title
    if(hasLgdPos){plot0 <- plot0 + guides(color = guide_legend(title.position = lgdPos))}
    plot0  <- plot0 + theme(legend.direction = "vertical", legend.box = "vertical")
    
    ###### ** Add and title
    plot0  <- plot0 + ggtitle(title0)
    
    ###### ** Add scales
    plot0  <- plot0 + scale_x_continuous(xTitle, breaks=x_breaks, limits=x_limits)
    plot0  <- plot0 + scale_y_continuous(y_label)
    plot0  <- plot0 + scale_linetype_discrete("Variant")
    plot0  <- plot0 + scale_shape_discrete("Variant")
    plot0  <- plot0 + scale_color_discrete("Region")
    
  } ### End if(doPlot)
  
  ###### ** Adjust appearance ######
  plot0  <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=11))
  plot0  <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  plot0  <- plot0 + theme(axis.title.x  = element_text(hjust = 0.5, size=9))
  plot0  <- plot0 + theme(axis.title.y  = element_text(hjust = 0.5, size=9))
  plot0  <- plot0 + theme(legend.position = "bottom")

  if(do_slr){plot0 <- plot0 + theme(axis.text.x = element_text(angle=90))}
  
  ###### Control Guides
  plot0  <- plot0 + theme(legend.position = "bottom")
  
  ###### ** Add themes & margins ######
  ### Theme
  if(hasTheme  ){
    if(theme=="bw"){plot0 <- plot0 + theme_bw()}
    else           {plot0 <- plot0 + theme0}
  } ### End if(hasTheme  )
  ### Margins
  if(hasMargins){
    plot0 <- plot0 + theme(plot.margin = margin(
      t = margins[1],  # Top margin
      r = margins[2],  # Right margin
      b = margins[3],  # Bottom margin
      l = margins[4],  # Left margin
      unit = mUnit
    ))
  } ### End if(hasMargins)
  
  ###### Format Legend ######
  ### Add guide to legend
  nLgdCols  <- 7
  # nLgdCols  <- nRegions
  # nLgdCols |> print()
  plot0  <- plot0 + guides(linetype        = guide_legend(ncol=nLgdCols, order = 1))
  plot0  <- plot0 + guides(color           = guide_legend(ncol=nLgdCols, order = 2))
  plot0  <- plot0 + theme(legend.box.just  = "left")
  plot0  <- plot0 + theme(legend.box       = "vertical")
  # plot0     <- plot0 + theme(legend.direction = "vertical")
  plot0  <- plot0 + theme(legend.position = "bottom")
  plot0  <- plot0 + theme(legend.title     = element_text(size=10))
  plot0  <- plot0 + theme(legend.text      = element_text(size=9 ))
  plot0  <- plot0 + theme(legend.spacing.y = unit(0.05, "cm"))
  # refPlot0  <- refPlot0 + theme(legend.box.margin = margin(t=0.05, r=0.05, b=0.05, l=0.05, unit='cm'))
  
  
  ###### Return ######
  ### Return the plot
  if(print_msg){ message("...Finished.")}
  return(plot0)
}



### This function plots degrees of warming by sector, variant, impact year, and type
create_scaledImpact_plots <- function(
    data,
    sector,
    xCol      = "driverValue",
    yCol      = "scaled_impacts",
    colorCol  = "model",
    modelType = "GCM",
    # byState   = FALSE, 
    nTicks    = 5,
    silent    = TRUE,
    options   = list(
      # title      = "Scaled Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Scaled Impacts",
      lgdTitle   = "Model",
      nameBreak  = 18, ### Sector name break
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Messaging ######
  print_msg <- !silent
  if(print_msg){ "Running create_scaledImpact_plots()..." |> message()}
  ### Model Type
  # modelType %>% print()
  do_gcm    <- "gcm" %in% (modelType |> tolower())
  do_slr    <- "slr" %in% (modelType |> tolower())
  # modelType |> print(); do_gcm |> print(); do_slr |> print()
  
  ###### Get from FrEDI Namespace ######
  # get_colScale <- utils::getFromNamespace("get_colScale", "FrEDI")
  
  ###### Format Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); modelType |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(model_type == modelType)
  df0       <- df0 |> filter(sector     == sector0  )
  # df0 |> glimpse()
  
  ###### By State ######
  byState    <- df0[["state"]] |> unique() |> (function(x){!("N/A" %in% x)})()
  stateCols <- c("state", "postal")
  # if(byState){stateCols <- c("state", "postal")} else{stateCols <- c()}
  
  ###### Plot Options ######
  ### Defaults
  # def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by GMSL (cm)")
  # def_xTitles <- list(GCM=expression("Degrees of Warming (°C)") , SLR="GMSL (cm)")
  def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by Year")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)") , SLR="Year")
  def_lgdLbls <- list(GCM="Model", SLR="Scenario")
  def_margins <- list(GCM=c(0, 0, .15, 0), SLR=c(0, .2, .15, 0))
  ### Defaults: Default Heights Below
  def_title   <- do_gcm |> ifelse(def_titles [["GCM"]], def_titles [["SLR"]])
  def_xTitle  <- do_gcm |> ifelse(def_xTitles[["GCM"]], def_xTitles[["SLR"]])
  def_margin  <- do_gcm |> ifelse(def_margins[["GCM"]], def_margins[["SLR"]])
  def_lgdLbl  <- do_gcm |> ifelse(def_lgdLbls[["GCM"]], def_lgdLbls[["SLR"]])
  # def_colCol  <- "model"
  def_lgdPos  <- "top"
  def_yTitle  <- "Scaled Impacts"
  def_mUnit   <- "cm"
  def_theme   <- NULL
  def_nameBrk <- 18
  ### Values
  title0      <- options[["title"     ]]
  xTitle      <- options[["xTitle"    ]]
  yTitle      <- options[["yTitle"    ]]
  lgdLbl      <- options[["lgdTitle"  ]]
  lgdPos      <- options[["lgdPos"    ]]
  heights     <- options[["heights"   ]]
  margins     <- options[["margins"   ]]
  mUnit       <- options[["marginUnit"]]
  theme0      <- options[["theme"     ]]
  nameBrk     <- options[["nameBreak" ]]
  # xTitle |> print()
  ### Plot options
  hasTitle    <- !(is.null(title0 ))
  hasXTitle   <- !(is.null(xTitle ))
  hasYTitle   <- !(is.null(yTitle ))
  hasLgdLbl   <- !(is.null(lgdLbl ))
  hasLgdPos   <- !(is.null(lgdPos ))
  hasHeights  <- !(is.null(heights))
  hasMargins  <- !(is.null(margins))
  hasMUnits   <- !(is.null(mUnit  ))
  hasTheme    <- !(is.null(theme0 ))
  hasNameBrk  <- !(is.null(nameBrk))
  ### Values: Height Values Below
  if(!hasTitle  ){title0  <- def_title  }
  if(!hasXTitle ){xTitle  <- def_xTitle }
  if(!hasYTitle ){yTitle  <- def_yTitle }
  if(!hasLgdLbl ){lgdLbl  <- def_lgdLbl }
  if(!hasMargins){margins <- def_margin }
  if(!hasMUnits ){mUnit   <- def_mUnit  }
  if(!hasTheme  ){theme0  <- def_theme  }
  if(!hasNameBrk){nameBrk <- def_nameBrk}
  # title0 |> print(); def_xTitle |> print()
  # xTitle |> print()
  ### Update plot options
  plotOpts0    <- list(
    title      = title0,
    xTitle     = xTitle,
    yTitle     = yTitle,
    lgdTitle   = lgdLbl,
    margins    = margins,
    marginUnit = mUnit,
    theme      = theme0
  )
  
  ###### Get Sector Info ######
  # infoList0     <- df0 |> get_region_plotInfo(yCol=yCol, byState=byState, silent=silent)
  infoList0     <- df0 |> get_region_plotInfo(yCol=yCol, silent=silent)
  df_info       <- infoList0[["sectorInfo"]]
  df_minMax     <- infoList0[["minMax"    ]]
  df_iter       <- infoList0[["df_iter"   ]]
  nCol          <- infoList0[["nCol"      ]]
  nRow          <- infoList0[["nRow"      ]]
  # nCol |> c(nRow) |> print()
  ### Unique values
  cSectors      <- infoList0[["cSectors" ]]
  cVariants     <- infoList0[["cVariants"]]
  cImpTypes     <- infoList0[["cImpTypes"]]
  cImpYears     <- infoList0[["cImpYears"]]
  cRegions      <- infoList0[["cRegions" ]]
  cStates       <- infoList0[["cStates"  ]]
  ### Numbers
  nSectors      <- cSectors  |> length()
  nVariants     <- cVariants |> length()
  nImpTypes     <- cImpTypes |> length()
  nImpYears     <- cImpYears |> length()
  # nRegions      <- cRegions  |> length()
  nStates       <- cStates   |> length()
  # cSectors |> print(); cVariants |> print(); cImpTypes |> print(); cImpYears |> print(); cStates |> head() |> print(); cModels |> print();
  # c(nSectors, nVariants, nImpTypes, nImpYears, nRegions, nModels, nStates) |> print()
  
  ### Add maxUnitValue to df_iter
  # join0         <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols) |> c("model")
  # df_iter
  
  ###### Drop Data ######
  ### Join iteration with data and drop models
  # join0         <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols) |> c("model")
  join0         <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols) |> c("model")
  join0         <- join0 |> c("maxUnitValue")
  # if(byState){join0 <- join0 |> c("state", "postal")}
  df0           <- df0 |> left_join(df_info, by=c(join0))
  df0           <- df0 |> filter(!is.na(nObs))
  # cModels       <- df0[["model" ]] |> unique()
  # cRegions      <- df0[["region"]] |> unique()
  cModels       <- df_info[["model" ]] |> unique()
  cRegions      <- df_info[["region"]] |> unique()
  nModels       <- cModels   |> length()
  nRegions      <- cRegions  |> length()
  rm(join0)
  # nRegions |> print()
  # df0[["region"]] |> unique() |> print()
  # df0           <- df0 |> filter(sector     %in% cSectors)
  # df0           <- df0 |> filter(variant    %in% cVariants)
  # df0           <- df0 |> filter(impactType %in% cImpTypes)
  # df0           <- df0 |> filter(impactYear %in% cImpYears)
  df0           <- df0 |> filter(region     %in% cRegions)
  # df0           <- df0 |> filter(model      %in% cModels)
  
  
  ###### Factor Model ######
  for(i in df_iter |> row_number()){
    col_i    <- df_iter[["column"]][i]
    cCol_i   <- df_iter[["cName" ]][i]
    levels_i <- infoList0[[cCol_i]]
    df0      <- df0 |> mutate_at(c(col_i), factor, levels=levels_i)
    rm(i, col_i, cCol_i, levels_i)
  } ### End for(i in df_iter |> row_number())
  
  ### Factor models
  if(do_slr){
    labs0   <- c(30, 50, 100, 150, 200, 250) |> paste0(" cm")
    # lvls0   <- 1:(labs0 |> length())
    df0     <- df0 |> mutate(model = model |> factor(levels=labs0))
  } ### End if(do_slr)
  
  
  ### Convert maxUnitValue to numeric
  df0     <- df0 |> mutate_at(c("maxUnitValue"), as.character)
  df0     <- df0 |> mutate_at(c("maxUnitValue"), as.numeric  )
  
  ###### Plot Title Info ######
  ### Default for now
  # x_denom <- y_denom <- 1
  
  ###### ** X Breaks ######
  if(xCol == "year"){
    x_limits <- c(2000, 2100)
    x_breaks <- seq(x_limits[1] - 10, x_limits[2] + 10, by = 20)
    x_denom  <- 1
    x_info   <- NULL
    # x_info   <- list()
    # x_info[["denom" ]] <- x_denom
    # x_info[["breaks"]] <- x_breaks
    # x_info[["limits"]] <- x_limits
  } ### End if(xCol == "year")
  else              {
    x_limits <- c(-1, 11)
    x_breaks <- seq(0, 10, by=2)
    x_denom  <- 1
    x_info   <- NULL
  } ### End else(xCol == "year")
  
  ###### ** Y-Breaks ######
  y_info     <- infoList0[["minMax"]] #|> filter(plotRow == row0)
  y_info     <- y_info |> mutate(sector=sector0)
  y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
  # y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
  ### Additional info
  y_scale    <- y_info[["scale" ]]
  # y_scale |> names() |> print()
  y_p10      <- y_info[["p10"   ]]
  y_denom    <- y_info[["denom" ]]
  y_breaks   <- y_info[["breaks"]]
  y_limits   <- y_info[["limits"]]
  y_label    <- y_info[["label" ]]
  # ### Labeling
  y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  y_label    <- y_label #|> paste0(y_prelabel, "$2015")
  y_label    <- "Scaled Impacts (" |> paste0(y_label, ")")
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()
  
  
  ###### State vs. Region Options ######
  ### What to iterate over
  ### Number of iteration values
  if(byState){cIter1 <- cRegions} else{cIter1 <- "All"} ### End else
  nIter1 <- cIter1 |> length()
  # nIter2 <- cIter2 |> length()
  
  # ###### Reference Plot ######
  # ### Reference plots
  # refPlot0   <- df0 |> create_scaledImpact_plot(
  #   sector0   = sector0,
  #   # variant0  = cVariants[1],
  #   impType0  = cImpTypes[1],
  #   impYear0  = cImpYears[1],
  #   region0   = cRegions[1],
  #   byState   = byState, 
  #   infoList0 = infoList0, ### Dataframe with sector info...output from get_region_plotInfo
  #   xCol      = xCol,   ### X-Column,
  #   yCol      = yCol,   ### Y-Column,
  #   xInfo     = x_info, ### xScale...outputs of get_colScale
  #   colorCol  = colorCol,
  #   refPlot   = TRUE, ### Whether to do a ref plot
  #   silent    = silent,
  #   options   = plotOpts0
  # )
  # # refPlot0 |> print()
  # 
  # ###### Legend & Spacer #####
  # ### Add guide to legend
  # # nLgdCols  <- 4
  # nLgdCols  <- nRegions
  # # nLgdCols |> print()
  # refPlot0  <- refPlot0 + guides(linetype        = guide_legend(ncol=nLgdCols, order = 1))
  # refPlot0  <- refPlot0 + guides(color           = guide_legend(ncol=nLgdCols, order = 2))
  # refPlot0  <- refPlot0 + theme(legend.box.just  = "left")
  # refPlot0  <- refPlot0 + theme(legend.title     = element_text(size=10))
  # refPlot0  <- refPlot0 + theme(legend.text      = element_text(size=9))
  # refPlot0  <- refPlot0 + theme(legend.spacing.y = unit(0.05, "cm"))
  # # refPlot0  <- refPlot0 + theme(legend.box.margin = margin(t=0.05, r=0.05, b=0.05, l=0.05, unit='cm'))
  # 
  # ###### Common Plot Elements ######
  nLgdCols  <- nRegions
  spacer0   <- ggplot() + theme_void()
  # legend0   <- refPlot0 |> ggpubr::get_legend()
  # # # "got here..." |> print()
  # grobLgd0  <- ggarrange(plotlist=list(legend=legend0))
  
  ###### Create Plot List ######
  ### Iterate over Impact Years
  # cIter1 |> print()
  listIter1 <- cIter1 |> map(function(iter1_i){
    listYears0 <- cImpYears |> map(function(impYear_j){
      listTypes_j <- cImpTypes |> map(function(impType_k){
        # iter1_i |> print(); impYear_j |> print(); impType_k |> print(); 
        ### Figure out min/max across all variants for an impact type to get the y-scale
        region_k  <- iter1_i
        # "\t\t" |> paste0(c(iter1_i, impYear_j, impType_k, region_k) |> paste(collapse=", ")) |> message()
        ###### Create the plot ######
        plot_k    <- df0 |> create_scaledImpact_plot(
          sector0   = sector0,
          impType0  = impType_k,
          impYear0  = impYear_j,
          region0   = region_k,
          # byState   = byState,
          infoList0 = infoList0, ### Dataframe with sector info...output from get_region_plotInfo
          xCol      = xCol,   ### X-Column,
          yCol      = yCol,   ### Y-Column,
          colorCol  = colorCol,
          xInfo     = x_info, ### xScale...outputs of get_colScale
          refPlot   = FALSE, ### Whether to do a ref plot
          silent    = silent,
          options   = plotOpts0
        )
        # plot_k |> print()
        
        ###### Annotate Plots ######
        ### Labels on top
        ### Longest impact type: "Acute Myocardial Infarction"
        typeTitle_k <- "Impact Type: " |> paste0(impType_k)
        grobType_k  <- text_grob(typeTitle_k, face="italic", size=11)
        plotGrid_k  <- plot_k
        plotList_k  <- list(spacer1=spacer0, plots=plotGrid_k, spacer2=spacer0)
        # plotGrid_k  <- ggarrange(plotlist=plotList_k, nrow=3, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1, 0.01))
        # plotGrid_k  <- ggarrange(plotlist=plotList_k, nrow=3, ncol=1, common.legend=T, legend="bottom", heights=c(0.01, 1, 0.1))
        plotGrid_k  <- ggarrange(plotlist=plotList_k, nrow=3, ncol=1, heights=c(0.01, 1, 0.1))
        plotGrid_k  <- plotGrid_k |> annotate_figure(top=grobType_k)
        
        ### Return
        return(plotGrid_k)
      })
      
      ### Name the plots
      # listTypes_j |> length() |> print(); cImpTypes |> print()
      listTypes_j <- listTypes_j |> set_names(cImpTypes)
      
      ### Arrange plot list
      # plotGrid_j  <- ggarrange(plotlist=listTypes_j, ncol=1, nrow=nRow, common.legend=F, legend="none")
      plotGrid_j  <- ggarrange(plotlist=listTypes_j, ncol=1, nrow=nRow, common.legend=F)
      
      ### Add spacer to the top
      # "got here2..." |> print()
      yearTitle_j <- "Impact Year: " |> paste0(impYear_j)
      grobYear_j  <- text_grob(yearTitle_j, face="plain", size=13)
      plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j)
      plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=2, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1))
      plotGrid_j  <- plotGrid_j |> annotate_figure(top=grobYear_j)
      
      ### Add Plot Title & Y Title
      # plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j, spacer2=spacer0, legend=grobLgd0)
      # plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j, legend=grobLgd0)
      plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j, spacer2=spacer0)
      # nModels |> print()
      modMult     <- (nModels - 1) %/% nLgdCols + 1
      # plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=4, ncol=1, legend="none", heights=c(0.01, nImpTypes, 0.01, 0.2))
      # plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=4, ncol=1, legend="none", heights=c(0.01, nImpTypes, 0.01, 0.2 * modMult))
      # plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=3, ncol=1, legend="none", heights=c(0.01, nImpTypes, 0.2))
      plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=3, ncol=1, legend="none", heights=c(0.01, nImpTypes, 0.01))
      title0_j    <- sector0
      grobTit_j   <- text_grob(title0_j, color="black", size = 14, face="bold", hjust=0.5)
      plotGrid_j  <- plotGrid_j |> annotate_figure(top=grobTit_j)
      
      plotYTit_j  <- text_grob(yTitle, color = "black", rot  = 90)
      plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j, spacer2=spacer0)
      plotGrid_j  <- ggarrange(plotlist=plotList_j, nrow=1, legend="none", widths=c(0.01, nRegions, 0.01))
      plotGrid_j  <- plotGrid_j |> annotate_figure(left=plotYTit_j)
      # return(plotGrid_j)
      
      ###### Return Impact Type Plot ######
      return(plotGrid_j)
    })
    ### Name the plots
    listYears0 <- listYears0 |> set_names(cImpYears)
    return(listYears0)
  }) ### End iter1_i
  ### Name the plots
  listIter1 <- listIter1 |> set_names(cIter1)
  
  ###### Return ######
  ### Return the plot
  if(print_msg) message("Finished.")
  return(listIter1)
}

