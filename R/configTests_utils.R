## Function to check if column has at least one non NA value
has_nonNA_values_df <- function(x) {
  ### Check whether values in x are NA
  x <- x |> is.na()
  ### Calculate number of rows
  y <- tibble(numRows = x |> nrow())
  ### Number of NA values
  y <- y |> mutate(numNA = x |> colSums() |> nrow() |> is.null() |> if_else(0,1))
  ### Whether all results are missing
  y <- y |> mutate(allNA = (numRows == numNA))
  ### Filter to values with allNA
  y <- y |> filter(allNA)
  ### Get number of rows %>%
  z <- y |> nrow()
  # z <- 1 * (z > 0)
  ### Return
  return(z)
} ### End has_nonNA_values_df

### Function to check non NA values for other types
has_nonNA_values_misc <- function(x) {
  class_x <- x |> class()
  len_x   <- x |> length()
  isList0 <- "list" %in% class_x
  if(isList0) {
    names_x    <- x |> names()
    hasNames_x <- !(names_x |> is.null())
    if(hasNames_x) {y <- names_x |> map(~ x[[.]] |> class()) |> unlist()}
    else           {y <- x       |> map(~ . |> class()) |> unlist()}
    skip_y <- ("function" %in% y) | ("list" %in% y)
    # skip_y |> print()
    ### If y has functions
    if(skip_y) {y <- FALSE}
    else  {
      y0 <- x |> map(~ . |> unlist() |> is.na()) |> unlist()
      # y0 |> head |> print()
      y0 <- y0 |> all(na.rm=TRUE)
      y1 <- x |> map(~ .y |> length()) |> unlist()
      y  <- y0 & (y1 > 1)
    } ### End else(skip_y)
  } ### End if(isList0) 
  else        {y <- x |> is.na() |> all(na.rm=TRUE)}
  ### Which observations
  which_x <- y |> which()
  ### Count NA values
  z       <- y |> which() |> length()
  ### Return
  return(z)
} ### End has_nonNA_values_misc

### For iterating over list names
### Calculating number of columns
fun_nCol <- function(z, a, b){
  # z |> print()
  # if_else("data.frame" %in% a[[z]], b[[z]] |> ncol(), 0 |> as.integer())
  val1 <- "data.frame" %in% a[[z]]
  val2 <- b[[z]] |> ncol()
  val3 <- 0    |> as.integer()
  val2 <- val2 |> is.null() |> ifelse(val3, val2)
  # val1 |> print(); val2 |> print(); val3 |> print()
  y <- if_else(val1, true=val2, false=val3)
  return(y)
} ### End fun_nCol

### Calculating number of rows
fun_nRow <- function(z, a, b){
  # z |> print()
  # if_else("data.frame" %in% a[[z]], b[[z]] |> nrow(), b[[z]] |> length())
  val1 <- "data.frame" %in% a[[z]]
  val2 <- b[[z]] |> nrow()
  val3 <- b[[z]] |> length()
  val2 <- val2   |> is.null() |> ifelse(val3, val2)
  # val1 |> print(); val2 |> print(); val3 |> print()
  y    <- if_else(val1, true=val2, false=val3)
  return(y)
} ### End fun_nRow

### Calculating distinct rows or values
fun_nUnq <- function(z, a, b){
  ### Objects
  class_z <- a[[z]]
  obj_z   <- b[[z]]
  do_df0  <- "data.frame" %in% class_z
  ### Values
  val1    <- do_df0
  ### What to do for data frames
  if(do_df0) {val2 <- obj_z |> distinct() |> nrow()}
  else       {val2 <- obj_z |> unique() |> length()}
  # val1 |> print(); val2 |> print(); val3 |> print()
  val3    <- val2
  y       <- if_else(val1, true=val2, false=val3)
  return(y)
} ### End fun_nUnq

### Calculating columns with all NA vales
fun_nNna <- function(z, a, b){
  ### Values
  val1 <- "data.frame" %in% a[[z]]
  ### What to do for data frames
  if(val1) {val2 <- b[[z]] |> has_nonNA_values_df()  }
  else     {val2 <- b[[z]] |> has_nonNA_values_misc()}
  ### Other values
  val3 <- val2
  y    <- if_else(val1, val2, val3)
  return(y)
} ### End fun_nNna

###### get_region_plotInfo ######
### Get region plot info (for scaled impact plots)
get_region_plotInfo <- function(
    df0, ### Data
    yCol      = "scaled_impacts",
    byState   = FALSE,
    groupCols = c("sector", "variant", "impactType", "impactYear", "region", "model"),
    nCol      = 4,
    silent    = TRUE
){
  ###### Initialize Return List ######
  list0  <- list()
  
  ###### Grouping Columns ######
  ### Add state to grouping
  if(byState){
    group0    <- c("state", "postal", "model")
    groupCols <- groupCols[!(groupCols %in% group0)]
    groupCols <- groupCols |> c(group0)
    rm(group0)
  } ### End if(byType)
  
  ###### Get Value Ranges ######
  # df0 |> glimpse()
  df_sectorInfo <- df0 |> fun_limitsByGroup(
    sumCols   = yCol,
    groupCols = groupCols,
    silent    = silent
  )
  # df_sectorInfo |> print()
  
  ###### Number of NA Values ######
  ### Get number of observations in a group
  group0        <- groupCols
  df_na         <- df0 |> 
    mutate_at(.vars=c(yCol), is.na) |>
    rename_at(.vars=c(yCol), ~c("nNA")) |>
    mutate(nObs = 1) |>
    group_by_at(.vars=c(groupCols)) |> 
    summarize_at(.vars=c("nObs", "nNA"), sum) |> ungroup()
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
  df_iter    <- tibble(column = cCols) |>
    mutate(colSuffix = case_when(
      column == "impactType" ~ "ImpTypes",
      column == "impactYear" ~ "ImpYears",
      .default = column |> str_to_title() |> paste0("s")
    )) |> 
    mutate(cName = "c" |> paste0(colSuffix)) |> 
    mutate(nName = "n" |> paste0(colSuffix)) |>
    mutate(factorCol = column  |> paste0("_factor")) |>
    mutate(orderCol  = "order_" |> paste0(column))
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
  colCol    <- byState |> ifelse("state", "region")
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
  df_minMax   <- df_minMax |> gather(key="summary_type", value = "summary_value", -c("plotRow"))
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
    byState  = FALSE,
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
  
  ###### Data ######
  df0        <- data |> filter(sector     == sector0)
  df0        <- df0  |> filter(impactType == impType0)
  df0        <- df0  |> filter(impactYear == impYear0)
  if(byState){df0 <- df0 |> filter(region == region0)} 
  type0      <- df0[["model_type"]] |> unique()
  # df0 |> glimpse()
  
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
      x_limits   <- c(2010, 2090)
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
  def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by GMSL (cm)")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)"), SLR="GMSL (cm)")
  def_lgdLbls <- list(GCM="Region", SLR="Year")
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
  title0 <- byState |> ifelse(state0, region0)
  title0 <- "Region: " |> paste0(title0)
  
  ###### Create the plot ######
  # colorCol |> print(); def_lgdLbl |> print()
  # plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[colorCol]]))
  ### Group values
  groups0  <- c("sector", "variant", "impactType", "impactYear", "region", "model", xCol)
  facetCol <- byState |> ifelse("state", "region") 
  if(byState){groups0 <- groups0 |> c("state")} 
  df0    <- df0 |> group_by_at(.vars=c(groups0))
  
  ### Points dataframe
  if(do_slr){df_points0 <- df0 |> filter(year %in% x_breaks)}
  else      {df_points0 <- df0}
  
  ### Plot
  # if(byState){
  #   plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[colorCol]], group=interaction(sector, variant, impactType, impactYear, region, state, model)))
  # } else{
  #   plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[colorCol]], group=interaction(sector, variant, impactType, impactYear, region, model)))
  # }
  # if(byState){
  #   plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[variant]], group=interaction(sector, variant, impactType, impactYear, region, state, model)))
  # } else{
  #   plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[[variant]], group=interaction(sector, variant, impactType, impactYear, region, model)))
  # }
  if(byState){
    plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["region"]], group=interaction(sector, variant, impactType, impactYear, region, state, model)))
  } else{
    plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["region"]], group=interaction(sector, variant, impactType, impactYear, region, model)))
  }
  
  ###### * Add Geoms ######
  plot0  <- plot0 + geom_line(aes(linetype = .data[["variant"]]), alpha=0.5)
  # plot0  <- plot0 + facet_grid(.~.data[[facetCol]])
  # plot0  <- plot0 + facet_grid(model~region)
  # plot0  <- plot0 + facet_grid(model~.data[[facetCol]])
  plot0  <- plot0 + facet_grid(model~.data[["region"]])
  
  ###### ** Adjust legend title ######
  if(hasLgdPos){plot0 <- plot0 + guides(color = guide_legend(title.position = lgdPos))}
  plot0  <- plot0 + theme(legend.direction = "vertical", legend.box = "vertical")
  
  ###### ** Add and title ######
  plot0  <- plot0 + ggtitle(title0)
  
  ###### ** Add scales ######
  plot0  <- plot0 + scale_x_continuous(xTitle, breaks=x_breaks, limits=x_limits)
  plot0  <- plot0 + scale_y_continuous(y_label)
  plot0  <- plot0 + scale_linetype_discrete("Variant")
  # plot0  <- plot0 + scale_color_discrete("Variant")
  # plot0  <- plot0 + scale_color_discrete(lgdLbl)
  plot0  <- plot0 + scale_color_discrete("Region")
  
  ###### ** Adjust Appearance ######
  plot0  <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=11))
  plot0  <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  plot0  <- plot0 + theme(axis.title.x  = element_text(hjust = 0.5, size=9))
  plot0  <- plot0 + theme(axis.title.y  = element_text(hjust = 0.5, size=9))
  plot0  <- plot0 + theme(legend.position = "bottom")
  
  ###### ** Add Themes & Margins ######
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
  
  ###### Legend ######
  ### Add guide to legend
  nLgdCols  <- 7
  # nLgdCols  <- nRegions
  # nLgdCols |> print()
  plot0  <- plot0 + guides(linetype        = guide_legend(ncol=nLgdCols, order = 1))
  plot0  <- plot0 + guides(color           = guide_legend(ncol=nLgdCols, order = 2))
  plot0  <- plot0 + theme(legend.box.just  = "left")
  plot0  <- plot0 + theme(legend.title     = element_text(size=10))
  plot0  <- plot0 + theme(legend.text      = element_text(size=9))
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
    byState   = FALSE, 
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
  
  ###### Format Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); modelType |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(model_type == modelType)
  df0       <- df0 |> filter(sector     == sector0  )
  # df0 |> glimpse()
  
  ###### Plot Options ######
  ### Defaults
  def_titles  <- list(GCM="Scaled Impacts by Degrees of Warming", SLR="Scaled Impacts by GMSL (cm)")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)"), SLR="GMSL (cm)")
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
  infoList0     <- df0 |> get_region_plotInfo(yCol=yCol, byState=byState, silent=silent)
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
  nRegions      <- cRegions  |> length()
  nStates       <- cStates   |> length()
  # cSectors |> print(); cVariants |> print(); cImpTypes |> print(); cImpYears |> print(); cStates |> head() |> print(); cModels |> print();
  # c(nSectors, nVariants, nImpTypes, nImpYears, nRegions, nModels, nStates) |> print()
  
  ###### Drop Data ######
  ### Join iteration with data and drop models
  join0         <- c("sector", "variant", "impactType", "impactYear", "region", "model")
  if(byState){join0 <- join0 |> c("state", "postal")}
  df0           <- df0 |> left_join(df_info, by=c(join0))
  df0           <- df0 |> filter(!is.na(nObs))
  cModels       <- df0[["model"]] |> unique()
  nModels       <- cModels   |> length()
  rm(join0)
  # nRegions |> print()
  # df0[["region"]] |> unique() |> print()
  # df0           <- df0 |> filter(sector     %in% cSectors)
  # df0           <- df0 |> filter(variant    %in% cVariants)
  # df0           <- df0 |> filter(impactType %in% cImpTypes)
  # df0           <- df0 |> filter(impactYear %in% cImpYears)
  # df0           <- df0 |> filter(region     %in% cRegions)
  # df0           <- df0 |> filter(model      %in% cModels)
  
  
  ###### Factor Model ######
  for(i in df_iter |> row_number()){
    col_i    <- df_iter[["column"]][i]
    cCol_i   <- df_iter[["cName" ]][i]
    levels_i <- infoList0[[cCol_i]]
    df0      <- df0 |> mutate_at(.vars=c(col_i), factor, levels=levels_i)
    rm(i, col_i, cCol_i, levels_i)
  } ### End for(i in df_iter |> row_number())
  
  ###### Plot Title Info ######
  ### Default for now
  # x_denom <- y_denom <- 1
  
  ###### ** X Breaks ######
  if(xCol == "year"){
    x_limits <- c(2010, 2090)
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
  if(byState){cIter1 <- cRegions} 
  else{cIter1 <- "All"} ### End else
  ### Number of iteration values
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
  listIter1 <- cIter1 |> map(function(iter1_i){
    listYears0 <- cImpYears |> map(function(impYear_j){
      listTypes_j <- cImpTypes |> map(function(impType_k){
        ### Figure out min/max across all variants for an impact type to get the y-scale
        region_k  <- iter1_i
        ###### Create the plot ######
        plot_k    <- df0 |> create_scaledImpact_plot(
          sector0   = sector0,
          impType0  = impType_k,
          impYear0  = impYear_j,
          region0   = region_k,
          byState   = byState,
          infoList0 = infoList0, ### Dataframe with sector info...output from get_region_plotInfo
          xCol      = xCol,   ### X-Column,
          yCol      = yCol,   ### Y-Column,
          colorCol  = colorCol,
          xInfo     = x_info, ### xScale...outputs of get_colScale
          refPlot   = FALSE, ### Whether to do a ref plot
          silent    = silent,
          options   = plotOpts0
        )
        
        
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
      listTypes_j <- listTypes_j |> addListNames(cImpTypes)
      
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
    listYears0 <- listYears0 |> addListNames(cImpYears)
    return(listYears0)
  }) ### End iter1_i
  ### Name the plots
  listIter1 <- listIter1 |> addListNames(cIter1)
  
  ###### Return ######
  ### Return the plot
  if(print_msg) message("Finished.")
  return(listIter1)
}

