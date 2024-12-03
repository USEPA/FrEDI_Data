### This function plots degrees of warming by sector, variant, impact year, and type
create_scaledImpact_plot <- function(
    data,
    sector0,
    impType0,
    impYear0,
    region0,   ### Region or state
    infoList0, ### Dataframe with sector info...output from get_region_plotInfo
    xCol     = "driverValue"   , ### X-Column,
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
  print_msg  <- !silent
  if(print_msg) "Running create_scaledImpact_plot()..." |> message()
  
  ###### Get from FrEDI Namespace ######
  # get_colScale <- utils::getFromNamespace("get_colScale", "FrEDI")
  
  ###### Data ######
  df0        <- data  |> filter(sector     == sector0 )
  df0        <- df0   |> filter(impactType == impType0)
  df0        <- df0   |> filter(impactYear == impYear0)
  df0        <- df0   |> filter(region     == region0 )
  type0      <- df0   |> pull(modelType) |> unique()
  typeLC0    <- type0 |> tolower()
  # df0 |> glimpse()
  
  ###### Values ######
  ###### ** By State ######
  # byState    <- df0 |> pull(state) |> unique() |> (function(x){!("N/A" %in% x)})()
  byState    <- (df0 |> filter(!(state %in% "N/A")) |> nrow()) & (df0 |> nrow())
  stateCols  <- c("state", "postal")
  if(byState) df0 <- df0 |> filter(region == region0)
  
  ###### ** Model Types ######
  # type0 |> print()
  do_gcm     <- "gcm" %in% typeLC0
  do_slr     <- "slr" %in% typeLC0
  
  ###### Plot Setup ######
  ###### ** Sector Info ######
  ### Filter to sector info
  info0      <- infoList0[["sectorInfo"]]
  info0      <- info0 |> filter(sector     == sector0 )
  info0      <- info0 |> filter(impactType == impType0)
  info0      <- info0 |> filter(impactYear == impYear0)
  info0      <- info0 |> filter(region     == region0 )
  
  ### Get sector values
  # index0     <- info0 |> pull(sector_order) |> unique() |> first()
  index0     <- info0 |> pull(order_sector) |> unique() |> first()
  row0       <- info0 |> pull(plotRow     ) |> unique() |> first()
  col0       <- info0 |> pull(plotCol     ) |> unique() |> first()
  
  ###### ** Breaks info ######
  ###### ** -- X Breaks ######
  do_xInfo   <- xInfo |> is.null()
  if(do_xInfo) {
    xInfo <- xInfo |> getXAxisScale(
      xCol    = xCol,
      maxYear = 2100,
      yrUnit  = 20
    ) ### End getXAxisScale
  } ### End if(do_xInfo)
  ### Assign to objects
  x_limits   <- xInfo[["limits"]]
  x_breaks   <- xInfo[["breaks"]]
  x_denom    <- xInfo[["denom" ]]
  
  ###### ** -- Y-Breaks ######
  ### Filter to y columns info
  y_info     <- infoList0[["minMax"]]
  # y_info     <- y_info |> filter(plotRow == row0)
  # y_info     <- y_info |> mutate(sector = sector0)
  y_info     <- y_info |> filter(sector     == sector0 )
  y_info     <- y_info |> filter(impactType == impType0)
  y_info     <- y_info |> filter(impactYear == impYear0)
  y_info     <- y_info |> filter(region     == region0 )
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
  y_label    <- (y_label == "") |> ifelse("Scaled Impacts", paste0("(", y_label, ")"))
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()
  
  ###### ** Mutate Data ######
  # "got here" |> print(); x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom
  # x_limits |> print(); df0[[xCol]] |> range(na.rm=T) |> print()
  # y_limits |> print(); df0[[yCol]] |> range(na.rm=T) |> print()
  # df0[df0[[yCol]] |> is.na(),] |> nrow() |> print()
  # df0 |> glimpse()
  
  ###### ** Plot Options ######
  ### Values
  plotOpts0   <- typeLC0 |> get_scaledImpactPlotTitles(options=options)
  title0      <- plotOpts0[["title"     ]]
  xTitle      <- plotOpts0[["xTitle"    ]]
  yTitle      <- plotOpts0[["yTitle"    ]]
  lgdLbl      <- plotOpts0[["lgdTitle"  ]]
  lgdPos      <- plotOpts0[["lgdPos"    ]]
  heights     <- plotOpts0[["heights"   ]]
  margins     <- plotOpts0[["margins"   ]]
  mUnit       <- plotOpts0[["marginUnit"]]
  theme0      <- plotOpts0[["theme"     ]]
  ### Conditionals
  hasLgdPos   <- !(lgdPos  |> is.null())
  hasMargins  <- !(margins |> is.null())
  hasTheme    <- !(theme0  |> is.null())
  # xTitle |> print()
  
  ###### Standardize column names ######
  title0      <- "Region: " |> paste0(region0)
  # xCol |> c(yCol) |> print()
  
  ###### Create the plot ######
  ### Group values
  # groups0     <- c("sector", "variant", "impactType", "impactYear", "region", "model", "maxUnitValue")
  # # groups0  <- groups0 |> c(xCol)
  ### Legend labels and facet col
  facetCol    <- byState |> ifelse("state", "region") 
  colorLbl    <- facetCol |> str_to_title()
  shapeLbl    <- "Variant"
  ### Other columns
  if(byState) {
    stateCol0 <- "state"
    regCol0   <- "state"
  } else {
    # groups0   <- groups0
    stateCol0 <- c()
    regCol0   <- "region"
  } ### End if(byState)
  ### Groups & Faceting column
  # groups0     <- c("sector", "variant", "impactType", "impactYear", "region", stateCol0, "model", "maxUnitValue") |> unique()
  groups0     <- c("sector", "variant", "impactType", "impactYear", "region", stateCol0, "model") |> unique()
  # groups0  <- groups0 |> c(xCol)
  facetCol <- regCol0
  ### Group values
  df0         <- df0 |> group_by_at(c(groups0))
  # rm(groups0)
  
  ### Points dataframe
  # if(do_slr) df_points0 <- df0 |> filter(year %in% x_breaks)
  # else       df_points0 <- df0
  if(do_gcm) {
    # "Do GCM" |> print()
    ### Plot these values as lines
    df0_1 <- df0 |> filter((maxUnitValue < 6 & driverValue <= maxUnitValue) | maxUnitValue >= 6) 
    ### Plot these values as points
    df0_2 <- df0 |> filter((maxUnitValue < 6 & driverValue >= maxUnitValue))
  } else if(do_slr) {
    # "Do SLR" |> print()
    ### Filter to values
    df0   <- df0 |> filter(year >= x_limits[1], year <= x_limits[2])
    df0   <- df0 |> filter(!(model %in% c("0cm", "0 cm", NA)))
    ### Plot these values as lines
    df0_1 <- df0
    # return(df0)
    ### Plot these values as points
    df0_2 <- df0 |> filter(year %in% x_breaks)
  } ### End if(do_gcm)
  # df0_1 |> dim() |> print()
  # df0_2 |> dim() |> print()
  
  ###### ** Initialize plot
  ### Initialize plot
  plot0       <- ggplot()
  
  ### Check if the plot needs to be made
  allNA       <- df0 |> pull(yCol) |> is.na() |> all()
  # df0 |> nrow() |> print(); x_breaks |> print(); x_limits |> print()
  doPlot      <- !allNA
  if(doPlot) {
    # x_limits |> print(); df0[[xCol]] |> range(na.rm=T) |> print()
    # y_limits |> print(); df0[[yCol]] |> range(na.rm=T) |> print()
    
    ### Plot values as lines
    plot0  <- plot0 + geom_line(
      data  = df0_1,
      alpha = 0.65,
      aes(
        x        = .data[[xCol]], 
        y        = .data[[yCol]], 
        color    = .data[[regCol0]],
        linetype = .data[["variant"]],
        group    = interaction(!!!syms(groups0))
      ) ### End aes
    ) ### End geom_line
    # "got here1" |> print()
    # return(plot0)
    
    ### Plot values as points
    plot0  <- plot0 + geom_point(
      data  = df0_2,
      alpha = 0.65, 
      aes(
        x     = .data[[xCol]], 
        y     = .data[[yCol]], 
        color = .data[[regCol0]], 
        shape = .data[["variant"]], 
        group = interaction(!!!syms(groups0))
      ) ### End aes
    ) ### End geom_line
    # "got here2" |> print()
    
    ###### ** Add facet_grid
    plot0  <- plot0 + facet_grid(model~.data[[regCol0]])
    # plot0 |> print()
    
    ###### ** Add and title
    plot0  <- plot0 + ggtitle(title0)
    
    ###### ** Add scales
    plot0  <- plot0 + scale_x_continuous(xTitle, breaks=x_breaks, limits=x_limits)
    plot0  <- plot0 + scale_y_continuous(y_label)
    plot0  <- plot0 + scale_color_discrete(colorLbl)
    plot0  <- plot0 + scale_shape_discrete(shapeLbl)
    plot0  <- plot0 + scale_linetype_discrete(shapeLbl)
    
    ###### ** Adjust legend title
    if(hasLgdPos) plot0 <- plot0 + guides(color=guide_legend(title.position=lgdPos))
    plot0  <- plot0 + theme(legend.direction="vertical", legend.box="vertical")
    
  } ### End if(doPlot)
  
  ###### ** Adjust appearance ######
  plot0  <- plot0 + theme(plot.title    = element_text(hjust=0.5, size=11))
  plot0  <- plot0 + theme(plot.subtitle = element_text(hjust=0.5, size=10))
  plot0  <- plot0 + theme(axis.title.x  = element_text(hjust=0.5, size=9 ))
  plot0  <- plot0 + theme(axis.title.y  = element_text(hjust=0.5, size=9 ))
  
  if(do_slr) plot0 <- plot0 + theme(axis.text.x = element_text(angle=90))
  
  ###### Control Guides
  plot0  <- plot0 + theme(legend.position = "bottom")
  
  ###### ** Add themes & margins ######
  ### Theme
  if(hasTheme){
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
  plot0  <- plot0 + theme(legend.position  = "bottom")
  plot0  <- plot0 + theme(legend.title     = element_text(size=10))
  plot0  <- plot0 + theme(legend.text      = element_text(size=9 ))
  plot0  <- plot0 + theme(legend.spacing.y = unit(0.05, "cm"))
  # refPlot0  <- refPlot0 + theme(legend.box.margin = margin(t=0.05, r=0.05, b=0.05, l=0.05, unit='cm'))
  # "got here3" |> print()
  
  ###### Return ######
  ### Return the plot
  if(print_msg){ message("...Finished.")}
  return(plot0)
}