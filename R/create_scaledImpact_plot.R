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
  type0      <- df0[["modelType"]] |> unique()
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
  do_xInfo   <- xInfo |> is.null()
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
  y_label    <- (y_label=="") |> ifelse("Scaled Impacts", paste0("(", y_label, ")"))
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()
  
  ###### Mutate Data ######
  # "got here" |> print(); x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom
  
  
  ###### Plot Options ######
  ###### Defaults ######
  ### Defaults
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
  title0 <- "Region: " |> paste0(region0)
  
  ###### Create the plot ######
  ### Group values
  # groups0  <- c("sector", "variant", "impactType", "impactYear", "region", "model", "maxUnitValue", xCol)
  groups0  <- c("sector", "variant", "impactType", "impactYear", "region", "model", "maxUnitValue")
  # xCol |> c(yCol) |> print()
  # groups0  <- c("sector", "variant", "impactType", "impactYear", "region", "model", "maxUnitValue")
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
    group0 <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCol0) |> c("model") |> unique()
    # group0 |> print(); df0 |> glimpse(); xCol |> c(yCol) |> print()
    # group0  <- groups0 |> c(stateCol0) |> c("model") |> unique()
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
          x        = .data[[xCol]], 
          y        = .data[[yCol]], 
          color    = .data[[regCol0]],
          linetype = .data[["variant"]],
          group    = interaction(!!!syms(group0))
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
          shape = .data[["variant"]],
          group = interaction(!!!syms(group0))
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
          linetype = .data[["variant"]],
          group    = interaction(!!!syms(group0))
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
          shape   = .data[["variant"]],
          group    = interaction(!!!syms(group0))
        ), ### End aes
        alpha=0.65 
      ) ### End geom_line
    }
    
    ###### * Add geoms
    # plot0  <- plot0 + geom_line(aes(linetype = .data[["variant"]]), alpha=0.5)
    
    ###### ** Add facet_grid
    plot0  <- plot0 + facet_grid(model~.data[[regCol0]])
    # plot0 |> print()
    
    ###### ** Add and title
    plot0  <- plot0 + ggtitle(title0)
    
    ###### ** Add scales
    plot0  <- plot0 + scale_x_continuous(xTitle, breaks=x_breaks, limits=x_limits)
    plot0  <- plot0 + scale_y_continuous(y_label)
    plot0  <- plot0 + scale_color_discrete("Region")
    # plot0  <- plot0 + scale_linetype_discrete("Variant")
    # plot0  <- plot0 + scale_shape_discrete("Variant")
    
    ###### ** Adjust legend title
    if(hasLgdPos){plot0 <- plot0 + guides(color = guide_legend(title.position = lgdPos))}
    plot0  <- plot0 + theme(legend.direction = "vertical", legend.box = "vertical")
    
  } ### End if(doPlot)
  
  ###### ** Adjust appearance ######
  plot0  <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=11))
  plot0  <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  plot0  <- plot0 + theme(axis.title.x  = element_text(hjust = 0.5, size=9 ))
  plot0  <- plot0 + theme(axis.title.y  = element_text(hjust = 0.5, size=9 ))
  
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
  plot0  <- plot0 + theme(legend.position  = "bottom")
  plot0  <- plot0 + theme(legend.title     = element_text(size=10))
  plot0  <- plot0 + theme(legend.text      = element_text(size=9 ))
  plot0  <- plot0 + theme(legend.spacing.y = unit(0.05, "cm"))
  # refPlot0  <- refPlot0 + theme(legend.box.margin = margin(t=0.05, r=0.05, b=0.05, l=0.05, unit='cm'))
  
  
  ###### Return ######
  ### Return the plot
  if(print_msg){ message("...Finished.")}
  return(plot0)
}