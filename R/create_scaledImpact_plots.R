### This function plots degrees of warming by sector, variant, impact year, and type
create_scaledImpact_plots <- function(
    data,
    sector,
    xCol      = "driverValue",
    yCol      = "scaled_impacts",
    colorCol  = "model",
    type0 = "GCM",
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
  # type0 %>% print()
  do_gcm    <- "gcm" %in% (type0 |> tolower())
  do_slr    <- "slr" %in% (type0 |> tolower())
  # type0 |> print(); do_gcm |> print(); do_slr |> print()
  
  ###### Get from FrEDI Namespace ######
  # get_colScale <- utils::getFromNamespace("get_colScale", "FrEDI")
  
  ###### Format Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); type0 |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(modelType == type0)
  df0       <- df0 |> filter(sector    == sector0)
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
  df0           <- df0 |> filter(!(nObs |> is.na()))
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
    # if(col_i == "model" & do_slr){
    #   col_i |> print(); cCol_i |> print(); levels_i |> print();df0 |> pull(all_of(col_i)) |> unique() |> print()
    #   c(30, 50, 100, 150, 200, 250) |> paste0("cm")
    # } ### end if(col_i == "model")
    df0      <- df0 |> mutate_at(c(col_i), factor, levels=levels_i)
    rm(i, col_i, cCol_i, levels_i)
  } ### End for(i in df_iter |> row_number())
  
  ### Factor models
  if(do_slr){
    # labs0   <- c(30, 50, 100, 150, 200, 250) |> paste0(" cm")
    labs0   <- c(30, 50, 100, 150, 200, 250) |> paste0("cm")
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
  
  ###### Reference Plot ######
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

  ###### Legend & Spacer #####
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

  ###### Common Plot Elements ######
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
        # region_k |> print
        # "\t\t" |> paste0(c(region_k, impYear_j, impType_k) |> paste(collapse=", ")) |> message()
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
        # "got here1" |> print()
        # plot_k |> print()
        
        ###### Annotate Plots ######
        ### Labels on top
        ### Longest impact type: "Acute Myocardial Infarction"
        typeTitle_k <- "Impact Type: " |> paste0(impType_k)
        grobType_k  <- text_grob(typeTitle_k, face="italic", size=11)
        # "got here2" |> print()
        plotGrid_k  <- plot_k
        # "got here3" |> print()
        plotList_k  <- list(spacer1=spacer0, plots=plotGrid_k, spacer2=spacer0)
        # "got here4" |> print()
        plotGrid_k  <- ggarrange(plotlist=plotList_k, nrow=3, ncol=1, heights=c(0.01, 1, 0.1))
        # "got here5" |> print()
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
      plotList_j  <- list(spacer1=spacer0, plot=plotGrid_j, spacer2=spacer0)
      # nModels |> print()
      modMult     <- (nModels - 1) %/% nLgdCols + 1
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