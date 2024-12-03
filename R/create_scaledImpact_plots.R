### This function plots degrees of warming by sector, variant, impact year, and type
create_scaledImpact_plots <- function(
    data,
    sector,
    xCol      = "driverValue",
    yCol      = "scaled_impacts",
    colorCol  = "model",
    type0     = "GCM",
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
  # type0 |> print()
  typeLC0   <- type0 |> tolower()
  do_gcm    <- "gcm" %in% typeLC0
  do_slr    <- "slr" %in% typeLC0
  # type0 |> print(); do_gcm |> print(); do_slr |> print()
  
  ###### Get from FrEDI Namespace ######
  # get_colScale <- utils::getFromNamespace("get_colScale", "FrEDI")
  
  ###### Format Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); type0 |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(sector == sector0)
  df0       <- df0 |> filter((modelType |> tolower()) %in% typeLC0)
  # df0 |> glimpse()
  
  ###### By State ######
  # byState   <- df0 |> pull(state) |> unique() |> (function(x){!("N/A" %in% x)})()
  byState   <- df0 |> filter(!(state %in% "N/A")) |> nrow()
  stateCols <- c("state", "postal")
  
  ###### Plot Options ######
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
  # xTitle |> print()
  
  ###### Get Sector Info ######
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
  nRegions      <- cRegions  |> length()
  nStates       <- cStates   |> length()
  # cSectors |> print(); cVariants |> print(); cImpTypes |> print(); cImpYears |> print(); cStates |> head() |> print(); cModels |> print();
  # c(nSectors, nVariants, nImpTypes, nImpYears, nRegions, nModels, nStates) |> print()
  
  ### Add maxUnitValue to df_iter
  # join0         <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols) |> c("model")
  # df_iter
  
  ###### Drop Data ######
  ### Join iteration with data and drop models
  names0        <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols) |> c("model")
  join0         <- names0    |> c("maxUnitValue")
  df0           <- df0       |> left_join(df_info, by=c(join0))
  df0           <- df0       |> filter(!(nObs |> is.na()))
  cModels       <- df_info   |> pull(model ) |> unique()
  cRegions      <- df_info   |> pull(region) |> unique()
  nModels       <- cModels   |> length()
  nRegions      <- cRegions  |> length()
  df0           <- df0       |> filter(model      %in% cModels)
  df0           <- df0       |> filter(region %in% cRegions)
  rm(join0)
  
  
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
    # slrUnit <- paste0(" ", "cm")
    slrUnit <- paste0("cm")
    labs0   <- c(30, 50, 100, 150, 200, 250) |> paste0(slrUnit)
    df0     <- df0 |> mutate(model = model |> factor(levels=labs0))
  } ### End if(do_slr)
  
  ### Convert maxUnitValue to numeric
  df0     <- df0 |> mutate_at(c("maxUnitValue"), as.character)
  df0     <- df0 |> mutate_at(c("maxUnitValue"), as.numeric  )
  
  ###### Plot Title Info ######
  ### Default for now
  # x_denom <- y_denom <- 1
  
  ###### ** X Breaks ######
  x_info     <- NULL
  x_info     <- x_info |> getXAxisScale(
    xCol    = xCol,
    maxYear = 2100,
    yrUnit  = 20
  ) ### End getXAxisScale
  ### Assign to objects
  x_limits   <- x_info[["limits"]]
  x_breaks   <- x_info[["breaks"]]
  x_denom    <- x_info[["denom" ]]
  # x_info     <- NULL
  
  ###### ** Y-Breaks ######
  y_info     <- infoList0[["minMax"]]
  # y_info     <- y_info |> filter(plotRow == row0)
  # y_info     <- y_info |> mutate(sector=sector0)
  y_info     <- y_info |> filter(sector     == sector0 )
  y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
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
  # y_label    <- y_label |> paste0(y_prelabel, "$2015")
  y_label    <- y_label
  y_label    <- "Scaled Impacts (" |> paste0(y_label, ")")
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()
  
  ###### State vs. Region Options ######
  ### What to iterate over
  ### Number of iteration values
  if(byState){cIter1 <- cRegions} else{cIter1 <- "All"} ### End else
  nIter1 <- cIter1 |> length()
  # nIter2 <- cIter2 |> length()

  ###### Common Plot Elements ######
  nLgdCols  <- nRegions
  spacer0   <- ggplot() + theme_void()
  
  ###### Create Plot List ######
  ### Iterate over Impact Years
  # cIter1 |> print()
  listIter1 <- cIter1 |> map(function(iter1_i){
    listYears0 <- cImpYears |> map(function(impYear_j){
      listTypes_j <- cImpTypes |> map(function(impType_k){
        # iter1_i |> print(); impYear_j |> print(); impType_k |> print(); 
        ### Figure out min/max across all variants for an impact type to get the y-scale
        region_k  <- iter1_i
        # region_k |> print()
        # "\t\t" |> paste0(c(region_k, impYear_j, impType_k) |> paste(collapse=", ")) |> message()
        ###### Create the plot ######
        plot_k    <- df0 |> create_scaledImpact_plot(
          sector0   = sector0,
          impType0  = impType_k,
          impYear0  = impYear_j,
          region0   = region_k,
          infoList0 = infoList0, ### Dataframe with sector info...output from get_region_plotInfo
          xCol      = xCol,      ### X-Column,
          yCol      = yCol,      ### Y-Column,
          colorCol  = colorCol,
          xInfo     = x_info,    ### xScale...outputs of get_colScale
          refPlot   = FALSE,     ### Whether to do a ref plot
          silent    = silent,
          options   = plotOpts0
        ) ### End create_scaledImpact_plot
        # plot_k |> print()
        
        ###### Annotate Plots ######
        ### Labels on top
        ### Longest impact type: "Acute Myocardial Infarction"
        typeTitle_k <- "Impact Type: " |> paste0(impType_k)
        grobType_k  <- text_grob(typeTitle_k, face="italic", size=11)
        plotGrid_k  <- plot_k
        plotList_k  <- list(spacer1=spacer0, plots=plotGrid_k, spacer2=spacer0)
        plotGrid_k  <- ggarrange(plotlist=plotList_k, nrow=3, ncol=1, heights=c(0.01, 1, 0.1))
        plotGrid_k  <- plotGrid_k |> annotate_figure(top=grobType_k)
        plotGrid_k |> print()
        ### Return
        gc()
        return(plotGrid_k)
      })
      
      ### Name the plots
      # listTypes_j |> length() |> print(); cImpTypes |> print()
      listTypes_j <- listTypes_j |> set_names(cImpTypes)
      # return(listTypes_j)
      
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
      plotGrid_j |> print()
      
      ###### Return Impact Type Plot ######
      gc()
      return(plotGrid_j)
    })
    ### Name the plots
    listYears0 <- listYears0 |> set_names(cImpYears)
    gc()
    return(listYears0)
  }) ### End iter1_i
  ### Name the plots
  listIter1 <- listIter1 |> set_names(cIter1)
  
  ###### Return ######
  ### Return the plot
  if(print_msg) message("Finished.")
  return(listIter1)
}