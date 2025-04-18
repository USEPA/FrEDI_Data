require(tidyverse)
###### Dimension Tests ######
#' test_loadData
#'
#' @param data_list A list of named tables to be examined
#' @param outPath A filepath to where test files are saved
#' @param save  A TRUE/FALSE option to save files or not
#' @param return A TRUE/FALSE option to return the test_table
#'
#' @return Returns a table object if "return" option is TRUE
#' @export
#'
#' @examples
dataInfo_test <- function(
    dataList = list(),
    outPath  = ".",
    csvName  = "dataInfo_test",
    save     = TRUE,  ### Whether to save results to file
    return   = TRUE   ### Whether to return results
) {
  ###### List Info ######
  ### List names
  ### Length of list and list names
  listNames <- dataList  |> names()
  lenList   <- dataList  |> length()
  lenName   <- listNames |> length()
  # c(lenList, lenName, lenList==lenName) |> print()
  
  ###### List Object Types ######
  ### Get info on object types and add names
  ### Simplify list of types
  # c("data.frame", "list", "character", "numeric")
  listTypes <- listNames |> map(~ (dataList[[.]] |> class())) |> set_names(listNames)
  ### Simplify types
  listTypes <- listNames |> map(~ case_when(
    ("data.frame" %in% listTypes[[.]]) ~ "data.frame",
    ("list"       %in% listTypes[[.]]) ~ "list",
    ("character"  %in% listTypes[[.]]) ~ "character",
    ("numeric"    %in% listTypes[[.]]) ~ "numeric",
    TRUE ~ "N/A"
  )) |> set_names(listNames)
  
  ###### Initial Table Info ######
  ### Initialize table of info...make methods specific to class
  ### Get class of table object
  df_info   <- tibble(table = listNames)
  df_info   <- df_info |> mutate(itemClass = listTypes |> unlist())
  
  ### In each table, count number of: columns, rows, distinct rows, and 
  ###   - Columns
  ###   - Rows
  ###   - Distinct rows
  ###   - Number of columns with all missing values
  ### Expressions
  nCols0    <- list(z=listNames, a=listTypes, b=dataList) |> pmap(fun_nCol ) |> unlist()
  nRows0    <- list(z=listNames, a=listTypes, b=dataList) |> pmap(fun_nRow ) |> unlist()
  unique0   <- list(z=listNames, a=listTypes, b=dataList) |> pmap(fun_nUnq ) |> unlist()
  allNACols <- list(z=listNames, a=listTypes, b=dataList) |> pmap(fun_allNA) |> unlist()
  # allNACols |> print()
  
  ### Add to df_info
  df_info   <- df_info |> mutate(num_cols    = nCols0   )
  df_info   <- df_info |> mutate(num_rows    = nRows0   )
  df_info   <- df_info |> mutate(unique_rows = unique0  )
  df_info   <- df_info |> mutate(cols_wAllNA = allNACols)
  
  ###### Check Tests ######
  ### Check number of columns with some non-missing values is equal to the number of columns
  df_info   <- df_info |> mutate(na_flag  = 1 * (cols_wAllNA > 0))
  #### Check if each table has duplicate values: Number of rows should equal number of unique rows
  ### List of tables to make exceptions for
  # except0   <- c("data_scaledImpacts")
  # except0   <- c("gcmImpacts")
  except0   <- c()
  df_info   <- df_info |> mutate(has_dups = case_when(
    table %in% except0 ~ F,
    itemClass == "list" ~ F,
    .default = !(num_rows == unique_rows)
  )) ### End mutate
  ### Check whether all tests are passed
  df_info   <- df_info |> mutate(passed = case_when(
    # itemClass == "list" ~ T, 
    has_dups == T | na_flag == T ~ F, 
    has_dups == F & na_flag == F ~ T
  ))
  ### Mutate logicals to numeric
  mutate0   <- c("has_dups", "passed")
  df_info   <- df_info |> mutate_at(c(mutate0), as.numeric)
  ### Remove intermediates
  rm(except0, mutate0)
  
  ### Print Out tables if there are any values that don't pass
  df_flags  <- df_info  |> filter(passed == 0)
  numFlags  <- df_flags |> nrow()
  hasFlags  <- numFlags > 0
  
  ### Message user
  "Checking tables..." |> message()
  msg_flags <- hasFlags |> ifelse("Some tables don't pass", "All tables pass") |> paste0("...")
  "\t" |> paste0(msg_flags) |> message()
  rm(msg_flags)
  
  ### Print flagged table results
  if(hasFlags){df_flags |> glimpse()}
  
  ### Save Option Outputs ####
  if(save){
    "Saving data checks" |> paste0("...") |> message()
    outDir    <- outPath |> file.path("data_tests")
    outExt    <- "." |> paste0("csv")
    csvName   <- csvName |> paste0(outExt)
    outFile   <- outDir  |> file.path(csvName)
    rm(outExt, csvName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  |> dir.exists()
    if(!odExists) outDir |> dir.create(showWarnings = F)
    rm(odExists)
    ## Save the test results
    df_info  |> write_csv(outFile)
  } ### End if_else
  
  ###### Return ######
  "Finished." |> message()
  if(return) return(df_info)
  else       return()
}

###### General Configuration Tests ######
### Function to initialize plots list
initialize_gen_plotsList <- function(x=1){
  list0 <- list()
  # list0[["temp"]] <- list(plotName="Temp Plot", xy=c(1 , 2 ))
  # list0[["slr" ]] <- list(plotName="SLR Plot" , xy=c(12, 2 ))
  # list0[["gdp" ]] <- list(plotName="GDP Plot" , xy=c(1 , 25))
  # list0[["pop" ]] <- list(plotName="Pop Plot" , xy=c(12, 25))
  list0[["temp"]] <- list(plotName="Temp Plot", xy=c(1, 2))
  list0[["slr" ]] <- list(plotName="SLR Plot" , xy=c(1, 2))
  list0[["gdp" ]] <- list(plotName="GDP Plot" , xy=c(1, 2))
  list0[["pop" ]] <- list(plotName="Pop Plot" , xy=c(1, 2))
  return(list0)
}

### Configuration plots function
### Add general configuration plot to workbook
add_gen_plot <- function(
    wbook,
    outPath   = ".",
    plotName  = "temp",
    plotsList = initialize_gen_plotsList()
){
  ### Info
  plotNames <- c("temp", "slr", "gdp", "pop")
  doPop     <- "pop" %in% (plotName |> tolower())
  # plotNames <- plotsList |> names()
  
  ### Plot info
  path0   <- outPath
  fType0  <- "png"
  units0  <- "in"
  dpi0    <- 200
  # width0  <- 6.0
  # height0 <- 4.5
  width0  <- 6.0 + (doPop |> ifelse(4, 0))
  height0 <- 4.5 + (doPop |> ifelse(0, 0))
  
  
  ### Adjust width and height for population
  
  
  list0  <- plotsList[[plotName]]
  plot0  <- list0[["plot"]]
  xy0    <- list0[["xy"]]
  # plotsList |> names() |> print(); list0 |> names() |> print(); xy0 |> print()
  # c(sheet0, fType0, units0, sheet0, xy0[1], xy0[2], width0, height0, units0)  |> print()
  file0  <- "tmp_" |> paste0(plotName, ".png")
  fpath0 <- path0  |> file.path(file0)
  # fpath0 |> print(); plot0 |> print()
  
  ### Add worksheet
  # sheet0  <- sheet
  sheet0 <- list0[["plotName"]]
  wbook |> addWorksheet(sheetName = sheet0)
  # rm(sheet)
  # sheet0 |> print()
  
  ### Temporarily Save Plots
  ggsave(
    filename = file0,
    path     = path0,
    plot     = plot0,
    device   = fType0,
    dpi      = dpi0,
    width    = width0,
    height   = height0,
    units    = units0
  ) ### End ggsave
  
  ### Add plots to workbook
  insertImage(
    wb       = wbook,
    sheet    = sheet0,
    file     = fpath0,
    startCol = xy0[1],
    startRow = xy0[2],
    width    = width0,
    height   = height0,
    units    = units0
  ) ### End insertImage
  
  # ### Delete temporary file
  # fpath0 |> file.remove()
  # rm(plotName, plot0, xy0, file0, path0, fpath0)
  
  ### Return
  return(wbook)
} 



### Configuration Test
general_config_test <- function(
    configuredData = NULL , ### List of configured data, e.g., as output from configureSystemData()
    outPath        = "."  , ### Where to save results
    xlsxName       = "configTestResults" |> paste0(".", "xlsx"), ### File name for outputs
    doPlots        = FALSE, ### Whether to create scaled impact plots
    return         = TRUE , ### Whether to return results
    save           = TRUE , ### Whether to save results to file
    overwrite      = TRUE , ### Whether to overwrite an existing file if saving
    msg0           = "\t"
){
  ### Messaging
  msgN <- "\n"
  msgN |> paste0(msg0, "Running general_config_test()...") |> message()
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  
  ###### Create Workbook ######
  if(save) {
    outDir    <- outPath |> file.path("data_tests")
    outFile   <- outDir  |> file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  |> dir.exists()
    if(!odExists) outDir |> dir.create(showWarnings = F)
    rm(odExists)
    ### Create Excel workbook
    wbook0    <- createWorkbook()
  } ### End if(save)
  
  ###### Assign Objects ######
  configList0 <- configuredData[["fredi_config"]]
  for(name_i in configList0 |> names()) {name_i |> assign(configList0[[name_i]]); rm(name_i)}
  
  ###### Initialize List ######
  ### Initialize list for saving values
  ### Initialize list for default plots
  saveList    <- list()
  listPlots   <- initialize_gen_plotsList()
  
  ### Whether reshaed rseults exist
  ### Get names of lists of configured data list
  dataNames   <- configuredData |> names()
  
  ###### Data Names ######
  ### Data Names
  cfigName0   <- "configuredData_baseTest"
  defParam0   <- "defaultParameters"
  defPlots0   <- "defaultPlots"
  
  
  ###### Reshaped Data ######
  ### Check if reshaped data exists
  rshpExists  <- dataNames |> grepl("rsData", x = _) |> any()
  
  ### Breakout reshaped Data if it exists, then drop from list
  rshpName0   <- "rshpData_baseTest"
  rshpData    <- configuredData[["rsData"]]
  hasRshp     <- !(rshpData |> is.null()) & (rshpData |> length())
  configuredData <- configuredData |> (function(list0, x=c("rsData")){list0[!((list0 |> names()) %in% x)]})()
  # # configuredData[["rsData"]] <- NULL
  
  ### If reshapedData exists, check if it's the correct class
  if(rshpExists) {
    ### Check State reshaped data if it exists
    if(hasRshp){
      class0   <- rshpData |> class()
      is_list0 <- "list" %in% class0
      ### If reshapedData is not a list, message the user
      if(!is_list0) {
        paste0(msg1, "`reshapedData` must be of class \`list\`...") |> message()
        paste0(msg2, "Skipping reshaped data", "...", msgN) |> message()
      } else {
        paste0(msg1, "Testing reshaped data ...") |> message()
        ### Get results, add results to list
        rshpResults <- rshpData |> (function(list0){
          names0 <- list0 |> names()
          list0  <- list(name0_i = names0, list0_i = list0)
          df0    <- list0 |> pmap(function(name0_i, list0_i){
            name0_i |> print()
            df0_i <- list0_i |> dataInfo_test(save=F, return=T)
            df0_i <- df0_i   |> mutate(listName = name0_i)
            return(df0_i)
          }) |> bind_rows()
          ### Return
          return(df0)
        })()
        saveList[[rshpName0]] <- rshpResults
      } ### End if(!is_list0)
      rm(class0, is_list0)
    } ### End if(!(rshpData |> is.null()))
  }  else {
    # rshpResults <- tibble()
  } ### End if(rshpExists)
  
  
  ###### Configured Data ######
  ### Check if configuredData exists
  has_data0  <- !(configuredData |> is.null())
  ### If reshapedData exists, check if it's the correct class
  if(has_data0) {
    class0   <- configuredData$frediData |> class()
    is_list0 <- "list" %in% class0
    ### If configuredData is not a list, message the user
    if(!is_list0) {
      paste0(msg1, "`configuredData` must be of class \`list\`...") |> message()
      paste0(msg2, "Exiting", "...", msgN) |> message()
      return()
    }  else {
      paste0(msg1, "Testing configured data ...") |> message()
      dataResults <- configuredData |> (function(list0){
        names0 <- list0 |> names()
        list0  <- list(name0_i = names0, list0_i = list0)
        df0    <- list0 |> pmap(function(name0_i, list0_i){
          # name0_i |> print()
          df0_i <- list0_i |> dataInfo_test(save=F, return=T)
          df0_i <- df0_i |> mutate(listName = name0_i)
          return(df0_i)
        }) |> bind_rows()
        return(df0)
      })()
    } ### End else(!is_list0)
    rm(class0, is_list0)
  } else {
    dataResults <- tibble()
  } ### End if(has_reshape0)
  ### Add table to list
  saveList[[cfigName0]] <- dataResults
  
  
  ###### Configuration Values ######
  ### Drop messages from list
  drop0         <- c("messages_data", "list_messages")
  listConfig0   <- configList0 |> (function(list0, x=drop0){list0[!((list0 |> names()) %in% x)]})()
  rm(drop0)
  ### Create table
  defaultsList  <- listConfig0 |> names() |> map(function(
    name_i,
    list0 = listConfig0
  ){
    val0_i <- list0[[name_i]]
    type_i <- val0_i |> class() |> paste(collapse=", ")
    val1_i <- val0_i |> paste(collapse=", ")
    df_i   <- tibble(parameter=name_i, class=type_i, value=val1_i)
    return(df_i)
  }) |> bind_rows()
  ### Add table to list
  saveList[[defParam0]] <- defaultsList
  
  ###### Add Data to Excel Workbook ######
  ### Add Data to Excel workbook if save
  if(save) {
    ### Sheet Names
    names0  <- saveList |> names()
    for(name_i in names0) {
      wbook0 |> addWorksheet(sheetName = name_i)
      wbook0 |> writeDataTable(sheet = name_i, saveList[[name_i]])
      rm(name_i)
    } ### End for(name_i in names0)
    rm(names0)
  } ### End if(save)
  
  ###### Default Plots ######
  ### Plot values
  lab_yrs0  <- "Year"
  lim_yrs0  <- c(2000, 2300)
  brk_yrs0  <- seq(lim_yrs0[1], lim_yrs0[2], by=20)
  
  ### Temp plot
  # configuredData$scenarioData |> names() |> print()
  temp_plot <- configuredData$scenarioData[["gcam_default"]] |> (function(df0){
    years0 <- df0 |> pull(year) |> get_years_fromData()
    labYrs <- "Year"
    limYrs <- years0 |> range()
    brkYrs <- limYrs[1] |> seq(limYrs[2], by=20)
    yLab0  <- expression("CONUS Degrees of Warming ("~degree*C*")")
    p0     <- df0 |> ggplot() +
      geom_line(aes(x = year, y = temp_C_conus)) +
      scale_x_continuous(labYrs, breaks=brkYrs, limits=limYrs) +
      scale_y_continuous(yLab0) +
      ggtitle("Default Temperature Scenario")
    return(p0)
  })()
    
  ### SLR plot
  slr_plot  <- configuredData$scenarioData[["gcam_default"]] |> (function(df0){
    years0 <- df0 |> pull(year) |> get_years_fromData()
    labYrs <- "Year"
    limYrs <- years0 |> range()
    brkYrs <- limYrs[1] |> seq(limYrs[2], by=20)
    p0     <- df0 |> ggplot() +
      geom_line(aes(x = year, y = slr_cm)) +
      scale_x_continuous(lab_yrs0, breaks=brkYrs, limits=limYrs) +
      scale_y_continuous("SLR (cm)") +
      ggtitle("Default SLR Scenario")
    return(p0)
  })()
    
    
  ### GDP Plot: Convert to Billions
  gdp_plot <- configuredData$scenarioData[["gdp_default"]] |> (function(df0){
    years0 <- df0 |> pull(year) |> get_years_fromData()
    labYrs <- "Year"
    limYrs <- years0 |> range()
    brkYrs <- limYrs[1] |> seq(limYrs[2], by=20)
    df0    <- df0 |> mutate(gdp_usd = gdp_usd / 1e12)
    p0     <- df0 |> ggplot() +
      geom_line(aes(x = year, y = gdp_usd)) +
      scale_x_continuous(labYrs, breaks=brkYrs, limits=limYrs) +
      scale_y_continuous("U.S. National GDP (2015$, trillions)") +
      ggtitle("Default GDP Scenario")
    return(p0)
  })()
    
  
  ### Pop plot 
  pop_plot <- configuredData$scenarioData[["pop_default"]] |> (function(df0){
    years0 <- df0 |> pull(year) |> get_years_fromData()
    labYrs <- "Year"
    limYrs <- years0 |> range()
    brkYrs <- limYrs[1] |> seq(limYrs[2], by=20)
    
    df0    <- df0 |> mutate(pop = pop / 1e6)
    df1    <- df0 |> filter(year == 2050)
    p0     <- df0 |> ggplot() +
      geom_line(aes(x=year, y=pop, color=region, group=interaction(region, state)), alpha = 0.75) +
      scale_x_continuous(labYrs, breaks=brkYrs, limits=limYrs) +
      scale_y_continuous("State Population (millions)") +
      geom_text(data=df1, aes(x=year, y=pop, label=postal), stat="identity", alpha=0.6) +
      facet_grid(.~region) +
      # theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position = "bottom") +
      scale_color_discrete("State") +
      ggtitle("Default Population Scenario")
    return(p0)
  })()
    
    
  ### Add plots to list
  listPlots[["temp"]] <- list(plot=temp_plot) |> c(listPlots[["temp"]])
  listPlots[["slr" ]] <- list(plot=slr_plot ) |> c(listPlots[["slr" ]])
  listPlots[["gdp" ]] <- list(plot=gdp_plot ) |> c(listPlots[["gdp" ]])
  listPlots[["pop" ]] <- list(plot=pop_plot ) |> c(listPlots[["pop" ]])
  ### Add plot list to saveList
  saveList[[defPlots0]] <- listPlots
  
  
  ###### Add Plots to Excel Workbook ######
  if(save) {
    ### Add worksheet
    names0  <- listPlots |> names()
    # sheet0  <- defPlots0
    
    ### Add Plots
    for(name_i in names0){
      # wbook0 <-
      add_gen_plot(
        plotsList = listPlots,
        plotName  = name_i,
        wbook     = wbook0,
        # sheet     = sheet0,
        outPath   = outDir
      ) ### End add_gen_plot
    } ### End: for name_i
  } ### End if(save)
  
  ###### Create Scaled Impact Results ######
  # ### Get results
  if(doPlots) {
    scaledData  <- configuredData |> get_fredi_sectorOptions_results()
    scaledPlots <- configuredData |> get_scaled_impact_plots()
    ### Save
    if(save) {
      wbook0 |> addWorksheet(sheetName = "scaledImpacts_data")
      wbook0 |> writeDataTable(sheet = "scaledImpacts_data", scaledData)
      rm(name_i)
    } ### End if(save)

    ### Add to return list, drop intermediate objects
    saveList[["scaledImpactsPlots"]] <- list(data=scaledData, plots=scaledPlots)
    rm(scaledData, scaledPlots)
  } ### End if(doPlots)
 
  
  ###### Save Outputs ######
  ### Save the workbook, remove workbook
  if(save){
    paste0(msg1, "Saving data tests...") |> message()
    wbook0  |> saveWorkbook(file=outFile, overwrite=overwrite)
    rm(wbook0)
    ### Remove temporary image files
    files0 <- outDir |> list.files(pattern="tmp_", full.names=T)
    files0 |> file.remove()
  } ### End if(save)
  
  ###### Return ######
  paste0(msg0, "...Finished running general_config_test().") |> paste0(msgN) |> message()
  if(return) return(saveList)
}



###### New Sector Plot Function ######
get_fredi_sectorOptions <- function(
    dfGroups  ### rDataList$stateData$nonNAGroups, where rDataList is output from configureSystemData()
){
  ### Load scenario ID function
  get_scenario_id <- utils::getFromNamespace("get_scenario_id", "FrEDI")
  
  ### Add scenario ID
  include0   <- c("region") |> c("state", "postal") |> c("model")
  dfGroups   <- dfGroups    |> get_scenario_id(include=c(include0))
  
  ### Return
  return(dfGroups)
}


get_fredi_sectorOptions_results <- function(
    dataList ### Data list, rDataList produced by createSystemData
){
  
  ###### Data Lists ######
  frediData  <- dataList[["frediData"]]
  dataList   <- dataList[["stateData"]]
  
  ### State Columns
  stateCols  <- c("state", "postal")
  
  ###### Sector Options ######
  df0        <- dataList[["nonNAGroups"]] |> get_fredi_sectorOptions()
  names0     <- df0 |> names()
  # df0 |> glimpse()
  
  ###### Split Data ######
  ### Split sectors by model type
  df_gcm     <- df0 |> filter(modelType=="gcm")
  df_slr     <- df0 |> filter(modelType=="slr")
  ### Number of sector options
  n_gcm      <- df_gcm |> nrow()
  n_slr      <- df_slr |> nrow()
  ### Whether to do GCM and/or SLR
  do_gcm     <- n_gcm > 0
  do_slr     <- n_slr > 0
  
  ###### Initialize Tibble ######
  df0        <- tibble()
  
  ###### Do SLR Results ######
  if(do_slr){
    ### Load SLR data
    ### Join df_slr with impacts
    # c("sector", "variant", "impactType", "impactYear", "region", stateCols, "modelType", "model")
    # names0  <- df_slr |> names()
    select0 <- names0 |> c("year", "scaled_impacts")
    slrImp  <- dataList[["slrImpacts"]]
    slrImp  <- slrImp |> select(all_of(select0))
    # slrImp |> glimpse(); df_slr |> glimpse()
    df_slr  <- df_slr |> left_join(slrImp, by=c(names0))
    rm(select0, slrImp)
    
    ### Add driverValue
    ### Relocate columns
    move0      <- c("driverValue", "scaled_impacts")
    after0     <- c("year")
    df_slr     <- df_slr |> mutate(driverValue=model |> str_replace("cm", "") |> as.numeric())
    df_slr     <- df_slr |> relocate(all_of(move0), .after=all_of(after0))
    rm(move0, after0)
    # df_slr |> glimpse()
    
    ### Bind with initial results
    df0        <- df0 |> rbind(df_slr)
    rm(df_slr)
  } ### End if(do_slr)
  
  ###### Do GCM Results ######
  if(do_gcm){
    ### Get function list
    funList    <- dataList[["gcmImpFuncs"]]
    funNames   <- funList  |> names()
    nFunctions <- funNames |> length()
    
    ### Create temperature scenario
    ### Execute the impact functions across new sectors (creates a wide tibble)
    ### Add temperatures
    ### Gather scenario values
    keyCols0   <- funNames
    df_temps   <- tibble(temp_C = -1:11)
    df_vals    <- df_temps |> map_df(~ funList |> map_df(exec,.x))
    rm(funList, funNames, nFunctions)
    
    ### Mutate and pivot
    df_vals    <- df_vals  |> mutate(driverValue = df_temps |> pull(temp_C))
    df_vals    <- df_vals  |> pivot_longer(
      all_of(keyCols0), 
      names_to  = "scenario_id",
      values_to = "scaled_impacts"
    ) ### End pivot_longer
    rm(keyCols0)
    
    ### Join with df_gcm
    # df_vals |> names() |> print(); df_gcm |> names() |> print()
    join0      <- c("scenario_id")
    select0    <- join0
    df_gcm     <- df_vals |> left_join(df_gcm, by=c(join0))
    df_gcm     <- df_gcm  |> relocate(all_of(select0))
    rm(df_vals, select0, join0)
    # df_gcm |> glimpse()
    
    ### Add year
    ### Relocate columns
    move0      <- c("year")
    before0    <- c("driverValue")
    df_gcm     <- df_gcm |> mutate(year = impactYear |> na_if("NA") |> as.numeric())
    df_gcm     <- df_gcm |> relocate(all_of(move0), .before=all_of(before0))
    rm(move0, before0)
    
    ### Bind with initial results
    # df0 |> names() |> print(); df_gcm |> names() |> print()
    # select0    <- df_gcm |> names() |> (function(x, y=df0 |> names()){x[x %in% y]})()
    df0        <- df0 |> rbind(df_gcm)
    rm(df_gcm)
  } ### End if(do_gcm)
  
  ###### Add Model Type Info ######
  renameAt0  <- c("modelType_id", "modelUnit_label")
  renameTo0  <- c("modelType", "modelUnit")
  move0      <- c("modelUnitDesc", "modelUnitScale")
  after0     <- c("model")
  join0      <- c("modelType")
  select0    <- renameTo0 |> c(move0)
  df_mTypes  <- frediData[["co_modelTypes"]]
  df_mTypes  <- df_mTypes |> rename_at(c(renameAt0), ~c(renameTo0))
  df_mTypes  <- df_mTypes |> select(all_of(select0))
  df0        <- df0       |> left_join(df_mTypes, by=c(join0))
  df0        <- df0       |> relocate(all_of(select0), .after=all_of(after0))
  rm(renameAt0, renameTo0, select0, move0, after0, join0)

  ###### Add Model Info ######
  renameAt0  <- c("model_id")
  renameTo0  <- c("model")
  move0      <- c("maxUnitValue")
  after0     <- c("modelUnitScale")
  join0      <- c("model")
  select0    <- renameTo0 |> c(move0)
  df_models  <- frediData[["co_models"]]
  df_models  <- df_models |> rename_at(c(renameAt0), ~c(renameTo0))
  df_models  <- df_models |> select(all_of(select0))
  df0        <- df0       |> left_join(df_models, by=c(join0))
  df0        <- df0       |> relocate(all_of(select0), .after=all_of(after0))
  rm(renameAt0, renameTo0, select0, move0, after0, join0)

  
  ###### Select Columns ######
  # arrange0   <- c("sector", "variant", "impactType", "impactYear", "region", stateCols, "modelType", "model")
  select0    <- c("scenario_id") |> c(names0) |> c("modelUnit", "modelUnitDesc", "modelUnitScale", "maxUnitValue", "driverValue", "year", "scaled_impacts")
  df0        <- df0 |> select(all_of(select0))
  df0        <- df0 |> arrange_at(c(names0))
  df0        <- df0 |> mutate(modelType = modelType |> toupper())
  
  ###### Return ######
  return(df0)
}



#### Plot information by model type
make_scaled_impact_plots <- function(
    df0,     ### Data frame for plot data
    yCol     = "scaled_impacts", ### Column to use for y
    colorCol = "model",          ### Column to color
    options  = list(             ### Other options
      lgdTitle   = "Model",
      lgdPos     = "top",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    ), ### End list
    silent = TRUE
){
  ###### Get from FrEDI Namespace ######
  ### Other values
  years      <- df0 |> pull(impactYear) |> unique()
  models     <- df0 |> pull(modelType ) |> unique()
  ### Data frame to iterate over
  do_gcm     <- "gcm" %in% tolower(models)
  do_slr     <- "slr" %in% tolower(models)
  
  ### Get iteration list
  df_types   <- df0 |> 
    select(c("sector", "impactYear", "modelType")) |> 
    unique() |>
    mutate(label = sector |> paste0("_", impactYear))
  # df_types |> glimpse()
  
  ### Get list
  # models |> print()
  list0    <- models |> map(function(.x){
    paste0("Creating plots for model type ", .x, "...") |> message()
    df_x      <- df0 |> filter(modelType %in% c(.x))
    # df_x |> glimpse()
    ### Sectors
    types_x   <- df_types |> filter(modelType==.x)
    sectors_x <- types_x  |> pull(sector)
    ### Get X column
    xCol_x    <- ((.x |> tolower()) == "gcm") |> ifelse("driverValue", "year")
    # df_types |> glimpse()
    pList_x   <- list(x1=types_x |> pull(sector), x2=types_x |> pull(impactYear))
    ### Iterate over list
    list_x    <- pList_x |> pmap(function(x1, x2){
      x1 |> paste0("_", x2) |> print()
      df_y   <- df0  |> filter(sector == x1)
      df_y   <- df_y |> filter(impactYear %in% x2)
      
      ### Make plots
      plot_y <- df_y |> create_scaledImpact_plots(
        sector    = x1,
        type0     = .x,
        yCol      = yCol,
        xCol      = xCol_x,
        colorCol  = colorCol,
        silent    = silent,
        options   = options
      ) ### End create_scaledImpact_plots
      # plot_y |> names() |> print()
      ### Return
      gc()
      return(plot_y)
    })
    ### Add names
    labels_x <- types_x |> pull(label)
    list_x   <- list_x  |> set_names(labels_x)
    ### Return
    gc()
    return(list_x)
  })
  ### Add names
  list0   <- list0 |> set_names(models)
  ### Return
  gc()
  return(list0)
} ### End plot_DoW_by_sector



### Make scaled impact plots for sectors
get_scaled_impact_plots <- function(
    dataList,       ### Data list
    save    = TRUE, ### Whether to save images
    fpath   = "." |> file.path("data_tests") ### Where to save images
){
  ### returnList
  return0   <- list()
  ### Get results
  results0  <- dataList |> get_fredi_sectorOptions_results()
  return0[["data"]] <- results0
  # results0 |> glimpse()
  
  ### Make scaled impact plots
  plots0    <- results0 |> make_scaled_impact_plots()
  return0[["plots"]] <- plots0
  
  ### Save results
  if(save){
    "Saving plots..." |> message()
    save_gcm <- plots0 |> save_scaled_impact_figures(df0=results0, type0="GCM", fpath=fpath)
    save_slr <- plots0 |> save_scaled_impact_figures(df0=results0, type0="SLR", fpath=fpath)
  } ### End if save
  
  ### Return plot list
  return(return0)
}



###### save_scaled_impact_figures ######
### Wrapper function to help save appendix figures to file
save_scaled_impact_figures <- function(
    plotList, ### List of plots
    df0,      ### Dataframe used to create plots
    type0     = "GCM", ### Or SLR (model type)
    fpath     = ".",   ### Path to save files
    device    = "pdf", ### Type of image
    res       = 200,   ### If png, image resolution
    units     = "in",  ### Units to use for saving the file
    createDir = TRUE   ### Whether to create directory if it doesn't exist
){
  ### Get from FrEDI Namespace
  # check_and_create_path <- utils::getFromNamespace("check_and_create_path", "FrEDI")
  # save_image            <- utils::getFromNamespace("save_image", "FrEDI")
  ### Create directory if it doesn't exist
  fdir      <- fpath; rm(fpath)
  fdir      <- fdir |> file.path("images")
  created0  <- fdir |> check_and_create_path(createDir=createDir)
  ### Prepare data
  typeLC0   <- type0 |> tolower()
  df0       <- df0   |> filter((modelType |> tolower()) %in% typeLC0)
  list0     <- plotList[[type0]]
  ### Unique values
  names0    <- list0  |> names()
  sectors0  <- names0 |> map(function(.x){str_split(string=.x, pattern="_")[[1]][1]}) |> unlist() |> unique()
  refYears0 <- names0 |> map(function(.x){str_split(string=.x, pattern="_")[[1]][2]}) |> unlist() |> unique()
  # names0 |> print(); #sectors0 |> print(); refYears0 |> print()
  
  ### Iterate over sectors
  names0 |> map(function(.x){
    ### Plot .x
    list_x    <- list0[[.x]]
    # .x |> print()
    
    ### Split name into sector and ref year
    sector_x  <- .x |> map(function(.y){str_split(string=.y, pattern="_")[[1]][1]}) |> unlist() |> unique()
    year_x    <- .x |> map(function(.y){str_split(string=.y, pattern="_")[[1]][2]}) |> unlist() |> unique()
    regions_x <- list_x |> names()
    # sector_x |> c(year_x) |> print()
    
    ### Filter to data
    df_x      <- df0 |> filter(sector == sector_x)
    # df0 |> glimpse()
    
    ### Unique sector values
    c_types   <- df_x |> pull(impactType) |> unique()
    c_regions <- df_x |> pull(region    ) |> unique()
    c_states  <- df_x |> pull(state     ) |> unique()
    c_models  <- df_x |> filter(!(scaled_impacts |> is.na())) |> pull(model) |> unique()
    # c_years |> print(); c_types |> print(); c_vars |> print();
    
    ### Number of values
    # n_years   <- c_years |> length()
    n_types   <- c_types   |> length()
    n_regions <- c_regions |> length()
    n_states  <- c_states  |> length()
    n_models  <- c_models  |> length()
    # n_types |> c(n_vars, n_models) |> print()
    
    ### Get number of legend rows
    lgdCols  <- 4
    lgdRows  <- (n_models - 1) %/% lgdCols + 1
    
    ### Plot heights
    ### Functions for plot height & width
    # fun_plot_width   <- function(nvars =1){1.5 + 3.3 * nvars}
    fun_plot_width   <- function(nvars =1){1.5 + 2.0 * nvars}
    
    fun_plot_height1 <- function(nvars =1, ntypes=1){
      scale0 <- case_when(
        nvars   == 1 ~ 3.0,
        ntypes  == 5 ~ 1.25,
        ntypes  == 4 ~ 1.50,
        .default = 2.5
      ) ### End case_when()
      h0 <- 1.5 + scale0 * nvars
      h1 <- h0 * ntypes
      return(h1)
    } ### case_when()
    
    ### Widths
    w_x       <- n_regions |> fun_plot_width  ()
    h_x       <- n_models  |> fun_plot_height1(ntypes=n_types)
    # w_x |> c(h_x) |> print()
    
    ### Plot options
    units_x   <- units; 
    res_x     <- res  ; 
    dev_x     <- device |> tolower()
    opts_x    <- list(
      height = h_x,
      width  = w_x,
      res    = res_x,
      units  = units_x
    ) ### End options
    
    ### Iterate over regions
    regions_x |> map(function(.y){
      allReg_y  <- "all" == (.y |> tolower())
      regLbl_y  <- allReg_y |> ifelse(.y |> paste0("Regions"), .y)
      fname_y   <- sector_x |> paste0("_", regLbl_y, "_", year_x)
      list_y    <- list_x[[.y]]
      plot_y    <- list_y[[1]]
      saved_y   <- plot_y  |> save_image(
        fpath     = fdir , 
        fname     = fname_y,
        device    = dev_x,
        createDir = createDir,
        options   = opts_x
      ) ### End save_image
    })
    # "got here2" |> print()
  }) ### End map(function(.z))
  ### Return
} ### End save_appendix_figures


###### End of Page ######


