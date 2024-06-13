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
    save     = TRUE, ### Whether to save results to file
    return   = TRUE  ### Whether to return results
) {
  #dataList <- list_reshapeData 
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
  # cTypes    <- c("data.frame", "list", "character", "numeric")
  listTypes <- listNames |> map(~ (dataList[[.]] |> class()))
  ### Add names back to list
  listTypes <- listTypes |> (function(x){names(x) <- listNames; return(x)})()
  # listTypes[1] |> print()
  ### Simplify types
  listTypes <- listNames |> map(~ case_when(
    ("data.frame" %in% listTypes[[.]]) ~ "data.frame",
    ("list"       %in% listTypes[[.]]) ~ "list",
    ("character"  %in% listTypes[[.]]) ~ "character",
    ("numeric"    %in% listTypes[[.]]) ~ "numeric",
    TRUE ~ "N/A"
  ))
  ### Add names back to list
  listTypes <- listTypes |> (function(x){names(x) <- listNames; return(x)})()
  # c(length(listTypes), names(listTypes) |> length()) |> print()
  
  ###### Initial Table Info ######
  ### Initialize table of info...make methods specific to class
  ### Get class of table object
  df_info   <- tibble(table = listNames)
  df_info   <- df_info |> mutate(itemClass = listTypes |> unlist())
  
  ### Count number of columns in each table
  ### Count number of rows in each table
  ### Count number of distinct rows in each table
  ### Count number of missing values
  ### Expressions
  num_cols    <- listNames |> map(~ .x |> fun_nCol(a=listTypes, b=dataList)) |> unlist()
  num_rows    <- listNames |> map(~ .x |> fun_nRow(a=listTypes, b=dataList)) |> unlist()
  unique_rows <- listNames |> map(~ .x |> fun_nUnq(a=listTypes, b=dataList)) |> unlist()
  cols_wAllNA <- listNames |> map(~ .x |> fun_nNna(a=listTypes, b=dataList)) |> unlist()
  
  ### Add to df_info
  df_info   <- df_info |> mutate(num_cols    = num_cols)
  df_info   <- df_info |> mutate(num_rows    = num_rows)
  df_info   <- df_info |> mutate(unique_rows = unique_rows)
  df_info   <- df_info |> mutate(cols_wAllNA = cols_wAllNA)
  
  ###### Check Tests ######
  ### Check number of columns with some non-missing values is equal to the number of columns
  df_info   <- df_info |> mutate(na_flag  = 1 * (cols_wAllNA > 0))
  #### Check if each table has duplicate values: Number of rows should equal number of unique rows
  ### List of tables to make exceptions for
  except0   <- c("data_scaledImpacts")
  df_info   <- df_info |> mutate(has_dups = case_when((itemClass == "list") ~ F, (num_rows == unique_rows) ~ F, (table %in% except0) ~ F))
  ### Check whether all tests are passed
  df_info   <- df_info |> mutate(passed   = case_when((itemClass == "list") ~ T, (has_dups == T | na_flag == T) ~ F, (has_dups == F & na_flag == F) ~ T))
  ### Mutate logicals to numeric
  mutate0   <- c("has_dups", "passed")
  df_info   <- df_info |> mutate_at(c(mutate0), as.numeric)
  ### Remove intermediates
  rm(except0, mutate0)
  
  ### Print Out tables if there are any values that don't pass
  df_flags  <- df_info  |> filter(passed==0)
  numFlags  <- df_flags |> nrow()
  hasFlags  <- numFlags > 0
  
  ### Message user
  "Checking tables..." |> message()
  msg_flags <- ifelse(hasFlags, "Some tables don't pass", "All tables pass") |> paste0("...")
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
    outFile   <- outDir |> file.path(csvName)
    rm(outExt, csvName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir |> dir.exists()
    if(!odExists){outDir |> dir.create(showWarnings = F)}
    rm(odExists)
    ## Save the test results
    df_info  |> write_csv(outFile)
  } ### End if_else
  
  ###### Return ######
  "Finished." |> message()
  if(return) {return(df_info)}
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
  # plotNames <- plotsList |> names()
  
  ### Plot info
  path0   <- outPath
  fType0  <- "png"
  units0  <- "in"
  dpi0    <- 200
  width0  <- 6.0
  height0 <- 4.5
  
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
    configuredData = NULL, ### List of configured data, e.g., as output from configureSystemData()
    byState      = TRUE,  ### Whether to run results by state (deprecated)
    outPath      = ".",   ### Where to save results
    xlsxName     = "generalConfig_testResults.xlsx", ### File name for outputs
    return       = TRUE,  ### Whether to return results
    save         = TRUE,  ### Whether to save results to file
    overwrite    = TRUE,  ### Whether to overwrite an existing file if saving
    fredi_config = NULL   ### fredi_config list object
){
  ###### Create Workbook ######
  if(save){
    outDir    <- outPath |> file.path("data_tests")
    outFile   <- outDir  |> file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  |> dir.exists()
    if(!odExists){outDir |> dir.create(showWarnings = F)}
    rm(odExists)
    ### Create Excel workbook
    wbook0    <- createWorkbook()
  } ### End if(save)
  
  
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
  cfigName0   <- "configuredData_base_test"
  defParam0   <- "defaultParameters"
  defPlots0   <- "defaultPlots"
  
  
  ###### Reshaped Data ######
  ### Check if reshaped data exists
  rshpExists  <- dataNames |> grepl("rsData", x = _) |> any()
  
  ### Breakout reshaped Data if it exists, then drop from list
  rshp_state0 <- "rshpData_base_test_state"
  rshpData    <- configuredData[["rsData_state"]]
  configuredData[["rsData_state"]] <- NULL
  
  ### If reshapedData exists, check if it's the correct class
  if(rshpExists) {
    ### Check State reshaped data if it exists
    if(!(rshpData |> is.null())){
      class0   <- rshpData |> class()
      is_list0 <- "list" %in% class0
      ### If reshapedData is not a list, message the user
      if(!is_list0) {
        "`reshapedData` must be of class \`list\`..." |> message()
        "\t" |> paste0("Exiting", "...", "\n") |> message()
        return()
      } else {
        "Testing reshaped data ..." |> message()
        ### Get results, add results to list
        rshpResults <- rshpData |> dataInfo_test(save=F, return=T)
        saveList[[rshp_state0]] <- rshpResults
      } ### End if(!is_list0)
      rm(class0, is_list0)
    } ### End if(!(rshpData |> is.null()))
  }  else {
    reshape0 <- data.frame()
  } ### End if(rshpExists)
  
  # ### If no reshapedData passed to argument, try to load from file
  # else          {
  #   expr0    <- reshapedFile |> read.csv()
  #   reshape0 <- try(expr=expr0 |> eval, silent=T)
  #   class0   <- reshape0 |> class()
  #   is_df0   <- "data.frame" %in% class0
  #   ### Exit if unsuccessful
  #   if(!is_df0) {
  #     "Could not load file at `reshapedFile=\`" |> paste0(reshapedFile, "\'`...") |> message()
  #     "\t" |> paste0("Exiting", "...", "\n") |> message()
  #   }
  #   rm(expr0, class0, is_df0)
  # } ### End else(has_data0)
  # ### Remove intermediate objects
  # rm(has_data0, has_file0)
  
  
  ###### Configured Data ######
  ### Check if configuredData exists
  has_data0  <- !(configuredData |> is.null())
  ### If reshapedData exists, check if it's the correct class
  if(has_data0) {
    class0   <- configuredData$frediData$data |> class()
    is_list0 <- "list" %in% class0
    ### If configuredData is not a list, message the user
    if(!is_list0) {
      "`configuredData` must be of class \`list\`..." |> message()
      "\t" |> paste0("Exiting", "...", "\n") |> message()
      return()
    }  else {
      configure0 <- configuredData$frediData$data |> dataInfo_test(save=F, return=T)
    } ### End else(!is_list0)
    rm(class0, is_list0)
  } else {
    configure0 <- data.frame()
  } ### End if(has_reshape0)
  ### Add table to list
  saveList[[cfigName0]] <- configure0
  
  
  ###### Default Values ######
  ### Items from fredi_config:
  listConfig0   <- fredi_config
  c_defaults0   <- c("aggList0" , "minYear", "maxYear", "baseYear0", "rate0")
  c_defaults1   <- c("aggLevels", "minYear", "maxYear", "baseYear" , "rate" )
  n_defaults0   <- listConfig0 |> names()
  w_defaults0   <- (n_defaults0 %in% c_defaults0) |> which()
  ### Filter to specified items, update names
  listConfig1   <- listConfig0[w_defaults0]
  ### Create table
  defaultsList  <- listConfig1 |> names() |> map(function(
    name_i,
    list0 = listConfig1
  ){
    val0_i <- list0[[name_i]]
    type_i <- val0_i |> class() |> paste(collapse=", ")
    val1_i <- val0_i |> paste(collapse=", ")
    df_i   <- tibble(parameter=name_i, class=type_i, value=val1_i)
    return(df_i)
  }) |> bind_rows()
  ### Update parameter names
  defaultsList  <- defaultsList |> mutate(parameter=parameter |> factor(c_defaults0, c_defaults1))
  defaultsList  <- defaultsList |> mutate(parameter=parameter |> as.character())
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
  lab_tmp0  <- expression("CONUS Degrees of Warming ("~degree*C*")")
  lab_yrs0  <- "Year"
  lim_yrs0  <- c(2000, 2300)
  brk_yrs0  <- seq(lim_yrs0[1], lim_yrs0[2], by=20)
  
  ### Temp plot
  temp_plot <- configuredData$frediData$data[["temp_default"]] |>
    ggplot() +
    geom_line(aes(x = year, y = temp_C_conus)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
    scale_y_continuous(lab_tmp0) +
    ggtitle("Default Temperature Scenario")
  ### SLR plot
  slr_plot  <- configuredData$frediData$data[["slr_default"]] |>
    ggplot() +
    geom_line(aes(x = year, y = slr_cm)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
    scale_y_continuous("SLR (cm)") +
    ggtitle("Default SLR Scenario")
  ### GDP Plot: Convert to Billions
  # # if(byState){
  #   gdp_plot <- configuredData$stateData$data[["df_defaultScenario"]] |>
  #     mutate(gdp_usd = gdp_usd / 1e12) |>
  #     ggplot() +
  #     geom_line(aes(x = year, y = gdp_usd)) +
  #     scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
  #     scale_y_continuous("U.S. National GDP (2015$, trillions)") +
  #     ggtitle("Default GDP Scenario")
  # # } else{
  gdp_plot <- configuredData$frediData$data[["gdp_default"]] |>
    mutate(gdp_usd = gdp_usd / 1e12) |>
    ggplot() +
    geom_line(aes(x = year, y = gdp_usd)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
    scale_y_continuous("U.S. National GDP (2015$, trillions)") +
    ggtitle("Default GDP Scenario")
  # }
  
  ### Pop plot 
  # if(byState){
    pop_plot <- configuredData$stateData$data$pop_default |>
      mutate(state_pop = state_pop / 1e6) |>
      ggplot() +
      geom_line(aes(x = year, y = state_pop, color = state), alpha = 0.75) +
      scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
      scale_y_continuous("State Population (millions)") +
      # theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position = "bottom") +
      scale_color_discrete("State") +
      ggtitle("Default Population Scenario")
  # } else{
  #   pop_plot <- configuredData$frediData$data$pop_default |>
  #     mutate(reg_pop = reg_pop / 1e6) |>
  #     ggplot() +
  #     geom_line(aes(x = year, y = reg_pop, color = region), alpha = 0.75) +
  #     scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits=lim_yrs0) +
  #     scale_y_continuous("Regional Population (millions)") +
  #     # theme(axis.text.x = element_text(angle=90)) +
  #     theme(legend.position = "bottom") +
  #     scale_color_discrete("Region") +
  #     ggtitle("Default Population Scenario")
  # }
    
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
  ### Get results
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
  
  ###### Save Outputs ######
  ### Save the workbook, remove workbook
  if(save){
    "Saving data tests" |> paste0("...") |> message()
    wbook0  |> saveWorkbook(file=outFile, overwrite=overwrite)
    rm(wbook0)
    ### Remove temporary image files
    files0 <- outDir |> list.files(pattern="tmp_", full.names=T)
    files0 |> file.remove()
  } ### End if(save)
  
  ###### Return ######
  if(return) return(saveList)
}



###### New Sector Plot Function ######
get_fredi_sectorOptions <- function(
    dataList ### Data list, rDataList produced by createSystemData
){
  ### Assign data objects
  df_sect    <- dataList[["co_sectors"]]
  df_vars    <- dataList[["co_variants"   ]]
  df_types   <- dataList[["co_impactTypes"]]
  df_years   <- dataList[["co_impactYears"]]
  df_regions <- dataList[["co_regions"    ]]
  df_models  <- dataList[["co_models"     ]]
  df_states  <- dataList[["co_states"     ]]
  
  ### State Columns
  stateCols  <- c("state", "postal")
  
  ### Select columns
  ### - Columns
  select0    <- c("sector_id", "variant_label", "variant_id")
  select1    <- c("sector_id", "impactType_label", "impactType_id")
  select2    <- c("sector_id", "impactYear_label", "impactYear_id")
  select3    <- c("model_id", "model_dot", "model_label", "modelType", "maxUnitValue")
  select4    <- c("region_label", "region_dot")
  select5    <- c("region") |> c(stateCols)
  ### - Select
  df_vars    <- df_vars    |> select(all_of(select0))
  df_types   <- df_types   |> select(all_of(select1))
  df_years   <- df_years   |> select(all_of(select2))
  df_models  <- df_models  |> select(all_of(select3))
  df_regions <- df_regions |> select(all_of(select4))
  df_states  <- df_states  |> select(all_of(select5))
  ### - Remove vars
  rm(select0, select1, select2, select3, select4, select5)
  
  ### Rename some names
  df_states  <- df_states  |> rename_at(c("region"), ~"region_label")
  
  ### Join sectors and variants
  df_x       <- df_sect |> left_join(df_vars  , by="sector_id")
  df_x       <- df_x    |> left_join(df_types , by="sector_id", relationship="many-to-many")
  df_x       <- df_x    |> left_join(df_years , by="sector_id", relationship="many-to-many")
  df_x       <- df_x    |> left_join(df_models, by="modelType", relationship="many-to-many")
  
  ### Join with regions & states
  join0      <- c("joinCol")
  join1      <- c("region_label")
  df_regions <- df_regions |> mutate(joinCol=1)
  df_x       <- df_x       |> mutate(joinCol=1) 
  df_x       <- df_x |> left_join(df_regions, by=c(join0), relationship="many-to-many")
  df_x       <- df_x |> left_join(df_states , by=c(join1), relationship="many-to-many")
  df_x       <- df_x |> select(-all_of(join0))
  rm(join0, join1)
  
  ### Summarize at region level if byState==FALSE
  group0     <- df_x |> names()
  df_x       <- df_x |> mutate(state  = (byState == 1) |> ifelse(state , "N/A"))
  df_x       <- df_x |> mutate(postal = (byState == 1) |> ifelse(postal, "N/A"))
  df_x       <- df_x |> group_by_at(c(group0)) |> summarize(n=n(), .groups="drop")
  df_x       <- df_x |> select(-c("n"))
  rm(group0)
  
  ### Get scenario ID
  get_scenario_id <- utils::getFromNamespace("get_scenario_id", "FrEDI")
  rename0    <- c("sector_id", "variant_id", "impactType_id", "impactYear_id", "region_dot", "model_label", "modelType")
  rename1    <- c("sector", "variant", "impactType", "impactYear", "region", "model", "model_type")
  include0   <- c("region") |> c(stateCols) |> c("model")
  df_x       <- df_x |> rename_at(c(rename0), ~c(rename1))
  df_x       <- df_x |> get_scenario_id(include=c(include0))
  rm(rename0, rename1, include0)
  
  ### Rename columns
  rename0    <- c("sector", "variant", "impactType", "impactYear", "region")
  rename1    <- c("sector_id", "variant_id", "impactType_id", "impactYear_id", "region_dot")
  rename2    <- c("sector_label", "variant_label", "impactType_label", "impactYear_label", "region_label")
  df_x       <- df_x |> rename_at(c(rename0), ~c(rename1))
  df_x       <- df_x |> rename_at(c(rename2), ~c(rename0))
  rm(rename0, rename1, rename2)
  
  ### Return
  return(df_x)
}




get_fredi_sectorOptions_results <- function(
    dataList ### Data list, rDataList produced by createSystemData
){
  
  ###### Data Lists ######
  frediData  <- dataList[["frediData"]][["data"]]
  dataList   <- dataList[["stateData"]][["data"]]
  
  ### State Columns
  stateCols <- c("state", "postal")
  
  ###### Sector Options ######
  df0        <- frediData |> get_fredi_sectorOptions()
  # df0 |> glimpse()
  
  ###### Split Data ######
  ### Split sectors by model type
  df_gcm     <- df0 |> filter(model_type=="gcm")
  df_slr     <- df0 |> filter(model_type=="slr")
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
    slrImp     <- dataList[["slrImpacts"]]
    # slrImp |> glimpse(); 
    
    ### Join df_slr with impacts
    rename0    <- c("sector", "variant", "impactType", "impactYear", "region")
    rename1    <- c("sector_id", "variant_id", "impactType_id", "impactYear_id", "region_dot")
    join0      <- rename1 |> c(stateCols) |> c("model_dot", "model_type") |> c("byState")
    slrImp     <- slrImp |> rename_at(c(rename0), ~c(rename1))
    slrImp     <- slrImp |> select(-c("model"))
    # slrImp |> glimpse(); df_slr |> glimpse()
    df_slr     <- df_slr |> left_join(slrImp, by=c(join0))
    rm(join0, slrImp)
    
    ### Add driverValue
    df_slr     <- df_slr |> mutate(driverValue=gsub("cm", "", model_dot) |> as.numeric())
    # df_slr |> glimpse()
    
    ### Relocate columns
    select0    <- c("scenario_id")
    df_slr     <- df_slr |> relocate(all_of(select0))
    
    ### Bind with initial results
    df0        <- df0 |> rbind(df_slr)
    rm(df_slr)
  } ### End if(do_slr)
  
  ###### Do GCM Results ######
  if(do_gcm){
    ### Get function list
    funList    <- dataList[["list_impactFunctions"]]
    funNames   <- funList  |> names()
    nFunctions <- funNames |> length()
    
    ### Create temperature scenario
    ### Execute the impact functions across new sectors (creates a wide tibble)
    ### Add temperatures
    ### Gather scenario values
    keyCols0   <- funNames
    df_temps   <- tibble(temp_C = -1:11)
    df_vals    <- df_temps |> map_df(~ funList |> map_df(exec,.x))
    df_vals    <- df_vals  |> mutate(driverValue = df_temps[["temp_C"]])
    df_vals    <- df_vals |> pivot_longer(
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
    ### Add year
    df_gcm     <- df_gcm |> mutate(year = impactYear |> na_if("N/A"))
    df_gcm     <- df_gcm |> mutate(year = year |> as.numeric())
    ### Bind with initial results
    # df0 |> names() |> print(); df_gcm |> names() |> print()
    df0        <- df0 |> rbind(df_gcm)
    rm(df_gcm)
  } ### End if(do_gcm)
  
  ###### Add Model Type Info ######
  rename0    <- c("modelType_id", "modelUnit_label")
  rename1    <- c("model_type", "modelUnit")
  join0      <- c("model_type")
  df_mTypes  <- frediData[["co_modelTypes" ]]
  df_mTypes  <- df_mTypes  |> rename_at(c(rename0), ~c(rename1))
  df0        <- df0 |> left_join(df_mTypes, by=c(join0))
  rm(rename0, rename1, join0)
  
  ###### Arrange ######
  ### Arrange and add scaled impacts to list of items to save
  arrange0   <- c("sector", "variant", "impactType", "impactYear")
  arrange0   <- c("region") |> c(stateCols) |> c("model_type", "model")
  df0        <- df0 |> arrange_at(c(arrange0))
  rm(arrange0)
  
  ###### Select Columns ######
  select0    <- c("scenario_id", "sector", "variant", "impactType", "impactYear", "region") |> c(stateCols)
  select0    <- select0 |> c("model_type", "model", "scaled_impacts", "modelUnit", "maxUnitValue", "driverValue", "year")
  mutate0    <- c("variant", "impactType", "impactYear")
  df0        <- df0 |> mutate_at(c(mutate0), function(y){y |> na_if("N/A") |> replace_na("NA")})
  df0        <- df0 |> select(all_of(select0))
  df0        <- df0 |> mutate(model_type = model_type |> toupper())
  
  ###### Return ######
  return(df0)
}


#### Plot information by model type
make_scaled_impact_plots <- function(
    df0,    ### Data frame for plot data
    yCol    = "scaled_impacts", ### Column to use for y
    colorCol= "model",          ### Column to color
    options = list(             ### Other options
      lgdTitle   = "Model",
      lgdPos     = "top",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    ) ### End list
){
  ###### Get from FrEDI Namespace ######
  ### Other values
  years      <- df0[["impactYear"]] |> unique()
  models     <- df0[["model_type"]] |> unique()
  ### Data frame to iterate over
  do_gcm     <- "gcm" %in% tolower(models)
  do_slr     <- "slr" %in% tolower(models)
  
  ### Get iteration list
  df_types   <- df0 |> 
    group_by_at(.c("sector", "impactYear", "model_type")) |> 
    summarize(n=n(), .groups="drop") |> select(-c("n")) |>
    mutate(label = sector |> paste0("_", impactYear))
  # df_types |> glimpse()
  
  ### Get list
  # models |> print()
  list0    <- models |> map(function(.x){
    paste0("Creating plots for model type ", .x, "...") |> message()
    df_x      <- df0 |> filter(model_type %in% c(.x))
    # df_x |> glimpse()
    ### Sectors
    types_x   <- df_types |> filter(model_type==.x)
    sectors_x <- types_x[["sector"]]
    ### Get X column
    xCol_x    <- ((.x |> tolower()) == "gcm") |> ifelse("driverValue", "year")
    # df_types |> glimpse()
    pList_x   <- list(x1=types_x[["sector"]], x2=types_x[["impactYear"]])
    ### Iterate over list
    list_x    <- pList_x |> pmap(function(x1, x2){
      x1 |> paste0("_", x2) |> print()
      df_y   <- df0  |> filter(sector == x1)
      df_y   <- df_y |> filter(impactYear %in% x2)
      
      ### Make plots
      plot_y <- df_y |> create_scaledImpact_plots(
        sector    = x1,
        modelType = .x,
        yCol      = yCol,
        xCol      = xCol_x,
        colorCol  = colorCol,
        # byState   = byState,
        silent    = TRUE,
        options   = options
      ) ### End create_scaledImpact_plots
      # plot_y |> names() |> print()
      ### Return
      return(plot_y)
    })
    ### Add names
    labels_x <- types_x[["label"]]
    list_x   <- list_x |> set_names(labels_x)
    ### Return
    return(list_x)
  })
  ### Add names
  list0   <- list0 |> set_names(models)
  ### Return
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
    save_gcm <- plots0 |> save_scaled_impact_figures(df0=results0, modelType="GCM", fpath=fpath)
    save_slr <- plots0 |> save_scaled_impact_figures(df0=results0, modelType="SLR", fpath=fpath)
  } ### End if save
  
  ### Return plot list
  return(return0)
}



###### save_scaled_impact_figures ######
### Wrapper function to help save appendix figures to file
save_scaled_impact_figures <- function(
    plotList, ### List of plots
    df0,      ### Dataframe used to create plots
    modelType = "GCM", ### Or SLR
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
  df0       <- df0  |> filter(model_type %in% modelType)
  list0     <- plotList[[modelType]]
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
    c_types   <- df_x[["impactType"]] |> unique()
    c_regions <- df_x[["region"    ]] |> unique()
    c_states  <- df_x[["state"     ]] |> unique()
    c_models  <- (df_x |> filter(!is.na(scaled_impacts)))[["model"]] |> unique()
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




###### New Sector Configuration Tests ######
#' configTest_newSectors
#'
#' @param newData
#' @param refDataFile
#' @param outPath
#' @param xslxName
#' @param save
#' @param return
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
newSectors_config_test <- function(
    newData     = NULL,  ### Data list with new data
    refDataFile = "." |> file.path("data", "sysdata.rda"), ### Path to old data for comparison
    outPath     = ".",     ### Path to save outputs
    xslxName    = "newSectorsConfig_testResults.xlsx",     ### Name of test file
    byState     = FALSE, ### Whether results are by state (deprecated)
    return      = TRUE,  ### Whether to return results
    save        = TRUE,  ### Whether to save results
    overwrite   = TRUE   ### If save is true, whether to overwrite file
){
  ###### Create Workbook ######
  if(save){
    outDir    <- outPath |> file.path("data_tests")
    outFile   <- outDir  |> file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  |> dir.exists()
    if(!odExists){outDir |> dir.create(showWarnings = F)}
    rm(odExists)
    ### Create Excel workbook
    wbook0    <- createWorkbook()
  } ### End if(save)
  
  
  ###### Initialize Save List ######
  saveList  <- list()
  
  
  ###### Data Names ######
  ### Names of objects to save
  c_config0 <- "tests"
  c_diff0   <- "tests_diffs"
  c_impact0 <- "scaledImpacts_values"
  c_plots0  <- "scaledImpacts_plots"
  
  
  ###### Load Reference Data ######
  ### Load ref data
  newEnv  <- new.env()
  refDataFile |> load(verbose = F, envir=newEnv)
  # ls(envir=newEnv) |> print()
  refData <- "rDataList" |> get(envir=newEnv, inherits = F)
  # ls() |> print(); refData() |> names() |> print()
  refFunList <- refData[["list_impactFunctions"]]
  rm(newEnv)
  # return(refData)
  
  
  ###### Format New Data ######
  newFunList <- newData[["list_impactFunctions"]]
  # return(refData)
  
  
  ###### Table Info ######
  ### Create table of status, rename and drop some columns
  ### Mutate values for changes_expected
  levels0   <- c("No", "Maybe", "Yes")
  mutate0   <- c("changes_expected")
  df_status <- newData[["testDev"]]
  df_status <- df_status |> rename_at(c("Changes.if.new.sector.added"), ~mutate0)
  df_status <- df_status |> mutate_at(c(mutate0), factor, levels=levels0)
  rm(mutate0, levels0)
  
  
  ###### Compare New & Ref Data ######
  ###### ** Get Test Info ######
  ### Get test info for new and old data
  newTests  <- newData |> dataInfo_test(save=F, return=T)
  refTests  <- newData |> dataInfo_test(save=F, return=T)
  ### Select appropriate columns and join old and new test info
  join0     <- c("table")
  sum0      <- c("num_cols", "num_rows")
  rename0   <- c("numCols" , "numRows" )
  select0   <- join0 |> c("itemClass", sum0)
  select1   <- join0 |> c(sum0)
  suffix0   <- c("_new", "_ref")
  ### Select columns
  newTests  <- newTests |> select(all_of(select0))
  refTests  <- refTests |> select(all_of(select1))
  ### Rename columns
  newTests  <- newTests |> rename_at(c(sum0), ~rename0)
  refTests  <- refTests |> rename_at(c(sum0), ~rename0)
  ### Join old and new
  df_tests  <- newTests |> left_join(refTests, by=c(join0), suffix=suffix0)
  rm(join0, sum0, select0, select1, rename0); rm(newTests, refTests)
  
  
  ###### ** Join Tests and Test Info ######
  ### Join df_tests with df_status
  join0     <- c("Table.Name")
  rename0   <- c("table")
  ### Check number of rows before
  dim0      <- c(nrow(df_status), nrow(df_tests))
  ### Rename columns and join columns
  df_tests  <- df_tests  |> rename_at(c(rename0), ~join0)
  df_status <- df_status |> left_join(df_tests, by=c(join0))
  ### Check number of rows before
  dim1      <- c(nrow(df_status), nrow(df_tests))
  all0      <- (dim1 == dim0) |> all()
  rm(join0, rename0, all0); rm(df_tests)
  
  
  ###### ** Compare Values ######
  ### Could filter on `table_test` columns if different tests required in the future
  ### When no changes are expected still get dimensions and check that values are identical
  # df_status |> names() |> print()
  df_status <- df_status |> mutate(sameDims = 1 * ((numCols_new == numCols_ref) & (numRows_new == numRows_ref)))
  ### Check values
  checkVals <- df_status |> nrow() |> seq_len() |> map(function(
    i, 
    df1 = newData, 
    df2 = refData
  ){
    ### Names
    name_i  <- df_status[["Table.Name"]][i]
    df1_i   <- df1[[name_i]]
    df2_i   <- df2[[name_i]]
    
    ### Check whether to check values
    skip_i  <- ("list" %in% class(df1_i)) | df1_i |> is.null() | df2_i |> is.null()
    check_i <- !skip_i
    ### Initialize return value
    y_i     <- NA
    if(check_i) {y_i <- 1 * identical(df1_i, df2_i)}
    return(y_i)
  }) |> unlist()
  # checkVals |> print()
  df_status <- df_status |> mutate(sameVals = checkVals)
  df_status <- df_status |> mutate(hasDiffs = 1 * (!sameDims | !sameVals))
  rm(checkVals)
  
  
  ###### ** Arrange Test Results ######
  ### Arrange values and add to save list
  arrange0  <- c("changes_expected", "hasDiffs", "sameDims", "sameVals", "Table.Name")
  df_status <- df_status |> arrange_at(c(arrange0))
  saveList[[c_config0]] <- df_status
  rm(arrange0)
  
  
  ###### Create Workbook ######
  ### Create workbook if(save)
  ### Add worksheet with test info
  if(save){
    wbook0  <- createWorkbook()
    sheet0  <- c_config0
    wbook0 |> addWorksheet(sheetName = sheet0)
    wbook0 |> writeDataTable(sheet = sheet0, x = df_status)
    rm(sheet0)
  } ### End if(save)
  
  
  ###### Print Test Results ######
  ### Filter to tables with differences and add to list and workbook
  df_diff <- df_status |> filter(hasDiffs == 1)
  saveList[[c_diff0]] <- df_diff
  # df_diff |> glimpse()
  
  ### Iterate over names of tables with differences:
  ### - Add tables with differences to list
  ### - Write tables with differences to xlsx workbook
  names0  <- df_diff[["Table.Name"]]
  names0 |> walk(function(
    name_i,
    new0=newData[[name_i]],
    ref0=refData[[name_i]]
  ){
    ### Worksheet/list name
    sheet0 <- name_i |> paste("diff", sep="_")
    
    ### Get difference
    join0  <- new0 |> names() |> (function(y, z=ref0){y[(y %in% names(z))]})()
    diff0  <- new0 |> anti_join(ref0, by=c(join0))
    rm(join0)
    
    ### Add table to list
    saveList[[sheet0]] <- diff0
    
    ### Add worksheet and write data table if(save)
    if(save) {
      wbook0 |> addWorksheet(sheetName = sheet0)
      wbook0 |> writeDataTable(sheet = sheet0, diff0)
    } ### End if(save)
  }) ### End function(name_i), end walk
  
  
  ###### Save Workbook ######
  if(save){
    "Saving new sector results" |> paste0("...") |> message()
    wbook0  |> saveWorkbook(file=outFile, overwrite=overwrite)
    rm(wbook0)
  } ### End if(save)
  
  
  ###### Return ######
  if(return) return(saveList)
} 
### End function


###### End of Page ######


