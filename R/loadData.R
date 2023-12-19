### Last updated: 2022.01.10
### 2022.01.10: Added SLR sectors
### This function loads the data from a directory specified by the user. There is a default directory structure it will try to load from.
loadData <- function(
    fileDir   = "." |> file.path("inst", "extdata"), ### Path to project
    fileName  = "FrEDI_config.xlsx", ### name of excel file with config information
    sheetName = "tableNames",
    byState   = FALSE,
    silent    = FALSE
) {
  # if(is.null(sheetName)){sheetName <- "tableNames"}
  # print(getwd())
  if (silent |> is.null()) {silent <- F}
  
  ###### File Path ######
  #fileExt       <- "xlsx"
  #fileName      <- fileName |> paste0(".", fileExt)
  filePath      <- fileDir |> file.path(fileName)
  ###### Load Table of Tables ######
  ### Load table with names of data tables
  # print(filePath)
  
  df_tableNames <- openxlsx::read.xlsx(
    filePath,
    sheet    = sheetName,
    rowNames = T,
    startRow = 2
  )
  ### Filter to those for importing and replace NA values in some columns
  mutate0 <- c("excludeCol_ids", "Notes")
  df_tableNames <- df_tableNames |> filter(Import == 1)
  df_tableNames <- df_tableNames |> mutate_at(.vars = c(mutate0), replace_na, "")
  rm("mutate0")
  
  ### Number of data tables
  num_dataTables <- df_tableNames |> nrow()
  
  ###### Load Each Table ######
  ### Iterate over the list of data tables: import them and add them to the list of tables
  ### Initialize the list
  dataList       <- list()
  tableNames     <- df_tableNames[["Table.Name"]]
  
  for (i in 1:num_dataTables) {
    tableName_i <- tableNames[i]
    ### Message the user
    if (!silent) {
      message("\t\t", "Importing table '", tableName_i, "' from Excel...")
    }
    ### Subset table info
    tableInfo_i <- df_tableNames[i, ]
    ### Read in the table
    table_i     <- openxlsx::read.xlsx(
      filePath,
      colNames = T,
      rowNames = T,
      sheet = tableInfo_i$Worksheet,
      cols  = tableInfo_i$id_colIndex + 0:tableInfo_i$num_tableCols,
      rows  = tableInfo_i$Header.Row  + 0:tableInfo_i$Number.of.Data.Rows,
      na.strings = ""
    )
    
    ### Exclude some columns
    ### Columns to exclude by splitting values in `excludeCol_ids` then adjust for row ID in Excel
    ### Exclude the relevant columns
    # tableInfo_i$excludeCol_ids |> print
    excludeIds_i  <- tableInfo_i[["excludeCol_ids"]]
    doExclude_i   <- !(excludeIds_i == "")
    if (doExclude_i) {
      excludeCols_i <- excludeIds_i |> str_split(", ") |> unlist() |> as.numeric()
      excludeCols_i <- excludeCols_i - 1
      table_i       <- table_i |> select(-c(all_of(excludeCols_i)))
      rm("excludeCols_i")
    }
    rm("excludeIds_i", "doExclude_i")
    
    ### Add the table to the list
    # dataList[[i]] <- table_i
    dataList[[tableName_i]] <- table_i
    
    ### Remove intermediate values
    rm("table_i", "tableInfo_i", "tableName_i", "i")
  } ### End lapply

  ###### Set Aside GDP Default ######
  ### Default scenario
  gdp_default   <- dataList[["co_defaultScenario"]]
  gdp_default   <- gdp_default |> select(c("year", "gdp_usd"))
  dataList[["gdp_default"]] <- gdp_default
  
  ###### Load State Data ######
  if (byState) {
    # ### State sectors
    state_sectors <- dataList     [["co_sectors"]] |> filter(byState == 1)
    state_sectors <- state_sectors[["sector_id" ]] |> unique()
    ### Load state data
    state_fpath   <- fileDir     |> file.path("state")
    state_data    <- state_fpath |> loadStateData(sectors = state_sectors)
    ### Replace region values with state data
    dataList[["data_scaledImpacts"]] <- state_data[["df_gcmStateImpacts"]]
    dataList[["slrImpacts"        ]] <- state_data[["df_slrStateImpacts"]]
    dataList[["scalarDataframe"   ]] <- state_data[["df_stateScalars"   ]]
    dataList[["co_defaultScenario"]] <- state_data[["df_statePop"       ]]
  } ### End if(byState)
  
  ###### Return ######
  return(dataList)
}