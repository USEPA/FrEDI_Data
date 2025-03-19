### Load and format FrEDI configuration file
loadFrediConfig <- function(
    configDir   = "." |> file.path("inst", "extdata", "fredi"), ### Path to Excel config file
    configFile  = "FrEDI_config.xlsx", ### Name of Excel file with config information, relative to frediDir
    configSheet = "tableNames",        ### Name of sheet in configFile containing table with info about tables
    silent      = FALSE,               ### Level of messaging
    msg0        = 0                    ### Messaging index level
) {
  ###### Messaging ######
  msgUser       <- !silent
  msgN          <- "\n"
  # msg1          <- msg0 |> paste("\t")  
  msg1 <- msg0 + 1
  msg2 <- msg0 + 2
  if(msgUser) msg0 |> get_msgPrefix(newline=F) |> paste0("Running loadFrediConfig()...") |> message()
  # if(msgUser) msg0 |> get_msgPrefix(newline=T) |> paste0("Running loadFrediConfig...") |> message()
  
  ###### File Paths ######
  ### Config file
  configPath    <- configDir |> file.path(configFile)
  if(msgUser) {configDir |> c(configFile) |> print(); configDir |> list.files() |> print(); configSheet |> print()}
  
  ###### Load Table of Tables ######
  ### Load table with names of data tables
  # frediFile |> print()
  df_tables     <- configPath |> openxlsx::read.xlsx(
      sheet    = configSheet,
      rowNames = T,
      startRow = 2
    ) ### End read.xlsx
  
  ### Filter to those for importing and replace NA values in some columns
  mutate0       <- c("excludeCol_ids", "Notes")
  df_tables     <- df_tables |> filter(Import == 1)
  df_tables     <- df_tables |> mutate_at(c(mutate0), replace_na, "")
  rm(mutate0)
  
  ### Number of data tables
  nTables       <- df_tables |> nrow()
  
  ###### Load Each Table ######
  ### Iterate over the list of data tables: Read them them and add them to the list of tables
  ### Initialize the list
  dataList      <- list()
  tableNames    <- df_tables |> pull(Table.Name)
  for (name_i in tableNames) {
    ### Message the user
    if (msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("Importing table '", name_i, "' from Excel...") |> message()
    ### Subset table info
    info_i      <- df_tables |> filter(Table.Name == name_i)
    sheet_i     <-  info_i |> pull(Worksheet)
    cols_i      <- (info_i |> pull(id_colIndex)) + (0:(info_i |> pull(num_tableCols)))
    rows_i      <- (info_i |> pull(Header.Row )) + (0:(info_i |> pull(Number.of.Data.Rows)))
    ### Read in the table
    table_i     <- configPath |> openxlsx::read.xlsx(
      colNames   = T,
      rowNames   = T,
      sheet      = sheet_i,
      cols       = cols_i,
      rows       = rows_i,
      na.strings = ""
    ) |> as_tibble() ### End read.xlsx
    
    ### Exclude some columns
    ### Columns to exclude by splitting values in `excludeCol_ids` then adjust for row ID in Excel
    ### Exclude the relevant columns
    # tableInfo_i$excludeCol_ids |> print()
    excludeIds_i  <- info_i |> pull(excludeCol_ids)
    doExclude_i   <- !(excludeIds_i == "")
    if (doExclude_i) {
      excludeCols_i <- excludeIds_i |> str_split(", ") |> unlist() |> as.numeric()
      excludeCols_i <- excludeCols_i - 1
      table_i       <- table_i |> select(-all_of(excludeCols_i))
      rm(excludeCols_i)
    } ### End if (doExclude_i) 
    rm(excludeIds_i, doExclude_i)
    
    ### Add the table to the list
    dataList[[name_i]] <- table_i
    
    ### Remove intermediate values
    rm(table_i, info_i, name_i)
  } ### End for (i in 1:nTables)
  
  ###### Return ######
  if(msgUser) msg1 |> get_msgPrefix(newline=F) |> paste0("...Fnished running loadFrediConfig().") |> message()
  if(msgUser) msg1 |> get_msgPrefix(newline=T) |> message()
  return(dataList)
}