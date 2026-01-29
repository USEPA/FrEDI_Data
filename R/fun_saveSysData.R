### Save Objects to sys data
fun_saveSysData <- function(
    dataDir     = "." |> file.path("data"),
    # controlFile = "controlData",
    # scenarioDir = "scenarios",
    outFile     = "sysdata",
    modules     = c("ghg", "sv"),
    extStrs     = c("rda", "rds")
){
  ### Extensions
  strSep0    <- "|"
  dotStr0    <- "\\."
  extStr0    <- extStrs |> paste(collapse=strSep0)
  extStr1    <- dotStr0 |> paste0(extStrs) |> paste(collapse=strSep0)
  #browser()
  # ### Load scenario files
  # scenDir    <- dataDir   |> file.path(scenarioDir)
  # scenFiles  <- scenDir   |> list.files(pattern=extStr0, full.names=F)
  # scenNames  <- scenFiles |> str_replace(pattern=extStr1, "")
  ### - Module Files
  modFiles   <- dataDir |> file.path(modules) |>
    map(list.files, pattern=extStr0, full.names=F) |>
    set_names(modules)
  modNames   <- modFiles |>
    map(str_replace, pattern=extStr1, "") |>
    unlist()
  # modFiles |> print()
  # modNames |> unlist() |> print()

  ### Data names
  # dataNames  <- c(controlFile, scenNames) |> c(modNames |> unlist())
  dataNames  <- modNames |> unlist()

  # ### Load data
  # ### - Load control data
  # dataDir |> list.files() |> print()
  # ctrlPath   <- dataDir |> list.files(pattern=controlFile, full.names=T)
  # ctrlPath |> load()
  # # ctrlPath |> print()
  # 
  # ### - Scenarios
  # for(file_j in scenFiles){
  #   scenDir |> file.path(file_j) |> load()
  # }


  ### - Module data
  for(mod_i in modules){
    files_i <- modFiles[[mod_i]]
    for(file_j in files_i){
      dataDir |> file.path(mod_i, file_j) |> load()
    }
  }
  
  
  ### Save data
  #outPath <- dataDir |> file.path(outFile) |> paste0(".", "rda")
  #save(list=c(dataNames), filSe=outPath)
  ### Update Database
  db_tmp_path <- file.path(dataDir, "fredi")
  con <- load_fredi_db(fredi_db_path = db_tmp_path)
  
  for (dat_i in dataNames){
  DBI::dbExecute(conn = con, paste0("DROP TABLE IF EXISTS ",dat_i))
  DBI::dbExecute(conn = con, paste0("CREATE TABLE ",dat_i," (value BLOB)"))
  DBI::dbExecute(con,paste0("INSERT INTO ",dat_i," (value) VALUES (:value)"), 
                 params = list(value = list(serialize(eval(parse(text = dat_i)), connection = NULL)))
  )
  }
  
  DBI::dbDisconnect(con)
  
  outPath <- dataDir |> file.path(outFile)
  zip(zipfile = outPath,files = file.path(db_tmp_path,"fredi_data") )
  
  
  ### Return
  return("Success")
}
