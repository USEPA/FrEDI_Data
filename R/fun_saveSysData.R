### Save Objects to sys data
fun_saveSysData <- function(
    dataDir     = "." |> file.path("data"),
    # controlFile = "controlData",
    # scenarioDir = "scenarios",
    outFile     = "tmp_sysData",
    modules     = c("fredi", "ghg", "sv"),
    extStrs     = c("rda", "rds")
){
  ### Extensions
  strSep0    <- "|"
  dotStr0    <- "\\."
  extStr0    <- extStrs |> paste(collapse=strSep0)
  extStr1    <- dotStr0 |> paste0(extStrs) |> paste(collapse=strSep0)

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
  outPath <- dataDir |> file.path(outFile) |> paste0(".", "rda")
  save(list=c(dataNames), file=outPath)

  ### Return
  return("Success")
}
