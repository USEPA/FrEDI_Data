### Created 2022.02.25
### The purpose of this function is to assist in adding SV objects to FrEDI sysdata.rda
update_sysdata <- function(
  projectPath = ".", ### Path to project
  dataPath    = "." |> file.path("data"), 
  mainFile    = "sysdata.rda", ###
  sv          = TRUE , ### Whether to update SV, population, formatting info
  svPath      = dataPath |> file.path("sv"),
  svExt       = "rda", ### r Object Extension
  silent      = FALSE, ### Whether to message the user
  save        = TRUE , ### Whether to save
  return      = TRUE   ### Whether to return
){
  # require(tidyverse)
  paste0("Running update_sysdata():", "\n") |> message()
  
  ###### Variables ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser     <- !silent
  
  
  ###### Initialize List ######
  dataList    <- list()
  

  ###### File Paths ######
  ### Directories
  frediPath   <- system.file(package="FrEDI")
  hasProject  <- !(projectPath |> is.null())
  hasDataPath <- !(dataPath    |> is.null())
  hasSvPath   <- !(svPath      |> is.null())
  projectPath <- hasProject  |> ifelse(projectPath, ".")
  dataPath    <- hasDataPath |> ifelse(dataPath, projectPath |> file.path("data")) 
  svPath      <- hasSvPath   |> ifelse(svPath  , dataPath    |> file.path("sv"))
  ghgPath     <- projectPath |> file.path("ghg")
  
  ### Main FrEDI system data
  sysDataName <- mainFile
  sysDataPath <- dataPath |> file.path(sysDataName)
  
  ### Create tibble with SV files
  ### Add file name and file path
  df_sv       <- tibble(
    file   = c("svDataList", "svPopData", "format_styles"),
    object = c("svDataList", "svPopList", "format_styles")
  ) |> ### End tibble
    mutate(fileName = file   |> paste0(".", svExt)) |> 
    mutate(filePath = svPath |> file.path(fileName))
  
  ### Data extensions
  svDataExtLC <- svExt |> tolower()
  doRda       <- "rds" %in% svDataExtLC
  old_ext     <- "." |> paste0(svExt)
  new_ext     <- "." |> paste0("rda")
  # new_ext     <- doRda    |> ifelse("rda", svExt)

  
  ###### Load Main FrEDI Data ######
  ### sysdata.rda
  ### Check file exists:
  ### - If the file exists, load it into a new environment, then data list
  ### - Otherwise, message the user
  hasFile     <- sysDataPath |> file.exists()
  if(hasFile) {
    # dataList <- sysDataPath |> (function(x){admisc::objRDA(x)})()
    # dataList |> print()
    # sysDataPath |> load()
    sysDataPath |> load(temp_env <- new.env())
    dataList <- temp_env |> as.list()
    rm(temp_env)
    dataList |> names() |> print(); dataList$rDataList |> names() |> print()
  } else{
    ### Message user
    paste0("Warning: File '", sysDataPath, "' does not exist!") |> print()
  } ### End if(hasFile)
  
  ###### SV Data  ######
  ### Load SV data objects: svDataList, svPopData, format_styles 
  if(sv) {
    ### Object names
    svNames0  <- df_sv |> pull(object)
    ### Iterate over paths and load objects
    svList    <- df_sv |> as.list() |> pmap(function(file, object, fileName, filePath){
      file |> print()
      ### Initialize list
      list0 <- list()
      ### Check that the file exists
      hasFile <- filePath |> file.exists()
      if(!hasFile) paste0("Warning: File '", filePath, "' does not exist!") |> print()
      ### Load file into a new environment and then into a list
      if(hasFile) {
        filePath |> load(temp_env <- new.env())
        list0  <- temp_env |> as.list()
        # list0 |> names() |> print()
        rm(temp_env)
      } ### End if(hasFile)
      ### Return
      return(list0)
    }) |> set_names(svNames0)
    # test1 <- list(a=list(x=list(m=1, n=2), o=2), b=list(y=1), c=list(z=3)); test1
    # test2 <- test1 |> unlist(recursive=F); test2; test2 |> names()
    # test1 |> names() |> paste0("\\.") |> paste(collapse="|")
    # test2 |> names() |> str_replace(test1 |> names() |> paste0("\\.") |> paste(collapse="|"), "")
    ### Unlist one level
    svList   <- svList   |> unlist(recursive=F)
    svNames1 <- svList   |> names()
    ### Standardize names
    svStr    <- svNames0 |> paste0("\\.") |> paste(collapse="|")
    svNames2 <- svNames1 |> str_replace(svStr, "")
    svList   <- svList   |> set_names(svNames2)
    ### Add list to existing list
    dataList <- dataList |> c(svList)
    svNames0 |> print(); svNames1 |> print(); svNames2 |> print(); dataList |> names() |> print()
  } ### End if(sv)
  
  
  ###### Save Files ######
  ### Save the data and return
  if(save) {
    ### Message user
    paste0("Saving data to file '", sysDataPath, "'...") |> print()
    ### Object Names and string
    dataNames <- dataList  |> names()
    namesStr  <- dataNames |> paste(collapse="|")
    ### Assign objects to names in environment
    for(name_i in dataNames) {name_i |> assign(dataList[[name_i]]); rm(name_i)}
    ### Save files
    eval(substitute(save(list=ls(pattern=x), file=y), list(x=namesStr, y=sysDataPath)))
  } ### End if(save)
  
  ###### Return ######
  ### Return
  message("\n", "Finished", ".")
  if(return) return(dataList)
  else       return()
}


###### End Script ######
