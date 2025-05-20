load_fredi_db <- function(
    fredi_db_path ## path to Fredi DB data
    ){
  
  files <- list.files(fredi_db_path)
  ### check if db has been unzipped
  unzip_check <- "fredi_data" %in% files 
  
  ## If needs to be unzipped, unzip it
  if(!unzip_check){
    unzip(zipfile = file.path(fredi_db_path,files))
    
    fredi_db <- file.path(fredi_db_path,"fredi_data")
  } else {
    fredi_db <- file.path(fredi_db_path,"fredi_data")
  }
  
  con <-  DBI::dbConnect(RSQLite::SQLite(), fredi_db)
  
  return(con)
}
