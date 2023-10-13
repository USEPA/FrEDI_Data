###### Overview ######
### This file contains helper functions for FrEDI's state-level functionality.

###### loadStateScaledImpacts ######
### Load state scaled impact data from a specified directory.

loadStateScaledImpacts <- function(
    fpath   = "." %>% file.path("inst", "extdata", "state"), # file path to directory with slr and gcm folders containing scaled impacts
    sectors = c() # list of sectors with state-level data available
){
  gcm_col_names <- c("state", "postal", "sector", "variant", "impactType", "model", "modelUnitValue", "impactYear", "value")
  slr_col_names <- c("state", "postal", "sector", "variant", "impactType", "model", "year", "value")
  slr_sectors   <- sectors[sectors %in% c("HTF", "CoastalProperties")]
  gcm_sectors   <- sectors[!(sectors %in% c("HTF", "CoastalProperties"))]
  
  gcm_sector_data_list <- gcm_sectors %>%
    map(function(sector){
      sector_data <- fpath %>%
        file.path("gcm", paste0(sector, "_scaledImpacts.csv")) %>%
        read.csv %>%
        select(all_of(gcm_col_names)) %>%
        as_tibble
    })
  
  slr_sector_data_list <- slr_sectors %>%
    map(function(sector){
      sector_data <- fpath %>%
        file.path("slr", paste0(sector, "_scaledImpacts.csv")) %>%
        read.csv %>%
        select(all_of(slr_col_names)) %>%
        as_tibble
    })
  
  gcm_sector_data <- gcm_sector_data_list %>%
    bind_rows
  
  slr_sector_data <- slr_sector_data_list %>%
    bind_rows
  
  sector_data <- list(gcm = gcm_sector_data, slr = slr_sector_data)
  
  return(sector_data)
}


###### loadStateScalars ######
### Load state scalar data from a specified directory.

loadStateScalars <- function(
    fpath = "." %>% file.path("inst", "extdata", "state") # file path to directory with scalars folder containing scalars
){
  scalar_col_names <- c("state", "postal", "scalarName", "year", "value")
  fnames           <- fpath %>% file.path("scalars") %>% list.files
  
  scalar_data_list <- fnames %>%
    map(function(file){
      scalar_data <- fpath %>%
        file.path("scalars", file) %>%
        read.csv %>%
        select(all_of(scalar_col_names)) %>%
        as_tibble
    })
  
  scalar_data <- scalar_data_list %>%
    bind_rows
  
  return(scalar_data)
}


###### loadStateData ######
### Load state scalar and scaled impacts data from a specified directory.

loadStateData <- function(
    fpath   = "." %>% file.path("inst", "extdata", "state"), # file path to directory with folders containing data
    sectors = c()
){
  scalars        <- fpath %>% loadStateScalars
  scaled_impacts <- fpath %>% loadStateScaledImpacts(sectors = sectors)
  
  state_data <- list(df_stateScalars    = scalars %>% as.data.frame,
                     df_gcmStateImpacts = scaled_impacts$gcm %>% as.data.frame,
                     df_slrStateImpacts = scaled_impacts$slr %>% as.data.frame)
  
  return(state_data)
}