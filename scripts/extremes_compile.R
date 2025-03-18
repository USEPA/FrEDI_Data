### Directories
projectDir  <- "." ### Directory in which to find the FrEDI_Data project/package
dataDir     <- "." |> file.path("inst", "extdata") ### Path to projectDir
configDir   <- "extremes"  ### Module directory relative to dataDir
outDir      <- "data" |> file.path("extremes") ### Directory to save outputs relative to projectDir
projectDir |> devtools::load_all() 
frediDir <- projectDir|> file.path("..", "FrEDI")
frediDir |> devtools::load_all()

### Info on config file, output file
configFile  <- "extremes_config"  |> paste0(".", "xlsx") ### Name of excel file with config information
outFile     <- "tmp_extremesData" |> paste0(".", "rda" ) ### Name of file to which to save results

### Load scripts
scriptDir   <- projectDir |> file.path("scripts")
scriptDir |> file.path("configureFrediData.R") |> source()
#projectDir |> devtools::load_all() 

### Run scripts
#projectDir |> devtools::load_all() 
test0      <- projectDir |> configureFrediData(
  dataDir     = dataDir,
  configDir   = configDir,
  configFile  = configFile,
  reshape     = TRUE , ### Whether to include reshaped data in outputs (e.g., for testing)
  extend_all  = FALSE, ### Whether to extend all GCM model observations to maximum range
  doScalars   = FALSE, ### Whether or not do format scalars
  doScenarios = FALSE, ### Whether to load scenarios
  outDir      = outDir,
  outFile     = outFile,
  save = T
) ### End configureFrediData

### List files
projectDir |> file.path(outDir) |> list.files() |> print()
