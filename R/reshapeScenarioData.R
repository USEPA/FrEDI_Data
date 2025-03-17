#' reshapeScenariosData
#'
#' @param scenarioData List with scenario data (as output from `loadScenarioData`)
#' @param silent Indicate level of messaging
#' @param msg0 Prefix for messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeScenarioData <- function(
    # dfInfo, ### Table with scenario info
    # scenarioData = NULL, ### List with scenario data
    scenarioList = NULL, ### List of scenarios by type, output of loadScenarioData
    inputInfo    = NULL, ### List with input info
    path0        = "." |> file.path("scenarios"), ### Path relative to other data
    popRatioName = "state_population_ratios", ### File name for popRatio
    silent       = TRUE, ### Level of messaging
    msg0         = "\t"  ### Prefix for messaging
) {
  ### Messaging ----------------
  msgN          <- "\n"
  msg1          <- msg0 |> paste("\t")  
  if (!silent) paste0(msg0, "In reshapeScenarioData:"   ) |> message()
  if (!silent) paste0(msg1, "Reshaping scenario data...") |> message()
  
  ### Get Unique Values for Data ----------------
  dfFiles     <- dataList[["co_scenarios"]]
  dfInfo      <- dataList[["co_inputInfo"]]
  types0      <- dfInfo |> pull(inputName) |> unique()
  
  ### Join file paths and info
  join0       <- "inputName"
  dfFiles     <- dfFiles |> left_join(dfInfo, by=join0)
  
  # ### Add region to scalar data frame
  # gcamData   <- scenarioData[["gcamData"     ]]
  # gdpData    <- scenarioData[["gdpData"      ]]
  # popData    <- scenarioData[["popData"      ]]
  # ratiosData <- scenarioData[["popRatiosData"]]
  # ratiosData |> glimpse()
  
  ### Replace data with NA values
  gcamData   <- gcamData   |> distinct()
  gdpData    <- gdpData    |> distinct()
  popData    <- popData    |> distinct()
  ratiosData <- ratiosData |> distinct()
  # ratiosData |> glimpse()
  
  ### Format data
  popData    <- popData    |> mutate(region = region |> str_replace("\\.", " "))
  ratiosData <- ratiosData |> mutate(region = region |> str_replace("\\.", " "))
  
  ### Update in List
  scenarioData[["gcamData"     ]] <- gcamData
  scenarioData[["gdpData"      ]] <- gdpData
  scenarioData[["popData"      ]] <- popData
  scenarioData[["popRatiosData"]] <- ratiosData
  # scenarioData[["popRatiosData"]] |> dim() |> print()
  
  ### GCAM Scenarios ----------------
  ### Get reference years and add to fredi_config
  if(msgUser) msg2 |> paste0("Formatting GCAM scenarios...") |> message()
  # co_modelTypes |> glimpse()
  refYear0       <- co_modelTypes |> pull(modelRefYear) |> min()
  ### Default temperature scenario
  ### Columns, years for interpolation
  gcamData       <- scenarios[["gcamData"]]
  # gcamData |> glimpse()
  gcam_scenarios <- gcamData       |> format_gcamData()
  gcam_default   <- gcam_scenarios |> filter(year >= refYear0) |> filter(scenario == "ECS_3.0_REF")
  ### Add to list, remove intermediate values
  scenarios[["gcam_scenarios"]] <- gcam_scenarios
  scenarios[["gcam_default"  ]] <- gcam_default
  rm(refYear0, gcamData)
  
  ### GCAM Scenarios ----------------
  # ### Get reference years and add to fredi_config
  # if(msgUser) msg2 |> paste0("Formatting GCAM scenarios...") |> message()
  # # co_modelTypes |> glimpse()
  # refYear0       <- co_modelTypes |> pull(modelRefYear) |> min()
  # ### Default temperature scenario
  # ### Columns, years for interpolation
  # gcamData       <- scenarios[["gcamData"]]
  # # gcamData |> glimpse()
  # gcam_scenarios <- gcamData       |> format_gcamData()
  # gcam_default   <- gcam_scenarios |> filter(year >= refYear0) |> filter(scenario == "ECS_3.0_REF")
  # ### Add to list, remove intermediate values
  # scenarios[["gcam_scenarios"]] <- gcam_scenarios
  # scenarios[["gcam_default"  ]] <- gcam_default
  # rm(refYear0, gcamData)
  # 
  # 
  ### Socioeconomic Scenario ----------------
  # # if(msgUser) msg2 |> paste0("Creating socioeconomic scenario...") |> message()
  # # 
  # # ### Interpolate annual values for GDP:
  # # ### Filter to first unique region
  # # ### Select GDP columns and add national total
  # # gdpData     <- scenarios[["gdpData"]]
  # # gdp_default <- gdpData |> interpolate_gdp()
  # # scenarios[["gdp_default"]] <- gdp_default
  # # rm(gdpData)
  # # 
  # # ### Interpolate annual values for population and add to data list
  # # popData     <- scenarios[["popData"]]
  # # pop_default <- popData |> interpolate_pop()
  # # scenarios[["pop_default"]] <- pop_default
  # # rm(popData)
  # # # pop_default |> names() |> print()
  # # 
  # # ### Default socioeconomic scenario:
  # # ### Use to assess default scalars but don't add to list
  # # pop_default <- pop_default |> mutate(region = region |> str_replace_all(" ", ""))
  # # df_national <- gdp_default |> create_nationalScenario(pop0=pop_default)
  # # # df_national |> names() |> print()
  # # rm(gdp_default, pop_default)
  # 
  
  
  ### Format Scalars ----------------
  # ### Interpolate values to annual levels
  # # if(msgUser) 
  # msg1 |> paste0("Formatting scalars...") |> message()
  # # scalarDataframe |> names() |> print()
  # ### Get data
  # scalars     <- stateData[["scalarData"]]
  # ### df_mainScalars
  # df_scalars  <- fun_formatScalars(
  #   data_x  = scalars,          ### rDataList$scalarDataframe
  #   info_x  = co_scalarInfo,    ### rDataList$co_scalarInfo
  
  ### Socioeconomic Scenario ######
  # if(msgUser) msg2 |> paste0("Creating socioeconomic scenario...") |> message()
  # 
  # ### Interpolate annual values for GDP:
  # ### Filter to first unique region
  # ### Select GDP columns and add national total
  # gdpData     <- scenarios[["gdpData"]]
  # gdp_default <- gdpData |> interpolate_gdp()
  # scenarios[["gdp_default"]] <- gdp_default
  # rm(gdpData)
  # 
  # ### Interpolate annual values for population and add to data list
  # popData     <- scenarios[["popData"]]
  # pop_default <- popData |> interpolate_pop()
  # scenarios[["pop_default"]] <- pop_default
  # rm(popData)
  # # pop_default |> names() |> print()
  # 
  # ### Default socioeconomic scenario:
  # ### Use to assess default scalars but don't add to list
  # pop_default <- pop_default |> mutate(region = region |> str_replace_all(" ", ""))
  # df_national <- gdp_default |> create_nationalScenario(pop0=pop_default)
  # # df_national |> names() |> print()
  # rm(gdp_default, pop_default)
  
  ###### Return ----------------
  ### Return the list of dataframes
  if (!silent) paste0(msg0, "...Finished running reshapeScenarioData.", msgN) |> message()
  return(scenarioData)
}