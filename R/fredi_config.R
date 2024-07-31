### This file contains hard coded values for constants used throughout the temperature binning process
###### Config List for createSystemData ######
frediConfig <- function(
    minYear0 = 2010, 
    maxYear0 = 2100, 
    npdYear0 = 2300
){
  ###### Constants ######
  ###### Named constant values that are used frequently in the model
  ### Millions, billions
  # fredi_config[["k_million"]] <- 10**6
  # fredi_config[["k_billion"]] <- 10**9
  
  
  ###### Data Years ######
  ### Min year, max year
  list0[["minYear0" ]] <- 2010
  list0[["maxYear0" ]] <- 2100
  list0[["npdYear0" ]] <- 2300
  
  
  ###### Default values ######
  ### Base year, discount rate
  ### Types of aggregation
  list0[["elasticity0"]] <- 1
  list0[["aggList0"   ]] <- c("national", "modelaverage", "impactyear", "impacttype")
  list0[["groupLvls0" ]] <- c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region", "state", "postal")
  
  
  ###### temps2slr constants ###### 
  ### - Phi: (i.e., "c", above) is a temperature-independent rate term with e-folding time tau2. I.e., phi is the multi-millennial contribution to GMSL in mm/year.
  ### - Scalar: Sensitivity of the GSL rate to a deviation of T(t) from an equilibrium temperature of Te(t).
  ### - Alpha: The value obtained in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "a").
  ### - Tau1: the timescale on which the actual temperature relaxes toward the equilibrium temperature. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau").
  ### - Tau2: e-folding time(scale) for phi. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau_C").
  list0[["phi0" ]] <- 0.14
  list0[["alpha"]] <- 4.0
  list0[["tau1" ]] <- 174
  list0[["tau2" ]] <- 4175
  
  ###### Messages for createSystemData ######
  ### Initialize list
  messages_data    <- list()
  ### Populate list
  messages_data[["loadPackages"]] <- "Loading required packages..."
  messages_data[["sourceCode"  ]] <- "Loading custom functions..."
  messages_data[["loadInputs"  ]] <- list(try="Loading input data...", success="Input data loaded.", fail="Could not find input data.")
  messages_data[["calcScalars" ]] <- list(try="Calculating physical and economic scalars...", success="Physical and economic scalars calculated.")
  messages_data[["interpFuns"  ]] <- list(try="Getting interpolation functions...", success="Interpolation functions complete.")
  messages_data[["saveRData"   ]] <- list(try="Saving results...", success="R data loaded.", fail="Could not find R data.")
  messages_data[["outPath"     ]] <- list(fail="Output directory not found.", success="Created output directory.", skip="No directory created. Exiting...")
  messages_data[["aggImpacts"  ]] <- list(try="Aggregating impacts...")
  ### Update in list
  list0[["messages_data"]] <- messages_data
  
  ###### Configuration Messages ######
  ### List of messages for run_fredi
  list_messages = list()
  list_messages[["loadPackages" ]] <- "Loading required packages..."
  list_messages[["sourceCode"   ]] <- "Loading custom functions..."
  list_messages[["loadRData"    ]] <- list(try="Loading R data...", success="R data loaded.", fail="Could not find R data.")
  list_messages[["loadInputs"   ]] <- list(try="Loading input data...", success="Input data loaded.", fail="Could not find input data.")
  list_messages[["globalTemps"  ]] <- "Converting global temperature scenario to CONUS scenario..."
  list_messages[["updatePopGDP" ]] <- "Updating socioeconomic scenario..."
  list_messages[["updateScalars"]] <- list(try="Updating physical and economic scalars...", success="Physical and economic scalars updated.")
  list_messages[["scaledImpacts"]] <- list(try="Calculating annual scaled impacts", success="Calculation of scaled impacts complete.")
  list_messages[["aggImpacts"   ]] <- list(try="Aggregating impacts...")
  list_messages[["impactTypes"  ]] <- list(try="Summing over impact types...")
  list_messages[["impactYears"  ]] <- list(try="Interpolating between impact estimate years...")
  list_messages[["modelAves"    ]] <- list(try="Getting model averages...")
  list_messages[["national"     ]] <- list(try="Calculating national totals...")
  list0[["list_messages"]] <- list_messages

  
  ###### Return ######
  return(list0)
}