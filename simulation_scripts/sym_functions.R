library(microxanox)
library(tidyverse)

# strain parameter: both set to SB values

sym_new_SB_strain_parameter <- function(
    n = 1,
    values = "symmetric"
){
  
  # Check for supported values of the argument "values"
  if (is.na(values)) {
    values <- "NA"
  }
  if (!(values %in% c("NA", "symmetric"))) {
    stop("Not supported value for `values`!\n", "Only NA, 'NA' and 'symmetric' supported!")
  }
  
  ## Create object
  x <- rep(as.numeric(NA), n)
  nm <- paste0("SB_", 1:n)
  result <- data.frame(
    strain_name = nm,
    g_max_SB = x,
    k_SB_SO = x,
    k_SB_P = x,
    h_O_SB = x,
    y_SO_SB = x,  
    y_P_SB = x,
    m_SB = x,
    i_SB = x
  )
  
  ## Add values for the case of Bush et al 2017, SB
  if (values == "symmetric") {
    
    custom <- list( "g_max" = 0.1,  # growth rate
                    "k_B_P" = 0.5,  # SC50 on Phosphorus
                    "h_S_B" = 100,  # IC50 on substrates produced by bacteria
                    "y_P_B" = 1.67e8,  # abundance (bacteria) to concentration (P) conversion factor
                    "p_S_B" = 3e-8,  # abundance (bacteria) to concentration (Substrate) conversion factor (=production of S)
                    "m_B" = 0.04,    # mortality rate
                    "i_B" = 0)     # imigration of bacteria
    
    result$g_max_SB = rep(custom$g_max, n)
    result$k_SB_SO = rep(20, n)
    result$k_SB_P = rep(custom$k_B_P, n)
    result$h_O_SB = rep(custom$h_S_B, n)
    result$y_SO_SB = rep(custom$p_S_B, n)
    result$y_P_SB = rep(custom$y_P_B, n)
    result$m_SB = rep(custom$m_B)
    result$i_SB = rep(custom$i_B, n)
  }
  class(result) <- append( "SB_strain_parameter", class(result))
  return(result)
}


# for completeness

sym_new_PB_strain_parameter <- function(
    n = 1,
    values = "symmetric"
){
  
  # Check for supported values of the argument "values"
  if (is.na(values)) {
    values <- "NA"
  }
  if (!(values %in% c("NA", "symmetric"))) {
    stop("Not supported value for `values`!\n", "Only NA, 'NA' and 'symmetric' supported!")
  }
  
  ## Create object
  x <- rep(as.numeric(NA), n)
  nm <- paste0("PB_", 1:n)
  result <- data.frame(
    strain_name = nm,
    g_max_PB = x,
    k_PB_SR = x,
    k_PB_P = x,
    h_O_PB = x,
    y_SR_PB = x,
    y_P_PB = x,
    m_PB = x,
    i_PB = x
  )
  
  ## Add values for the case of Bush et al 2017, PB
  if (values == "symmetric") {
    result$g_max_PB = rep(0.07, n)
    result$k_PB_SR = rep(10, n)
    result$k_PB_P = rep(0.5, n)
    result$h_O_PB = rep(100, n)
    result$y_SR_PB = rep(1.25e7, n)  ## Y_S_PB in the main text
    result$y_P_PB = rep(1.67, n)
    result$m_PB = rep(0.028, n)
    result$i_PB = rep(0, n)
  }
  class(result) <- append( "PB_strain_parameter", class(result))
  return(result)
}


# strain parameter: set to SB values

sym_new_CB_strain_parameter <- function(
    n = 1,
    values = "symmetric"
){
  
  # Check for supported values of the argument "values"
  if (is.na(values)) {
    values <- "NA"
  }
  if (!(values %in% c("NA", "symmetric"))) {
    stop("Not supported value for `values`!\n", "Only NA, 'NA' and 'symmetric' supported!")
  }
  
  ## Create object
  x <- rep(as.numeric(NA), n)
  nm <- paste0("CB_", 1:n)
  result <- data.frame(
    strain_name = nm,
    g_max_CB = x,
    k_CB_P = x,
    h_SR_CB = x,
    y_P_CB = x,
    Pr_CB = x,
    m_CB = x,
    i_CB = x
  )
  
  ## Add values for the case of Bush et al 2017, SB
  if (values == "symmetric") {
    custom <- list( "g_max" = 0.1,  # growth rate
                    "k_B_P" = 0.5,  # SC50 on Phosphorus
                    "h_S_B" = 100,  # IC50 on substrates produced by bacteria
                    "y_P_B" = 1.67e8,  # abundance (bacteria) to concentration (P) conversion factor
                    "p_S_B" = 3e-8,  # abundance (bacteria) to concentration (Substrate) conversion factor (=production of S)
                    "m_B" = 0.04,    # mortality rate
                    "i_B" = 0)     # imigration of bacteria
    
    result$g_max_CB = rep(custom$g_max, n)
    result$k_CB_P = rep(custom$k_B_P, n)
    result$h_SR_CB = rep(custom$h_S_B, n)
    result$y_P_CB = rep(custom$y_P_B, n)
    result$Pr_CB = rep(custom$p_S_B, n)
    result$m_CB = rep(custom$m_B, n)
    result$i_CB = rep(custom$i_B, n)
  }
  class(result) <- append( "CB_strain_parameter", class(result))
  return(result)
}


# set up of symmetric initial state

sym_new_initial_state <- function(
    n_CB = 1,
    n_PB = 1,
    n_SB = 1,
    values = "symmetric"
){
  
  if (is.na(values)) {
    values <- "NA"
  }
  
  switch(
    EXPR = values,
    "symmetric" = {
      CB <-  1e5
      PB <-  0
      SB <-  1e5
      SO <-  0
      SR <-  20
      O  <-  20
      P  <-  10
    },
    "NA" = {
      CB <-  as.numeric(NA)
      PB <-  as.numeric(NA)
      SB <-  as.numeric(NA)
      SO <-  as.numeric(NA)
      SR <-  as.numeric(NA)
      O  <-  as.numeric(NA)
      P  <-  as.numeric(NA)
    },
    stop("Not supported value for `values`!\n", "Only NA, 'NA' and 'symmetric` are supported!")
  )
  
  result <- c(
    rep(CB, n_CB),
    rep(PB, n_PB),
    rep(SB, n_SB),
    ##
    SO,
    SR,
    O,
    P
  )
  names(result) <- c(
    paste0("CB_", 1:n_CB),
    paste0("PB_", 1:n_PB),
    paste0("SB_", 1:n_SB),
    ##
    "SO",
    "SR",
    "O",
    "P"
  )
  class(result) <- append( "initial_state", class(result))
  return(result)
}



# creating of symmetric strain parameter

sym_new_strain_parameter <- function(
    n_CB = 1,
    values_CB = "symmetric",
    n_PB = 1,
    values_PB = "symmetric",
    n_SB = 1,
    values_SB = "symmetric",
    values_other = "symmetric",
    values_initial_state = "symmetric"
){
  
  parms <- list()
  
  ## strain parameters
  
  parms$CB <- sym_new_CB_strain_parameter(n = n_CB, values = values_CB)
  parms$PB <- sym_new_PB_strain_parameter(n = n_PB, values = values_PB)
  parms$SB <- sym_new_SB_strain_parameter(n = n_SB, values = values_SB)
  
  ## other paramters
  
  ## substrate diffusivity
  parms$a_S <- as.numeric(NA)
  parms$a_O <- as.numeric(NA)
  parms$a_P <- as.numeric(NA)
  
  ## background substrate concentration
  parms$back_SR <- as.numeric(NA)
  parms$back_SO <- as.numeric(NA)
  parms$back_O <- as.numeric(NA)
  parms$back_P <- as.numeric(NA)
  
  ## oxidation rate of reduced sulphur
  parms$c <- as.numeric(NA)
  
  if (values_other == "symmetric") {
    ## substrate diffusivity
    parms$a_S <- 0.1
    parms$a_O <- 0.1
    parms$a_P <- 0.1
    ## background substrate concentration
    parms$back_SR <- 100
    parms$back_SO <- 0
    parms$back_O <- 100
    parms$back_P <- 10
    ## oxidisation rate of reduced sulphur
    parms$c <- 1e-2
  }
  
  ## set initial conditions
  parms$initial_state <- sym_new_initial_state(
    n_CB = n_CB, 
    n_PB = n_PB, 
    n_SB = n_SB, 
    values = values_initial_state
  )
  
  parms$ss_expt <- NULL
  
  class(parms) <- append("strain_parameter", class(parms))
  
  return(parms)
}





# plots dynamics of chemical substances, CB and SB

sym_plot_dynamics <- function(
    simulation_result, 
    every_n = 1,
    plot_a = TRUE
){
  
  ## define colours
  colfunc_CB <- grDevices::colorRampPalette(c("#024F17", "#B5FFC9"))
  colfunc_SB <- grDevices::colorRampPalette(c("#7D1402", "#FCBEB3"))
  colfunc_PB <- grDevices::colorRampPalette(c("#6E0172", "#F9AEFC"))
  
  col_SR <- "#ea9999"
  col_O <- "#6edd3d"
  col_a <- "#6b5205"
  col_P <- "#6fa8dc"
  
  ## data wrangling
  temp <- simulation_result$result %>%
    dplyr::filter(dplyr::row_number() %% every_n == 0 ) %>%
    dplyr::mutate(aO = 10^aO, aS = 10^aS) %>%
    tidyr::gather(species, quantity, 2:ncol(.)) %>% 
    dplyr::mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
                  functional_group = dplyr::case_when(str_detect(species, "CB_") ~ "CB",
                                                      str_detect(species, "SB_") ~ "SB",
                                                      str_detect(species, "PB_") ~ "PB"),
                  log10_quantity=log10(quantity)) %>%
    dplyr::mutate(microbe = functional_group)
  
  temp_S <- temp %>% 
    dplyr::filter(var_type == "Substrate") %>%
    dplyr::mutate(substrate = species)
  
  if(plot_a == FALSE){
    temp_S <- temp_S %>%
      filter(species != "aO", species != "aS")
  }
  
  num_CB_strains <- nrow(simulation_result$strain_parameter$CB)
  num_SB_strains <- nrow(simulation_result$strain_parameter$SB)
  num_PB_strains <- nrow(simulation_result$strain_parameter$PB)
  
  p1 <- temp %>%
    dplyr::filter(functional_group == "CB") %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    ggplot2::ggplot(aes(x=time, y=log10_quantity, col=microbe)) +
    ggplot2::geom_line() +
    # ylab('log10(quantity)') +
    ylab('log10(abundance)\n[cells / L]') +
    ylim(0,10) +
    # xlab('time [hours]') +
    xlab(" ") + 
    ggplot2::scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) +
    ggplot2::theme_bw()
  
  p2 <- temp %>%
    dplyr::filter(functional_group == "SB") %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    ggplot2::ggplot(aes(x=time, y=log10_quantity, col=microbe)) +
    ggplot2::geom_line() +
    ylab('log10(abundance)\n[cells / L]') +
    ylim(0,10) +
    # xlab('time [hours]') +
    xlab(" ") +
    ggplot2::scale_colour_manual(values = colfunc_SB(num_SB_strains))+
    ggplot2::guides(colour = guide_legend(ncol = 3)) + 
    ggplot2::theme_bw()
  
  # p3 <- temp %>%
  #   dplyr::filter(functional_group == "PB") %>%
  #   mutate(species = factor(species, levels = unique(species))) %>%
  #   ggplot2::ggplot(aes(x=time, y=log10_quantity, col=species)) +
  #   ggplot2::geom_line() +
  #   ylab('log10(quantity [cells])') +
  #   xlab('time [hours]') +
  #   ggplot2::scale_colour_manual(values = colfunc_PB(num_PB_strains))+
  #   ggplot2::guides(colour = guide_legend(ncol = 3))
  
  p4 <- temp_S %>%
    dplyr::filter(species != "SO") %>%
    ggplot(aes(x=time, y=log10_quantity, col=substrate)) +
    ggplot2::geom_line() +
    ggplot2::ylab('log10(concentration)\n[µM]') +
    ggplot2::xlab('time [hours]') +
    ggplot2::theme_bw()
  
  if(plot_a == FALSE){
    p4 <- p4 + scale_color_manual(values = c(col_O, col_P, col_SR))
  } else{
    p4 <- p4 + scale_color_manual(values = c(col_a, col_O, col_P, col_SR))
  }
  
  
  
  return (p1 / p2 / p4)
  
}


sym_plot_temporal_ss <- function(temporal_results, zoom = FALSE){
  
  
  p_organisms <- temporal_results %>%
    select(a_O, direction,
           starts_with("CB"),
           starts_with("SB")) %>%
    gather(key = species, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = a_O, y = log10(Quantity + 1), col = species, linetype = direction), size = 1) + 
    scale_color_manual(values = c("#38761d", "#cc0000" )) + 
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", size = 2) +
    ylab(expression(Log[10](Abundance+1))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    ylim(0,10) +
    theme_bw() +
    labs(title="Organisms")
  
  p_substrates <- temporal_results %>%
    select(a_O, direction, SR, O, P) %>%
    gather(key = substrate, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = a_O, y = log10(Quantity + 1), col = substrate, linetype = direction), size = 1) + 
    scale_color_manual(values = c("#38761d", "#2986cc", "#cc0000")) +
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", size = 2) +
    ylab(expression(Log[10](Quantity))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    #vylim(0,10) +
    theme_bw() +
    labs(title="Substrates")
  
  if (zoom == TRUE){
    p_organisms <- p_organisms + xlim(-2, 0)
    p_substrates <- p_substrates + xlim(-2,0)
    # print("zoom true")
  }
  
  
  return (p_organisms / p_substrates)
}

### from 2023 ##############################################################


sym_new_runsim_parameter <- function(
    ...
){
  p <- list(
    dynamic_model = NA, # default_dynamic_model,
    event_definition = NA, # default_event_definition,
    strain_parameter = NA, # default_parameter_values,
    event_interval = NA, # default_event_interval,
    noise_sigma = NA, # default_noise_sigma,
    minimum_abundances = NA, # default_minimum_abundances,
    sim_duration = NA, # default_sim_duration,
    sim_sample_interval = NA, # default_sim_sample_interval,
    log10a_series = NA, # default_log10a_series,
    sym_axis = FALSE,
    solver_method = "radau" # "radau",
  )
  if (!inherits(p, "runsim_parameter")) {
    class(p) <- append(class(p), "runsim_parameter")
  }
  if (...length() > 0) {
    valid <- ...names() %in% names(p)
    if (!all(valid)) {
      stop(paste0("Non defined parameter supplied: ", paste(...names()[!valid], collapse = ", ")))
    } else {
      for (nm in 1:...length()) {
        p[[...names()[[nm]]]] <- ...elt(nm)
      }
    }
  }
  
  return(p)
}

sym_run_simulation <- function(
    parameter
){
  if (!inherits(parameter, "runsim_parameter")) {
    stop("parameter has to be an object of type `runsim_parameter`!")
  }
  
  if(parameter$sim_sample_interval > parameter$sim_duration){
    stop("Simulation sample interval is greater than simulation duration... it should be shorter.")
  } 
  
  ## make the times at which observations will be recorded
  times <- seq(
    0,
    parameter$sim_duration,
    by = parameter$sim_sample_interval
  )
  
  ## make the times at which events will occur
  event_times <- seq(min(times),
                     max(times),
                     by = parameter$event_interval)
  
  if (!parameter$sym_axis){
    parameter$sym_axis <- mean(parameter$log10a_series)
  }
  
  ## create the series of oxygen diffusivity values
  log10a_forcing <- matrix(
    ncol = 3,
    byrow = F,
    data = c(
      ceiling(
        seq(
          0,
          max(times),
          length=length(parameter$log10a_series)
        )
      ),
      parameter$log10a_series,                         # oxygen diffusivities
      2 * parameter$sym_axis - parameter$log10a_series # mirrored sulfur diffusivities
      
    )
  )
  
  ## Make the function to give the oxygen diffusivity at a particular time      
  l_f_f_O <- approxfun(
    x = log10a_forcing[,1],
    y = log10a_forcing[,2],
    method = "linear",
    rule = 2
  )
  

  ## Make the function to give the sulfur diffusivity at a particular time      
  l_f_f_S <- approxfun(
    x = log10a_forcing[,1],
    y = log10a_forcing[,3],
    method = "linear",
    rule = 2
  )
  
  
  ## Run the simulation
  out <- as.data.frame(
    deSolve::ode(
      y = parameter$strain_parameter$initial_state,
      times = times,
      func = parameter$dynamic_model,
      parms = parameter$strain_parameter,
      method = parameter$solver_method,
      events = list(
        func = parameter$event_definition,
        time = event_times
      ),
      log10aO_forcing_func = l_f_f_O,
      log10aS_forcing_func = l_f_f_S,
      noise_sigma = parameter$noise_sigma,
      minimum_abundances = parameter$minimum_abundances
    )
  )
  
  
  result <- new_runsim_results(parameter, out)
  
  return(result)
}

# PB and SO rates set to zero for reasons below, SB growth set to growth1

sym_bushplus_dynamic_model <- function(
    t, 
    state, 
    parameters, 
    log10aO_forcing_func,
    log10aS_forcing_func,
    ...
){
  
  ## unpack state variables from the state object
  CB <- state[grep("CB", names(state))]
  names_CB <- names(CB)[order(names(CB))]
  CB <- as.numeric(CB[order(names(CB))])
  PB <- state[grep("PB", names(state))]
  names_PB <- names(PB)[order(names(PB))]
  PB <- as.numeric(PB[order(names(PB))])
  SB <- state[grep("SB", names(state))]
  names_SB <- names(SB)[order(names(SB))]
  SB <- as.numeric(SB[order(names(SB))])
  # print(c(CB, PB, SB))
  
  
  # rates of change of CB and SB
  CB_growth_rate <- growth1(state["P"], parameters$CB$g_max_CB, parameters$CB$k_CB_P) * inhibition(state["SR"], parameters$CB$h_SR_CB) * CB
  SB_growth_rate <- growth1(state["P"], parameters$SB$g_max_SB, parameters$SB$k_SB_P) * inhibition(state["O"], parameters$SB$h_O_SB) * SB
  
  CB_mortality_rate <- parameters$CB$m_CB * CB
  SB_mortality_rate <- parameters$SB$m_SB * SB
  
  CB_rate <- CB_growth_rate - CB_mortality_rate + parameters$CB$i_CB
  SB_rate <- SB_growth_rate - SB_mortality_rate + parameters$SB$i_SB
  
  # PB rates of change
  # PB_growth_rate <- growth2(state["P"], state["SR"], parameters$PB$g_max_PB, parameters$PB$k_PB_P, parameters$PB$k_PB_SR) * inhibition(state["O"], parameters$PB$h_O_PB) * PB
  # PB_mortality_rate <- parameters$PB$m_PB * PB
  # PB_rate <- PB_growth_rate - PB_mortality_rate + parameters$PB$i_PB
  PB_growth_rate <- 0 # no need to calculate in every step
  PB_rate <- 0
  
  # Substrate rates of change
  # SO_rate <- sum(1 / parameters$PB$y_SR_PB * PB_growth_rate) -
  #   sum(1 / parameters$SB$y_SO_SB * SB_growth_rate) +
  #   parameters$c * state["O"] * state["SR"] +
  #   parameters$a_S * (parameters$back_SO -state[["SO"]])
  SO_rate <- 0 # leaving the ODE will lead to drastic decrease,
  ## because only term != 0 is consumption by SB (inexisting in growth1)
  
  
  SR_rate <- sum(parameters$SB$y_SO_SB * SB_growth_rate) -
    parameters$c * state["O"] * state["SR"] + # = 0
    10^log10aS_forcing_func(t) * (parameters$back_SR - state["SR"])
  
  O_rate <- sum(parameters$CB$Pr_CB * CB_growth_rate) -
    parameters$c * state["O"] * state["SR"] + # = 0
    10^log10aO_forcing_func(t) * (parameters$back_O - state["O"]) # look at to construct symmetry only based on log10a forc, looking at func only
  
  P_rate <- - sum(1 / parameters$CB$y_P_CB * CB_growth_rate) -
    #sum(1 / parameters$PB$y_P_PB * PB_growth_rate) -
    sum(1 / parameters$SB$y_P_SB * SB_growth_rate) +
    parameters$a_P * (parameters$back_P - state["P"])
  
  # Assemble results
  result <- list(
    c(
      CB_rate,
      PB_rate,
      SB_rate,
      SO_rate = SO_rate,
      SR_rate = SR_rate,
      O_rate = O_rate,
      P_rate = P_rate
    ),
    aO = log10aO_forcing_func(t),
    aS = log10aS_forcing_func(t)
  )
  
  # Name results
  names(result[[1]]) <- c(
    parameters$CB$strain_name,
    parameters$PB$strain_name,
    parameters$SB$strain_name,
    "SO_rate",
    "SR_rate",
    "O_rate",
    "P_rate"
  )
  
  
  return(result)
}

sym_event_definition_2 <- function(
    times,
    state,
    parms,
    log10aO_forcing_func,
    log10aS_forcing_func,
    noise_sigma,
    minimum_abundances
){
  with(
    as.list(state),
    {
      ## here adding a bit of noise to the abiotics
      # if(noise_sigma!=0) {
      SO <- SO + rnorm(1, 0, noise_sigma*SO)*noise_sigma*SO
      SR <- SR + rnorm(1, 0, noise_sigma*SR)*noise_sigma*SR
      O <-  O  + rnorm(1, 0, noise_sigma*O)*noise_sigma *O
      P <-  P  + rnorm(1, 0, noise_sigma*P)*noise_sigma *P
      
      ## and below setting the abundance to the minimum, in case it happens to be below it
      CB <- state[grep("CB", names(state))]
      names_CB <- names(CB)[order(names(CB))]
      CB <- as.numeric(CB[order(names(CB))])
      CB <- CB + minimum_abundances["CB"]
      
      PB <- state[grep("PB", names(state))]
      names_PB <- names(PB)[order(names(PB))]
      PB <- as.numeric(PB[order(names(PB))])
      PB <- PB + minimum_abundances["PB"]
      
      SB <- state[grep("SB", names(state))]
      names_SB <- names(SB)[order(names(SB))]
      SB <- as.numeric(SB[order(names(SB))])
      SB <- SB + minimum_abundances["SB"]
      
      # Assemble results
      result <- c(
        CB,
        PB,
        SB,
        SO = SO,
        SR = SR,
        O = O,
        P = P
      )
      
      # Name results
      names(result) <- c(
        names_CB,
        names_PB,
        names_SB,
        "SO",
        "SR",
        "O",
        "P"
      )
      
      return(result)
    }
  )
}

plot_dynamics_symmetric <- function(
    simulation_result, 
    every_n = 1,
    plot_a = TRUE
){
  
  ## define colours
  colfunc_CB <- grDevices::colorRampPalette(c("#024F17", "#B5FFC9"))
  colfunc_SB <- grDevices::colorRampPalette(c("#7D1402", "#FCBEB3"))
  colfunc_PB <- grDevices::colorRampPalette(c("#6E0172", "#F9AEFC"))
  
  col_SR <- "#ea9999"
  col_O <- "#6edd3d"
  col_aO <- "#6ba405"
  col_aS <- "#6b3205"
  col_P <- "#6fa8dc"
          
        ## data wrangling
        temp <- simulation_result$result %>%
          dplyr::filter(dplyr::row_number() %% every_n == 0 ) %>%
          dplyr::mutate(aO = 10^aO, aS = 10^aS) %>%
          tidyr::gather(species, quantity, 2:ncol(.)) %>% 
          dplyr::mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
                        functional_group = dplyr::case_when(str_detect(species, "CB_") ~ "CB",
                                                            str_detect(species, "SB_") ~ "SB",
                                                            str_detect(species, "PB_") ~ "PB"),
                        log10_quantity=log10(quantity)) %>%
          dplyr::mutate(microbe = functional_group)
        
        temp_S <- temp %>% 
          dplyr::filter(var_type == "Substrate") %>%
          dplyr::mutate(substrate = species)
        
        if(plot_a == FALSE){
          temp_S <- temp_S %>%
            filter(species != "aO", species != "aS")
        }
        
        num_CB_strains <- nrow(simulation_result$strain_parameter$CB)
        num_SB_strains <- nrow(simulation_result$strain_parameter$SB)
        num_PB_strains <- nrow(simulation_result$strain_parameter$PB)
        
        p1 <- temp %>%
          dplyr::filter(functional_group == "CB") %>%
          mutate(species = factor(species, levels = unique(species))) %>%
          ggplot2::ggplot(aes(x=time, y=log10_quantity, col=microbe)) +
          ggplot2::geom_line() +
          # ylab('log10(quantity)') +
          ylab('log10(abundance)\n[cells / L]') +
          ylim(0,10) +
          # xlab('time [hours]') +
          xlab(" ") + 
          ggplot2::scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
          ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) +
          ggplot2::theme_bw()
        
        p2 <- temp %>%
          dplyr::filter(functional_group == "SB") %>%
          mutate(species = factor(species, levels = unique(species))) %>%
          ggplot2::ggplot(aes(x=time, y=log10_quantity, col=microbe)) +
          ggplot2::geom_line() +
          ylab('log10(abundance)\n[cells / L]') +
          ylim(0,10) +
          # xlab('time [hours]') +
          xlab(" ") +
          ggplot2::scale_colour_manual(values = colfunc_SB(num_SB_strains))+
          ggplot2::guides(colour = guide_legend(ncol = 3)) + 
          ggplot2::theme_bw()
        
        # p3 <- temp %>%
        #   dplyr::filter(functional_group == "PB") %>%
        #   mutate(species = factor(species, levels = unique(species))) %>%
        #   ggplot2::ggplot(aes(x=time, y=log10_quantity, col=species)) +
        #   ggplot2::geom_line() +
        #   ylab('log10(quantity [cells])') +
        #   xlab('time [hours]') +
        #   ggplot2::scale_colour_manual(values = colfunc_PB(num_PB_strains))+
        #   ggplot2::guides(colour = guide_legend(ncol = 3))
        
        p4 <- temp_S %>%
          dplyr::filter(species != "SO") %>%
          ggplot(aes(x=time, y=log10_quantity, col=substrate)) +
          ggplot2::geom_line() +
          ggplot2::ylab('log10(concentration)\n[µM]') +
          ggplot2::xlab('time [hours]') +
          ggplot2::theme_bw()
        
        if(plot_a == FALSE){
          p4 <- p4 + scale_color_manual(values = c(col_O, col_P, col_SR))
        } else{
          p4 <- p4 + scale_color_manual(values = c(col_aO, col_aS, col_O, col_P, col_SR))
        }
        
        
        
        return (p1 / p2 / p4)
        
}

sym_run_temporal_ssfind <- function(parameter) {
  
  ## recalculate the wait time (length of a step)
  wait_time <- parameter$sim_duration / length(parameter$log10a_series)
  
  ## make function for the increasing ox diff steps
  up_l_f_f <- approxfun(x = wait_time * c(0:length(parameter$log10a_series)), 
                        y = (c(parameter$log10a_series, parameter$log10a_series[length(parameter$log10a_series)])),
                        method = "constant", rule = 1)
  
  ## make function for the decreasing ox diff steps
  down_l_f_f <- approxfun(x = wait_time * c(0:length(parameter$log10a_series)), 
                          y = c(rev(parameter$log10a_series), parameter$log10a_series[1]),
                          method = "constant", rule = 1)
  
  ## make times at which observations are made (i.e. at the end of a step)
  times <- c(0,
             seq(parameter$sim_sample_interval - 1,
                 parameter$sim_duration,
                 by = parameter$sim_sample_interval))
  
  ## make times at which events occur
  event_times <- c(0, seq(parameter$event_interval-1,
                          max(times),
                          by = parameter$event_interval))
  
  ## run a simulation for increasing ox diff
  up_res <- as.data.frame(
    deSolve::ode(
      y = parameter$strain_parameter$initial_state,
      times = times,
      func = parameter$dynamic_model,
      parms = parameter$strain_parameter,
      method = parameter$solver_method,
      events = list(
        func = parameter$event_definition,
        time = event_times
      ),
      log10aO_forcing_func = up_l_f_f, # oxygen diffusivity increases
      log10aS_forcing_func = down_l_f_f, # sulfur diffusivity decreases
      noise_sigma = parameter$noise_sigma,
      minimum_abundances = parameter$minimum_abundances
    )
  )
  ## organise results
  up_res <- up_res %>%
    filter(time %in% times) %>%
    slice(-1) %>%
    mutate(recovery = "oxic")
  
  ## run a simulation for decreasing ox diff
  down_res <- as.data.frame(
    deSolve::ode(
      y = parameter$strain_parameter$initial_state,
      times = times,
      func = parameter$dynamic_model,
      parms = parameter$strain_parameter,
      method = parameter$solver_method,
      events = list(
        func = parameter$event_definition,
        time = event_times
      ),
      log10aO_forcing_func = down_l_f_f,
      log10aS_forcing_func = up_l_f_f,
      noise_sigma = parameter$noise_sigma,
      minimum_abundances = parameter$minimum_abundances
    )
  )
  ## organise results
  down_res <- down_res %>%
    filter(time %in% times) %>%
    slice(-1) %>%
    mutate(recovery = "anoxic")
  
  ## combine results
  result <- rbind(up_res, down_res)
  
  result <- new_temporal_ssfind_results(result = result)
  return(result)
}


sym_plot_temporal_ss <- function(temporal_results, zoom = FALSE){
  
  
  p_organisms <- temporal_results %>%
    select(aO, recovery,
           starts_with("CB"),
           starts_with("SB")) %>%
    gather(key = species, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = aO, y = log10(Quantity + 1), col = species, linetype = recovery), linewidth = 1) + 
    scale_color_manual(values = c("#38761d", "#cc0000" )) + 
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", linewidth = 2) +
    ylab(expression(Log[10](Abundance+1))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    ylim(0,10) +
    theme_bw() +
    labs(title="Organisms")
  
  p_substrates <- temporal_results %>%
    select(aO, recovery, SR, O, P) %>%
    gather(key = substrate, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = aO, y = log10(Quantity + 1), col = substrate, linetype = recovery), linewidth = 1) + 
    scale_color_manual(values = c("#38761d", "#2986cc", "#cc0000")) +
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", linewidth = 2) +
    ylab(expression(Log[10](Quantity))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    #vylim(0,10) +
    theme_bw() +
    labs(title="Substrates")
  
  if (zoom == TRUE){
    library(ggforce)
    p_organisms <- p_organisms + ggforce::facet_zoom(ylim = c(-2,0))
    p_substrates <- p_substrates + xlim(-2,0)
    # print("zoom true")
  }
  
  
  return (p_organisms / p_substrates)
}

sym_get_stability_measures_temporal_ssfind_result <- function(
    ss_object,
    threshold_diff_log10scale = 3,
    ...
) {
  

  
  if (inherits(ss_object, "replication_ssfind_result")) {
    result <- ss_object$result
  } else {
    result <- ss_object
  }
  
  # reformat the data.table so it fits the function
  result <- result %>%
    mutate(direction = ifelse(recovery == "oxic", "up", "down"), 
           a_O = aO) %>%
    select(-recovery, -aS, -aO)
  
  ## The following is preparing the data
  these <- grep("B_", names(result))
  these <- c(these, which(names(result) %in% c("SO", "SR", "O", "P")))
  temp <- result %>%
    # dplyr::filter(dplyr::across(these, ~ .x >-0.001)) %>% ## there are rarely negative abundances greater than -0.001. This line and the na.omit removes them 
    tidyr::gather(key = "Species", value = Quantity, these) %>%
    select(-time) %>%
    tidyr::spread(key = direction, value=Quantity, drop=T) %>%
    na.omit() 
  
  ## then get the stability measures
  res <- temp %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(hyst_tot_raw = get_hysteresis_total(up, down),
                     hyst_range_raw = get_hysteresis_range(up, down, a_O, 10^threshold_diff_log10scale),
                     hyst_min_raw = get_hysteresis_min(up, down, a_O, 10^threshold_diff_log10scale),
                     hyst_max_raw = get_hysteresis_max(up, down, a_O, 10^threshold_diff_log10scale),
                     nl_up_raw = get_nonlinearity(a_O, up),
                     nl_down_raw = get_nonlinearity(a_O, down),
                     hyst_tot_log = get_hysteresis_total(log10(up+1), log10(down+1)),
                     hyst_range_log = get_hysteresis_range(log10(up+1), log10(down+1), a_O, threshold_diff_log10scale),
                     hyst_min_log = get_hysteresis_min(log10(up+1), log10(down+1), a_O, threshold_diff_log10scale),
                     hyst_max_log = get_hysteresis_max(log10(up+1), log10(down+1), a_O, threshold_diff_log10scale),
                     nl_up_log = get_nonlinearity(a_O, log10(up+1)),
                     nl_down_log = get_nonlinearity(a_O, log10(down+1))
    ) 
  
  res %>% na.omit()
  
}

bush_plot_temporal_ss <- function(temporal_results, zoom = FALSE){
  
  temporal_results <- temporal_results %>% 
                      mutate(aO = a_O) %>% 
                      mutate(recovery = ifelse(direction == "up", "oxic", "anoxic"))
  
  
  p_organisms <- temporal_results %>%
    select(aO, recovery,
           starts_with("CB"),
           starts_with("SB"),
           starts_with("PB")) %>%
    gather(key = species, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = aO, y = log10(Quantity + 1), col = species, linetype = recovery), linewidth = 1) + 
    scale_color_manual(values = c("#38761d", "#cc0000", "purple" )) + 
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", linewidth = 2) +
    ylab(expression(Log[10](Abundance+1))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    ylim(0,10) +
    theme_bw() +
    labs(title="Organisms")
  
  p_substrates <- temporal_results %>%
    select(aO, recovery, SR, SO, O, P) %>%
    gather(key = substrate, value = Quantity, 3:ncol(.)) %>%
    ggplot() + 
    geom_path(aes(x = aO, y = log10(Quantity + 1), col = substrate, linetype = recovery), linewidth = 1) + 
    scale_color_manual(values = c("#38761d", "#2986cc", "#cc0000", "darkred")) +
    geom_vline(xintercept = log10(0.1), linetype = "dotted", color = "black", linewidth = 2) +
    ylab(expression(Log[10](Quantity))) +
    xlab(expression(Log[10](Oxygen~diffusivity))) +
    #vylim(0,10) +
    theme_bw() +
    labs(title="Substrates")
  
  if (zoom == TRUE){
    library(ggforce)
    p_organisms <- p_organisms + ggforce::facet_zoom(ylim = c(-2,0))
    p_substrates <- p_substrates + xlim(-2,0)
    # print("zoom true")
  }
  
  
  return (p_organisms / p_substrates)
}

perturbations <- function(
  times,
  state,
  parms,
  log10aO_forcing_func,
  log10aS_forcing_func,
  noise_sigma,
  minimum_abundances
){
  with(
    as.list(state),
    {
      
      ## and below setting the abundance to the minimum, in case it happens to be below it
      CB <- state[grep("CB", names(state))]
      names_CB <- names(CB)[order(names(CB))]
      CB <- as.numeric(CB[order(names(CB))])
      CB <- CB + minimum_abundances["CB"]
      
      PB <- state[grep("PB", names(state))]
      names_PB <- names(PB)[order(names(PB))]
      PB <- as.numeric(PB[order(names(PB))])
      PB <- PB + minimum_abundances["PB"]
      
      SB <- state[grep("SB", names(state))]
      names_SB <- names(SB)[order(names(SB))]
      SB <- as.numeric(SB[order(names(SB))])
      SB <- SB + minimum_abundances["SB"]
      
      if (times == as.integer(2e6)){
        print(paste0("perturbation_", parameter$perturbation))
        CB <- CB * parameter$perturbation
      }
      
      # Assemble results
      result <- c(
        CB,
        PB,
        SB,
        SO = SO,
        SR = SR,
        O = O,
        P = P
      )
      
      # Name results
      names(result) <- c(
        names_CB,
        names_PB,
        names_SB,
        "SO",
        "SR",
        "O",
        "P"
      )
      
      return(result)
    }
  )
}