## set up experiment: parameter set and framework



# wait_time  # time spent at each step, later set for ss finding
num_strains <- 1 # number of strains per group
event_interval <- 1000 # interval of event occurence
symmetry_axis <- -1 # log10()
steps <- 300
start_aO <- -1.25 # difference is much smaller due to divergence of diffusivities
start_aS <- -0.75


log10a_series <- seq(start_aO, start_aS, length = steps)


num_CB_strains <- num_strains
num_SB_strains <- num_strains
num_PB_strains <- num_strains

sp <- new_strain_parameter(
  n_CB = num_CB_strains,
  values_CB = "symmetric",
  n_PB = num_SB_strains,
  values_PB = "symmetric",
  n_SB = num_PB_strains,
  values_SB = "symmetric",
  values_other  = "symmetric",
  values_initial_state = "symmetric"
  
)

parameter <- new_runsim_parameter(
  dynamic_model = symmetric_bushplus_dynamic_model,  
  event_definition = event_definition_symmetric,       
  event_interval = event_interval,
  noise_sigma = 0,
  minimum_abundances = c(1, 0 , 1),       # PB stays 0
  strain_parameter = sp,
  log10a_series =  log10a_series,
  sym_axis = -1
)

names(parameter$minimum_abundances) <- c("CB", "PB", "SB")
rm(sp)



parameter$sim_duration <- length(parameter$log10a_series) * wait_time 
parameter$strain_parameter$initial_state["CB_1"] <- 1e5
parameter$strain_parameter$initial_state["SB_1"] <- 1e5
parameter$sim_duration <- wait_time * length(parameter$log10a_series)


parameter$sim_sample_interval <- wait_time  # to avoid having negative ODE results

