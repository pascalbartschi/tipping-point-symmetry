## set up experiment: parameter set and framework



# wait_time  # time spent at each step, later set for ss finding
num_strains <- 1 # number of strains per group
event_interval <- 1000 # interval of event occurences
# ymmetry_axis <- -1 # log10()
steps <- 300
start <- -8 # difference is much smaller due to divergence of diffusivities
end <- 0


log10a_series <- seq(start, end, length = steps)

parameter <- new_runsim_parameter(
  dynamic_model = bushplus_dynamic_model,  
  event_definition = event_definition_2,       
  event_interval = event_interval,
  noise_sigma = 0,
  minimum_abundances = c(1 , 1 , 1),       
  strain_parameter = new_strain_parameter(n_CB = 1, n_SB = 1, n_PB = 1, values_initial_state = "bush_ssfig3"),
  log10a_series =  log10a_series
)

names(parameter$minimum_abundances) <- c("CB", "PB", "SB")
# rm(sp)
# parameter$strain_parameter$initial_state["CB_1"] <- 1e8

parameter$sim_duration <- wait_time * length(parameter$log10a_series)
parameter$sim_sample_interval <- wait_time  # to avoid having negative ODE results

