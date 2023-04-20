## run unstable simulation

num_cores <- 24
# use of multiple cores
options(mc.cores = num_cores)

tot_time <- 1e5

## setup the experiment
steps <- 1e1
wait_time <- tot_time / steps
samp_interval <- 1e4
source("setup_unstable_experiment.R")

# ## setup the experiment
# wait_time <- 1e3
# steps <- 1e3
# source("setup_unstable_experiment.R")


# simulate on sym axis
parameter$strain_parameter$initial_state["CB_1"] <- 1e9
parameter$strain_parameter$initial_state["SB_1"] <- 1e9
# event interval
parameter$event_interval <- 1000
# if perturbation needs to be used: set != 1
parameter$perturbation <- 1

# set the file path
ssfile_path <- file.path("perturbations",
                         paste0("result_temporal_unstable_eq_events_",
                                parameter$event_interval,
                                "_waitime_",
                                wait_time,
                                "_steps_", steps,
                                ".RDS"))
ssfile_path <- file.path("perturbations", "owen.RDS")

# ssfile_path <- file.path("perturbations", "no_events.RDS")
expt_res <- sym_run_simulation(parameter)
plot_dynamics_symmetric(expt_res)
# saveRDS(expt_res, ssfile_path)
