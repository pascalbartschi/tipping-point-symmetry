√# experiments

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("simulation_scripts/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e6

# seperate directiory
folder_path <- paste0("simulation_scripts/experiments_RDS_wt", wait_time)
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}


# experiment 1: temporal method
source("simulation_scripts/setup_sym_experiment_temporal.R")
res1 <- run_temporal_ssfind_symmetric(parameter)
saveRDS(res1, "simulation_scripts/experiments_RDS/symmetric_simulation_symmetric_temporal.RDS")
rm(parameter)

# experiment 2: time dynamics
source("simulation_scripts/setup_sym_experiment_timedynamics.R")
res2 <- run_simulation_symmetric(parameter)
saveRDS(res2, "simulation_scripts/experiments_RDS/symmetric_simulation_time_dynamics.RDS")
rm(parameter)

# experiment 3: bush reproduction
source("simulation_scripts/setup_bush_experiment.R")
res3 <- run_temporal_ssfind(parameter)
saveRDS(res3, "simulation_scripts/experiments_RDS/asymmetric_simulation_bush_temporal.RDS")
rm(parameter)






