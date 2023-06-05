# experiments of the 
# - temporal method in symmetry
# - time dynamics in symmetry
# - reproduction of bush results with temporal method

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("simulation_scripts/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e2

# seperate directiory
folder_path <- paste0("simulation_scripts/experiments_RDS_wt", wait_time)
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}
# force a garbage collector to avoid recursive gc
gc(full = TRUE)

# experiment 1: temporal method
source("simulation_scripts/setup_sym_experiment_temporal.R")
res1 <- run_temporal_ssfind_symmetric(parameter)
saveRDS(res1, paste0(folder_path, "/simulation_symmetric_temporal.RDS"))
rm(parameter)

# experiment 2: time dynamics
gc(full = TRUE)
source("simulation_scripts/setup_sym_experiment_timedynamics.R")
res2 <- run_simulation_symmetric(parameter)
saveRDS(res2, paste0(folder_path, "/simulation_time_dynamics_symmetric.RDS"))
rm(parameter)

# experiment 3: bush reproduction
gc(full = TRUE)
source("simulation_scripts/setup_bush_experiment.R")
res3 <- run_temporal_ssfind(parameter)
saveRDS(res3, paste0(folder_path, "/simulation_bush_temporal.RDS"))
rm(parameter)




