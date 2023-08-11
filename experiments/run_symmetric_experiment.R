# experiments of the 
# - temporal method in symmetry
# - time dynamics in symmetry

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("experiments/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e6

# seperate directiory
folder_path <- "data/symmetric-sim/"

# create dir
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}
# force a garbage collector to avoid recursive gc
gc(full = TRUE)

# experiment 1: temporal method
source("experiments/setup_sym_experiment_temporal.R")
res1 <- run_temporal_ssfind_symmetric(parameter)
saveRDS(res1, paste0(folder_path, "/simulation_symmetric_temporal.RDS"))
rm(parameter)

# experiment 2: time dynamics
gc(full = TRUE)
source("experiments/setup_sym_experiment_timedynamics.R")
res2 <- run_simulation_symmetric(parameter)
saveRDS(res2, paste0(folder_path, "/simulation_time_dynamics_symmetric.RDS"))
rm(parameter)





