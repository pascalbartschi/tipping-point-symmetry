# experiments of the 
# - reproduction of bush results with temporal method

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("experiments/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e6

# seperate directiory
folder_path <- "data/bush-sim/"

# create dir
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}
# force a garbage collector to avoid recursive gc
gc(full = TRUE)

# experiment 3: bush reproduction
gc(full = TRUE)
source("experiments/setup_bush_experiment.R")
res3 <- run_temporal_ssfind(parameter)
saveRDS(res3, paste0(folder_path, "/simulation_bush_temporal.RDS"))
rm(parameter)




