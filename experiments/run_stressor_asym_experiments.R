# stressor asymmetry experiments 

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("experiments/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e6

# seperate directiory
folder_path <- "data/asymmetric-sim/stressor-asym/"

# create dir
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}

# force a garbage collector to avoid recursive gc
gc(full = TRUE)

## asymmetric growth rate
# import the symmetric parameterset
source("experiments/setup_sym_experiment_temporal.R")
asym_factors <- seq(0, 2, by = 0.2)
parameter$log10a_series <- seq(-2, 0,length = 300)
for (asym_f in asym_factors){
  parameter <- update_asym_factor(parameter, asym_f)
  temp_res <- run_temporal_ssfind_symmetric(parameter)
  saveRDS(temp_res, paste0(folder_path, "/simulation_asymmetric_stressor.asym", asym_f, ".RDS"))
  rm(temp_res)
  gc(full = TRUE)
  print(paste("Asymmmetric experiment asym_f =", asym_f, "done and saved in", folder_path))
}