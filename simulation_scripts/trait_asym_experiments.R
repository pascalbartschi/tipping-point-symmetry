# trait asymmetry experiments

num_cores <- 30
# use of multiple cores
options(mc.cores = num_cores) 

# load the packages
source("simulation_scripts/read_microxanox.R")

# define a wait_time = sample interval
wait_time <- 1e2

# seperate directiory
folder_path <- paste0("simulation_scripts/asymmetry_experiments_RDS_wt", wait_time)
if (!dir.exists(here::here(folder_path))){
  dir.create(folder_path)
}

# force a garbage collector to avoid recursive gc
gc(full = TRUE)

## asymmetric growth rate
# import the symmetric parameterset
source("simulation_scripts/setup_sym_experiment_temporal.R")
parameter$log10a_series <- seq(-2, 0,length = 300)
# decrease growth rates of sulfur-reducing bacteria (anoxic state is expected to undesirable)
g.rates_max <- seq(0.09, 0.06, by = -0.01)
for (g_max in g.rates_max){
  parameter$strain_parameter$SB$g_max_SB <- g_max
  temp_res <- run_temporal_ssfind_symmetric(parameter)
  saveRDS(temp_res, paste0(folder_path, "/simulation_asymmetric_gmaxSB", g_max, ".RDS"))
  rm(temp_res)
  gc(full = TRUE)
  print(paste("Asymmmetric experiment g_max =", g_max, "done and saved in", folder_path))
}

## asymmetric half-inhibition constant
# import the symmetric parameterset
source("simulation_scripts/setup_sym_experiment_temporal.R")
parameter$log10a_series <- seq(-2, 0,length = 300)
# decrease half inhibtion constant of oxygen on sulfur-reducing bacteria (anoxic state is expected to undesirable)
inhib.rates <- seq(90, 60 ,by = -10)
for (h_O_SB in inhib.rates){
  parameter$strain_parameter$SB$h_O_SB <- h_O_SB
  temp_res <- run_temporal_ssfind_symmetric(parameter)
  saveRDS(temp_res, paste0(folder_path, "/simulation_asymmetric_hOSB", h_O_SB, ".RDS"))
  rm(temp_res)
  gc(full = TRUE)
  print(paste("Asymmmetric experiment h_O_SB =", h_O_SB, "done and saved in", folder_path))
}

