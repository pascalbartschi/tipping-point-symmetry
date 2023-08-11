#to manually explore the each of the results
source("analysis_scripts/read_microxanox.R")

# file to glimpse 
file <- "simulation_scripts/asymmetry_experiments_RDS_wt1e+06/simulation_asymmetric_stressor.asym1.RDS"

theme_set(theme_bw())

# plotting
res <- readRDS(file)
plot_temporal_ss(res)
plot_symmetry_measures(res, species = "O")
plot_trajectory_symmetry(res)

# numbers
print(get_symmetry_measures(res))