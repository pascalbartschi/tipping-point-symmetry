# to manually explore s specific simulation of the results

source("analysis/read_microxanox.R")

# file to glimpse 
file <- "data/asymmetric-sim/stressor-asym/simulation_asymmetric_stressor.asym0.2.RDS" # look at some file here

theme_set(theme_bw())

# plotting
res <- readRDS(file)
# plot_temporal_ss(res)
# plot_symmetry_measures(res, species = "O")
plot_trajectory_symmetry(res)

# numbers
print(get_symmetry_measurements(res))
