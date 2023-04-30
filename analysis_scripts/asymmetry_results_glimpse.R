#to manually explore the each of the results
res <- readRDS("simulation_scripts/asymmetry_experiments_RDS_wt1e+06/simulation_asymmetric_gmaxSB0.07.RDS")
plot_temporal_ss(res)
print(get_symmetry_measures(res))
plot_symmetry_measures(res, species = "O")
plot_trajectory_symmetry(res)
