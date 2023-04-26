# read the package manually
source("analysis_scripts/read_microxanox.R")

# set the theme such that all figures look the same
theme_set(
  theme(# axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        legend.position = "none") 
)
# point to directory from project
wd <- "simulation_scripts/experiments_RDS_wt1e+06"

# figure 1a,b done in powerpoint
labs(# title = paste(trajectory, "trajectories"),
  x = "aO",
  y = "concentration", 
  color = "type")


# figure 1c, d: asymmetry in magnitudes in bush simulation
bush_res <- readRDS(file.path(wd, "simulation_bush_temporal.RDS"))
fig_1c <- plot_trajectory_symmetry(res = bush_res, trajectory = "recovery", typ = "substrate") + labs(x = "aO",  y = "concentration")
fig_1d <- plot_trajectory_symmetry(res = bush_res, trajectory = "collapse", typ = "substrate") + labs(x = "aO",  y = "concentration")


# figure 3
sym_res <- readRDS(file.path(wd, "simulation_symmetric_temporal.RDS"))
fig_3a <- plot_trajectory_symmetry(res = sym_res, trajectory = "recovery", typ = "substrate") + labs(x = "aO",  y = "concentration")
fig_3b <- plot_trajectory_symmetry(res = sym_res, trajectory = "collapse", typ = "substrate") + labs(x = "aO",  y = "concentration")

