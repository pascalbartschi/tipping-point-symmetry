# read the package manually
source("analysis_scripts/read_microxanox.R")

# set the theme such that all figures look the same
theme_set(
  theme(text = element_text(family  ="Arial", size = 7),
        axis.title = element_blank(),
        title = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Arial", size = 5),
        # axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                      ends = "both")),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"),
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
fig_1c <- plot_trajectory_symmetry(res = bush_res, trajectory = "recovery", typ = "substrate") 
fig_1d <- plot_trajectory_symmetry(res = bush_res, trajectory = "collapse", typ = "substrate") 
fig_1cd <- ggarrange(fig_1c, fig_1d, nrow = 1, ncol = 2)
# figure 2: powerpoint

# figure 3 a) # HERE SIMULATION NEED TO BE RERUN WITH SMALLER RANGE
sym_res <- readRDS(file.path("simulation_scripts", "asymmetry_experiments_RDS_wt1e+06", "simulation_asymmetric_stressor.asym1.RDS")) # file.path(wd, "simulation_symmetric_temporal.RDS")
fig_3a <- plot_trajectory_symmetry(res = sym_res, trajectory = "recovery", typ = "substrate") #+ labs(x = "aO",  y = "concentration")
fig_3b <- plot_trajectory_symmetry(res = sym_res, trajectory = "collapse", typ = "substrate")
asym_res_trait <- readRDS(file.path("simulation_scripts", "asymmetry_experiments_RDS_wt1e+06", "simulation_asymmetric_hOSB60.RDS"))
fig_3c <- plot_trajectory_symmetry(res = asym_res_trait, trajectory = "recovery", typ = "substrate")
fig_3d <- plot_trajectory_symmetry(res = asym_res_trait, trajectory = "collapse", typ = "substrate")
asym_res_stressor <- readRDS(file.path("simulation_scripts", "asymmetry_experiments_RDS_wt1e+06", "simulation_asymmetric_stressor.asym0.6.RDS"))
fig_3e <- plot_trajectory_symmetry(res = asym_res_stressor, trajectory = "recovery", typ = "substrate")
fig_3f <- plot_trajectory_symmetry(res = asym_res_stressor, trajectory = "collapse", typ = "substrate")
fig_3 <- ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, fig_3f, nrow = 3, ncol = 2)

# figure 4

# save all the figure
if (!(dir.exists("figures"))){dir.create("figures")}
a <- 5

# ggsave(filename = file.path("figures", "Fig1c_bush_recovery.png"), plot = fig_1c, dpi = "retina", height = a, width = a, unit = "cm")
# ggsave(filename = file.path("figures", "Fig1d_bush_collapse.png"), plot = fig_1d, dpi = "retina", height = a, width = a, unit = "cm")
ggsave(filename = file.path("figures", "Fig1cd_bush_trajectories.png"), plot = fig_1cd, dpi = "retina", height = a, width = 2*a + 0.5, unit = "cm")
# ggsave(filename = file.path("figures", "Fig3a_sym_recovery.png"), plot = fig_3a, dpi = "retina", height = a, width = a, unit = "cm")
# ggsave(filename = file.path("figures", "Fig3b_sym_collapse.png"), plot = fig_3b, dpi = "retina", height = a, width = a, unit = "cm")
ggsave(filename = file.path("figures", "Fig_3_asymmetric trajectories.png"), plot = fig_3, dpi = "retina", height = 3 * a + 2 * 0.5, width = 2*a + 0.5, unit = "cm")

