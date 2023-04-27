# read the package manually
source("analysis_scripts/read_microxanox.R")

# set the theme such that all figures look the same
theme_set(
  theme(text = element_text(family  ="Arial", size = 7),
        axis.title = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Arial", size = 5),
        # axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                      ends = "last")),
        # axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
        #                                                ends = "last")),
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

# figure 3
sym_res <- readRDS(file.path(wd, "simulation_symmetric_temporal.RDS"))
fig_3a <- plot_trajectory_symmetry(res = sym_res, trajectory = "recovery", typ = "substrate") #+ labs(x = "aO",  y = "concentration")
fig_3b <- plot_trajectory_symmetry(res = sym_res, trajectory = "collapse", typ = "substrate")

# save all the figure
if (!(dir.exists("figures"))){dir.create("figures")}
a <- 5

ggsave(filename = file.path("figures", "Fig1c_bush_recovery.png"), plot = fig_1c, dpi = "retina", height = a, width = a, unit = "cm")
ggsave(filename = file.path("figures", "Fig1d_bush_collapse.png"), plot = fig_1d, dpi = "retina", height = a, width = a, unit = "cm")
ggsave(filename = file.path("figures", "Fig1cd_bush_trajectories.png"), plot = fig_1cd, dpi = "retina", height = a, width = 2*a + 0.5, unit = "cm")
ggsave(filename = file.path("figures", "Fig3a_sym_recovery.png"), plot = fig_3a, dpi = "retina", height = a, width = a, unit = "cm")
ggsave(filename = file.path("figures", "Fig3b_sym_collapse.png"), plot = fig_3b, dpi = "retina", height = a, width = a, unit = "cm")

