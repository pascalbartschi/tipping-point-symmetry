# import the analysis 
source("analysis_scripts/asymmetric_response_analysis.R")

# set the theme

theme_set(
  theme(title = element_blank(),
        text = element_text(family  ="Arial", size = 7),
        axis.title = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Arial", size = 5),
        # axis.text.y = element_blank(),
        panel.grid.major  = element_line(linewidth = 0.3,
                                         linetype = "dashed",
                                         color = "#dedddd"),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "both")),
        axis.line.y = element_line(),
        # axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
        #                                                ends = "last")),
        legend.position = "none") 
)

### plot gmax
## shifts
ggplot(data = asym_measures$gmaxS %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = abs(asym_val - sym_val), y = shift, shape = shift_type, color = species), size = 4) + 
  labs(x = "Delta gmax values")

## hysteresis area
ggplot(data = asym_measures$gmaxS %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = abs(asym_val - sym_val), y = hyst_area, color = species), size = 4) + 
  labs(x = "Delta gmax values")

### plot hOSB
## shifts
ggplot(data = asym_measures$hOSB %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = abs(asym_val - sym_val), y = shift, shape = shift_type, color = species), size = 4) + 
  labs(x = "Delta hOSB values")

## hysteresis area
ggplot(data = asym_measures$hOSB %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = abs(asym_val - sym_val), y = hyst_area, color = species), size = 4) + 
  labs(x = "Delta hOSB values")

### plot stressor
## shifts
ggplot(data = asym_measures$stressor %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = abs(asym_val - sym_val), y = shift, shape = shift_type, color = species), size = 4) + 
  labs(x = "Delta asym factor")