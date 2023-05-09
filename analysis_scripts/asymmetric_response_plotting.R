# import the analysis 
source("analysis_scripts/asymmetric_response_analysis.R")

# set the theme

theme_set(
  theme(# title = element_blank(),
        text = element_text(family  ="Arial", size = 7),
        axis.title = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Arial", size = 5),
        # axis.text.y = element_blank(),
        panel.grid  = element_line(linewidth = 0.3,
                                         linetype = "dashed",
                                         color = "#dedddd"),
        panel.background = element_blank(),
        # axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
        #                                                ends = "both")),
        #axis.line.y = element_line(),
        # axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
        #                                                ends = "last")),
        panel.border = element_rect(fill = NA, linewidth = 0.3),
        # legend.position = "none"
        ) 
)

# control the plot properties
sz = 4
lw = 2
### plot gmax
## shifts
ggplot(data = asym_measures$gmaxS %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = shift, shape = shift_type, color = species), size = sz) + 
  geom_point(aes(x = 0, y = shift_sym, shape = shift_type), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "gmax, shifts", x = "Delta gmax values")

## hysteresis area
ggplot(data = asym_measures$gmaxS %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_area, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_area_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "SB gmax, hysteresis area", x = "Delta gmax values")

## hysteresis range
ggplot(data = asym_measures$gmaxS %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_range, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_range_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "SB gmax, range", x = "Delta gmax values")

## tipping point delta
asym_measures$gmaxS %>% 
  filter(species_type == "substrate") %>% 
  select(contains("TP"), asym_val, sym_val) %>%
  mutate(anox_TP_delta = anox_TP_sym - anox_TP, 
         ox_TP_delta = ox_TP_sym - ox_TP) %>%
  select(ends_with("delta"), asym_val, sym_val) %>%
  gather(key = "delta_TP_type", value = "delta_TP_value", -c(asym_val, sym_val)) %>%

  ggplot() + 
    geom_point(aes(x = (asym_val - sym_val), y = delta_TP_value, color = delta_TP_type), size = sz) + 
    geom_point(aes(x = 0, y = 0), size = sz, color = "#7E3C2F") +
    scale_color_manual(values = c("#FF0000", "#00BD54")) +
    labs(title = "SB gmax, TP_delta", x = "Delta TP values")



### plot hOSB
## shifts
ggplot(data = asym_measures$hOSB %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = shift, shape = shift_type, color = species), size = sz) + 
  geom_point(aes(x = 0, y = shift_sym, shape = shift_type), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "hOSB, shifts", x = "Delta h_O_SB values")

## hysteresis area
ggplot(data = asym_measures$hOSB %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_area, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_area_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "hOSB, area", x = "Delta h_O_SB values")

## hysteresis range
ggplot(data = asym_measures$hOSB %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_range, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_range_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "hOSB, range", x = "Delta gmax values")

## tipping point delta
asym_measures$hOSB %>% 
  filter(species_type == "substrate") %>% 
  select(contains("TP"), asym_val, sym_val) %>%
  mutate(anox_TP_delta = anox_TP_sym - anox_TP, 
         ox_TP_delta = ox_TP_sym - ox_TP) %>%
  select(ends_with("delta"), asym_val, sym_val) %>%
  gather(key = "delta_TP_type", value = "delta_TP_value", -c(asym_val, sym_val)) %>%
  
  ggplot() + 
  geom_point(aes(x = (asym_val - sym_val), y = delta_TP_value, color = delta_TP_type), size = sz) + 
  geom_point(aes(x = 0, y = 0), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#FF0000", "#00BD54")) +
  labs(title = "hOSB, TP_delta", x = "Delta TP values")

### plot stressor
## shifts
ggplot(data = asym_measures$stressor %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = shift, shape = shift_type, color = species), size = sz) + 
  geom_point(aes(x = 0, y = shift_sym, shape = shift_type), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "stressor, shifts", x = "Delta gmax values")

## hysteresis area
ggplot(data = asym_measures$stressor %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_area, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_area_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "stressor, area", x = "Delta gmax values")

## hysteresis range
ggplot(data = asym_measures$stressor %>% filter(species_type == "substrate")) + 
  geom_point(aes(x = (asym_val - sym_val), y = hyst_range, color = species), size = sz) + 
  geom_point(aes(x = 0, y = hyst_range_sym), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#00BD54", "#FF0000")) +
  labs(title = "stressor, range", x = "Delta gmax values")

## tipping point delta
asym_measures$stressor %>% 
  filter(species_type == "substrate") %>% 
  select(contains("TP"), asym_val, sym_val) %>%
  mutate(anox_TP_delta = anox_TP_sym - anox_TP, 
         ox_TP_delta = ox_TP_sym - ox_TP) %>%
  select(ends_with("delta"), asym_val, sym_val) %>%
  gather(key = "delta_TP_type", value = "delta_TP_value", -c(asym_val, sym_val)) %>%
  
  ggplot() + 
  geom_point(aes(x = (asym_val - sym_val), y = delta_TP_value, color = delta_TP_type), size = sz) + 
  geom_point(aes(x = 0, y = 0), size = sz, color = "#7E3C2F") +
  scale_color_manual(values = c("#FF0000", "#00BD54")) +
  labs(title = "stressor, TP_delta", x = "Delta TP values")

