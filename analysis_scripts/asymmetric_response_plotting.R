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

plot_asymmetric_response <- function(asym_measures,
                                     subject,
                                     sz = 2,
                                     lw = 4,
                                     pprint = TRUE
                                     ){
  if (!(subject %in% names(asym_measures))){
    stop("Please input subject contained in list asym_measures. Call names(asym_measures) to check available subjects.")
    }
  # search for index
  index = which(subject == names(asym_measures))
  # individual shifts
  p1 <- ggplot(data = asym_measures[[index]] %>% filter(species_type == "substrate")) + 
    geom_point(aes(x = (asym_val - sym_val), y = shift, shape = shift_type, color = species), size = sz) + 
    geom_point(aes(x = 0, y = shift_sym, shape = shift_type), size = sz, color = "#7E3C2F") +
    scale_color_manual(values = c("#00BD54", "#FF0000")) +
    labs(title = paste("SB", subject, "shifts"), x = paste("Delta", subject, "values"))
  
  ## hysteresis area
  p2 <- ggplot(data = asym_measures[[index]] %>% filter(species_type == "substrate")) + 
    geom_point(aes(x = (asym_val - sym_val), y = hyst_area, color = species), size = sz) + 
    geom_point(aes(x = 0, y = hyst_area_sym), size = sz, color = "#7E3C2F") +
    scale_color_manual(values = c("#00BD54", "#FF0000")) +
    labs(title = paste("SB", subject, "hysteresis area"), x = paste("Delta", subject, "values"))
  
  ## hysteresis range
  p3 <- ggplot(data = asym_measures[[index]] %>% filter(species_type == "substrate")) + 
    geom_point(aes(x = (asym_val - sym_val), y = hyst_range, color = species), size = sz) + 
    geom_point(aes(x = 0, y = hyst_range_sym), size = sz, color = "#7E3C2F") +
    scale_color_manual(values = c("#00BD54", "#FF0000")) +
    labs(title = paste("SB", subject, "hysteresis range"), x = paste("Delta", subject, "values"))
  
  ## tipping point delta
  p4 <- asym_measures[[index]] %>% 
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
    labs(title = paste("SB", subject, "TP delta"), x = paste("Delta", subject, "values"))
  
  # measurement for total shift for one direction of environmental change
  # add up the recovery shifts of SR and collapse shifts of O for instance
  p5 <- cbind(rbind(asym_measures[[index]] %>% 
                filter(species_type == "substrate", species == "SR", shift_type == "recovery") %>%
                select(-c(species_type, hyst_area, hyst_range, hyst_area_sym, hyst_range_sym)),
              asym_measures[[index]] %>% 
                filter(species_type == "substrate", species == "SR", shift_type == "collapse") %>%
                select(-c(species_type, hyst_area, hyst_range, hyst_area_sym, hyst_range_sym))) %>%
          rename(SR_shift = shift, SR_shift_type = shift_type),
        rbind(asym_measures[[index]] %>% 
                filter(species_type == "substrate", species == "O", shift_type == "collapse") %>%
                select(c(shift, shift_type, species)),
              asym_measures[[index]] %>% 
                filter(species_type == "substrate", species == "O", shift_type == "recovery") %>%
                select(c(shift, shift_type, species))) %>%
          rename(O_shift = shift, O_shift_type = shift_type)) %>%
    select(-species) %>%
    mutate(total_shift = O_shift + SR_shift, 
           total_shift_to = ifelse(SR_shift_type == "recovery", "anoxic", "oxic"), 
           total_shift_sym = sum(shift_sym) / (nrow(.)/2)) %>% 
    ggplot()+
    geom_point(aes(x = (asym_val - sym_val), y = total_shift, color = total_shift_to)) + 
    geom_point(aes(x = 0, y = total_shift_sym), color = "#7E3C2F")  + 
    scale_color_manual(values = c("#FF0000", "#00BD54")) +
    labs(title = paste("SB", subject, "total shift"), x = paste("Delta", subject, "values"))
  plot_list <- list(p1, p2, p3, p4, p5)
  
  if (pprint){
    for (plot in plot_list){print(plot)}
  }
  else {
    return(plot_list) 
  }
}

# plot_asymmetric_response(asym_measures, "gmaxS")

plot_asymmetric_response(asym_measures, "hOSB")
# two discussion points:
#* hOSB enhanced (SB favoured in tolerance towards stress) results in higher total shift to anoxic state. 
#  Bigger shift means larger change in structure and function of the ecosystem (Regime shifts), and thus the more
#  tolerant strain induces more ecosystem change once tipping point has been hit.
#* h0SB decreased (SB infavoured), more ecosystem change is induced when SB collapse. Attention: When in anoxic state
#  and trying to recover oxic, decreasing tolerance of SB leads to "earlier" TP of SB, but the shift back may be greater,
# than expected!
#* it may be important whether tolerance is increased or decreased as the shift deltas are greater when decreased

# plot_asymmetric_response(asym_measures, "stressor")
