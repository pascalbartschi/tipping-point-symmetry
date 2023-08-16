# This script aims to visualize asymmetric response as a function of assymmetric conditions/ traits

# load the packages
source("experiments/read_microxanox.R")

paths = c("data/asymmetric-sim/trait-asym/",  "data/asymmetric-sim/stressor-asym/", "data/asymmetric-sim/trait-asym/") 

# extract files matching pattern
files = list()

files$hOSB <- list.files(path = paths[1], pattern = "*hOSB*")
files$stressor <- list.files(path = paths[2], pattern = "*stressor*")
# files$gmaxS <- list.files(path = "data/asymmetric-sim/trait-asym/", pattern = "*gmaxSB*") # these files must be requested, as they are gitignored!

columns <-  c("species", "species_type", "shift", "shift_type", "hyst_area", "asym_val", "shift_sym", "hyst_area_sym", "sym_val")
asym_measures <- lapply(files, function(f) {
  data.frame(matrix(nrow = 0, ncol = length(columns)), stringsAsFactors = FALSE)
})
names(asym_measures) <- names(files)
species_names <- c("CB", "SB", "SR", "O")
nrowlen <- length(species_names) * 2

# laod symmetric simulation
baseline <- readRDS("data/symmetric-sim/simulation_symmetric_temporal.RDS")
bm <- get_symmetry_measurements(baseline)
baseline_h_O_SB <- 100
baseline_gmaxSB <- 0.1
baseline_asym_factor <- 1
baseline_vals <- c(baseline_h_O_SB, baseline_asym_factor, baseline_gmaxSB)


for (i in 1:length(files)) {
  # name columns
  colnames(asym_measures[[i]]) <- columns
  for (filename in files[[i]]){
    # print(paste("Analysed:", filename))
    tm <- get_symmetry_measurements(readRDS(paste0(paths[i], filename)))
    newrows <- data.frame("species" = rep(species_names, times = nrowlen/4),
                          "species_type" = rep(c("bacteria", "bacteria", "substrate", "substrate"), times = nrowlen/4),
                          "shift" = c(tm$abs_shift_recovery[1:(nrowlen/2)], tm$abs_shift_catastrophy[1:(nrowlen/2)]), 
                          "shift_type" = rep(c("recovery", "collapse"), each = nrowlen/2),
                          "hyst_area" = rep(tm$hyst_area[1:(nrowlen/2)], times = nrowlen/4),
                          "hyst_range" = rep(tm$hyst_range[1:(nrowlen/2)], times = nrowlen/4),
                          "anox_TP" = rep(tm$anox_TP[1:(nrowlen/2)], times = nrowlen/4),
                          "ox_TP" = rep(tm$ox_TP[1:(nrowlen/2)], times = nrowlen/4),
                          "asym_val" = rep(as.numeric(str_extract_all(filename, "[-+]?\\d*\\.?\\d+")[[1]]), times = nrowlen),
                          "shift_sym" = c(bm$abs_shift_recovery[1:(nrowlen/2)], bm$abs_shift_catastrophy[1:(nrowlen/2)]), 
                          "hyst_area_sym" = rep(bm$hyst_area[1:(nrowlen/2)], times = nrowlen/4),
                          "hyst_range_sym" = rep(bm$hyst_range[1:(nrowlen/2)], times = nrowlen/4),
                          "anox_TP_sym" = rep(bm$anox_TP[1:(nrowlen/2)], times = nrowlen/4),
                          "ox_TP_sym" = rep(bm$ox_TP[1:(nrowlen/2)], times = nrowlen/4),
                          "sym_val" = rep(baseline_vals[i], times = nrowlen))
    asym_measures[[i]] <- rbind(newrows, asym_measures[[i]])
  }
}









