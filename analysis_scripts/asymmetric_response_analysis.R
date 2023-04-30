# this script aims to visualize asymmetric response as a function of assymmetric conditions/ traits

# load the packages
source("simulation_scripts/read_microxanox.R")

# specifiy folder
wt <- 1e6
path <- paste0("simulation_scripts/asymmetry_experiments_RDS_wt", wt, "/")
# extract files matching pattern
files = list()
files$gmaxS <- list.files(path = path, pattern = "*gmaxS*")
files$hOSB <- list.files(path = path, pattern = "*hOSB*")
files$stressor <- list.files(path = path, pattern = "*stressor*")

columns <-  c("species", "species_type", "shift", "shift_type", "hyst_area", "asym_val", "shift_sym", "hyst_area_sym", "sym_val")
asym_measures <- lapply(files, function(f) {
  data.frame(matrix(nrow = 0, ncol = length(columns)), stringsAsFactors = FALSE)
})
names(asym_measures) <- names(files)
species_names <- c("CB", "SB", "SR", "O")
nrowlen <- length(species_names) * 2

# laod symmetric simulation
baseline <- readRDS(paste0("simulation_scripts/experiments_RDS_wt", wt, "/simulation_symmetric_temporal.RDS"))
bm <- get_symmetry_measures(baseline)
baseline_h_O_SB <- 100
baseline_gmaxSB <- 0.1
baseline_asym_factor <- 1
baseline_vals <- c(baseline_gmaxSB, baseline_h_O_SB, baseline_asym_factor)


for (i in 1:length(files)) {
  # name columns
  colnames(asym_measures[[i]]) <- columns
  for (filename in files[[i]]){
    tm <- get_symmetry_measures(readRDS(paste0(path, filename)))
    newrows <- data.frame("species" = rep(species_names, times = nrowlen/4),
                          "species_type" = rep(c("bacteria", "bacteria", "substrate", "substrate"), times = 2),
                          "shift" = c(tm$abs_shift_recovery[1:(nrowlen/2)], tm$abs_shift_catastrophy[1:(nrowlen/2)]), 
                          "shift_type" = rep(c("recovery", "collapse"), each = nrowlen/2),
                          "hyst_area" = rep(tm$hyst_area[1:(nrowlen/2)], times = nrowlen/4),
                          "asym_val" = rep(as.numeric(str_extract_all(filename, "[-+]?\\d*\\.?\\d+")[[1]]), times = nrowlen),
                          "shift_sym" = c(bm$abs_shift_recovery[1:(nrowlen/2)], bm$abs_shift_catastrophy[1:(nrowlen/2)]), 
                          "hyst_area_sym" = rep(bm$hyst_area[1:(nrowlen/2)], times = nrowlen/4),
                          "sym_val" = rep(baseline_vals[i], times = nrowlen))
    asym_measures[[i]] <- rbind(newrows, asym_measures[[i]])
  }
}









