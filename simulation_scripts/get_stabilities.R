library(tidyverse)
library(microxanox)
source("sym_functions.R")


files <- list.files("ss_data/")
ss_measures <- list()
names <- c()
# gather the stabitity measures
for (i in 1:length(files)){

  file <- files[i]
   # read data
  res <- readRDS(file.path("ss_data", file))
  # apply function
  mes <- sym_get_stability_measures_temporal_ssfind_result(res)
  name <- sub("result_temporal_ss_find","", file)
  names <- c(names, paste0("meas_", sub(".RDS", "", name)))
  ss_measures[[i]] <- mes
  
  sym_plot_temporal_ss(res)
}

names(ss_measures) <- names
