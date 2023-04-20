# run symmetric experiment

## load useful libraries
library(here)
library(microxanox)
library(tidyverse)
source("sym_functions.R") # altered functions not in package

num_cores <- 1
# use of multiple cores
# options(mc.cores = num_cores)

## setup the experiment
wait_time <- 1e4
source("setup_bush_experiment.R")


# estimate time it will take to process
time <- (wait_time * length(parameter$log10a_series)) / (num_cores * 0.000123965 + 126)

# parameter$log10a_series <- rep(-1, 300)
# parameter$strain_parameter$initial_state["CB_1"] <- init_state


expt_res <- run_temporal_ssfind(parameter)

file_path <-file.path("ss_data", "bush_experiment.RDS")

saveRDS(expt_res, file_path)




