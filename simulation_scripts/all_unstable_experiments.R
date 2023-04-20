## run all symmetric experiments

## load useful libraries
library(here)
library(microxanox)
library(tidyverse)
source("sym_functions.R") # altered functions not in package

## create local directory if not exist
foldername <- "eq_data"

if (!dir.exists(here::here(foldername))){
  dir.create(foldername)
}

## test flag to test script
test <- FALSE

## perform experiments upon wait_time vector
if (test == TRUE){
  init_states_CB <- c()
} else {init_states_CB <- c(1e5)}


for (init_state in init_states_CB){
  ssfile_path <- file.path(foldername,
                           paste0("result_temporal_unstable_initCB_",
                                  formatC(init_state, format = "e", digits = 0),
                                  ".RDS"))
  
  ssfile_path <- file.path(foldername,
                           paste0("result_temporal_unstable_initCB_",
                                  formatC(init_state, format = "e", digits = 0),
                                  "_2.RDS"))
  
  source("run_unstable_experiment.R")
  
  print(paste("Init CB",
              formatC(init_state, format = "e", digits = 0),
              "finished"))
}
