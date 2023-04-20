## run all symmetric experiments

## load useful libraries
library(here)
library(microxanox)
library(tidyverse)
source("sym_functions.R") # altered functions not in package

## create local directory if not exist
foldername <- "ss_data_1"

if (!dir.exists(here::here(foldername))){
  dir.create(foldername)
}

## test flag to test script
test <- FALSE

## perform experiments upon wait_time vector
if (test == TRUE){
  wait_times <- c(1, 10, 100)
} else {wait_times <- c(1e6)}


for (wait_time in wait_times){
  ssfile_path <- file.path(foldername,
                        paste0("result_temporal_ss_find",
                               formatC(wait_time, format = "e", digits = 0),
                               ".RDS"))

  source("run_sym_experiment.R")
  print(paste("Wait_time",
               formatC(wait_time, format = "e", digits = 0),
              "finished"))
}
