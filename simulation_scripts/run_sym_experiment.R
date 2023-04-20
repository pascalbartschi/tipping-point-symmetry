# run symmetric experiment

num_cores <- 26
# use of multiple cores
options(mc.cores = num_cores)

## setup the experiment
source("setup_sym_experiment.R")


# estimate time it will take to process
time <- (wait_time * length(parameter$log10a_series)) / (num_cores * 0.000123965 + 126)


expt_res <- sym_run_temporal_ssfind(parameter)

saveRDS(expt_res, ssfile_path)

