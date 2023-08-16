# estimate algorithmic complexity

source("analysis/read_microxanox.R")

wts <- c(1e2, 1e3, 1e4)
steps <- 300
times <- rep(NA, times = 3)
c = 0


for (wt in wts){
  c <- c + 1
  wait_time <- wt
  source("experiments/setup_sym_experiment_temporal.R")
  a <- Sys.time()
  print(run_temporal_ssfind_symmetric(parameter))
  b <- Sys.time()
  times[c] <- b - a
  
}

df <- data.frame(duration = wts * steps, 
                 times = times)

ggplot(data = df, mapping = aes(x = log10(duration), y = times)) + 
  geom_point() + 
  labs(x = "log10(duration)", y = "time[s]") +
  theme_bw()
