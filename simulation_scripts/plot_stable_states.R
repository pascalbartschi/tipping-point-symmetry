### visulizie experiment outputs
library(here)
library(microxanox)
library(tidyverse)

ss_dir <- "ss_data"
files <- list.files(here::here(ss_dir))

for (file in files){
  dat <- readRDS(file.path(ss_dir,
                           file))
  p <- sym_plot_temporal_ss(dat)
  ggsave(file.path(ss_dir, 
                   paste0(file, "_plot.pdf")), 
         p)
}

