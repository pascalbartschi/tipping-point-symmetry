library(tidyverse)

# asymmetry factor, change this to observe
asym_factor <- 0.2 # mutliplies the extremes of the sequence, is and argument of the parameter set

# define prerequesites
time <- seq(0, 100, length = 300) # times series
diffusivity <- seq(-2, 0, length = 300)
axis <- mean(diffusivity)

# calculate diff2
delta <- abs(axis - min(diffusivity)) * asym_factor
under <- axis - delta
upper <- axis + delta
diffusivity2 <- 2*axis - (seq(under, upper, length = length(diffusivity))) # formula

ggplot() + 
  geom_line(aes(x = time, y=  diffusivity, color = "a1")) + 
  geom_line(aes(x=time, y = diffusivity2, color = "a2")) + 
  geom_hline(aes(yintercept = axis, color = "sym axis"), linetype = "dashed") + 
  theme_bw()



