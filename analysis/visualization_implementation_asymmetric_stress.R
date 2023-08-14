library(ggplot2)
library(tidyverse)

# asymmetry factor, change this to observe
asym_factor <- 0.6 # mutliplies the extremes of the sequence, is and argument of the parameter set

# define prerequesites
time <- seq(0, 100, length = 300) # times series
diffusivity <- seq(-2, 0, length = 300)
axis <- mean(diffusivity)

# calculate diff2
delta <- abs(axis - min(diffusivity)) * asym_factor
under <- axis - delta
upper <- axis + delta
diffusivity2 <- 2*axis - (seq(under, upper, length = length(diffusivity))) # formula

plt.df <- data.frame(value = c(diffusivity, diffusivity2), 
                     key = rep(c("Oxygen diffusivity", "Sulfur diffusivity"), each = 300), 
                     time = rep(time, times = 2))

ggplot(data = plt.df) + 
  geom_line(aes(x = time, y = value, color = key)) + 
  scale_color_manual(values = c("#e85050", "#5da1df")) +
  # geom_hline(aes(yintercept = axis, color = "sym axis"), linetype = "dashed") + 
  labs(x = "Time", y = "", color = "") + 
  theme_bw()



