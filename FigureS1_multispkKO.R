rm(list = ls())

# package
library(tidyverse)
library(nlme)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# load dataset
df <- read.csv(paste0(dir_home, "/KO_stat.csv")) %>% 
  filter(t_peak1_DP1 > 0, t_peak1_DP1 < 1)

# plot
figs1b <- ggplot(df, aes(x = t_peak2, y = z_peak2, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  geom_rect(aes(xmin = 0, xmax = 5, ymin = 3, ymax = 8.5), fill = NA, color = "blue") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank(), legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  xlab("Second peak time (ms)") +
  ylab("Z-score of second peak")
figs1b
