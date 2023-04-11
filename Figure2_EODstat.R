rm(list = ls())

# package
library(tidyverse)
library(nlme)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# load dataset
df <- read.csv(paste0(dir_home, "/EOD_stat.csv"))

# figure 2b (day & EOD duration) ----------------------------------------

# linear mixed-effect model
anova(lme(Duration ~ Treatment * Day, random = ~1|ID, method = "ML", data = df))

# plot
fig2b <- ggplot(df) +
  geom_line(aes(x = Day, y = Duration, colour = Treatment, group = ID)) +
  stat_summary(aes(x = Day, y = Duration, colour = Treatment), fun = mean, geom = "line", size = 1) +
  stat_summary(aes(x = Day, y = Duration, colour = Treatment), fun.data = "mean_se", geom = "errorbar", size = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  scale_x_continuous(breaks = seq(0, 13, 1), labels = as.vector(t(cbind(seq(0, 12, 2), rep("", 7))))) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("EOD duration (ms)")
fig2b


# figure 2c (day & ppf) -------------------------------------------------

# linear mixed-effect model
anova(lme(PeakFreq ~ Treatment * Day, random = ~1|ID, method = "ML", data = df))

# plot
fig2c <- ggplot(df) +
  geom_line(aes(x = Day, y = PeakFreq, colour = Treatment, group = ID)) +
  stat_summary(aes(x = Day, y = PeakFreq, colour = Treatment), fun = mean, geom = "line", size = 1) +
  stat_summary(aes(x = Day, y = PeakFreq, colour = Treatment), fun.data = "mean_se", geom = "errorbar", size = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  scale_x_continuous(breaks = seq(0, 13, 1), labels = as.vector(t(cbind(seq(0, 12, 2), rep("", 7))))) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Peak power frequency (kHz)")
fig2c


# figure 2d (day & dp1) -------------------------------------------------

# linear mixed-effect model
anova(lme(DP1 ~ Treatment * Day, random = ~1|ID, method = "ML", data = df))

# plot
fig2d <- ggplot(df) +
  geom_line(aes(x = Day, y = DP1, colour = Treatment, group = ID)) +
  stat_summary(aes(x = Day, y = DP1, colour = Treatment), fun = mean, geom = "line", size = 1) +
  stat_summary(aes(x = Day, y = DP1, colour = Treatment), fun.data = "mean_se", geom = "errorbar", size = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  scale_x_continuous(breaks = seq(0, 13, 1), labels = as.vector(t(cbind(seq(0, 12, 2), rep("", 7))))) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Delay to EOD peak 1 (ms)")
fig2d

