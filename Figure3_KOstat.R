rm(list = ls())

# package
library(tidyverse)
library(nlme)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# load dataset
df <- read.csv(paste0(dir_home, "/KO_stat.csv")) %>% 
  filter(t_peak1_DP1 > 0, t_peak1_DP1 < 1)


# figure 3c (day & KO peak) -----------------------------------------------

# linear mexed-effect model
anova(lme(t_peak1 ~ Day * Treatment, random = ~1|KO_ID, method = "ML", data = df))

# plot
fig3c <- ggplot(df, aes(x = Day, y = t_peak1, colour = Treatment, group = KO_ID)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  xlab("Days after tretment") +
  ylab("KO peak latency (ms)") +
  scale_x_continuous(breaks = seq(2, 16, 2)) +
  theme(legend.position = "none")
fig3c


# figure 3d (dp1 & KO peak latency from EOD onset) -----------------------------------------------

# linear mixed-effect model
model_3d <- lme(t_peak1 ~ DP1, random = ~1|KO_ID, method = "ML", data = df)
anova(model_3d)

# plot
fig3d <- ggplot(df, aes(x = DP1, y = t_peak1, colour = Treatment, group = KO_ID)) +
  geom_point(aes(colour = Treatment)) +
  geom_line(aes(colour = Treatment, group = KO_ID)) +
  geom_abline(slope = model_3d$coefficients$fixed[2], intercept = model_3d$coefficients$fixed[1], color = "blue", 
              linewidth = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  xlab("Delay to EOD peak 1 (ms)") +
  ylab("KO peak latency (ms)") +
  theme(legend.position = "none")
fig3d


# figure 3e (dp1 & KO peak latency from dp1) ----------------------------------------

# linear mixed-effect model
model_3e <- lme(t_peak1_DP1 ~ DP1, random = ~1|KO_ID, method = "ML", data = df)
anova(model_3e)

# plot
fig3e <- ggplot(df, aes(x = DP1, y = t_peak1_DP1, colour = Treatment, group = KO_ID)) +
  geom_point(aes(colour = Treatment)) +
  geom_line(aes(colour = Treatment, group = KO_ID)) +
  geom_abline(slope = model_3e$coefficients$fixed[2], intercept = model_3e$coefficients$fixed[1], color = "blue", 
              linewidth = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  xlab("Delay to EOD peak 1 (ms)") +
  ylab("KO peak latency from EOD onset (ms)") +
  theme(legend.position = "none")
fig3e
