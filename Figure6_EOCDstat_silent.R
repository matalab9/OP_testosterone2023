rm(list = ls())

# package
library(tidyverse)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# run EOCD parameter program
source(paste0(dir_home, "/EOCD_para.R"))
rm(list = subset(ls(), !(ls() %in% c("df_para", "dir_home", "df"))))


# figure 6d (representative inhibition curves) ----------------------------

fig6d <- filter(df, Surgery == "Silent", Day > 13) %>% 
  ggplot(aes(x = Latency, y = Amps, colour = Treatment, group = ID)) +
  geom_line() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Stimulus delay from CN onset (ms)") +
  ylab("Normalized amplitude\nof evoked potential")
fig6d


# figure 6e (day & onset slope) -------------------------------------------

anova(lm(Slop1 ~ Treatment * Day, filter(df_para, Surgery == "Silent")))

fig6e <- filter(df_para, Surgery == "Silent") %>% 
  ggplot(aes(x = Day, y = Slop1, colour = Treatment)) +
  geom_point(shape = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Onset slope")
fig6e


# figure 6f (day & onset inflection point) --------------------------------

anova(lm(Infl1 ~ Treatment * Day, filter(df_para, Surgery == "Silent")))

fig6f <- filter(df_para, Surgery == "Intact") %>% 
  ggplot(aes(x = Day, y = Infl1, colour = Treatment)) +
  geom_point(shape = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Onset inflection point")
fig6f


# figure 6g (day & offset slope) -------------------------------------------

anova(lm(Slop2 ~ Treatment * Day, filter(df_para, Surgery == "Silent")))

fig6g <- filter(df_para, Surgery == "Silent") %>% 
  ggplot(aes(x = Day, y = Slop2, colour = Treatment)) +
  geom_point(shape = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Offset slope")
fig6g


# figure 4h (day & onset inflection point) --------------------------------

anova(lm(Infl2 ~ Treatment * Day, filter(df_para, Surgery == "Silent")))

fig6h <- filter(df_para, Surgery == "Silent") %>% 
  ggplot(aes(x = Day, y = Infl2, colour = Treatment)) +
  geom_point(shape = 1) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Offset inflection point")
fig6h
