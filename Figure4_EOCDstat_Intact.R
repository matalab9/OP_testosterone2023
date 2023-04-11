rm(list = ls())

# package
library(tidyverse)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# run EOCD parameter program
source(paste0(dir_home, "/EOCD_para.R"))
rm(list = subset(ls(), !(ls() %in% c("df_para", "dir_home", "df"))))


# figure 4c (representative inhibition curves) ----------------------------

fig4c <- filter(df, Surgery == "Intact", Day > 13) %>% 
  ggplot(aes(x = Latency, y = Amps, colour = Treatment, group = ID)) +
  geom_line() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Stimulus delay from EODC onset (ms)") +
  ylab("Normalized amplitude\nof evoked potential")
fig4c


# figure 4e (day & onset slope) -------------------------------------------

anova(lm(Slop1 ~ Treatment * Day, filter(df_para, Surgery == "Intact")))

fig4e <- filter(df_para, Surgery == "Intact") %>% 
  ggplot(aes(x = Day, y = Slop1, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Onset slope")
fig4e


# figure 4f (day & onset inflection point) --------------------------------

anova(lm(Infl1 ~ Treatment * Day, filter(df_para, Surgery == "Intact")))

fig4f <- filter(df_para, Surgery == "Intact") %>% 
  ggplot(aes(x = Day, y = Infl1, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Onset inflection point")
fig4f


# figure 4g (day & offset slope) -------------------------------------------

anova(lm(Slop2 ~ Treatment * Day, filter(df_para, Surgery == "Intact")))

fig4g <- filter(df_para, Surgery == "Intact") %>% 
  ggplot(aes(x = Day, y = Slop2, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Offset slope")
fig4g


# figure 4h (day & onset inflection point) --------------------------------

anova(lm(Infl2 ~ Treatment * Day, filter(df_para, Surgery == "Intact")))

fig4h <- filter(df_para, Surgery == "Intact") %>% 
  ggplot(aes(x = Day, y = Infl2, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Offset inflection point")
fig4h
