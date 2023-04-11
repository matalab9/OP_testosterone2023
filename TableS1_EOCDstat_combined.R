rm(list = ls())

# package
library(tidyverse)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# run EOCD parameter program
source(paste0(dir_home, "/EOCD_para.R"))
rm(list = subset(ls(), !(ls() %in% c("df_para", "dir_home"))))

# Onset slope
slope_onset <- anova(lm(Slop1 ~ Treatment * Day * Surgery, data = df_para))
inflectionpoint_onset <- anova(lm(Infl1 ~ Treatment * Day * Surgery, data = df_para))
slope_offset <- anova(lm(Slop2 ~ Treatment * Day * Surgery, data = df_para))
inflectionpoint_offset <- anova(lm(Infl2 ~ Treatment * Day * Surgery, data = df_para))

tables1 <- rbind(slope_onset, inflectionpoint_onset, slope_offset, inflectionpoint_offset)

write.csv(tables1, "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data/TableS1.csv")
