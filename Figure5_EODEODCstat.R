rm(list = ls())

# package
library(tidyverse)

# set your directory
dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# run EOCD parameter program
source(paste0(dir_home, "/EOCD_para.R"))
rm(list = subset(ls(), !(ls() %in% c("df_para", "dir_home", "df"))))

# KO data
df_KO <- read.csv(paste0(dir_home, "/KO_stat.csv")) %>% 
  filter(t_peak1_DP1 > 0, t_peak1_DP1 < 1)
model <- lme(t_peak1 ~ DP1, random = ~1|KO_ID, method = "ML", data = df_KO)
intercept <- model$coefficients$fixed[1]
slope <- model$coefficients$fixed[2]

# EOCD data
df_para <- filter(df_para, Surgery == "Intact") %>% 
  mutate(t_KOspk = slope * (DP1 - EODonset) + EODonset + intercept,
         Onset10 = Infl1 - log(9) / Slop1)


# figure 5b (day & EOD onset) ---------------------------------------------

anova(lm(EODonset ~ Treatment * Day, data = df_para))

fig5b <- ggplot(df_para, aes(x = Day, y = EODonset, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  scale_y_continuous(limit = c(2.3, 3.3), breaks = seq(2.4, 3.2, 0.2)) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Delay to EOD onset\nfrom EODC onset (ms)")
fig5b


# figure 5c (day & dp1) ---------------------------------------------

anova(lm(DP1 ~ Treatment * Day, data = df_para))

fig5c <- ggplot(df_para, aes(x = Day, y = DP1, colour = Treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  scale_y_continuous(limit = c(2.7, 3.7), breaks = seq(2.6, 3.6, 0.2)) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Days after treatment") +
  ylab("Delay to EOD peak 1\nfrom EODC onset (ms)")
fig5c


# figure 5d (KO time & 10% onset) ---------------------------------------------

lm(Onset10 ~ t_KOspk, data = df_para)
cor.test(df_para$Onset10, df_para$t_KOspk)

fig5d <- ggplot(df_para, aes(x = t_KOspk, y = Onset10)) +
  geom_point(aes(colour = Treatment)) +
  scale_colour_manual(values = c("#FF00FF", "#000000")) +
  theme_classic() +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  xlab("Estimated KO spike timing (ms)") +
  ylab("10% inhibition onset") +
  scale_y_continuous(limit = c(2.4, 3.5), breaks = seq(2.4, 3.4, 0.2)) +
  theme(legend.position = "none")
fig5d
