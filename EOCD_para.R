# package
library(tidyverse)

# set your directory
# dir_home <- "/Volumes/carlsonlab/MSF/Manuscripts/Testosterone2023/Data"

# load dataset
df <- read.csv(paste0(dir_home, "/EOCD_inhibitioncurve.csv"))

# sigmoid fitting
IDs <- unique(df$ID)
n_ID <- length(IDs)

df_para <- data.frame(
  ID = IDs,
  Treatment = NA,
  Surgery = NA,
  Day = NA,
  DP1 = NA,
  EODonset = NA,
  Infl1 = NA,
  Infl2 = NA,
  Slop1 = NA,
  Slop2 = NA
)

df$sigm1 <- 0
df$sigm2 <- 0

for (ii in 1:n_ID) {
  dat_ii <- filter(df, ID == IDs[ii])
  min_idx <- which.min(dat_ii$Amps)
  
  jj <- 1
  while (dat_ii$Amps[min_idx - jj] < 0.5 | dat_ii$Amps[min_idx - jj] - dat_ii$Amps[min_idx - jj + 1] > 0) {
    jj <- jj + 1
  }
  
  kk <- 1
  while (dat_ii$Amps[min_idx + kk] < 0.5 | (dat_ii$Amps[min_idx + kk] - dat_ii$Amps[min_idx + kk - 1] > 0 & dat_ii$Amps[min_idx + kk + 1] - dat_ii$Amps[min_idx + kk] > 0)) {
    kk <- kk + 1
  }
  
  sigm1_idx <- (min_idx - jj + 1):min_idx
  sigm2_idx <- min_idx:(min_idx + kk)
  
  pre_amp <- dat_ii$Amps[sigm1_idx]
  pos_amp <- dat_ii$Amps[sigm2_idx]
  pre_lat <- dat_ii$Latency[sigm1_idx]
  pos_lat <- dat_ii$Latency[sigm2_idx]
  
  df$sigm1[df$ID == IDs[ii]][sigm1_idx] <- 1
  df$sigm2[df$ID == IDs[ii]][sigm2_idx] <- 1
  
  # common sigmoid curve fitting
  fit3 <- nls(pre_amp ~ (1 / (1 + exp(Slop1 * (Infl1 - pre_lat)))), start = c(Slop1 = -1, Infl1 = mean(pre_lat)))
  coef3 <- coefficients(fit3)
  fit4 <- nls(pos_amp ~ (1 / (1 + exp(Slop2 * (Infl2 - pos_lat)))), start = c(Slop2 = 1, Infl2 = mean(pos_lat)))
  coef4 <- coefficients(fit4)
  
  df_para$Slop1[ii] <- coef3[1]
  df_para$Infl1[ii] <- coef3[2]
  df_para$Slop2[ii] <- coef4[1]
  df_para$Infl2[ii] <- coef4[2]
  df_para$Treatment[ii] <- dat_ii$Treatment[1]
  df_para$Surgery[ii] <- dat_ii$Surgery[1]
  df_para$Day[ii] <- dat_ii$Day[1]
  df_para$DP1[ii] <- dat_ii$DP1[1]
  df_para$EODonset[ii] <- dat_ii$EODonset[1]
}
