# Author: Benjamin Richardson
# plot and conduct statistics for scrambled speech behavior experiment 1


library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
####################################################
##    Experiment 1    ##
####################################################

    ####################################################
    ##    Hit Rates    ##
    ####################################################

hit_rates <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Hit_Rates_exp_1.csv", header = FALSE)
S_col <- array(1:as.numeric(count(hit_rates)))

# Add the new column at the first position
hit_rates <- cbind(col1 = S_col, hit_rates)

colnames(hit_rates) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
hit_rates <- pivot_longer(hit_rates, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                               names_to = c("Masker","Talker"), names_sep = "_", values_to = "HitRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
hit_rates[, to.factor] <- lapply(hit_rates[, to.factor], as.factor)

summary_data_hit_rate <- hit_rates %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_hitrate = mean(HitRate),
    sem_hitrate = sd(HitRate) / sqrt(n()),
    .groups = "drop"
  )

# Create numeric x-position for Talker
hit_rates <- hit_rates %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_hit_rate <- summary_data_hit_rate %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

ggplot(hit_rates, aes(x = x_pos, y = HitRate, color = Masker, fill = Masker)) +
  # Subject-wise lines
  geom_line(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Subject-wise points
  geom_point(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Group mean points
  geom_point(data = summary_data_hit_rate, 
             aes(x = x_pos, y = mean_hitrate, color = Masker, fill = Masker),
             size = 3, shape = 21, stroke = 1.5, inherit.aes = FALSE) +
  # SEM error bars
  geom_errorbar(data = summary_data_hit_rate,
                aes(x = x_pos, ymin = mean_hitrate - sem_hitrate, ymax = mean_hitrate + sem_hitrate, color = Masker),
                size = 1.5,width = 0.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = unique(hit_rates$Talker_num),
                     labels = unique(hit_rates$Talker)) +
  labs(title = "Hit Rate by Masker and Talker Experiment 1", y = "Hit Rate", x = "Talker") +
  theme_minimal() +
  ylim(c(0,1))
  theme(legend.position = "right")
  

    ####################################################
    ##    FA Rates    ##
    ####################################################

FA_rates <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_FA_Rates_exp_1.csv")
S_col <- array(1:as.numeric(count(FA_rates)))

# Add the new column at the first position
FA_rates <- cbind(col1 = S_col, FA_rates)

colnames(FA_rates) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
FA_rates <- pivot_longer(FA_rates, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                          names_to = c("Masker","Talker"), names_sep = "_", values_to = "FARate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
FA_rates[, to.factor] <- lapply(FA_rates[, to.factor], as.factor)

summary_data_FA_rate <- FA_rates %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_FArate = mean(FARate),
    sem_FArate = sd(FARate) / sqrt(n()),
    .groups = "drop"
  )

FA_rates <- FA_rates %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_FA_rate <- summary_data_FA_rate %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

ggplot(FA_rates, aes(x = x_pos, y = FARate, color = Masker, fill = Masker)) +
  # Subject-wise lines
  geom_line(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Subject-wise points
  geom_point(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Group mean points
  geom_point(data = summary_data_FA_rate, 
             aes(x = x_pos, y = mean_FArate, color = Masker, fill = Masker),
             size = 3, shape = 21, stroke = 1.5, inherit.aes = FALSE) +
  # SEM error bars
  geom_errorbar(data = summary_data_FA_rate,
                aes(x = x_pos, ymin = mean_FArate - sem_FArate, ymax = mean_FArate + sem_FArate, color = Masker),
                size = 1.5,width = 0.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = unique(FA_rates$Talker_num),
                     labels = unique(FA_rates$Talker)) +
  labs(title = "FA Rate by Masker and Talker Experiment 1", y = "FA Rate", x = "Talker") +
  theme_minimal() +
  ylim(c(0,1)) +
  theme(legend.position = "right")


    ####################################################
    ##    D primes    ##
    ####################################################

d_primes <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_D_primes_exp_1.csv")
S_col <- array(1:as.numeric(count(d_primes)))

# Add the new column at the first position
d_primes <- cbind(col1 = S_col, d_primes)

colnames(d_primes) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
d_primes <- pivot_longer(d_primes, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                         names_to = c("Masker","Talker"), names_sep = "_", values_to = "DPrime")
# Organize Factors
to.factor <- c('S','Masker','Talker')
d_primes[, to.factor] <- lapply(d_primes[, to.factor], as.factor)

summary_data_DPrime <- d_primes %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_DPrime = mean(DPrime),
    sem_DPrime = sd(DPrime) / sqrt(n()),
    .groups = "drop"
  )

d_primes <- d_primes %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_DPrime <- summary_data_DPrime %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

ggplot(d_primes, aes(x = x_pos, y = DPrime, color = Masker, fill = Masker)) +
  # Subject-wise lines
  geom_line(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Subject-wise points
  geom_point(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Group mean points
  geom_point(data = summary_data_DPrime, 
             aes(x = x_pos, y = mean_DPrime, color = Masker, fill = Masker),
             size = 3, shape = 21, stroke = 1.5, inherit.aes = FALSE) +
  # SEM error bars
  geom_errorbar(data = summary_data_DPrime,
                aes(x = x_pos, ymin = mean_DPrime - sem_DPrime, ymax = mean_DPrime + sem_DPrime, color = Masker),
                size = 1.5,width = 0.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = unique(d_primes$Talker_num),
                     labels = unique(d_primes$Talker)) +
  labs(title = "d' by Masker and Talker Experiment 1", y = "d'", x = "Talker") +
  theme_minimal() +
  ylim(c(0,5.5)) +
  theme(legend.position = "right")


##### D prime statistics ######



model_dprime_exp1 <- mixed(DPrime ~ Masker*Talker + (1|S),data= d_primes,control = lmerControl(optimizer = "bobyqa"))

model_dprime_exp1


# Post hocs
# Effect of talker when masker is scrambled
d_primes$Talker <- relevel(d_primes$Talker, "Different")
posthoc_talker_scrambled_exp1 <- lmer(DPrime ~ Talker + (1|S),
                             data= subset(d_primes, Masker == "Scrambled"), 
                             control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_talker_scrambled_exp1)

posthoc_talker_unscrambled_exp1 <- lmer(DPrime ~ Talker + (1|S),
                                 data= subset(d_primes, Masker == "Unscrambled"), 
                                 control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_talker_unscrambled_exp1)



####################################################
##    Masker Rates    ##
####################################################

masker_rates <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Masker_rates_exp_1.csv")
S_col <- array(1:as.numeric(count(masker_rates)))

# Add the new column at the first position
masker_rates <- cbind(col1 = S_col, masker_rates)

colnames(masker_rates) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
masker_rates <- pivot_longer(masker_rates, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                         names_to = c("Masker","Talker"), names_sep = "_", values_to = "MaskerRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
masker_rates[, to.factor] <- lapply(masker_rates[, to.factor], as.factor)

summary_data_MaskerRate <- masker_rates %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_MaskerRate = mean(MaskerRate),
    sem_MaskerRate = sd(MaskerRate) / sqrt(n()),
    .groups = "drop"
  )

masker_rates <- masker_rates %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_MaskerRate <- summary_data_MaskerRate %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

ggplot(masker_rates, aes(x = x_pos, y = MaskerRate, color = Masker, fill = Masker)) +
  # Subject-wise lines
  geom_line(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Subject-wise points
  geom_point(aes(group = interaction(S,Masker)), alpha = 0.3) +
  # Group mean points
  geom_point(data = summary_data_MaskerRate, 
             aes(x = x_pos, y = mean_MaskerRate, color = Masker, fill = Masker),
             size = 3, shape = 21, stroke = 1.5, inherit.aes = FALSE) +
  # SEM error bars
  geom_errorbar(data = summary_data_MaskerRate,
                aes(x = x_pos, ymin = mean_MaskerRate - sem_MaskerRate, ymax = mean_MaskerRate + sem_MaskerRate, color = Masker),
                size = 1.5,width = 0.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = unique(masker_rates$Talker_num),
                     labels = unique(masker_rates$Talker)) +
  labs(title = "Masker Color Rate by Masker and Talker Experiment 1", y = "Masker Rate", x = "Talker") +
  theme_minimal() +
  ylim(c(0,0.1)) +
  theme(legend.position = "right")


##### Masker Rate statistics ######

model_masker_rate_exp1 <- mixed(MaskerRate ~ Talker + (1|S),data= subset(masker_rates,Masker == "Unscrambled"),control = lmerControl(optimizer = "bobyqa"))

model_masker_rate_exp1

posthoc_talker_exp1 <- lmer(MaskerRate ~ Talker + (1|S),
                            data= subset(masker_rates, Masker == "Unscrambled"), 
                            control = lmerControl(optimizer = "bobyqa"))

summary(posthoc_talker_exp1)