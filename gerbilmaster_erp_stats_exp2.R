# Author: Benjamin Richardson
# plot and conduct statistics for scrambled speech p1,n1,p2, and p3 components experiment 1

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
#library(rhdf5)
# Read the .mat file
library(R.matlab)
p1_data <- read.csv('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p1_target_exp2.csv')
n1_data <- read.csv('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_n1_target_exp2.csv')
p2_data <- read.csv('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p2_target_exp2.csv')
p3_data <- read.csv('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p3_target_exp2.csv')

names(p1_data)[names(p1_data) == "Amplitude"] <- "p1"
names(n1_data)[names(n1_data) == "Amplitude"] <- "n1"
names(p2_data)[names(p2_data) == "Amplitude"] <- "p2"
names(p3_data)[names(p3_data) == "Amplitude"] <- "p3"

# Merge all data frames on the common identifier columns
all_data <- Reduce(function(x, y) merge(x, y, by = c("S", "Masker", "Talker", "WordType","Electrode")), list(p1_data, n1_data, p2_data, p3_data))

# Organize Factors
to.factor <- c("S", "Masker", "Talker", "WordType","Electrode")
all_data[, to.factor] <- lapply(all_data[, to.factor], as.factor)

#frontocentral_electrodes <- c("Fp1","AF3","F7","F3","FC1","FC5","FC6","FC2","F4","F8","AF4","Fp2","Fz","Cz")
frontocentral_electrodes <- c("Fz", "FC1", "FC2", "C3", "Cz", "C4", "CP1", "CP2")
parietooccipital_electrodes <- c("P3", "Pz", "PO3", "O1", "Oz", "O2", "PO4", "P4")

# Define ERP variables to pivot
erp_vars <- c("p1", "n1", "p2", "p3")

# For frontocentral electrodes: calculate mean(p1 - n1) per subject and group
frontocentral_summary <- all_data %>%
  filter(Electrode %in% frontocentral_electrodes) %>%
  mutate(diff_n1_p1 = p1 - n1) %>%
  group_by(S, Masker, Talker, WordType) %>%
  summarise(mean_diff = mean(diff_n1_p1, na.rm = TRUE)) %>%
  ungroup()

# For parietooccipital electrodes: calculate mean p3 per subject and group
parietooccipital_summary <- all_data %>%
  filter(Electrode %in% parietooccipital_electrodes) %>%
  group_by(S, Masker, Talker, WordType) %>%
  summarise(mean_p3 = mean(p3, na.rm = TRUE)) %>%
  ungroup()

ggplot(frontocentral_summary,
       aes(Talker, mean_diff)) +
  facet_grid(WordType~Masker) +  
  geom_hline(yintercept = 0) +
  #geom_dotplot(aes(color = S, fill = S), binaxis = 'y', stackdir = 'center', alpha = 0.5) + 
  geom_point(aes(color = S, fill = S, group = S),alpha = 0.5, show.legend = FALSE) + 
  geom_line(aes(color = S, group = S), alpha = 0.5, show.legend = FALSE) + 
  stat_summary(fun.y=mean, geom="point", size=3, group = 1) +
  stat_summary(fun.y=mean, geom="line", size=0.5, group = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  #scale_fill_manual(values = c("Same Talker" = "#F8766D", "Different Talker" = "#00BFC4")) +
  #scale_color_manual(values = c("Same Talker" = "#F8766D", "Different Talker" = "#00BFC4")) +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.y = element_text(angle = 270),
        axis.text.x = element_text(angle = 30, size = 15, hjust = 1),
        legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.line.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_blank()) +
  labs(title = "p1 - n1 by Condition Experiment 2", 
       x = "", y = "Amplitude (mV)") +
  ylim(c(-10,10))



ggplot(parietooccipital_summary,
       aes(Talker, mean_p3)) +
  facet_grid(WordType~Masker) +  
  geom_hline(yintercept = 0) +
  #geom_dotplot(aes(color = S, fill = S), binaxis = 'y', stackdir = 'center', alpha = 0.5) + 
  geom_point(aes(color = S, fill = S, group = S),alpha = 0.5, show.legend = FALSE) + 
  geom_line(aes(color = S, group = S), alpha = 0.5, show.legend = FALSE) + 
  stat_summary(fun.y=mean, geom="point", size=3, group = 1) +
  stat_summary(fun.y=mean, geom="line", size=0.5, group = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  #scale_fill_manual(values = c("Same Talker" = "#F8766D", "Different Talker" = "#00BFC4")) +
  #scale_color_manual(values = c("Same Talker" = "#F8766D", "Different Talker" = "#00BFC4")) +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.y = element_text(angle = 270),
        axis.text.x = element_text(angle = 30, size = 15, hjust = 1),
        legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.line.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_blank()) +
  labs(title = "P3 by Condition Experiment 2", 
       x = "", y = "Amplitude (mV)") +
  ylim(c(-10,10))


model_p1n1_exp2 <- lmer(mean_diff ~ Masker*Talker*WordType + (1|S),data= frontocentral_summary,control = lmerControl(optimizer = "bobyqa"))

anova(model_p1n1_exp2)

# Significant main effect of word type
em_p1n1_wordtype <- emmeans(model_p1n1_exp2, ~ WordType)
pairs(em_p1n1_wordtype, adjust = "bonferroni")

# Significant interaction between masker and talker
em_p1n1_masker_talker <- emmeans(model_p1n1_exp2, ~ Masker * Talker)
pairs(em_p1n1_masker_talker, by = "Talker", adjust = "bonferroni")



model_p3_exp2 <- lmer(mean_p3 ~ Masker*Talker*WordType + (1|S),data= parietooccipital_summary,control = lmerControl(optimizer = "bobyqa"))

anova(model_p3_exp2)

# Significant main effect of word type
em_p3_wordtype <- emmeans(model_p3_exp2, ~ WordType)
pairs(em_p3_wordtype, adjust = "bonferroni")
