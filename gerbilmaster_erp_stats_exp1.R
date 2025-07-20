# Author: Benjamin Richardson
# plot and conduct statistics for scrambled speech p1,n1,p2, and p3 components experiment 1

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
library(rhdf5)
# Read the .mat file
library(R.matlab)
p1_data <- read.csv('C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\all_subs_p1_exp1.csv')
n1_data <- read.csv('C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\all_subs_n1_exp1.csv')
p2_data <- read.csv('C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\all_subs_p2_exp1.csv')
p3_data <- read.csv('C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\all_subs_p3_exp1.csv')

names(p1_data)[names(p1_data) == "Amplitude"] <- "p1"
names(n1_data)[names(n1_data) == "Amplitude"] <- "n1"
names(p2_data)[names(p2_data) == "Amplitude"] <- "p2"
names(p3_data)[names(p3_data) == "Amplitude"] <- "p3"

# Merge all data frames on the common identifier columns
all_data <- Reduce(function(x, y) merge(x, y, by = c("S", "Masker", "Talker", "WordType")), list(p1_data, n1_data, p2_data, p3_data))

ggplot(all_data,
       aes(Talker, n1 - p1)) +
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
  labs(title = "N1 - P1 by Condition Experiment 1", 
       x = "", y = "Amplitude (mV)") +
  ylim(c(-9,2))




ggplot(all_data,
       aes(Talker, p3)) +
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
  labs(title = "P300 by Condition Experiment 1", 
       x = "", y = "Amplitude (mV)") +
  ylim(c(-2.5,6))