# Author: Benjamin Richardson
# Combined analysis for Experiments 1 and 2: p1−n1 and P3 plots
# Two figures: Figure1 = p1−n1, Figure2 = P3, 2x2 layout with rows=Experiment, cols=WordType

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(afex)
library(dplyr)
library(R.matlab)

############################################################
### 1. Load and combine both experiments
############################################################

load_exp <- function(exp_num){
  
  p1 <- read.csv(paste0('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p1_target_exp', exp_num, '.csv'))
  n1 <- read.csv(paste0('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_n1_target_exp', exp_num, '.csv'))
  p2 <- read.csv(paste0('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p2_target_exp', exp_num, '.csv'))
  p3 <- read.csv(paste0('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/all_subs_p3_target_exp', exp_num, '.csv'))
  
  names(p1)[names(p1)=="Amplitude"] <- "p1"
  names(n1)[names(n1)=="Amplitude"] <- "n1"
  names(p2)[names(p2)=="Amplitude"] <- "p2"
  names(p3)[names(p3)=="Amplitude"] <- "p3"
  
  df <- Reduce(
    function(x,y) merge(x,y, by=c("S","Masker","Talker","WordType","Electrode")),
    list(p1,n1,p2,p3)
  )
  
  df$Experiment <- factor(paste0("Exp", exp_num))
  df
}

exp1 <- load_exp(1)
exp2 <- load_exp(2)
all_data <- rbind(exp1, exp2)

# Factor organization
to.factor <- c("S","Masker","Talker","WordType","Electrode","Experiment")
all_data[to.factor] <- lapply(all_data[to.factor], as.factor)

############################################################
### 2. Compute summary values: p1−n1 and P3
############################################################

frontocentral_electrodes <- c("Fz","FC1","FC2","C3","Cz","C4","CP1","CP2")
parietooccipital_electrodes <- c("P3","Pz","PO3","O1","Oz","O2","PO4","P4")

# p1 − n1
fc_summary <- all_data %>%
  filter(Electrode %in% frontocentral_electrodes) %>%
  mutate(diff = p1 - n1) %>%
  group_by(Experiment, S, Masker, Talker, WordType) %>%
  summarise(mean_diff = mean(diff, na.rm=TRUE), .groups="drop")

# P3
p3_summary <- all_data %>%
  filter(Electrode %in% parietooccipital_electrodes) %>%
  group_by(Experiment, S, Masker, Talker, WordType) %>%
  summarise(mean_p3 = mean(p3, na.rm=TRUE), .groups="drop")

############################################################
### 3. Add aesthetics: color and shape mappings
############################################################

fc_summary$Masker <- factor(fc_summary$Masker, levels=c("Scrambled","Unscrambled"))
p3_summary$Masker <- factor(p3_summary$Masker, levels=c("Scrambled","Unscrambled"))

fc_summary$WordType <- factor(fc_summary$WordType, levels=c("Color","Object"))
p3_summary$WordType <- factor(p3_summary$WordType, levels=c("Color","Object"))

shape_map <- c("Scrambled"= 15, "Unscrambled"= 17)

# Define colors using RGB (0–1 scale)
words_color     <- rgb(97/255, 156/255, 255/255)   # ggplot default blue
scrambled_color <- rgb(248/255, 118/255, 109/255)  # ggplot default orange

color_map <- c("Unscrambled" = words_color, "Scrambled" = scrambled_color)

############################################################
### 4. Ensure Talker order
############################################################

fc_summary$Talker <- factor(fc_summary$Talker, levels=c("Same","Different"))
p3_summary$Talker <- factor(p3_summary$Talker, levels=c("Same","Different"))

############################################################
### 5. Plot function: x-axis = Talker, offset for Masker, 2x2 facets
# Rows = Experiment, Columns = WordType
############################################################

offset_map <- c("Scrambled" = -0.25, "Unscrambled" = 0.25)
talker_map <- c("Same" = 1, "Different" = 2)

make_plot_2x2 <- function(df, yvar, row_label) {
  
  # 1. Compute x positions
  offset_map <- c("Scrambled" = -0.25, "Words" = 0.25)
  talker_map <- c("Same" = 1, "Different" = 2)
  
  df <- df %>%
    mutate(
      RowLabel = factor(row_label),
      TalkerNum = talker_map[Talker],
      x_pos = TalkerNum + offset_map[Masker]
    )
  
  # 2. Compute summary stats per condition
  sum_df <- df %>%
    group_by(RowLabel, Experiment, WordType, Talker, Masker, x_pos) %>%
    summarise(
      mean_val = mean(.data[[yvar]], na.rm = TRUE),
      se_val   = sd(.data[[yvar]], na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  
  # 3. Compute line color for each subject based on which condition is higher
  df <- df %>%
    group_by(Experiment, S, Talker, WordType, RowLabel) %>%
    mutate(masker_diff = .data[[yvar]][Masker == "Scrambled"] -
             .data[[yvar]][Masker == "Unscrambled"]) %>%
    mutate(line_color = ifelse(masker_diff > 0, rgb(248/255, 118/255, 109/255) , rgb(97/255, 156/255, 255/255) ))

  # 4. Plot
  ggplot() +
    geom_hline(yintercept=0) +
    
    # Subject lines colored by which condition is higher
    geom_line(data = df,
              aes(x = x_pos, y = .data[[yvar]], color = I(line_color),
                  group = interaction(S, Talker, WordType))) +
    
    # Subject points
    geom_point(data = df,
               aes(x = x_pos, y = .data[[yvar]],
                    shape = Masker), color = "gray",
               size = 2.3, stroke = 0.7) +
    
    # Summary error bars (black)
    geom_errorbar(data = sum_df,
                  aes(x = x_pos,
                      ymin = mean_val - se_val,
                      ymax = mean_val + se_val),
                  color="black",
                  width = 0.2, linewidth = 0.7) +
    
    # Summary line (black dashed connecting same/different)
    geom_line(data = sum_df,
              aes(x = x_pos, y = mean_val, group = Talker),
              color = "black", linetype = "dashed", size = 1.2) +
    
    
    # Summary points (black outline)
    geom_point(data = sum_df,
               aes(x = x_pos, y = mean_val, color = Masker, shape = Masker),
               fill = "white", size = 4, stroke = 1.2) +
    
    
    
    # Shape mapping for masker points
    scale_shape_manual(values = shape_map) +
    scale_color_manual(values = color_map) + 

    # Facets
    facet_wrap(~ Experiment + WordType, nrow = 1) + 
  
    
    scale_x_continuous(breaks = c(1, 2),
                       labels = levels(df$Talker),
                       expand = expansion(mult = c(0.08, 0.08))) +
    
    labs(x = "Talker", y = "Amplitude (mV)") +
    
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.text.x = element_text(size=11),
          axis.title.y = element_text(size=12),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background  = element_rect(fill = "transparent", color = NA),
          legend.position = "none")   # removes all legends
}


############################################################
### 6. Generate figures
############################################################

# Figure 1: p1−n1
figure1 <- make_plot_2x2(fc_summary, "mean_diff", "p1 − n1")

# Figure 2: P3
figure2 <- make_plot_2x2(p3_summary, "mean_p3", "P3")

# Display figures
figure1
figure2

ggsave("/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/p1n1_results.svg", figure1,device="svg", width = 15, height = 6, units = "in", bg = "transparent")
ggsave("/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/p3_results.svg", figure2, device="svg", width = 15, height = 6, units = "in", bg = "transparent")
