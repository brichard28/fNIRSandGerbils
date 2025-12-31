# Author: Benjamin Richardson
# Clean consolidated behavioral analysis for scrambled-speech experiments 1 & 2

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(afex)
library(emmeans)
library(patchwork)
library(grid)
library(ggforce)

############################################################
# 1. UNIVERSAL SETTINGS
############################################################

talker_map <- c("Same" = 1, "Different" = 2)

masker_offset_map <- c(
  "Scrambled" = -0.1,
  "Words"     =  0.1
)

# Define colors using RGB (0–1 scale)
words_color     <- rgb(97/255, 156/255, 255/255)   # ggplot default blue
scrambled_color <- rgb(248/255, 118/255, 109/255)  # ggplot default orange

color_map <- c("Words" = words_color, "Scrambled" = scrambled_color)


############################################################
# 2. UNIVERSAL LOADER FOR HIT / FA / MASKER
############################################################

load_behavior_measure <- function(filepath, measure_name) {
  
  df <- read.csv(filepath, header = FALSE)
  
  S_col <- 1:nrow(df)
  df <- cbind(S = S_col, df)
  
  colnames(df) <- c("S",
                    "Scrambled_Different","Scrambled_Same",
                    "Unscrambled_Different","Unscrambled_Same")
  
  df <- pivot_longer(df,
                     cols = -S,
                     names_to = c("Masker","Talker"),
                     names_sep = "_",
                     values_to = measure_name)
  
  df$Masker <- recode(df$Masker, "Unscrambled" = "Words")
  df$Talker <- factor(df$Talker, levels = c("Same","Different"))
  df$S <- factor(df$S)
  df$Masker <- factor(df$Masker, levels = c("Scrambled","Words"))
  
  df <- df %>%
    mutate(
      Talker_num = talker_map[Talker],
      Masker_offset = masker_offset_map[Masker],
      x_pos = Talker_num + Masker_offset
    )
  
  df
}


############################################################
# 3. LOAD ALL MEASURES FOR A SINGLE EXPERIMENT
############################################################

load_experiment <- function(exp_num) {
  
  base <- "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/"
  
  hit  <- load_behavior_measure(
    paste0(base, "Scrambled_Speech_Hit_Rates_exp_", exp_num, ".csv"),
    "HitRate"
  )
  
  fa <- load_behavior_measure(
    paste0(base, "Scrambled_Speech_FA_Rates_exp_", exp_num, ".csv"),
    "FARate"
  )
  
  masker <- load_behavior_measure(
    paste0(base, "Scrambled_Speech_Masker_rates_exp_", exp_num, ".csv"),
    "MaskerRate"
  )
  
  hit <- hit %>%
    rename(Value = HitRate) %>%
    mutate(Measure = "Hit Rate")
  
  fa <- fa %>%
    rename(Value = FARate) %>%
    mutate(Measure = "Within-stream FA Rate")
  
  masker <- masker %>%
    rename(Value = MaskerRate) %>%
    mutate(Measure = "Between-stream FA Rate")
  
  
  masker <- masker %>% filter(Masker == "Words")
  
  bind_rows(hit, fa, masker) %>%
    mutate(Experiment = paste0("Exp. ", exp_num))
}

############################################################
# 4. Load Exp 1 and Exp 2 into one table
############################################################

long_data <- bind_rows(
  load_experiment(1),
  load_experiment(2)
)

long_data$Measure <- factor(long_data$Measure,
                            levels = c("Hit Rate",
                                       "Within-stream FA Rate",
                                       "Between-stream FA Rate"))

############################################################
# 5. Summary statistics per condition
############################################################

summary_data <- long_data %>%
  group_by(Experiment, Measure, Masker, Talker, x_pos) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sem_value  = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


############################################################
# 6. Custom y-limits per measure
############################################################

y_limits <- tibble(
  Measure = levels(long_data$Measure),
  ymin = c(0.4, 0, 0),
  ymax = c(1.0, 0.4, 0.1)
)

long_data <- long_data %>%
  left_join(y_limits, by = "Measure")

summary_data <- summary_data %>%
  left_join(y_limits, by = "Measure")

long_data <- long_data %>%
  mutate(Measure = factor(Measure,
                          levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate")))

summary_data <- summary_data %>%
  mutate(Measure = factor(Measure,
                          levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate")))

levels(long_data$Measure) <- str_wrap(levels(long_data$Measure), width = 15)
levels(summary_data$Measure) <- str_wrap(levels(summary_data$Measure), width = 15)


############################################################
# 7. Compute central tick positions for Talker axis
############################################################

talker_positions <- long_data %>%
  group_by(Talker) %>%
  summarise(mean_x = mean(x_pos), .groups="drop") %>%
  arrange(mean_x)

talker_labels_wrapped <- stringr::str_wrap(talker_positions$Talker, width = 1)

############################################################
# 8. Plot (means now black + dashed; individuals still colored)
############################################################

shape_map <- c("Scrambled" = 15, "Words" = 17)

p <- ggplot(long_data, aes(x = x_pos, y = Value, color = Masker, fill = Masker)) +
  
  # Individual subject lines
  geom_line(aes(group = interaction(S, Masker)), alpha = 0.3) +
  
  # Individual subject points
  geom_point(aes(group = interaction(S, Masker)), alpha = 0.3) +
  
  # # GRAND-MEAN LINE — black + dashed
  # geom_line(
  #   data = summary_data,
  #   aes(x = x_pos, y = mean_value, group = Masker),
  #   color = "black",
  #   size = 1.2,
  #   linetype = "dashed",
  #   inherit.aes = FALSE
  # ) +
  # 
  # GRAND-MEAN POINTS — black outline, white fill
  geom_point(
    data = summary_data,
    aes(x = x_pos, y = mean_value, shape = Masker, color = Masker, fill = Masker),
    size = 5.0,
    stroke = 1.2,
    inherit.aes = FALSE
  ) +
  
  # GRAND-MEAN ERROR BARS — black
  geom_errorbar(
    data = summary_data,
    aes(
      x = x_pos,
      ymin = mean_value - sem_value,
      ymax = mean_value + sem_value, 
      color = Masker
    ),
    width = 0.3,
    size = 1.1,
    inherit.aes = FALSE
  ) +
  
  # Shapes still correspond to Masker
  scale_shape_manual(values = shape_map) +
  scale_color_manual(values = color_map) + 

  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  
  scale_x_continuous(
    breaks = talker_positions$mean_x,
    labels = talker_labels_wrapped
  ) +
  
  labs(x = "Talker", y = "") +
  
  facet_grid(Measure ~ Experiment, scales = "free_y", switch = "y") +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y.left = element_text(margin = margin(l = 10), size = 14, face = "bold"),
    axis.text.y.left = element_text(margin = margin(r = 5), size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = unit(c(5,5,5,40), "pt"),
    strip.placement = "outside"
  )


p

ggsave(
  "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/PAPER FIGURES/behavior_plot_raw.svg",
  p,
  width = 8,
  height = 8,
  device = "svg"
)


##### Hit Rates ####
# Experiment 1
model_hitrate_exp1 <- lmer(Value ~ Masker*Talker + (1|S), data = subset(long_data, Measure == "Hit Rate" & Experiment == "Exp. 1"), control = lmerControl(optimizer = "bobyqa"))
anova(model_hitrate_exp1)
# Significant interaction between Masker and Talker
em_hitrate_exp1 <- emmeans(model_hitrate_exp1, ~ Masker*Talker)
pairs(em_hitrate_exp1, by = "Masker", adjust = "bonferroni")


# Experiment 2
model_hitrate_exp2 <- lmer(Value ~ Masker*Talker + (1|S), data = subset(long_data, Measure == "Hit Rate" & Experiment == "Exp. 2"), control = lmerControl(optimizer = "bobyqa"))
anova(model_hitrate_exp2)
# Significant interaction between Masker and Talker
em_hitrate_exp2 <- emmeans(model_hitrate_exp2, ~Masker*Talker)
pairs(em_hitrate_exp2, by = "Talker", adjust = "bonferroni")

#### Within-stream False alarm rate ####
# Experiment 1
model_wsFArate_exp1 <- lmer(Value ~ Masker*Talker + (1|S), data = subset(long_data, Measure == "Within-stream\nFA Rate" & Experiment == "Exp. 1"), control = lmerControl(optimizer = "bobyqa"))
anova(model_wsFArate_exp1)
# Significant main effect of Masker
em_wsFArate_exp1 <- emmeans(model_wsFArate_exp1, ~ Masker)
pairs(em_wsFArate_exp1, adjust = "bonferroni")

# Experiment 2
model_wsFArate_exp2 <- mixed(Value ~ Masker*Talker + (1|S), data = subset(long_data, Measure == "Within-stream\nFA Rate" & Experiment == "Exp. 2"), control = lmerControl(optimizer = "bobyqa"))
anova(model_wsFArate_exp2)
# Significant interaction between Masker and Talker
em_wsFArate_exp2 <- emmeans(model_wsFArate_exp2, ~ Masker * Talker)
pairs(em_wsFArate_exp2, by = "Masker", adjust = "bonferroni")

#### Between-stream False alarm rate ####
# Experiment 1
model_bsFArate_exp1 <- mixed(Value ~ Talker + (1|S), data = subset(long_data, Measure == "Between-stream\nFA Rate" & Experiment == "Exp. 1"), control = lmerControl(optimizer = "bobyqa"))
anova(model_bsFArate_exp1)
# Significant effect of Talker
em_bsFArate_exp1 <- emmeans(model_bsFArate_exp1, ~ Talker)
pairs(em_bsFArate_exp1, adjust = "bonferroni")

model_bsFArate_exp2 <- mixed(Value ~ Talker + (1|S), data = subset(long_data, Measure == "Between-stream\nFA Rate" & Experiment == "Exp. 2"), control = lmerControl(optimizer = "bobyqa"))
anova(model_bsFArate_exp2)
# Significant effect of Talker
em_bsFArate_exp2 <- emmeans(model_bsFArate_exp2, ~ Talker)
pairs(em_bsFArate_exp2, adjust = "bonferroni")
