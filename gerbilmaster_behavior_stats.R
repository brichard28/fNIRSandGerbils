# Author: Benjamin Richardson
# plot and conduct statistics for scrambled speech behavior experiment 1


library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(dplyr)
library(grid)
library(patchwork)
library(ggforce)
library(emmeans)
####################################################
##    Experiment 1    ##
####################################################

    ####################################################
    ##    Hit Rates    ##
    ####################################################

hit_rates_exp1 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Hit_Rates_exp_1.csv", header = FALSE)
S_col <- array(1:as.numeric(count(hit_rates_exp1)))

# Add the new column at the first position
hit_rates_exp1 <- cbind(col1 = S_col, hit_rates_exp1)

colnames(hit_rates_exp1) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
hit_rates_exp1 <- pivot_longer(hit_rates_exp1, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                               names_to = c("Masker","Talker"), names_sep = "_", values_to = "HitRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
hit_rates_exp1[, to.factor] <- lapply(hit_rates_exp1[, to.factor], as.factor)

hit_rates_exp1 <- hit_rates_exp1 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_hit_rate_exp1 <- hit_rates_exp1 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_hitrate = mean(HitRate),
    sem_hitrate = sd(HitRate) / sqrt(n()),
    .groups = "drop"
  )

# Create numeric x-position for Talker
hit_rates_exp1 <- hit_rates_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_hit_rate_exp1 <- summary_data_hit_rate_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

    ####################################################
    ##    FA Rates    ##
    ####################################################

FA_rates_exp1 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_FA_Rates_exp_1.csv")
S_col <- array(1:as.numeric(count(FA_rates_exp1)))

# Add the new column at the first position
FA_rates_exp1 <- cbind(col1 = S_col, FA_rates_exp1)

colnames(FA_rates_exp1) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
FA_rates_exp1 <- pivot_longer(FA_rates_exp1, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                          names_to = c("Masker","Talker"), names_sep = "_", values_to = "FARate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
FA_rates_exp1[, to.factor] <- lapply(FA_rates_exp1[, to.factor], as.factor)


FA_rates_exp1 <- FA_rates_exp1 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_FA_rate_exp1 <- FA_rates_exp1 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_FArate = mean(FARate),
    sem_FArate = sd(FARate) / sqrt(n()),
    .groups = "drop"
  )

FA_rates_exp1 <- FA_rates_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_FA_rate_exp1 <- summary_data_FA_rate_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )


####################################################
##    Masker Rates    ##
####################################################

masker_rates_exp1 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Masker_rates_exp_1.csv")
S_col <- array(1:as.numeric(count(masker_rates_exp1)))

# Add the new column at the first position
masker_rates_exp1 <- cbind(col1 = S_col, masker_rates_exp1)

colnames(masker_rates_exp1) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
masker_rates_exp1 <- pivot_longer(masker_rates_exp1, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                         names_to = c("Masker","Talker"), names_sep = "_", values_to = "MaskerRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
masker_rates_exp1[, to.factor] <- lapply(masker_rates_exp1[, to.factor], as.factor)

masker_rates_exp1 <- masker_rates_exp1 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_MaskerRate_exp1 <- masker_rates_exp1 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_MaskerRate = mean(MaskerRate),
    sem_MaskerRate = sd(MaskerRate) / sqrt(n()),
    .groups = "drop"
  )

masker_rates_exp1 <- masker_rates_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_MaskerRate_exp1 <- summary_data_MaskerRate_exp1 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )










####################################################
##    Experiment 2    ##
####################################################

####################################################
##    Hit Rates    ##
####################################################

hit_rates_exp2 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Hit_Rates_exp_2.csv", header = FALSE)
S_col <- array(1:as.numeric(count(hit_rates_exp2)))

# Add the new column at the first position
hit_rates_exp2 <- cbind(col1 = S_col, hit_rates_exp2)

colnames(hit_rates_exp2) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
hit_rates_exp2 <- pivot_longer(hit_rates_exp2, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                          names_to = c("Masker","Talker"), names_sep = "_", values_to = "HitRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
hit_rates_exp2[, to.factor] <- lapply(hit_rates_exp2[, to.factor], as.factor)

hit_rates_exp2 <- hit_rates_exp2 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_hit_rate_exp2 <- hit_rates_exp2 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_hitrate = mean(HitRate),
    sem_hitrate = sd(HitRate) / sqrt(n()),
    .groups = "drop"
  )

# Create numeric x-position for Talker
hit_rates_exp2 <- hit_rates_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_hit_rate_exp2 <- summary_data_hit_rate_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

####################################################
##    FA Rates    ##
####################################################

FA_rates_exp2 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_FA_Rates_exp_2.csv")
S_col <- array(1:as.numeric(count(FA_rates_exp2)))

# Add the new column at the first position
FA_rates_exp2 <- cbind(col1 = S_col, FA_rates_exp2)

colnames(FA_rates_exp2) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
FA_rates_exp2 <- pivot_longer(FA_rates_exp2, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                         names_to = c("Masker","Talker"), names_sep = "_", values_to = "FARate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
FA_rates_exp2[, to.factor] <- lapply(FA_rates_exp2[, to.factor], as.factor)

FA_rates_exp2 <- FA_rates_exp2 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_FA_rate_exp2 <- FA_rates_exp2 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_FArate = mean(FARate),
    sem_FArate = sd(FARate) / sqrt(n()),
    .groups = "drop"
  )

FA_rates_exp2 <- FA_rates_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_FA_rate_exp2 <- summary_data_FA_rate_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )


####################################################
##    Masker Rates    ##
####################################################

masker_rates_exp2 <- read.csv("C:\\Users\\benri\\Documents\\GitHub\\fNIRSandGerbils\\data\\Scrambled_Speech_Masker_rates_exp_2.csv")
S_col <- array(1:as.numeric(count(masker_rates_exp2)))

# Add the new column at the first position
masker_rates_exp2 <- cbind(col1 = S_col, masker_rates_exp2)

colnames(masker_rates_exp2) <- c("S","Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same")
masker_rates_exp2 <- pivot_longer(masker_rates_exp2, cols=c("Scrambled_Different","Scrambled_Same","Unscrambled_Different","Unscrambled_Same"),
                             names_to = c("Masker","Talker"), names_sep = "_", values_to = "MaskerRate")
# Organize Factors
to.factor <- c('S','Masker','Talker')
masker_rates_exp2[, to.factor] <- lapply(masker_rates_exp2[, to.factor], as.factor)

masker_rates_exp2 <- masker_rates_exp2 %>%
  mutate(Masker = recode(Masker,
                        "Unscrambled" = "Words"))

summary_data_MaskerRate_exp2 <- masker_rates_exp2 %>%
  group_by(Masker, Talker) %>%
  summarise(
    mean_MaskerRate = mean(MaskerRate),
    sem_MaskerRate = sd(MaskerRate) / sqrt(n()),
    .groups = "drop"
  )

masker_rates_exp2 <- masker_rates_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )

summary_data_MaskerRate_exp2 <- summary_data_MaskerRate_exp2 %>%
  mutate(
    Talker_num = as.numeric(factor(Talker)),
    Masker_offset = case_when(
      Masker == unique(Masker)[1] ~ -0.2,
      Masker == unique(Masker)[2] ~ 0.2
    ),
    x_pos = Talker_num + Masker_offset
  )






####################################################
##    All Statistics    ##
####################################################

##### Exp 1 Hit Rate statistics ######
model_hit_rate_exp1 <- mixed(HitRate ~ Masker*Talker + (1|S),data= hit_rates_exp1,control = lmerControl(optimizer = "bobyqa"))

model_hit_rate_exp1

# Significant interaction of masker and talker, so lets do follow up tests
hit_rates_exp1$Talker <- relevel(hit_rates_exp1$Talker, "Different")
this_model <- lmer(HitRate ~ Talker + (1|S),
                            data= subset(hit_rates_exp1, Masker == "Scrambled"), 
                            control = lmerControl(optimizer = "bobyqa"))
posthoc_hit_rate_talker_scrambled_exp1 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_hit_rate_talker_scrambled_exp1$contrasts
# Significant effect of talker when the masker is scrambled for experiment 1

this_model <- lmer(HitRate ~ Talker + (1|S),
                   data= subset(hit_rates_exp1, Masker == "Words"), 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_hit_rate_talker_words_exp1 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_hit_rate_talker_words_exp1$contrasts
# Sginficant effect of talker when the masker is unscrambled for experiment 1

##### Exp 2 Hit Rate statistics ######
model_hit_rate_exp2 <- mixed(HitRate ~ Masker*Talker + (1|S),data= hit_rates_exp2,control = lmerControl(optimizer = "bobyqa"))

model_hit_rate_exp2
# Significant interaction of masker and talker, so lets do follow up tests
hit_rates_exp2$Talker <- relevel(hit_rates_exp2$Talker, "Different")
this_model <- lmer(HitRate ~ Talker + (1|S),
                   data= subset(hit_rates_exp2, Masker == "Scrambled"), 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_hit_rate_talker_scrambled_exp2 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_hit_rate_talker_scrambled_exp2$contrasts
# There is not a significant effect of talker when the masker is scrambled for experiment 2

this_model <- lmer(HitRate ~ Talker + (1|S),
                   data= subset(hit_rates_exp2, Masker == "Words"), 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_hit_rate_talker_words_exp2 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_hit_rate_talker_words_exp2$contrasts
# Significant effect of talker when the masker is words for experiment 2



##### Exp 1 Within-stream FA Rate statistics ######
model_FA_rate_exp1 <- mixed(FARate ~ Masker*Talker + (1|S),data= FA_rates_exp1,control = lmerControl(optimizer = "bobyqa"))

model_FA_rate_exp1

# Significant effect of Masker only, follow up with comparison
this_model <- lmer(FARate ~ Masker + (1|S),
                   data= FA_rates_exp1, 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_FA_rate_masker_exp1 <- emmeans(this_model, pairwise ~ Masker, adjust = "bonferroni")
posthoc_FA_rate_masker_exp1$contrasts


##### Exp 2 Within-stream FA Rate statistics ######
model_FA_rate_exp2 <- mixed(FARate ~ Masker*Talker + (1|S),data= FA_rates_exp2,control = lmerControl(optimizer = "bobyqa"))

model_FA_rate_exp2

# Significant effect of Masker only, follow up with comparison
this_model <- lmer(FARate ~ Masker + (1|S),
                   data= FA_rates_exp2, 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_FA_rate_masker_exp2 <- emmeans(this_model, pairwise ~ Masker, adjust = "bonferroni")
posthoc_FA_rate_masker_exp2$contrasts



##### Exp 1 Masker Rate statistics ######
model_masker_rate_exp1 <- mixed(MaskerRate ~ Talker + (1|S),data= subset(masker_rates_exp1,Masker == "Words"),control = lmerControl(optimizer = "bobyqa"))

model_masker_rate_exp1

# Significant effect of Talker, follow up with comparison
this_model <- lmer(MaskerRate ~ Talker + (1|S),
                            data= subset(masker_rates_exp1, Masker == "Words"), 
                            control = lmerControl(optimizer = "bobyqa"))
posthoc_masker_rate_talker_exp1 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_masker_rate_talker_exp1$contrasts


##### Exp 2 Masker Rate statistics ######
model_masker_rate_exp2 <- mixed(MaskerRate ~ Talker + (1|S),data= subset(masker_rates_exp2,Masker == "Words"),control = lmerControl(optimizer = "bobyqa"))

model_masker_rate_exp2

# Significant effect of Talker, follow up with comparison
this_model <- lmer(MaskerRate ~ Talker + (1|S),
                   data= subset(masker_rates_exp2, Masker == "Words"), 
                   control = lmerControl(optimizer = "bobyqa"))
posthoc_masker_rate_talker_exp2 <- emmeans(this_model, pairwise ~ Talker, adjust = "bonferroni")
posthoc_masker_rate_talker_exp2$contrasts

library(ggplot2)
library(patchwork)





# --- Combine all plots with shared legend ---
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)

# ---------------------------
# Prepare long-format data
# ---------------------------

long_data <- bind_rows(
  hit_rates_exp1 %>%
    select(S, Masker, Talker, x_pos, HitRate) %>%
    rename(Value = HitRate) %>%
    mutate(Measure = "Hit Rate", Experiment = "Exp. 1"),
  
  FA_rates_exp1 %>%
    select(S, Masker, Talker, x_pos, FARate) %>%
    rename(Value = FARate) %>%
    mutate(Measure = "Within-stream FA Rate", Experiment = "Exp. 1"),
  
  masker_rates_exp1 %>%
    select(S, Masker, Talker, x_pos, MaskerRate) %>%
    rename(Value = MaskerRate) %>%
    mutate(Measure = "Between-stream FA Rate", Experiment = "Exp. 1"),
  
  hit_rates_exp2 %>%
    select(S, Masker, Talker, x_pos, HitRate) %>%
    rename(Value = HitRate) %>%
    mutate(Measure = "Hit Rate", Experiment = "Exp. 2"),
  
  FA_rates_exp2 %>%
    select(S, Masker, Talker, x_pos, FARate) %>%
    rename(Value = FARate) %>%
    mutate(Measure = "Within-stream FA Rate", Experiment = "Exp. 2"),
  
  masker_rates_exp2 %>%
    select(S, Masker, Talker, x_pos, MaskerRate) %>%
    rename(Value = MaskerRate) %>%
    mutate(Measure = "Between-stream FA Rate", Experiment = "Exp. 2")
)

# Remove scrambled masker for Masker Rate
long_data_filtered <- long_data %>%
  filter(!(Measure == "Between-stream FA Rate" & Masker == "Scrambled"))

# Summary statistics
summary_data_filtered <- long_data_filtered %>%
  group_by(Experiment, Measure, Masker, Talker, x_pos) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sem_value  = sd(Value, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )


# Calculate talker x positions
talker_positions <- long_data_filtered %>%
  group_by(Talker) %>%
  summarise(mean_x = mean(x_pos)) %>%
  arrange(mean_x)

talker_labels_wrapped <- str_wrap(talker_positions$Talker, width = 1)

# ---------------------------
# Custom y-limits per Measure
# ---------------------------
# Wrap long left y-axis labels



long_data_filtered$Measure <- factor(long_data_filtered$Measure,
                                     levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate"))
summary_data_filtered$Measure <- factor(summary_data_filtered$Measure,
                                        levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate"))

y_limits <- tibble(
  Measure = levels(long_data_filtered$Measure),
  ymin = c(0.4, 0, 0),
  ymax = c(1, 0.4, 0.1)
)

# Merge y-limits into summary_data_filtered and long_data_filtered
summary_data_filtered <- summary_data_filtered %>%
  left_join(y_limits, by = "Measure")

long_data_filtered <- long_data_filtered %>%
  left_join(y_limits, by = "Measure")

# After all filtering and joining for y-limits
long_data_filtered <- long_data_filtered %>%
  mutate(Measure = factor(Measure,
                          levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate")))

summary_data_filtered <- summary_data_filtered %>%
  mutate(Measure = factor(Measure,
                          levels = c("Hit Rate", "Within-stream FA Rate", "Between-stream FA Rate")))

levels(long_data_filtered$Measure) <- str_wrap(levels(long_data_filtered$Measure), width = 15)
levels(summary_data_filtered$Measure) <- str_wrap(levels(summary_data_filtered$Measure), width = 15)

# ---------------------------
# Plot
# ---------------------------
p <- ggplot(long_data_filtered, aes(x = x_pos, y = Value, color = Masker, fill = Masker)) +
  geom_line(aes(group = interaction(S, Masker)), alpha = 0.3) +
  geom_point(aes(group = interaction(S, Masker)), alpha = 0.3) +
  geom_line(data = summary_data_filtered,
            aes(x = x_pos, y = mean_value, group = Masker, color = Masker),
            size = 1.2, inherit.aes = FALSE) +
  geom_point(data = summary_data_filtered,
             aes(x = x_pos, y = mean_value, color = Masker, fill = Masker),
             size = 3, shape = 21, stroke = 1.2, inherit.aes = FALSE) +
  geom_errorbar(data = summary_data_filtered,
                aes(x = x_pos, ymin = mean_value - sem_value, ymax = mean_value + sem_value, color = Masker),
                width = 0.2, size = 1.2, inherit.aes = FALSE) +
  geom_blank(data = long_data_filtered, aes(y = ymin)) +
  geom_blank(data = long_data_filtered, aes(y = ymax)) +
  scale_x_continuous(breaks = talker_positions$mean_x,
                     labels = talker_labels_wrapped) +
  labs(x = "Talker", y = "") +
  facet_grid(Measure ~ Experiment, scales = "free_y", switch = "y") +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y.left = element_text(margin = margin(l = 10), size = 14, face = "bold"),
    axis.text.y.left = element_text(margin = margin(r = 5), size = 12),
    axis.text.x = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = unit(c(5,5,5,40), "pt"),
    strip.placement = "outside"
  )
p
