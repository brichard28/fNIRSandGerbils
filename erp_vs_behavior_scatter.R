# Author: Benjamin Richardson
# Individual-subject scatter plots: ERP components (p1-n1, P3) vs behavioral
# measures (Hit Rate, Within-stream FA Rate, Between-stream FA Rate)
# Experiments 1 & 2, style matched to behavior_plot / p1n1_p3 scripts

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(afex)
library(emmeans)
library(patchwork)
library(grid)
library(ggforce)
library(R.matlab)

############################################################
# 1. UNIVERSAL SETTINGS (matched to existing scripts)
############################################################

data_base <- "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/"
fig_base  <- "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/PAPER FIGURES/"

words_color     <- rgb(97/255, 156/255, 255/255)   # ggplot default blue
scrambled_color <- rgb(248/255, 118/255, 109/255)  # ggplot default orange
color_map <- c("Words" = words_color, "Scrambled" = scrambled_color)
shape_map <- c("Scrambled" = 15, "Words" = 17)

frontocentral_electrodes    <- c("Fz", "FC1", "FC2", "C3", "Cz", "C4", "CP1", "CP2")
parietooccipital_electrodes <- c("P3", "Pz", "PO3", "O1", "Oz", "O2", "PO4", "P4")

# Row order of subjects in the behavioral CSVs (Scrambled_Speech_*_exp_N.csv),
# taken directly from the MATLAB curr_subject_ID definitions. Row 1 of each
# behavioral CSV corresponds to the first ID in the relevant vector, row 2 to
# the second, etc.
subject_ids <- list(
  "1" = c('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041',
          '7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'),
  "2" = c('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069',
          '7070','7071','7072','7073','7076','7077','7078','7079','7080','7082')
)

############################################################
# 2. LOAD BEHAVIORAL DATA (Hit / Within-FA / Between-FA), subject level
#    (loader matched to behavior_plot script, with row -> subject ID mapping)
############################################################

load_behavior_measure <- function(filepath, measure_name, exp_num) {
  
  df <- read.csv(filepath, header = FALSE)
  df <- cbind(row_num = 1:nrow(df), df)
  colnames(df) <- c("row_num",
                    "Scrambled_Different","Scrambled_Same",
                    "Unscrambled_Different","Unscrambled_Same")
  
  ids <- subject_ids[[as.character(exp_num)]]
  if (nrow(df) != length(ids)) {
    warning(sprintf(
      "Exp %d: %d rows in %s but %d subject IDs on record -- row/ID mapping may be wrong.",
      exp_num, nrow(df), basename(filepath), length(ids)
    ))
  }
  df$S <- ids[df$row_num]
  df$row_num <- NULL
  
  df <- pivot_longer(df, cols = -S, names_to = c("Masker","Talker"),
                     names_sep = "_", values_to = measure_name)
  
  df$Masker <- recode(df$Masker, "Unscrambled" = "Words")
  df$Talker <- factor(df$Talker, levels = c("Same","Different"))
  df$S <- trimws(as.character(df$S))
  df$Masker <- factor(df$Masker, levels = c("Scrambled","Words"))
  
  df
}

load_behavior_experiment <- function(exp_num) {
  
  hit <- load_behavior_measure(
    paste0(data_base, "Scrambled_Speech_Hit_Rates_exp_", exp_num, ".csv"),
    "HitRate", exp_num) %>%
    rename(Value = HitRate) %>%
    mutate(Measure = "Hit Rate")
  
  fa <- load_behavior_measure(
    paste0(data_base, "Scrambled_Speech_FA_Rates_exp_", exp_num, ".csv"),
    "FARate", exp_num) %>%
    rename(Value = FARate) %>%
    mutate(Measure = "Within-stream FA Rate")
  
  masker <- load_behavior_measure(
    paste0(data_base, "Scrambled_Speech_Masker_rates_exp_", exp_num, ".csv"),
    "MaskerRate", exp_num) %>%
    filter(Masker == "Words") %>%
    rename(Value = MaskerRate) %>%
    mutate(Measure = "Between-stream FA Rate")
  
  bind_rows(hit, fa, masker) %>%
    mutate(Experiment = paste0("Exp. ", exp_num)) %>%
    select(S, Masker, Talker, Measure, Value, Experiment)
}

behavior_data <- bind_rows(
  load_behavior_experiment(1),
  load_behavior_experiment(2)
) %>%
  mutate(Measure = factor(Measure,
                          levels = c("Hit Rate",
                                     "Within-stream FA Rate",
                                     "Between-stream FA Rate")))

############################################################
# 3. LOAD ERP DATA (p1-n1 difference & P3), subject level
#    Averaged across WordType and Electrode, since behavior has
#    no WordType factor -> one value per S x Masker x Talker
############################################################

# Normalizes Masker/Talker labels to a common scheme. Applied to EACH of the
# p1/n1/p3 files individually, BEFORE merging them together -- if the raw
# label spelling differs slightly across those files (e.g. "Words" in one,
# "Unscrambled" in another), merging on the raw strings silently drops every
# row where the labels don't match verbatim, which is what was happening here.
normalize_masker_talker <- function(df) {
  
  df$S <- trimws(as.character(df$S))
  
  masker_chr <- trimws(as.character(df$Masker))
  df$Masker <- case_when(
    grepl("unscrambled", masker_chr, ignore.case = TRUE) ~ "Words",
    grepl("word", masker_chr, ignore.case = TRUE)        ~ "Words",
    grepl("scrambled", masker_chr, ignore.case = TRUE)   ~ "Scrambled",
    TRUE ~ masker_chr
  )
  
  talker_chr <- trimws(as.character(df$Talker))
  df$Talker <- case_when(
    grepl("^same", talker_chr, ignore.case = TRUE) ~ "Same",
    grepl("^diff", talker_chr, ignore.case = TRUE) ~ "Different",
    TRUE ~ talker_chr
  )
  
  df
}

load_erp_experiment <- function(exp_num) {
  
  p1_data <- read.csv(paste0(data_base, "all_subs_p1_target_exp", exp_num, "_incl_resp.csv"))
  n1_data <- read.csv(paste0(data_base, "all_subs_n1_target_exp", exp_num, "_incl_resp.csv"))
  p3_data <- read.csv(paste0(data_base, "all_subs_p3_target_exp", exp_num, "_incl_resp.csv"))
  
  names(p1_data)[names(p1_data) == "Amplitude"] <- "p1"
  names(n1_data)[names(n1_data) == "Amplitude"] <- "n1"
  names(p3_data)[names(p3_data) == "Amplitude"] <- "p3"
  
  # Diagnostic: raw label sets per file, before normalization/merge
  cat(sprintf("---- Exp %d raw ERP labels (before normalization) ----\n", exp_num))
  cat("p1_data$Masker: ", paste(unique(p1_data$Masker), collapse = ", "), "\n")
  cat("n1_data$Masker: ", paste(unique(n1_data$Masker), collapse = ", "), "\n")
  cat("p3_data$Masker: ", paste(unique(p3_data$Masker), collapse = ", "), "\n")
  cat("p1_data$Talker: ", paste(unique(p1_data$Talker), collapse = ", "), "\n")
  cat("n1_data$Talker: ", paste(unique(n1_data$Talker), collapse = ", "), "\n")
  cat("p3_data$Talker: ", paste(unique(p3_data$Talker), collapse = ", "), "\n\n")
  
  p1_data <- normalize_masker_talker(p1_data)
  n1_data <- normalize_masker_talker(n1_data)
  p3_data <- normalize_masker_talker(p3_data)
  
  cat(sprintf("---- Exp %d Masker counts AFTER normalization, before merge ----\n", exp_num))
  cat("p1_data: "); print(table(p1_data$Masker))
  cat("n1_data: "); print(table(n1_data$Masker))
  cat("p3_data: "); print(table(p3_data$Masker))
  
  all_data <- Reduce(function(x, y) merge(x, y, by = c("S", "Masker", "Talker", "WordType","Electrode")),
                     list(p1_data, n1_data, p3_data))
  
  cat(sprintf("---- Exp %d Masker counts AFTER 3-way merge ----\n", exp_num))
  print(table(all_data$Masker))
  
  to.factor <- c("S", "Masker", "Talker", "WordType","Electrode")
  all_data[, to.factor] <- lapply(all_data[, to.factor], as.factor)
  
  frontocentral <- all_data %>%
    filter(Electrode %in% frontocentral_electrodes) %>%
    mutate(diff_n1_p1 = p1 - n1) %>%
    group_by(S, Masker, Talker) %>%
    summarise(mean_diff = mean(diff_n1_p1, na.rm = TRUE), .groups = "drop")
  
  parietooccipital <- all_data %>%
    filter(Electrode %in% parietooccipital_electrodes) %>%
    group_by(S, Masker, Talker) %>%
    summarise(mean_p3 = mean(p3, na.rm = TRUE), .groups = "drop")
  
  cat(sprintf("---- Exp %d Masker counts AFTER electrode filter + groupby ----\n", exp_num))
  cat("frontocentral: "); print(table(frontocentral$Masker))
  cat("parietooccipital: "); print(table(parietooccipital$Masker))
  
  finalize <- function(df) {
    df$S <- trimws(as.character(df$S))
    df$Masker <- factor(as.character(df$Masker), levels = c("Scrambled","Words"))
    df$Talker <- factor(as.character(df$Talker), levels = c("Same","Different"))
    df
  }
  
  list(
    p1n1 = finalize(frontocentral)    %>% mutate(Experiment = paste0("Exp. ", exp_num)),
    p3   = finalize(parietooccipital) %>% mutate(Experiment = paste0("Exp. ", exp_num))
  )
}

erp_exp1 <- load_erp_experiment(1)
erp_exp2 <- load_erp_experiment(2)

p1n1_data <- bind_rows(erp_exp1$p1n1, erp_exp2$p1n1)
p3_data_all <- bind_rows(erp_exp1$p3, erp_exp2$p3)

############################################################
# 4. DIAGNOSTIC CHECK — run this if a join still comes back with 0 rows.
############################################################

cat("---- S ----\n")
cat("behavior_data$S sample: ", paste(head(sort(unique(behavior_data$S)), 10), collapse = ", "), "\n")
cat("p1n1_data$S sample:     ", paste(head(sort(unique(p1n1_data$S)), 10), collapse = ", "), "\n")
cat("S in behavior but not ERP: ", paste(setdiff(unique(behavior_data$S), unique(p1n1_data$S)), collapse = ", "), "\n")
cat("S in ERP but not behavior: ", paste(setdiff(unique(p1n1_data$S), unique(behavior_data$S)), collapse = ", "), "\n\n")

cat("---- Masker ----\n")
cat("behavior_data$Masker levels: ", paste(unique(behavior_data$Masker), collapse = ", "), "\n")
cat("p1n1_data$Masker levels:     ", paste(unique(p1n1_data$Masker), collapse = ", "), "\n\n")

cat("---- Talker ----\n")
cat("behavior_data$Talker levels: ", paste(unique(behavior_data$Talker), collapse = ", "), "\n")
cat("p1n1_data$Talker levels:     ", paste(unique(p1n1_data$Talker), collapse = ", "), "\n\n")

############################################################
# 5. MERGE ERP WITH BEHAVIOR, SUBJECT LEVEL
############################################################

p1n1_vs_behavior <- behavior_data %>%
  inner_join(p1n1_data, by = c("S","Masker","Talker","Experiment"))

p3_vs_behavior <- behavior_data %>%
  inner_join(p3_data_all, by = c("S","Masker","Talker","Experiment"))

cat("p1n1_vs_behavior rows:", nrow(p1n1_vs_behavior), "\n")
cat("p3_vs_behavior rows:  ", nrow(p3_vs_behavior), "\n")
cat("p1n1_vs_behavior Masker counts: "); print(table(p1n1_vs_behavior$Masker))
cat("p3_vs_behavior Masker counts:   "); print(table(p3_vs_behavior$Masker))

############################################################
# 6. PLOTTING FUNCTION (style matched to existing figures)
############################################################

plot_erp_vs_behavior <- function(data, erp_col, erp_label) {
  
  data_hit   <- data %>% filter(Measure == "Hit Rate")
  data_other <- data %>% filter(Measure != "Hit Rate")
  
  ggplot(data, aes(x = .data[[erp_col]], y = Value, color = Masker)) +
    
    geom_point(aes(shape = Masker), size = 2.5, alpha = 0.6) +
    
    geom_smooth(aes(group = Masker), method = "lm", se = FALSE,
                size = 0.8, formula = y ~ x) +
    
    # Hit Rate panels: correlation labels in the bottom-right corner
    stat_cor(data = data_hit, aes(color = Masker), method = "pearson",
             size = 3.5, show.legend = FALSE,
             label.x.npc = 0.6,
             label.y.npc = c(0.10, 0.07)) +
    
    # Within-stream / Between-stream FA panels: labels in the top-right corner
    stat_cor(data = data_other, aes(color = Masker), method = "pearson",
             size = 3.5, show.legend = FALSE,
             label.x.npc = 0.6,
             label.y.npc = c(0.90, 0.87)) +
    
    scale_color_manual(values = color_map) +
    scale_shape_manual(values = shape_map) +
    
    facet_grid(Measure ~ Experiment, scales = "free", switch = "y") +
    
    labs(x = erp_label, y = "") +
    
    theme_minimal() +
    theme(
      legend.position = "right",
      strip.text = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      panel.spacing = unit(1, "lines"),
      plot.margin = unit(c(5,5,5,40), "pt"),
      strip.placement = "outside"
    )
}

############################################################
# 7. p1-n1 vs behavior
############################################################

p_p1n1 <- plot_erp_vs_behavior(p1n1_vs_behavior, "mean_diff",
                               "P1 - N1 (\u00b5V)")
p_p1n1

ggsave(paste0(fig_base, "p1n1_vs_behavior.svg"), p_p1n1,
       width = 8, height = 8, device = "svg")

############################################################
# 8. P3 vs behavior
############################################################

p_p3 <- plot_erp_vs_behavior(p3_vs_behavior, "mean_p3",
                             "P300 (\u00b5V)")
p_p3

ggsave(paste0(fig_base, "p300_vs_behavior.svg"), p_p3,
       width = 8, height = 8, device = "svg")