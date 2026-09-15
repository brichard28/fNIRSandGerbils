# Author: Benjamin Richardson
# Combined figure builder for Experiments 1 and 2: P1-N1 and P300
#
# Replaces the old MATLAB-plots + R-plots + Inkscape-assembly workflow.
# Reads:
#   1) the tidy per-subject time-course CSVs exported by
#      export_target_timecourse.m (target_timecourse_exp1.csv / exp2.csv)
#   2) the existing per-trial P1/N1/P2/P3 amplitude CSVs (unchanged --
#      this is the same loading/summarising logic as the original
#      combined-analysis script, just wrapped into functions)
#
# Produces two 8-panel figures (2 rows = Experiment, 4 cols =
# WordType x Talker), each panel showing:
#   - the Scrambled vs Word grand-average trace with shaded measurement
#     window(s)
#   - an inset with every subject's paired Scrambled/Word summary value
#     plus the group mean +/- SE

library(tidyverse)
library(patchwork)
library(svglite)

############################################################
### 0. Paths -- adjust to your machine
############################################################

data_dir <- "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data"
out_dir  <- "/Users/benrichardson/Documents/GitHub/fNIRSandGerbils"

############################################################
### 1. Load tidy time-course data (from export_target_timecourse.m)
############################################################

timecourse <- bind_rows(
  read_csv(file.path(data_dir, "target_timecourse_exp1.csv"), show_col_types = FALSE),
  read_csv(file.path(data_dir, "target_timecourse_exp2.csv"), show_col_types = FALSE)
) %>%
  mutate(
    Masker      = factor(Masker, levels = c("Scrambled", "Unscrambled")),
    WordType    = factor(WordType, levels = c("Color", "Object")),
    Talker      = factor(Talker, levels = c("Same", "Different")),
    Experiment  = factor(Experiment, levels = c("Exp1", "Exp2")),
    ChannelGroup = factor(ChannelGroup, levels = c("Frontocentral", "Parietooccipital"))
  )

############################################################
### 2. Load and summarise P1/N1/P3 amplitudes
###    (unchanged logic from the original combined-analysis script)
############################################################

load_exp <- function(exp_num) {
  p1 <- read.csv(file.path(data_dir, paste0("all_subs_p1_target_exp", exp_num, "_incl_resp.csv")))
  n1 <- read.csv(file.path(data_dir, paste0("all_subs_n1_target_exp", exp_num, "_incl_resp.csv")))
  p2 <- read.csv(file.path(data_dir, paste0("all_subs_p2_target_exp", exp_num, "_incl_resp.csv")))
  p3 <- read.csv(file.path(data_dir, paste0("all_subs_p3_target_exp", exp_num, "_incl_resp.csv")))
  
  names(p1)[names(p1) == "Amplitude"] <- "p1"
  names(n1)[names(n1) == "Amplitude"] <- "n1"
  names(p2)[names(p2) == "Amplitude"] <- "p2"
  names(p3)[names(p3) == "Amplitude"] <- "p3"
  
  df <- Reduce(
    function(x, y) merge(x, y, by = c("S", "Masker", "Talker", "WordType", "Electrode")),
    list(p1, n1, p2, p3)
  )
  df$Experiment <- factor(paste0("Exp", exp_num))
  df
}

exp1 <- load_exp(1)
exp2 <- load_exp(2)
all_data <- rbind(exp1, exp2)

to.factor <- c("S", "Masker", "Talker", "WordType", "Electrode", "Experiment")
all_data[to.factor] <- lapply(all_data[to.factor], as.factor)

frontocentral_electrodes    <- c("Fz","FC1","FC2","C3","Cz","C4","CP1","CP2")
parietooccipital_electrodes <- c("P3","Pz","PO3","O1","Oz","O2","PO4","P4")

fc_summary <- all_data %>%
  filter(Electrode %in% frontocentral_electrodes) %>%
  mutate(diff = p1 - n1) %>%
  group_by(Experiment, S, Masker, Talker, WordType) %>%
  summarise(value = mean(diff, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Masker     = factor(Masker, levels = c("Scrambled", "Unscrambled")),
    WordType   = factor(WordType, levels = c("Color", "Object")),
    Talker     = factor(Talker, levels = c("Same", "Different")),
    Experiment = factor(Experiment, levels = c("Exp1", "Exp2"))
  )

p3_summary <- all_data %>%
  filter(Electrode %in% parietooccipital_electrodes) %>%
  group_by(Experiment, S, Masker, Talker, WordType) %>%
  summarise(value = mean(p3, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Masker     = factor(Masker, levels = c("Scrambled", "Unscrambled")),
    WordType   = factor(WordType, levels = c("Color", "Object")),
    Talker     = factor(Talker, levels = c("Same", "Different")),
    Experiment = factor(Experiment, levels = c("Exp1", "Exp2"))
  )

############################################################
### 3. Shared aesthetics
############################################################

words_color     <- rgb(97/255, 156/255, 255/255)   # ggplot default blue
scrambled_color <- rgb(248/255, 118/255, 109/255)  # ggplot default orange
color_map <- c("Unscrambled" = words_color, "Scrambled" = scrambled_color)
shape_map <- c("Scrambled" = 15, "Unscrambled" = 17)
masker_labels <- c("Scrambled" = "Scrambled", "Unscrambled" = "Word")

window_fill <- "grey40"

############################################################
### 4. Panel builders
############################################################

# -- main trace panel: grand-average Scrambled vs Word, with shaded
#    measurement window(s) -----------------------------------------
make_trace_panel <- function(exp_label, word_type, talker, channel_group,
                             windows, y_limits, show_legend = FALSE,
                             show_x_axis = TRUE, show_y_axis = TRUE) {
  
  df <- timecourse %>%
    filter(Experiment == exp_label, WordType == word_type,
           Talker == talker, ChannelGroup == channel_group)
  
  trace_summary <- df %>%
    group_by(Time_ms, Masker) %>%
    summarise(
      mean_amp = mean(Amplitude, na.rm = TRUE),
      se_amp   = sd(Amplitude, na.rm = TRUE) / sqrt(n_distinct(Subject)),
      .groups  = "drop"
    )
  
  p <- ggplot(trace_summary, aes(x = Time_ms, y = mean_amp, color = Masker, fill = Masker))
  
  # shaded measurement window(s), drawn first so trace sits on top
  for (w in windows) {
    p <- p + annotate("rect", xmin = w$start, xmax = w$end,
                      ymin = -Inf, ymax = Inf, fill = window_fill, alpha = 0.12)
  }
  
  p <- p +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_ribbon(aes(ymin = mean_amp - se_amp, ymax = mean_amp + se_amp), color = NA, alpha = 0.25) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = color_map, labels = masker_labels, name = NULL) +
    scale_fill_manual(values = color_map, labels = masker_labels, name = NULL) +
    scale_x_continuous(limits = c(-50, 750), expand = c(0, 0)) +
    coord_cartesian(ylim = y_limits) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(fill = "transparent", colour = "grey20"),
      axis.text.y = element_text(size = 12, color = "grey20"),
      axis.text.x = element_text(size = 12, color = "grey20"),
      legend.position = if (show_legend) "top" else "none",
      plot.margin = margin(2, 2, 2, 2)
    )
  
  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  if (!show_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  
  p
}

# -- inset panel: every subject's paired value + group mean/SE -----
make_inset_panel <- function(exp_label, word_type, talker, summary_df, y_limits) {
  
  df <- summary_df %>%
    filter(Experiment == exp_label, WordType == word_type, Talker == talker)
  
  # deterministic per-subject horizontal offset (not per-point), so each
  # subject's Scrambled/Word points share an x position and the
  # connecting line between them is a straight diagonal, not zig-zagged
  subj_offset <- df %>%
    distinct(S) %>%
    mutate(offset = (row_number() %% 7 - 3) * 0.02)
  
  df <- df %>%
    left_join(subj_offset, by = "S") %>%
    mutate(x_num = as.numeric(Masker) + offset)
  
  sum_df <- df %>%
    group_by(Masker) %>%
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      se_val   = sd(value, na.rm = TRUE) / sqrt(n()),
      x_num    = as.numeric(Masker)[1],
      .groups  = "drop"
    )
  
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_line(data = df, aes(x = x_num, y = value, group = S),
              color = "grey60", linewidth = 0.3, alpha = 0.6) +
    geom_point(data = df, aes(x = x_num, y = value, color = "grey50"),
               size = 1, alpha = 0.7, show.legend = FALSE) +
    geom_errorbar(data = sum_df, aes(x = x_num, ymin = mean_val - se_val,
                                     ymax = mean_val + se_val, color = Masker),
                  width = 0.15, linewidth = 0.9, show.legend = FALSE) +
    geom_point(data = sum_df, aes(x = x_num, y = mean_val, color = Masker),
               size = 3, show.legend = FALSE) +
    scale_color_manual(values = color_map) +
    scale_x_continuous(breaks = c(1, 2), labels = masker_labels[levels(df$Masker)],
                       limits = c(0.75, 2.25)) +
    coord_cartesian(ylim = y_limits) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 7) +
    theme(
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.2, color = "grey60"),
      axis.ticks.y = element_line(linewidth = 0.2, color = "grey60"),
      axis.text.y = element_text(size = 12, color = "grey20"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
      plot.margin = margin(3, 3, 3, 3)
    )
}

# -- combine trace + inset into one composite panel -----------------
make_composite_panel <- function(exp_label, word_type, talker, channel_group,
                                 windows, y_limits_trace, y_limits_inset, summary_df,
                                 show_legend = FALSE, show_x_axis = TRUE, show_y_axis = TRUE) {
  
  main  <- make_trace_panel(exp_label, word_type, talker, channel_group, windows, y_limits_trace,
                            show_legend, show_x_axis, show_y_axis)
  inset <- make_inset_panel(exp_label, word_type, talker, summary_df, y_limits_inset)
  
  # inset fixed in the southeast corner of every panel
  main + inset_element(inset, left = 0.50, bottom = 0.02, right = 0.99, top = 0.45)
}

############################################################
### 5. Build the P1-N1 figure (Frontocentral, 2 shaded windows)
############################################################

panel_specs <- expand_grid(
  Experiment = factor(c("Exp1", "Exp2"), levels = c("Exp1", "Exp2")),
  WordType   = factor(c("Color", "Object"), levels = c("Color", "Object")),
  Talker     = factor(c("Same", "Different"), levels = c("Same", "Different"))
) %>%
  arrange(Experiment, WordType, Talker)

p1n1_windows <- list(
  list(start = 30,  end = 150), # P1
  list(start = 150, end = 250)  # N1
)

p1n1_panels <- pmap(panel_specs, function(Experiment, WordType, Talker) {
  make_composite_panel(
    exp_label = Experiment, word_type = WordType, talker = Talker,
    channel_group = "Frontocentral", windows = p1n1_windows,
    y_limits_trace = c(-5.25, 1.5), y_limits_inset = c(-1, 7.5),
    summary_df = fc_summary,
    show_legend = (Experiment == "Exp1" & WordType == "Color" & Talker == "Same"),
    show_x_axis = (Experiment == "Exp2"),
    show_y_axis = (WordType == "Color" & Talker == "Same")
  )
})

# 2 rows (Experiment) x 4 cols (WordType x Talker), in the row order
# already produced by arrange() above
p1n1_figure <- wrap_plots(p1n1_panels, nrow = 2, ncol = 4)

ggsave(file.path(out_dir, "p1n1_combined_figure_raw.svg"), p1n1_figure,
       device = svglite::svglite, width = 15, height = 7, units = "in", bg = "white")

############################################################
### 6. Build the P300 figure (Parietooccipital, 1 shaded window)
############################################################

p300_windows <- list(list(start = 250, end = 600))

p300_panels <- pmap(panel_specs, function(Experiment, WordType, Talker) {
  make_composite_panel(
    exp_label = Experiment, word_type = WordType, talker = Talker,
    channel_group = "Parietooccipital", windows = p300_windows,
    y_limits_trace = c(-5.0, 1.5), y_limits_inset = c(-2, 5),
    summary_df = p3_summary,
    show_legend = (Experiment == "Exp1" & WordType == "Color" & Talker == "Same"),
    show_x_axis = (Experiment == "Exp2"),
    show_y_axis = (WordType == "Color" & Talker == "Same")
  )
})

p300_figure <- wrap_plots(p300_panels, nrow = 2, ncol = 4)

ggsave(file.path(out_dir, "p300_combined_figure_raw.svg"), p300_figure,
       device = svglite::svglite, width = 15, height = 7, units = "in", bg = "white")

p1n1_figure
p300_figure