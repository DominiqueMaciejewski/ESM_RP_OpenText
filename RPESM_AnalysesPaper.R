# ---- Behind the Number: Using Open-Ended Responses to Uncover Response Processes in Emotion Ratings in ESM Studies----
#
# This script examines the occurrence and distribution of response processes
# in the ESM open-text responses, including individual differences and changes
# over the study period.
#
# DATA
# Input:
# RPESM_MPath_Qualtrics_RP_2026-09-03
#
# Open-text responses are stored across the emotion-specific response columns:
# happy_RP, relaxed_RP, irritated_RP, nervous_RP, down_RP
#
# MAIN ANALYSES
# - Overall frequencies of response-process codes and categories
# - Venn Diagram of overlap between timeframes
# - Distribution of response-process codes across emotions
# - Distribution of response-process codes across emotion intensities
# - Between-person differences in response-process occurrence, word count, and response process count
# - Correlation between word count and number of response-process codes
# - Open-text response length and unique word count
# - Within-person changes in response processes before vs. after the
# halfway interview
# - Individual response-process profiles (heatmaps)
#
# OUTPUT
# Figures for RQ1-RQ4
# Supporting figures

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(viridis)
library(RColorBrewer)
library(ggnewscale)
library(patchwork)
library(ggpattern)
library(epubr)
library(scales)

proj_root = here::here()
data_dir = file.path(here::here(), "2_Data")
figures_dir = file.path(proj_root, "4_Figures")
load(file.path(data_dir, "RPESM_MPath_Qualtrics_RP_2026-09-03.RData"))

# ---- Sample ----
sample_size <- unique(RPESM_merged_data$participant_ID)
observations <- sum(RPESM_merged_data$valid)
rp_observations <- nrow(RPESM_merged_data %>% filter(!is.na(RP_emotion)))

## ---- Dropout ----
idx_dropout <- RPESM_merged_data[["duration"]] < 28

RPESM_merged_data <- RPESM_merged_data %>%
  mutate(dropout = duration < 28)

## ---- Compliance ----
RPESM_merged_data <- RPESM_merged_data %>%
  group_by(participant_ID) %>%
  mutate(compliance = sum(valid, na.rm = T) / 140) %>%
  ungroup()

## ---- Descriptive ----
data_person <- RPESM_merged_data %>%
  filter(!duplicated(participant_ID)) %>%
  select(participant_ID,
    gender,
    age,
    student,
    culture,
    dropout,
    duration,
    compliance) %>%
  mutate(across(gender:compliance, as.numeric),
         student = factor(student,
                          levels = c(1),
                          labels = "yes"),
         gender = factor(gender,
                         levels = c(1, 2),
                         labels = c("male", "female")))

### ---- Student ----
table(data_person[["student"]])
### ---- Gender ----
table(data_person[["gender"]])
### ---- Culture ----
table(data_person[["culture"]])
### ---- Dropout ----
table(data_person[["dropout"]])

do.call(rbind, lapply(c("compliance", "duration", "age"), function(col) {
    x <- data_person[[col]]
    data.frame(
      variable = col,
      n = sum(!is.na(x)),
      mean = round(mean(x, na.rm = TRUE), 2),
      sd = round(sd(x, na.rm = TRUE), 2),
      min = round(min(x, na.rm = TRUE), 2),
      max = round(max(x, na.rm = TRUE), 2))}))

# ---- Data Frame ----
## Select RP variables

RP_data <- RPESM_merged_data %>%
  select(
    participant_ID, obsno, valid, timeStampSent, daycum, beepno, RP_emotion,
    average_level_comparison, numbered_comparison, intensity_comparison, past_temporal_comparison, future_temporal_comparison, personally_impactful_event_comparison, criteria_based_comparison, social_comparison, # Comparison
    gut_feeling, introspection_affect, introspection_cognition, introspection_embodied, referring_to_other_emotions, struggle_identifying_feeling, control_over_situation_or_emotion, self_concept_identity, # Internal
    contextual_evidence, absent_evidence, counterfactual_evidence, interpersonal_evidence, health_evidence, # Evidence
    current_evidence_collection, past_evidence_collection, future_evidence_collection, # Time Evidence
    unsure_participant, idiosyncratic_code, unclear_to_coder, # Other
    comparison_standards, internal_processes, evidence_collection, other_codes, # Categoties
    happy, relaxed, irritated, nervous, down, # Emotions
    happy_RP, relaxed_RP, irritated_RP, nervous_RP, down_RP, # Open text
    cog_interview_date_day15
  ) %>%
  rename_with(~str_to_title(str_replace_all(.,"_", " ")), .cols = average_level_comparison:other_codes)

comparison_standards <- RP_data %>%
  select(`Average Level Comparison`:`Social Comparison`) %>%
  names()

internal_processes <-  RP_data %>%
  select(`Gut Feeling`:`Self Concept Identity`) %>%
  names()

evidence_collection <- RP_data %>%
  select(`Contextual Evidence`:`Health Evidence`) %>%
  names()

time_evidence <- c("Current Evidence Collection", "Past Evidence Collection", "Future Evidence Collection")

other_codes <-  RP_data %>%
  select(`Unsure Participant`:`Unclear To Coder`) %>%
  names()

categories <- c("Comparison Standards", "Internal Processes", "Evidence Collection", "Time Evidence", "Other Codes")

RP_data <- RP_data %>%
  mutate(`Time Evidence` = as.numeric((`Current Evidence Collection`+ `Past Evidence Collection` + `Future Evidence Collection`) > 0),
         .before = `Other Codes`)

category_lookup <- tibble(
  response_process = c(comparison_standards, internal_processes, evidence_collection, time_evidence, other_codes),
  Category = case_when(
    response_process %in% comparison_standards ~ "Comparison Standards",
    response_process %in% internal_processes ~ "Internal Processes",
    response_process %in% evidence_collection ~ "Evidence Collection",
    response_process %in% time_evidence ~ "Time Evidence",
    response_process %in% other_codes ~ "Other Codes",
    TRUE ~ NA
  ))

category_order <- c("Comparison Standards", "Internal Processes", "Evidence Collection", "Time Evidence", "Other Codes")

rp_cols <- category_lookup$response_process

emotion_order <- c("happy", "relaxed", "irritated", "nervous", "down")
## ---- Descriptive ----
RP_data %>% filter(!is.na(RP_emotion)) %>%
  rowwise() %>% mutate(sum_rp = sum(c_across(all_of(rp_cols)))) %>%
  select(sum_rp) %>%
  summary()


# ---- RQ1: RP Frequency ----
# Compute frequencies
codes_freq <- RP_data %>%
  select(all_of(rp_cols)) %>%
  summarise_all(~sum(.x, na.rm = T)) %>%
  pivot_longer(everything(), names_to = "Code", values_to = "Code Frequency") %>%
  left_join(category_lookup %>% select(response_process, Category), by = c("Code" = "response_process"))

freq_df <- RP_data %>%
  select(all_of(categories)) %>%
  summarise_all(~sum(.x, na.rm = T)) %>%
  pivot_longer(everything(), names_to = "Category", values_to = "Category Presence") %>%
  right_join(codes_freq, by = "Category") %>%
  group_by(Category)%>%
  mutate(`Category Total` = sum(`Code Frequency`), .after = "Category Presence") %>%
  arrange(desc(`Category Total`), desc(`Code Frequency`))

freq_df <- freq_df %>%
  mutate(Category = factor(Category, levels = unique(freq_df$Category)))

## ---- Relative Frequency Plot ----
rel_freq_df <- freq_df %>%
  mutate(`Relative frequency (Category)` = round(`Category Presence`/ 4171 * 100, 2), .after = "Category Presence") %>%
  mutate(Code = factor(Code, levels = rev(Code)),
         Category = factor(Category, levels = rev(unique(Category))),
         `Relative frequency` = round(`Code Frequency`/ 4171 * 100, 2),
         `Category labels` = factor(paste0(`Category`, " \n(", `Relative frequency (Category)`, "%)")))

ggplot(rel_freq_df, aes(x = Code, y = `Relative frequency`, fill = `Category labels` )) +
  facet_grid(`Category labels`  ~ .,
             scales = "free_y",
             space = "free_y") +
  scale_fill_viridis_d(option = "F", begin = 0.5, end = 0.85) +
  geom_col() +
  geom_text(aes(label = paste0(`Relative frequency`, "%")),
            hjust = -0.05,
            size = 4) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Response Process",
       y = "Proportion (%)",
       title = "Proportional Occurrence of Response Processes",
       subtitle = "Percentage of total responses (N = 4.171) in which a code was applied ") +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(size = 12, angle = 0, face = "bold", hjust = 0),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "gray95", color = NA),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

ggsave(file.path(figures_dir, "RQ1 - Frequency/Relative Frequency Plot.png"),
       width = 17,
       height = 10,
       units = "in",
       dpi = 600)

## ---- Time Evidence ----
timescales <- RP_data %>% filter(`Time Evidence` == 1& !is.na(RP_data$`Time Evidence`)) %>% select(c("Current Evidence Collection", "Past Evidence Collection", "Future Evidence Collection"))

names(timescales) <- gsub("\\s+\\S*", "\\1", names(timescales), perl = TRUE)

png(file = file.path(figures_dir, "RQ1 - Frequency/Venn Diagram.png"),
  width = 16,
  height = 16,
  units = "in",
  res = 600)

plot(eulerr::euler(timescales, shape = "ellipse"),
     quantities = list(type = c("counts", "percent"),
                       fontsize = 20,
                       font = 1,
                       round = 4,
                       cex = 0.8),
     fills = list(fill = c("#5A1E51FF", "#EF5840FF", "#F7C5A5FF"), alpha = 0.5),
     labels = list(fontsize = 22, col = "black"))

dev.off()

# ---- RQ2: RP Between Emotions ----
#' To better understand the occurrence of different response processes, we explored whether these differed between different emotions (e.g., happy versus sad) and intensities (e.g., low versus high levels of happiness).
## ---- RQ2A: How response processes differ across emotions ----
# summary of how often the emotion was asked across participants
RP_data %>%
  filter(!is.na(RP_emotion)) %>%
  group_by(RP_emotion) %>%
  summarize(n())

codes_emo_freq <- RP_data %>%
  filter(!is.na(RP_emotion)) %>%
  pivot_longer(cols = all_of(rp_cols),
               names_to = "Code",
               values_to = "coded") %>%
  group_by(Code, RP_emotion) %>%
  summarise(`Frequency emotion` = sum(coded)) %>%
  left_join(rel_freq_df, by = "Code") %>%
  mutate(RP_emotion = factor(RP_emotion, levels = rev(emotion_order))) %>%
  mutate(Code = factor(Code, levels = rev(rel_freq_df$Code)),
         `Relative frequency (Emotion)` = 100*`Frequency emotion`/ 4171, .after = "Frequency emotion")

ggplot(codes_emo_freq , aes(x = Code, y = `Relative frequency (Emotion)`, fill = `RP_emotion` )) +
  facet_grid(`Category labels`  ~ .,
             scales = "free_y",
             space = "free_y") +
  scale_fill_manual(values = c( "#BD5579", "#b0315b", "#c63d4d", "#355e8d", "#3d4e8a"), name = "Emotion") +
  geom_col() +
  geom_text(data = codes_emo_freq  %>% distinct(Code, `Category labels`, `Relative frequency`),
            aes(x = Code, y = `Relative frequency`, label = paste0(`Relative frequency`, "%")),
            hjust = -0.05,
            inherit.aes = FALSE) +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Response Process",
       y = "Proportion (%)",
       title = "Proportional Occurrence of Response Processes",
       subtitle = "Percentage of total responses (N = 4.171) in which a code was applied ") +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(size = 12, angle = 0, face = "bold", hjust = 0),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "gray95", color = NA),
        legend.position = "top",
        legend.key.spacing.x = unit(1, "pt"),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

ggsave(file.path(figures_dir, "RQ2a - Emotions/Relative Frequency Plot with Emotions.png"),
       width = 17,
       height = 10,
       units = "in",
       dpi = 600)

## ---- RQ2B: How response processes differ across intensities ----
### ---- PA ----
rp_data_pa <- RP_data %>%
  filter(!is.na(RP_emotion), RP_emotion %in% c("happy", "relaxed"))

nrow(rp_data_pa)

category_freq_df_pa <- rp_data_pa %>%
  select(all_of(categories)) %>%
  summarise_all(~sum(.x, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Category", values_to = "Category Presence") %>%
  arrange(desc(`Category Presence`)) %>%
  mutate(Category = factor(Category, levels = unique(Category)),
         `Relative frequency (Category)` = round(`Category Presence` / nrow(rp_data_pa) * 100, 2)) %>%
  arrange(Category) %>%
  mutate(`Category labels` = factor(paste0(Category, " \n(", `Relative frequency (Category)`, "%)"),
                                    levels = paste0(Category, " \n(", `Relative frequency (Category)`, "%)")))

codes_int_freq_pa <- rp_data_pa %>%
  mutate(intensity = case_when(RP_emotion == "happy" ~ happy,
                               RP_emotion == "relaxed" ~ relaxed,
                               .default = NA),
         .after = "RP_emotion") %>%
  pivot_longer(cols = all_of(rp_cols),
               names_to = "Code",
               values_to = "coded") %>%
  group_by(Code, intensity) %>%
  summarise(`Frequency emotion` = sum(coded), .groups = "drop_last") %>%
  left_join(category_lookup, by = c("Code" = "response_process")) %>%
  ungroup() %>%
  mutate(Intensity = factor(intensity, levels = rev(0:10)),
         `Relative frequency (Intensity)` = 100 * `Frequency emotion` / nrow(rp_data_pa),
         .after = "Frequency emotion") %>%
  left_join(category_freq_df_pa %>%
              select(Category, `Category Presence`, `Relative frequency (Category)`, `Category labels`),
            by = "Category")

arrange_pa_codes <- codes_int_freq_pa %>%
  group_by(Code, `Category labels`) %>%
  summarise(`Relative frequency (Code)` = sum(`Relative frequency (Intensity)`), .groups = "drop") %>%
  group_by(`Category labels`) %>%
  mutate(`Category Frequency` = sum(`Relative frequency (Code)`)) %>%
  arrange(`Category Frequency`, `Relative frequency (Code)`)

codes_int_freq_pa <- codes_int_freq_pa %>%
  mutate(Code = factor(Code, levels = (arrange_pa_codes$Code)))

blue_pal <- colorRampPalette(brewer.pal(9, "Blues"))(11)

int_freq_pa <- ggplot(codes_int_freq_pa, aes(x = Code, y = `Relative frequency (Intensity)`, fill = Intensity)) +
  facet_grid(rows = vars(`Category labels`),
             scales = "free",
             space = "free",
             axes = "all_x",
             axis.labels = "all_x") +
  scale_fill_manual(values = blue_pal, guide = guide_legend(nrow = 1), breaks = c(0:10))+
  geom_col() +
  geom_text(data = arrange_pa_codes, aes(x = Code, y = `Relative frequency (Code)`, label = paste0(round(`Relative frequency (Code)`, 2), "%")),
            hjust = -0.05,
            inherit.aes = FALSE) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .10))
  ) +
  coord_flip(clip = "off") +  theme_minimal() +
  labs(x = "Response Process",
       y = "Proportion (%)",
       title = "Proportional Occurrence of Response Processes for PA",
       subtitle = "Percentage of all responses where a positive item was asked (N = 1.669) in which a code was applied ") +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(size = 12, angle = 0, face = "bold", hjust = 0),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "gray95", color = NA),
        legend.position = "top", legend.key.spacing.x = unit(1, "pt"),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# Density Distribution

hist_pa <- RP_data %>%
  filter(!is.na(RP_emotion), RP_emotion %in% c("happy", "relaxed")) %>%
  mutate(intensity = case_when(RP_emotion == "happy" ~ happy,
                               RP_emotion == "relaxed" ~ relaxed,
                               .default = NA)) %>%
  filter(!is.na(intensity)) %>%
  count(RP_emotion, intensity, name = "n") %>%
  group_by(RP_emotion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(intensity = factor(intensity, levels = rev(0:10))) %>%
  ggplot(aes(x = intensity, y = prop, fill = intensity, pattern = RP_emotion)) +
  geom_col_pattern(position = position_dodge2(preserve = "single"), color = NA, pattern_fill = "gray", pattern_density = 0.15, pattern_spacing = 0.1, pattern_angle = 45) +
  scale_pattern_manual(values = c(happy = "none", relaxed = "stripe"), name = "Emotion") +
  scale_fill_manual(values = blue_pal, guide = guide_legend(nrow = 1), breaks = c(0:10))+
  guides(fill="none") +
  scale_x_discrete(limits = as.character(0:10), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        legend.position = "right") +
  labs(x = "Intensity", y = "Frequency", title = "")

combined_plot_pa <- int_freq_pa / hist_pa +
  plot_layout(heights = c(5, 1))

combined_plot_pa

ggsave(file.path(figures_dir, "RQ2b - Intensities/Relative Frequency Plot with Intensities PA.png"), combined_plot_pa,
       width = 17,
       height = 10,
       units = "in",
       dpi = 600)

### ---- NA ----
rp_data_na <- RP_data %>%
  filter(!is.na(RP_emotion), RP_emotion %in% c("irritated", "nervous", "down"))
nrow(rp_data_na)

category_freq_df_na <- rp_data_na %>%
  select(all_of(categories)) %>%
  summarise_all(~sum(.x, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Category", values_to = "Category Presence") %>%
  arrange(desc(`Category Presence`)) %>%
  mutate(Category = factor(Category, levels = unique(Category)),
         `Relative frequency (Category)` = round(`Category Presence` / nrow(rp_data_na) * 100, 2)) %>%
  arrange(Category) %>%
  mutate(`Category labels` = factor(paste0(Category, " \n(", `Relative frequency (Category)`, "%)"),
                                    levels = paste0(Category, " \n(", `Relative frequency (Category)`, "%)")))

codes_int_freq_na <- rp_data_na %>%
  mutate(intensity = case_when(RP_emotion == "down" ~ down,
                               RP_emotion == "nervous" ~ nervous,
                               RP_emotion == "irritated" ~ irritated,
                               .default = NA),
         .after = "RP_emotion") %>%
  pivot_longer(cols = all_of(rp_cols),
               names_to = "Code",
               values_to = "coded") %>%
  group_by(Code, intensity) %>%
  summarise(`Frequency emotion` = sum(coded), .groups = "drop_last") %>%
  left_join(category_lookup, by = c("Code" = "response_process")) %>%
  ungroup() %>%
  mutate(Intensity = factor(intensity, levels = rev(0:10)),
         `Relative frequency (Intensity)` = 100 * `Frequency emotion` / nrow(rp_data_na),
         .after = "Frequency emotion") %>%
  left_join(category_freq_df_na %>%
              select(Category, `Category Presence`, `Relative frequency (Category)`, `Category labels`),
            by = "Category")

arrange_na_codes <- codes_int_freq_na %>%
  group_by(Code, `Category labels`) %>%
  summarise(`Relative frequency (Code)` = sum(`Relative frequency (Intensity)`), .groups = "drop") %>%
  group_by(`Category labels`) %>%
  mutate(`Category Frequency` = sum(`Relative frequency (Code)`)) %>%
  arrange(`Category Frequency`, `Relative frequency (Code)`)

codes_int_freq_na <- codes_int_freq_na %>%
  mutate(Code = factor(Code, levels = (arrange_na_codes$Code)))

red_pal <- colorRampPalette(brewer.pal(9, "Reds"))(11)

int_freq_na <- ggplot(codes_int_freq_na, aes(x = Code, y = `Relative frequency (Intensity)`, fill = Intensity)) +
  facet_grid(rows = vars(`Category labels`),
             scales = "free",
             space = "free",
             axes = "all_x",
             axis.labels = "all_x") +
  scale_fill_manual(values = red_pal, guide = guide_legend(nrow = 1), breaks = c(0:10))+
  geom_col() +
  geom_text(data = arrange_na_codes, aes(x = Code, y = `Relative frequency (Code)`, label = paste0(round(`Relative frequency (Code)`, 2), "%")),
            hjust = -0.05,
            inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, .10))) +
  coord_flip(clip = "off") +
  theme_minimal() +
  labs(x = "Response Process",
       y = "Proportion (%)",
       title = "Proportional Occurrence of Response Processes for NA",
       subtitle = "Percentage of all responses where a negative item was asked (N = 2.502) in which a code was applied ") +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(size = 12, angle = 0, face = "bold", hjust = 0),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "gray95", color = NA),
        legend.position = "top", legend.key.spacing.x = unit(1, "pt"),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# Density Distribution

hist_na <- RP_data %>%
  filter(!is.na(RP_emotion), RP_emotion %in% c("irritated", "nervous", "down")) %>%
  mutate(intensity = case_when(RP_emotion == "irritated" ~ irritated,
                               RP_emotion == "nervous" ~ nervous,
                               RP_emotion == "down" ~ down,
                               .default = NA),
         RP_emotion = factor(RP_emotion, levels = c("irritated", "nervous", "down"))) %>%
  filter(!is.na(intensity)) %>%
  count(RP_emotion, intensity, name = "n") %>%
  group_by(RP_emotion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(intensity = factor(intensity, levels = rev(0:10))) %>%
  ggplot(aes(x = intensity, y = prop, fill = intensity, pattern = RP_emotion)) +
  geom_col_pattern(aes(pattern = RP_emotion), position = position_dodge2(preserve = "single"), color = NA, pattern_fill = "gray", pattern_density = 0.15, pattern_spacing = 0.1, pattern_angle = 45) +
  scale_pattern_manual(values = c(irritated = "stripe", nervous = "none", down = "circle"), name = "Emotion") +
  scale_fill_manual(values = red_pal, guide = guide_legend(nrow = 1), breaks = c(0:10))+
  guides(fill="none") +
  scale_x_discrete(limits = as.character(0:10), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        legend.position = "right") +
  labs(x = "Intensity", y = "Proportion", title = "")

combined_plot_na <- int_freq_na / hist_na +
  plot_layout(heights = c(5, 1))

combined_plot_na

ggsave(file.path(figures_dir, "RQ2b - Intensities/Relative Frequency Plot with Intensities NA.png"), combined_plot_na,
       width = 17,
       height = 10,
       units = "in",
       dpi = 600)

# ---- RQ3: Consistency Between People ----
# Third, we explored the differences in response processes between individuals (i.e., heterogeneity between participants).
# Compute Relative Frequency of Each Response Process Across Occasion per Person

rp_count_codes <- RP_data %>%
  filter(if_any(rp_cols, ~ !is.na(.))) %>%
  select("participant_ID", "obsno", all_of(rp_cols)) %>%
  mutate(rp_count = rowSums(across(all_of(rp_cols)), na.rm = TRUE), .after = "obsno") %>%
  group_by(participant_ID) %>%
  summarise(observations = n(),
            average_rp_per_observation = mean(rp_count),
            across(all_of(rp_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

rp_count_bp_long <- rp_count_codes %>%
  pivot_longer(cols = all_of(rp_cols),
               names_to = "response_process",
               values_to = "count") %>%
  group_by(participant_ID) %>%
  mutate(Total = sum(count, na.rm = TRUE),
         relative = ifelse(count > 0, count / Total, NA),
         response_process = factor(response_process, levels = rel_freq_df$Code)) %>%
  ungroup() %>%
  mutate(participant_ID = forcats::fct_reorder(factor(participant_ID), desc(average_rp_per_observation)))

ggplot(rp_count_bp_long,
       aes(x = response_process, y = participant_ID, fill = relative)) +
  geom_tile() +
  scale_fill_viridis_c(option = "F", direction = -1, trans = "sqrt",na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 50, 50)) +
  labs(fill = "Relative frequency")

ggsave(file.path(figures_dir, "RQ3 - Between Person Consistency/Heatmap RP Between Persons.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

# Compute Person Level Response Process Count Per Occasion Across Response Processes Codes
rp_count_wp <- RP_data %>%
  filter(if_any(all_of(rp_cols), ~ !is.na(.))) %>%
  group_by(participant_ID, obsno) %>%
  summarise(across(all_of(rp_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(rp_count = rowSums(across(all_of(rp_cols))))

# Get summary statistics of occasion level response process count per person
rp_count_bp <- rp_count_wp %>%
  group_by(participant_ID) %>%
  summarise(mean_rp_count = mean(rp_count),
            min_rp_count = min(rp_count),
            max_rp_count = max(rp_count),
            sum_rp_count = sum(rp_count),
            .groups = "drop")

p_rp_count <- ggplot(
  rp_count_wp,
  aes(x = participant_ID,
      y = rp_count,
      fill = participant_ID,
      color = participant_ID)) +
  geom_violin(alpha = .4) +
  geom_boxplot(width = .15, linewidth = 0.8, alpha = .8, outlier.shape = NA) +
  stat_summary(fun = mean,
               geom = "crossbar",
               width = .15,
               colour = "grey90",
               linewidth = 0.7) +
  scale_y_continuous(breaks = breaks_pretty()) +
  geom_jitter(alpha = 0.25, width = 0.1, height = 0.1, size = 1) +
  scale_color_viridis_d(option = "F", direction = -1, begin = 0, end = .8) +
  scale_fill_viridis_d(option = "F", direction = -1, begin = 0, end = .8) +
  theme_minimal() +
  labs(x = "Participants", y = "Response Process Count Per Occasion") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot = p_rp_count,
  file.path(figures_dir, "Supporting figures/distribution_rp_between_persons.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

# Compute Word Counts For Open-Text Responses Per Occasion Across Response Process Codes
word_count_wp <- RP_data %>%
  select(participant_ID, obsno, happy_RP:nervous_RP) %>%
  mutate(open_text_response = iconv(
    !!quote(coalesce(!!!across(happy_RP:nervous_RP))),
    from = "",
    to = "UTF-8",
    sub = ""),
    open_text_word_count = if_else(is.na(open_text_response), NA, count_words(open_text_response)))

word_count_bp <- word_count_wp %>%
  group_by(participant_ID) %>%
  summarise(mean_word_count = mean(open_text_word_count, na.rm = T),
    min_word_count = min(open_text_word_count, na.rm = T),
    max_word_count = max(open_text_word_count, na.rm = T),
    sum_word_count = sum(open_text_word_count, na.rm = T),
    .groups = "drop")

p_word_count <- ggplot(word_count_wp,
  aes(x = participant_ID,
      y = open_text_word_count,
      fill = participant_ID,
      color = participant_ID)) +
  geom_violin(alpha = .4) +
  geom_boxplot(width = .15, linewidth = 0.8, alpha = .8, outlier.shape = NA) +
  geom_jitter(alpha = 0.25, width = 0.1, height = 0.1, size = 1) +
  stat_summary(fun = mean,
               geom = "crossbar",
               width = .15,
               colour = "grey90",
               linewidth = 0.7) +
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_color_viridis_d(option = "F", direction = -1, begin = 0, end = .8) +
  scale_fill_viridis_d(option = "F", direction = -1, begin = 0, end = .8) +
  theme_minimal() +
  labs(x = " ", y = "Word Count Per Occasion") +
  theme(legend.position = "none",
        axis.text.x = element_blank())

ggsave(plot = p_word_count,
  file.path(figures_dir,"Supporting figures/distribution_word_count_between_persons.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

counts_plot <- ((p_word_count + theme(axis.title.x = element_blank()))/ (p_rp_count + theme(axis.title.x = element_blank())))/
  patchwork::wrap_elements(grid::textGrob("Participant ID")) +
  plot_layout(heights = c(7, 7, 0.3))  +
  plot_annotation(title = "Within Person Distribution of Word Count and Response Process Codes")

ggsave(plot = counts_plot,
  file.path(figures_dir,"Supporting figures/distribution_word_rp_count_combined.png"),
  width = 16,
  height = 14,
  units = "in",
  dpi = 150)

## ---- Correlation Word Count RP Count ----
bp_word_rp_count <- rp_count_bp %>%
  left_join(word_count_bp, by = "participant_ID", "obsno")

bp_word_rp_count %>% summary()

# corr between average word count and average rp count
cor.test(bp_word_rp_count$mean_rp_count, bp_word_rp_count$mean_word_count)

wp_word_rp_count <- rp_count_wp %>%
  select(participant_ID, obsno, rp_count) %>%
  left_join(word_count_wp %>% select(participant_ID, obsno, open_text_word_count),
            by = c("participant_ID", "obsno")) %>%
  group_by(participant_ID) %>%
  mutate(r = cor(rp_count, open_text_word_count, use = "complete.obs"))

# average within
summary_wp_cor <- wp_word_rp_count %>%
  ungroup() %>%
  summarise(mean_r_wp_word_rp = mean(r, na.rm = TRUE),
    sd_r_wp_word_rp = sd(r, na.rm = TRUE),
    min_r_wp_word_rp = min(r, na.rm = TRUE),
    max_n_r_wp_word_rp = max(r, na.rm = TRUE)  )

cor.test(wp_word_rp_count$open_text_word_count,
         wp_word_rp_count$rp_count,
         use = "complete.obs")

ggplot(wp_word_rp_count,
       aes(x = open_text_word_count, y = rp_count, colour = participant_ID)) +
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.2, alpha = .05) +
  annotate("text", x = 300, y = 2, hjust = 1.1, vjust = 1.5,
           label = paste0("mean r = ", round(summary_wp_cor$mean_r_wp_word_rp, 2),
                          "\np = <.001"), size = 5) +
  scale_colour_viridis_d(option = "F", begin = 0, end = .85) +
  labs(x = "Word count", y = "Response process count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

ggsave(file.path(figures_dir, "Supporting figures/cor_word_count_rp_count.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

### ---- Unique Words and Word Counts ----
q75 <- quantile(word_count_wp$open_text_word_count, .75,na.rm = TRUE)

p_word_density <- word_count_wp %>%
  filter(!is.na(open_text_word_count)) %>%
  ggplot(aes(x = open_text_word_count)) +
  geom_histogram(colour = "white") +
  geom_vline(xintercept = q75,
    linetype = "dashed",
    colour = "grey30",
    linewidth = 1) +
  annotate("text", x = q75, y = Inf,
    label = paste0("75th percentile = ", round(q75)),
    angle = 90, vjust = 1.4, hjust = 1.1, colour = "grey30") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  theme_minimal() +
  labs(x = "Word Count Per Open-Text Response",
    y = "Density",
    title = "Distribution of Open-Text Response Length")

word_count_wp <- word_count_wp %>%
  mutate(clean_text = open_text_response %>%
  tolower() %>% stringr::str_replace_all("[[:punct:]]", "") %>% stringr::str_squish(),
  unique_word_count = if_else(is.na(clean_text), NA,
                      sapply(strsplit(clean_text, "\\s+"),
                    function(x) length(unique(x)))))
ggsave(plot = p_word_density,
  file.path(figures_dir, "Supporting figures/word_count.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

p_unique_words <- ggplot(word_count_wp,
  aes(x = open_text_word_count,
    y = unique_word_count)) +
  geom_point(alpha = .25,
    size = 1.5) +
  geom_smooth(method = "loess",
    se = TRUE,
    colour = "grey30",
    linewidth = 1) +
  theme_minimal() +
  labs(x = "Total Word Count",
    y = "Number of Unique Words",
    title = "Unique Words by Open-Text Response Length")

ggsave(plot = p_unique_words,
  file.path(figures_dir, "Supporting figures/unique_word_count_word_diversity.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

p_word_density /p_unique_words + plot_layout(heights = c(7, 7, 0.3))

ggsave(file.path(figures_dir, "Supporting figures/word_count_word_diversity_combined.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

# ---- RQ4: Consistency Within People ----
consistency_data_valid <- RP_data %>%
  mutate(assessment_date = as.Date(timeStampSent),
         interview_date  = as.Date(cog_interview_date_day15)) %>%
  filter(!is.na(interview_date), !is.na(RP_emotion)) %>% # the participants who did not have an interview were removed from the analyses
  mutate(period = factor(ifelse(assessment_date < interview_date, "Before", "After"),
                         levels = c("Before", "After")),
         PA = rowSums(across(c(happy, relaxed)), na.rm = TRUE),
         `NA` = rowSums(across(c(nervous, irritated, down)), na.rm = TRUE),
         `RP Count` = rowSums(across(all_of(rp_cols)), na.rm = TRUE))

occasion_rp_relative <- consistency_data_valid %>%
  select(participant_ID, period, all_of(rp_cols)) %>%
  pivot_longer(cols = all_of(rp_cols), names_to = "Code", values_to = "coded") %>%
  group_by(participant_ID, period, Code) %>%
  summarise(coded = sum(coded, na.rm = TRUE), .groups = "drop") %>%
  group_by(participant_ID, period) %>%
  mutate(total_rp_period = sum(coded, na.rm = TRUE),
         relative = if_else(total_rp_period > 0, coded / total_rp_period, 0)) %>%
  ungroup()

# Average RP Count and emotion intensity per occasion
occasion_averages <- consistency_data_valid %>%
  select(participant_ID, period, `RP Count`, PA, `NA`) %>%
  pivot_longer(cols = c(`RP Count`, PA, `NA`), names_to = "Code", values_to = "coded") %>%
  group_by(participant_ID, period, Code) %>%
  summarise(relative = mean(coded, na.rm = TRUE),.groups = "drop")

# Combine
participant_long_codes <- bind_rows(occasion_rp_relative %>% select(participant_ID, period, Code, relative),
  occasion_averages %>% select(participant_ID, period, Code, relative)
) %>%
  mutate(Code = factor(Code, levels = c("RP Count", "PA", "NA",rev(as.character(rel_freq_df$Code)))))


rp_summary_codes <- participant_long_codes %>%
  group_by(period, Code) %>%
  summarise(avg_relative = mean(relative, na.rm = TRUE), .groups = "drop")

before_after_codes <- ggplot() +
  geom_tile(data = rp_summary_codes %>% filter(Code %in% c("RP Count", "PA", "NA")), aes(x = period, y = Code, fill = avg_relative), color = "white") +
  scale_fill_viridis_c(option = "F", direction = -1, name = "Average") +
  new_scale_fill() +
  geom_tile(data = rp_summary_codes %>% filter(!Code %in% c("RP Count", "PA", "NA")),aes(x = period, y = Code, fill = avg_relative), color = "white") + scale_fill_viridis_c(option = "F",direction = -1,name = "Average relative\nfrequency")+
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Response process", fill = "Average use",
       title = "Response process usage before vs after cognitive interview")

before_after_codes

ggsave(file.path(figures_dir, "RQ4 - Within Person Consistency/Average use Before After with RP PA.png"),
       width = 12,
       height = 16,
       units = "in",
       dpi = 150)

# relative code frequency change
code_change <- participant_long_codes %>%
  select(participant_ID, period, Code, relative) %>%
  pivot_wider(names_from = period, values_from = relative, values_fill = 0) %>%
  mutate(change = After - Before)

# mean rp count change
mean_rp_count_change <- consistency_data_valid %>%
  select(participant_ID, period, all_of(rp_cols)) %>%
  mutate(count_rp_occasion = rowSums(across(all_of(rp_cols)), na.rm = TRUE)) %>%
  group_by(participant_ID, period) %>%
  summarise(mean_count = mean(count_rp_occasion, na.rm = TRUE), .groups = "drop") %>%
  mutate(Code = "Mean RP Count") %>%
  pivot_wider(names_from = period, values_from = mean_count, values_fill = 0) %>%
  mutate(change = After - Before)

# pa/na mean change
pa_na_change <- consistency_data_valid %>%
  rowwise() %>%
  mutate(mean_PA = mean(c(happy, relaxed), na.rm = TRUE),
         mean_NA = mean(c(irritated, nervous, down), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(participant_ID, period) %>%
  summarise(`PA Mean` = mean(mean_PA, na.rm = TRUE),
            `NA Mean` = mean(mean_NA, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(`PA Mean`, `NA Mean`), names_to = "Code", values_to = "value") %>%
  pivot_wider(names_from = period, values_from = value, values_fill = 0) %>%
  mutate(change = After - Before)

# order "Codes" and the three additional values
reversed_codes <- rev(levels(code_change$Code))
all_code_levels <- c("Mean RP Count", "PA Mean", "NA Mean", reversed_codes)

# order participants
participant_order <- mean_rp_count_change %>%
  arrange(desc(change)) %>%
  pull(participant_ID)

code_change <- code_change %>% mutate(Code = factor(Code, levels = all_code_levels), participant_ID = factor(participant_ID, levels = participant_order))
mean_rp_count_change <- mean_rp_count_change %>% mutate(Code = factor(Code, levels = all_code_levels), participant_ID = factor(participant_ID, levels = participant_order))
pa_na_change <- pa_na_change %>% mutate(Code = factor(Code, levels = all_code_levels), participant_ID = factor(participant_ID, levels = participant_order))


heatmap_change_codes <- ggplot() +
  # Layer 1: mean change
  geom_tile(data = rbind(mean_rp_count_change, pa_na_change),
            aes(x = Code, y = participant_ID, fill = change),
            color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
                       midpoint = 0, name = "Mean \nchange") +
  geom_text(data = rbind(mean_rp_count_change, pa_na_change),
            aes(x = Code, y = participant_ID, label = round(change, 2)),
            size = 3) +
  new_scale_fill() +

  # Layer 2: relative frequency
  geom_tile(data = code_change,
            aes(x = Code, y = participant_ID, fill = change),
            color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
                       midpoint = 0, name = "Relative freq.\nchange (pp)") +
  geom_text(data = code_change,
            aes(x = Code, y = participant_ID, label = round(change, 2)),
            size = 3) +
  new_scale_fill() +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        axis.text.y = element_text(size = 12)) +
  labs(y = NULL,
       title = "Changes in Response Process Use Before and After Interview",
       subtitle = "Mean RP Count, PA/NA and Relative Frequency Change (After − Before)",
       caption= "Ordered by Change in Mean RP Count")

heatmap_change_codes

ggsave(file.path(figures_dir, "RQ4 - Within Person Consistency/Relative Change WP After-Before.png"),
       width = 16,
       height = 12,
       units = "in",
       dpi = 150)

# Categories
participant_long_categories <- consistency_data_valid %>%
  select(participant_ID, period, all_of(categories)) %>%
  pivot_longer(cols = all_of(categories), names_to = "Category", values_to = "coded") %>%
  group_by(participant_ID, period, Category) %>%
  summarise(count = sum(coded, na.rm = TRUE), .groups = "drop") %>%
  group_by(participant_ID, period) %>%
  mutate(total = sum(count, na.rm = TRUE),
         relative = ifelse(total > 0, count / total, 0)) %>%
  ungroup() %>%
  mutate(Category = factor(Category, levels = rev(unique(rel_freq_df$Category))))

rp_summary_categories <- participant_long_categories %>%
  group_by(period, Category) %>%
  summarise(avg_use = mean(relative, na.rm = TRUE), .groups = "drop")

before_after_categories <- ggplot(rp_summary_categories, aes(x = period, y = Category, fill = avg_use)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "F", direction = -1) +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Category", fill = "Average use",
       title = "Category usage before vs after cognitive interview")

before_after_categories

ggsave(file.path(figures_dir, "Supporting figures/avg_before_after_categories.png"),
  width = 12,
  height = 16,
  units = "in",
  dpi = 150)


# relative category frequency change
category_change <- participant_long_categories %>%
  select(participant_ID, period, Category, relative) %>%
  pivot_wider(names_from = period, values_from = relative, values_fill = 0) %>%
  mutate(change = After - Before)

# mean category count change
mean_category_count_change <- consistency_data_valid %>%
  select(participant_ID, period, all_of(categories)) %>%
  mutate(count_category_occasion = rowSums(across(all_of(categories)), na.rm = TRUE)) %>%
  group_by(participant_ID, period) %>%
  summarise(mean_category_count = mean(count_category_occasion, na.rm = TRUE), .groups = "drop") %>%
  mutate(Category = "Mean Category Count") %>%
  pivot_wider(names_from = period, values_from = mean_category_count, values_fill = 0) %>%
  mutate(change = After - Before)

# order "Codes" and the three additional values
reversed_categories <- rev(levels(category_change$Category))
all_category_levels <- c("Mean Category Count", "PA Mean", "NA Mean", reversed_categories)

# order participants
participant_order_cat <- mean_category_count_change %>%
  arrange(desc(change)) %>%
  pull(participant_ID)

category_change <- category_change %>% mutate(Category = factor(Category, levels = all_category_levels), participant_ID = factor(participant_ID, levels = participant_order_cat))
mean_category_count_change <- mean_category_count_change %>% mutate(Category = factor(Category, levels = all_category_levels), participant_ID = factor(participant_ID, levels = participant_order_cat))
pa_na_change_cat <- pa_na_change %>% mutate(Code = factor(Code, levels = all_category_levels), participant_ID = factor(participant_ID, levels = participant_order_cat)) %>% rename(Category = Code)

heatmap_change_categories <- ggplot() +
  # Layer 1: mean change
  geom_tile(data = rbind(mean_category_count_change, pa_na_change_cat),
            aes(x = Category, y = participant_ID, fill = change),
            color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
                       midpoint = 0, name = "Mean \nchange") +
  geom_text(data = rbind(mean_category_count_change,pa_na_change_cat),
            aes(x = Category, y = participant_ID, label = round(change, 2)),
            size = 3) +
  new_scale_fill() +

  # Layer 2: relative frequency
  geom_tile(data = category_change,
            aes(x = Category, y = participant_ID, fill = change),
            color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
                       midpoint = 0, name = "Relative freq.\nchange (pp)") +
  geom_text(data = category_change,
            aes(x = Category, y = participant_ID, label = round(change, 2)),
            size = 3) +
  new_scale_fill() +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 9)) +
  labs(y = NULL,
       title = "Changes in Response Process Use Before and After Interview",
       subtitle = "Mean RP Count, PA/NA, and Relative Frequency Change (After − Before)",
       caption= "Ordered by Change in Mean Category Count")


heatmap_change_categories

ggsave(file.path(figures_dir,"Supporting figures/rel_change_wp_after-before_ctegories.png"),
  width = 16,
  height = 12,
  units = "in",
  dpi = 150)

## ---- Correlation ----
mean_rp_pa_na_changes <-
  rbind(mean_rp_count_change, pa_na_change) %>%
  select(-c(Before, After)) %>%
  pivot_wider(names_from = "Code", values_from = "change")

mean_rp_pa_na_changes %>% summary()

# corr between average word count and average rp count
cor_rp_pa_change_test = cor.test(mean_rp_pa_na_changes$`Mean RP Count`, mean_rp_pa_na_changes$`PA Mean`)
cor_rp_na_change_test = cor.test(mean_rp_pa_na_changes$`Mean RP Count`, mean_rp_pa_na_changes$`NA Mean`)

mean_rp_count_change %>% select(-c(Before, After)) %>%
  left_join(pa_na_change %>% select(-c(Before, After)), by = "participant_ID") %>%
  rename(Valence = Code.y, `Mean Change` = change.x, `Intensity Change` = change.y) %>%
  ggplot(
    aes(x = `Mean Change`, y = `Intensity Change`, colour = Valence)) +
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = .1) +
  annotate("text", x = 1, y = 2, hjust = 1.1, vjust = 1.5,
           label = paste0("NA - Word Count \nr = ", round(cor_rp_na_change_test$estimate, 2),
                          "\np = <.001"), size = 5) +
  scale_colour_viridis_d(option = "F", begin = 0, end = .85) +
  labs(x = "Change in Mean RP Count (After - Before)", y = "Change in Intensity (After - Before)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(figures_dir, "Supporting figures/intensity_rp_change_correlation.png"),
  width = 16,
 height = 12,
 units = "in",
 dpi = 150)

## ---- RQ4 Supplement: Individual plots ----
  # Lastly, we explored whether individuals change or stay consistent in their response processes over time (i.e., intra-individual change).

participant <- 1707460

participant_raw <- RP_data %>%
    filter(participant_ID == participant, daycum > 4) %>%
    select(obsno, timeStampSent, valid, cog_interview_date_day15, all_of(rp_cols), happy, relaxed, irritated, nervous, down)

participant_data <- participant_raw %>%
    mutate(week = as.integer(floor(difftime(timeStampSent, min(timeStampSent, na.rm = TRUE), units = "days") / 7)) + 1) %>%
    fill(week, .direction = "downup") %>%
    fill(cog_interview_date_day15, .direction = "downup")


participant_long <- participant_data  %>%
    pivot_longer(cols = all_of(rp_cols), names_to = "response_process", values_to = "coded") %>%
    mutate(
      response_process = factor(response_process, levels = rev(rp_cols)),
      category = case_when(response_process %in% comparison_standards~ "Comparison Standards",
                           response_process %in% internal_processes ~ "Internal Processes",
                           response_process %in% evidence_collection ~ "Evidence Collection",
                           response_process %in% time_evidence ~ "Time Evidence",
                           TRUE ~ "Other Codes"),
      fill_val = case_when(coded == 1 ~ as.character(response_process), TRUE ~ "unused"),
      border_val = ifelse(fill_val == "unused", NA, "white"),
      category = factor(category, levels = category_order)
    )

process_colors <- participant_long %>%
    distinct(response_process, category) %>%
    arrange(category, response_process) %>%
    group_by(category) %>%
    mutate(color = viridis(n(), option = category_pal[as.character(category)[1]], begin = 0.1, end = 0.9)) %>%
    ungroup()

category_fills <- setNames(process_colors$color, process_colors$response_process)

# interview time point
interview_obsno <- participant_long %>%
    filter(as.Date(timeStampSent) <= cog_interview_date_day15) %>%
    summarise(interview_obsno = max(obsno) + 0.5) %>%
    pull(interview_obsno)

# week information
week_bounds <- participant_long %>%
    distinct(obsno, week) %>%
    group_by(week) %>%
    summarise(xmin = min(obsno) - 0.5, xmax = max(obsno) + 0.5, .groups = "drop") %>%
    mutate(shade = factor(week %% 2))

week_mids <- week_bounds %>% mutate(mid = (xmin + xmax) / 2)

all_obs <- seq(min(participant_long$obsno), max(participant_long$obsno), by = 1)

missing_obs <- participant_long %>% filter(valid == 0) %>% distinct(obsno)

# geom_objects
week_shading <- geom_rect(data = week_bounds, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade),
                              inherit.aes = FALSE, alpha = 0.5)

week_scale <- scale_fill_manual(values = c("0" = "grey80", "1" = "white"), guide = "none")

obs_gridlines <- geom_vline(xintercept = all_obs, color = "grey92", linewidth = 0.15)

interview_line <- geom_vline(xintercept = interview_obsno, linetype = "dashed", color = "black", linewidth = 0.6)

missing_shading <- geom_rect(data = missing_obs, aes(xmin = obsno - 0.5, xmax = obsno + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "lightblue", alpha = 0.5)

shared_xlim <- c(min(all_obs) - 1, max(all_obs) + 1)

tile_p <- participant_long %>%
    ggplot() +
    missing_shading + week_shading + week_scale + interview_line + obs_gridlines +
    new_scale_fill() +
    geom_tile(aes(x = obsno, y = response_process, fill = fill_val, color = border_val), linewidth = 0.2) +
    scale_fill_manual(values = c(category_fills, "unused" = NA), na.value = NA) +
    scale_color_identity() +
    scale_x_continuous(limits = shared_xlim, expand = expansion(add = 0.5),sec.axis = dup_axis(breaks = week_mids$mid, labels = paste0("Wk ", week_mids$week), name = NULL)) +
    facet_grid(category ~ ., scales = "free_y", space = "free_y") +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
      panel.spacing.y = unit(0.3, "lines"),
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = NA),
      legend.position = "none",
      axis.text.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      axis.text.x.top = element_text(face = "bold", size = 9))

time_p <- participant_data %>%
    distinct(obsno, week, happy, down, irritated, relaxed, nervous) %>%
    pivot_longer(cols = c(happy, relaxed, irritated, nervous, down), names_to = "emotion", values_to = "intensity") %>%
    mutate(emotion = factor(emotion, levels = c("happy", "relaxed", "irritated", "nervous", "down"))) %>%
    ggplot() +
    missing_shading + week_shading + week_scale + interview_line +  obs_gridlines +
    geom_line(aes(x = obsno, y = intensity, color = emotion), linewidth = 0.85) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(limits = shared_xlim, expand = expansion(add = 0.5))+
    scale_color_manual(values = c("#283593", "#1565C0", "#e63e00", "#8E0038", "#BD5579"), name = "Emotion") +
    labs(x = "Observation", y = "Emotion intensity") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
      legend.position = "bottom")

tile_p / time_p +
    plot_layout(heights = c(5, 1)) +
    plot_annotation(title = paste0("Response Process Use Over Time Participant ", participant))

process_colors <- tibble(response_process = factor(rp_cols, levels = rev(rp_cols))) %>%
    mutate(category = case_when(
        response_process %in% str_to_title(str_replace_all(comparison_standards, "_", " ")) ~ "Comparison Standards",
        response_process %in% str_to_title(str_replace_all(internal_processes, "_", " ")) ~ "Internal Processes",
        response_process %in% str_to_title(str_replace_all(evidence_collection, "_", " ")) ~ "Evidence Collection",
        response_process %in% str_to_title(str_replace_all(time_evidence, "_", " ")) ~ "Time Evidence",
        TRUE ~ "Other Codes")) %>%
    arrange(category, response_process) %>%
    group_by(category) %>%
    mutate(color = viridis(n(), option = category_pal[as.character(category)[1]], begin = 0.1, end = 0.9)) %>%
    ungroup()

category_fills <- setNames(process_colors$color, as.character(process_colors$response_process))

pids <- unique(RP_data$participant_ID)

for (i in pids) {
    participant <- i

    participant_raw <- RP_data %>%
      filter(participant_ID == participant, daycum > 4) %>%
      select(obsno, timeStampSent, valid, cog_interview_date_day15, all_of(rp_cols), happy, relaxed, irritated, nervous, down)

    participant_data <- participant_raw %>%
      mutate(week = as.integer(floor(difftime(timeStampSent, min(timeStampSent, na.rm = TRUE), units = "days") / 7)) + 1) %>%
      fill(week, .direction = "downup") %>%
      fill(cog_interview_date_day15, .direction = "downup")


    participant_long <- participant_data  %>%
      pivot_longer(cols = all_of(rp_cols), names_to = "response_process", values_to = "coded") %>%
      mutate(response_process = factor(response_process, levels = rev(rp_cols)),
        category = case_when(response_process %in% str_to_title(str_replace_all(comparison_standards, "_", " ")) ~ "Comparison Standards",
                             response_process %in% str_to_title(str_replace_all(internal_processes, "_", " ")) ~ "Internal Processes",
                             response_process %in% str_to_title(str_replace_all(evidence_collection, "_", " ")) ~ "Evidence Collection",
                             response_process %in% str_to_title(str_replace_all(time_evidence, "_", " ")) ~ "Time Evidence",
                             TRUE ~ "Other Codes"),
        fill_val = case_when(coded == 1 ~ as.character(response_process), TRUE ~ "unused"),
        border_val = ifelse(fill_val == "unused", NA, "white"),
        category = factor(category, levels = category_order))

    # interview time point
    interview_obsno <- participant_long %>%
      filter(as.Date(timeStampSent) <= cog_interview_date_day15) %>%
      summarise(interview_obsno = max(obsno) + 0.5) %>%
      pull(interview_obsno)

    # week information
    week_bounds <- participant_long %>%
      distinct(obsno, week) %>%
      group_by(week) %>%
      summarise(xmin = min(obsno) - 0.5, xmax = max(obsno) + 0.5, .groups = "drop") %>%
      mutate(shade = factor(week %% 2))

    week_mids <- week_bounds %>% mutate(mid = (xmin + xmax) / 2)

    all_obs <- seq(min(participant_long$obsno), max(participant_long$obsno), by = 1)

    missing_obs <- participant_long %>% filter(valid == 0) %>% distinct(obsno)

    # geom_objects
    week_shading <- geom_rect(data = week_bounds, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade),
                                inherit.aes = FALSE, alpha = 0.5)
    week_scale <- scale_fill_manual(values = c("0" = "grey80", "1" = "white"), guide = "none")

    obs_gridlines <- geom_vline(xintercept = all_obs, color = "grey92", linewidth = 0.15)

    interview_line <- geom_vline(xintercept = interview_obsno, linetype = "dashed", color = "black", linewidth = 0.6)

    missing_shading <- geom_rect(data = missing_obs, aes(xmin = obsno - 0.5, xmax = obsno + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "lightblue", alpha = 0.5)

    shared_xlim <- c(min(all_obs) - 1, max(all_obs) + 1)

    tile_p <- participant_long %>%
      ggplot() +
      missing_shading + week_shading + week_scale + interview_line + obs_gridlines +
      ggnewscale::new_scale_fill() +
      geom_tile(aes(x = obsno, y = response_process, fill = fill_val, color = border_val), linewidth = 0.2) +
      scale_fill_manual(values = c(category_fills, "unused" = NA), na.value = NA) +
      scale_color_identity() +
      scale_x_continuous(limits = shared_xlim, expand = expansion(add = 0.5),sec.axis = dup_axis(breaks = week_mids$mid, labels = paste0("Wk ", week_mids$week), name = NULL)) +
      facet_grid(category ~ ., scales = "free_y", space = "free_y") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
        panel.spacing.y = unit(0.3, "lines"),
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA),
        legend.position = "none",
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.text.x.top = element_text(face = "bold", size = 9))

    time_p <- participant_data %>%
      distinct(obsno, week, happy, relaxed, irritated, nervous, down) %>%
      pivot_longer(cols = c(happy, relaxed, irritated, nervous, down), names_to = "emotion", values_to = "intensity") %>%
      mutate(emotion = factor(emotion, levels = c("happy", "relaxed", "irritated", "nervous", "down"))) %>%
      ggplot() +
      missing_shading + week_shading + week_scale + interview_line +  obs_gridlines +
      geom_line(aes(x = obsno, y = intensity, color = emotion), linewidth = 0.85) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      scale_x_continuous(limits = shared_xlim, expand = expansion(add = 0.5))+
      scale_color_manual(values = c("#283593", "#1565C0", "#e63e00", "#8E0038", "#BD5579"), name = "Emotion") +
      labs(x = "Observation", y = "Emotion intensity") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
        legend.position = "bottom")

    combined_plot <- tile_p / time_p +
      plot_layout(heights = c(5, 1)) +
      plot_annotation(title = paste0(
          "Response Process Use Over Time\nParticipant ", participant))

    ggsave(filename = file.path(figures_dir,
           "Supporting figures/RQ4 - Individual Plots Across time",
            paste0("Participant_", participant, ".png")),
      plot = combined_plot,
      width = 16,
      height = 12,
      units = "in",
      dpi = 150)
  }
