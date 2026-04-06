

# Import Libraries

library(haven)
library(tidyverse)
library(data.table)
library(lubridate)


# Lixipark .sas tables

vs <- read_sas("../data/vs.sas7bdat")
dm <- read_sas("../data/dm.sas7bdat")
sc <- read_sas("../data/sc.sas7bdat")
ra <- read_sas("../data/ra.sas7bdat")
cm <- read_sas("../data/cm.sas7bdat")
pt <- read_sas("../data/pt.sas7bdat")

groupe <- read_sas("../data/groupe.sas7bdat")

# Group labels

groupe %>% group_by(RAN_GRP_LIB) %>% count() # 78 * 78


# Modified UPDRS Scales
# ignore NAs using  rowSums(across(c(x,y,x)), na.rm=T)
# tremor mod, non_tremor mod, eleven_item, partIII original

names(sc)

sc <- sc %>%
  mutate(tremor_mod_scale = rowSums(across(c(SCMDS2_10, SCMDS3_15A, SCMDS3_15B, 
                                             SCMDS3_16A, SCMDS3_16B, SCMDS3_17A, SCMDS3_17B, 
                                             SCMDS3_17C, SCMDS3_17D, SCMDS3_17E, SCMDS3_18)), 
                                    na.rm = TRUE)) %>%
  mutate(non_tremor_mod_scale = rowSums(across(c(SCMDS2_1, SCMDS2_2, SCMDS2_3, 
                                                 SCMDS2_4, SCMDS2_5, SCMDS2_6, SCMDS2_7, 
                                                 SCMDS2_8, SCMDS2_9, SCMDS2_11, SCMDS2_12, SCMDS2_13,
                                                 SCMDS3_1, SCMDS3_2, SCMDS3_3A, SCMDS3_3B, SCMDS3_3C,
                                                 SCMDS3_3D, SCMDS3_3E, SCMDS3_4A, SCMDS3_4B, SCMDS3_5A, SCMDS3_5B, 
                                                 SCMDS3_6A, SCMDS3_6B, SCMDS3_7A, SCMDS3_7B, SCMDS3_8A, SCMDS3_8B, 
                                                 SCMDS3_9, SCMDS3_10, SCMDS3_11, SCMDS3_12, SCMDS3_13, SCMDS3_14)), 
                                        na.rm = TRUE)) %>%
  mutate(eleven_item_mod_scale = rowSums(across(c(SCMDS1_13, SCMDS2_1, SCMDS2_4, SCMDS2_5, 
                                                  SCMDS2_6, SCMDS2_7, SCMDS2_8, SCMDS2_9,
                                                  SCMDS2_11, SCMDS2_12, SCMDS2_13)), 
                                         na.rm = TRUE)) %>%
  mutate(mds_partIII_total = rowSums(across(c(SCMDS3_1, SCMDS3_2, 
                                              SCMDS3_3A, SCMDS3_3B, SCMDS3_3C, SCMDS3_3D, SCMDS3_3E,
                                              SCMDS3_4A, SCMDS3_4B, 
                                              SCMDS3_5A, SCMDS3_5B, 
                                              SCMDS3_6A, SCMDS3_6B, 
                                              SCMDS3_7A, SCMDS3_7B, 
                                              SCMDS3_8A, SCMDS3_8B, 
                                              SCMDS3_9, SCMDS3_10, SCMDS3_11, SCMDS3_12, SCMDS3_13, SCMDS3_14,
                                              SCMDS3_15A, SCMDS3_15B, 
                                              SCMDS3_16A, SCMDS3_16B, 
                                              SCMDS3_17A, SCMDS3_17B, SCMDS3_17C, SCMDS3_17D, SCMDS3_17E,
                                              SCMDS3_18)), 
                                     na.rm = TRUE))


# count NAs using
# rowSums( is.na( across( c(x,y,z) ) ) )

sc <- sc %>%
  mutate(tremor_items_missing = rowSums(is.na(across(c(SCMDS2_10, SCMDS3_15A, SCMDS3_15B, 
                                                       SCMDS3_16A, SCMDS3_16B, SCMDS3_17A, SCMDS3_17B, 
                                                       SCMDS3_17C, SCMDS3_17D, SCMDS3_17E, SCMDS3_18)))),
         non_tremor_items_missing = rowSums(is.na(across(c(SCMDS2_1, SCMDS2_2, SCMDS2_3, 
                                                           SCMDS2_4, SCMDS2_5, SCMDS2_6, SCMDS2_7, 
                                                           SCMDS2_8, SCMDS2_9, SCMDS2_11, SCMDS2_12, SCMDS2_13,
                                                           SCMDS3_1, SCMDS3_2, SCMDS3_3A, SCMDS3_3B, SCMDS3_3C,
                                                           SCMDS3_3D, SCMDS3_3E, SCMDS3_4A, SCMDS3_4B, SCMDS3_5A, SCMDS3_5B, 
                                                           SCMDS3_6A, SCMDS3_6B, SCMDS3_7A, SCMDS3_7B, SCMDS3_8A, SCMDS3_8B, 
                                                           SCMDS3_9, SCMDS3_10, SCMDS3_11, SCMDS3_12, SCMDS3_13, SCMDS3_14)))),
         eleven_item_missing = rowSums(is.na(across(c(SCMDS1_13, SCMDS2_1, SCMDS2_4, SCMDS2_5, 
                                                      SCMDS2_6, SCMDS2_7, SCMDS2_8, SCMDS2_9,
                                                      SCMDS2_11, SCMDS2_12, SCMDS2_13)))),
         mds_partIII_missing = rowSums(is.na(across(c(SCMDS3_1, SCMDS3_2, 
                                                      SCMDS3_3A, SCMDS3_3B, SCMDS3_3C, SCMDS3_3D, SCMDS3_3E,
                                                      SCMDS3_4A, SCMDS3_4B, 
                                                      SCMDS3_5A, SCMDS3_5B, 
                                                      SCMDS3_6A, SCMDS3_6B, 
                                                      SCMDS3_7A, SCMDS3_7B, 
                                                      SCMDS3_8A, SCMDS3_8B, 
                                                      SCMDS3_9, SCMDS3_10, SCMDS3_11, SCMDS3_12, SCMDS3_13, SCMDS3_14,
                                                      SCMDS3_15A, SCMDS3_15B, 
                                                      SCMDS3_16A, SCMDS3_16B, 
                                                      SCMDS3_17A, SCMDS3_17B, SCMDS3_17C, SCMDS3_17D, SCMDS3_17E,
                                                      SCMDS3_18)))))



eleven_item_scale <- sc %>% filter(VISITNAM %in% c("Visit 2 - Baseline", "Visit 6 - Follow-up", "Visit 8 - Follow-up")) %>%
  select(USUBJID, VISITNUM, VISITNAM, SCMDS1_13, SCMDS2_1, SCMDS2_4, SCMDS2_5, 
         SCMDS2_6, SCMDS2_7, SCMDS2_8, SCMDS2_9,
         SCMDS2_11, SCMDS2_12, SCMDS2_13,
         eleven_item_missing, eleven_item_mod_scale)


eleven_item_scale <- eleven_item_scale %>% filter(eleven_item_missing!=11) %>% select(-eleven_item_missing)

eleven_item_scale <- eleven_item_scale %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID")


# Define the 11 items (excluding the total score)
eleven_items <- c("SCMDS1_13", "SCMDS2_1", "SCMDS2_4", "SCMDS2_5", 
                  "SCMDS2_6", "SCMDS2_7", "SCMDS2_8", "SCMDS2_9", 
                  "SCMDS2_11", "SCMDS2_12", "SCMDS2_13")

eleven_items_long <- eleven_item_scale %>%
  select(USUBJID, RAN_GRP_LIB, VISITNUM, VISITNAM, all_of(eleven_items)) %>%
  pivot_longer(
    cols = all_of(eleven_items),
    names_to = "item",
    values_to = "score"
  ) %>%
  mutate(
    item_label = case_when(
      item == "SCMDS1_13" ~ "1.13 Fatigue",
      item == "SCMDS2_1" ~ "2.1 Speech",
      item == "SCMDS2_4" ~ "2.4 Eating",
      item == "SCMDS2_5" ~ "2.5 Dressing",
      item == "SCMDS2_6" ~ "2.6 Hygiene",
      item == "SCMDS2_7" ~ "2.7 Handwriting",
      item == "SCMDS2_8" ~ "2.8 Hobbies",
      item == "SCMDS2_9" ~ "2.9 Turning in bed",
      item == "SCMDS2_11" ~ "2.11 Getting up",
      item == "SCMDS2_12" ~ "2.12 Walking",
      item == "SCMDS2_13" ~ "2.13 Freezing"
    ),
    visit_num = case_when(
      VISITNAM == "Visit 2 - Baseline" ~ 0,
      VISITNAM == "Visit 6 - Follow-up" ~ 6,
      VISITNAM == "Visit 8 - Follow-up" ~ 12
    ),
    treatment = ifelse(RAN_GRP_LIB == "1 - Placebo", "Placebo", "Lixisenatide")
  ) %>%
  filter(!is.na(visit_num))


eleven_total_long <- eleven_item_scale %>%
  select(USUBJID, RAN_GRP_LIB, VISITNUM, VISITNAM, eleven_item_mod_scale) %>%
  mutate(
    visit_num = case_when(
      VISITNAM == "Visit 2 - Baseline" ~ 0,
      VISITNAM == "Visit 6 - Follow-up" ~ 6,
      VISITNAM == "Visit 8 - Follow-up" ~ 12
    ),
    treatment = ifelse(RAN_GRP_LIB == "1 - Placebo", "Placebo", "Lixisenatide")
  ) %>%
  filter(!is.na(visit_num))


# 1. Spaghetti plot for TOTAL 11-Item Scale
# Sample a subset for clarity (e.g., 50 random subjects per group)
set.seed(123)
sample_subjects <- eleven_total_long %>%
  group_by(treatment) %>%
  summarise(unique_subjects = list(sample(unique(USUBJID), min(50, length(unique(USUBJID)))))) %>%
  unnest(unique_subjects) %>%
  pull(unique_subjects)

total_spaghetti <- ggplot(eleven_total_long , 
                          aes(x = visit_num, y = eleven_item_mod_scale, 
                              group = USUBJID, color = treatment)) +
  geom_line(alpha = 0.3, linewidth = 0.5) +
  geom_point(alpha = 0.5, size = 1.8, stroke=2, shape=1) +
  stat_summary(aes(group = treatment), fun = mean, geom = "line", size = 1.5,  alpha = 0.8) +
  stat_summary(aes(group = treatment), fun = mean, geom = "point", size = 5, shape=1, stroke=3, alpha = 0.8) +
  scale_color_manual(values = c("Placebo" = "#7c626c", "Lixisenatide" = "#32435d")) +
  scale_x_continuous(breaks = c(0, 6, 12)) +
  facet_wrap(~treatment) +
  labs(
    title = "11-Item Scale: Individual Trajectories",
    subtitle = "Thin lines: individual subjects | Thick lines: group means",
    x = "\n Visit",
    y = "Total Score (0-48) \n",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


print(total_spaghetti)

ggsave("../out/eleven_item_spaghetti_total.svg", total_spaghetti, width = 6, height = 5)

# 2. Spaghetti plots for INDIVIDUAL ITEMS (faceted)
# Calculate proportion of subjects with any change
item_change_stats <- eleven_items_long %>%
  group_by(USUBJID, item) %>%
  arrange(visit_num) %>%
  summarise(
    baseline_score = first(score),
    max_score = max(score, na.rm = TRUE),
    any_change = max_score > baseline_score,
    .groups = "drop"
  ) %>%
  group_by(item) %>%
  summarise(
    n_subjects = n(),
    n_with_change = sum(any_change, na.rm = TRUE),
    pct_with_change = round(n_with_change / n_subjects * 100, 1)
  ) %>%
  arrange(desc(pct_with_change))

print(item_change_stats, n = Inf)

# Create faceted spaghetti plot for individual items
items_spaghetti <- ggplot(eleven_items_long, 
                          aes(x = visit_num, y = score, group = USUBJID, color = treatment)) +
  geom_line(alpha = 0.3, size = 0.4) +
  geom_point(alpha = 0.5, size = 1.8, stroke=2, shape=1) +
  stat_summary(aes(group = treatment), fun = mean, geom = "line", size = 1.2, alpha = 0.9) +
  facet_wrap(~item_label + treatment, ncol = 4, scales = "free_y") +
  scale_color_manual(values = c("Placebo" = "#7c626c", "Lixisenatide" = "#32435d")) +
  scale_x_continuous(breaks = c(0, 6, 12)) +
  scale_y_continuous(limits=c(0,4), breaks=seq(0,5,1)) +
  labs(
    title = "11-Item Scale: Individual Item Trajectories",
    subtitle = "Thin lines: individual subjects | Thick lines: group means",
    x = "\n Visit",
    y = "Item Score (0-4) \n",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 



print(items_spaghetti)

ggsave("../out/eleven_item_spaghetti_items.svg", items_spaghetti, width = 10, height = 14)



# 3. Heatmap of change frequencies
# Create a matrix showing which subjects changed on which items
change_matrix <- eleven_items_long %>%
  group_by(USUBJID, item, treatment) %>%
  arrange(visit_num) %>%
  summarise(
    baseline = first(score),
    final = last(score),
    changed = final > baseline,
    .groups = "drop"
  ) %>%
  mutate(
    item_label = case_when(
      item == "SCMDS1_13" ~ "Fatigue",
      item == "SCMDS2_1" ~ "Speech",
      item == "SCMDS2_4" ~ "Eating",
      item == "SCMDS2_5" ~ "Dressing",
      item == "SCMDS2_6" ~ "Hygiene",
      item == "SCMDS2_7" ~ "Handwriting",
      item == "SCMDS2_8" ~ "Hobbies",
      item == "SCMDS2_9" ~ "Turning",
      item == "SCMDS2_11" ~ "Getting up",
      item == "SCMDS2_12" ~ "Walking",
      item == "SCMDS2_13" ~ "Freezing"
    )
  )

# Summary of change by item and treatment
change_summary <- change_matrix %>%
  group_by(item_label, treatment) %>%
  summarise(
    n_subjects = n(),
    n_changed = sum(changed),
    pct_changed = round(n_changed / n_subjects * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(treatment, desc(pct_changed))

print(change_summary, n = Inf)


change_summary <- change_summary %>%
  filter(!is.na(pct_changed)) %>%
  mutate(
    item_label = factor(item_label, levels = rev(c(
      "Fatigue", "Hobbies", "Eating", "Getting up", "Speech", "Walking",
      "Dressing", "Hygiene", "Turning", "Handwriting", "Freezing"
    )))
  )




change_summary %>% select(item_label, treatment, pct_changed) %>%
  spread(key=treatment, value=pct_changed) %>% mutate(Lixisenatide =ifelse(is.na(Lixisenatide ),0,Lixisenatide )) %>%
  summarise(Lixisenatide =mean(Lixisenatide ), Placebo=mean(Placebo))

change_summary_long <- change_summary %>%
  mutate(
    pct_unchanged = 100 - pct_changed,
    item_label = factor(item_label, levels = rev(c(
      "Fatigue", "Hobbies", "Eating", "Getting up", "Speech", "Walking",
      "Dressing", "Hygiene", "Turning", "Handwriting", "Freezing"
    )))
  )

bar_plot_stacked <- ggplot(change_summary_long, aes(x = item_label, y = pct_changed, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(pct_changed, "%")), 
            position = position_dodge(0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Placebo" = "#7c626c", "Lixisenatide" = "#32435d")) +
  labs(
    title = "Proportion of Subjects with Worsening on Each 11-Item Scale Item",
    subtitle = "Percentage of subjects showing any increase from baseline to Month 12",
    x = "",
    y = "Subjects with Worsening (%)",
    fill = "Treatment"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

print(bar_plot_stacked)
