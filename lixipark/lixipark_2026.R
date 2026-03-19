

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


# select scales

sc <- sc %>% select(USUBJID, VISITNUM, VISITNAM, 
              tremor_mod_scale, non_tremor_mod_scale,eleven_item_mod_scale, 
              tremor_items_missing, non_tremor_items_missing, eleven_item_missing, 
              mds_partIII_total, mds_partIII_missing)

# filter for baseline, 6 months, 12 months

sc <- sc %>% filter(VISITNAM %in% c("Visit 2 - Baseline", "Visit 6 - Follow-up", "Visit 8 - Follow-up"))

# See how many missing

table(sc$tremor_items_missing)
table(sc$non_tremor_items_missing)
table(sc$eleven_item_missing)
table(sc$mds_partIII_missing)

# remove row where things are completely missing

sc_complete_missing <- sc %>%
  filter(tremor_items_missing == 11 | 
         non_tremor_items_missing == 35 | 
         eleven_item_missing == 11 |
         mds_partIII_missing == 33)

# check where they are, mostly month 6

sc_complete_missing %>%
  select(USUBJID, VISITNAM, ends_with("missing")) %>%
  arrange(USUBJID)


# Also remove if too many items missing: TBD

sc <- sc %>%
  mutate(
    exclude_complete_tremor = tremor_items_missing == 11,
    exclude_complete_non_tremor = non_tremor_items_missing == 35,
    exclude_complete_eleven = eleven_item_missing == 11,
    exclude_complete_partIII = mds_partIII_missing == 33,
    
    exclude_excessive_tremor = tremor_items_missing > 2,  # >20% missing
    exclude_excessive_non_tremor = non_tremor_items_missing > 7,  # >20% of 35 items
    exclude_excessive_eleven = eleven_item_missing > 2,  # >20% missing
    exclude_excessive_mds_III = mds_partIII_missing > 7,
    
    exclude_from_primary = exclude_complete_tremor | 
                           exclude_complete_non_tremor | 
                           exclude_complete_eleven |
                           exclude_excessive_tremor |
                           exclude_excessive_non_tremor |
                           exclude_excessive_eleven | 
                           exclude_complete_partIII | 
                           exclude_excessive_mds_III
  )

table(sc$exclude_from_primary)


excluded_primary <- sc %>% 
  filter(exclude_from_primary == TRUE) %>%
  select(USUBJID, VISITNAM, ends_with("missing"), starts_with("exclude_complete"))

excluded_primary %>%
  arrange(USUBJID, VISITNAM) %>%
  print(n = Inf)

excluded_primary %>%
  group_by(USUBJID) %>%
  summarise(
    visits_missing = n(),
    visits = paste(VISITNAM, collapse = "; ")
  ) %>%
  print(n = Inf)


subjects_excluded <- sc %>%
  group_by(USUBJID) %>%
  summarise(
    total_visits = n(),
    excluded_visits = sum(exclude_from_primary),
    pct_excluded = excluded_visits/total_visits * 100
  ) %>%
  filter(excluded_visits > 0) %>%
  arrange(desc(pct_excluded))

subjects_excluded

# Final datasets

# Dataset for primary analyses (excluding flagged rows)
sc_primary_analysis <- sc %>%
  filter(exclude_from_primary == FALSE)

length(unique(sc_primary_analysis$USUBJID)) # 156 - all kept


# For longitudinal analyses, ensure subjects have all 3 visits

sc_longitudinal <- sc_primary_analysis %>%
  group_by(USUBJID) %>%
  filter(n() == 3) %>%  # Must have all 3 visits
  ungroup()

cat("Subjects with all 3 visits:", n_distinct(sc_longitudinal$USUBJID)) # 137

# makes sense, in line with the lixipark trial paper
sc_longitudinal %>% select(USUBJID, VISITNUM, mds_partIII_total) %>%
  inner_join(groupe) %>% drop_na() %>%
  group_by(VISITNUM, RAN_GRP_LIB) %>% summarise(mean=mean(mds_partIII_total), sd=sd(mds_partIII_total))


sc_wide <- sc_longitudinal %>%
  select(USUBJID, VISITNAM, 
         tremor_mod_scale, non_tremor_mod_scale, eleven_item_mod_scale,
         mds_partIII_total) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISITNAM,
    values_from = c(tremor_mod_scale, non_tremor_mod_scale, eleven_item_mod_scale,
                   mds_partIII_total),
    names_glue = "{.value}_{VISITNAM}"
  )


names(sc_wide)

temp <- sc_wide %>% inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB) )

names(temp)


temp_long <- temp %>%
  # Select only the correct tremor scale columns
  select(USUBJID, RAN_GRP_LIB, 
         tremor_visit2 = `tremor_mod_scale_Visit 2 - Baseline`,
         tremor_visit6 = `tremor_mod_scale_Visit 6 - Follow-up`,
         tremor_visit8 = `tremor_mod_scale_Visit 8 - Follow-up`,
         non_tremor_visit2 = `non_tremor_mod_scale_Visit 2 - Baseline`,
         non_tremor_visit6 = `non_tremor_mod_scale_Visit 6 - Follow-up`,
         non_tremor_visit8 = `non_tremor_mod_scale_Visit 8 - Follow-up`,
         eleven_visit2 = `eleven_item_mod_scale_Visit 2 - Baseline`,
         eleven_visit6 = `eleven_item_mod_scale_Visit 6 - Follow-up`,
         eleven_visit8 = `eleven_item_mod_scale_Visit 8 - Follow-up`,
         mdsIII_visit2 = `mds_partIII_total_Visit 2 - Baseline`,
         mdsIII_visit6 = `mds_partIII_total_Visit 6 - Follow-up`,
         mdsIII_visit8 = `mds_partIII_total_Visit 8 - Follow-up`) %>%
  # Now pivot to long format
  pivot_longer(
    cols = -c(USUBJID, RAN_GRP_LIB),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Extract scale and visit
  mutate(
    scale = case_when(
       str_detect(variable, "non_tremor") ~ "Non-Tremor Scale",
      str_detect(variable, "tremor") ~ "Tremor Scale",
      str_detect(variable, "eleven") ~ "11-Item Scale",
      str_detect(variable, "mdsIII_") ~ "MDS-UPDRS Part III"),
    visit = case_when(
      str_detect(variable, "visit2") ~ "Baseline",
      str_detect(variable, "visit6") ~ "Month 6",
      str_detect(variable, "visit8") ~ "Month 12"
    ),
    visit = factor(visit, levels = c("Baseline", "Month 6", "Month 12"))
  ) %>%
  select(USUBJID, RAN_GRP_LIB, scale, visit, value)

# Check the corrected counts
table(temp_long$scale, temp_long$visit)

# Now plot with corrected data
primary_scales <- c("MDS-UPDRS Part III", "Non-Tremor Scale",  "Tremor Scale", "11-Item Scale")

plot_primary <- temp_long %>%
  filter(scale %in% primary_scales) %>%
  mutate(scale = factor(scale, levels = primary_scales)) %>%
  ggplot(aes(x = visit, y = value, fill = RAN_GRP_LIB, colour=RAN_GRP_LIB)) +
  geom_boxplot(alpha = 0.7, outliers = FALSE, notch = TRUE) +
  geom_jitter(alpha=0.5, shape=1, size=0.5, stroke=1, width = 0.5) +
  facet_wrap(~scale, scales = "free_y", nrow = 4) +
  scale_fill_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  scale_color_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  labs(
    title = "MDS-UPDRS Scores \nOver Time",
    subtitle = paste0("n = ", n_distinct(temp_long$USUBJID), " subjects"),
    x = "",
    y = "Score \n",
    fill = "Treatment",
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
        axis.text.x = element_text(size = 12, angle = 45, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(plot_primary)


ggsave(file="../out/4_scales_3_time_points_exact_score.svg", plot=plot_primary, width=3.5, height=10)





summary_comparisons <- temp_long %>%
  group_by(scale, visit) %>%
  # First, get group-wise summaries
  group_modify(~{
    # Separate by treatment group
    placebo_vals <- .x %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(value)
    lixi_vals <- .x %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(value)
    
    # Calculate statistics for Placebo
    placebo_mean <- mean(placebo_vals, na.rm = TRUE)
    placebo_sd <- sd(placebo_vals, na.rm = TRUE)
    placebo_median <- median(placebo_vals, na.rm = TRUE)
    placebo_q1 <- quantile(placebo_vals, 0.25, na.rm = TRUE)
    placebo_q3 <- quantile(placebo_vals, 0.75, na.rm = TRUE)
    placebo_n <- length(placebo_vals)
    
    # Calculate statistics for Lixisenatide
    lixi_mean <- mean(lixi_vals, na.rm = TRUE)
    lixi_sd <- sd(lixi_vals, na.rm = TRUE)
    lixi_median <- median(lixi_vals, na.rm = TRUE)
    lixi_q1 <- quantile(lixi_vals, 0.25, na.rm = TRUE)
    lixi_q3 <- quantile(lixi_vals, 0.75, na.rm = TRUE)
    lixi_n <- length(lixi_vals)
    
    # Perform Wilcoxon test
    wt <- wilcox.test(value ~ RAN_GRP_LIB, data = .x, conf.int = TRUE, conf.level = 0.95)
    
    # Return as a single row
    tibble(
      placebo_n = placebo_n,
      placebo_mean_sd = paste0(round(placebo_mean, 1), " ± ", round(placebo_sd, 1)),
      placebo_median_iqr = paste0(round(placebo_median, 1), " [", round(placebo_q1, 1), "-", round(placebo_q3, 1), "]"),
      
      lixi_n = lixi_n,
      lixi_mean_sd = paste0(round(lixi_mean, 1), " ± ", round(lixi_sd, 1)),
      lixi_median_iqr = paste0(round(lixi_median, 1), " [", round(lixi_q1, 1), "-", round(lixi_q3, 1), "]"),
      
      difference = round(wt$estimate, 2),
      conf_low = round(wt$conf.int[1], 2),
      conf_high = round(wt$conf.int[2], 2),
      p_value = wt$p.value
    )
  }) %>%
  ungroup() %>%
  # Format p-value
  mutate(
    p_value_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    ),
    # Reorder scales for better presentation
    scale = factor(scale, levels = c("Tremor Scale", "Non-Tremor Scale", "11-Item Scale",
                                     "MDS-UPDRS Part III"))
  ) %>%
  arrange(scale, visit)


# View the full table
summary_comparisons %>%
  select(scale, visit, 
         placebo_mean_sd, placebo_median_iqr,
         lixi_mean_sd, lixi_median_iqr,
         p_value_formatted) %>%
  print(n = Inf)



# First, calculate summary statistics for plotting
plot_data_means <- temp_long %>%
  filter(scale %in% primary_scales) %>%
  group_by(scale, visit, RAN_GRP_LIB) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    scale = factor(scale, levels = primary_scales),
    visit = factor(visit, levels = c("Baseline", "Month 6", "Month 12"))
  )

# View the data we'll plot
print(plot_data_means)





plot_primary_bars <- ggplot(plot_data_means, 
                            aes(x = visit, y = mean, fill = RAN_GRP_LIB)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), alpha = 0.7, width = 0.8) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(0.9), width = 0.2, size = 1.0) +
  facet_wrap(~scale, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  labs(
    title = "MDS-UPDRS Scores\nOver Time",
    subtitle = paste0("n = ", n_distinct(temp_long$USUBJID), " subjects"),
    x = "",
    y = "Score\n",
    fill = "Treatment"
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
        axis.text.x = element_text(size = 12, angle = 45, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(plot_primary_bars)

ggsave(file="../out/4_scales_3_time_points_means.svg", plot=plot_primary_bars, width=3.5, height=10)








# Calculate change scores
sc_wide <- sc_wide %>%
  mutate(
    # Primary outcomes change scores
    tremor_chg_6 = `tremor_mod_scale_Visit 6 - Follow-up` - `tremor_mod_scale_Visit 2 - Baseline`,
    tremor_chg_12 = `tremor_mod_scale_Visit 8 - Follow-up` - `tremor_mod_scale_Visit 2 - Baseline`,
    
    non_tremor_chg_6 = `non_tremor_mod_scale_Visit 6 - Follow-up` - `non_tremor_mod_scale_Visit 2 - Baseline`,
    non_tremor_chg_12 = `non_tremor_mod_scale_Visit 8 - Follow-up` - `non_tremor_mod_scale_Visit 2 - Baseline`,
    
    eleven_chg_6 = `eleven_item_mod_scale_Visit 6 - Follow-up` - `eleven_item_mod_scale_Visit 2 - Baseline`,
    eleven_chg_12 = `eleven_item_mod_scale_Visit 8 - Follow-up` - `eleven_item_mod_scale_Visit 2 - Baseline`,
    
    # MDS total parts change scores for comparison
    mdsIII_chg_6 = `mds_partIII_total_Visit 6 - Follow-up` - `mds_partIII_total_Visit 2 - Baseline`,
    mdsIII_chg_12 = `mds_partIII_total_Visit 8 - Follow-up` - `mds_partIII_total_Visit 2 - Baseline`
  
  ) %>%
  select(USUBJID, tremor_chg_6, tremor_chg_12, non_tremor_chg_6, non_tremor_chg_12, eleven_chg_6, eleven_chg_12, mdsIII_chg_6, mdsIII_chg_12)


names(sc_wide)





sc_wide <- sc_wide %>% inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB) )


#  Shapiro-Wilk test for all change scores
normality_tests <- sc_wide %>%
  select(tremor_chg_6, tremor_chg_12, 
         non_tremor_chg_6, non_tremor_chg_12,
         eleven_chg_6, eleven_chg_12,
         mdsIII_chg_6, mdsIII_chg_12) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    skewness = moments::skewness(value, na.rm = TRUE),
    kurtosis = moments::kurtosis(value, na.rm = TRUE),
    shapiro_stat = shapiro.test(value)$statistic,
    shapiro_p = shapiro.test(value)$p.value,
    normal = ifelse(shapiro_p > 0.05, "YES", "NO")
  ) %>%
  arrange(shapiro_p)

print(normality_tests, n = Inf)



normality_by_group <- sc_wide %>%
  select(RAN_GRP_LIB, tremor_chg_6:tremor_chg_12, 
         non_tremor_chg_6:non_tremor_chg_12,
         eleven_chg_6:eleven_chg_12,
         mdsIII_chg_6:mdsIII_chg_12) %>%
  pivot_longer(-RAN_GRP_LIB, names_to = "variable", values_to = "value") %>%
  group_by(RAN_GRP_LIB, variable) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(value)$p.value,
    normal = ifelse(shapiro_p > 0.05, "YES", "NO")
  ) %>%
  pivot_wider(names_from = RAN_GRP_LIB, values_from = c(shapiro_p, normal))

print(normality_by_group, n = Inf)



# Create long format for change scores
delta_long <- sc_wide %>%
  select(USUBJID, RAN_GRP_LIB, 
         tremor_chg_6, tremor_chg_12,
         non_tremor_chg_6, non_tremor_chg_12,
         eleven_chg_6, eleven_chg_12,
         mdsIII_chg_6, mdsIII_chg_12) %>%
  pivot_longer(
    cols = -c(USUBJID, RAN_GRP_LIB),
    names_to = "variable",
    values_to = "change_score"
  ) %>%
  mutate(
    scale = case_when(
       str_detect(variable, "non_tremor") ~ "Non-Tremor Scale",
      str_detect(variable, "tremor") ~ "Tremor Scale",
      str_detect(variable, "eleven") ~ "11-Item Scale",
      str_detect(variable, "mdsIII_") ~ "MDS-UPDRS Part III"
    ),
    timepoint = case_when(
      str_detect(variable, "chg_6") ~ "Month 6",
      str_detect(variable, "chg_12") ~ "Month 12"
    ),
    timepoint = factor(timepoint, levels = c("Month 6", "Month 12")),
    # Create a nice label for plotting
    scale_label = factor(scale, levels = c("Tremor Scale", "Non-Tremor Scale", 
                                           "11-Item Scale", "MDS-UPDRS Part III"))
  ) %>%
  select(USUBJID, RAN_GRP_LIB, scale, scale_label, timepoint, change_score) %>%
  drop_na()


delta_long

# Check counts
table(delta_long$scale, delta_long$timepoint)
table(delta_long$RAN_GRP_LIB, delta_long$timepoint)

# Summary statistics for change scores
delta_summary <- delta_long %>%
  group_by(scale, timepoint, RAN_GRP_LIB) %>%
  summarise(
    n = n(),
    mean = mean(change_score, na.rm = TRUE),
    sd = sd(change_score, na.rm = TRUE),
    median = median(change_score, na.rm = TRUE),
    q1 = quantile(change_score, 0.25, na.rm = TRUE),
    q3 = quantile(change_score, 0.75, na.rm = TRUE),
    min = min(change_score, na.rm = TRUE),
    max = max(change_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 2)),
    mean_sd = paste0(mean, " ± ", sd),
    median_iqr = paste0(median, " [", q1, "-", q3, "]")
  )

# View summary
print(delta_summary, n = Inf)




wilcoxon_delta <- delta_long %>%
  group_by(scale, timepoint) %>%
  group_modify(~{
    wt <- wilcox.test(change_score ~ RAN_GRP_LIB, data = .x, conf.int = TRUE)
    tibble(
      difference = wt$estimate,
      conf_low = wt$conf.int[1],
      conf_high = wt$conf.int[2],
      p_value = wt$p.value
    )
  }) %>%
  mutate(
    across(where(is.numeric), ~round(., 3)),
    p_value_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    )
  )

print(wilcoxon_delta, n = Inf)


desired_scale_order <- c("MDS-UPDRS Part III", "Non-Tremor Scale", "Tremor Scale", "11-Item Scale")

delta_long <- delta_long %>%
  mutate(scale = factor(scale, levels = desired_scale_order))

plot_delta_box <- ggplot(delta_long, 
                         aes(x = timepoint, y = change_score, 
                             fill = RAN_GRP_LIB, colour = RAN_GRP_LIB)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, notch = TRUE) +
  geom_jitter(alpha=0.5, shape=1, size=0.5, stroke=1, width = 0.5) +
  facet_wrap(~scale, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  scale_color_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 1.0) +
  labs(
    title = "Change in Scores from Baseline",
    subtitle = paste0("n = ", n_distinct(delta_long$USUBJID), " subjects"),
    x = "",
    y = "Change Score\n(positive = worsening)\n",
    fill = "Treatment",
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
        axis.text.x = element_text(size = 12, angle = 45, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(plot_delta_box)

ggsave(file="../out/deltas_box.svg", plot=plot_delta_box, width=3.8, height=10)




delta_summary <- delta_long %>%
  group_by(scale, timepoint, RAN_GRP_LIB) %>%
  summarise(
    n = n(),
    mean = mean(change_score, na.rm = TRUE),
    sd = sd(change_score, na.rm = TRUE),
    median = median(change_score, na.rm = TRUE),
    q1 = quantile(change_score, 0.25, na.rm = TRUE),
    q3 = quantile(change_score, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(delta_summary, n = Inf)

plot_delta_bars <- ggplot(delta_summary, 
                          aes(x = timepoint, y = mean, fill = RAN_GRP_LIB)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), 
           alpha = 0.7, width = 0.8, color = NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(0.9), width = 0.2, size = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 1.0) +
  facet_wrap(~scale, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("1 - Placebo" = "#7c626c", "2 - Lixisenatide" = "#32435d")) +
  labs(
    title = "Change in Scores from Baseline",
    subtitle = paste0("n = ", n_distinct(delta_long$USUBJID), " subjects"),
    x = "",
    y = "Change Score\n(positive = worsening)\n",
    fill = "Treatment"
  ) +
  # Theme - matching your boxplot style
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
        axis.text.x = element_text(size = 12, angle = 45, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


print(plot_delta_bars)


ggsave(file="../out/deltas_bar.svg", plot=plot_delta_bars, width=3.8, height=10)



delta_long

dm %>%
  select(USUBJID, BRTHDAT, CONSDAT, CONSDATD1) %>%
  head(10)


dm_age <- dm %>%
  select(USUBJID, BRTHDAT, CONSDAT, CONSDATD1) %>%
  mutate(
    birth_year = as.numeric(str_extract(BRTHDAT, "\\d{4}")),
    birth_month_str = str_extract(BRTHDAT, "[A-Z]{3}"),
    birth_month = case_when(
      birth_month_str == "JAN" ~ 1,
      birth_month_str == "FEB" ~ 2,
      birth_month_str == "MAR" ~ 3,
      birth_month_str == "APR" ~ 4,
      birth_month_str == "MAY" ~ 5,
      birth_month_str == "JUN" ~ 6,
      birth_month_str == "JUL" ~ 7,
      birth_month_str == "AUG" ~ 8,
      birth_month_str == "SEP" ~ 9,
      birth_month_str == "OCT" ~ 10,
      birth_month_str == "NOV" ~ 11,
      birth_month_str == "DEC" ~ 12,
      TRUE ~ NA_real_
    ),
        birth_date = as.Date(paste(birth_year, birth_month, "15", sep = "-")),
      consent_date = CONSDATD1,
    age_years = as.numeric(difftime(consent_date, birth_date, units = "days")) / 365.25,
    age = floor(age_years)  
  ) %>%
  select(USUBJID, age, birth_date, consent_date, age_years)


dm_age %>% 
  select(USUBJID, age) %>%
  head(20)

summary(dm_age$age)
hist(dm_age$age, breaks = 20, main = "Age Distribution", xlab = "Age (years)")

# Create age groups (for subgroup analysis)
dm_age <- dm_age %>%
  mutate(
    age_group = case_when(
      age < 60 ~ "<60",
      age >= 60 ~ "≥60"
    )
  ) %>% select(USUBJID, age_group)

 
dm_age <- dm_age %>% inner_join(delta_long) %>%
  filter(timepoint=="Month 12")

dm_age %>%
  group_by(age_group, RAN_GRP_LIB) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  pivot_wider(names_from = RAN_GRP_LIB, values_from = n)

#   age_group `1 - Placebo` `2 - Lixisenatide`
# 1 <60                  34                 41
# 2 ≥60                  36                 26

dm_age %>%
  group_by(scale, RAN_GRP_LIB, age_group) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  print(n = Inf)

#  scale              RAN_GRP_LIB      age_group     n
#  1 MDS-UPDRS Part III 1 - Placebo      <60          34
#  2 MDS-UPDRS Part III 1 - Placebo      ≥60          36
#  3 MDS-UPDRS Part III 2 - Lixisenatide <60          41
#  4 MDS-UPDRS Part III 2 - Lixisenatide ≥60          26
#  5 Non-Tremor Scale   1 - Placebo      <60          34
#  6 Non-Tremor Scale   1 - Placebo      ≥60          36
#  7 Non-Tremor Scale   2 - Lixisenatide <60          41
#  8 Non-Tremor Scale   2 - Lixisenatide ≥60          26
#  9 Tremor Scale       1 - Placebo      <60          34
# 10 Tremor Scale       1 - Placebo      ≥60          36
# 11 Tremor Scale       2 - Lixisenatide <60          41
# 12 Tremor Scale       2 - Lixisenatide ≥60          26
# 13 11-Item Scale      1 - Placebo      <60          34
# 14 11-Item Scale      1 - Placebo      ≥60          36
# 15 11-Item Scale      2 - Lixisenatide <60          41
# 16 11-Item Scale      2 - Lixisenatide ≥60          26



# WITHIN EACH TREATMENT GROUP: Compare <60 vs ≥60
age_within_treatment <- dm_age %>%
  group_by(scale, RAN_GRP_LIB) %>%
  group_modify(~{
    wt <- wilcox.test(change_score ~ age_group, data = .x, conf.int = TRUE)
    n_young <- sum(.x$age_group == "<60")
    n_old <- sum(.x$age_group == "≥60")
        young_mean <- mean(.x$change_score[.x$age_group == "<60"], na.rm = TRUE)
    old_mean <- mean(.x$change_score[.x$age_group == "≥60"], na.rm = TRUE)
    tibble(
      n_young = n_young,
      n_old = n_old,
      young_mean = round(young_mean, 2),
      old_mean = round(old_mean, 2),
      difference = round(wt$estimate, 2),
      conf_low = round(wt$conf.int[1], 2),
      conf_high = round(wt$conf.int[2], 2),
      p_value = wt$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(
    p_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    ),
    sig_star = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

print(age_within_treatment, n = Inf)

#   scale   RAN_GRP_LIB n_young n_old young_mean old_mean difference conf_low conf_high p_value p_formatted sig_star
#   <fct>   <chr>         <int> <int>      <dbl>    <dbl>      <dbl>    <dbl>     <dbl>   <dbl> <chr>       <chr>   
# 1 MDS-UP… 1 - Placebo      34    36       4.12     1.78          2       -1         5  0.176  0.18        ""      
# 2 MDS-UP… 2 - Lixise…      41    26      -0.32     0.5          -1       -5         3  0.558  0.56        ""      
# 3 Non-Tr… 1 - Placebo      34    36       4.26     2.69          2       -2         5  0.365  0.36        ""      
# 4 Non-Tr… 2 - Lixise…      41    26       0.98     1.27          0       -4         3  0.954  0.95        ""      
# 5 Tremor… 1 - Placebo      34    36       1.44     0.36          1        0         2  0.0934 0.09        ""      
# 6 Tremor… 2 - Lixise…      41    26       0.39     0.19          0       -1         1  0.773  0.77        ""      
# 7 11-Ite… 1 - Placebo      34    36       1.68     1.31          0       -1         2  0.700  0.7         ""      
# 8 11-Ite… 2 - Lixise…      41    26       1.54     0.77          1       -1         2  0.384  0.38        ""      


age_desc <- dm_age %>%
  group_by(scale, RAN_GRP_LIB, age_group) %>%
  summarise(
    n = n(),
    mean = mean(change_score, na.rm = TRUE),
    sd = sd(change_score, na.rm = TRUE),
    median = median(change_score, na.rm = TRUE),
    q1 = quantile(change_score, 0.25, na.rm = TRUE),
    q3 = quantile(change_score, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_sd = paste0(round(mean, 1), " ± ", round(sd, 1)),
    median_iqr = paste0(round(median, 1), " [", round(q1, 1), "-", round(q3, 1), "]")
  )

print(age_desc, n = Inf)

age_desc %>% select(scale, RAN_GRP_LIB, age_group, mean_sd, median_iqr) %>%
  mutate(mean_sd=paste0(mean_sd, paste0(" | ", median_iqr))) %>% select(-median_iqr) %>%
  spread(key=age_group, value=mean_sd)

#     scale              RAN_GRP_LIB      `<60`                  `≥60`                   
# 1 MDS-UPDRS Part III 1 - Placebo      4.1 ± 6.6 | 4 [-0.8-7] 1.8 ± 6.6 | 1 [-2-6.2]  
# 2 MDS-UPDRS Part III 2 - Lixisenatide -0.3 ± 7.1 | -1 [-6-5] 0.5 ± 6.8 | 0.5 [-3.8-6]
# 3 Non-Tremor Scale   1 - Placebo      4.3 ± 7.3 | 3 [0.2-9]  2.7 ± 6.9 | 3 [-2-8]    
# 4 Non-Tremor Scale   2 - Lixisenatide 1 ± 7.1 | 1 [-3-5]     1.3 ± 7.4 | 0 [-2-6.5]  
# 5 Tremor Scale       1 - Placebo      1.4 ± 2.7 | 1 [0-3]    0.4 ± 2 | 0 [-1-1]      
# 6 Tremor Scale       2 - Lixisenatide 0.4 ± 2.3 | 0 [-1-2]   0.2 ± 2.2 | 0 [-0.8-1]  
# 7 11-Item Scale      1 - Placebo      1.7 ± 3.3 | 1 [0-4.8]  1.3 ± 2.6 | 1 [-0.2-3]  
# 8 11-Item Scale      2 - Lixisenatide 1.5 ± 3.2 | 1 [0-2]    0.8 ± 3.9 | 1 [-1-2] 





library(effsize)  # for Cohen's d (still relevant for effect size)

# Function to calculate standardized mean difference (Cohen's d for paired samples)
# This is also known as the standardized response mean (SRM)
calc_smd <- function(data, group_name, timepoint_val) {
  data_group <- data %>% 
    filter(RAN_GRP_LIB == group_name, timepoint == timepoint_val)
  # Calculate Cohen's d (paired) - which is mean_change / sd_change
  mean_change <- mean(data_group$change_score, na.rm = TRUE)
  sd_change <- sd(data_group$change_score, na.rm = TRUE)
  n <- nrow(data_group)
  # Standardized mean difference (SMR)
  smd <- mean_change / sd_change
  # 95% CI for SMD using formula: smd ± 1.96 * sqrt(1/n + smd^2/(2*n))
  se_smd <- sqrt(1/n + smd^2/(2*n))
  ci_lower <- smd - 1.96 * se_smd
  ci_upper <- smd + 1.96 * se_smd
  tibble(
    treatment = group_name,
    timepoint = timepoint_val,
    n = n,
    mean_change = mean_change,
    sd_change = sd_change,
    smd = smd,
    smd_ci_lower = ci_lower,
    smd_ci_upper = ci_upper,
    smd_magnitude = case_when(
      abs(smd) < 0.2 ~ "negligible",
      abs(smd) < 0.5 ~ "small",
      abs(smd) < 0.8 ~ "medium",
      TRUE ~ "large"
    )
  )
}

# Calculate SMD for each scale, treatment group, and timepoint
smd_results <- delta_long %>%
  group_by(scale) %>%
  group_modify(~{
    bind_rows(
      # Month 6
      calc_smd(.x, "1 - Placebo", "Month 6"),
      calc_smd(.x, "2 - Lixisenatide", "Month 6"),
      # Month 12
      calc_smd(.x, "1 - Placebo", "Month 12"),
      calc_smd(.x, "2 - Lixisenatide", "Month 12")
    )
  }) %>%
  ungroup() %>%
  mutate(
    across(where(is.numeric), ~round(., 3)),
    ci = paste0("[", smd_ci_lower, ", ", smd_ci_upper, "]"),
    smd_display = paste0(smd, " ", ci)
  )

# View results
print(smd_results, n = Inf)



smd_wide <- smd_results %>%
  select(scale, treatment, timepoint, smd, smd_ci_lower, smd_ci_upper, smd_magnitude) %>%
  pivot_wider(
    id_cols = c(scale, timepoint),
    names_from = treatment,
    values_from = c(smd, smd_ci_lower, smd_ci_upper, smd_magnitude),
    names_glue = "{treatment}_{.value}"
  ) %>%
  mutate(
    Placebo_display = paste0(round(`1 - Placebo_smd`, 2), " [", 
                             round(`1 - Placebo_smd_ci_lower`, 2), "-", 
                             round(`1 - Placebo_smd_ci_upper`, 2), "]"),
    Lixisenatide_display = paste0(round(`2 - Lixisenatide_smd`, 2), " [",
                                  round(`2 - Lixisenatide_smd_ci_lower`, 2), "-",
                                  round(`2 - Lixisenatide_smd_ci_upper`, 2), "]")
  ) %>%
  select(scale, timepoint, Placebo_display, Lixisenatide_display)

print(smd_wide, n = Inf)


smd_plot_data <- smd_results %>%
  mutate(
    treatment_clean = ifelse(treatment == "1 - Placebo", "Placebo", "Lixisenatide"),
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale"))
  )



smd_plot <- ggplot(smd_plot_data, 
                   aes(x = smd, y = scale, color = treatment_clean)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, stroke=2, shape=1) +
   geom_linerange(aes(xmin = smd_ci_lower, xmax = smd_ci_upper, y = scale),
                 position = position_dodge(width = 0.5), 
                 size = 2, alpha = 0.7, lineend = "round", orientation = "y") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Reference lines for effect size magnitudes
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  scale_color_manual(values = c("Placebo" = "#7c626c", "Lixisenatide" = "#32435d")) +
  scale_shape_manual(values = c("Month 6" = 16, "Month 12" = 17)) +
  labs(
    title = "Standardized Mean Change from Baseline",
    subtitle = "Cohen's d (positive = worsening)",
    x = "\n Standardized Mean Difference (95% CI)",
    y = "",
    color = "Treatment",
    shape = "Timepoint"
  ) +
  facet_wrap(~timepoint) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), limits = c(-1, 1)) +
   theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 12, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(smd_plot)

ggsave(file = "../out/smd_forest_plot.svg", plot = smd_plot, width = 10, height = 4)





# Calculate SMD for each scale and timepoint (ignoring treatment groups)
smd_overall <- delta_long %>%
  group_by(scale, timepoint) %>%
  summarise(
    n = n(),
    mean_change = mean(change_score, na.rm = TRUE),
    sd_change = sd(change_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Standardized mean difference (SMR) = mean_change / sd_change
    smd = mean_change / sd_change,
    # 95% CI for SMD
    se_smd = sqrt(1/n + smd^2/(2*n)),
    smd_ci_lower = smd - 1.96 * se_smd,
    smd_ci_upper = smd + 1.96 * se_smd,
    # Magnitude interpretation
    smd_magnitude = case_when(
      abs(smd) < 0.2 ~ "negligible",
      abs(smd) < 0.5 ~ "small",
      abs(smd) < 0.8 ~ "medium",
      TRUE ~ "large"
    ),
    # Format for display
    across(where(is.numeric), ~round(., 3)),
    smd_display = paste0(smd, " [", smd_ci_lower, "-", smd_ci_upper, "] ", smd_magnitude)
  )

# View results
print(smd_overall, n = Inf)

# Create a nice table
smd_overall_table <- smd_overall %>%
  select(scale, timepoint, n, mean_change, sd_change, smd_display) %>%
  arrange(scale, timepoint)

print(smd_overall_table, n = Inf)

# Plot comparing scales directly
smd_overall_plot_data <- smd_overall %>%
  mutate(
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    # Create a numeric version for positioning
    scale_num = as.numeric(scale)
  )

# Plot
smd_overall_plot <- ggplot(smd_overall_plot_data, 
                           aes(x = smd, y = scale, color = timepoint)) +
  # Lineranges for CI
  geom_linerange(aes(xmin = smd_ci_lower, xmax = smd_ci_upper, y = scale),
                 position = position_dodge(width = 0.5), 
                 size = 2, alpha = 0.5, lineend = "round") +
  # Points for SMD
  geom_point(aes(), 
             position = position_dodge(width = 0.5), 
             size = 3, stroke = 2,shape=1) +
  # Reference lines
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  # Colors and shapes
  scale_color_manual(values = c("Month 6" = "#7c626c", "Month 12" = "#32435d")) +
  # Labels
  labs(
    title = "Standardized Mean Change from Baseline",
    subtitle = "Cohen's d (positive = worsening)",
    x = "\n Standardized Mean Difference (95% CI)",
    y = "",
    color = "Treatment",
    shape = "Timepoint"
  ) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), limits = c(-1, 1)) +
   theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 12, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(smd_overall_plot)

ggsave("../out/smd_scale_comparison.svg", smd_overall_plot, width = 7, height = 4)


# Create a ranking of scales by sensitivity (absolute SMD)
scale_ranking <- smd_overall %>%
  filter(timepoint == "Month 12") %>%
  mutate(
    abs_smd = abs(smd),
    rank = rank(-abs_smd)
  ) %>%
  select(scale, abs_smd, rank, smd_magnitude) %>%
  arrange(rank)

print(scale_ranking, n = Inf)


#  scale              abs_smd  rank smd_magnitude
# 1 11-Item Scale        0.427     1 small        
# 2 Non-Tremor Scale     0.319     2 small        
# 3 Tremor Scale         0.259     3 small        
# 4 MDS-UPDRS Part III   0.215     4 small   





vs <- read_sas("../data/vs.sas7bdat")
sc <- read_sas("../data/sc.sas7bdat")
pt <- read_sas("../data/pt.sas7bdat")
groupe <- read_sas("../data/groupe.sas7bdat")


vs <- vs %>% select(USUBJID, VISITNUM, VISITNAM, VSDATD1)
vs <- vs %>% distinct()

range(vs$VSDATD1, na.rm=T)


pt <- pt %>% select(USUBJID, PTYN, PTSPID, PTTRT, PTNAMEATN, PTPOSO, PTTIMDAY, PTSTDATD1, PTENDATD1)

pt <- pt %>% 
             mutate(PTPOSO=str_replace(PTPOSO, ",", ".")) %>%
             mutate(PTPOSO=str_replace(PTPOSO, " MG", "")) %>%
             mutate(PTPOSO=str_replace(PTPOSO, "MG", "")) 


pt$PTPOSO <- as.numeric(pt$PTPOSO)

unique(pt$PTTIMDAY)

pt$PTTIMDAY <- parse_number(pt$PTTIMDAY)

pt <- pt %>% mutate(PTTIMDAY=ifelse(is.na(PTTIMDAY),1,PTTIMDAY))

range(pt$PTSTDATD1, na.rm=T)
range(pt$PTENDATD1, na.rm=T)

pt <- pt %>% filter(PTNAMEATN!="DIRECT ACTING ANTIVIRALS")


pt <- pt %>% mutate(PTTRT=ifelse(grepl("SIFROL", PTTRT), "SIFROL", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("ROPIN", PTTRT), "ROPINIROL", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("REQUIP", PTTRT), "REQUIP", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("MODOPAR", PTTRT), "MODOPAR", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("LEVODOPA", PTTRT), "LEVODOPA-CARBIDOPA", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("L-DOPA", PTTRT), "LEVODOPA-CARBIDOPA", PTTRT))

pt <- pt %>% mutate(PTTRT=ifelse(grepl("SINEMET", PTTRT), "SINEMET", PTTRT))

data.frame(pt %>% select(PTTRT, PTNAMEATN) %>% distinct()) %>%
  arrange(PTNAMEATN, PTTRT)

pt <- pt %>% filter(PTTRT!="ARTANE" & PTTRT!="PARKINANE")




ledd_conversion <- tibble(
  PTNAMEATN = c("Dopa and dopa derivatives", "Dopa and dopa derivatives", 
                "Dopa and dopa derivatives", "Dopa and dopa derivatives",
                "Dopamine agonists", "Dopamine agonists", "Dopamine agonists",
                "Dopamine agonists", "Dopamine agonists", "Dopamine agonists",
                "Monoamine oxidase B inhibitors", "Monoamine oxidase B inhibitors"),
  PTTRT = c("LEVODOPA-CARBIDOPA", "MODOPAR", "SINEMET", "STALEVO",
            "NEUPRO", "PRAMIPEXOLE LP", "REQUIP", "ROPINIROL", "SIFROL", "TRIVASTAL LP",
            "AZILECT", "RASAGILINE"),
  FACTOR = c(1, 1, 1, 0.33,
             30, 100, 20, 20, 100, 1,
             100, 100)
)


study_start <- min(vs$VSDATD1, na.rm = TRUE) - 1  # "2018-06-12"
study_end <- max(vs$VSDATD1, na.rm = TRUE) + 1    # "2021-04-16"

pt_clean <- pt %>%
  mutate(
    PTSTDATD1 = if_else(is.na(PTSTDATD1), study_start, PTSTDATD1),
    PTENDATD1 = if_else(is.na(PTENDATD1), study_end, PTENDATD1),
    # Ensure dates are in correct order
    PTENDATD1 = if_else(PTENDATD1 < PTSTDATD1, PTSTDATD1 + days(1), PTENDATD1)
  ) %>%
  left_join(ledd_conversion, by = c("PTTRT", "PTNAMEATN")) %>%
  mutate(
    daily_dose_mg = PTPOSO * PTTIMDAY,
    ledd_contribution = daily_dose_mg * FACTOR
  )


calculate_ledd_at_date <- function(patient_id, date, med_data) {
  med_data %>%
    filter(
      USUBJID == patient_id,
      PTSTDATD1 <= date,
      PTENDATD1 >= date
    ) %>%
    pull(ledd_contribution) %>%
    sum(na.rm = TRUE)
}


baseline_data <- vs %>%
  filter(VISITNUM == 2) %>% drop_na() %>%
  select(USUBJID, baseline_date = VSDATD1) %>%
  mutate(
    baseline_ledd = map2_dbl(
      USUBJID, baseline_date,
      ~calculate_ledd_at_date(.x, .y, pt_clean)
    )
  )


mean(baseline_data$baseline_ledd)



followup_visits <- vs %>%
  filter(VISITNUM > 2, VISITNUM < 90) %>%  drop_na() %>%
  select(USUBJID, VISITNUM, VISITNAM, visit_date = VSDATD1) %>%
  arrange(USUBJID, visit_date)



ledd_changes <- followup_visits %>%
  left_join(baseline_data, by = "USUBJID") %>%
  mutate(
    visit_ledd = map2_dbl(
      USUBJID, visit_date,
      ~calculate_ledd_at_date(.x, .y, pt_clean)
    ),
    ledd_increased = visit_ledd > baseline_ledd
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    # Mark first visit where LEDD increased
    first_increase = ledd_increased & !duplicated(ledd_increased & ledd_increased)
  ) %>%
  ungroup()

ledd_changes %>% summarise(mean=mean(baseline_ledd ))

ledd_changes %>% filter(VISITNUM==8) %>%
  summarise(mean=mean(visit_ledd))





ledd_changes %>%
  inner_join(groupe) %>%
  select(USUBJID, RAN_GRP_LIB, baseline_ledd) %>% distinct() %>%
  inner_join(
    ledd_changes %>%
  inner_join(groupe) %>% filter(VISITNUM ==8) %>%
  select(USUBJID, RAN_GRP_LIB, visit_ledd ) %>% distinct()
  ) %>% mutate(diff=visit_ledd-baseline_ledd) %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(mean=mean(diff), sd=sd(diff))


survival_data <- ledd_changes %>%
  group_by(USUBJID) %>%
  summarise(
    baseline_date = first(baseline_date),
    baseline_ledd = first(baseline_ledd),
    # Handle Inf by converting to NA
    event_date = if_else(
      any(first_increase, na.rm = TRUE),
      min(visit_date[first_increase], na.rm = TRUE),
      NA_Date_
    ),
    event_occurred = any(first_increase, na.rm = TRUE),
    last_visit_date = max(visit_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Time in days - convert difftime to numeric
    time_days = if_else(
      event_occurred,
      as.numeric(difftime(event_date, baseline_date, units = "days")),
      as.numeric(difftime(last_visit_date, baseline_date, units = "days"))
    ),
    # Time in months (for interpretation)
    time_months = time_days / 30.44,
    event = as.integer(event_occurred)
  )


survival_data %>%
  group_by(event) %>%
  summarise(
    n = n(),
    mean_time = mean(time_months, na.rm = TRUE)
  )


library(survival)
library(survminer)


survival_data %>%
  left_join(groupe %>% select(USUBJID, RAN_GRP_LIB)) %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(median=median(baseline_ledd), q1=quantile(baseline_ledd, 0.25), q3=quantile(baseline_ledd, 0.75))

survival_data %>%
  left_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  mutate(treatment = ifelse(RAN_GRP_LIB == "1 - Placebo", "Placebo", "Lixisenatide")) %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    events = sum(event),
    censored = n() - events,
    event_pct = mean(event) * 100,
    median_time = median(time_months[event == 1], na.rm = TRUE)
  )


#   treatment        n events censored event_pct median_time
# 1 Lixisenatide    78     24       54      30.8        9.02
# 2 Placebo         78     23       55      29.5        9.03





surv_object <- with(survival_data, Surv(time = time_months, event = event))

# Add treatment group
survival_data <- survival_data %>%
  left_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  mutate(
    treatment = factor(ifelse(RAN_GRP_LIB == "1 - Placebo", "Placebo", "Lixisenatide"),
                       levels = c("Placebo", "Lixisenatide"))
  )


km_fit <- survfit(Surv(time_months, event) ~ treatment, data = survival_data)

# Print summary
summary(km_fit)
print(km_fit)

# 2. Log-rank test
logrank_test <- survdiff(Surv(time_months, event) ~ treatment, data = survival_data)
print(logrank_test)

# Call:
# survdiff(formula = Surv(time_months, event) ~ treatment, data = survival_data)
# 
#                         N Observed Expected (O-E)^2/E (O-E)^2/V
# treatment=Placebo      78       23     24.3    0.0665     0.141
# treatment=Lixisenatide 78       24     22.7    0.0710     0.141
# 
#  Chisq= 0.1  on 1 degrees of freedom, p= 0.7 
#  

# Calculate p-value from log-rank test
p_value <- 1 - pchisq(logrank_test$chisq, df = 1)
cat("Log-rank test p-value:", p_value, "\n")

# Log-rank test p-value: 0.7071856 


km_plot <- ggsurvplot(
  km_fit,
  data = survival_data,
  pval = TRUE,                      # Show p-value
  pval.method = TRUE,                # Show which test
  conf.int = TRUE,                   # Show confidence intervals
  risk.table = TRUE,                  # Show risk table
  risk.table.col = "strata",          # Color risk table by group
  palette = c("#7c626c", "#32435d"),  # Your colors
  xlab = "\n Time (months)",
  ylab = "Probability of no LEDD increase \n",
  title = "Time to First LEDD Increase by Treatment Group",
  subtitle = paste0("n = ", nrow(survival_data), " subjects"),
  legend.title = "Treatment\n",
  legend.labs = c("Placebo", "Lixisenatide"),
  ggtheme = theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.0),
      plot.subtitle = element_text(hjust = 0.0),
      legend.position = "bottom"
    )
)

print(km_plot)

ggsave("../out/km_ledd_treatment.svg", plot = km_plot$plot, width = 6, height = 6)

ggsave("../out/km_table_ledd_treatment.svg", plot = km_plot$table, width = 6, height = 3)


# Calculate 1-year survival probabilities
# Get survival probabilities at 12 months
summary_12mo <- summary(km_fit, times = 12)


# Extract for each group
placebo_surv <- summary_12mo$surv[summary_12mo$strata == "treatment=Placebo"]
lixi_surv <- summary_12mo$surv[summary_12mo$strata == "treatment=Lixisenatide"]

placebo_lower <- summary_12mo$lower[summary_12mo$strata == "treatment=Placebo"]
placebo_upper <- summary_12mo$upper[summary_12mo$strata == "treatment=Placebo"]
lixi_lower <- summary_12mo$lower[summary_12mo$strata == "treatment=Lixisenatide"]
lixi_upper <- summary_12mo$upper[summary_12mo$strata == "treatment=Lixisenatide"]

cat("\n1-year survival probability (no LEDD increase):\n")
cat("Placebo: ", round(placebo_surv * 100, 1), "% [", 
    round(placebo_lower * 100, 1), "-", round(placebo_upper * 100, 1), "]\n", sep = "")
cat("Lixisenatide: ", round(lixi_surv * 100, 1), "% [", 
    round(lixi_lower * 100, 1), "-", round(lixi_upper * 100, 1), "]\n", sep = "")


# 1-year survival probability (no LEDD increase):
# Placebo: 73.3% [63.9-84.2]
# Lixisenatide: 68.9% [58.8-80.7]


# Cox proportional hazards model (unadjusted)
cox_unadjusted <- coxph(Surv(time_months, event) ~ treatment, data = survival_data)
summary(cox_unadjusted)

# Extract hazard ratio and CI
hr <- exp(coef(cox_unadjusted))
hr_ci <- exp(confint(cox_unadjusted))
cat("\nCox model (unadjusted):\n")
cat("HR = ", round(hr, 2), " [", round(hr_ci[1], 2), "-", round(hr_ci[2], 2), "], p = ", 
    round(summary(cox_unadjusted)$coefficients[1, "Pr(>|z|)"], 3), "\n", sep = "")

# HR = 1.11 [0.63-1.98], p = 0.711




baseline_scores <- temp_long %>% filter(visit=="Baseline")






library(broom)

baseline_wide <- baseline_scores %>%
  select(USUBJID, scale, value) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = scale,
    values_from = value
  ) %>%
  # Clean up column names (remove spaces for easier coding)
  rename_with(~make.names(.))

# Join with survival data
survival_data_scales <- survival_data %>%
  left_join(baseline_wide, by = "USUBJID")


# Univariate Cox models for each scale
scales_to_test <- c("MDS.UPDRS.Part.III", "Non.Tremor.Scale", "Tremor.Scale", "X11.Item.Scale")

univariate_results <- map_df(scales_to_test, function(s) {
  formula <- as.formula(paste0("Surv(time_months, event) ~ ", s))
  cox_fit <- coxph(formula, data = survival_data_scales)
  tidy_cox <- tidy(cox_fit, conf.int = TRUE)
  
  tibble(
    scale = s,
    hr = exp(tidy_cox$estimate),
    hr_lower = exp(tidy_cox$conf.low),
    hr_upper = exp(tidy_cox$conf.high),
    p_value = tidy_cox$p.value,
    concordance = cox_fit$concordance[1]
  )
})

print(univariate_results, digits = 3)


#   scale                 hr hr_lower hr_upper p_value concordance
# 1 MDS.UPDRS.Part.III  1.04    1.00      1.08  0.0280        2655
# 2 Non.Tremor.Scale    1.04    1.01      1.07  0.0178        2640
# 3 Tremor.Scale        1.08    0.981     1.18  0.121         2325
# 4 X11.Item.Scale      1.07    0.999     1.15  0.0522        2478


# Prepare data for plotting
forest_data <- univariate_results %>%
  mutate(
    scale_clean = case_when(
      scale == "MDS.UPDRS.Part.III" ~ "MDS-UPDRS Part III",
      scale == "Non.Tremor.Scale" ~ "Non-Tremor Scale",
      scale == "Tremor.Scale" ~ "Tremor Scale",
      scale == "X11.Item.Scale" ~ "11-Item Scale"
    ),
    scale_clean = factor(scale_clean, 
                         levels = c("MDS-UPDRS Part III", 
                                    "Non-Tremor Scale", 
                                    "11-Item Scale", 
                                    "Tremor Scale")),
    sig_star = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )




# Add version with HR values as text on the left
hr_plot_with_hr <- ggplot(forest_data, 
                          aes(x = hr, y = scale_clean)) +
  # Lineranges for CI
  geom_linerange(aes(xmin = hr_lower, xmax = hr_upper, y = scale_clean),
                 size = 2, alpha = 0.7, lineend = "round", color = "#32435d") +
  # Point estimate
  geom_point(size = 2, stroke = 3, shape = 1, color = "#32435d") +
  # Vertical line at HR=1
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  # Add HR values as text
  geom_text(aes(x = 0.91, label = paste0(round(hr, 2), " [", 
                                          round(hr_lower, 2), "-", 
                                          round(hr_upper, 2), "]")), 
            size = 3.5, color = "gray20", hjust = 0) +
  # Add p-values
  geom_text(aes(x = 1.22, label = paste0("p-val: ", round(p_value, 3)) ), 
            size = 3.5, hjust = 0) +
  # Labels
  labs(
    title = "Baseline Clinical Scales as Predictors of Time to First LEDD Increase",
    subtitle = "Hazard ratios from univariate Cox models",
    x = "\n Hazard Ratio (95% CI)",
    y = ""
  ) +
  scale_x_continuous(breaks = c(0.95, 1, 1.05, 1.1, 1.15, 1.2), 
                     limits = c(0.9, 1.3)) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, face = "bold", hjust = 1),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust = -0.1),
        axis.text.y = element_text(size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.margin = margin(10, 30, 10, 10, "pt")) 

print(hr_plot_with_hr)

ggsave("../out/ledd_scales_forest_with_hr.svg", hr_plot_with_hr, width = 10, height = 3)




  

  





survival_data_scales_2 <- survival_data_scales %>% mutate(baseline_ledd=baseline_ledd/100)

cox_ledd <- coxph(Surv(time_months, event) ~ baseline_ledd, data = survival_data_scales_2)
summary(cox_ledd)

tidy_ledd <- tidy(cox_ledd, conf.int = TRUE)

# Add to results
ledd_result <- tibble(
  scale = "baseline_ledd",
  hr = exp(tidy_ledd$estimate),
  hr_lower = exp(tidy_ledd$conf.low),
  hr_upper = exp(tidy_ledd$conf.high),
  p_value = tidy_ledd$p.value,
  concordance = cox_ledd$concordance[1]
)





demo_vars <- dm %>% select(USUBJID, VSDATD1, BRTHDAT, SEX, DMHIPDDAT) %>%
  left_join(sc %>% filter(VISITNUM==1) %>%
  select(USUBJID, SCHOYA))

demo_vars %>% group_by(SEX) %>% count()

demo_calculated <- demo_vars %>%
  mutate(
    # Parse birth date
    birth_year = as.numeric(str_extract(BRTHDAT, "\\d{4}")),
    birth_month_str = str_extract(BRTHDAT, "[A-Z]{3}"),
    # Convert to title case to match month.abb (e.g., "JUL" -> "Jul")
    birth_month_title = str_to_title(birth_month_str),
    birth_month = match(birth_month_title, month.abb),
    birth_date = as.Date(paste(birth_year, birth_month, "15", sep = "-"), format = "%Y-%m-%d"),
    
    # Parse PD diagnosis date (DMHIPDDAT)
    diag_year = as.numeric(str_extract(DMHIPDDAT, "\\d{4}")),
    diag_month_str = str_extract(DMHIPDDAT, "[A-Z]{3}"),
    diag_month_title = str_to_title(diag_month_str),
    diag_month = match(diag_month_title, month.abb),
    diag_date = as.Date(paste(diag_year, diag_month, "15", sep = "-"), format = "%Y-%m-%d"),
    
    # Calculate age at baseline
    age_years = as.numeric(difftime(VSDATD1, birth_date, units = "days")) / 365.25,
    age = floor(age_years),
    
    # Calculate disease duration at baseline
    disease_duration_years = as.numeric(difftime(VSDATD1, diag_date, units = "days")) / 365.25,
    disease_duration = round(disease_duration_years, 1),
    
    # Sex as factor
    sex_factor = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    
    # H&Y as numeric
    hy_stage = SCHOYA
  ) %>%
  select(USUBJID, age, disease_duration, sex_factor, hy_stage)

# Check results
demo_calculated %>% head(10)


survival_data_scales_2 <- survival_data_scales_2 %>%
  left_join(demo_calculated, by = "USUBJID")

summary(survival_data_scales_2[, c("age", "disease_duration", "hy_stage")])
table(survival_data_scales_2$sex_factor)

run_univariate_cox <- function(data, var_name) {
  formula <- as.formula(paste0("Surv(time_months, event) ~ ", var_name))
  cox_fit <- coxph(formula, data = data)
  tidy_cox <- tidy(cox_fit, conf.int = TRUE)
  
  if(nrow(tidy_cox) > 1) {
    wald_p <- coef(summary(cox_fit))[, "Pr(>|z|)"]  # This gives p for each level
    tibble(
      variable = var_name,
      level = tidy_cox$term,
      hr = exp(tidy_cox$estimate),
      hr_lower = exp(tidy_cox$conf.low),
      hr_upper = exp(tidy_cox$conf.high),
      p_value = tidy_cox$p.value
    )
  } else {
    tibble(
      variable = var_name,
      level = var_name,
      hr = exp(tidy_cox$estimate),
      hr_lower = exp(tidy_cox$conf.low),
      hr_upper = exp(tidy_cox$conf.high),
      p_value = tidy_cox$p.value
    )
  }
}

demo_vars_to_test <- c("age", "disease_duration", "sex_factor", "hy_stage")



demo_results <- map_df(demo_vars_to_test, function(v) {
  run_univariate_cox(survival_data_scales_2, v)
})


demo_results_clean <- demo_results %>%
  mutate(
    across(where(is.numeric), ~round(., 3)),
    hr_ci = paste0(hr, " [", hr_lower, "-", hr_upper, "]"),
    p_display = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(p_value),
      TRUE ~ as.character(p_value)
    )
  ) %>%
  select(variable, level, hr_ci, p_display)

print(demo_results_clean, n = Inf)




# Prepare demographic data for plotting
demo_forest_data <- demo_results %>%
  mutate(
    # Create clean variable names
    variable_clean = case_when(
      variable == "age" ~ "Age (per year)",
      variable == "disease_duration" ~ "Disease Duration (per year)",
      variable == "sex_factor" ~ "Sex (Female vs Male)",
      variable == "hy_stage" ~ "Hoehn & Yahr Stage"
    ),
    # For disease duration, we'll keep the HR but truncate the CI visually
    # The actual CI is [0.11-202] but we'll cap the upper bound for plotting
    hr_upper_plot = ifelse(variable == "disease_duration", 8, hr_upper),
    # Add significance star
    sig_star = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Format HR for display
    hr_display = case_when(
      variable == "disease_duration" ~ "4.76 [0.11-202.0]",
      TRUE ~ paste0(round(hr, 2), " [", round(hr_lower, 2), "-", round(hr_upper, 2), "]")
    ),
    p_display = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      p_value < 0.05 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    )
  ) %>%
  # Order by p-value or logical order
  mutate(variable_clean = factor(variable_clean, 
                                  levels = c("Age (per year)",
                                             "Sex (Female vs Male)",
                                             "Hoehn & Yahr Stage",
                                             "Disease Duration (per year)")))


# Forest plot for demographic variables
demo_plot <- ggplot(demo_forest_data, 
                    aes(x = hr, y = variable_clean)) +
  # Lineranges for CI (using truncated upper bound for disease duration)
  geom_linerange(aes(xmin = hr_lower, xmax = hr_upper_plot, y = variable_clean),
                 size = 2, alpha = 0.7, lineend = "round", color = "#7c626c") +
  # Point estimate
  geom_point(size = 2, stroke = 3, shape = 1, color = "#7c626c") +
  # Vertical line at HR=1
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  # Add HR values as text on left
  geom_text(aes(x =-1.5, label = hr_display), 
            size = 3.5, hjust = 0) +
  # Add p-values
  geom_text(aes(x = 7.5, label = paste0("p-val = ", p_display)), 
            size = 3.5, hjust = 0) +
  # Add note about truncated CI for disease duration
  annotate("text", x = 3.5, y = 1, 
           label = "† CI extends to 202.0", 
           size = 3, hjust = 0) +
  # Labels
  labs(
    title = "Demographic Variables as Predictors of Time to First LEDD Increase",
    subtitle = "Hazard ratios from univariate Cox models",
    x = "\n Hazard Ratio (95% CI)",
    y = "",
    caption = "† Confidence interval truncated for visualization"
  ) +
  # X-axis breaks (set breaks but DON'T set limits here)
  scale_x_continuous(breaks = c(-2,0, 1, 2, 4, 6, 8)) +
  # Use coord_cartesian to zoom in without clipping
  coord_cartesian(xlim = c(-2.0, 8.5)) +
  # Theme - matching previous style
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, face = "bold", hjust = 1),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust = -0.1),
        axis.text.y = element_text(size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
        plot.margin = margin(10, 30, 10, 10, "pt")) 

print(demo_plot)

ggsave("../out/ledd_demo_forest.svg", demo_plot, width = 10, height = 3.0)



