

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


# remove row where things are completely missing

sc_complete_missing <- sc %>%
  filter(tremor_items_missing == 11 | 
         non_tremor_items_missing == 35 | 
         eleven_item_missing == 11 |
         mds_partIII_missing == 33)



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
                           exclude_complete_partIII 
  )


# Dataset for primary analyses (excluding flagged rows)
sc_primary_analysis <- sc %>%
  filter(exclude_from_primary == FALSE)


sc_wide <- sc_primary_analysis %>%
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




# Create scaled/normalized version (using theoretical max)
sc_wide_norm <- sc_wide %>%
  mutate(
    # Tremor Scale (max 44)
    tremor_norm_2 = `tremor_mod_scale_Visit 2 - Baseline` / 44,
    tremor_norm_6 = `tremor_mod_scale_Visit 6 - Follow-up` / 44,
    tremor_norm_8 = `tremor_mod_scale_Visit 8 - Follow-up` / 44,
    
    # Non-Tremor Scale (max 104)
    non_tremor_norm_2 = `non_tremor_mod_scale_Visit 2 - Baseline` / 104,
    non_tremor_norm_6 = `non_tremor_mod_scale_Visit 6 - Follow-up` / 104,
    non_tremor_norm_8 = `non_tremor_mod_scale_Visit 8 - Follow-up` / 104,
    
    # 11-Item Scale (max 48)
    eleven_norm_2 = `eleven_item_mod_scale_Visit 2 - Baseline` / 48,
    eleven_norm_6 = `eleven_item_mod_scale_Visit 6 - Follow-up` / 48,
    eleven_norm_8 = `eleven_item_mod_scale_Visit 8 - Follow-up` / 48,
    
    # MDS-UPDRS Part III (max 132)
    mdsIII_norm_2 = `mds_partIII_total_Visit 2 - Baseline` / 132,
    mdsIII_norm_6 = `mds_partIII_total_Visit 6 - Follow-up` / 132,
    mdsIII_norm_8 = `mds_partIII_total_Visit 8 - Follow-up` / 132
  )




# Create normalized exact scores dataset for Month 12
absolute_scores_norm <- sc_wide_norm %>%
  select(USUBJID,
         tremor_m12 = tremor_norm_8,
         non_tremor_m12 = non_tremor_norm_8,
         eleven_m12 = eleven_norm_8,
         mdsIII_m12 = mdsIII_norm_8) %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  drop_na()

# Also for Month 6
absolute_scores_norm_6 <- sc_wide_norm %>%
  select(USUBJID,
         tremor_m6 = tremor_norm_6,
         non_tremor_m6 = non_tremor_norm_6,
         eleven_m6 = eleven_norm_6,
         mdsIII_m6 = mdsIII_norm_6) %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  drop_na()

baseline_scores_norm <- sc_wide_norm %>%
  select(USUBJID,
         tremor_bl = tremor_norm_2,
         non_tremor_bl = non_tremor_norm_2,
         eleven_bl = eleven_norm_2,
         mdsIII_bl = mdsIII_norm_2) %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  drop_na()


# Function to run Wilcoxon test and get summary stats
compare_scale <- function(data, scale_name, timepoint) {
  
  # Get values for each group
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[scale_name]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[scale_name]])
  
  # Summary statistics
  n_placebo <- length(placebo)
  n_lixi <- length(lixi)
  mean_placebo <- mean(placebo, na.rm = TRUE)
  mean_lixi <- mean(lixi, na.rm = TRUE)
  median_placebo <- median(placebo, na.rm = TRUE)
  median_lixi <- median(lixi, na.rm = TRUE)
  sd_placebo <- sd(placebo, na.rm = TRUE)
  sd_lixi <- sd(lixi, na.rm = TRUE)
  
  # Wilcoxon test
  wt <- wilcox.test(placebo, lixi, conf.int = TRUE, conf.level = 0.95)
  
  tibble(
    scale = scale_name,
    timepoint = timepoint,
    n_placebo = n_placebo,
    n_lixi = n_lixi,
    mean_placebo = round(mean_placebo, 3),
    mean_lixi = round(mean_lixi, 3),
    median_placebo = round(median_placebo, 3),
    median_lixi = round(median_lixi, 3),
    sd_placebo = round(sd_placebo, 3),
    sd_lixi = round(sd_lixi, 3),
    norm_placebo = round(mean_placebo , 3),
    norm_lixi = round(mean_lixi , 3),
    norm_median_diff = round((mean_lixi - mean_placebo) , 3),
    p_value = wt$p.value,
    hodges_lehmann  = wt$estimate,
    conf_low = wt$conf.int[1],
    conf_high = wt$conf.int[2]
  )
}



# Run the same compare_scale function for baseline
results_baseline <- bind_rows(
  compare_scale(baseline_scores_norm, "tremor_bl", "Baseline"),
  compare_scale(baseline_scores_norm, "non_tremor_bl", "Baseline"),
  compare_scale(baseline_scores_norm, "eleven_bl", "Baseline"),
  compare_scale(baseline_scores_norm, "mdsIII_bl", "Baseline")
)

# Run for all scales at Month 6
results_m6 <- bind_rows(
  compare_scale(absolute_scores_norm_6, "tremor_m6", "Month 6"),
  compare_scale(absolute_scores_norm_6, "non_tremor_m6", "Month 6"),
  compare_scale(absolute_scores_norm_6, "eleven_m6", "Month 6"),
  compare_scale(absolute_scores_norm_6, "mdsIII_m6", "Month 6")
)

# Run for all scales at Month 12
results_m12 <- bind_rows(
  compare_scale(absolute_scores_norm, "tremor_m12", "Month 12"),
  compare_scale(absolute_scores_norm, "non_tremor_m12", "Month 12"),
  compare_scale(absolute_scores_norm, "eleven_m12", "Month 12"),
  compare_scale(absolute_scores_norm, "mdsIII_m12", "Month 12")
)

# Combine results
all_results <- bind_rows(results_baseline, results_m6, results_m12) %>%
  mutate(
    scale_clean = case_when(
      scale == "tremor_bl" | scale == "tremor_m6" | scale == "tremor_m12" ~ "Tremor Scale",
      scale == "non_tremor_bl" | scale == "non_tremor_m6" | scale == "non_tremor_m12" ~ "Non-Tremor Scale",
      scale == "eleven_bl" | scale == "eleven_m6" | scale == "eleven_m12" ~ "11-Item Scale",
      scale == "mdsIII_bl" | scale == "mdsIII_m6" | scale == "mdsIII_m12" ~ "MDS-UPDRS Part III"
    ),
    scale_clean = factor(scale_clean, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                                  "Tremor Scale", "11-Item Scale")),
    timepoint = factor(timepoint, levels = c("Baseline", "Month 6", "Month 12")),
    p_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      p_value < 0.05 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    )
  ) %>%
  arrange(scale_clean, timepoint)

# View all results
print(all_results %>% 
  select(scale_clean, timepoint, n_placebo, n_lixi, 
         norm_placebo, norm_lixi, norm_median_diff , hodges_lehmann,
         p_formatted), n = Inf) 







# Calculate normalized change scores (deltas) for each patient
delta_normalized <- sc_wide_norm %>%
  # Calculate change from baseline to each timepoint
  mutate(
    # Month 6 changes
    tremor_delta_6 = tremor_norm_6 - tremor_norm_2,
    non_tremor_delta_6 = non_tremor_norm_6 - non_tremor_norm_2,
    eleven_delta_6 = eleven_norm_6 - eleven_norm_2,
    mdsIII_delta_6 = mdsIII_norm_6 - mdsIII_norm_2,
    
    # Month 12 changes
    tremor_delta_12 = tremor_norm_8 - tremor_norm_2,
    non_tremor_delta_12 = non_tremor_norm_8 - non_tremor_norm_2,
    eleven_delta_12 = eleven_norm_8 - eleven_norm_2,
    mdsIII_delta_12 = mdsIII_norm_8 - mdsIII_norm_2
  ) %>%
  # Add treatment group
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  # Keep relevant columns
  select(USUBJID, RAN_GRP_LIB,
         tremor_delta_6, non_tremor_delta_6, eleven_delta_6, mdsIII_delta_6,
         tremor_delta_12, non_tremor_delta_12, eleven_delta_12, mdsIII_delta_12) %>%
  drop_na()

# Function to compare deltas between groups using Wilcoxon
compare_deltas <- function(data, delta_var, timepoint, scale_name) {
  
  # Extract values for each group
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[delta_var]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[delta_var]])
  
  # Summary statistics
  n_placebo <- length(placebo)
  n_lixi <- length(lixi)
  mean_placebo <- mean(placebo, na.rm = TRUE)
  mean_lixi <- mean(lixi, na.rm = TRUE)
  median_placebo <- median(placebo, na.rm = TRUE)
  median_lixi <- median(lixi, na.rm = TRUE)
  sd_placebo <- sd(placebo, na.rm = TRUE)
  sd_lixi <- sd(lixi, na.rm = TRUE)
  
  # Wilcoxon test
  wt <- wilcox.test(placebo, lixi, conf.int = TRUE, conf.level = 0.95)
  
  tibble(
    scale = scale_name,
    timepoint = timepoint,
    n_placebo = n_placebo,
    n_lixi = n_lixi,
    mean_placebo = round(mean_placebo, 4),
    mean_lixi = round(mean_lixi, 4),
    median_placebo = round(median_placebo, 4),
    median_lixi = round(median_lixi, 4),
    sd_placebo = round(sd_placebo, 4),
    sd_lixi = round(sd_lixi, 4),
    norm_placebo = round(mean_placebo , 3),
    norm_lixi = round(mean_lixi , 3),
    norm_median_diff = round((mean_lixi - mean_placebo) , 2),
    p_value = wt$p.value,
    hodges_lehmann = wt$estimate,
    conf_low = wt$conf.int[1],
    conf_high = wt$conf.int[2]
  )
}

# Run for all scales at Month 6
results_delta_6 <- bind_rows(
  compare_deltas(delta_normalized, "tremor_delta_6", "Month 6", "Tremor Scale"),
  compare_deltas(delta_normalized, "non_tremor_delta_6", "Month 6", "Non-Tremor Scale"),
  compare_deltas(delta_normalized, "eleven_delta_6", "Month 6", "11-Item Scale"),
  compare_deltas(delta_normalized, "mdsIII_delta_6", "Month 6", "MDS-UPDRS Part III")
)

# Run for all scales at Month 12
results_delta_12 <- bind_rows(
  compare_deltas(delta_normalized, "tremor_delta_12", "Month 12", "Tremor Scale"),
  compare_deltas(delta_normalized, "non_tremor_delta_12", "Month 12", "Non-Tremor Scale"),
  compare_deltas(delta_normalized, "eleven_delta_12", "Month 12", "11-Item Scale"),
  compare_deltas(delta_normalized, "mdsIII_delta_12", "Month 12", "MDS-UPDRS Part III")
)

# Combine results
delta_results <- bind_rows(results_delta_6, results_delta_12) %>%
  mutate(
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    p_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ as.character(round(p_value, 3)),
      p_value < 0.05 ~ as.character(round(p_value, 3)),
      TRUE ~ as.character(round(p_value, 2))
    ),
    sig_star = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(timepoint, scale)

# View results
print(delta_results %>% 
  select(scale, timepoint, n_placebo, n_lixi, 
         norm_placebo, norm_lixi, norm_median_diff,
         p_formatted, sig_star, hodges_lehmann, conf_low, conf_high), n = Inf)

