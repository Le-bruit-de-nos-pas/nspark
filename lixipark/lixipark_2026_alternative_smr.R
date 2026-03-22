

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





# COMPARE DELTAS AT EACH TIME POINT


# 12 months

# Create change scores from sc_wide
treatment_effect_data <- sc_wide %>%
  mutate(
    tremor_chg_12 = `tremor_mod_scale_Visit 8 - Follow-up` - `tremor_mod_scale_Visit 2 - Baseline`,
    non_tremor_chg_12 = `non_tremor_mod_scale_Visit 8 - Follow-up` - `non_tremor_mod_scale_Visit 2 - Baseline`,
    eleven_chg_12 = `eleven_item_mod_scale_Visit 8 - Follow-up` - `eleven_item_mod_scale_Visit 2 - Baseline`,
    mdsIII_chg_12 = `mds_partIII_total_Visit 8 - Follow-up` - `mds_partIII_total_Visit 2 - Baseline`
  ) %>%
  # Add treatment group
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  # Keep only relevant columns
  select(USUBJID, RAN_GRP_LIB, 
         tremor_chg_12, non_tremor_chg_12, 
         eleven_chg_12, mdsIII_chg_12) %>%
  drop_na()

treatment_effect_data %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(n = n())



calc_treatment_effect_size <- function(data, outcome_var) {
  
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[outcome_var]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[outcome_var]])
  
  n1 <- length(placebo)
  n2 <- length(lixi)
  
  mean1 <- mean(placebo, na.rm = TRUE)
  mean2 <- mean(lixi, na.rm = TRUE)
  sd1 <- sd(placebo, na.rm = TRUE)
  sd2 <- sd(lixi, na.rm = TRUE)
  
  mean_diff <- mean2 - mean1
  
  # 1. Cohen's d (pooled SD)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- mean_diff / pooled_sd
  
  se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2)))
  cohens_d_ci_lower <- cohens_d - 1.96 * se_d
  cohens_d_ci_upper <- cohens_d + 1.96 * se_d
  
  # 2. Hedges' g (bias correction)
  correction <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  hedges_g <- cohens_d * correction
  hedges_g_ci_lower <- cohens_d_ci_lower * correction
  hedges_g_ci_upper <- cohens_d_ci_upper * correction
  
  # 3. Glass's Δ (using placebo SD)
  glass_delta <- mean_diff / sd1
  se_glass <- sqrt((n1 + n2) / (n1 * n2) + glass_delta^2 / (2 * n1))
  glass_delta_ci_lower <- glass_delta - 1.96 * se_glass
  glass_delta_ci_upper <- glass_delta + 1.96 * se_glass
  
  tibble(
    outcome = outcome_var,
    n_placebo = n1,
    n_lixi = n2,
    mean_change_placebo = round(mean1, 2),
    mean_change_lixi = round(mean2, 2),
    mean_diff = round(mean_diff, 2),
    cohens_d = round(cohens_d, 3),
    cohens_d_ci_lower = round(cohens_d_ci_lower, 3),
    cohens_d_ci_upper = round(cohens_d_ci_upper, 3),
    hedges_g = round(hedges_g, 3),
    hedges_g_ci_lower = round(hedges_g_ci_lower, 3),
    hedges_g_ci_upper = round(hedges_g_ci_upper, 3),
    glass_delta = round(glass_delta, 3),
    glass_delta_ci_lower = round(glass_delta_ci_lower, 3),
    glass_delta_ci_upper = round(glass_delta_ci_upper, 3)
  )
}

treatment_effect_sizes <- bind_rows(
  calc_treatment_effect_size(treatment_effect_data, "tremor_chg_12"),
  calc_treatment_effect_size(treatment_effect_data, "non_tremor_chg_12"),
  calc_treatment_effect_size(treatment_effect_data, "eleven_chg_12"),
  calc_treatment_effect_size(treatment_effect_data, "mdsIII_chg_12")
)

print(treatment_effect_sizes, n = Inf)


#   outcome       n_placebo n_lixi mean_change_placebo mean_change_lixi mean_diff cohens_d cohens_d_ci_lower cohens_d_ci_upper hedges_g hedges_g_ci_lower hedges_g_ci_upper glass_delta glass_delta_ci_lower glass_delta_ci_upper
#   <chr>             <int>  <int>               <dbl>            <dbl>     <dbl>    <dbl>             <dbl>             <dbl>    <dbl>             <dbl>             <dbl>       <dbl>                <dbl>                <dbl>
# 1 tremor_chg_12        75     77                0.87             0.42     -0.45   -0.192            -0.511             0.127   -0.191            -0.508             0.126      -0.187               -0.507                0.132
# 2 non_tremor_c…        75     77                3.57             1.04     -2.53   -0.346            -0.666            -0.026   -0.344            -0.663            -0.026      -0.35                -0.673               -0.027
# 3 eleven_chg_12        75     77                1.37             1.3      -0.07   -0.022            -0.34              0.296   -0.022            -0.338             0.295      -0.024               -0.342                0.294
# 4 mdsIII_chg_12        75     77                3.04            -0.04     -3.08   -0.445            -0.767            -0.123   -0.443            -0.763            -0.123      -0.449               -0.775               -0.123


effect_size_plot_data <- treatment_effect_sizes %>%
  pivot_longer(
    cols = c(cohens_d, hedges_g, glass_delta),
    names_to = "metric",
    values_to = "effect_size"
  ) %>%
  mutate(
    ci_lower = case_when(
      metric == "cohens_d" ~ cohens_d_ci_lower,
      metric == "hedges_g" ~ hedges_g_ci_lower,
      metric == "glass_delta" ~ glass_delta_ci_lower
    ),
    ci_upper = case_when(
      metric == "cohens_d" ~ cohens_d_ci_upper,
      metric == "hedges_g" ~ hedges_g_ci_upper,
      metric == "glass_delta" ~ glass_delta_ci_upper
    )
  ) %>%
  mutate(
    scale = case_when(
      outcome == "tremor_chg_12" ~ "Tremor Scale",
      outcome == "non_tremor_chg_12" ~ "Non-Tremor Scale",
      outcome == "eleven_chg_12" ~ "11-Item Scale",
      outcome == "mdsIII_chg_12" ~ "MDS-UPDRS Part III"
    ),
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    metric = factor(metric, levels = c("cohens_d", "hedges_g", "glass_delta"),
                    labels = c("Cohen's d", "Hedges' g", "Glass's Δ"))
  ) %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

print(effect_size_plot_data, n = Inf)

effect_size_plot <- ggplot(effect_size_plot_data, 
                           aes(x = effect_size, y = scale)) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, y = scale),
                 size = 2, alpha = 0.7, lineend = "round", color = "#32435d") +
  geom_point(size = 3, stroke = 2, shape = 1, color = "#32435d") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title = "Treatment Effect Sizes at Month 12",
    subtitle = "Negative values favor lixisenatide (less worsening)",
    x = "\n Standardized Effect Size (95% CI)",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), 
                     limits = c(-1, 1)) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.margin = margin(10, 10, 10, 10, "pt")) 

print(effect_size_plot)

ggsave(file = "../out/treatment_effect_sizes_forest.svg",  plot = effect_size_plot, width = 8, height = 6)




# 6 months

# Create change scores from sc_wide
treatment_effect_data <- sc_wide %>%
  mutate(
    tremor_chg_6 = `tremor_mod_scale_Visit 6 - Follow-up` - `tremor_mod_scale_Visit 2 - Baseline`,
    non_tremor_chg_6 = `non_tremor_mod_scale_Visit 6 - Follow-up` - `non_tremor_mod_scale_Visit 2 - Baseline`,
    eleven_chg_6 = `eleven_item_mod_scale_Visit 6 - Follow-up` - `eleven_item_mod_scale_Visit 2 - Baseline`,
    mdsIII_chg_6 = `mds_partIII_total_Visit 6 - Follow-up` - `mds_partIII_total_Visit 2 - Baseline`
  ) %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  select(USUBJID, RAN_GRP_LIB, 
         tremor_chg_6, non_tremor_chg_6, 
         eleven_chg_6, mdsIII_chg_6) %>%
  drop_na()

treatment_effect_data %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(n = n())


calc_treatment_effect_size <- function(data, outcome_var) {
  
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[outcome_var]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[outcome_var]])
  
  n1 <- length(placebo)
  n2 <- length(lixi)
  
  mean1 <- mean(placebo, na.rm = TRUE)
  mean2 <- mean(lixi, na.rm = TRUE)
  sd1 <- sd(placebo, na.rm = TRUE)
  sd2 <- sd(lixi, na.rm = TRUE)
  
  mean_diff <- mean2 - mean1
  
  # 1. Cohen's d (pooled SD)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- mean_diff / pooled_sd
  
  se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2)))
  cohens_d_ci_lower <- cohens_d - 1.96 * se_d
  cohens_d_ci_upper <- cohens_d + 1.96 * se_d
  
  # 2. Hedges' g (bias correction)
  correction <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  hedges_g <- cohens_d * correction
  hedges_g_ci_lower <- cohens_d_ci_lower * correction
  hedges_g_ci_upper <- cohens_d_ci_upper * correction
  
  # 3. Glass's Δ (using placebo SD)
  glass_delta <- mean_diff / sd1
  se_glass <- sqrt((n1 + n2) / (n1 * n2) + glass_delta^2 / (2 * n1))
  glass_delta_ci_lower <- glass_delta - 1.96 * se_glass
  glass_delta_ci_upper <- glass_delta + 1.96 * se_glass
  
  tibble(
    outcome = outcome_var,
    n_placebo = n1,
    n_lixi = n2,
    mean_change_placebo = round(mean1, 2),
    mean_change_lixi = round(mean2, 2),
    mean_diff = round(mean_diff, 2),
    cohens_d = round(cohens_d, 3),
    cohens_d_ci_lower = round(cohens_d_ci_lower, 3),
    cohens_d_ci_upper = round(cohens_d_ci_upper, 3),
    hedges_g = round(hedges_g, 3),
    hedges_g_ci_lower = round(hedges_g_ci_lower, 3),
    hedges_g_ci_upper = round(hedges_g_ci_upper, 3),
    glass_delta = round(glass_delta, 3),
    glass_delta_ci_lower = round(glass_delta_ci_lower, 3),
    glass_delta_ci_upper = round(glass_delta_ci_upper, 3)
  )
}

treatment_effect_sizes <- bind_rows(
  calc_treatment_effect_size(treatment_effect_data, "tremor_chg_6"),
  calc_treatment_effect_size(treatment_effect_data, "non_tremor_chg_6"),
  calc_treatment_effect_size(treatment_effect_data, "eleven_chg_6"),
  calc_treatment_effect_size(treatment_effect_data, "mdsIII_chg_6")
)

print(treatment_effect_sizes, n = Inf)


#   outcome        n_placebo n_lixi mean_change_placebo mean_change_lixi mean_diff cohens_d cohens_d_ci_lower cohens_d_ci_upper hedges_g hedges_g_ci_lower hedges_g_ci_upper glass_delta glass_delta_ci_lower glass_delta_ci_upper
#   <chr>              <int>  <int>               <dbl>            <dbl>     <dbl>    <dbl>             <dbl>             <dbl>    <dbl>             <dbl>             <dbl>       <dbl>                <dbl>                <dbl>
# 1 tremor_chg_6          73     68                0.74            -0.12     -0.86   -0.415            -0.749            -0.081   -0.413            -0.745            -0.081      -0.405               -0.742               -0.069
# 2 non_tremor_ch…        73     68                1.52             1.32     -0.2    -0.03             -0.36              0.3     -0.03             -0.358             0.299      -0.032               -0.363                0.298
# 3 eleven_chg_6          73     68                0.55             0.56      0.01    0.004            -0.327             0.334    0.004            -0.325             0.332       0.004               -0.326                0.334
# 4 mdsIII_chg_6          73     68                1.6              0.54     -1.06   -0.177            -0.508             0.154   -0.177            -0.506             0.153      -0.183               -0.514                0.149

effect_size_plot_data <- treatment_effect_sizes %>%
  pivot_longer(
    cols = c(cohens_d, hedges_g, glass_delta),
    names_to = "metric",
    values_to = "effect_size"
  ) %>%
  mutate(
    ci_lower = case_when(
      metric == "cohens_d" ~ cohens_d_ci_lower,
      metric == "hedges_g" ~ hedges_g_ci_lower,
      metric == "glass_delta" ~ glass_delta_ci_lower
    ),
    ci_upper = case_when(
      metric == "cohens_d" ~ cohens_d_ci_upper,
      metric == "hedges_g" ~ hedges_g_ci_upper,
      metric == "glass_delta" ~ glass_delta_ci_upper
    )
  ) %>%
  mutate(
    scale = case_when(
      outcome == "tremor_chg_6" ~ "Tremor Scale",
      outcome == "non_tremor_chg_6" ~ "Non-Tremor Scale",
      outcome == "eleven_chg_6" ~ "11-Item Scale",
      outcome == "mdsIII_chg_6" ~ "MDS-UPDRS Part III"
    ),
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    metric = factor(metric, levels = c("cohens_d", "hedges_g", "glass_delta"),
                    labels = c("Cohen's d", "Hedges' g", "Glass's Δ"))
  ) %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

print(effect_size_plot_data, n = Inf)

effect_size_plot <- ggplot(effect_size_plot_data, 
                           aes(x = effect_size, y = scale)) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, y = scale),
                 size = 2, alpha = 0.7, lineend = "round", color = "#7c626c") +
  geom_point(size = 3, stroke = 2, shape = 1, color = "#7c626c") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title = "Treatment Effect Sizes at Month 6",
    subtitle = "Negative values favor lixisenatide (less worsening)",
    x = "\n Standardized Effect Size (95% CI)",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), 
                     limits = c(-1, 1)) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.margin = margin(10, 10, 10, 10, "pt")) 

print(effect_size_plot)

ggsave(file = "../out/treatment_effect_sizes_forest.svg",  plot = effect_size_plot, width = 8, height = 6)



















# COMPARE ABSOLUTE SCORES AT EACH TIME POINT

# 12 month


#  Create dataset with absolute scores at Month 12
absolute_scores_data <- sc_wide %>%
  select(USUBJID,
         tremor_m12 = `tremor_mod_scale_Visit 8 - Follow-up`,
         non_tremor_m12 = `non_tremor_mod_scale_Visit 8 - Follow-up`,
         eleven_m12 = `eleven_item_mod_scale_Visit 8 - Follow-up`,
         mdsIII_m12 = `mds_partIII_total_Visit 8 - Follow-up`) %>%
  # Add treatment group
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  drop_na()

absolute_scores_data %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(n = n())

calc_absolute_effect_size_numeric <- function(data, outcome_var) {
  
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[outcome_var]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[outcome_var]])
  
  n1 <- length(placebo)
  n2 <- length(lixi)
  
  mean1 <- mean(placebo, na.rm = TRUE)
  mean2 <- mean(lixi, na.rm = TRUE)
  sd1 <- sd(placebo, na.rm = TRUE)
  sd2 <- sd(lixi, na.rm = TRUE)
  
  mean_diff <- mean2 - mean1
  
  # 1. Cohen's d (pooled SD)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- mean_diff / pooled_sd
  
  se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2)))
  cohens_d_ci_lower <- cohens_d - 1.96 * se_d
  cohens_d_ci_upper <- cohens_d + 1.96 * se_d
  
  # 2. Hedges' g (bias correction)
  correction <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  hedges_g <- cohens_d * correction
  hedges_g_ci_lower <- cohens_d_ci_lower * correction
  hedges_g_ci_upper <- cohens_d_ci_upper * correction
  
  # 3. Glass's Δ (using placebo SD)
  glass_delta <- mean_diff / sd1
  se_glass <- sqrt((n1 + n2) / (n1 * n2) + glass_delta^2 / (2 * n1))
  glass_delta_ci_lower <- glass_delta - 1.96 * se_glass
  glass_delta_ci_upper <- glass_delta + 1.96 * se_glass
  
  tibble(
    outcome = outcome_var,
    n_placebo = n1,
    n_lixi = n2,
    mean_placebo = round(mean1, 2),
    mean_lixi = round(mean2, 2),
    mean_diff = round(mean_diff, 2),
    cohens_d = round(cohens_d, 3),
    cohens_d_ci_lower = round(cohens_d_ci_lower, 3),
    cohens_d_ci_upper = round(cohens_d_ci_upper, 3),
    hedges_g = round(hedges_g, 3),
    hedges_g_ci_lower = round(hedges_g_ci_lower, 3),
    hedges_g_ci_upper = round(hedges_g_ci_upper, 3),
    glass_delta = round(glass_delta, 3),
    glass_delta_ci_lower = round(glass_delta_ci_lower, 3),
    glass_delta_ci_upper = round(glass_delta_ci_upper, 3)
  )
}

absolute_effect_sizes_numeric <- bind_rows(
  calc_absolute_effect_size_numeric(absolute_scores_data, "tremor_m12"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "non_tremor_m12"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "eleven_m12"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "mdsIII_m12")
)


#   outcome        n_placebo n_lixi mean_placebo mean_lixi mean_diff cohens_d cohens_d_ci_lower cohens_d_ci_upper hedges_g hedges_g_ci_lower hedges_g_ci_upper glass_delta glass_delta_ci_lower glass_delta_ci_upper
#   <chr>              <int>  <int>        <dbl>     <dbl>     <dbl>    <dbl>             <dbl>             <dbl>    <dbl>             <dbl>             <dbl>       <dbl>                <dbl>                <dbl>
# 1 tremor_m12            76     77         4.91      3.58     -1.32   -0.357            -0.677            -0.038   -0.355            -0.673            -0.038      -0.32                -0.641                0.001
# 2 non_tremor_m12        76     77        20.1      17.9      -2.22   -0.203            -0.521             0.115   -0.202            -0.518             0.114      -0.194               -0.512                0.124
# 3 eleven_m12            76     77         5.59      5.69      0.1     0.02             -0.297             0.337    0.02             -0.295             0.335       0.021               -0.296                0.338
# 4 mdsIII_m12            76     77        18.5      14.9      -3.62   -0.414            -0.735            -0.094   -0.412            -0.731            -0.093      -0.366               -0.689               -0.044


absolute_plot_data <- absolute_effect_sizes_numeric %>%
  pivot_longer(
    cols = c(cohens_d, hedges_g, glass_delta),
    names_to = "metric",
    values_to = "effect_size"
  ) %>%
  mutate(
    ci_lower = case_when(
      metric == "cohens_d" ~ cohens_d_ci_lower,
      metric == "hedges_g" ~ hedges_g_ci_lower,
      metric == "glass_delta" ~ glass_delta_ci_lower
    ),
    ci_upper = case_when(
      metric == "cohens_d" ~ cohens_d_ci_upper,
      metric == "hedges_g" ~ hedges_g_ci_upper,
      metric == "glass_delta" ~ glass_delta_ci_upper
    ),
    scale = case_when(
      outcome == "tremor_m12" ~ "Tremor Scale",
      outcome == "non_tremor_m12" ~ "Non-Tremor Scale",
      outcome == "eleven_m12" ~ "11-Item Scale",
      outcome == "mdsIII_m12" ~ "MDS-UPDRS Part III"
    ),
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    metric = factor(metric, levels = c("cohens_d", "hedges_g", "glass_delta"),
                    labels = c("Cohen's d", "Hedges' g", "Glass's Δ"))
  ) %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

absolute_plot <- ggplot(absolute_plot_data, 
                        aes(x = effect_size, y = scale)) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, y = scale),
                 size = 2, alpha = 0.7, lineend = "round", color = "#32435d") +
  geom_point(size = 3, stroke = 2, shape = 1, color = "#32435d") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title = "Treatment Effect Sizes at Month 12 (Absolute Scores)",
    subtitle = "Negative values favor lixisenatide (lower scores at Month 12)",
    x = "\n Standardized Effect Size (95% CI)",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), 
                     limits = c(-1, 1)) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.margin = margin(10, 10, 10, 10, "pt")) 

print(absolute_plot)

ggsave(file = "../out/absolute_scores_effect_sizes_forest.svg", 
       plot = absolute_plot, width = 8, height = 6)



# 6 months

# Create dataset with absolute scores at Month 12
absolute_scores_data <- sc_wide %>%
  select(USUBJID,
         tremor_m6 = `tremor_mod_scale_Visit 6 - Follow-up`,
         non_tremor_m6 = `non_tremor_mod_scale_Visit 6 - Follow-up`,
         eleven_m6 = `eleven_item_mod_scale_Visit 6 - Follow-up`,
         mdsIII_m6 = `mds_partIII_total_Visit 6 - Follow-up`) %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  drop_na()

absolute_scores_data %>%
  group_by(RAN_GRP_LIB) %>%
  summarise(n = n())

calc_absolute_effect_size_numeric <- function(data, outcome_var) {
  
  placebo <- data %>% filter(RAN_GRP_LIB == "1 - Placebo") %>% pull(.data[[outcome_var]])
  lixi <- data %>% filter(RAN_GRP_LIB == "2 - Lixisenatide") %>% pull(.data[[outcome_var]])
  
  n1 <- length(placebo)
  n2 <- length(lixi)
  
  mean1 <- mean(placebo, na.rm = TRUE)
  mean2 <- mean(lixi, na.rm = TRUE)
  sd1 <- sd(placebo, na.rm = TRUE)
  sd2 <- sd(lixi, na.rm = TRUE)
  
  mean_diff <- mean2 - mean1
  
  # 1. Cohen's d (pooled SD)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- mean_diff / pooled_sd
  
  se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2)))
  cohens_d_ci_lower <- cohens_d - 1.96 * se_d
  cohens_d_ci_upper <- cohens_d + 1.96 * se_d
  
  # 2. Hedges' g (bias correction)
  correction <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  hedges_g <- cohens_d * correction
  hedges_g_ci_lower <- cohens_d_ci_lower * correction
  hedges_g_ci_upper <- cohens_d_ci_upper * correction
  
  # 3. Glass's Δ (using placebo SD)
  glass_delta <- mean_diff / sd1
  se_glass <- sqrt((n1 + n2) / (n1 * n2) + glass_delta^2 / (2 * n1))
  glass_delta_ci_lower <- glass_delta - 1.96 * se_glass
  glass_delta_ci_upper <- glass_delta + 1.96 * se_glass
  
  tibble(
    outcome = outcome_var,
    n_placebo = n1,
    n_lixi = n2,
    mean_placebo = round(mean1, 2),
    mean_lixi = round(mean2, 2),
    mean_diff = round(mean_diff, 2),
    cohens_d = round(cohens_d, 3),
    cohens_d_ci_lower = round(cohens_d_ci_lower, 3),
    cohens_d_ci_upper = round(cohens_d_ci_upper, 3),
    hedges_g = round(hedges_g, 3),
    hedges_g_ci_lower = round(hedges_g_ci_lower, 3),
    hedges_g_ci_upper = round(hedges_g_ci_upper, 3),
    glass_delta = round(glass_delta, 3),
    glass_delta_ci_lower = round(glass_delta_ci_lower, 3),
    glass_delta_ci_upper = round(glass_delta_ci_upper, 3)
  )
}

absolute_effect_sizes_numeric <- bind_rows(
  calc_absolute_effect_size_numeric(absolute_scores_data, "tremor_m6"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "non_tremor_m6"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "eleven_m6"),
  calc_absolute_effect_size_numeric(absolute_scores_data, "mdsIII_m6")
)


#  outcome       n_placebo n_lixi mean_placebo mean_lixi mean_diff cohens_d cohens_d_ci_lower cohens_d_ci_upper hedges_g hedges_g_ci_lower hedges_g_ci_upper glass_delta glass_delta_ci_lower glass_delta_ci_upper
#   <chr>             <int>  <int>        <dbl>     <dbl>     <dbl>    <dbl>             <dbl>             <dbl>    <dbl>             <dbl>             <dbl>       <dbl>                <dbl>                <dbl>
# 1 tremor_m6            74     68         4.82      3.04     -1.78   -0.523            -0.858            -0.188   -0.52             -0.853            -0.187      -0.461               -0.798               -0.123
# 2 non_tremor_m6        74     68        18.4      17.9      -0.55   -0.051            -0.38              0.278   -0.051            -0.378             0.277      -0.051               -0.38                 0.278
# 3 eleven_m6            74     68         4.86      4.75     -0.11   -0.026            -0.355             0.304   -0.026            -0.353             0.302      -0.025               -0.354                0.304
# 4 mdsIII_m6            74     68        17.3      15.4      -1.93   -0.22             -0.55              0.111   -0.219            -0.547             0.11       -0.211               -0.542                0.12 

absolute_plot_data <- absolute_effect_sizes_numeric %>%
  pivot_longer(
    cols = c(cohens_d, hedges_g, glass_delta),
    names_to = "metric",
    values_to = "effect_size"
  ) %>%
  mutate(
    ci_lower = case_when(
      metric == "cohens_d" ~ cohens_d_ci_lower,
      metric == "hedges_g" ~ hedges_g_ci_lower,
      metric == "glass_delta" ~ glass_delta_ci_lower
    ),
    ci_upper = case_when(
      metric == "cohens_d" ~ cohens_d_ci_upper,
      metric == "hedges_g" ~ hedges_g_ci_upper,
      metric == "glass_delta" ~ glass_delta_ci_upper
    ),
    scale = case_when(
      outcome == "tremor_m6" ~ "Tremor Scale",
      outcome == "non_tremor_m6" ~ "Non-Tremor Scale",
      outcome == "eleven_m6" ~ "11-Item Scale",
      outcome == "mdsIII_m6" ~ "MDS-UPDRS Part III"
    ),
    scale = factor(scale, levels = c("MDS-UPDRS Part III", "Non-Tremor Scale", 
                                      "Tremor Scale", "11-Item Scale")),
    metric = factor(metric, levels = c("cohens_d", "hedges_g", "glass_delta"),
                    labels = c("Cohen's d", "Hedges' g", "Glass's Δ"))
  ) %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

absolute_plot <- ggplot(absolute_plot_data, 
                        aes(x = effect_size, y = scale)) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, y = scale),
                 size = 2, alpha = 0.7, lineend = "round", color = "#7c626c") +
  geom_point(size = 3, stroke = 2, shape = 1, color = "#7c626c") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~metric, ncol = 1) +
  labs(
    title = "Treatment Effect Sizes at Month 6 (Absolute Scores)",
    subtitle = "Negative values favor lixisenatide (lower scores at Month 6)",
    x = "\n Standardized Effect Size (95% CI)",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8), 
                     limits = c(-1, 1)) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 11),
        plot.margin = margin(10, 10, 10, 10, "pt")) 

print(absolute_plot)

ggsave(file = "../out/absolute_scores_effect_sizes_forest.svg", 
       plot = absolute_plot, width = 8, height = 6)
