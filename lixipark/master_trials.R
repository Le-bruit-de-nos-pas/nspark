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
groupe %>% group_by(RAN_GRP_LIB) %>% count() # 78 * 78

sc <- sc %>%
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


sc <- sc %>%
  mutate(mds_partIII_missing = rowSums(is.na(across(c(SCMDS3_1, SCMDS3_2, 
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

sc <- sc %>%  filter(mds_partIII_missing<33 & VISITNUM != 92) %>% 
  select(USUBJID, VISITNUM, VISITNAM, mds_partIII_total, mds_partIII_missing) 
 
unique(sc$VISITNAM)
unique(sc$VISITNUM)

drop <- sc  %>% select(USUBJID, VISITNAM, mds_partIII_total) %>% spread(key=VISITNAM, value=mds_partIII_total)


sc_with_trt <- sc %>%
  inner_join(groupe %>% select(USUBJID, RAN_GRP_LIB), by = "USUBJID") %>%
  filter(RAN_GRP_LIB == "1 - Placebo") 


sc_with_trt <- sc_with_trt %>%
  arrange(USUBJID, VISITNUM) %>%
  group_by(USUBJID) %>%
  mutate(
    baseline_mds = mds_partIII_total[VISITNUM == 2],
    mds_diff = mds_partIII_total - baseline_mds,
    milestone_3 = mds_diff >= 3,
    milestone_4 = mds_diff >= 4,
    milestone_5 = mds_diff >= 5,
    time_months = case_when(
      VISITNUM == 2 ~ 0,
      VISITNUM == 6 ~ 6,
      VISITNUM == 8 ~ 12,
      VISITNUM == 91 ~ 14
    )
  ) %>%
  ungroup() %>%
  select(USUBJID, VISITNAM, baseline_mds, mds_diff, milestone_3:milestone_5, time_months)


milestone_proportions <- sc_with_trt %>%
  group_by(VISITNAM, time_months) %>%
  summarise(
    n = n(),
    prop_milestone_3 = mean(milestone_3, na.rm = TRUE) * 100,
    prop_milestone_4 = mean(milestone_4, na.rm = TRUE) * 100,
    prop_milestone_5 = mean(milestone_5, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(milestone_proportions)


library(binom)

milestone_proportions_ci <- sc_with_trt %>%
  group_by(VISITNAM, time_months) %>%
  summarise(
    n = n(),
    n_m3 = sum(milestone_3, na.rm = TRUE),
    n_m4 = sum(milestone_4, na.rm = TRUE),
    n_m5 = sum(milestone_5, na.rm = TRUE),
    prop_m3 = n_m3 / n * 100,
    prop_m4 = n_m4 / n * 100,
    prop_m5 = n_m5 / n * 100,
    # Intervalles de confiance exacts (Clopper-Pearson)
    ci_m3_lower = binom.confint(n_m3, n, methods = "exact")$lower * 100,
    ci_m3_upper = binom.confint(n_m3, n, methods = "exact")$upper * 100,
    ci_m4_lower = binom.confint(n_m4, n, methods = "exact")$lower * 100,
    ci_m4_upper = binom.confint(n_m4, n, methods = "exact")$upper * 100,
    ci_m5_lower = binom.confint(n_m5, n, methods = "exact")$lower * 100,
    ci_m5_upper = binom.confint(n_m5, n, methods = "exact")$upper * 100,
    .groups = "drop"
  )

print(milestone_proportions_ci)




plot_milestones <- milestone_proportions %>%
  pivot_longer(cols = starts_with("prop_milestone"), 
               names_to = "milestone", 
               values_to = "proportion") %>%
  mutate(
    milestone = case_when(
      milestone == "prop_milestone_3" ~ "≥ +3 points",
      milestone == "prop_milestone_4" ~ "≥ +4 points",
      milestone == "prop_milestone_5" ~ "≥ +5 points"
    ),
    milestone = factor(milestone, levels = c("≥ +3 points", "≥ +4 points", "≥ +5 points"))
  ) %>%
  ggplot(aes(x = time_months, y = proportion, color = milestone, group = milestone)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape=1, stroke=2) +
  scale_color_manual(values = c("≥ +3 points" = "#73B7BF", 
                                 "≥ +4 points" = "#0E1157", 
                                 "≥ +5 points" = "#731A34")) +
  scale_x_continuous(breaks = c(0, 6, 12, 14), limits = c(0, 14)) +
  labs(
    title = "Proportion of Placebo Patients ~ Motor Milestone",
    subtitle = "MDS-UPDRS Part III (ON) Score Increases from Baseline\n",
    x = "\n Months from baseline",
    y = "Patients (%) \n",
    color = "Milestone"
  ) +
   theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


print(plot_milestones)

ggsave("../out/placebo_milestones_proportions.svg", plot_milestones, width = 5, height = 5)

milestone_table <- milestone_proportions_ci %>%
  select(VISITNAM, time_months, n, 
         prop_m3, ci_m3_lower, ci_m3_upper,
         prop_m4, ci_m4_lower, ci_m4_upper,
         prop_m5, ci_m5_lower, ci_m5_upper) %>%
  mutate(
    m3_display = paste0(round(prop_m3, 1), "% [", round(ci_m3_lower, 1), "-", round(ci_m3_upper, 1), "]"),
    m4_display = paste0(round(prop_m4, 1), "% [", round(ci_m4_lower, 1), "-", round(ci_m4_upper, 1), "]"),
    m5_display = paste0(round(prop_m5, 1), "% [", round(ci_m5_lower, 1), "-", round(ci_m5_upper, 1), "]")
  ) %>%
  select(VISITNAM, time_months, n, m3_display, m4_display, m5_display)

print(milestone_table)




milestone_times <- sc_with_trt %>%
  group_by(USUBJID) %>%
  summarise(
    baseline_mds = first(baseline_mds),
    # Temps jusqu'au premier milestone 3
    time_m3 = min(time_months[milestone_3 == TRUE & time_months > 0], na.rm = TRUE),
    event_m3 = !is.infinite(time_m3),
    time_m3 = ifelse(event_m3, time_m3, 14),  # Censored à 14 mois si non atteint
    # Temps jusqu'au premier milestone 4
    time_m4 = min(time_months[milestone_4 == TRUE & time_months > 0], na.rm = TRUE),
    event_m4 = !is.infinite(time_m4),
    time_m4 = ifelse(event_m4, time_m4, 14),
    # Temps jusqu'au premier milestone 5
    time_m5 = min(time_months[milestone_5 == TRUE & time_months > 0], na.rm = TRUE),
    event_m5 = !is.infinite(time_m5),
    time_m5 = ifelse(event_m5, time_m5, 14),
    .groups = "drop"
  ) %>%
  filter(!is.na(baseline_mds))


library(survival)
library(survminer)






# Combined plot with all three milestones on one graph

# Create a combined survival object with milestone as strata
milestone_combined <- milestone_times %>%
  pivot_longer(
    cols = c(time_m3, time_m4, time_m5),
    names_to = "milestone_type",
    values_to = "time"
  ) %>%
  mutate(
    event = case_when(
      milestone_type == "time_m3" ~ event_m3,
      milestone_type == "time_m4" ~ event_m4,
      milestone_type == "time_m5" ~ event_m5
    ),
    milestone_label = case_when(
      milestone_type == "time_m3" ~ "≥ +3 points",
      milestone_type == "time_m4" ~ "≥ +4 points",
      milestone_type == "time_m5" ~ "≥ +5 points"
    ),
    milestone_label = factor(milestone_label, 
                             levels = c("≥ +3 points", "≥ +4 points", "≥ +5 points"))
  ) %>%
  filter(!is.na(time))

# Fit combined KM
km_combined <- survfit(Surv(time, event) ~ milestone_label, data = milestone_combined)

# Combined plot
plot_combined <- ggsurvplot(
  km_combined,
  data = milestone_combined,
  conf.int = TRUE,
  conf.int.style = "ribbon",
  risk.table = TRUE,
  risk.table.col = "strata",
  risk.table.height = 0.30,
  pval = TRUE,               # Add log-rank p-value
  pval.method = TRUE,
  surv.median.line = "hv",
  # Add censoring ticks
  censor = TRUE,
  censor.shape = "|",
  censor.size = 4,
  xlab = "Time (months)",
  ylab = "Progression-Free Probability",
  title = "Time to First Motor Progression Milestone",
  subtitle = "Placebo group - MDS-UPDRS Part III (ON medication)",
  legend.title = "Milestone\n",
  legend.labs = c("≥ +3 points", "≥ +4 points", "≥ +5 points"),
  palette = c("≥ +3 points" = "#73B7BF", 
              "≥ +4 points" = "#0E1157", 
              "≥ +5 points" = "#731A34"),
  ggtheme = theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
)

print(plot_combined)
ggsave("../out/placebo_milestones_combined_survival.svg", plot_combined$plot, width = 6, height = 6)
ggsave("../out/placebo_milestones_combined_risk_table.svg", plot_combined$table, width = 6, height = 3)




# Define individual KM fits for quantile extraction
km_m3 <- survfit(Surv(time_m3, event_m3) ~ 1, data = milestone_times)
km_m4 <- survfit(Surv(time_m4, event_m4) ~ 1, data = milestone_times)
km_m5 <- survfit(Surv(time_m5, event_m5) ~ 1, data = milestone_times)

# Summary table with key quantiles
summary_quantiles <- function(km_fit, milestone_name) {
  s <- summary(km_fit, times = c(6, 12, 14))
  tibble(
    milestone = milestone_name,
    time = s$time,
    survival = s$surv,
    cum_incidence = 1 - s$surv,
    lower_ci = 1 - s$upper,
    upper_ci = 1 - s$lower,
    n_risk = s$n.risk
  )
}

quantile_table <- bind_rows(
  summary_quantiles(km_m3, "≥ +3 points"),
  summary_quantiles(km_m4, "≥ +4 points"),
  summary_quantiles(km_m5, "≥ +5 points")
) %>%
  mutate(
    cum_inc_display = paste0(round(cum_incidence * 100, 1), "% [", 
                              round(lower_ci * 100, 1), "-", 
                              round(upper_ci * 100, 1), "]")
  ) %>%
  select(milestone, time, n_risk, cum_inc_display)

print(quantile_table, n = Inf)
















# LEDDS



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
  filter(VISITNUM > 2, VISITNUM < 92) %>%  drop_na() %>%
  select(USUBJID, VISITNUM, VISITNAM, visit_date = VSDATD1) %>%
  arrange(USUBJID, visit_date)



ledd_changes <- followup_visits %>%
  left_join(baseline_data, by = "USUBJID") %>%
  mutate(
    visit_ledd = map2_dbl(
      USUBJID, visit_date,
      ~calculate_ledd_at_date(.x, .y, pt_clean)
    ),
    ledd_increased = visit_ledd > (baseline_ledd+100)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    first_increase = ledd_increased & !duplicated(ledd_increased & ledd_increased)
  ) %>%
  ungroup()

ledd_changes %>% summarise(mean=mean(baseline_ledd ))

ledd_changes %>% filter(VISITNUM==8) %>%
  summarise(mean=mean(visit_ledd))




survival_data <- ledd_changes %>%
    inner_join(groupe) %>%
  filter(RAN_GRP_LIB=="1 - Placebo") %>%
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



time_monthssurvival_data <- survival_data %>%
  mutate(
    time_months = round(time_months),
    time_months = ifelse(time_months == 0, 1, time_months)
  )



library(survival)
library(survminer)

survival_data %>%
  summarise(
    n = n(),
    events = sum(event),
    censored = n() - events,
    event_pct = mean(event) * 100,
    median_time = median(time_months[event == 1], na.rm = TRUE)
  )

#       n events censored event_pct median_time
# 1    78     17       61      21.8        9.20

km_fit <- survfit(Surv(time_months, event) ~ 1, data = survival_data)

# Print summary
summary(km_fit)
print(km_fit)

km_plot <- ggsurvplot(
  km_fit,
  data = survival_data,
  conf.int = TRUE,
  conf.int.style = "ribbon",
  risk.table = TRUE,
  palette = c("#7c626c"),  
  xlab = "\n Time (months)",
  ylab = "Probability of no >100 mg LEDD increase \n",
  title = "Time to First >100 mg LEDD Increase in the Placebo Group",
  subtitle = paste0("n = ", nrow(survival_data), " subjects"),
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


ledd_milestone <- ledd_changes %>%
  inner_join(groupe, by = "USUBJID") %>%
  filter(RAN_GRP_LIB == "1 - Placebo") %>%  # Placebo only
  mutate(
    ledd_milestone = visit_ledd > (baseline_ledd + 100)
  )



# 1. NAÏVE PROPORTIONS AT EACH VISIT
ledd_proportions <- ledd_milestone %>%
  group_by(VISITNAM, VISITNUM) %>%
  summarise(
    n = n(),
    n_met = sum(ledd_milestone, na.rm = TRUE),
    proportion = n_met / n * 100,
    .groups = "drop"
  ) %>%
  arrange(VISITNUM) %>%
  mutate(
    time_months = case_when(
      VISITNUM == 2 ~ 0,
      VISITNUM == 6 ~ 6,
      VISITNUM == 8 ~ 12,
      VISITNUM == 91 ~ 14
    )
  )

print("Naïve proportions at each visit:")
print(ledd_proportions)



# NAÏVE PROPORTIONS WITH CONFIDENCE INTERVALS
ledd_proportions_ci <- ledd_milestone %>%
  group_by(VISITNAM, VISITNUM, time_months = case_when(
    VISITNUM == 2 ~ 0,
    VISITNUM == 6 ~ 6,
    VISITNUM == 8 ~ 12,
    VISITNUM == 91 ~ 14
  )) %>%
  summarise(
    n = n(),
    n_met = sum(ledd_milestone, na.rm = TRUE),
    prop = n_met / n * 100,
    ci_lower = binom.confint(n_met, n, methods = "exact")$lower * 100,
    ci_upper = binom.confint(n_met, n, methods = "exact")$upper * 100,
    .groups = "drop"
  )

print("Naïve proportions with 95% CI:")
print(ledd_proportions_ci)




# CUMULATIVE INCIDENCE (from survival analysis)
# Using the survival_data you already created
cumulative_incidence <- survfit(Surv(time_months, event) ~ 1, data = survival_data)

# Get cumulative incidence at specific timepoints
cuminc_at_times <- summary(cumulative_incidence, times = c(6, 12, 14))

cuminc_table <- tibble(
  time_months = cuminc_at_times$time,
  n_risk = cuminc_at_times$n.risk,
  n_events = cuminc_at_times$n.event,
  cum_events = cumsum(cuminc_at_times$n.event),
  cum_incidence = 1 - cuminc_at_times$surv,
  cum_incidence_lower = 1 - cuminc_at_times$upper,
  cum_incidence_upper = 1 - cuminc_at_times$lower
) %>%
  mutate(
    cum_inc_display = paste0(round(cum_incidence * 100, 1), "% [", 
                              round(cum_incidence_lower * 100, 1), "-", 
                              round(cum_incidence_upper * 100, 1), "]")
  )

print("Cumulative incidence (Kaplan-Meier):")
print(cuminc_table)

# 4. COMBINED TABLE (Naïve vs Cumulative)
combined_table <- ledd_proportions_ci %>%
  select(time_months, n, prop, ci_lower, ci_upper) %>%
  mutate(
    naive_display = paste0(round(prop, 1), "% [", round(ci_lower, 1), "-", round(ci_upper, 1), "]")
  ) %>%
  left_join(
    cuminc_table %>% select(time_months, cum_inc_display, n_risk),
    by = "time_months"
  ) %>%
  select(time_months, n, naive_display, n_risk, cum_inc_display)

print("Combined comparison (Naïve vs Cumulative):")
print(combined_table)





# Naïve proportions vs Cumulative incidence
comparison_plot <- ggplot() +
  # Naïve proportions (points with error bars)
  geom_point(data = ledd_proportions_ci, 
             aes(x = time_months, y = prop, color = "Naïve proportion"), 
             size = 3, shape = 1, stroke = 1.5) +
  geom_errorbar(data = ledd_proportions_ci,
                aes(x = time_months, ymin = ci_lower, ymax = ci_upper, color = "Naïve proportion"),
                width = 0.5, alpha = 0.7) +
  # # Cumulative incidence (step line with ribbon)
  # geom_step(data = cuminc_table,
  #           aes(x = time_months, y = cum_incidence * 100, color = "Cumulative incidence"), 
  #           size = 1.2) +
  geom_ribbon(data = cuminc_table,
              aes(x = time_months, ymin = cum_incidence_lower * 100, ymax = cum_incidence_upper * 100,
                  fill = "Cumulative incidence"), alpha = 0.2) +
  scale_color_manual(values = c("Naïve proportion" = "#7c626c", "Cumulative incidence" = "#377eb8")) +
  scale_fill_manual(values = c("Cumulative incidence" = "#377eb8")) +
  scale_x_continuous(breaks = c(0, 6, 12, 14), limits = c(0, 15)) +
  labs(
    title = "LEDD Increase >100 mg: Naïve Proportions vs Cumulative Incidence",
    subtitle = "Placebo group only",
    x = "\n Time (months)",
    y = "Patients (%) \n",
    color = "Method",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(comparison_plot)

ggsave("../out/ledd_milestone_naive_vs_cuminc.svg", comparison_plot, width = 7, height = 5)

# 6. DETAILED TABLE FOR PAPER
ledd_milestone_table <- bind_rows(
  # Naïve proportions
  ledd_proportions_ci %>%
    mutate(
      method = "Naïve proportion",
      estimate = paste0(round(prop, 1), "% [", round(ci_lower, 1), "-", round(ci_upper, 1), "]")
    ) %>%
    select(time_months, method, n, estimate),
  # Cumulative incidence
  cuminc_table %>%
    mutate(
      method = "Cumulative incidence (KM)",
      n = n_risk,
      estimate = cum_inc_display
    ) %>%
    select(time_months, method, n, estimate)
) %>%
  arrange(time_months, method)

print("Publication-ready table:")
print(ledd_milestone_table, n = Inf)



# Check the exact event times
survival_data %>%
  filter(USUBJID %in% month6_patients) %>%
  select(USUBJID, time_months, event_date, baseline_date) %>%
  mutate(
    days_to_event = as.numeric(difftime(event_date, baseline_date, units = "days")),
    exact_months = days_to_event / 30.44
  )
