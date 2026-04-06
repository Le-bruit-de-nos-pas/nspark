

library(haven)
library(tidyverse)
library(data.table)
library(lubridate)


admin <- read_sas("../data/admin.sas7bdat")

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

ecmp <- read_sas("../data/ecmp.sas7bdat")

ecmp <- ecmp %>% 
  mutate(
    ecmp_hypodopa = rowSums(select(., ECMP1:ECMP5), na.rm = TRUE),
    ecmp_icd_binary = if_else(rowSums(select(., ECMP12, ECMP17, ECMP18, ECMP19) >= 2, na.rm = TRUE) >= 1,1, 0),
    ecmp_icd_severity = rowSums(select(., ECMP12, ECMP17, ECMP18, ECMP19), na.rm = TRUE),
    ecmp_icb_binary = if_else(rowSums(select(., ECMP10:ECMP21) >= 1, na.rm = TRUE) >= 1,1, 0),
    ecmp_icb_severity = rowSums(select(., ECMP10:ECMP21), na.rm = TRUE)
  )

ecmp <- ecmp %>%
  inner_join(treat_pats_groups, by = "SUBJID")

ecmp <- ecmp %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

ecmp <- ecmp %>% filter(!is.na(ECMPDT))

ecmp %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  group_by(TREATMENT, VISIT) %>% summarise(mean=mean(ecmp_icd_binary))


dysk_pats <- fread("../out/dysk_pats.txt")

df <- ecmp %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))  %>%
  select(SUBJID, VISIT, ecmp_icd_binary) %>% mutate(SUBJID=as.numeric(SUBJID)) %>%
  inner_join(dysk_pats %>% select(-ADMREMNU, -GRP))  


df_long <- df %>%
  arrange(SUBJID, VISIT) %>%
  group_by(SUBJID) %>%
  mutate(
    start = lag(VISIT, default = 0),
    stop  = VISIT,
    # Events (occur at END of interval)
    ICD_event  = ecmp_icd_binary,
    dysk_event = MDS68,
    # Lagged exposures (state during interval)
    ICD_lag  = lag(ecmp_icd_binary, default = 0),
    dysk_lag = lag(MDS68, default = 0)
  ) %>%
  ungroup()


df_long <- df_long %>% filter(stop > 0)



df_long %>% filter(VISIT==3 & ICD_lag != 0) %>% select(SUBJID ) %>% distinct()

library(survival)



# Does ICD increase risk of later dyskinesia?

cox_dysk <- coxph(
  Surv(start, stop, dysk_event) ~ ICD_lag + TREATMENT,
  data = df_long
)

summary(cox_dysk)

# Does dyskinesia increase risk of later ICD?
cox_icd <- coxph(
  Surv(start, stop, ICD_event) ~ dysk_lag + TREATMENT,
  data = df_long
)

summary(cox_icd)


cox_interaction <- coxph(
  Surv(start, stop, dysk_event) ~ ICD_lag * TREATMENT,
  data = df_long
)

summary(cox_interaction)



df_ms <- df %>%
  mutate(
    state = case_when(
      ecmp_icd_binary == 0 & MDS68 == 0 ~ 0,
      ecmp_icd_binary == 1 & MDS68 == 0 ~ 1,
      ecmp_icd_binary == 0 & MDS68 == 1 ~ 2,
      ecmp_icd_binary == 1 & MDS68 == 1 ~ 3
    )
  )



# Step 1: Create patient-level summary data
df_patient <- df_ms %>%
  group_by(SUBJID) %>%
  summarize(
    ever_icd = max(ecmp_icd_binary, na.rm = TRUE),
    ever_dysk = max(MDS68, na.rm = TRUE),
    treatment = first(TREATMENT),
    # Get first onset times
    first_icd_time = if(any(ecmp_icd_binary == 1)) min(VISIT[ecmp_icd_binary == 1]) else NA,
    first_dysk_time = if(any(MDS68 == 1)) min(VISIT[MDS68 == 1]) else NA,
    last_visit = max(VISIT, na.rm = TRUE),
    .groups = "drop"
  )



# Association between ICD and dyskinesia (binary outcomes)
table_assoc <- table(df_patient$ever_icd, df_patient$ever_dysk)
print("Contingency Table:")
print(table_assoc)

# 0   1
# 0 142  34
# 1  29   1

# Fisher's exact test (appropriate for small cell counts)
fisher_result <- fisher.test(table_assoc)
print(fisher_result)

# Fisher's Exact Test for Count Data
# 
# data:  table_assoc
# p-value = 0.03356
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.003430868 0.940064899
# sample estimates:
# odds ratio 
#  0.1448237 
#  
#  
# 2. Treatment effects on each outcome separately
# Effect on dyskinesia
dysk_model <- glm(ever_dysk ~ treatment, 
                  data = df_patient, 
                  family = binomial())
summary(dysk_model)

# Call:
#   glm(formula = ever_dysk ~ treatment, family = binomial(), data = df_patient)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.9694     0.3082  -6.391 1.65e-10 ***
#   treatmentPlacebo   0.6623     0.3876   1.709   0.0875 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 187.76  on 205  degrees of freedom
# Residual deviance: 184.73  on 204  degrees of freedom
# AIC: 188.73
# 
# Number of Fisher Scoring iterations: 4
# 

# Effect on ICD
icd_model <- glm(ever_icd ~ treatment, 
                 data = df_patient, 
                 family = binomial())
summary(icd_model)


# Call:
#   glm(formula = ever_icd ~ treatment, family = binomial(), data = df_patient)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.9694     0.3082  -6.391 1.65e-10 ***
#   treatmentPlacebo   0.3600     0.4020   0.895    0.371    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 171.00  on 205  degrees of freedom
# Residual deviance: 170.19  on 204  degrees of freedom
# AIC: 174.19
# 
# Number of Fisher Scoring iterations: 4



#  summary statistics by treatment
summary_stats <- df_patient %>%
  group_by(treatment) %>%
  summarize(
    n = n(),
    n_icd = sum(ever_icd),
    pct_icd = 100 * n_icd / n,
    n_dysk = sum(ever_dysk),
    pct_dysk = 100 * n_dysk / n,
    n_both = sum(ever_icd == 1 & ever_dysk == 1),
    pct_both = 100 * n_both / n
  )



print("Summary Statistics by Treatment:")
print(summary_stats)

# treatment      n n_icd pct_icd n_dysk pct_dysk n_both pct_both
# Amantadine    98    12    12.2     12     12.2      0    0    
# Placebo      108    18    16.7     23     21.3      1    0.926


# Step 2: Create contingency table and test association
cat("\n=== Association between ICD and Dyskinesia ===\n")
table_assoc <- table(df_patient$ever_icd, df_patient$ever_dysk)
print(table_assoc)

# 0   1
# 0 142  34
# 1  29   1


# Fisher's exact test
fisher_result <- fisher.test(table_assoc)
print(fisher_result)

#     0   1
# 0 142  34
# 1  29   1

#  Logistic regression
logit_model <- glm(ever_dysk ~ ever_icd + treatment, 
                   data = df_patient, 
                   family = binomial())
cat("\n=== Logistic Regression Results ===\n")
summary(logit_model)


# Call:
#   glm(formula = ever_dysk ~ ever_icd + treatment, family = binomial(), 
#       data = df_patient)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.8433     0.3112  -5.923 3.16e-09 ***
#   ever_icd          -2.0181     1.0381  -1.944   0.0519 .  
# treatmentPlacebo   0.7297     0.3922   1.860   0.0628 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 187.76  on 205  degrees of freedom
# Residual deviance: 177.93  on 203  degrees of freedom
# AIC: 183.93
# 
# Number of Fisher Scoring iterations: 6


# Calculate odds ratios with confidence intervals
exp_coef <- exp(coef(logit_model))
exp_ci <- exp(confint(logit_model))
cat("\nOdds Ratios:\n")
print(cbind(OR = exp_coef, CI_lower = exp_ci[,1], CI_upper = exp_ci[,2]))

# OR    CI_lower  CI_upper
# (Intercept)      0.1582918 0.081754654 0.2799747
# ever_icd         0.1329139 0.007309147 0.6628371
# treatmentPlacebo 2.0743952 0.977157873 4.5998972


#  Time-to-event analysis
cat("\n=== Time-to-Event Analysis ===\n")

# Prepare data for competing risks
df_compete <- df_patient %>%
  mutate(
    # Time to first event (minimum of onset times)
    time_to_event = pmin(first_icd_time, first_dysk_time, na.rm = TRUE),
    # Event type: 1=ICD, 2=Dysk, 0=censored
    event_type = case_when(
      ever_icd == 1 & ever_dysk == 0 ~ 1,
      ever_icd == 0 & ever_dysk == 1 ~ 2,
      ever_icd == 1 & ever_dysk == 1 & first_icd_time < first_dysk_time ~ 1,
      ever_icd == 1 & ever_dysk == 1 & first_dysk_time < first_icd_time ~ 2,
      ever_icd == 1 & ever_dysk == 1 & first_icd_time == first_dysk_time ~ 3,
      TRUE ~ 0
    ),
    # For censored patients, use last visit
    time_to_event = ifelse(event_type == 0, last_visit, time_to_event),
    # Treatment indicator
    treatment_amantadine = ifelse(treatment == "Amantadine", 1, 0)
  )

# Remove rows with NA times
df_compete <- df_compete %>%
  filter(!is.na(time_to_event))

# Check event distribution
cat("\nEvent type distribution:\n")
print(table(df_compete$event_type))


# 0   1   2 
# 142  30  34 


# Time-dependent Cox model (using all visits)
cat("\n=== Time-Dependent Cox Model ===\n")

# Make sure TREATMENT column exists (check case sensitivity)
if("TREATMENT" %in% names(df_ms)) {
  df_time_dep <- df_ms %>%
    arrange(SUBJID, VISIT) %>%
    group_by(SUBJID) %>%
    mutate(
      # Start and stop times
      start_time = lag(VISIT, default = 0),
      stop_time = VISIT,
      # Event indicator (dyskinesia onset at this visit)
      event = ifelse(MDS68 == 1 & lag(MDS68, default = 0) == 0, 1, 0),
      # Time-varying covariate: current ICD status
      current_icd = ecmp_icd_binary
    ) %>%
    ungroup() %>%
    filter(stop_time > 0)  # Remove time zero
  
  # Cox model with time-varying covariate
  cox_td <- coxph(Surv(start_time, stop_time, event) ~ current_icd + TREATMENT + 
                    cluster(SUBJID), 
                  data = df_time_dep)
  
  print(summary(cox_td))
  
  # Calculate hazard ratio for ICD
  hr_icd <- exp(coef(cox_td)["current_icd"])
  hr_ci <- exp(confint(cox_td)["current_icd",])
  cat("\nHazard Ratio for Dyskinesia given ICD:\n")
  cat(sprintf("HR = %.2f (95%% CI: %.2f-%.2f), p = %.4f\n", 
              hr_icd, hr_ci[1], hr_ci[2], 
              summary(cox_td)$coefficients["current_icd", "Pr(>|z|)"]))
}


# Call:
#   coxph(formula = Surv(start_time, stop_time, event) ~ current_icd + 
#           TREATMENT, data = df_time_dep, cluster = SUBJID)
# 
# n= 578, number of events= 36 
# 
# coef  exp(coef)   se(coef)  robust se       z Pr(>|z|)    
# current_icd      -1.720e+01  3.389e-08  3.071e+03  2.753e-01 -62.482   <2e-16 ***
#   TREATMENTPlacebo  6.349e-01  1.887e+00  3.536e-01  3.379e-01   1.879   0.0603 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# current_icd      3.389e-08  2.951e+07 1.976e-08 5.812e-08
# TREATMENTPlacebo 1.887e+00  5.300e-01 9.730e-01 3.659e+00
# 
# Concordance= 0.612  (se = 0.037 )
# Likelihood ratio test= 9.01  on 2 df,   p=0.01
# Wald test            = 3933  on 2 df,   p=<2e-16
# Score (logrank) test = 6.27  on 2 df,   p=0.04,   Robust = 17.04  p=2e-04
# 
# (Note: the likelihood ratio and score tests assume independence of
#   observations within a cluster, the Wald and robust score tests do not).
# 
# Hazard Ratio for Dyskinesia given ICD:
#   HR = 0.00 (95% CI: 0.00-0.00), p = 0.0000
# Message d'avis :
# Dans agreg.fit(X, Y, istrat, offset, init, control, weights = weights,  :
#   Loglik converged before variable  1 ; beta may be infinite. 


cat("\n=== Visualization ===\n")

# Bar plot of proportions
# Version with both counts and percentages
bar_data_complete <- df_patient %>%
  group_by(treatment, ever_icd, ever_dysk) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment, ever_icd) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100,
    icd_label = ifelse(ever_icd == 0, "No ICD", "ICD"),
    dysk_label = ifelse(ever_dysk == 0, "No Dysk", "Dysk"),
    label_text = sprintf("n=%d (%.1f%%)", count, percentage)
  )

plot <- ggplot(bar_data_complete, 
               aes(x = icd_label, 
                   y = percentage/100, 
                   fill = dysk_label)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  facet_wrap(~treatment) +
  geom_text(aes(label = label_text),
            position = position_fill(vjust = 0.5),
            size = 3.5,
            fontface = "bold",
            color = "white") +
  labs(title = "% Dyskinesia by ICD Status",
       x = "\n ICD Status", 
       y = "Proportion \n",
       fill = "Dyskinesia") +
  theme_minimal() +
  scale_fill_manual(values = c("No Dysk" = "#2f3941", "Dysk" = "#aa3951")) +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")


plot


ggsave(file = "../out/dysk_icd.svg", plot = plot, width = 5, height = 5)


# Box plot of onset times
df_onset <- df_patient %>%
  filter(ever_icd == 1 | ever_dysk == 1) %>%
  pivot_longer(cols = c(first_icd_time, first_dysk_time),
               names_to = "event_type",
               values_to = "onset_time") %>%
  filter(!is.na(onset_time)) %>%
  mutate(event_type = ifelse(event_type == "first_icd_time", "ICD", "Dyskinesia"))


if(nrow(df_onset) > 0) {
  plot <- ggplot(df_onset, aes(x = event_type, y = onset_time, fill = event_type, color=event_type)) +
    geom_boxplot(outliers = F, notch=T, alpha=0.5, width=0.5) +
    geom_jitter(height=0.1, width=0.4, shape=1, size=3, stroke=2) +
    facet_wrap(~treatment) +
    labs(title = "ICD|Dyskinesia Onset Times",
         x = "\n Event Type",
         y = "Time to Onset (months)\n") +
    theme_minimal() +
    scale_fill_manual(values = c("ICD" = "#2f3941", "Dyskinesia" = "#aa3951")) +
    scale_colour_manual(values = c("ICD" = "#2f3941", "Dyskinesia" = "#aa3951")) +
    theme(text = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top")
  
  plot
  
}


ggsave(file = "../out/dysk_icd.svg", plot = plot, width = 4, height = 5)




library(dplyr)
library(tidyr)
library(ggplot2)

# Prepare onset data
df_onset <- df_patient %>%
  filter(ever_icd == 1 | ever_dysk == 1) %>%
  pivot_longer(cols = c(first_icd_time, first_dysk_time),
               names_to = "event_type",
               values_to = "onset_time") %>%
  filter(!is.na(onset_time)) %>%
  mutate(
    event_type = ifelse(event_type == "first_icd_time", "ICD", "Dyskinesia"),
    
    # Force into the 4 categories
    onset_cat = factor(onset_time, levels = c(0, 3, 9, 18))
  )

# Aggregate for stacked bars
bar_data <- df_onset %>%
  mutate(onset_time=as.factor(onset_time)) %>%
  group_by(treatment, event_type, onset_time ) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment, event_type) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    label = sprintf("n=%d (%.0f%%)", count, proportion * 100)
  )

# Plot
plot <- ggplot(bar_data,
               aes(x = event_type,
                   y = proportion,
                   fill = onset_time)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  facet_wrap(~treatment) +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white",
            fontface = "bold") +
  labs(title = "Onset Time Distribution (ICD vs Dyskinesia)",
       x = "\nEvent Type",
       y = "Proportion\n",
       fill = "Onset Time (months)") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "0"  = "#BFBABA",
    "3"  = "#EDCED5",
    "9"  = "#C75B72",
    "18" = "#aa3951"
  )) +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

plot


ggsave(file = "../out/dysk_icd_onset_stacked.svg",
       plot = plot, width = 5, height = 5)



df_patient %>% group_by(treatment) %>%
  summarise(ever_icd =sum(ever_icd ), ever_dysk =sum(ever_dysk ))


# Prepare long data for each event type
df_events <- df_patient %>%
  pivot_longer(
    cols = c(first_icd_time, first_dysk_time),
    names_to = "event_type",
    values_to = "time"
  ) %>%
  mutate(
    event_type = case_when(
      event_type == "first_icd_time" & ever_icd == 1 & ever_dysk == 0 ~ "ICD",
      event_type == "first_dysk_time" & ever_icd == 0 & ever_dysk == 1 ~ "Dyskinesia",
      event_type == "first_icd_time" & ever_icd == 1 & ever_dysk == 1 ~ "Both",
      event_type == "first_dysk_time" & ever_icd == 1 & ever_dysk == 1 ~ "Both",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(event_type)) %>%
  group_by(treatment, event_type) %>%
  arrange(time) %>%
  mutate(cum_events = row_number())

# Ensure zero time point for all treatment-event_type combos
df_zero <- expand.grid(
  treatment = unique(df_patient$treatment),
  event_type = c("ICD", "Dyskinesia", "Both"),
  time = 0
) %>%
  mutate(cum_events = 0)

df_plot <- bind_rows(df_zero, df_events)

# Plot
plot <- ggplot(df_plot %>% filter(event_type!="Both"), 
               aes(x = time, y = cum_events, color = event_type, linetype = treatment)) +
  geom_step(size = 1) +
  #geom_point(size = 2, shape=1, size=2, stroke=2) +
  scale_color_manual(values = c("ICD"="#aa3951", "Dyskinesia"="#2f3941", "Both"="#C45E74")) +
  labs(
    title = "Cumulative Number of Events",
    x = "\nTime (months)",
    y = "Cumulative Events\n",
    color = "Event Type",
    linetype = "Treatment"
  ) +
  facet_wrap(~event_type, ncol=1) +
  theme_minimal() +
  scale_fill_manual(values = c("ICD" = "#2f3941", "Dyskinesia" = "#aa3951")) +
  scale_colour_manual(values = c("ICD" = "#2f3941", "Dyskinesia" = "#aa3951")) +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")


plot
ggsave(file = "../out/dysk_icd.svg", plot = plot, width = 4, height = 5)






# Create a summary table
summary_table <- data.frame(
  Analysis = c("Association Test", "Logistic Regression", "Logistic Regression", 
               "Fine-Gray Model (ICD)", "Fine-Gray Model (Dyskinesia)", 
               "Time-Dependent Cox"),
  Variable = c("ICD vs Dyskinesia", "ICD", "Treatment (Placebo)", 
               "Treatment (Amantadine)", "Treatment (Amantadine)", 
               "Current ICD Status"),
  Effect = c("OR = 0.14", "OR = 0.13", "OR = 2.07", 
             "HR = 0.74", "HR = 0.60", "HR = 0.00"),
  CI = c("(0.003-0.94)", "(0.007-0.66)", "(0.98-4.60)", 
         "(0.36-1.52)", "(0.31-1.18)", "(0.00-0.00)"),
  P_value = c("0.034", "0.052", "0.063", 
              "0.41", "0.14", "< 0.0001")
)

print(summary_table)




