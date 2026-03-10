

library(haven)
library(tidyverse)
library(data.table)
library(lubridate)


# ICD Binary and Continuous -------------

admin <- read_sas("../data/admin.sas7bdat")

admin %>% group_by(VISIT) %>% count()

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

fwrite(treat_pats_groups, "../out/treat_pats_groups.txt")

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

plot <- ecmp %>% group_by(VISIT) %>% count()  %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  ggplot(aes(VISIT, n)) +
  geom_col(fill = "black", alpha=0.8, width=0.8) +
  labs(
    title = "Number of Patients With Known ICB Status",
    subtitle = "ECMP-based ICB [12-item]\n",
    x = "\n Number of Months From Baseline",
    y = "Number of Patients\n") +
  scale_x_continuous(
    breaks = seq(-1, 20, by = 1),          # ← this is the key line
    limits = c(-1.5, 20.5),               # optional: prevents bars touching edges
    expand = expansion(mult = c(0.02, 0.1))  # optional: tiny left + more right padding
  ) +
  theme_minimal() +
  ylim(0,210) +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right")+ 
  geom_text(aes(label = n),
    vjust = -0.4,                        # put text slightly above the bar
    size  = 4.2,                         # adjust text size as needed
    fontface = "bold",
    color = "black")

plot 

ggsave(file = "../out/number_with_ecmp_icb.svg", plot = plot, width = 5, height = 3)



baseline_icb <- ecmp %>%
  filter(VISIT == 2) %>%
  group_by(TREATMENT) %>%
  summarise(
    N = n(),
    ICB_n = sum(ecmp_icb_binary == 1, na.rm = TRUE),
    ICB_pct = mean(ecmp_icb_binary == 1, na.rm = TRUE) * 100
  )

baseline_icb

#   TREATMENT      N ICB_n ICB_pct
# 1 Amantadine    98    71    72.4
# 2 Placebo      106    74    69.8

data_long <- baseline_icb %>%
  mutate(No_ICB_perc = 100 - ICB_pct) %>%
  select(TREATMENT, ICB_pct, No_ICB_perc) %>%
  pivot_longer(cols = c(ICB_pct, No_ICB_perc), 
               names_to = "ICB_Status", 
               values_to = "Count") %>%
  mutate(ICB_Status = recode(ICB_Status, 
                             "ICB_pct" = "Yes ICB", 
                             "No_ICB_perc" = "No ICB"))

# Create stacked bar chart
plot <- ggplot(data_long, aes(x = TREATMENT, y = Count, fill = ICB_Status)) +
  geom_bar(stat = "identity", position = "stack", width=0.5) +
  geom_text(aes(label = paste0(round(Count,1),"%") ), 
            position = position_stack(vjust = 0.6),
            size = 2.5, fontface = "bold", color = "white") +
  labs(title = "% ICB Status by Treatment",
           subtitle = "ECMP-based ICB [12-item]\n",
       x = "",
       y = "\n Patient %",
       fill = "ICB Status") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom") +
  coord_flip() +
  scale_fill_manual(values = c("Yes ICB" = "#aa3951", "No ICB" = "#2f3941")) 

plot

ggsave(file = "../out/percent_with_pos_ecmp_icb.svg", plot = plot, width = 4, height = 3)


baseline_status <- ecmp %>%
  filter(VISIT == 2) %>%
  select(SUBJID, ecmp_icb_binary) %>%
  rename(ICB_baseline = ecmp_icb_binary)

ecmp_long <- ecmp %>%
  left_join(baseline_status, by = "SUBJID")

# 0 at baseline

incident_icb_data <- ecmp_long %>%
  filter(ICB_baseline == 0)

length(unique(incident_icb_data$SUBJID)) # 59

first_icb_per_patient <- incident_icb_data %>% filter(ecmp_icb_binary==1) %>% 
  select(SUBJID, VISIT) %>% group_by(SUBJID) %>%
  filter(VISIT==min(VISIT)) %>% distinct()

length(unique(first_icb_per_patient$SUBJID)) # 39

last_cencor_per_patient <- incident_icb_data %>% anti_join(first_icb_per_patient %>% select(SUBJID)) %>%
  select(SUBJID, VISIT) %>% group_by(SUBJID) %>%
  filter(VISIT==max(VISIT)) %>% distinct()

length(unique(last_cencor_per_patient$SUBJID)) # 20

first_icb_per_patient %>% bind_rows(last_cencor_per_patient) # 175

baseline_date <- ecmp %>%
  filter(VISIT == 2) %>%
  select(SUBJID, VISIT_baseline = VISIT)

surv_data <- bind_rows(
  first_icb_per_patient %>% mutate(event = 1),
  last_cencor_per_patient %>% mutate(event = 0)
)


surv_data <- surv_data %>%
  left_join(baseline_date, by = "SUBJID") %>%
  left_join(treat_pats_groups, by = "SUBJID")

surv_data <- surv_data %>%
  mutate(VISIT_baseline=0) %>%
  mutate(VISIT=ifelse(VISIT==3, 3, ifelse(VISIT==5,9,18))) %>%
  mutate(
    time_months = as.numeric(VISIT - VISIT_baseline )
  )


range(surv_data$time_months)

summary(surv_data$time_months)

library(survival)
library(survminer)

km_fit <- survfit(Surv(time_months, event) ~ TREATMENT, data = surv_data)

plot <- ggsurvplot(km_fit,
           data = surv_data,
           # Title and labels
           title = "ICB Incidence ECMP-based ICB [12-item] \nKaplan-Meier Curve",
           xlab = "Time (months)",
           ylab = ' "ICB-free"  Probability\n', 
           legend.title = "Treatment Group",
           legend.labs = c("Amantadine", "Placebo"),
           # Confidence intervals
           conf.int = TRUE,
           conf.int.style = "ribbon",
           conf.int.alpha = 0.2,
           # Risk table
           risk.table = TRUE,
           risk.table.col = "strata",
           risk.table.height = 0.25,
           risk.table.y.text = TRUE,
           # P-value
           pval = TRUE,
           pval.method = TRUE,
           # Axis limits
           xlim = c(0, max(surv_data$time_months)),
           break.time.by = 3,  # breaks every 3 months 
           # Styling
           palette = c("#aa3951", "#2f3941"),  # colorblind-friendly
           ggtheme = theme_minimal() +
                     theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                           legend.position = "right",
                           axis.title = element_text(size = 12),
                           axis.text = element_text(size = 10),
                            panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()),
           surv.median.line = "hv",  # show median survival lines
           censor.shape = "|",
           censor.size = 4
)

plot_export <- plot$plot
plot_export

ggsave(file = "../out/km_icb_free_prob.svg", plot = plot_export, width = 4, height = 4)

plot_export <- plot$table

ggsave(file = "../out/km_icb_free_prob_table.svg", plot = plot_export, width = 5, height = 1.5)



survdiff(Surv(time_months, event) ~ TREATMENT, data = surv_data)

# Call:
# survdiff(formula = Surv(time_months, event) ~ TREATMENT, data = surv_data)
# 
#                       N Observed Expected (O-E)^2/E (O-E)^2/V
# TREATMENT=Amantadine 27       20       16     0.979      2.48
# TREATMENT=Placebo    32       19       23     0.684      2.48
# 
#  Chisq= 2.5  on 1 degrees of freedom, p= 0.1






cox1 <- coxph(Surv(time_months, event) ~ TREATMENT, data = surv_data)

summary(cox1)

# Call:
# coxph(formula = Surv(time_months, event) ~ TREATMENT, data = surv_data)
# 
#   n= 59, number of events= 39 
# 
#                     coef exp(coef) se(coef)      z Pr(>|z|)
# TREATMENTPlacebo -0.4919    0.6115   0.3211 -1.532    0.126
# 
#                  exp(coef) exp(-coef) lower .95 upper .95
# TREATMENTPlacebo    0.6115      1.635    0.3259     1.147
# 
# Concordance= 0.592  (se = 0.05 )
# Likelihood ratio test= 2.33  on 1 df,   p=0.1
# Wald test            = 2.35  on 1 df,   p=0.1
# Score (logrank) test = 2.39  on 1 df,   p=0.1

cox.zph(cox1)

#          chisq df    p
# TREATMENT  1.28  1 0.26
# GLOBAL     1.28  1 0.26


ecmp_longitudinal <- ecmp %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

length(unique(ecmp_longitudinal$SUBJID)) # 206


library(lme4)

model1 <- lmer(
  ecmp_icb_severity ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_longitudinal
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: ecmp_icb_severity ~ TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_longitudinal
# 
# REML criterion at convergence: 3048.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.1819 -0.5019 -0.2251  0.3503  6.1484 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 1.580    1.257   
#  Residual             1.919    1.385   
# Number of obs: 786, groups:  SUBJID, 206
# 
# Fixed effects:
#                         Estimate Std. Error t value
# (Intercept)             1.644422   0.164470   9.998
# TREATMENTPlacebo        0.145037   0.227386   0.638
# VISIT                  -0.005814   0.010619  -0.547
# TREATMENTPlacebo:VISIT  0.004862   0.014605   0.333
# 
# Correlation of Fixed Effects:
#             (Intr) TREATMENTPl VISIT 
# TREATMENTPl -0.723                   
# VISIT       -0.456  0.330            
# TREATMENTP:  0.331 -0.459      -0.727


model2 <- lmer(
  ecmp_icb_severity ~ TREATMENT * VISIT + (VISIT | SUBJID),
  data = ecmp_longitudinal
)

anova(model1, model2)

# Data: ecmp_longitudinal
# Models:
# model1: ecmp_icb_severity ~ TREATMENT * VISIT + (1 | SUBJID)
# model2: ecmp_icb_severity ~ TREATMENT * VISIT + (VISIT | SUBJID)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    6 3042.1 3070.1 -1515.0   3030.1                     
# model2    8 3045.1 3082.5 -1514.6   3029.1 0.9384  2     0.6255



library(emmeans)

emm <- emmeans(model1, ~ TREATMENT | VISIT)

emm

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(0))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   -0.145 0.227 320  -0.638  0.5240
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(3))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    -0.16 0.211 242  -0.757  0.4499
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(9))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   -0.189 0.204 210  -0.926  0.3554
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(18))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   -0.233 0.257 461  -0.905  0.3657
# 
# Degrees-of-freedom method: kenward-roger 

plot_data <- data.frame(ecmp %>% inner_join(treat_pats_groups) %>%
  mutate(ecmp_icb_severity=ifelse(ecmp_icb_severity>=5, "5+", ecmp_icb_severity)) %>%
  group_by(TREATMENT , VISIT, ecmp_icb_severity) %>% 
  count() %>% rename("num"="n") %>%
  left_join(ecmp %>% inner_join(treat_pats_groups) %>%
  group_by(TREATMENT , VISIT) %>% 
  count() %>% rename("den"="n")) %>%
  mutate(perc=num/den) %>% select(-num, -den)) %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

# plot_data$ecmp_icb_severity <- factor(plot_data$ecmp_icb_severity, 
#                                        levels = sort(unique(plot_data$ecmp_icb_severity)))



# Create the stacked bar chart
plot <- plot_data %>%
  #mutate(ecmp_icb_severity=ifelse(ecmp_icb_severity>=5, "5+", ecmp_icb_severity)) %>%
  ggplot( aes(x = factor(VISIT), y = perc, fill = ecmp_icb_severity)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~TREATMENT, ncol = 2) +
    labs(title = "ECMP-based ICB [12-item] Severity Distribution",
       x = "\n Number of Months From Baseline",
       y = "Cohort Proportion \n",
       fill = "ICB Total Severity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values=c(  "#c1cfd1", "#89949C", "#4E5A63", "#2F3941",  "#C45E74", "#aa3951" )) +
  theme_minimal() +
   theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right")  +
    geom_text(aes(label = ifelse(perc > 0.02, scales::percent(perc, accuracy = 1), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white", fontface = "bold")


plot

ggsave(file = "../out/stacked_icb_sev.svg", plot = plot, width = 8, height = 5)


plot <- ecmp %>% inner_join(treat_pats_groups) %>%
   mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  rename("Treatment"="TREATMENT") %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  ggplot(aes(VISIT , ecmp_icb_severity, colour=Treatment, fill=Treatment)) +
  geom_boxplot(alpha=0.5, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.6, size=0.5, stroke=2, shape=1, width = 0.3, height=0.1) +
  theme_minimal() +
  ylim(0,14) +
  scale_colour_manual(values=c("#aa3951", "#2f3941")) +
  scale_fill_manual(values=c("#aa3951", "#2f3941")) +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top") +
  labs(title = "ECMP-based ICB [12-item] Distribution",
       x = "\n Number of Months From Baseline",
       y = "ICB Total Score \n",
       fill = "Treatment") 


plot

ggsave(file = "../out/box_icb_sev.svg", plot = plot, width = 4, height = 5)
