

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

#fwrite(treat_pats_groups, "../out/treat_pats_groups.txt")

ecmp <- read_sas("../data/ecmp.sas7bdat")

ecmp %>% filter(!is.na(ECMPDT)) %>%
  select(SUBJID, VISIT) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=VISIT, value=exp)  %>%
  group_by(`8`, `16`) %>% count()
 


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


ecmp %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  group_by(TREATMENT, VISIT) %>% summarise(mean=mean(ecmp_icb_binary))


# dysk_pats <- fread("../out/dysk_pats.txt")
# 
# dysk_pats %>% filter(VISIT==18) %>% group_by(TREATMENT, MDS68) %>% count()
# 
# ecmp %>%
#   mutate(VISIT=ifelse(VISIT==2, 0,
#                       ifelse(VISIT==3, 3, 
#                              ifelse(VISIT==5,9,18)))) %>%
#   mutate(SUBJID=as.numeric(SUBJID)) %>%
#   inner_join(dysk_pats %>% filter(VISIT==18) %>% select(SUBJID , MDS68)) %>%
#   group_by(TREATMENT, VISIT, MDS68) %>% filter(VISIT==18) %>%
#   summarise(mean=mean(ecmp_icd_binary))
 

# ecmp %>%
#   mutate(VISIT=ifelse(VISIT==2, 0,
#                       ifelse(VISIT==3, 3, 
#                              ifelse(VISIT==5,9,18)))) %>%
#   mutate(SUBJID=as.numeric(SUBJID)) %>%
#   inner_join(dysk_pats %>% filter(VISIT==18) %>% select(SUBJID , MDS68)) %>%
#   group_by(TREATMENT, VISIT, MDS68) %>% filter(VISIT==18) %>%
#   summarise(mean=mean(ecmp_icb_binary))


# ecmp %>%
#   mutate(VISIT=ifelse(VISIT==2, 0,
#                       ifelse(VISIT==3, 3, 
#                              ifelse(VISIT==5,9,18)))) %>%
#   mutate(SUBJID=as.numeric(SUBJID)) %>%
#   inner_join(dysk_pats %>% filter(VISIT==18) %>% select(SUBJID , MDS68)) %>%
#   group_by(TREATMENT, VISIT, MDS68) %>% filter(VISIT==18) %>%
#   mutate(ecmp_hypodopa=ifelse(ecmp_hypodopa>0,1,0)) %>%
#   summarise(mean=mean(ecmp_hypodopa))






plot <- ecmp %>% group_by(VISIT) %>% count()  %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  ggplot(aes(VISIT, n)) +
  geom_col(fill = "black", alpha=0.8, width=0.8) +
  labs(
    title = "Number of Patients With Known ICD Status",
    subtitle = "ECMP-based ICD [4-item]\n",
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

ggsave(file = "../out/number_with_ecmp_icd.svg", plot = plot, width = 5, height = 3)



baseline_icd <- ecmp %>%
  filter(VISIT == 2) %>%
  group_by(TREATMENT) %>%
  summarise(
    N = n(),
    ICD_n = sum(ecmp_icd_binary == 1, na.rm = TRUE),
    ICD_pct = mean(ecmp_icd_binary == 1, na.rm = TRUE) * 100
  )

baseline_icd

#   TREATMENT      N ICD_n ICD_pct
#   <chr>      <int> <int>   <dbl>
# 1 Amantadine    98     5    5.10
# 2 Placebo      106     5    4.67

data_long <- baseline_icd %>%
  mutate(No_ICD_perc = 100 - ICD_pct) %>%
  select(TREATMENT, ICD_pct, No_ICD_perc) %>%
  pivot_longer(cols = c(ICD_pct, No_ICD_perc), 
               names_to = "ICD_Status", 
               values_to = "Count") %>%
  mutate(ICD_Status = recode(ICD_Status, 
                             "ICD_pct" = "Yes ICD", 
                             "No_ICD_perc" = "No ICD"))

# Create stacked bar chart
plot <- ggplot(data_long, aes(x = TREATMENT, y = Count, fill = ICD_Status)) +
  geom_bar(stat = "identity", position = "stack", width=0.5) +
  geom_text(aes(label = paste0(round(Count,1),"%") ), 
            position = position_stack(vjust = 0.6),
            size = 2.5, fontface = "bold", color = "white") +
  labs(title = "% ICD Status by Treatment",
           subtitle = "ECMP-based ICD [4-item]\n",
       x = "",
       y = "\n Patient %",
       fill = "ICD Status") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom") +
  coord_flip() +
  scale_fill_manual(values = c("Yes ICD" = "#aa3951", "No ICD" = "#2f3941")) 

plot

ggsave(file = "../out/percent_with_pos_ecmp_icd.svg", plot = plot, width = 4, height = 3)


baseline_status <- ecmp %>%
  filter(VISIT == 2) %>%
  select(SUBJID, ecmp_icd_binary) %>%
  rename(ICD_baseline = ecmp_icd_binary)

ecmp_long <- ecmp %>%
  left_join(baseline_status, by = "SUBJID")

# 0 at baseline

incident_icd_data <- ecmp_long %>%
  filter(ICD_baseline == 0)

length(unique(incident_icd_data$SUBJID)) # 194

first_icd_per_patient <- incident_icd_data %>% filter(ecmp_icd_binary==1) %>% 
  select(SUBJID, VISIT) %>% group_by(SUBJID) %>%
  filter(VISIT==min(VISIT)) %>% distinct()

length(unique(first_icd_per_patient$SUBJID)) # 20

last_cencor_per_patient <- incident_icd_data %>% anti_join(first_icd_per_patient %>% select(SUBJID)) %>%
  select(SUBJID, VISIT) %>% group_by(SUBJID) %>%
  filter(VISIT==max(VISIT)) %>% distinct()

length(unique(last_cencor_per_patient$SUBJID)) # 174

first_icd_per_patient %>% bind_rows(last_cencor_per_patient) # 194

baseline_date <- ecmp %>%
  filter(VISIT == 2) %>%
  select(SUBJID, VISIT_baseline = VISIT)

surv_data <- bind_rows(
  first_icd_per_patient %>% mutate(event = 1),
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


fwrite(surv_data, "surv_data.csv")

range(surv_data$time_months)

summary(surv_data$time_months)

library(survival)
library(survminer)

km_fit <- survfit(Surv(time_months, event) ~ TREATMENT, data = surv_data)

plot <- ggsurvplot(km_fit,
           data = surv_data,
           # Title and labels
           title = "ICD Incidence ECMP-based ICD [4-item] \nKaplan-Meier Curve",
           xlab = "Time (months)",
           ylab = ' "ICD-free"  Probability\n', 
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
                           axis.title = element_text(size = 10),
                           axis.text = element_text(size = 10),
                            panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()),
           surv.median.line = "hv",  # show median survival lines
           censor.shape = "|",
           censor.size = 4
)

plot_export <- plot$plot
plot_export

ggsave(file = "../out/km_icd_free_prob.svg", plot = plot_export, width = 4, height = 4)

plot_export <- plot$table

ggsave(file = "../out/km_icd_free_prob_table.svg", plot = plot_export, width = 5, height = 1.5)



survdiff(Surv(time_months, event) ~ TREATMENT, data = surv_data)

# Call:
# survdiff(formula = Surv(time_months, event) ~ TREATMENT, data = surv_data)

#                        N Observed Expected (O-E)^2/E (O-E)^2/V
# TREATMENT=Amantadine  93        8     9.48     0.231     0.441
# TREATMENT=Placebo    101       12    10.52     0.208     0.441
# 
#  Chisq= 0.4  on 1 degrees of freedom, p= 0.5 







cox1 <- coxph(Surv(time_months, event) ~ TREATMENT, data = surv_data)

summary(cox1)

# Call:
# coxph(formula = Surv(time_months, event) ~ TREATMENT, data = surv_data)
# 
#   n= 194, number of events= 20 
# 
#                    coef exp(coef) se(coef)     z Pr(>|z|)
# TREATMENTPlacebo 0.3010    1.3512   0.4564 0.659     0.51
# 
#                  exp(coef) exp(-coef) lower .95 upper .95
# TREATMENTPlacebo     1.351     0.7401    0.5523     3.306
# 
# Concordance= 0.532  (se = 0.057 )
# Likelihood ratio test= 0.44  on 1 df,   p=0.5
# Wald test            = 0.43  on 1 df,   p=0.5
# Score (logrank) test = 0.44  on 1 df,   p=0.5

cox.zph(cox1)

#     chisq df    p
# TREATMENT  2.06  1 0.15
# GLOBAL     2.06  1 0.15


length(unique(ecmp_longitudinal$SUBJID)) # 206

ecmp_longitudinal <- ecmp %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))


library(lme4)

model1 <- lmer(
  ecmp_icd_severity ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_longitudinal
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: ecmp_icd_severity ~ TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_longitudinal
# 
# REML criterion at convergence: 1724.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0294 -0.4720 -0.2049  0.2402  5.3214 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.2459   0.4959  
#  Residual             0.3669   0.6057  
# Number of obs: 786, groups:  SUBJID, 206
# 
# Fixed effects:
#                         Estimate Std. Error t value
# (Intercept)             0.466470   0.067802   6.880
# TREATMENTPlacebo       -0.016081   0.093749  -0.172
# VISIT                  -0.006471   0.004640  -1.395
# TREATMENTPlacebo:VISIT  0.005765   0.006382   0.903
# 
# Correlation of Fixed Effects:
#             (Intr) TREATMENTPl VISIT 
# TREATMENTPl -0.723                   
# VISIT       -0.484  0.350            
# TREATMENTP:  0.352 -0.487      -0.727


model2 <- lmer(
  ecmp_icd_severity ~ TREATMENT * VISIT + (VISIT | SUBJID),
  data = ecmp_longitudinal
)

anova(model1, model2)

# Data: ecmp_longitudinal
# Models:
# model1: ecmp_icd_severity ~ TREATMENT * VISIT + (1 | SUBJID)
# model2: ecmp_icd_severity ~ TREATMENT * VISIT + (VISIT | SUBJID)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    6 1710.4 1738.4 -849.19   1698.4                     
# model2    8 1711.6 1749.0 -847.81   1695.6 2.7544  2     0.2523



library(emmeans)

emm <- emmeans(model1, ~ TREATMENT | VISIT)

emm

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(0))))

#  contrast             estimate     SE  df t.ratio p.value
#  Amantadine - Placebo   0.0161 0.0937 339   0.172  0.8639
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(3))))

#  contrast             estimate     SE  df t.ratio p.value
#  Amantadine - Placebo -0.00121 0.0861 248  -0.014  0.9887
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(9))))

#  contrast             estimate     SE  df t.ratio p.value
#  Amantadine - Placebo  -0.0358 0.0827 211  -0.433  0.6656
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(18))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo  -0.0877 0.107 497  -0.818  0.4138
# 
# Degrees-of-freedom method: kenward-roger

plot_data <- data.frame(ecmp %>% inner_join(treat_pats_groups) %>%
  group_by(TREATMENT , VISIT, ecmp_icd_severity) %>% 
  count() %>% rename("num"="n") %>%
  left_join(ecmp %>% inner_join(treat_pats_groups) %>%
  group_by(TREATMENT , VISIT) %>% 
  count() %>% rename("den"="n")) %>%
  mutate(perc=num/den) %>% select(-num, -den)) %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

plot_data$ecmp_icd_severity <- factor(plot_data$ecmp_icd_severity, 
                                       levels = sort(unique(plot_data$ecmp_icd_severity)))



# Create the stacked bar chart
plot <- ggplot(plot_data, aes(x = factor(VISIT), y = perc, fill = ecmp_icd_severity)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~TREATMENT, ncol = 2) +
    labs(title = "ECMP-based ICD [4-item] Severity Distribution",
       x = "\n Number of Months From Baseline",
       y = "Cohort Proportion \n",
       fill = "ICD Total Severity") +
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

ggsave(file = "../out/stacked_icd_sev.svg", plot = plot, width = 8, height = 5)


plot <- ecmp %>% inner_join(treat_pats_groups) %>%
   mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  rename("Treatment"="TREATMENT") %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  ggplot(aes(VISIT , ecmp_icd_severity, colour=Treatment, fill=Treatment)) +
  geom_boxplot(alpha=0.5, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.6, size=0.5, stroke=2, shape=1, width = 0.3, height=0.1) +
  theme_minimal() +
  scale_colour_manual(values=c("#aa3951", "#2f3941")) +
  scale_fill_manual(values=c("#aa3951", "#2f3941")) +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top") +
  labs(title = "ECMP-based ICD [4-item] Distribution",
       x = "\n Number of Months From Baseline",
       y = "ICD Total Score \n",
       fill = "Treatment") 


plot

ggsave(file = "../out/box_icd_sev.svg", plot = plot, width = 4, height = 5)


data <- ecmp %>% inner_join(treat_pats_groups) %>%
   mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  rename("Treatment"="TREATMENT") %>% select(Treatment, SUBJID, VISIT, ecmp_icd_severity)



surv_data 



