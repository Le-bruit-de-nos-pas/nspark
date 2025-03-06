library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)

# overall data ---------------------------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))


# --------------------------------

# Levodopa ON vs OFF at baseline H&Y -------------------------------


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, Contains_Levodopa) %>% count()


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, FRZGT1W ) %>% count()


data_1 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FRZGT1W=ifelse(is.na(FRZGT1W),0,FRZGT1W)) %>%
  mutate(FRZGT1W=cumsum(FRZGT1W)) %>%
  mutate(FRZGT1W=ifelse(FRZGT1W==0,0,1)) %>%
  filter(FRZGT1W==1) %>%
  filter(elapsed==min(elapsed)) %>%
  ungroup() %>% select(elapsed, FRZGT1W, LD_baseline) %>% ungroup()


data_2 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FRZGT1W=ifelse(is.na(FRZGT1W),0,FRZGT1W)) %>%
  mutate(FRZGT1W=cumsum(FRZGT1W)) %>%
  mutate(FRZGT1W=ifelse(FRZGT1W==0,0,1)) %>%
  anti_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(FRZGT1W>0) %>% select(PATNO) %>% distinct()) %>%
  filter(elapsed==max(elapsed)) %>%
  ungroup() %>% select(elapsed, FRZGT1W, LD_baseline ) %>% ungroup()


data <- data_1 %>% bind_rows(data_2)

unique(data$FRZGT1W)

unique(data$LD_baseline)

sum(is.na(data))

mean(data$FRZGT1W)
mean(data$LD_baseline)


data <- data %>% mutate(LD_baseline=ifelse(LD_baseline==1, "Levodopa baseline", "no LD baseline"))

library(survival)
library(survminer)


km_fit <- survfit(Surv(elapsed , FRZGT1W  ) ~ LD_baseline   , data = data)

summary(km_fit)

km_fit

data %>% group_by(LD_baseline, FRZGT1W) %>% count()

# 600x600

# Step 3: Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = data, 
           pval = FALSE,          # Add p-value for log-rank test
           conf.int = TRUE,      # Add confidence interval
           #risk.table = TRUE,    # Add risk table to the plot
           palette = c("#CD3333", "#83CBEB"), # Example color palette
           ggtheme = theme_minimal(),
           xlab=("\n Number of days From Baseline"),
           ylab=("Proportion FOG-free \n")) # Clean theme

# Step 4: Summary and log-rank test to compare the groups
summary(km_fit)

# Optional: If you want to do a formal comparison
log_rank_test <- survdiff(Surv(elapsed , FRZGT1W ) ~ LD_baseline, data = data)

log_rank_test

survdiff(Surv(elapsed , FRZGT1W ) ~ LD_baseline, data = data, rho = 1)  # Peto-Peto test

# Call:
# survdiff(formula = Surv(elapsed, FRZGT1W) ~ LD_baseline, data = data, 
#     rho = 1)
# 
#                                  N Observed Expected (O-E)^2/E (O-E)^2/V
# LD_baseline=Levodopa baseline   13     2.52     0.87    3.1128      3.38
# LD_baseline=no LD baseline    1044   190.59   192.23    0.0141      3.38
# 
#  Chisq= 3.4  on 1 degrees of freedom, p= 0.07 


coxph(Surv(elapsed, FRZGT1W) ~ LD_baseline, data = data)

# Call:
# coxph(formula = Surv(elapsed, FRZGT1W) ~ LD_baseline, data = data)
# 
#                              coef exp(coef) se(coef)      z      p
# LD_baselineno LD baseline -1.1915    0.3038   0.5855 -2.035 0.0419
# 
# Likelihood ratio test=2.93  on 1 df, p=0.08691
# n= 1057, number of events= 274 







data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

data <- data %>% select(PATNO, elapsed, LD_baseline, hy_on )

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


library(nlme)

lme_model <- nlme::lme(fixed = hy_on ~ elapsed + LD_baseline,
                 random = ~ elapsed | PATNO,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC    BIC    logLik
#   6869.263 6915.8 -3427.632
# 
# Random effects:
#  Formula: ~elapsed | PATNO
#  Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev     Corr  
# (Intercept) 0.31156648 (Intr)
# elapsed     0.04766092 0.083 
# Residual    0.37027027       
# 
# Fixed effects:  hy_on ~ elapsed + LD_baseline 
#                 Value  Std.Error   DF   t-value p-value
# (Intercept) 1.8831412 0.01205813 4644 156.17191  0.0000
# elapsed     0.0559892 0.00275354 4644  20.33355  0.0000
# LD_baseline 0.2086848 0.10623752 1055   1.96432  0.0498
#  Correlation: 
#             (Intr) elapsd
# elapsed      0.309       
# LD_baseline -0.100  0.010
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -5.7175389 -0.4138536  0.1035038  0.4078185  3.8053817 
# 
# Number of Observations: 5702
# Number of Groups: 1057 


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, hy_stage = hy_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$hy_stage))



data <- data %>% select(ID, time, hy_stage , FRZGT1W, LD_baseline)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ LD_baseline  + hy_stage + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ LD_baseline + hy_stage, 
#     data = data, cluster = ID)
# 
#   n= 4645, number of events= 550 
# 
#                coef exp(coef) se(coef) robust se     z Pr(>|z|)    
# LD_baseline 0.86852   2.38337  0.35801   0.21001 4.136 3.54e-05 ***
# hy_stage    0.57684   1.78041  0.08466   0.09852 5.855 4.76e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# LD_baseline     2.383     0.4196     1.579     3.597
# hy_stage        1.780     0.5617     1.468     2.160
# 
# Concordance= 0.577  (se = 0.013 )
# Likelihood ratio test= 50.96  on 2 df,   p=9e-12
# Wald test            = 48.81  on 2 df,   p=3e-11
# Score (logrank) test = 53.02  on 2 df,   p=3e-12,   Robust = 31.04  p=2e-07
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).




survminer::ggforest(cox_model_td, data = data)








# --------------------------------


# NOT ON LEVODOPA AT BASELINE H&Y-----------------------------------------

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, Contains_Levodopa) %>% count()


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, FRZGT1W ) %>% count()


  

data_1 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FRZGT1W=ifelse(is.na(FRZGT1W),0,FRZGT1W)) %>%
  mutate(FRZGT1W=cumsum(FRZGT1W)) %>%
  mutate(FRZGT1W=ifelse(FRZGT1W==0,0,1)) %>%
  filter(FRZGT1W==1) %>%
  filter(elapsed==min(elapsed)) %>%
  ungroup() %>% select(elapsed, FRZGT1W, Levodopa_EXP) %>% ungroup()


data_2 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FRZGT1W=ifelse(is.na(FRZGT1W),0,FRZGT1W)) %>%
  mutate(FRZGT1W=cumsum(FRZGT1W)) %>%
  mutate(FRZGT1W=ifelse(FRZGT1W==0,0,1)) %>%
  anti_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(FRZGT1W>0) %>% select(PATNO) %>% distinct()) %>%
  filter(elapsed==max(elapsed)) %>%
  ungroup() %>% select(elapsed, FRZGT1W, Levodopa_EXP ) %>% ungroup()



data <- data_1 %>% bind_rows(data_2)

unique(data$FRZGT1W)

sum(is.na(data))


data <- data %>% mutate(Levodopa_EXP=ifelse(Levodopa_EXP==1, "Levodopa-experienced", "Levodopa-naive"))





library(survival)
library(survminer)


km_fit <- survfit(Surv(elapsed , FRZGT1W  ) ~ Levodopa_EXP   , data = data)

summary(km_fit)

km_fit

data %>% group_by(Levodopa_EXP, FRZGT1W) %>% count()

# Step 3: Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = data, 
           pval = TRUE,          # Add p-value for log-rank test
           conf.int = TRUE,      # Add confidence interval
           #risk.table = TRUE,    # Add risk table to the plot
           palette = c("#CD3333", "#83CBEB"), # Example color palette
           ggtheme = theme_minimal(),
           xlab=("\n Number of days From Baseline"),
           ylab=("Proportion FOG-free \n")) # Clean theme

# Step 4: Summary and log-rank test to compare the groups
summary(km_fit)

# Optional: If you want to do a formal comparison
log_rank_test <- survdiff(Surv(elapsed , FRZGT1W ) ~ Levodopa_EXP, data = data)

log_rank_test

survdiff(Surv(elapsed , FRZGT1W ) ~ Levodopa_EXP, data = data, rho = 1)  # Peto-Peto test

# Call:
# survdiff(formula = Surv(elapsed, FRZGT1W) ~ Levodopa_EXP, data = data, 
#     rho = 1)
# 
#                                     N Observed Expected (O-E)^2/E (O-E)^2/V
# Levodopa_EXP=Levodopa-experienced 249     65.6     75.4     1.250      2.77
# Levodopa_EXP=Levodopa-naive       795    125.6    115.8     0.813      2.77
# 
#  Chisq= 2.8  on 1 degrees of freedom, p= 0.1 
#  
coxph(Surv(elapsed, FRZGT1W) ~ Levodopa_EXP, data = data)

# Call:
# coxph(formula = Surv(elapsed, FRZGT1W) ~ Levodopa_EXP, data = data)
# 
#                              coef exp(coef) se(coef)     z     p
# Levodopa_EXPLevodopa-naive 0.1638    1.1780   0.1273 1.287 0.198
# 
# Likelihood ratio test=1.68  on 1 df, p=0.1951
# n= 1044, number of events= 271 




data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

data <- data %>% select(PATNO, elapsed, Levodopa_EXP, hy_on )


data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


library(nlme)

lme_model <- nlme::lme(fixed = hy_on ~ elapsed + Levodopa_EXP,
                 random = ~ elapsed | PATNO,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   6774.352 6820.817 -3380.176
# 
# Random effects:
#  Formula: ~elapsed | PATNO
#  Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev     Corr  
# (Intercept) 0.31119183 (Intr)
# elapsed     0.04731109 0.069 
# Residual    0.36924609       
# 
# Fixed effects:  hy_on ~ elapsed + Levodopa_EXP 
#                   Value  Std.Error   DF   t-value p-value
# (Intercept)   1.8914326 0.01412293 4599 133.92633  0.0000
# elapsed       0.0563565 0.00276394 4599  20.38993  0.0000
# Levodopa_EXP -0.0275452 0.02604049 1042  -1.05778  0.2904
#  Correlation: 
#              (Intr) elapsd
# elapsed       0.315       
# Levodopa_EXP -0.523 -0.111
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -5.7357328 -0.4162793  0.1060922  0.4063696  3.8118402 
# 
# Number of Observations: 5644
# Number of Groups: 1044 



data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, hy_stage = hy_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$hy_stage))



data <- data %>% select(ID, time, hy_stage , FRZGT1W, Levodopa_EXP)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ Levodopa_EXP  + hy_stage + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ Levodopa_EXP + hy_stage, 
#     data = data, cluster = ID)
# 
#   n= 4600, number of events= 542 
# 
#                  coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Levodopa_EXP -0.04977   0.95145  0.08829   0.11275 -0.441    0.659    
# hy_stage      0.57880   1.78389  0.08581   0.10042  5.764 8.22e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#              exp(coef) exp(-coef) lower .95 upper .95
# Levodopa_EXP    0.9514     1.0510    0.7628     1.187
# hy_stage        1.7839     0.5606    1.4652     2.172
# 
# Concordance= 0.592  (se = 0.016 )
# Likelihood ratio test= 45.98  on 2 df,   p=1e-10
# Wald test            = 33.53  on 2 df,   p=5e-08
# Score (logrank) test = 45.79  on 2 df,   p=1e-10,   Robust = 31.91  p=1e-07
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).




survminer::ggforest(cox_model_td, data = data)



# ---------------
# Levodopa ON vs OFF at baseline UPDRS III -------------------------------

updrs3_score_on <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(updrs3_score_on)

updrs3_score_on <- updrs3_score_on %>% select(PATNO,updrs3_score_on, visit_date)

updrs3_score_on <- updrs3_score_on %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 

updrs3_score_on$PATNO <- as.numeric(updrs3_score_on$PATNO)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(updrs3_score_on)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>% mutate(updrs3_score_on=updrs3_score_on/10)

data <- data %>% select(PATNO, elapsed, LD_baseline, updrs3_score_on  )

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


library(nlme)

lme_model <- nlme::lme(fixed = updrs3_score_on  ~ elapsed + LD_baseline,
                 random = ~ elapsed | PATNO,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC     BIC    logLik
#   14708.97 14755.3 -7347.483
# 
# Random effects:
#  Formula: ~elapsed | PATNO
#  Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev    Corr  
# (Intercept) 0.9131453 (Intr)
# elapsed     0.1365889 0.475 
# Residual    0.7152904       
# 
# Fixed effects:  updrs3_score_on ~ elapsed + LD_baseline 
#                 Value  Std.Error   DF  t-value p-value
# (Intercept) 2.3797699 0.03314711 4484 71.79419  0.0000
# elapsed     0.0794157 0.00718230 4484 11.05714  0.0000
# LD_baseline 0.2113505 0.25556901 1055  0.82698  0.4084
#  Correlation: 
#             (Intr) elapsd
# elapsed      0.529       
# LD_baseline -0.092  0.003
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.92306225 -0.57112885 -0.06076698  0.50830690  5.29246915 
# 
# Number of Observations: 5542
# Number of Groups: 1057 



data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>% mutate(updrs3_score_on=updrs3_score_on/10)

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, updrs3_score_on = updrs3_score_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$updrs3_score_on))


data <- data %>% select(ID, time, updrs3_score_on , FRZGT1W, LD_baseline)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ LD_baseline  + updrs3_score_on + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ LD_baseline + updrs3_score_on, 
#     data = data, cluster = ID)
# 
#   n= 4488, number of events= 478 
# 
#                    coef exp(coef) se(coef) robust se     z Pr(>|z|)    
# LD_baseline     1.10614   3.02268  0.35894   0.28306 3.908 9.32e-05 ***
# updrs3_score_on 0.23165   1.26068  0.03188   0.03690 6.278 3.43e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# LD_baseline         3.023     0.3308     1.736     5.264
# updrs3_score_on     1.261     0.7932     1.173     1.355
# 
# Concordance= 0.617  (se = 0.021 )
# Likelihood ratio test= 56.14  on 2 df,   p=6e-13
# Wald test            = 47.62  on 2 df,   p=5e-11
# Score (logrank) test = 60.54  on 2 df,   p=7e-14,   Robust = 34.36  p=3e-08
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).



survminer::ggforest(cox_model_td, data = data)








# --------------------------------


# NOT ON LEVODOPA AT BASELINE UPDRS III-----------------------------------------

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))





data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>% mutate(updrs3_score_on=updrs3_score_on/10)

data <- data %>% select(PATNO, elapsed, Levodopa_EXP, updrs3_score_on )


data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


library(nlme)

lme_model <- nlme::lme(fixed = updrs3_score_on ~ elapsed + Levodopa_EXP,
                 random = ~ elapsed | PATNO,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   14551.16 14597.43 -7268.582
# 
# Random effects:
#  Formula: ~elapsed | PATNO
#  Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev    Corr  
# (Intercept) 0.9136944 (Intr)
# elapsed     0.1369071 0.478 
# Residual    0.7148268       
# 
# Fixed effects:  updrs3_score_on ~ elapsed + Levodopa_EXP 
#                   Value  Std.Error   DF  t-value p-value
# (Intercept)   2.3970129 0.03717197 4439 64.48441  0.0000
# elapsed       0.0804727 0.00722896 4439 11.13198  0.0000
# Levodopa_EXP -0.0568112 0.06505762 1042 -0.87324  0.3827
#  Correlation: 
#              (Intr) elapsd
# elapsed       0.490       
# Levodopa_EXP -0.448 -0.029
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.92625995 -0.57399606 -0.06263544  0.50457613  5.29345780 
# 
# Number of Observations: 5484
# Number of Groups: 1044 



data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>% mutate(updrs3_score_on=updrs3_score_on/10)

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, updrs3_score_on  = updrs3_score_on )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$updrs3_score_on ))



data <- data %>% select(ID, time, updrs3_score_on  , FRZGT1W, Levodopa_EXP)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ Levodopa_EXP  + updrs3_score_on  + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# 
# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ Levodopa_EXP + updrs3_score_on, 
#     data = data, cluster = ID)
# 
#   n= 4443, number of events= 470 
# 
#                    coef exp(coef) se(coef) robust se     z Pr(>|z|)    
# Levodopa_EXP    0.02497   1.02529  0.09464   0.11972 0.209    0.835    
# updrs3_score_on 0.23221   1.26138  0.03211   0.03712 6.255 3.97e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# Levodopa_EXP        1.025     0.9753    0.8109     1.296
# updrs3_score_on     1.261     0.7928    1.1729     1.357
# 
# Concordance= 0.614  (se = 0.021 )
# Likelihood ratio test= 50.51  on 2 df,   p=1e-11
# Wald test            = 39.15  on 2 df,   p=3e-09
# Score (logrank) test = 52.88  on 2 df,   p=3e-12,   Robust = 33.38  p=6e-08
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# ---------------
# All patient visits ----------------
# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 




PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))


PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(Contains_Levodopa, FRZGT1W) %>% count() %>%
  drop_na() %>% ungroup() %>% group_by(Contains_Levodopa) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)



test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(disease_duration=as.numeric(disease_duration)) %>% drop_na()



model <- ordinal::clm(as.factor(FRZGT1W) ~ disease_duration, data = test)

summary(model)


# formula: as.factor(FRZGT1W) ~ disease_duration
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2933 -2208.04 4426.07 6(0)  5.13e-09 3.2e+03
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# disease_duration  0.22251    0.01118   19.91   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   2.6961     0.1019   26.46
# 1|2   3.5296     0.1116   31.61
# 2|3   4.6656     0.1319   35.36
# 3|4   6.7824     0.2481   27.34


test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, Contains_Levodopa) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>% drop_na()


model <- ordinal::clm(as.factor(FRZGT1W) ~ as.factor(Contains_Levodopa ) , data = test)

summary(model)

# formula: as.factor(FRZGT1W) ~ as.factor(Contains_Levodopa)
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2934 -2444.48 4898.97 8(0)  6.44e-12 8.0e+01
# 
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)
# as.factor(Contains_Levodopa)1   0.1087     0.1963   0.554     0.58
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  1.13587    0.04405   25.79
# 1|2  1.86595    0.05498   33.94
# 2|3  2.91948    0.08418   34.68
# 3|4  4.98701    0.22460   22.20



PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(hy_on, FRZGT1W) %>% count() %>%
  drop_na() %>% ungroup() %>% group_by(hy_on) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)


test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, hy_on) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(hy_on=as.numeric(hy_on)) %>% drop_na()


model <- ordinal::clm(as.factor(FRZGT1W) ~ as.factor(hy_on ) , data = test)

summary(model)

# formula: as.factor(FRZGT1W) ~ as.factor(hy_on)
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2788 -2118.50 4250.99 7(0)  1.22e-11 3.3e+03
# 
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# as.factor(hy_on)1  -0.7163     0.6511  -1.100 0.271307    
# as.factor(hy_on)2   0.2841     0.6330   0.449 0.653580    
# as.factor(hy_on)3   2.4208     0.6408   3.778 0.000158 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.5811     0.6308   2.507
# 1|2   2.4267     0.6321   3.839
# 2|3   3.6125     0.6364   5.676
# 3|4   5.8548     0.6764   8.655





PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(disease_duration , FRZGT1W ) %>% count() %>%
  drop_na() %>%
  spread(key=FRZGT1W, value=n) %>%
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>%
  mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
  mutate(`2`=ifelse(is.na(`2`),0,`2`)) %>%
  mutate(`3`=ifelse(is.na(`3`),0,`3`)) %>%
  mutate(`4`=ifelse(is.na(`4`),0,`4`)) %>%
  mutate(tot=`0`+`1`+`2`+`3`+`4`) %>%
  mutate(`0`=`0`/tot) %>%
  mutate(`1`=`1`/tot) %>%
  mutate(`2`=`2`/tot) %>%
  mutate(`3`=`3`/tot) %>%
  mutate(`4`=`4`/tot)  %>% ungroup() %>%
  gather(FOG, value, `0`:`4`) %>%
  ggplot(aes(disease_duration, value, colour=FOG, fill=FOG)) +
  geom_smooth(se=F, size=2, alpha=0.5) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  ylab("Proportion of patient-visits \n") + xlab("\n Disease duration [years]") +
  scale_colour_manual(values=c("#F2F2F2", "#83CBEB", "#104862", "#F6C6AD", "#CD3333")) 




test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, hy_on, Contains_Levodopa, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>%
    mutate(hy_on=as.numeric(hy_on)) %>% drop_na()


dim(test)[1] # 2787

PPMI_Curated_Data_Cut_Public_20241211 %>% select(PATNO, FRZGT1W, hy_on, Contains_Levodopa, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>%
    mutate(hy_on=as.numeric(hy_on)) %>% drop_na() %>% select(PATNO) %>% distinct() # 1147

# Step 1: Total Effect
model_total <- lm(FRZGT1W  ~ as.factor(Contains_Levodopa ),  data=test)

# Step 2: Mediators Path
model_mediator1 <- lm(hy_on  ~ as.factor(Contains_Levodopa), data=test)
model_mediator2 <- lm(disease_duration  ~ as.factor(Contains_Levodopa), data=test)

# Step 3: Direct and Indirect Effects
model_direct <- lm(FRZGT1W ~ as.factor(Contains_Levodopa) +hy_on + disease_duration,  data=test)

# Mediation Analysis
# Mediation Analysis for Each Mediator
mediate_result1 <- mediation::mediate(model_mediator1, model_direct, treat = "as.factor(Contains_Levodopa)", mediator = "hy_on")
mediate_result2 <- mediation::mediate(model_mediator2, model_direct, treat = "as.factor(Contains_Levodopa)", mediator = "disease_duration")

# Summary of Mediation Results
summary(mediate_result1)
summary(mediate_result2)

# 
# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME             0.0194      -0.0129         0.05    0.23
# ADE              0.0141      -0.1240         0.14    0.89
# Total Effect     0.0335      -0.1027         0.16    0.66
# Prop. Mediated   0.1844      -6.3698         3.44    0.63
# 
# Sample Size Used: 2787 
# 
# 
# Simulations: 1000 
# 
# > summary(mediate_result2)
# 
# Causal Mediation Analysis  
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME             0.0260      -0.0203         0.07    0.29
# ADE              0.0148      -0.1143         0.15    0.81
# Total Effect     0.0408      -0.1013         0.19    0.59
# Prop. Mediated   0.2446      -3.7531         4.90    0.61
# 
# Sample Size Used: 2787 
# 
# 
# Simulations: 1000 
# 
# > 
#   
#   
#   
# -------

# All patient visits starting without FOG ----------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 




PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  filter(FOG_baseline==0)




test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(disease_duration=as.numeric(disease_duration)) %>% drop_na()



model <- ordinal::clm(as.factor(FRZGT1W) ~ disease_duration, data = test)

summary(model)

# formula: as.factor(FRZGT1W) ~ disease_duration
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2849 -1987.45 3984.89 6(0)  9.38e-08 3.8e+03
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# disease_duration  0.27304    0.01296   21.07   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   3.2647     0.1240   26.32
# 1|2   4.0441     0.1329   30.44
# 2|3   5.1889     0.1517   34.22
# 3|4   7.2757     0.2596   28.02



PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(Contains_Levodopa, FRZGT1W) %>% count() %>%
  drop_na() %>% ungroup() %>% group_by(Contains_Levodopa) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)


test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, Contains_Levodopa) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>% drop_na()


model <- ordinal::clm(as.factor(FRZGT1W) ~ as.factor(Contains_Levodopa ) , data = test)

summary(model)

# formula: as.factor(FRZGT1W) ~ as.factor(Contains_Levodopa)
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2850 -2281.92 4573.85 6(1)  3.99e-07 8.3e+01
# 
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)
# as.factor(Contains_Levodopa)1  0.06335    0.20839   0.304    0.761
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  1.22217    0.04571   26.74
# 1|2  1.88087    0.05612   33.51
# 2|3  2.92919    0.08586   34.12
# 3|4  4.95535    0.22463   22.06



PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(hy_on, FRZGT1W) %>% count() %>%
  drop_na() %>% ungroup() %>% group_by(hy_on) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)


test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, hy_on) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(hy_on=as.numeric(hy_on)) %>% drop_na()


model <- ordinal::clm(as.factor(FRZGT1W) ~ as.factor(hy_on ) , data = test)

summary(model)

# formula: as.factor(FRZGT1W) ~ as.factor(hy_on)
# data:    test
# 
#  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#  logit flexible  2707 -1951.73 3917.46 7(0)  5.45e-08 4.6e+03
# 
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# as.factor(hy_on)1  -0.5387     0.7742  -0.696 0.486543    
# as.factor(hy_on)2   0.5402     0.7555   0.715 0.474598    
# as.factor(hy_on)3   2.7658     0.7620   3.630 0.000284 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.9563     0.7534   2.597
# 1|2   2.7287     0.7546   3.616
# 2|3   3.9253     0.7584   5.176
# 3|4   6.1303     0.7921   7.739




PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(disease_duration , FRZGT1W ) %>% count() %>%
  drop_na() %>%
  spread(key=FRZGT1W, value=n) %>%
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>%
  mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
  mutate(`2`=ifelse(is.na(`2`),0,`2`)) %>%
  mutate(`3`=ifelse(is.na(`3`),0,`3`)) %>%
  mutate(`4`=ifelse(is.na(`4`),0,`4`)) %>%
  mutate(tot=`0`+`1`+`2`+`3`+`4`) %>%
  mutate(`0`=`0`/tot) %>%
  mutate(`1`=`1`/tot) %>%
  mutate(`2`=`2`/tot) %>%
  mutate(`3`=`3`/tot) %>%
  mutate(`4`=`4`/tot)  %>% ungroup() %>%
  gather(FOG, value, `0`:`4`) %>%
  ggplot(aes(disease_duration, value, colour=FOG, fill=FOG)) +
  geom_smooth(se=F, size=2, alpha=0.5) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  ylab("Proportion of patient-visits \n") + xlab("\n Disease duration [years]") +
  scale_colour_manual(values=c("#F2F2F2", "#83CBEB", "#104862", "#F6C6AD", "#CD3333")) 




test <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(FRZGT1W, hy_on, Contains_Levodopa, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>%
    mutate(hy_on=as.numeric(hy_on)) %>% drop_na()


dim(test)[1] # 2706

PPMI_Curated_Data_Cut_Public_20241211 %>% select(PATNO, FRZGT1W, hy_on, Contains_Levodopa, disease_duration) %>% 
  mutate(FRZGT1W=as.numeric(FRZGT1W)) %>%
  mutate(Contains_Levodopa=as.numeric(Contains_Levodopa)) %>%
    mutate(hy_on=as.numeric(hy_on)) %>% drop_na() %>% select(PATNO) %>% distinct() # 1102

# Step 1: Total Effect
model_total <- lm(FRZGT1W  ~ as.factor(Contains_Levodopa ),  data=test)

# Step 2: Mediators Path
model_mediator1 <- lm(hy_on  ~ as.factor(Contains_Levodopa), data=test)
model_mediator2 <- lm(disease_duration  ~ as.factor(Contains_Levodopa), data=test)

# Step 3: Direct and Indirect Effects
model_direct <- lm(FRZGT1W ~ as.factor(Contains_Levodopa) +hy_on + disease_duration,  data=test)

# Mediation Analysis
# Mediation Analysis for Each Mediator
mediate_result1 <- mediation::mediate(model_mediator1, model_direct, treat = "as.factor(Contains_Levodopa)", mediator = "hy_on")
mediate_result2 <- mediation::mediate(model_mediator2, model_direct, treat = "as.factor(Contains_Levodopa)", mediator = "disease_duration")

# Summary of Mediation Results
summary(mediate_result1)
summary(mediate_result2)



# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME            0.01574     -0.01909         0.05    0.33
# ADE            -0.00818     -0.14573         0.13    0.90
# Total Effect    0.00756     -0.13276         0.15    0.91
# Prop. Mediated  0.09926     -3.38900         4.71    0.84
# 
# Sample Size Used: 2706 
# 
# 
# Simulations: 1000 
# 
# > summary(mediate_result2)
# 
# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME            0.03540     -0.01422         0.09    0.16
# ADE            -0.00449     -0.14297         0.13    0.92
# Total Effect    0.03091     -0.11094         0.17    0.70
# Prop. Mediated  0.34105     -6.96541         8.49    0.66
# 
# Sample Size Used: 2706 
# 
# 
# Simulations: 1000 


# ---------
# overall data LEDD ---------------------------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))


# --------------------------------

# Levodopa ON vs OFF at baseline LEDD -------------------------------



LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  left_join(Levodopa_lookups_complete) %>% filter(Contains_Levodopa==1)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, ORIG_ENTRY, LEDD) %>% drop_na() %>%
 mutate(LEDD=as.numeric(LEDD)) %>% group_by(PATNO, ORIG_ENTRY) %>% summarise(LEDD=sum(LEDD, na.rm=T)) 


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y")) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO","visit_date"="ORIG_ENTRY"))

library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))


data <- data %>% select(PATNO, elapsed, LD_baseline, LEDD )

data <- data %>% mutate(LEDD=ifelse(is.na(LEDD),0,LEDD))

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


library(nlme)

lme_model <- nlme::lme(fixed = LEDD ~ elapsed + LD_baseline,
                 random = ~ elapsed | PATNO,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC    BIC    logLik
#   6869.263 6915.8 -3427.632
# 
# Random effects:
#  Formula: ~elapsed | PATNO
#  Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev     Corr  
# (Intercept) 0.31156648 (Intr)
# elapsed     0.04766092 0.083 
# Residual    0.37027027       
# 
# Fixed effects:  hy_on ~ elapsed + LD_baseline 
#                 Value  Std.Error   DF   t-value p-value
# (Intercept) 1.8831412 0.01205813 4644 156.17191  0.0000
# elapsed     0.0559892 0.00275354 4644  20.33355  0.0000
# LD_baseline 0.2086848 0.10623752 1055   1.96432  0.0498
#  Correlation: 
#             (Intr) elapsd
# elapsed      0.309       
# LD_baseline -0.100  0.010
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -5.7175389 -0.4138536  0.1035038  0.4078185  3.8053817 
# 
# Number of Observations: 5702
# Number of Groups: 1057 


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, LEDD = LEDD)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$LEDD))

data <- data %>% mutate(LEDD=ifelse(is.na(LEDD),0,LEDD))


data <- data %>% select(ID, time, LEDD , FRZGT1W, LD_baseline)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

data$LEDD <- data$LEDD/100

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ LD_baseline  + LEDD + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# 
# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ LD_baseline + LEDD, 
#     data = data, cluster = ID)
# 
#   n= 4937, number of events= 589 
# 
#                 coef exp(coef) se(coef) robust se     z Pr(>|z|)   
# LD_baseline 0.878939  2.408344 0.357371  0.330910 2.656  0.00790 **
# LEDD        0.017711  1.017869 0.009034  0.006039 2.933  0.00336 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# LD_baseline     2.408     0.4152     1.259     4.607
# LEDD            1.018     0.9824     1.006     1.030
# 
# Concordance= 0.519  (se = 0.01 )
# Likelihood ratio test= 7.89  on 2 df,   p=0.02
# Wald test            = 15.34  on 2 df,   p=5e-04
# Score (logrank) test = 10.38  on 2 df,   p=0.006,   Robust = 5.36  p=0.07
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).



survminer::ggforest(cox_model_td, data = data)








# --------------------------------


# NOT ON LEVODOPA AT BASELINE LEDD-----------------------------------------


LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  left_join(Levodopa_lookups_complete) %>% filter(Contains_Levodopa==1)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, ORIG_ENTRY, LEDD) %>% drop_na() %>%
 mutate(LEDD=as.numeric(LEDD)) %>% group_by(PATNO, ORIG_ENTRY) %>% summarise(LEDD=sum(LEDD, na.rm=T)) 


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y")) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO","visit_date"="ORIG_ENTRY"))


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))


library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

data <- data %>% select(PATNO, elapsed, Levodopa_EXP, LEDD )

data <- data %>% mutate(LEDD=ifelse(is.na(LEDD),0,LEDD))

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365



data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, LEDD = LEDD)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$LEDD))

data <- data %>% mutate(LEDD=ifelse(is.na(LEDD),0,LEDD))

data <- data %>% select(ID, time, LEDD , FRZGT1W, Levodopa_EXP)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

data$LEDD <- data$LEDD/100
# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ Levodopa_EXP  + LEDD + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ Levodopa_EXP + LEDD, 
#     data = data, cluster = ID)
# 
#   n= 4891, number of events= 581 
# 
#                   coef exp(coef)  se(coef) robust se      z Pr(>|z|)   
# Levodopa_EXP -0.065727  0.936387  0.085486  0.115592 -0.569  0.56962   
# LEDD          0.018558  1.018731  0.009026  0.006075  3.055  0.00225 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#              exp(coef) exp(-coef) lower .95 upper .95
# Levodopa_EXP    0.9364     1.0679    0.7466     1.174
# LEDD            1.0187     0.9816    1.0067     1.031
# 
# Concordance= 0.54  (se = 0.014 )
# Likelihood ratio test= 3.91  on 2 df,   p=0.1
# Wald test            = 9.35  on 2 df,   p=0.009
# Score (logrank) test = 4.61  on 2 df,   p=0.1,   Robust = 4.24  p=0.1
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).




survminer::ggforest(cox_model_td, data = data)



# ---------------
# overall data LEDD ---------------------------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))


# --------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))



# Levodopa ON vs OFF at baseline LD % LEDD -------------------------------


LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  left_join(Levodopa_lookups_complete)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  select(PATNO, ORIG_ENTRY, LEDD, Contains_Levodopa) %>% 
 mutate(LEDD=as.numeric(LEDD)) %>% group_by(PATNO, ORIG_ENTRY, Contains_Levodopa) %>% summarise(LEDD=sum(LEDD, na.rm=T))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%  group_by(PATNO, ORIG_ENTRY) %>% 
  mutate(TOTAL=sum(LEDD, na.rm=T)) %>% mutate(perc=LEDD/TOTAL) %>% filter(Contains_Levodopa==1)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(-c(Contains_Levodopa, LEDD, TOTAL))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y")) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO","visit_date"="ORIG_ENTRY"))


library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))


data <- data %>% select(PATNO, elapsed, LD_baseline, perc )

data <- data %>% mutate(perc=ifelse(is.na(perc),0,perc))

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365



data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, perc = perc)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$perc))

data <- data %>% mutate(perc=ifelse(is.na(perc),0,perc))


data <- data %>% select(ID, time, perc , FRZGT1W, LD_baseline)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))



library(survival)

data <- data %>% rename("LD_Proportion_LEDD"="perc")

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ LD_baseline  + LD_Proportion_LEDD + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ LD_baseline + LD_Proportion_LEDD, 
#     data = data, cluster = ID)
# 
#   n= 4937, number of events= 589 
# 
#                      coef exp(coef) se(coef) robust se     z Pr(>|z|)   
# LD_baseline        0.8776    2.4050   0.3576    0.3292 2.666  0.00768 **
# LD_Proportion_LEDD 0.0943    1.0989   0.1160    0.1064 0.886  0.37551   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                    exp(coef) exp(-coef) lower .95 upper .95
# LD_baseline            2.405     0.4158     1.262     4.585
# LD_Proportion_LEDD     1.099     0.9100     0.892     1.354
# 
# Concordance= 0.519  (se = 0.01 )
# Likelihood ratio test= 5.38  on 2 df,   p=0.07
# Wald test            = 7.9  on 2 df,   p=0.02
# Score (logrank) test = 7.25  on 2 df,   p=0.03,   Robust = 2.37  p=0.3
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).



survminer::ggforest(cox_model_td, data = data)








# --------------------------------


# NOT ON LEVODOPA AT BASELINE LD % LEDD -----------------------------------------


LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  left_join(Levodopa_lookups_complete)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  select(PATNO, ORIG_ENTRY, LEDD, Contains_Levodopa) %>% 
 mutate(LEDD=as.numeric(LEDD)) %>% group_by(PATNO, ORIG_ENTRY, Contains_Levodopa) %>% summarise(LEDD=sum(LEDD, na.rm=T))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%  group_by(PATNO, ORIG_ENTRY) %>% 
  mutate(TOTAL=sum(LEDD, na.rm=T)) %>% mutate(perc=LEDD/TOTAL) %>% filter(Contains_Levodopa==1)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(-c(Contains_Levodopa, LEDD, TOTAL))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y")) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO","visit_date"="ORIG_ENTRY"))


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))


library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

data <- data %>% select(PATNO, elapsed, Levodopa_EXP, perc )

data <- data %>% mutate(perc=ifelse(is.na(perc),0,perc))

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365

mean(data$perc)

data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, perc = perc)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$perc))

data <- data %>% mutate(perc=ifelse(is.na(perc),0,perc))

data <- data %>% select(ID, time, perc , FRZGT1W, Levodopa_EXP)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)

data <- data %>% rename("LD_Proportion_LEDD"="perc")

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ Levodopa_EXP  + LD_Proportion_LEDD + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# 
# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ Levodopa_EXP + LD_Proportion_LEDD, 
#     data = data, cluster = ID)
# 
#   n= 4891, number of events= 581 
# 
#                        coef exp(coef) se(coef) robust se      z Pr(>|z|)
# Levodopa_EXP       -0.06114   0.94069  0.08541   0.11565 -0.529    0.597
# LD_Proportion_LEDD  0.10156   1.10689  0.11720   0.10840  0.937    0.349
# 
#                    exp(coef) exp(-coef) lower .95 upper .95
# Levodopa_EXP          0.9407     1.0630    0.7499     1.180
# LD_Proportion_LEDD    1.1069     0.9034    0.8950     1.369
# 
# Concordance= 0.537  (se = 0.013 )
# Likelihood ratio test= 1.2  on 2 df,   p=0.6
# Wald test            = 1.11  on 2 df,   p=0.6
# Score (logrank) test = 1.21  on 2 df,   p=0.5,   Robust = 1.08  p=0.6
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# ---------------
# Summary table starts -----
# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

names(PPMI_Curated_Data_Cut_Public_20241211)


target_1044 <- PPMI_Curated_Data_Cut_Public_20241211


PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(PATNO=as.numeric(PATNO)) %>%
  inner_join(target_1044 %>% select(PATNO) %>% distinct())

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  filter(YEAR==min(YEAR))

mean(PPMI_Curated_Data_Cut_Public_20241211$age, na.rm=T) ; sd(PPMI_Curated_Data_Cut_Public_20241211$age, na.rm=T)
median(PPMI_Curated_Data_Cut_Public_20241211$age, na.rm=T) 
quantile(PPMI_Curated_Data_Cut_Public_20241211$age, 0.25)
quantile(PPMI_Curated_Data_Cut_Public_20241211$age, 0.75)

PPMI_Curated_Data_Cut_Public_20241211 %>% ungroup() %>%
  group_by(SEX) %>% count()

mean(PPMI_Curated_Data_Cut_Public_20241211$duration, na.rm=T) ; sd(PPMI_Curated_Data_Cut_Public_20241211$duration, na.rm=T)
median(PPMI_Curated_Data_Cut_Public_20241211$duration, na.rm=T) 
quantile(PPMI_Curated_Data_Cut_Public_20241211$duration, 0.25)
quantile(PPMI_Curated_Data_Cut_Public_20241211$duration, 0.75)

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(hy_on) %>% count()

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on)

mean(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on, na.rm=T) ; sd(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on, na.rm=T)
median(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on, na.rm=T) 
quantile(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on, 0.25, na.rm=T)
quantile(PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on, 0.75, na.rm=T)



target_1044

length(unique(target_1044$PATNO))

Features_of_Parkinsonism_12Feb2025 <- fread("Medical/Features_of_Parkinsonism_12Feb2025.csv")
Features_of_Parkinsonism_12Feb2025 <- Features_of_Parkinsonism_12Feb2025 %>% inner_join(target_1044 %>% select(PATNO) %>% distinct())


Features_of_Parkinsonism_12Feb2025 <- Features_of_Parkinsonism_12Feb2025 %>%
  mutate(INFODT =as.Date(paste("01/", as.character(INFODT )), "%d/%m/%Y")) 

Features_of_Parkinsonism_12Feb2025 <- Features_of_Parkinsonism_12Feb2025 %>% group_by(PATNO) %>% filter(INFODT==min(INFODT))


Features_of_Parkinsonism_12Feb2025 <- Features_of_Parkinsonism_12Feb2025 %>%
  mutate(FEATBRADY=ifelse(is.na(FEATBRADY),0,FEATBRADY)) %>% mutate(FEATBRADY=ifelse(FEATBRADY==0,0,1)) %>%
  mutate(FEATPOSINS =ifelse(is.na(FEATPOSINS ),0,FEATPOSINS )) %>% mutate(FEATPOSINS =ifelse(FEATPOSINS ==0,0,1)) %>%
  mutate(FEATRIGID=ifelse(is.na(FEATRIGID),0,FEATRIGID)) %>% mutate(FEATRIGID=ifelse(FEATRIGID==0,0,1)) %>%
  mutate(FEATTREMOR =ifelse(is.na(FEATTREMOR ),0,FEATTREMOR )) %>% mutate(FEATTREMOR =ifelse(FEATTREMOR ==0,0,1)) 


mean(Features_of_Parkinsonism_12Feb2025$FEATBRADY)
mean(Features_of_Parkinsonism_12Feb2025$FEATPOSINS)
mean(Features_of_Parkinsonism_12Feb2025$FEATRIGID)
mean(Features_of_Parkinsonism_12Feb2025$FEATTREMOR)


PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

PPMI_Curated_Data_Cut_Public_20241211 %>% select(PATNO, visit_date) %>% mutate(PATNO=as.numeric(PATNO)) %>%
  distinct() %>% group_by(PATNO) %>% count() %>% inner_join(target_1044 %>% select(PATNO) %>% distinct()) %>%
  ungroup() %>%
  summarise(mean=mean(n), sd=sd(n), median=mean(n), q25=quantile(n,0.25), q75=quantile(n, 0.75))
  


MDS_UPDRS_Part_III_12Feb2025 <- fread("Motor___MDS-UPDRS/MDS-UPDRS_Part_III_12Feb2025.csv")

names(MDS_UPDRS_Part_III_12Feb2025)


MDS_UPDRS_Part_III_12Feb2025 <- MDS_UPDRS_Part_III_12Feb2025 %>% inner_join(target_1044 %>% select(PATNO) %>% distinct())


MDS_UPDRS_Part_III_12Feb2025 %>%
  mutate(EXAMDT =as.Date(paste("01/", as.character(EXAMDT )), "%d/%m/%Y")) %>%
  select(PATNO, EXAMDT, NP3RISNG,NP3GAIT ,NP3FRZGT, NP3PSTBL) %>%
  mutate(Axial=NP3RISNG+NP3GAIT+NP3FRZGT+NP3PSTBL) %>%
  filter(!is.na(Axial)) %>%
  group_by(PATNO) %>% filter(EXAMDT==min(EXAMDT)) %>%
   ungroup() %>%
  summarise(mean=mean(Axial), sd=sd(Axial), median=mean(Axial), q25=quantile(Axial,0.25), q75=quantile(Axial, 0.75))
  


MDS_UPDRS_Part_III_12Feb2025 %>%
  mutate(EXAMDT =as.Date(paste("01/", as.character(EXAMDT )), "%d/%m/%Y")) %>%
  select(PATNO, EXAMDT, NP3RIGLL, NP3RIGLU, NP3RIGN, NP3RIGRL, NP3RIGRU) %>%
  mutate(Tremor=NP3RIGLL+NP3RIGLU+NP3RIGN+NP3RIGRL+NP3RIGRU) %>%
  filter(!is.na(Tremor)) %>%
  group_by(PATNO) %>% filter(EXAMDT==min(EXAMDT)) %>%
   ungroup() %>%
  summarise(mean=mean(Tremor), sd=sd(Tremor), median=mean(Tremor), q25=quantile(Tremor,0.25), q75=quantile(Tremor, 0.75))



MDS_UPDRS_Part_III_12Feb2025 %>%
  mutate(EXAMDT =as.Date(paste("01/", as.character(EXAMDT )), "%d/%m/%Y")) %>%
  select(PATNO, EXAMDT, NP3FTAPL, NP3FTAPR, NP3HMOVL, NP3HMOVR, NP3PRSPL, NP3PRSPR, NP3TTAPL, NP3TTAPR, NP3LGAGL, NP3LGAGR) %>%
  mutate(Brady=NP3FTAPL+NP3FTAPR+NP3HMOVL+NP3HMOVR+NP3PRSPL+NP3PRSPR+NP3TTAPL+NP3TTAPR+NP3LGAGL+NP3LGAGR) %>%
  filter(!is.na(Brady)) %>%
  group_by(PATNO) %>% filter(EXAMDT==min(EXAMDT)) %>%
   ungroup() %>%
  summarise(mean=mean(Brady), sd=sd(Brady), median=mean(Brady), q25=quantile(Brady,0.25), q75=quantile(Brady, 0.75))



MDS_UPDRS_Part_III_12Feb2025 %>%
  mutate(EXAMDT =as.Date(paste("01/", as.character(EXAMDT )), "%d/%m/%Y")) %>%
  select(PATNO, EXAMDT, NP3PTRML, NP3PTRMR, NP3KTRML, NP3KTRMR, NP3RTALJ, NP3RTALL, NP3RTALU, NP3RTARL, NP3RTARU, NP3RTCON) %>%
  mutate(Tremor=NP3PTRML+NP3PTRMR+NP3KTRML+NP3KTRMR+NP3RTALJ+NP3RTALL+NP3RTALU+NP3RTARL+NP3RTARU+NP3RTCON) %>%
  filter(!is.na(Tremor)) %>%
  group_by(PATNO) %>% filter(EXAMDT==min(EXAMDT)) %>%
   ungroup() %>%
  summarise(mean=mean(Tremor), sd=sd(Tremor), median=mean(Tremor), q25=quantile(Tremor,0.25), q75=quantile(Tremor, 0.75))



# -----
# overall data LEDD ---------------------------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))


# --------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))



# Levodopa ON vs OFF at baseline LD Disease duration -------------------------------



library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))


data <- data %>% select(PATNO, elapsed, LD_baseline, duration_yrs )

data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, duration_yrs = duration_yrs)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$duration_yrs))



data <- data %>% select(ID, time, duration_yrs , FRZGT1W, LD_baseline)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))



library(survival)


# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ LD_baseline  + duration_yrs + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ LD_baseline + duration_yrs, 
#     data = data, cluster = ID)
# 
#   n= 4937, number of events= 589 
# 
#                 coef exp(coef) se(coef) robust se     z
# LD_baseline  0.56849   1.76559  0.35915   0.22487 2.528
# duration_yrs 0.32154   1.37925  0.03569   0.04735 6.790
#              Pr(>|z|)    
# LD_baseline    0.0115 *  
# duration_yrs 1.12e-11 ***
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#              exp(coef) exp(-coef) lower .95 upper .95
# LD_baseline      1.766     0.5664     1.136     2.743
# duration_yrs     1.379     0.7250     1.257     1.513
# 
# Concordance= 0.593  (se = 0.019 )
# Likelihood ratio test= 73.33  on 2 df,   p=<2e-16
# Wald test            = 63.69  on 2 df,   p=1e-14
# Score (logrank) test = 91.68  on 2 df,   p=<2e-16,   Robust = 23.84  p=7e-06
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)








# --------------------------------


# NOT ON LEVODOPA AT BASELINE LD Disease duration -----------------------------------------



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))


library(survival)
library(survminer)


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

data <- data %>% select(PATNO, elapsed, Levodopa_EXP, duration_yrs      )


data <- data %>% drop_na()

data$elapsed <- (data$elapsed - mean(data$elapsed)) / 365

mean(data$perc)

data <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first))

# Rename columns for clarity
data <- data %>%
  rename(ID = PATNO, time = elapsed, duration_yrs      = duration_yrs     )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)


sum(is.na(data$FRZGT1W))
sum(is.na(data$duration_yrs     ))


data <- data %>% select(ID, time, duration_yrs      , FRZGT1W, Levodopa_EXP)

data <- data %>% drop_na()


# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)


unique(data$FRZGT1W)


data <- data %>% mutate(FRZGT1W =ifelse(FRZGT1W>0,1,FRZGT1W ))

library(survival)


# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, FRZGT1W ) ~ Levodopa_EXP  + duration_yrs      + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# Call:
# coxph(formula = Surv(Start, Stop, FRZGT1W) ~ Levodopa_EXP + duration_yrs, 
#     data = data, cluster = ID)
# 
#   n= 4891, number of events= 581 
# 
#                  coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Levodopa_EXP -0.02599   0.97434  0.08543   0.11717 -0.222    0.824    
# duration_yrs  0.31874   1.37539  0.03608   0.04824  6.607 3.93e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#              exp(coef) exp(-coef) lower .95 upper .95
# Levodopa_EXP    0.9743     1.0263    0.7744     1.226
# duration_yrs    1.3754     0.7271    1.2513     1.512
# 
# Concordance= 0.593  (se = 0.019 )
# Likelihood ratio test= 66.26  on 2 df,   p=4e-15
# Wald test            = 44.1  on 2 df,   p=3e-10
# Score (logrank) test = 81.92  on 2 df,   p=<2e-16,   Robust = 22.59  p=1e-05
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# ---------------
# MOCA ------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))

PATNO <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(PATNO) %>% distinct()


PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y"))  %>% 
  group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>%
  mutate(PATNO=as.numeric(PATNO)) %>%
  inner_join(PATNO) %>%
  select(PATNO, moca) %>% drop_na()


mean(PPMI_Curated_Data_Cut_Public_20241211$moca, na.rm=T) ; sd(PPMI_Curated_Data_Cut_Public_20241211$moca, na.rm=T)
median(PPMI_Curated_Data_Cut_Public_20241211$moca, na.rm=T) 
quantile(PPMI_Curated_Data_Cut_Public_20241211$moca, 0.25)
quantile(PPMI_Curated_Data_Cut_Public_20241211$moca, 0.75)

# LEDD baseline (carefull interpret) ---------------------------------------------------

# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FRZGT12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 





PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without FOG at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FRZGT1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FRZGT1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1057

unique(PPMI_Curated_Data_Cut_Public_20241211$FRZGT1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FRZGT1W=ifelse(is.na(FRZGT1W), 0, FRZGT1W))


PPMI_Curated_Data_Cut_Public_20241211


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(YEAR==min(YEAR)) %>%
  select(PATNO, visit_date, YEAR)



LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")


Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  left_join(Levodopa_lookups_complete) 


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% inner_join(PPMI_Curated_Data_Cut_Public_20241211 %>% select(PATNO) %>% distinct())



LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  mutate(STARTDT =as.Date(paste("01/", as.character(STARTDT )), "%d/%m/%Y"))  %>%
  mutate(STOPDT =as.Date(paste("01/", as.character(STOPDT )), "%d/%m/%Y")) 


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa!=1)


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  mutate(ORIG_ENTRY =as.Date(paste("01/", as.character(ORIG_ENTRY )), "%d/%m/%Y")) 

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% 
  select(PATNO, STARTDT, STOPDT, ORIG_ENTRY, LEDD)


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(
    PPMI_Curated_Data_Cut_Public_20241211 %>% ungroup() %>%
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025) %>%
  group_by(PATNO) %>% filter(ORIG_ENTRY ==min(ORIG_ENTRY )) %>%
      group_by(PATNO) %>% filter(visit_date<=(STOPDT )) %>%
        group_by(PATNO) %>% filter(visit_date>=(STARTDT     )) %>%
    group_by(PATNO) %>% summarise(LEDD=sum(as.numeric(LEDD)))
  ) %>%
  mutate(LEDD=ifelse(is.na(LEDD),0,LEDD)) 


mean(PPMI_Curated_Data_Cut_Public_20241211$LEDD, na.rm=T) ; sd(PPMI_Curated_Data_Cut_Public_20241211$LEDD, na.rm=T)
median(PPMI_Curated_Data_Cut_Public_20241211$LEDD, na.rm=T) 
quantile(PPMI_Curated_Data_Cut_Public_20241211$LEDD, 0.25)
quantile(PPMI_Curated_Data_Cut_Public_20241211$LEDD, 0.75)


# --------------------------------

# Falls  NOT ON LEVODOPA AT BASELINE  ----------
# Curated Data

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211$updrs3_score_on

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)


#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441


#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422


# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345


# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

# fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

Levodopa_lookups_complete <- fread("Other/Levodopa_lookups_complete.csv")

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(PATNO, LEDTRT, STARTDT) %>%
  left_join(Levodopa_lookups_complete) %>% select(-c(LEDTRT ))


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(PATNO, STARTDT) %>% summarise(Contains_Levodopa=max(Contains_Levodopa))

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% filter(Contains_Levodopa==1) %>% ungroup() 






# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

names(Determination_of_Freezing_and_Falls_12Feb2025)

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FLNFR1W , FLNFR12M)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FLNFR12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FLNFR1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FLNFR1W) %>% count() 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(disease_duration=duration_yrs+YEAR) %>% select(-c(EVENT_ID, FLNFR12M)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  left_join(LEDD_Concomitant_Medication_Log_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="STARTDT")) 



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>% 
  select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Contains_Levodopa=ifelse(is.na(Contains_Levodopa), 0, Contains_Levodopa)) 


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP), 0, Levodopa_EXP)) 







# Patients with vs without Levodopa at baseline

LD_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(Contains_Levodopa==1) %>% select(PATNO) %>% distinct()

length(LD_at_baseline_pats$PATNO) # 15

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))



# Patinets with vs without Falls at baseline

FOG_at_baseline_pats <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  filter(FLNFR1W >0) %>% select(PATNO) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>% filter(visit_date==min(visit_date)) %>% 
  group_by(FLNFR1W) %>% count()



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(LD_at_baseline_pats %>% mutate(LD_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(FOG_at_baseline_pats %>% mutate(FOG_baseline=1))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(LD_baseline=ifelse(is.na(LD_baseline), 0, LD_baseline))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% 
  mutate(FOG_baseline=ifelse(is.na(FOG_baseline), 0, FOG_baseline))



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% anti_join(FOG_at_baseline_pats)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) #1050

unique(PPMI_Curated_Data_Cut_Public_20241211$FLNFR1W)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% mutate(FLNFR1W=ifelse(is.na(FLNFR1W), 0, FLNFR1W))


PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

names(PPMI_Curated_Data_Cut_Public_20241211)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, Contains_Levodopa) %>% count()


PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR %in% c(0,1)) %>%
  group_by(YEAR, FLNFR1W ) %>% count()


  

data_1 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FLNFR1W=ifelse(is.na(FLNFR1W),0,FLNFR1W)) %>%
  mutate(FLNFR1W=cumsum(FLNFR1W)) %>%
  mutate(FLNFR1W=ifelse(FLNFR1W==0,0,1)) %>%
  filter(FLNFR1W==1) %>%
  filter(elapsed==min(elapsed)) %>%
  ungroup() %>% select(elapsed, FLNFR1W, Levodopa_EXP) %>% ungroup()


data_2 <- PPMI_Curated_Data_Cut_Public_20241211 %>% group_by(PATNO) %>%
  mutate(first=min(visit_date)) %>%
  mutate(elapsed=as.numeric(visit_date-first)) %>%
  arrange(PATNO, visit_date) %>%
  mutate(FLNFR1W=ifelse(is.na(FLNFR1W),0,FLNFR1W)) %>%
  mutate(FLNFR1W=cumsum(FLNFR1W)) %>%
  mutate(FLNFR1W=ifelse(FLNFR1W==0,0,1)) %>%
  anti_join(PPMI_Curated_Data_Cut_Public_20241211 %>% filter(FLNFR1W>0) %>% select(PATNO) %>% distinct()) %>%
  filter(elapsed==max(elapsed)) %>%
  ungroup() %>% select(elapsed, FLNFR1W, Levodopa_EXP ) %>% ungroup()



data <- data_1 %>% bind_rows(data_2)

unique(data$FLNFR1W)

sum(is.na(data))


data <- data %>% mutate(Levodopa_EXP=ifelse(Levodopa_EXP==1, "Levodopa-experienced", "Levodopa-naive"))





library(survival)
library(survminer)


km_fit <- survfit(Surv(elapsed , FLNFR1W  ) ~ Levodopa_EXP   , data = data)

summary(km_fit)

km_fit

data %>% group_by(Levodopa_EXP, FLNFR1W) %>% count()

# Step 3: Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = data, 
           pval = TRUE,          # Add p-value for log-rank test
           conf.int = TRUE,      # Add confidence interval
           risk.table = TRUE,    # Add risk table to the plot
           palette = c("#CD3333", "#83CBEB"), # Example color palette
           ggtheme = theme_minimal(),
           xlab=("\n Number of days From Baseline"),
           ylab=("Proportion FOG-free \n")) # Clean theme

# Step 4: Summary and log-rank test to compare the groups
summary(km_fit)

# Optional: If you want to do a formal comparison
log_rank_test <- survdiff(Surv(elapsed , FLNFR1W ) ~ Levodopa_EXP, data = data)

log_rank_test

survdiff(Surv(elapsed , FLNFR1W ) ~ Levodopa_EXP, data = data, rho = 1)  # Peto-Peto test

# Call:
# survdiff(formula = Surv(elapsed, FLNFR1W) ~ Levodopa_EXP, data = data, 
#     rho = 1)
# 
#                                     N Observed Expected (O-E)^2/E (O-E)^2/V
# Levodopa_EXP=Levodopa-experienced 246     54.8       79       7.4      16.5
# Levodopa_EXP=Levodopa-naive       791    143.5      119       4.9      16.5
# 
#  Chisq= 16.5  on 1 degrees of freedom, p= 5e-05 
#  

coxph(Surv(elapsed, FLNFR1W) ~ Levodopa_EXP, data = data)

# Call:
# coxph(formula = Surv(elapsed, FLNFR1W) ~ Levodopa_EXP, data = data)
# 
#                              coef exp(coef) se(coef)     z        p
# Levodopa_EXPLevodopa-naive 0.5023    1.6525   0.1317 3.813 0.000137
# 
# Likelihood ratio test=15.34  on 1 df, p=8.979e-05
# n= 1037, number of events= 277 



# ---------------
