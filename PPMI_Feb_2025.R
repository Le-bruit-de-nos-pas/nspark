library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

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




# Patinets with vs without Levodopa at baseline

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

coxph(Surv(elapsed, FRZGT1W) ~ LD_baseline, data = data)









# NOT ON LEVODOPA AT BASELINE - - - 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(LD_baseline==0)



PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% left_join(
  PPMI_Curated_Data_Cut_Public_20241211 %>% filter(Contains_Levodopa==1) %>%
    select(PATNO) %>% distinct() %>% mutate(Levodopa_EXP=1)
  ) %>% mutate(Levodopa_EXP=ifelse(is.na(Levodopa_EXP),0,Levodopa_EXP))



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

coxph(Surv(elapsed, FRZGT1W) ~ Levodopa_EXP, data = data)






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


# Fit survival curves based on the Cox model
surv_fit <- survival::survfit(cox_model_td, newdata = data.frame(Groups = c(0,1), hy_stage = mean(data$hy_stage)))

# Plot survival curves
survminer::ggsurvplot(surv_fit, data = data,
           conf.int = TRUE, # Show confidence intervals
           pval = TRUE, # Show p-value
           risk.table = TRUE, # Add risk table below plot
           legend.labs = c("Group 0", "Group 1"),
           xlab = "Time (Days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())


