

libraries <- c("data.table", "tidyverse", "readxl")

lapply(libraries, 
       function(libraries) {
         if (!require(libraries, character.only=TRUE)) {
           install.packages(libraries); require(libraries)
           }}
       )

(.packages())

general <- read_excel(path = "data/BDD_DUODOPAEI.xlsx", sheet = "general")


names(general)

plot <- general %>%
    ggplot(aes(as.factor(1), weight_t0)) +
    geom_boxplot(alpha = 0.7, notch = TRUE, width = 0.5, outlier.shape = NA, colour="#bf4438", fill="#bf4438") +
    geom_jitter(shape = 1, size = 2, stroke = 2.0, width = 0.3, colour="#bf4438", fill="#bf4438") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 10, vjust = -0.5),
      axis.title.y = element_text(size = 10, vjust = -0.5),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    xlab("\n Duodopa") +
    ylab("Weight duodopa start (kg) \n") +
  coord_cartesian(ylim=c(0,120))


ggsave(filename = paste0("output/weight_start", ".svg"), plot = plot, width = 5, height = 5, device = "svg")

names(general)

# Calculate proportions
data_prop <- general %>%
  mutate(dyskinesias_plus_1h=ifelse(dyskinesias_plus_1h==1,"dyskinesias_plus_1h", "no")) %>%
  count(dyskinesias_plus_1h) %>%
  mutate(proportion = n / sum(n),
         label = scales::percent(proportion))

dyskinesias_plus_1h <- ggplot(data_prop, aes(x = "", y = proportion, fill = dyskinesias_plus_1h)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.8) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportional Stacked Bar Chart by dyskinesias_1h status",
       x = NULL,
       y = "Proportion") +
  scale_color_manual(values = c("#bf4438", "#283f60")) +
    scale_fill_manual(values = c("#bf4438", "#283f60")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


ggsave(filename = paste0("output/dyskinesias_plus_1h", ".svg"), plot = dyskinesias_plus_1h, width = 5, height = 5, device = "svg")






therapy <- read_excel(path = "data/BDD_DUODOPAEI.xlsx", sheet = "therapy")


data.frame(therapy %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-therapies,-doses, -LEDD) %>% distinct() %>%
  mutate(LEDD_total=as.numeric(LEDD_total)) %>%
        filter(!is.na(LEDD_total)) %>%
  group_by(visit) %>%
  summarise(mean=round(mean(LEDD_total, na.rm=T),0),
            sd=round(sd(LEDD_total, na.rm=T),0),
            median=round(median(LEDD_total, na.rm=T),0),
            q1=round(quantile(LEDD_total, 0.25, na.rm=T),0),
            q3=round(quantile(LEDD_total, 0.75, na.rm=T),0),
            n=n()) %>%
    mutate(mean=paste(mean, sd, sep="-"))) %>% select(-sd) %>%
  mutate(median=paste0(median, "[", q1, "-", q3, "]")) %>% select(-q1, -q3)


data.frame(therapy %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-therapies,-doses, -LEDD) %>% distinct() %>%
  mutate(updrs_IV =as.numeric(updrs_IV )) %>%
    filter(!is.na(updrs_IV)) %>%
  group_by(visit) %>%
  summarise(mean=round(mean(updrs_IV , na.rm=T),0),
            sd=round(sd(updrs_IV , na.rm=T),0),
            median=round(median(updrs_IV , na.rm=T),0),
            q1=round(quantile(updrs_IV , 0.25, na.rm=T),0),
            q3=round(quantile(updrs_IV , 0.75, na.rm=T),0),
            n=n())) %>%
    mutate(mean=paste(mean, sd, sep="|")) %>% select(-sd) %>%
  mutate(median=paste0(median, "[", q1, "-", q3, "]")) %>% select(-q1, -q3)


test <- therapy %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-therapies,-doses, -LEDD) %>% distinct() %>%
  mutate(updrs_IV =as.numeric(updrs_IV )) %>%
    filter(!is.na(updrs_IV))


kruskal.test(updrs_IV ~ visit, data = test) # Kruskal-Wallis chi-squared = 62.512, df = 22, p-value = 9.425e-06

results_fsa_dunn <- FSA::dunnTest(updrs_IV ~ visit, data = test, method = "bonferroni")
results_fsa_dunn <- results_fsa_dunn$res

fwrite(results_fsa_dunn, "output/results_fsa_dunn_updrs.csv")


kruskal.test(LEDD_total ~ visit, data = test) # Kruskal-Wallis chi-squared = 17.014, df = 22, p-value = 0.7626

results_fsa_dunn <- FSA::dunnTest(LEDD_total ~ visit, data = test, method = "bonferroni")
results_fsa_dunn <- results_fsa_dunn$res

fwrite(results_fsa_dunn, "output/results_fsa_dunn_ledd.csv")

ledd_total_plot <- data.frame(therapy %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-therapies,-doses, -LEDD) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1))) %>%
  ggplot(aes(diff, LEDD_total)) +
  geom_smooth(method="loess", fill="#283f60", colour="#bf4438") +
  coord_cartesian(ylim=c(0,2000)) +
  theme_minimal() + 
  xlab("\n Number Elapsed Months from Duodopa Start") +
  ylab("LEDD Total (mg)")
  
ggsave(filename = paste0("output/ledd_total_plot", ".svg"), plot = ledd_total_plot, width = 5, height = 5, device = "svg")


updrs_iv_plot <- data.frame(therapy %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-therapies,-doses, -LEDD) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1))) %>%
  ggplot(aes(diff, updrs_IV)) +
  geom_smooth(method="loess") +
  geom_smooth(method="loess", fill="#283f60", colour="#bf4438") +
  coord_cartesian(ylim=c(0,15)) +
  theme_minimal() + 
  xlab("\n Number Elapsed Months from Duodopa Start") +
  ylab("UPDRS IV")

ggsave(filename = paste0("output/updrs_iv_plot", ".svg"), plot = updrs_iv_plot, width = 5, height = 5, device = "svg")
  


aes <- read_excel(path = "data/BDD_DUODOPAEI.xlsx", sheet = "aes")

data.frame(aes  %>%  select(patient_id, visit) %>% distinct() %>% 
              group_by(visit) %>% count() %>% rename("tot"="n"))


data.frame(aes %>% group_by(visit, adverse_effect) %>% count()  %>%
  ungroup() %>% 
  left_join(aes  %>% select(patient_id, visit) %>% distinct() %>% 
              group_by(visit) %>% count() %>% rename("tot"="n")) %>%
  mutate(perc=n/tot) %>% 
  select(-n,-tot) %>%
  spread(key=adverse_effect, value=perc))



data.frame(aes %>%  
  mutate(visit=ifelse(visit=="T 20", "T20", visit)) %>%
  select(patient_id,visit , LEDD_duodopa) %>% distinct() %>%
  mutate(LEDD_duodopa=as.numeric(LEDD_duodopa)) %>%
        filter(!is.na(LEDD_duodopa)) %>%
  group_by(visit) %>%
  summarise(mean=round(mean(LEDD_duodopa, na.rm=T),0),
            sd=round(sd(LEDD_duodopa, na.rm=T),0),
            median=round(median(LEDD_duodopa, na.rm=T),0),
            q1=round(quantile(LEDD_duodopa, 0.25, na.rm=T),0),
            q3=round(quantile(LEDD_duodopa, 0.75, na.rm=T),0),
            n=n()) %>%
    mutate(mean=paste(mean, sd, sep="-")) %>% select(-sd) %>%
  mutate(median=paste0(median, "[", q1, "-", q3, "]")) %>% select(-q1, -q3))





test <- aes %>%
  select(patient_id,visit , LEDD_duodopa) %>% distinct() %>%
  mutate(LEDD_duodopa =as.numeric(LEDD_duodopa )) %>%
    filter(!is.na(LEDD_duodopa))


kruskal.test(LEDD_duodopa ~ visit, data = test) # Kruskal-Wallis rank sum test
#data:  LEDD_duodopa by visit Kruskal-Wallis chi-squared = 20.801, df = 22, p-value = 0.533



ledd_total_plot <- data.frame(aes %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(patient_id,visit , visit_date, LEDD_duodopa, date_duodopa_start) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1))) %>%
  ggplot(aes(diff, LEDD_duodopa)) +
  geom_smooth(method="loess", fill="#283f60", colour="#bf4438") +
  coord_cartesian(ylim=c(0,2000)) +
  theme_minimal() + 
  xlab("\n Number Elapsed Months from Duodopa Start") +
  ylab("LEDD Duodopa (mg)")
  
ggsave(filename = paste0("output/ledd_duodopa_plot", ".svg"), plot = ledd_total_plot, width = 5, height = 5, device = "svg")



df <- data.frame(aes %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-adverse_treatment ,-weight, -LEDD_total , -updrs_IV, -LEDD_duodopa) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1)) %>%
     mutate(visit=ifelse(visit=="T 20","T20", visit)) %>%
  select(-visit_date, -date_duodopa_start, -visit )) %>% 
  drop_na() %>% group_by(patient_id, adverse_effect) %>% summarise(diff=min(diff)) %>% ungroup()
 

# Step 1: Get all unique patients and all unique adverse effects
all_patients <- unique(df$patient_id)
all_effects <- unique(df$adverse_effect)

# Step 2: Create all combinations
full_grid <- expand.grid(patient_id = all_patients, adverse_effect = all_effects)

# Step 3: Left join to fill missing combinations with NA in diff
df_complete <- full_grid %>%
  left_join(df, by = c("patient_id", "adverse_effect")) %>%
  arrange(patient_id, adverse_effect)

# View result
print(df_complete)

df_complete <- df_complete %>% mutate(event_occured=ifelse(is.na(diff), 0,1))

df_complete <- df_complete %>% left_join(general %>% select(patient_id, duration_followup_months )) %>%
  mutate(diff=ifelse(is.na(diff), duration_followup_months, diff)) %>% 
  select(-duration_followup_months)




df_complete <- df_complete %>% mutate(diff=ifelse(diff<=0,0,diff))

unique(df_complete$adverse_effect) #  [1]  1  2  3  4  5  6  7  8 10 11 12 13 14 15


df_ae <- df_complete %>% filter(adverse_effect==8)

library(survival)
library(survminer)


surv_obj <- Surv(time = df_ae$diff, event = df_ae$event_occured)
fit <- survfit(surv_obj ~ 1)
  
ggsurv <- ggsurvplot(
    fit,
    data = df_ae,
    risk.table = TRUE,
    conf.int = TRUE,
    cumevents = TRUE,
    cumcensor = TRUE,
    palette=c("midnightblue" ),
    title = paste("Device Complications"),
    xlab = "Months post Duodopa start",
    ylab = "Survival probability",
    surv.median.line = "hv",
    tables.height = 0.15,
  )

print(ggsurv)





library(patchwork)



df <- data.frame(aes %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-adverse_treatment ,-weight, -LEDD_total , -updrs_IV, -LEDD_duodopa) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1)) %>%
     mutate(visit=ifelse(visit=="T 20","T20", visit)) %>%
  select(-visit_date, -date_duodopa_start, -visit )) %>% 
  drop_na() %>% filter(diff>0) %>% group_by(patient_id, adverse_effect) %>% summarise(diff=min(diff)) %>% ungroup()
 

# Step 1: Get all unique patients and all unique adverse effects
all_patients <- unique(df$patient_id)
all_effects <- unique(df$adverse_effect)

# Step 2: Create all combinations
full_grid <- expand.grid(patient_id = all_patients, adverse_effect = all_effects)

# Step 3: Left join to fill missing combinations with NA in diff
df_complete <- full_grid %>%
  left_join(df, by = c("patient_id", "adverse_effect")) %>%
  arrange(patient_id, adverse_effect)

# View result
print(df_complete)

df_complete <- df_complete %>% mutate(event_occured=ifelse(is.na(diff), 0,1))

df_complete <- df_complete %>% left_join(general %>% select(patient_id, duration_followup_months )) %>%
  mutate(diff=ifelse(is.na(diff), duration_followup_months, diff)) %>% 
  select(-duration_followup_months)





# 1	Peripheral neuropathy
# 2	Hallucination
# 3	Confusion
# 4	Worsening of dyskinesia
# 5	Excessive weight loss cut-off (> % 10% of the weight) and another BMI < 22
# 6	Abdominal pain
# 7	Disease progression (e.g. advanced stage with general deterioration, cut Off from HY 2 to 3 , from 3 to 4, or increment of axial and motor complications (to be discussed)
# 8	Device complications (dislocations, occlusions or ruptures of the PEG-J tube, stoma site infections)
# 9	Lack of perceived clinical benefit, not satisfaction of patients and/or caregivers
# 10	Orthostatic hypotension 
# 11	Troubles du contrôle des impulsions
# 12	Somnolonce
# 13	Oedèms
# 14	Nodule sous-cutané
# 15	Psychose
# 16	Stimulation cérébrale profonde
# 17	Décès

# Filter data for a specific adverse effect (e.g., 1)
ae_plot <- df_complete %>% 
  filter(adverse_effect == 16, event_occured == 1)  # Only events, not censored

# Boxplot
p_box <- ggplot(ae_plot, aes(x = diff, y="")) +
  geom_boxplot(fill = "deepskyblue4", colour="deepskyblue4", notch=TRUE, alpha = 0.5) +
  geom_jitter(shape = 1, size = 2, stroke = 2.0, width = 0.3, colour="deepskyblue4", fill="deepskyblue4") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = "Time to Psychosis (months)", y = "", title = "Boxplot of Time to Psychosis")

# Density plot
p_dens <- ggplot(ae_plot, aes(x = diff)) +
  geom_density(fill = "deepskyblue4", colour="deepskyblue4", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Time to Psychosis (months)", y = "Patient density", title = "Density of Psychosis")

# Combine horizontally
plot <- p_box / p_dens + plot_layout(heights = c(1, 2))
plot


ggsave(filename = paste0("output/Psychosis_plot", ".svg"), plot = plot, width = 5, height = 5, device = "svg")



df_events <- df_complete %>%
  filter(event_occured == 1)

# Step 2: For each adverse effect, get the earliest time each patient had the event
first_events <- df_events %>%
  group_by(patient_id, adverse_effect) %>%
  summarise(first_time = min(diff), .groups = "drop")

# Step 3: Count cumulative number of patients per adverse effect over time
cumulative_df <- first_events %>%
  group_by(adverse_effect, first_time) %>%
  summarise(new_cases = n(), .groups = "drop") %>%
  arrange(adverse_effect, first_time) %>%
  group_by(adverse_effect) %>%
  mutate(cumulative_cases = cumsum(new_cases))



cumulative_df_complete <- cumulative_df %>%
  group_by(adverse_effect) %>%
  complete(first_time = full_seq(first_time, 1), fill = list(new_cases = 0)) %>%
  arrange(adverse_effect, first_time) %>%
  group_by(adverse_effect) %>%
  mutate(cumulative_cases = cumsum(new_cases)) %>%
  ungroup()


adverse_labels <- c(
  "1" = "Peripheral neuropathy",
  "2" = "Hallucination",
  "3" = "Confusion",
  "4" = "Worsening of dyskinesia",
  "5" = "Excessive weight loss / BMI < 22",
  "6" = "Abdominal pain",
  "7" = "Disease progression",
  "8" = "Device complications",
  "9" = "Lack of perceived benefit",
  "10" = "Orthostatic hypotension",
  "11" = "Impulse control disorders",
  "12" = "Somnolence",
  "13" = "Oedemas",
  "14" = "Subcutaneous nodule",
  "15" = "Psychosis",
  "16" = "Deep brain stimulation",
  "17" = "Death"
)


cumulative_df_complete <- cumulative_df_complete %>%
  mutate(
    adverse_effect_label = factor(
      adverse_labels[as.character(adverse_effect)],
      levels = adverse_labels
    )
  )


cool_colors <- c(
  "#1f77b4",  # blue
  "#ff7f0e",  # orange
  "#2ca02c",  # green
  "#d62728",  # red
  "#9467bd",  # purple
  "#8c564b",  # brown
  "#e377c2",  # pink
  "#7f7f7f",  # gray
  "#bcbd22",  # olive
  "#17becf",  # cyan
  "#aec7e8",  # light blue
  "#ffbb78",  # light orange
  "#98df8a",  # light green
  "#c5b0d5"   # light purple
)


plot <- ggplot(cumulative_df_complete, aes(x = first_time, y = cumulative_cases/122, color = factor(adverse_effect_label))) +
  geom_line(size = 2, alpha=0.7) +
    scale_color_manual(values = cool_colors) +
  labs(
    title = "De novo Cumulative Proportion of Patients with Adverse Effects Over Time",
    x = "Time Since Duodopa Start (months)",
    y = "De novo Cumulative Proportion of Patients",
    color = "Adverse Effect"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(filename = paste0("output/cumulative_plot", ".svg"), plot = plot, width = 8, height = 5, device = "svg")


names(general)

general_short <- general %>% select(gender, age_disease_onset, apo_pompe_t0, dbs_t0, disease_duration_duodopa_start, plus_5_medication_intakes, efficacy_fluctuations_plus_2h, dyskinesias_plus_1h, updrs_IV_t0, LEDD_total_t0, l_dopa_challenge_test_t0, mms_t0, moca_t0, weight_t0, dropout_cause)

general_short <- general_short %>% mutate(dropout_cause=ifelse(is.na(dropout_cause),0,
                                                               ifelse(dropout_cause==17,0,1))) %>% filter(!is.na(dropout_cause))

predictors <- general_short %>%
  select(-dropout_cause ) 

predictors_df <- as.data.frame(predictors)

df_matrix <- as.matrix(predictors_df)

target <- general_short$dropout_cause

dtrain <- xgboost::xgb.DMatrix(data = df_matrix, label = target, missing = NA)


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  nthread = 4
)


set.seed(123)

model <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 1
)




set.seed(123)

cv <- xgboost::xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 10,
  showsd = TRUE,
  stratified = TRUE,
  verbose = 1,
  early_stopping_rounds = 25,
  maximize = TRUE,
  metrics = "auc"
)

print(cv)



best_nrounds <- cv$best_iteration

final_model <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 1
)



library(caret)
library(xgboost)

# Prepare data
df_matrix <- as.matrix(predictors)
target <- general_short$dropout_cause

# Define cross-validation method
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
   savePredictions = "all"  
)

# Make target a factor with names for caret
target_factor <- factor(ifelse(target == 1, "yes", "no"))

# Define grid for tuning
tune_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.8, 1)
)

# Train the model
set.seed(123)
xgb_tuned <- train(
  x = df_matrix,
  y = target_factor,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "ROC"
)

# Check best parameters and performance
print(xgb_tuned$bestTune)
print(xgb_tuned)










library(pROC)
library(dplyr)

# Extract predictions from resamples
resample_preds <- xgb_tuned$pred

# Filter predictions to best tuning parameters
best_params <- xgb_tuned$bestTune

filtered_preds <- resample_preds %>%
  filter(nrounds == best_params$nrounds,
         max_depth == best_params$max_depth,
         eta == best_params$eta,
         gamma == best_params$gamma,
         colsample_bytree == best_params$colsample_bytree,
         min_child_weight == best_params$min_child_weight,
         subsample == best_params$subsample)

# Calculate ROC
roc_obj <- roc(response = filtered_preds$obs,
               predictor = filtered_preds$yes)  # 'yes' is the predicted prob for class "yes"

# Plot ROC
plot <- plot(roc_obj, main = "ROC Curve (CV Predictions)", col = "deepskyblue4")

cat("AUC:", auc(roc_obj), "\n") # 0.746


# Get the final xgboost model from caret
final_model <- xgb_tuned$finalModel

# Compute feature importance
importance_matrix <- xgboost::xgb.importance(model = final_model)

# View top features
print(importance_matrix)



# Plot feature importance
xgboost::xgb.plot.importance(importance_matrix, top_n = 20)


general_short %>% group_by(dropout_cause) %>%
  summarise(disease_duration_duodopa_start=mean(moca_t0, na.rm=T))


library(DALEX)

# Custom predict function for probabilities of class "yes"
predict_function <- function(model, newdata) {
  predict(model, newdata)
}

# Create explainer again using the correct y (as numeric)
explainer <- explain(
  model = xgb_tuned$finalModel,
  data = df_matrix,
  y = as.numeric(target_factor == "yes"),  # binary 0/1 target
  predict_function = predict_function,
  label = "xgb"
)


vi <- model_parts(explainer, type = "raw")
plot(vi)








# 1	Peripheral neuropathy
# 2	Hallucination
# 3	Confusion
# 4	Worsening of dyskinesia
# 5	Excessive weight loss cut-off (> % 10% of the weight) and another BMI < 22
# 6	Abdominal pain
# 7	Disease progression (e.g. advanced stage with general deterioration, cut Off from HY 2 to 3 , from 3 to 4, or increment of axial and motor complications (to be discussed)
# 8	Device complications (dislocations, occlusions or ruptures of the PEG-J tube, stoma site infections)
# 9	Lack of perceived clinical benefit, not satisfaction of patients and/or caregivers
# 10	Orthostatic hypotension 
# 11	Troubles du contrôle des impulsions
# 12	Somnolonce
# 13	Oedèms
# 14	Nodule sous-cutané
# 15	Psychose
# 16	Stimulation cérébrale profonde
# 17	Décès


df <- data.frame(aes %>% inner_join(general %>% select(patient_id, date_duodopa_start)) %>%
  select(-visit,-adverse_treatment) %>% distinct() %>%
    mutate(diff=lubridate::interval(date_duodopa_start, visit_date)%/% months(1))) %>%
  select(-visit_date, -date_duodopa_start)


df <- df %>% select(patient_id, diff, weight) %>% distinct() %>%
  group_by(patient_id, diff) %>% summarise(weight=min(weight)) %>% ungroup() %>%
  left_join(df %>% filter(adverse_effect==1) %>% 
              select(patient_id, diff) %>% distinct() %>% mutate(event=1))

df <- df %>% mutate(event=ifelse(is.na(event),0,event))

# Optional: Remove rows with missing key predictors
df_model <- na.omit(df[, c("event", "diff", "weight", "patient_id")])
df_model <- df_model %>% distinct()

library(lme4)

# Fit the model
model_mixed <- glmer(event ~ diff + weight + (1 | patient_id),
                     data = df_model,
                     family = binomial)

# View results
summary(model_mixed)












general$height <- general$height/100

general$bmi <- general$weight_t0/(general$height**2)


library(dplyr)
library(lme4)
library(broom.mixed)

# Step 1: Preprocess base data
df_all <- data.frame(
  aes %>%
    inner_join(general %>% select(patient_id, date_duodopa_start, height)) %>%
    select(-visit, -adverse_treatment) %>%
    distinct() %>%
    mutate(diff = lubridate::interval(date_duodopa_start, visit_date) %/% months(1))
) %>%
  select(-visit_date, -date_duodopa_start)


# Step 2: Prepare patient-level LEDD_total over time
base <- df_all %>% mutate(bmi=weight/(height**2)) %>%
  select(patient_id, diff, bmi) %>%
  distinct() %>%
  group_by(patient_id, diff) %>%
  summarise(bmi = min(bmi), .groups = "drop")

# Step 3: Loop over all unique adverse_effect values
adverse_effects <- sort(unique(df_all$adverse_effect))

results <- lapply(adverse_effects, function(ae) {
  
  # Subset for current AE event
  ae_df <- df_all %>%
    filter(adverse_effect == ae) %>%
    select(patient_id, diff) %>%
    distinct() %>%
    mutate(event = 1)
  
  # Merge and set event=0 for non-events
  df <- base %>%
    left_join(ae_df, by = c("patient_id", "diff")) %>%
    mutate(event = ifelse(is.na(event), 0, event))
  
  # Remove missing values and duplicates
  df_model <- na.omit(df[, c("event", "diff", "bmi", "patient_id")]) %>%
    distinct()
  
  # Fit model
  model <- tryCatch({
    glmer(event ~ diff + bmi + (1 | patient_id), data = df_model, family = binomial)
  }, error = function(e) return(NULL))
  
  # Extract LEDD_total coefficient if model fits
  if (!is.null(model)) {
    coefs <- tidy(model)
    coefs %>%
      filter(term == "bmi") %>%
      mutate(adverse_effect = ae) %>%
      select(adverse_effect, estimate, std.error, statistic, p.value)
  } else {
    data.frame(adverse_effect = ae, estimate = NA, std.error = NA, statistic = NA, p.value = NA)
  }
})

# Combine all results
results_df <- do.call(rbind, results)

# View results
print(results_df)




forest_df <- results_df %>%
  filter(!is.na(estimate)) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    or = exp(estimate),
    or_lower = exp(lower),
    or_upper = exp(upper),
    sig = p.value < 0.05
  )



adverse_labels <- c(
  "1" = "Peripheral neuropathy",
  "2" = "Hallucination",
  "3" = "Confusion",
  "4" = "Worsening of dyskinesia",
  "5" = "Excessive LEDD_total loss / BMI < 22",
  "6" = "Abdominal pain",
  "7" = "Disease progression",
  "8" = "Device complications",
  "9" = "Lack of perceived benefit",
  "10" = "Orthostatic hypotension",
  "11" = "Impulse control disorders",
  "12" = "Somnolence",
  "13" = "Oedemas",
  "14" = "Subcutaneous nodule",
  "15" = "Psychosis",
  "16" = "Deep brain stimulation",
  "17" = "Death"
)


forest_df <- forest_df %>%
  mutate(
    adverse_effect_label = factor(
      adverse_labels[as.character(adverse_effect)],
      levels = adverse_labels
    )
  )


ggplot(forest_df, aes(x = or, y = factor(adverse_effect_label), color = sig)) +
  geom_point(size=5, shape = 1, stroke = 3.0) +
  #geom_errorbarh(aes(xmin = or_lower, xmax = or_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("deepskyblue4", "firebrick")) +
  labs(
    title = "Odds Ratio of BMI Effect\nON Adverse Effects (Adjusted for Time)",
    x = "\n Odds Ratio (BMI)",
    y = "Adverse Effect \n",
    color = "Significant (p < 0.05)"
  ) +
  theme_minimal()




# Compare those who stop vs not stop 

library(dplyr)
library(purrr)



general_short <- general %>% select(gender, age_disease_onset, apo_pompe_t0, dbs_t0, disease_duration_duodopa_start, plus_5_medication_intakes, efficacy_fluctuations_plus_2h, dyskinesias_plus_1h, updrs_IV_t0, LEDD_total_t0, l_dopa_challenge_test_t0, mms_t0, moca_t0, weight_t0, dropout_cause)

general_short <- general_short %>% mutate(dropout_cause=ifelse(is.na(dropout_cause),0,
                                                               ifelse(dropout_cause==17,0,1))) %>% filter(!is.na(dropout_cause))


# Identify columns to test
vars_to_test <- setdiff(names(general_short), "dropout_cause")

# Function to summarize one variable by dropout_cause
summarize_variable <- function(var_name, data) {
  df <- data %>%
    select(dropout_cause, !!sym(var_name)) %>%
    filter(!is.na(dropout_cause), !is.na(!!sym(var_name)))
  
  if (nrow(df) == 0) {
    return(NULL)
  }

  var <- df[[var_name]]
  group <- as.factor(df$dropout_cause)
  
  # Determine type of variable and run appropriate test
  if (length(unique(var)) == 2) {
    p <- tryCatch(fisher.test(table(var, group))$p.value, error = function(e) NA)
    test <- "Fisher's Exact"
  } else {
    p <- tryCatch(wilcox.test(var ~ group)$p.value, error = function(e) NA)
    test <- "Mann-Whitney U"
  }

  # Descriptive stats per group
  summary_stats <- df %>%
    group_by(dropout_cause) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(var_name)),
      sd = sd(!!sym(var_name)),
      median = median(!!sym(var_name)),
      q1 = quantile(!!sym(var_name), 0.25),
      q3 = quantile(!!sym(var_name), 0.75),
      .groups = "drop"
    ) %>%
    mutate(variable = var_name, test_used = test, p_value = round(p, 5)) %>%
    relocate(variable, .before = dropout_cause)

  return(summary_stats)
}

# Apply to all variables and combine
full_summary <- map_dfr(vars_to_test, ~summarize_variable(.x, general_short))

# View result
print(full_summary)

data.frame(full_summary %>% mutate(mean=paste0(round(mean,2),  paste0(" | ", round(sd,2)))) %>%
  select(-sd) %>%
  mutate(median=paste0(median, paste0(" [", paste0(q1, paste0("-", paste0(q3, "]")))))) %>%
  select(-q1, -q3))



summary(general)

# Age: 37 missing
# UPDRS IV: 3 missing
# Ledd Total: 2 missing
# LCT: 26 missing
# MMS: 82 missing
# MOCA: 75 missing
# Weight: 21 missing
# Duration: 1 missing




# Predicte time on Duodopa conntinuous 

general_short <- general %>% select(gender, age_disease_onset, apo_pompe_t0, dbs_t0, 
                                    disease_duration_duodopa_start, plus_5_medication_intakes,
                                    efficacy_fluctuations_plus_2h, dyskinesias_plus_1h, 
                                    updrs_IV_t0, LEDD_total_t0, l_dopa_challenge_test_t0,
                                    mms_t0, moca_t0, weight_t0, dropout_cause, duration_followup_months) %>%
  mutate(exp=1) %>% mutate(dropout_cause=paste0("S", dropout_cause)) %>% spread(key=dropout_cause, value=exp)

names(general_short)

general_short <- general_short %>% select(-`SNA`) %>%
  mutate(`S1`=ifelse(is.na(`S1`),0,`S1`),
         `S2`=ifelse(is.na(`S2`),0,`S2`),
         `S7`=ifelse(is.na(`S7`),0,`S7`),
         `S8`=ifelse(is.na(`S8`),0,`S8`),
         `S9`=ifelse(is.na(`S9`),0,`S9`),
         `S16`=ifelse(is.na(`S16`),0,`S16`),
         `S17`=ifelse(is.na(`S17`),0,`S17`))



library(xgboost)
library(dplyr)
library(Matrix)

# Remove rows with missing target variable
df <- general_short %>% filter(!is.na(duration_followup_months))


predictors <- df %>%
  select(-duration_followup_months ) 

predictors_df <- as.data.frame(predictors)

df_matrix <- as.matrix(predictors_df)

target <- df$duration_followup_months


dtrain <- xgboost::xgb.DMatrix(data = df_matrix, label = target, missing = NA)


params <- list(
  objective = "reg:squarederror",  # Regression objective
  eval_metric = "rmse",            # Root Mean Squared Error
  max_depth = 6,
  eta = 0.1,
  nthread = 4
)

set.seed(123)

model <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 1
)



set.seed(123)

cv <- xgboost::xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 10,
  showsd = TRUE,
  verbose = 1,
  early_stopping_rounds = 25,
  maximize = FALSE,  # minimize RMSE
  metrics = "rmse"   # regression metric
)

print(cv)

best_nrounds <- cv$best_iteration

final_model <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 1
)



library(caret)
library(xgboost)

# Prepare data
df_matrix <- as.matrix(predictors)
target <- df$duration_followup_months




# Define cross-validation method (regression-appropriate)
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "all"
)


# Define grid for tuning
tune_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.8, 1)
)



# Train the model (regression)
set.seed(123)
xgb_tuned <- train(
  x = df_matrix,
  y = target,  # now numeric
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "RMSE"  # Or "MAE"
)


# Check best parameters and performance
print(xgb_tuned$bestTune)
print(xgb_tuned)






library(ggplot2)

# Filter predictions matching best tuning parameters
resample_preds <- xgb_tuned$pred
best_params <- xgb_tuned$bestTune

filtered_preds <- resample_preds %>%
  filter(nrounds == best_params$nrounds,
         max_depth == best_params$max_depth,
         eta == best_params$eta,
         gamma == best_params$gamma,
         colsample_bytree == best_params$colsample_bytree,
         min_child_weight == best_params$min_child_weight,
         subsample == best_params$subsample)

# Plot observed vs. predicted
ggplot(filtered_preds, aes(x = pred, y = obs)) +
  geom_point(alpha = 0.6, color = "deepskyblue4") +
  geom_abline(intercept = 0, slope = 1, color = "gray40", linetype = "dashed") +
  labs(title = "Observed vs Predicted (CV)",
       x = "Predicted duration_followup_months",
       y = "Observed duration_followup_months") +
  theme_minimal() +
  coord_cartesian(xlim=c(0,125), ylim=c(0,125))


cor(filtered_preds$pred, filtered_preds$obs)

final_model <- xgb_tuned$finalModel
importance_matrix <- xgb.importance(model = final_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix, top_n = 20)




library(DALEX)

# Predict function for numeric predictions (regression)
predict_function <- function(model, newdata) {
  predict(model, newdata)
}

explainer <- explain(
  model = xgb_tuned$finalModel,
  data = df_matrix,
  y = target,
  predict_function = predict_function,
  label = "xgb_regression"
)

# Raw importance (no permutation aggregation)
vi <- model_parts(explainer, type = "raw")
plot(vi)


general %>% select(weight_t0, duration_followup_months) %>% drop_na() %>%
  summarise(cor=cor(weight_t0, duration_followup_months, method="pearson"))










# Dropout rates -----------

general <- read_excel(path = "BDD_DUODOPAEI.xlsx", sheet = "general")

general <- general %>% select(patient_id, dropout_cause, duration_followup_months,	in_progress, date_duodopa_start, date_of_pump_dropout)


general$date_duodopa_start   <- as.Date(general$date_duodopa_start  )
general$date_of_pump_dropout <- as.Date(general$date_of_pump_dropout)

general <- general %>%
  mutate(diff=lubridate::interval(date_duodopa_start, date_of_pump_dropout)%/% months(1)) 
  

general <- general %>% mutate(dropout_cause=ifelse(is.na(dropout_cause) | dropout_cause==17, 0, 1)) %>%
                                filter(!is.na(duration_followup_months ))

       
general <- general %>%
  select(patient_id, dropout_cause, duration_followup_months)


general$year <- floor(general$duration_followup_months / 12)

table_by_year <- general %>%
  group_by(year) %>%
  summarise(
    at_risk = n(),
    dropouts = sum(dropout_cause),
    dropout_rate = dropouts / at_risk
  )

# year at_risk dropouts dropout_rate
# <dbl>   <int>    <dbl>        <dbl>
#   1     0      38       13        0.342
# 2     1      20        6        0.3  
# 3     2      20        4        0.2  
# 4     3      10        0        0    
# 5     4       8        3        0.375
# 6     5       6        1        0.167
# 7     6       5        2        0.4  
# 8     7       6        1        0.167
# 9     8       5        0        0    
# 10     9       2        0        0    
# 11    10       1        0        0 

weighted.mean(table_by_year$dropout_rate, table_by_year$at_risk)

# [1] 0.2479339

library(survival)

general <- general %>%
  mutate(duration_followup_months = ifelse(duration_followup_months <= 0,
                                           0.01, duration_followup_months))


fit <- survfit(surv_object ~ 1, data = general)

# Survival probabilities at each year (12, 24, 36 months…)
summary(fit, times = c(12, 24, 36, 48, 60))$surv

# [1] 0.8864034 0.8011822 0.7569649 0.7569649 0.6837103


library(survival)
library(dplyr)

surv_object <- Surv(general$duration_followup_months, general$dropout_cause)
fit <- survfit(surv_object ~ 1, data = general)

# KM survival at whole years (months = 12, 24, 36…)
time_points <- seq(0, 60, by = 12)
km_summary <- summary(fit, times = time_points)

# Tidy dataframe
km_df <- data.frame(
  time_months = km_summary$time,
  survival_prob = km_summary$surv
)

# Calculate per-year conditional dropout
annual_dropout <- km_df %>%
  mutate(
    year = time_months / 12,
    dropout_prob = 1 - survival_prob,
    # Conditional dropout in each interval
    interval_dropout = c(NA, -diff(survival_prob)),
    interval_dropout_rate = interval_dropout / lag(survival_prob)  # relative to survivors at start
  )

annual_dropout

# time_months survival_prob year dropout_prob interval_dropout interval_dropout_rate
# 1           0     1.0000000    0    0.0000000               NA                    NA
# 2          12     0.8864034    1    0.1135966       0.11359663            0.11359663
# 3          24     0.8011822    2    0.1988178       0.08522114            0.09614262
# 4          36     0.7569649    3    0.2430351       0.04421729            0.05519006
# 5          48     0.7569649    4    0.2430351       0.00000000            0.00000000
# 6          60     0.6837103    5    0.3162897       0.07325467            0.09677419


“The survival curve shows the cumulative probability of persistence, which steadily decreases over time.
The annual conditional dropout percentages represent the probability of dropout during a given year, among those still persistent at the start of that year.
These annual rates are not cumulative, so they can fluctuate year to year, but together they explain the shape of the survival curve.”


# Function to calculate number at risk and dropouts in each interval
get_yearly_dropout <- function(df, max_year = 10) {
  tibble(year = 1:max_year) %>%
    rowwise() %>%
    mutate(
      at_risk = sum(df$duration_followup_months >= (year-1)*12),
      dropouts = sum(df$dropout_cause == 1 &
                       df$duration_followup_months >= (year-1)*12 &
                       df$duration_followup_months < year*12),
      dropout_rate = dropouts / at_risk
    )
}

yearly_table <- get_yearly_dropout(general, max_year = 10)
yearly_table


# year at_risk dropouts dropout_rate
# <int>   <int>    <int>        <dbl>
#   1     1     121       13       0.107 
# 2     2      83        6       0.0723
# 3     3      63        4       0.0635
# 4     4      43        0       0     
# 5     5      33        3       0.0909
# 6     6      25        1       0.04  
# 7     7      19        2       0.105 
# 8     8      14        1       0.0714
# 9     9       8        0       0     
# 10    10       3        0       0  


# -----
