

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
    ecmp_icb_binary = if_else(rowSums(select(., ECMP10:ECMP21) >= 2, na.rm = TRUE) >= 1,1, 0),
    ecmp_icb_severity = rowSums(select(., ECMP10:ECMP21), na.rm = TRUE)
    )

ecmp <- ecmp %>%
  inner_join(treat_pats_groups, by = "SUBJID")

ecmp <- ecmp %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

ecmp <- ecmp %>% filter(!is.na(ECMPDT))

ecmp_longitudinal <- ecmp %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

length(unique(ecmp_longitudinal$SUBJID)) # 206

names(ecmp_longitudinal)

library(lme4)

model1 <- lmer(
  ecmp_hypodopa ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_longitudinal
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: ecmp_hypodopa ~ TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_longitudinal
# 
# REML criterion at convergence: 2813.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.7589 -0.4520 -0.2052  0.3308  6.6784 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 1.117    1.057   
#  Residual             1.436    1.198   
# Number of obs: 786, groups:  SUBJID, 206
# 
# Fixed effects:
#                         Estimate Std. Error t value
# (Intercept)             1.437070   0.139912  10.271
# TREATMENTPlacebo       -0.044792   0.193440  -0.232
# VISIT                  -0.000513   0.009184  -0.056
# TREATMENTPlacebo:VISIT  0.010984   0.012632   0.870
# 
# Correlation of Fixed Effects:
#             (Intr) TREATMENTPl VISIT 
# TREATMENTPl -0.723                   
# VISIT       -0.464  0.335            
# TREATMENTP:  0.337 -0.467      -0.727

model2 <- lmer(
  ecmp_hypodopa ~ TREATMENT * VISIT + (VISIT | SUBJID),
  data = ecmp_longitudinal
)

anova(model1, model2)


# Data: ecmp_longitudinal
# Models:
# model1: ecmp_hypodopa ~ TREATMENT * VISIT + (1 | SUBJID)
# model2: ecmp_hypodopa ~ TREATMENT * VISIT + (VISIT | SUBJID)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    6 2805.4 2833.4 -1396.7   2793.4                     
# model2    8 2808.6 2845.9 -1396.3   2792.6 0.8245  2     0.6621


library(emmeans)

emm <- emmeans(model1, ~ TREATMENT | VISIT)

emm

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(0))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   0.0448 0.193 325   0.232  0.8170
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(3))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   0.0118 0.179 243   0.066  0.9473
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(9))))

#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo  -0.0541 0.173 210  -0.313  0.7545
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model1, ~ TREATMENT, at = list(VISIT = c(18))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   -0.153 0.219 471  -0.697  0.4858
# 
# Degrees-of-freedom method: kenward-roger 

plot_data <- data.frame(ecmp %>% inner_join(treat_pats_groups) %>%
  mutate(ecmp_hypodopa=ifelse(ecmp_hypodopa>=5, "5+", ecmp_hypodopa)) %>%
  group_by(TREATMENT , VISIT, ecmp_hypodopa) %>% 
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
  ggplot( aes(x = factor(VISIT), y = perc, fill = ecmp_hypodopa)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~TREATMENT, ncol = 2) +
    labs(title = "ECMP-based Hypodopaminergic [5-item] Severity Distribution",
       x = "\n Number of Months From Baseline",
       y = "Cohort Proportion \n",
       fill = "Hypodopaminergic Total Severity") +
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
  ggplot(aes(VISIT , ecmp_hypodopa, colour=Treatment, fill=Treatment)) +
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
  labs(title = "ECMP-based Hypodopaminergic [5-item]",
       x = "\n Number of Months From Baseline",
       y = "Hypodopaminergic Total Score \n",
       fill = "Treatment") 


plot

ggsave(file = "../out/box_icb_sev.svg", plot = plot, width = 5, height = 5)


ecmp_longitudinal <- ecmp_longitudinal %>%
  group_by(SUBJID) %>%
  mutate(change = ecmp_hypodopa - first(ecmp_hypodopa)) %>%
  ungroup()


plot <- ecmp_longitudinal %>% inner_join(treat_pats_groups) %>%
  rename("Treatment"="TREATMENT") %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  ggplot(aes(VISIT , change, colour=Treatment, fill=Treatment)) +
  geom_boxplot(alpha=0.5, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.6, size=0.5, stroke=2, shape=1, width = 0.3, height=0.1) +
  theme_minimal() +
 # ylim(0,14) +
  scale_colour_manual(values=c("#aa3951", "#2f3941")) +
  scale_fill_manual(values=c("#aa3951", "#2f3941")) +
  theme(text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top") +
  labs(title = "ECMP-based Hypodopaminergic [5-item] Delta",
       x = "\n Number of Months From Baseline",
       y = "Hypodopaminergic Delta \n",
       fill = "Treatment") 


plot

ggsave(file = "../out/box_icb_sev.svg", plot = plot, width = 5, height = 5)





library(lme4)

model1 <- lmer(
  change ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_longitudinal
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: change ~ TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_longitudinal
# 
# REML criterion at convergence: 2712.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7038 -0.3861 -0.0065  0.3525  7.0997 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.5474   0.7398  
#  Residual             1.4293   1.1955  
# Number of obs: 786, groups:  SUBJID, 206
# 
# Fixed effects:
#                          Estimate Std. Error t value
# (Intercept)             0.0090234  0.1171088   0.077
# TREATMENTPlacebo       -0.0793105  0.1619659  -0.490
# VISIT                  -0.0009304  0.0091394  -0.102
# TREATMENTPlacebo:VISIT  0.0120521  0.0125742   0.958
# 
# Correlation of Fixed Effects:
#             (Intr) TREATMENTPl VISIT 
# TREATMENTPl -0.723                   
# VISIT       -0.554  0.400            
# TREATMENTP:  0.403 -0.557      -0.727



baseline_vals <- ecmp_longitudinal %>%
  filter(VISIT == 0) %>%
  select(SUBJID, baseline_hypo = ecmp_hypodopa)

ecmp_adj <- ecmp_longitudinal %>%
  left_join(baseline_vals, by = "SUBJID")

summary(lmer(ecmp_hypodopa ~ baseline_hypo + TREATMENT * VISIT + (1|SUBJID), data = ecmp_adj))

# Linear mixed model fit by REML ['lmerMod']
# Formula: ecmp_hypodopa ~ baseline_hypo + TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_adj
# 
# REML criterion at convergence: 2619.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.1862 -0.4319 -0.1678  0.3863  7.4202 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.2461   0.4961  
#  Residual             1.4348   1.1978  
# Number of obs: 780, groups:  SUBJID, 204
# 
# Fixed effects:
#                          Estimate Std. Error t value
# (Intercept)             0.5424740  0.1162903   4.665
# baseline_hypo           0.6269314  0.0374185  16.755
# TREATMENTPlacebo       -0.0780769  0.1432102  -0.545
# VISIT                  -0.0009767  0.0091329  -0.107
# TREATMENTPlacebo:VISIT  0.0108020  0.0126093   0.857
# 
# Correlation of Fixed Effects:
#             (Intr) bsln_h TREATMENTPl VISIT 
# baselin_hyp -0.460                          
# TREATMENTPl -0.637 -0.006                   
# VISIT       -0.560 -0.002  0.455            
# TREATMENTP:  0.404  0.004 -0.633      -0.724


ecmp_longitudinal$VISIT_factor <- factor(ecmp_longitudinal$VISIT)


model1 <- lmer(
  change ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_longitudinal
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: change ~ TREATMENT * VISIT + (1 | SUBJID)
#    Data: ecmp_longitudinal
# 
# REML criterion at convergence: 2712.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7038 -0.3861 -0.0065  0.3525  7.0997 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.5474   0.7398  
#  Residual             1.4293   1.1955  
# Number of obs: 786, groups:  SUBJID, 206
# 
# Fixed effects:
#                          Estimate Std. Error t value
# (Intercept)             0.0090234  0.1171088   0.077
# TREATMENTPlacebo       -0.0793105  0.1619659  -0.490
# VISIT                  -0.0009304  0.0091394  -0.102
# TREATMENTPlacebo:VISIT  0.0120521  0.0125742   0.958
# 
# Correlation of Fixed Effects:
#             (Intr) TREATMENTPl VISIT 
# TREATMENTPl -0.723                   
# VISIT       -0.554  0.400            
# TREATMENTP:  0.403 -0.557      -0.727


sd_baseline <- sd(ecmp_longitudinal$ecmp_hypodopa[ecmp_longitudinal$VISIT==0])

# 1.479765




model1 <- lmer(
  ecmp_hypodopa ~ TREATMENT * VISIT * baseline_hypo + (1 | SUBJID),
  data = ecmp_adj
)

summary(model1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: ecmp_hypodopa ~ TREATMENT * VISIT * baseline_hypo + (1 | SUBJID)
#    Data: ecmp_adj
# 
# REML criterion at convergence: 2592.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.8952 -0.4703 -0.0953  0.3129  7.5954 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.2704   0.520   
#  Residual             1.3315   1.154   
# Number of obs: 780, groups:  SUBJID, 204
# 
# Fixed effects:
#                                       Estimate Std. Error t value
# (Intercept)                           0.359832   0.141463   2.544
# TREATMENTPlacebo                     -0.289756   0.197039  -1.471
# VISIT                                 0.032525   0.012225   2.661
# baseline_hypo                         0.754921   0.068909  10.955
# TREATMENTPlacebo:VISIT                0.023502   0.016928   1.388
# TREATMENTPlacebo:baseline_hypo        0.142070   0.095492   1.488
# VISIT:baseline_hypo                  -0.023374   0.005926  -3.944
# TREATMENTPlacebo:VISIT:baseline_hypo -0.008396   0.008176  -1.027
# 
# Correlation of Fixed Effects:
#                   (Intr) TREATMENTPl VISIT  bsln_h TREATMENTPl:VISIT
# TREATMENTPl       -0.718                                            
# VISIT             -0.618  0.444                                     
# baselin_hyp       -0.696  0.499       0.430                         
# TREATMENTPl:VISIT  0.446 -0.622      -0.722 -0.310                  
# TREATMENTP:_       0.502 -0.699      -0.310 -0.722  0.434           
# VISIT:bsln_        0.432 -0.310      -0.694 -0.621  0.501           
# TREATMENTP:VISIT: -0.313  0.435       0.503  0.450 -0.696           
#                   TREATMENTP:_ VISIT:
# TREATMENTPl                          
# VISIT                                
# baselin_hyp                          
# TREATMENTPl:VISIT                    
# TREATMENTP:_                         
# VISIT:bsln_        0.448             
# TREATMENTP:VISIT: -0.622       -0.725

cor.test(ecmp$ecmp_hypodopa,  ecmp$ecmp_icd_severity, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  ecmp$ecmp_hypodopa and ecmp$ecmp_icd_severity
# S = 67230138, p-value = 1.814e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1692921
	
cor.test(ecmp$ecmp_hypodopa,  ecmp$ecmp_icb_severity, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  ecmp$ecmp_hypodopa and ecmp$ecmp_icb_severity
# S = 61480836, p-value = 8.644e-12
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2403316 
