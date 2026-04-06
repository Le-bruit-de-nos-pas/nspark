
library(haven)
library(tidyverse)
library(data.table)
library(lubridate)

options(scipen=999)


admin <- read_sas("../data/admin.sas7bdat")

admin %>% group_by(VISIT) %>% count()

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

lars <- read_sas("../data/lars.sas7bdat")

lars <- lars %>% select(SUBJID, VISIT, LARSCORE)

mean(lars$LARSCORE, na.rm=T)

unique(lars$VISIT)

lars <- lars %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

lars <- lars %>% drop_na() %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

lars2 <- lars %>%
  inner_join(treat_pats_groups, by = "SUBJID") %>%
  mutate(LARS_apathetic = ifelse(LARSCORE >= -21, 1, 0),
         TREATMENT = ifelse(GRP == "A", "Amantadine", "Placebo")) %>%
  select(SUBJID, VISIT, TREATMENT, LARSCORE, LARS_apathetic)


dysk_pats <- fread("../out/dysk_pats.txt")





lars2 %>%
  mutate(SUBJID=as.numeric(SUBJID)) %>%
  inner_join(dysk_pats %>% filter(VISIT==18) %>% select(SUBJID , MDS68)) %>%
  group_by(TREATMENT, VISIT, MDS68) %>% filter(VISIT==18) %>%
  summarise(mean=mean(LARS_apathetic))




lars_summary <- lars2 %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
    n = n(),
    n_apathetic = sum(LARS_apathetic, na.rm = TRUE),
    perc_apathetic = mean(LARS_apathetic, na.rm = TRUE) * 100,
    mean_score = mean(LARSCORE, na.rm = TRUE),
    sd_score = sd(LARSCORE, na.rm = TRUE),
    sem_score = sd_score / sqrt(n())
  ) %>%
  ungroup()

lars_summary

#  A tibble: 8 × 8
#   VISIT TREATMENT      n n_apathetic perc_apathetic mean_score sd_score sem_score
#   <dbl> <chr>      <int>       <dbl>          <dbl>      <dbl>    <dbl>     <dbl>
# 1     0 Amantadine   130          14          10.8       -27.9     6.71     0.588
# 2     0 Placebo      137          13           9.49      -28.5     5.49     0.469
# 3     3 Amantadine   122          20          16.4       -27.6     8.44     0.764
# 4     3 Placebo      135          13           9.63      -28.6     5.84     0.503
# 5     9 Amantadine   113          18          15.9       -27.2     9.53     0.897
# 6     9 Placebo      128          14          10.9       -28.8     5.66     0.500
# 7    18 Amantadine   111          17          15.3       -28.1     8.18     0.776
# 8    18 Placebo      125          19          15.2       -28.6     6.48     0.580

# Compute prevalence per visit and treatment
lars_prev <- lars2 %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
    prevalence = mean(LARS_apathetic, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    se = sqrt(prevalence * (1 - prevalence) / n),
    lower = pmax(prevalence - 1.96 * se, 0),
    upper = pmin(prevalence + 1.96 * se, 1),
    item = "LARS Apathy"
  )


# Plot points + lines
plot <- ggplot(lars_prev,
       aes(x = VISIT,
           y = prevalence,
           color = TREATMENT,
           group = TREATMENT)) +
  geom_line(size = 2, alpha=0.6) +
  geom_point(shape=1, size=2, stroke=2) +
  geom_linerange(
  aes(ymin = lower, ymax = upper),
  alpha = 0.2,
  linewidth = 2.5,
  lineend = "round"
) +
  scale_x_continuous(
    breaks = c(0, 3, 9, 18)
  ) +
  facet_wrap(~ item, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\nVisit (months)",
    y = "Prevalence of Apathy (LARS ≥ -21) \n",
    title = "",
    color = "Treatment Group"
  ) +
  #ylim(0,0.20) +
  scale_colour_manual(values=c("#aa3951", "#2f3941")) +
  scale_fill_manual(values=c("#aa3951", "#2f3941")) +
  theme_minimal() +
   theme(text = element_text(face = "bold"),
         axis.text = element_text(size = 10),
         axis.title = element_text(size = 10),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "top")

plot

ggsave(file = "../out/nmf.svg", plot = plot, width = 5, height = 5)



plot <- ggplot(lars_summary, aes(x = factor(VISIT), y = mean_score, fill = TREATMENT, colour = TREATMENT)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha=0.6) +
  geom_linerange(
  aes(ymin = mean_score - sem_score,
      ymax = mean_score + sem_score),
  position = position_dodge(width = 0.9),
  alpha = 0.3,
  linewidth = 2.5,
  lineend = "round"
) +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
  labs(
    x = "\nVisit (months)",
    y = "Mean LARS Score ± SEM \n",
    fill = "Treatment",  colour = "Treatment",
    title = "Continuous LARS Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

plot

ggsave(file = "../out/nmf.svg", plot = plot, width = 5, height = 5)


library(lme4)
library(emmeans)
library(broom.mixed)

lars2 <- lars %>% inner_join(treat_pats_groups)

model_lars <- lmer(LARSCORE ~ TREATMENT * VISIT + (1|SUBJID), data = lars2)

broom.mixed::tidy(model_lars) %>% 
  mutate(p.value = 2 * (1 - pnorm(abs(statistic))))


#   effect   group    term                    estimate std.error statistic p.value
#   <chr>    <chr>    <chr>                      <dbl>     <dbl>     <dbl>   <dbl>
# 1 fixed    NA       (Intercept)            -28.1        0.604   -46.4      0    
# 2 fixed    NA       TREATMENTPlacebo        -0.877      0.837    -1.05     0.295
# 3 fixed    NA       VISIT                    0.00206    0.0329    0.0627   0.950
# 4 fixed    NA       TREATMENTPlacebo:VISIT  -0.00625    0.0454   -0.138    0.891
# 5 ran_pars SUBJID   sd__(Intercept)          5.07      NA        NA       NA    
# 6 ran_pars Residual sd__Observation          4.83      NA        NA       NA    


emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(0))) %>%
  pairs() 


emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(3))) %>%
  pairs() 

emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(9))) %>%
  pairs() 

emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(18))) %>%
  pairs() 


# > emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(0))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.877 0.837 278   1.048  0.2956
# 
# Degrees-of-freedom method: kenward-roger 
# > emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(3))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.896 0.796 228   1.126  0.2612
# 
# Degrees-of-freedom method: kenward-roger 
# > emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(9))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate   SE  df t.ratio p.value
#  Amantadine - Placebo    0.934 0.78 210   1.197  0.2326
# 
# Degrees-of-freedom method: kenward-roger 
# > emmeans(model_lars, ~ TREATMENT, at = list(VISIT = c(18))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo     0.99 0.922 387   1.074  0.2835
# 
# Degrees-of-freedom method: kenward-roger 


# Create binary apathy variable
lars2 <- lars2 %>% mutate(LARS_apathetic = ifelse(LARSCORE >= -21, 1, 0))

# Fit mixed logistic regression
model_lars_bin <- glmer(LARS_apathetic ~ TREATMENT * VISIT + (1|SUBJID),
                        data = lars2,
                        family = binomial(link = "logit"))

broom.mixed::tidy(model_lars_bin) %>% 
  mutate(p.value = 2 * (1 - pnorm(abs(statistic))))

# # A tibble: 5 × 7
#   effect   group  term                   estimate std.error statistic      p.value
#   <chr>    <chr>  <chr>                     <dbl>     <dbl>     <dbl>        <dbl>
# 1 fixed    NA     (Intercept)             -4.44      0.873     -5.08   0.000000375
# 2 fixed    NA     TREATMENTPlacebo        -0.546     0.635     -0.860  0.390      
# 3 fixed    NA     VISIT                    0.0388    0.0264     1.47   0.142      
# 4 fixed    NA     TREATMENTPlacebo:VISIT   0.0106    0.0363     0.292  0.770      
# 5 ran_pars SUBJID sd__(Intercept)          3.26     NA         NA     NA     


emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(0))) %>%
  pairs() 


emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(3))) %>%
  pairs() 


emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(9))) %>%
  pairs() 

emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(18))) %>%
  pairs() 


# > emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(0))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df z.ratio p.value
#  Amantadine - Placebo    0.546 0.635 Inf   0.860  0.3898
# 
# Results are given on the log odds ratio (not the response) scale. 
# > 
# > 
# > emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(3))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df z.ratio p.value
#  Amantadine - Placebo    0.514 0.591 Inf   0.870  0.3844
# 
# Results are given on the log odds ratio (not the response) scale. 
# > 
# > 
# > emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(9))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate    SE  df z.ratio p.value
#  Amantadine - Placebo     0.45 0.559 Inf   0.805  0.4206
# 
# Results are given on the log odds ratio (not the response) scale. 
# > 
# > emmeans(model_lars_bin, ~ TREATMENT, at = list(VISIT = c(18))) %>%
# +   pairs() 
# NOTE: Results may be misleading due to involvement in interactions
#  contrast             estimate   SE  df z.ratio p.value
#  Amantadine - Placebo    0.355 0.66 Inf   0.538  0.5907
# 
# Results are given on the log odds ratio (not the response) scale. 



lars_prev <- lars2 %>%
  group_by(TREATMENT, VISIT) %>%
  summarise(
    prevalence = mean(LARS_apathetic, na.rm = TRUE),
    n = n(),
    se = sqrt(prevalence*(1-prevalence)/n)
  ) %>% ungroup()

# Plot with points + lines + error bars
ggplot(lars_prev,
       aes(x = VISIT, y = prevalence, color = TREATMENT, group = TREATMENT)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_errorbar(aes(ymin = prevalence - se, ymax = prevalence + se), width = 0.5) +
  scale_x_continuous(breaks = c(0, 3, 9, 18)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "\nVisit (months)",
       y = "Prevalence of Apathy (LARS ≥ -21) \n",
       color = "Treatment Group") +
  scale_colour_manual(values=c("#aa3951", "#2f3941")) +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "top")
