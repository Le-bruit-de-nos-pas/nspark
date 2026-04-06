
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

fwrite(treat_pats_groups, "../out/treat_pats_groups.txt")

efnm <- read_sas("../data/efnm.sas7bdat")

efnm_2 <- efnm %>% mutate(across(starts_with("EF") & ends_with("YN"), ~replace_na(., 0) )) 


efnm_2 <- efnm_2 %>% mutate(
  EF1 = EF1YN==1 & EF1PREYN==1,
    EF2 = EF2YN==1 & EF2PREYN==1,
    EF3 = EF3YN==1 & EF3PREYN==1,
    EF4 = EF4YN==1 & EF4PREYN==1,
    EF5 = EF5YN==1 & EF5PREYN==1,
    EF6 = EF6YN==1 & EF6PREYN==1,
    EF7 = EF7YN==1 & EF7PREYN==1,
    EF8 = EF8YN==1 & EF8PREYN==1,
    EF9 = EF9YN==1 & EF9PREYN==1,
    EF10 = EF10YN==1 & EF10PREYN==1,
    EF11 = EF11YN==1 & EF11PREYN==1,
    EF12 = EF12YN==1 & EF12PREYN==1,
    EF13 = EF13YN==1 & EF13PREYN==1,
    EF14 = EF14YN==1 & EF14PREYN==1,
    EF15 = EF15YN==1 & EF15PREYN==1,
    EF16 = EF16YN==1 & EF16PREYN==1,
    EF17 = EF17YN==1 & EF17PREYN==1,
    EF18 = EF18YN==1 & EF18PREYN==1,
    EF19 = EF19YN==1 & EF19PREYN==1,
    EF20 = EF20YN==1 & EF20PREYN==1,
    EF21 = EF21YN==1 & EF21PREYN==1,
    EF22 = EF22YN==1 & EF22PREYN==1
) %>%
  mutate(
    EFNM_total = rowSums(select(.,EF1:EF22)),
    NMF_bin = EFNM_total > 0,
    NMF_cognitive = rowSums(select(.,EF1:EF8)),
    NMF_cognitive_bin = NMF_cognitive > 0,
    NMF_dysautonomia = rowSums(select(.,EF9:EF18)),
    NMF_dysautonomia_bin = NMF_dysautonomia > 0,
    NMF_pain = rowSums(select(.,EF19:EF22)),
    NMF_pain_bin = NMF_pain > 0,

  )

efnm_2 <- efnm_2 %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

efnm_2 <- efnm_2 %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))


efnm_2 %>% inner_join(treat_pats_groups) %>% 
  group_by(TREATMENT, VISIT) %>% 
  summarise(mean=mean(EFNM_total), sd=sd(EFNM_total))

efnm_2 %>% inner_join(treat_pats_groups) %>% 
  group_by(TREATMENT, VISIT) %>%
  summarise(mean=mean(NMF_pain_bin))



dysk_pats <- fread("../out/dysk_pats.txt")



dysk_pats %>% filter(VISIT==18) %>% group_by(TREATMENT, MDS68) %>% count()


efnm_2 %>% mutate(SUBJID=as.numeric(SUBJID)) %>%
  inner_join(treat_pats_groups %>%  mutate(SUBJID=as.numeric(SUBJID)) ) %>%
  inner_join(dysk_pats %>% filter(VISIT==18) %>% select(SUBJID , MDS68)) %>%
  group_by(TREATMENT, VISIT, MDS68) %>% filter(VISIT==18) %>%
  summarise(mean=mean(NMF_bin))



efnm_2 %>%
  group_by(VISIT) %>%
  summarise(
  n = n(),
  NMF_bin_n = sum(NMF_bin),
  NMF_bin_pct = mean(NMF_bin)*100,
  NMF_cognitive_bin_n = sum(NMF_cognitive_bin),
  NMF_cognitive_bin_pct = mean(NMF_cognitive_bin)*100,
  NMF_dysautonomia_bin_n = sum(NMF_dysautonomia_bin),
  NMF_dysautonomia_bin_pct = mean(NMF_dysautonomia_bin)*100,
  NMF_pain_bin_n = sum(NMF_pain_bin),
  NMF_pain_bin_pct = mean(NMF_pain_bin)*100,
  )

to_plot <-  efnm_2 %>%
  inner_join(treat_pats_groups) %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
  n = n(),
  NMF_bin_n = sum(NMF_bin),
  NMF_bin_pct = mean(NMF_bin)*100,
  NMF_cognitive_bin_n = sum(NMF_cognitive_bin),
  NMF_cognitive_bin_pct = mean(NMF_cognitive_bin)*100,
  NMF_dysautonomia_bin_n = sum(NMF_dysautonomia_bin),
  NMF_dysautonomia_bin_pct = mean(NMF_dysautonomia_bin)*100,
  NMF_pain_bin_n = sum(NMF_pain_bin),
  NMF_pain_bin_pct = mean(NMF_pain_bin)*100,
  )

to_plot <- to_plot %>% select(VISIT, TREATMENT, NMF_bin_pct, NMF_cognitive_bin_pct, NMF_dysautonomia_bin_pct, NMF_pain_bin_pct)

to_plot <- to_plot %>% 
  pivot_longer(
    cols = NMF_bin_pct :NMF_pain_bin_pct,
    names_to = "domain",
    values_to = "perc"
  ) 
  

plot <- to_plot %>% mutate(perc=perc/100) %>%
  mutate(domain=ifelse(domain=="NMF_bin_pct", "NMF Total %",
                       ifelse(domain=="NMF_cognitive_bin_pct", "NMF Cogntitive %",
                              ifelse(domain=="NMF_dysautonomia_bin_pct", "NMF Dysautonomia %", "NMF Pain %")))) %>%
  mutate(domain=factor(domain, levels=c("NMF Total %","NMF Cogntitive %", "NMF Dysautonomia %", "NMF Pain %"))) %>%
  ggplot(aes(x = VISIT,
           y = perc,
           color = TREATMENT,
           group = TREATMENT)) +
  geom_line(size = 2, alpha=0.6) +
  geom_point(shape=1, size=2, stroke=2) +
  scale_x_continuous(
    breaks = seq(0, 18, by = 3)
  ) +
  facet_wrap(~ domain, ncol = 7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of NMF Total|Subdomain ≥1 \n",
    title = "",
    color = "Treatment Group"
  ) +
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


ggsave(file = "../out/nmf.svg", plot = plot, width = 12, height = 4)


to_plot <- efnm_2 %>%
  inner_join(treat_pats_groups) %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
    n = n(),
    NMF_bin_n = sum(NMF_bin),
    NMF_cognitive_bin_n = sum(NMF_cognitive_bin),
    NMF_dysautonomia_bin_n = sum(NMF_dysautonomia_bin),
    NMF_pain_bin_n = sum(NMF_pain_bin),
  )


to_plot_long <- to_plot %>%
  pivot_longer(
    cols = ends_with("_n"),
    names_to = "domain",
    values_to = "x"
  ) %>%
  mutate(
    p = x / n,
    se = sqrt(p * (1 - p) / n),
    lower = p - 1.96 * se,
    upper = p + 1.96 * se
  )




plot <- to_plot_long %>% 
  mutate(domain=ifelse(domain=="NMF_bin_n", "NMF Total %",
                       ifelse(domain=="NMF_cognitive_bin_n", "NMF Cogntitive %",
                             ifelse(domain=="NMF_dysautonomia_bin_n", "NMF Dysautonomia %", "NMF Pain %")))) %>%
   mutate(domain=factor(domain, levels=c("NMF Total %","NMF Cogntitive %", "NMF Dysautonomia %", "NMF Pain %"))) %>%
  ggplot(aes(x = VISIT,
           y = p,
           color = TREATMENT,
           group = TREATMENT)) +
  
  geom_line(size = 2, alpha=0.6) +
  geom_linerange(
  aes(ymin = lower, ymax = upper),
  alpha = 0.2,
  linewidth = 2.5,
  lineend = "round"
) +
  geom_point(shape=1, size=2, stroke=2) +
  scale_x_continuous(
    breaks = seq(0, 18, by = 3)
  ) +
  facet_wrap(~ domain, ncol = 7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of NMF Total|Subdomain ≥1 \n",
    title = "",
    color = "Treatment Group"
  ) +
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

ggsave(file = "../out/nmf.svg", plot = plot, width = 12, height = 5)






names(efnm_2)

to_test <- efnm_2 %>%
  inner_join(treat_pats_groups)  %>%
  select(SUBJID, VISIT, TREATMENT, EFNM_total, NMF_cognitive, NMF_dysautonomia, NMF_pain)



plot <- to_test %>% 
  rename("Treatment"="TREATMENT") %>%
  mutate(VISIT = as.factor(VISIT)) %>%
  pivot_longer(
    cols = EFNM_total:NMF_pain,
    names_to = "domain",
    values_to = "score"
  ) %>%
  ggplot(aes(x = VISIT, y = score, fill = Treatment, colour=Treatment)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.8),
    width = 0.7,
    alpha = 0.7
  ) +
  stat_summary(
  fun.data = mean_se,
  geom = "linerange",
  position = position_dodge(width = 0.8),
  linewidth = 2.5,
  alpha = 0.5,
  lineend = "round"
) +
  facet_wrap(~ domain, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "NMF (Sub)-Totals",
    x = "\n Number of Months From Baseline",
    y = "Mean NMF Score ± SEM\n",
    fill = "Treatment"
  )

plot

ggsave(file = "../out/nmf.svg", plot = plot, width = 12, height = 4)


library(emmeans)

model <- lme4::lmer(EFNM_total  ~ TREATMENT * VISIT + (1|SUBJID), data = to_test)

broom.mixed::tidy(model) %>% mutate(
    p.value = 2 * (1 - pnorm(abs(statistic)))
  )

#   effect   group    term                   estimate std.error statistic  p.value
#   <chr>    <chr>    <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
# 1 fixed    NA       (Intercept)             1.36       0.230      5.91   3.52e-9
# 2 fixed    NA       TREATMENTPlacebo       -0.337      0.318     -1.06   2.89e-1
# 3 fixed    NA       VISIT                   0.00457    0.0130     0.352  7.25e-1
# 4 fixed    NA       TREATMENTPlacebo:VISIT  0.00540    0.0178     0.303  7.62e-1
# 5 ran_pars SUBJID   sd__(Intercept)         1.90      NA         NA     NA      
# 6 ran_pars Residual sd__Observation         1.69      NA         NA     NA   


pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(0))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.337 0.318 286   1.060  0.2902
# 
# Degrees-of-freedom method: kenward-roger 


pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(3))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.321 0.301 231   1.067  0.2870
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(9))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.289 0.293 208   0.983  0.3265
# 
# Degrees-of-freedom method: kenward-roger

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(18))))

#  contrast             estimate   SE  df t.ratio p.value
#  Amantadine - Placebo     0.24 0.35 391   0.686  0.4933
# 
# Degrees-of-freedom method: kenward-roger 


model <- lme4::lmer(NMF_cognitive  ~ TREATMENT * VISIT + (1|SUBJID), data = to_test)

broom.mixed::tidy(model) %>% mutate(
    p.value = 2 * (1 - pnorm(abs(statistic)))
  )

#   effect   group    term                   estimate std.error statistic  p.value
#   <chr>    <chr>    <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
# 1 fixed    NA       (Intercept)             0.682     0.117       5.84   5.29e-9
# 2 fixed    NA       TREATMENTPlacebo       -0.268     0.161      -1.66   9.61e-2
# 3 fixed    NA       VISIT                   0.00136   0.00648     0.209  8.34e-1
# 4 fixed    NA       TREATMENTPlacebo:VISIT  0.00930   0.00890     1.04   2.96e-1
# 5 ran_pars SUBJID   sd__(Intercept)         0.967    NA          NA     NA      
# 6 ran_pars Residual sd__Observation         0.842    NA          NA     NA    



pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(0))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo    0.268 0.161 283   1.664  0.0972
# 
# Degrees-of-freedom method: kenward-roger 


pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(3))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo    0.241 0.153 230   1.574  0.1168
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(9))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo    0.185 0.149 208   1.239  0.2169
# 
# Degrees-of-freedom method: kenward-roger

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(18))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo    0.101 0.177 385   0.571  0.5682
# 
# Degrees-of-freedom method: kenward-roger 



model <- lme4::lmer(NMF_dysautonomia  ~ TREATMENT * VISIT + (1|SUBJID), data = to_test)

broom.mixed::tidy(model) %>% mutate(
    p.value = 2 * (1 - pnorm(abs(statistic)))
  )

#   effect   group    term                   estimate std.error statistic    p.value
#   <chr>    <chr>    <chr>                     <dbl>     <dbl>     <dbl>      <dbl>
# 1 fixed    NA       (Intercept)             0.401     0.0955      4.20   0.0000268
# 2 fixed    NA       TREATMENTPlacebo       -0.0521    0.132      -0.395  0.693    
# 3 fixed    NA       VISIT                   0.00289   0.00624     0.463  0.643    
# 4 fixed    NA       TREATMENTPlacebo:VISIT -0.00180   0.00858    -0.210  0.834    
# 5 ran_pars SUBJID   sd__(Intercept)         0.722    NA          NA     NA        
# 6 ran_pars Residual sd__Observation         0.813    NA          NA     NA  



pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(0))))

# contrast             estimate    SE  df t.ratio p.value
#  Amantadine - Placebo   0.0521 0.132 324   0.395  0.6928
# Degrees-of-freedom method: kenward-roger 


pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(3))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo   0.0575 0.122 243   0.472  0.6377
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(9))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo   0.0683 0.118 210   0.580  0.5622
# 
# Degrees-of-freedom method: kenward-roger

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(18))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo   0.0845 0.149 468   0.567  0.5712
# 
# Degrees-of-freedom method: kenward-roger 



model <- lme4::lmer(NMF_pain  ~ TREATMENT * VISIT + (1|SUBJID), data = to_test)

broom.mixed::tidy(model) %>% mutate(
    p.value = 2 * (1 - pnorm(abs(statistic)))
  )

#  effect   group    term                     estimate std.error statistic     p.value
#   <chr>    <chr>    <chr>                       <dbl>     <dbl>     <dbl>       <dbl>
# 1 fixed    NA       (Intercept)             0.277       0.0601     4.60    0.00000423
# 2 fixed    NA       TREATMENTPlacebo       -0.0150      0.0830    -0.181   0.856     
# 3 fixed    NA       VISIT                   0.0000979   0.00414    0.0236  0.981     
# 4 fixed    NA       TREATMENTPlacebo:VISIT -0.00174     0.00569   -0.306   0.760     
# 5 ran_pars SUBJID   sd__(Intercept)         0.436      NA         NA      NA         
# 6 ran_pars Residual sd__Observation         0.539      NA         NA      NA    



pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(0))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo    0.015 0.083 343   0.181  0.8563
# Degrees-of-freedom method: kenward-roger 


pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(3))))

 # contrast             estimate     SE  df t.ratio p.value
 # Amantadine - Placebo   0.0203 0.0761 249   0.266  0.7902
# 
# Degrees-of-freedom method: kenward-roger 

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(9))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo   0.0307 0.073 211   0.420  0.6746
# 
# Degrees-of-freedom method: kenward-roger

pairs(emmeans(model, ~ TREATMENT, at = list(VISIT = c(18))))

 # contrast             estimate    SE  df t.ratio p.value
 # Amantadine - Placebo   0.0464 0.095 501   0.488  0.6256
# 
# Degrees-of-freedom method: kenward-roger 

