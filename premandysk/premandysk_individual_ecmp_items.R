
library(haven)
library(tidyverse)
library(data.table)
library(lubridate)

options(scipen=999)

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



ecmp_items_long <- ecmp_longitudinal %>%
  pivot_longer(
    cols = ECMP1:ECMP21,
    names_to = "item",
    values_to = "score"
  )


library(broom.mixed)

item_results <- map_dfr(paste0("ECMP",1:21), function(i){

  tmp <- ecmp_longitudinal %>%
    select(SUBJID, VISIT, TREATMENT, all_of(i)) %>%
    rename(score = all_of(i))

  model <- lmer(score ~ TREATMENT * VISIT + (1|SUBJID), data = tmp)

  tidy(model) %>%
    mutate(item = i)

})

item_results <- item_results %>%
  mutate(
    p.value = 2 * (1 - pnorm(abs(statistic)))
  )

item_results <- item_results %>%
  group_by(term) %>%
  mutate(p_adj = p.adjust(p.value, method="BH"))

data.frame(item_results %>%
  filter(term == "TREATMENTPlacebo:VISIT"))


data.frame(item_results %>%
  filter(term == "VISIT"))


ecmp_items_long <- ecmp_longitudinal %>%
  select(SUBJID, VISIT, TREATMENT, ECMP1:ECMP21) %>%
  pivot_longer(
    cols = ECMP1:ECMP21,
    names_to = "item",
    values_to = "score"
  ) %>%
  mutate(
    icb_present = ifelse(score >= 1, 1, 0)
  )

icb_prev <- ecmp_items_long %>%
  group_by(VISIT, TREATMENT, item) %>%
  summarise(
    n = sum(!is.na(icb_present)),
    x = sum(icb_present, na.rm = TRUE),
    prevalence = x / n,
    se = sqrt(prevalence * (1 - prevalence) / n),
    lower = prevalence - 1.96 * se,
    upper = prevalence + 1.96 * se,
    .groups = "drop"
  )

icb_prev <- icb_prev %>%
  mutate(
    # Extract the numeric part from ECMP items
    item_num = as.numeric(str_extract(item, "\\d+")),
    # Create factor with levels ordered by the numeric value
    item = factor(item, levels = paste0("ECMP", sort(unique(item_num))), ordered = TRUE)
  ) %>%
  select(-item_num)
 
 
icb_prev <- icb_prev %>%
  mutate(
    lower = pmax(lower, 0),
    upper = pmin(upper, 1)
  )

plot <- ggplot(icb_prev,
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
    breaks = c(0,3,9,18)
  ) +
  facet_wrap(~ item, ncol = 7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of Individual ECMP item ≥1 \n",
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

ggsave(file = "../out/box_icb_sev.svg", plot = plot, width = 13, height = 8)




ecmp_longitudinal %>% mutate(ecmp_nmf=ECMP8+ECMP9) %>%
  select(SUBJID, VISIT, ecmp_nmf) %>% drop_na() %>%
  inner_join(treat_pats_groups) %>%
  mutate(ecmp_nmf=ifelse(ecmp_nmf >0,1,0)) %>%
  group_by(TREATMENT, VISIT) %>%
  summarise(mean=mean(ecmp_nmf ))



ecmp_longitudinal %>% mutate(ecmp_nmf=ECMP8+ECMP9) %>%
  select(SUBJID, VISIT, ecmp_nmf) %>% drop_na() %>%
  mutate(ecmp_nmf=ifelse(ecmp_nmf >0,1,0)) %>%
  inner_join(treat_pats_groups) %>%
  group_by(TREATMENT, VISIT) %>%
  summarise(mean=mean(ecmp_nmf ))




dysk_pats <- fread("../out/dysk_pats.txt")
dysk_pats <- dysk_pats %>% filter(VISIT==18)

dysk_pats %>% inner_join(ecmp_longitudinal %>% mutate(ecmp_nmf=ECMP8+ECMP9) %>%
  select(SUBJID, VISIT, ecmp_nmf) %>% drop_na() %>% mutate(SUBJID=as.numeric(SUBJID))) %>%
   mutate(ecmp_nmf=ifelse(ecmp_nmf >0,1,0)) %>%
  filter(VISIT==18) %>%
  group_by(TREATMENT, VISIT, MDS68 ) %>%
  summarise(mean=mean(ecmp_nmf ))
  


ecmp_nmf_df <- ecmp_longitudinal %>%
  mutate(ecmp_nmf = ECMP8 + ECMP9) %>%
  select(SUBJID, VISIT, ecmp_nmf) %>%
  drop_na() %>%
  mutate(
    ecmp_nmf_bin = ifelse(ecmp_nmf > 0, 1, 0),
    VISIT = ifelse(VISIT == 2, 0,
                   ifelse(VISIT == 3, 3,
                          ifelse(VISIT == 5, 9, 18)))
  ) %>%
  inner_join(treat_pats_groups, by = "SUBJID") %>%
  mutate(TREATMENT = factor(TREATMENT))


library(lme4)
library(broom.mixed)

model_ecmp_nmf <- glmer(
  ecmp_nmf_bin ~ TREATMENT * VISIT + (1 | SUBJID),
  data = ecmp_nmf_df,
  family = binomial(link = "logit")
)

broom.mixed::tidy(model_ecmp_nmf) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(statistic))))


library(emmeans)

emmeans(model_ecmp_nmf, ~ TREATMENT, at = list(VISIT = 0)) %>% pairs()
emmeans(model_ecmp_nmf, ~ TREATMENT, at = list(VISIT = 3)) %>% pairs()
emmeans(model_ecmp_nmf, ~ TREATMENT, at = list(VISIT = 9)) %>% pairs()
emmeans(model_ecmp_nmf, ~ TREATMENT, at = list(VISIT = 18)) %>% pairs()


emmeans(model_ecmp_nmf, ~ TREATMENT | VISIT, type = "response", at = list(VISIT = 0))
emmeans(model_ecmp_nmf, ~ TREATMENT | VISIT, type = "response", at = list(VISIT = 3))
emmeans(model_ecmp_nmf, ~ TREATMENT | VISIT, type = "response", at = list(VISIT = 9))
emmeans(model_ecmp_nmf, ~ TREATMENT | VISIT, type = "response", at = list(VISIT = 18))

# --------