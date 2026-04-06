
library(haven)
library(tidyverse)
library(data.table)
library(lubridate)
options(scipen=999)

admin <- read_sas("../data/admin.sas7bdat")

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

mds <- read_sas("../data/mds.sas7bdat")
 
mds %>% select(SUBJID, VISIT, MDS68) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  group_by(SUBJID, VISIT) %>% summarise(MDS68=max(MDS68)) %>%
  inner_join(treat_pats_groups) %>%
  mutate(MDS68=ifelse(MDS68>0,1,0)) %>%
  # filter(VISIT==18) %>% distinct() %>%
  group_by(TREATMENT , VISIT) %>%
    summarise(mean=mean(MDS68))


dysk_pats <-  mds %>% select(SUBJID, VISIT, MDS68) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  group_by(SUBJID, VISIT) %>% summarise(MDS68=max(MDS68)) %>%
  inner_join(treat_pats_groups) %>%
  mutate(MDS68=ifelse(MDS68>0,1,0)) 

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

ecmp <- ecmp %>%
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))


plot <- mds %>% select(SUBJID, VISIT, MDS68) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  group_by(SUBJID, VISIT) %>% summarise(MDS68=max(MDS68)) %>%
  mutate(MDS68=ifelse(MDS68==0,"No","Dysk")) %>%
    inner_join(ecmp) %>%
  inner_join(treat_pats_groups) %>%
  rename("Treatment"="TREATMENT") %>%
  mutate(VISIT = as.factor(VISIT)) %>%
    mutate(MDS68 = as.factor(MDS68)) %>%
  ggplot(aes(x = VISIT, y = ecmp_icd_severity , fill = MDS68, colour = MDS68)) +
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
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
  scale_colour_manual(values = c("#aa3951", "#2f3941")) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  facet_wrap(~Treatment, ncol=1) +
  labs(
    title = "ECMP Severity Score Hypodopaminergic [4-item]",
    x = "\n Number of Months From Baseline",
    y = "Mean ECMP Severity Score ± SEM\n",
    fill = "Dyskinesia", colour="Dyskinesia"
  )

plot

ggsave(file = "../out/mean_ecmp_sev_dysk.svg", plot = plot, width = 5, height = 7)




udysrs <- read_sas("../data/udysrs.sas7bdat")

unique(udysrs$VISIT)

udysrs <- udysrs %>%  select(SUBJID, VISIT, UDYHISSCO, UDYOBJSCO, UDYSCTOT) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  filter(VISIT %in% c(2,3,5, 8)) %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() 



udysrs %>% 
  inner_join(
     mds %>% select(SUBJID, VISIT, MDS68) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  group_by(SUBJID, VISIT) %>% summarise(MDS68=max(MDS68)) 
  ) %>%
  group_by(MDS68) %>% summarise(UDYSCTOT=mean(UDYSCTOT))

plot_data <- udysrs %>% 
  inner_join(
    mds %>% 
      select(SUBJID, VISIT, MDS68) %>% 
      filter(VISIT <= 8 | VISIT == 16) %>% 
      filter(VISIT >= 2) %>% 
      mutate(VISIT = ifelse(VISIT == 16, 8, VISIT)) %>%
      mutate(VISIT = ifelse(VISIT == 2, 0,
                      ifelse(VISIT == 3, 3, 
                      ifelse(VISIT == 5, 9, 18)))) %>%
      drop_na() %>%
      group_by(SUBJID, VISIT) %>% 
      summarise(MDS68 = max(MDS68), .groups = 'drop')
  )

plot_data_long <- plot_data %>%
  select(SUBJID, VISIT, MDS68, UDYHISSCO, UDYOBJSCO, UDYSCTOT) %>%
  pivot_longer(cols = c(UDYHISSCO, UDYOBJSCO, UDYSCTOT),
               names_to = "scale",
               values_to = "score") %>%
  mutate(scale = factor(scale, 
                        levels = c("UDYHISSCO", "UDYOBJSCO", "UDYSCTOT"),
                        labels = c("Historical Dyskinesia", 
                                   "Objective Dyskinesia", 
                                   "Total Dyskinesia Score")))



plot <- ggplot(plot_data_long, aes(x = as.factor(MDS68), y = score, 
                            fill = as.factor(MDS68), 
                            color = as.factor(MDS68))) +
  geom_boxplot(alpha = 0.5, size = 0.1, outliers = FALSE, notch = TRUE) +
    facet_wrap(~scale, ncol = 3) +
  geom_jitter(height = 0.2, width = 0.3, alpha = 0.5, size = 1, shape=1, stroke=2) +
  scale_fill_manual(values = c("#89949C", "#2F3941", "#aa3951"), 
                    name = "MDS-UPDRS IV Dysk",
                    labels = c("0", "1", "2+")) +
  scale_color_manual(values = c("#89949C", "#2F3941", "#aa3951"), 
                     name = "MDS-UPDRS IV Dysk",
                     labels = c("0", "1", "2+")) +
  labs(
    title = "Distribution of Dyskinesia Scores ~ \n MDS-UPDRS IV Dyskinesia Status",
    x = "\n MDS-UPDRS IV Dyskinesia Status",
    y = "Dyskinesia Score [UDYSRS Scale]) \n"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave(file = "../out/dysk.svg", plot = plot, width = 7, height = 4)




ecmp %>% select(SUBJID, VISIT, ecmp_icd_severity) %>%
  #mutate(ecmp_icd_severity=ifelse(ecmp_icd_severity>=1,1,0)) %>%
  inner_join(udysrs)  %>%
  group_by(ecmp_icd_severity) %>% summarise(mean=mean(UDYSCTOT), sd=sd(UDYSCTOT))


plot <- ecmp %>% select(SUBJID, VISIT, ecmp_icd_severity) %>%
  mutate(ecmp_icd_severity=ifelse(ecmp_icd_severity>=3,3,ecmp_icd_severity)) %>%
  inner_join(udysrs) %>%
  ggplot(aes(x = as.factor(ecmp_icd_severity), y = UDYSCTOT, 
                            fill = as.factor(ecmp_icd_severity), 
                            color = as.factor(ecmp_icd_severity))) +
  geom_boxplot(alpha = 0.5, size = 0.1, outliers = FALSE, notch = TRUE) +
  geom_jitter(height = 0.2, width = 0.3, alpha = 0.5, size = 1, shape=1, stroke=2) +
  scale_fill_manual(values = c("#aa3951", "#2F3941", "#4E5A63", "#c1cfd1"), 
                    name = "ECMP ICD Score",
                    labels = c("0", "1", "2", "3+")) +
  scale_color_manual(values = c("#aa3951", "#2F3941", "#4E5A63", "#c1cfd1"), 
                     name = "ECMP ICD Score",
                     labels = c("0", "1", "2", "3+")) +
  labs(
    title = "Distribution of Dyskinesia Scores ~ \n ECMP ICD Scores",
    x = "\n ECMP ICD Scores",
    y = "Dyskinesia Score [UDYSRS Scale]) \n"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave(file = "../out/dysk2.svg", plot = plot, width = 7, height = 4)



