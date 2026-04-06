
mds <- read_sas("../data/mds.sas7bdat")




mds <- mds %>% select(SUBJID, VISIT, MDS26, MDS27) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  mutate(MDS26=ifelse(MDS26==0,0,1)) %>%
  mutate(MDS27=ifelse(MDS27==0,0,1)) 


admin <- read_sas("../data/admin.sas7bdat")

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))


mds <- treat_pats_groups %>% select(SUBJID, TREATMENT) %>%
  inner_join(mds)


to_plot <- mds %>%
  inner_join(treat_pats_groups) %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
    n = n(),
    MDS26_n = sum(MDS26 ),
    MDS27_n = sum(MDS27)
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
  mutate(domain=ifelse(domain=="MDS26_n", "MDS-UPDRS 2.12 Falls %", "MDS-UPDRS 2.13 FOG %")) %>%
  filter(domain=="MDS-UPDRS 2.13 FOG %") %>%
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
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of FOG [MDS-UPDRS 2.13 ≥1] \n",
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

ggsave(file = "../out/fog.svg", plot = plot, width = 5, height = 5)





plot <- to_plot_long %>% 
  mutate(domain=ifelse(domain=="MDS26_n", "MDS-UPDRS 2.12 Falls %", "MDS-UPDRS 2.13 FOG %")) %>%
  filter(domain=="MDS-UPDRS 2.12 Falls %") %>%
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
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of Falls [MDS-UPDRS 2.12 ≥1] \n",
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

ggsave(file = "../out/falls.svg", plot = plot, width = 5, height = 5)
