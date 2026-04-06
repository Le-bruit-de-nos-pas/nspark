

library(haven)
library(tidyverse)
library(data.table)
library(lubridate)

admin <- read_sas("../data/admin.sas7bdat")

admin %>% group_by(VISIT) %>% count()

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

pdq8 <- read_sas("../data/pdq8.sas7bdat")

pdq8 <- pdq8 %>%
  mutate( pdq8 = rowSums(select(., PDQ81:PDQ88), na.rm = TRUE),
) %>% select(SUBJID, VISIT, pdq8)


pdq8 <- pdq8 %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

pdq8 <- pdq8 %>% group_by(VISIT) %>% 
  mutate(VISIT=ifelse(VISIT==2, 0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) 


mds <- read_sas("../data/mds.sas7bdat")
 
mds <- mds %>% select(SUBJID, VISIT, MDS26, MDS27, MDS68) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() %>%
  group_by(SUBJID, VISIT) %>% summarise(MDS26=max(MDS26),MDS27=max(MDS27),MDS68=max(MDS68)) %>%
  inner_join(treat_pats_groups) %>%
  mutate(MDS26=ifelse(MDS26>0,1,0),MDS27=ifelse(MDS27>0,1,0),MDS68=ifelse(MDS68>0,1,0)) 


pdq8 <- pdq8 %>% inner_join(mds)


pdq8 %>% group_by(SUBJID, VISIT) %>% count() %>% filter(n!=1)


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

names(efnm_2)

efnm_2 <- efnm_2 %>% select(SUBJID, VISIT, EFNM_total, NMF_cognitive, NMF_dysautonomia, NMF_pain)

pdq8 <- pdq8 %>% inner_join(efnm_2)


pdq8 %>% group_by(SUBJID, VISIT) %>% count() %>% filter(n!=1)


pfs <- read_sas("../data/pfs.sas7bdat")

pfs <- pfs %>%
  mutate(
    pfs = rowSums(select(.,PFS1:PFS10 ))  )

pfs <- pfs %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

pfs <- pfs %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

names(pfs)

pfs <- pfs %>% select(SUBJID, VISIT, pfs)

pdq8 <- pdq8 %>% inner_join(pfs)


pdq8 %>% group_by(SUBJID, VISIT) %>% count() %>% filter(n!=1)


pdq8 %>% filter(is.na(TREATMENT))

fg <- read_sas("../data/fg.sas7bdat")

fg <- fg %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

fg <- fg %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

names(fg)

fg <- fg %>% select(SUBJID, VISIT, FREESC, FREEZ3)



pdq8 <- pdq8 %>% inner_join(fg)


to_plot <- pdq8 %>%
  mutate(FREEZ3 = ifelse(FREEZ3 == 0, 0, 1)) %>%
  select(SUBJID, VISIT, TREATMENT, FREEZ3) %>%
  group_by(VISIT, TREATMENT) %>%
  summarise(
    n = n(),
    FREEZ3_n = sum(FREEZ3, na.rm=T),
    .groups = 'drop'
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
    lower = pmax(0, p - 1.96 * se),  # prevent negative lower bounds
    upper = pmin(1, p + 1.96 * se)   # prevent upper bounds > 1
  )

# Create the plot
plot <- to_plot_long %>% 
  mutate(domain = "FREEZ3 (Self-reported freezing)") %>%
  ggplot(aes(x = VISIT,
             y = p,
             color = TREATMENT,
             group = TREATMENT)) +
  
  geom_line(size = 2, alpha = 0.6) +
  geom_linerange(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    linewidth = 2.5,
    lineend = "round"
  ) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  scale_x_continuous(
    breaks = seq(0, 18, by = 3)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\n Visit (months)",
    y = "Prevalence of Freezing [FOG Quest #3 ≥1] \n",
    title = "",
    color = "Treatment Group"
  ) +
  scale_colour_manual(values = c("#aa3951", "#2f3941")) +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top") 

# Display the plot
print(plot)

# Save the plot
ggsave(file = "../out/fog.svg", plot = plot, width = 5, height = 5)



names(pdq8)

pdq8 %>% group_by(SUBJID, VISIT) %>% count() %>% filter(n!=1)




udysrs <- read_sas("../data/udysrs.sas7bdat")

unique(udysrs$VISIT)

udysrs <- udysrs %>%  select(SUBJID, VISIT, UDYHISSCO, UDYOBJSCO, UDYSCTOT) %>% 
  filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
  filter(VISIT %in% c(2,3,5, 8)) %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18)))) %>%
  drop_na() 


pdq8 <- pdq8  %>% left_join(udysrs)




# Check distribution of PDQ-8 over time
library(ggplot2)
library(lme4)

# Quick visualization


pdq8 %>% select(TREATMENT, FREESC) %>% drop_na() %>% 
  group_by(TREATMENT, VISIT)  %>% summarise(mean=mean(FREESC, na.rm=T), sd=sd(FREESC, na.rm=T), n=n())



pdq8 %>% select(TREATMENT, TREATMENT, VISIT, UDYSCTOT) %>% drop_na() %>% 
  group_by(TREATMENT, VISIT)  %>% summarise(mean=mean(UDYSCTOT, na.rm=T), sd=sd(UDYSCTOT, na.rm=T), n=n())


plot <- pdq8 %>% mutate(VISIT=as.factor(VISIT)) %>%
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = VISIT, y = pdq8,  colour=Treatment, fill=Treatment  )) +
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
  labs(
    title = "PDQ8 Scores",
    x = "\n Number of Months From Baseline",
    y = "Mean PFQ8 Total Score ± SEM\n",
    fill = "Treatment", colour="Treatment"
  )

plot
#ggsave(file = "../out/pdq8.svg", plot = plot, width = 4, height = 4)






plot <- pdq8 %>% mutate(VISIT=as.factor(VISIT)) %>%
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = VISIT, y = UDYSCTOT,  colour=Treatment, fill=Treatment  )) +
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
  labs(
    title = "Unified Dyskinesia Scale Scores",
    x = "\n Number of Months From Baseline",
    y = "Mean Unified Dyskinesia Scale Total Score ± SEM\n",
    fill = "Treatment", colour="Treatment"
  )

plot
ggsave(file = "../out/udysk.svg", plot = plot, width = 4, height = 4)





plot <- pdq8 %>% mutate(VISIT=as.factor(VISIT)) %>%
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = VISIT, y = FREESC,  colour=Treatment, fill=Treatment  )) +
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
  labs(
    title = "Freezing Q Scores",
    x = "\n Number of Months From Baseline",
    y = "Mean Freezing Q Total Score ± SEM\n",
    fill = "Treatment", colour="Treatment"
  )

plot

#ggsave(file = "../out/pdq8.svg", plot = plot, width = 4, height = 4)





names(pdq8)



# Define mediators
mediators <- c("MDS68", "MDS26", "MDS27", "EFNM_total", "NMF_cognitive", "NMF_dysautonomia", 
               "NMF_pain", "pfs", "FREESC", "UDYSCTOT", "FREEZ3")


pdq8 <- pdq8  %>% mutate(FREEZ3=ifelse(FREEZ3>0,1,0))


# Function to run mixed model for a single mediator
test_mediator <- function(mediator_name) {
  # Create formula
  formula <- as.formula(paste("pdq8 ~ VISIT +", mediator_name, "+ (1 | SUBJID)"))
  
  # Fit model
  model <- lmer(formula, data = pdq8)
  
  # Extract results for mediator
  coef_summary <- summary(model)$coefficients
  mediator_row <- coef_summary[mediator_name, , drop = FALSE]
  
  # Return results
  data.frame(
    mediator = mediator_name,
    coefficient = mediator_row[1],
    std_error = mediator_row[2],
    t_value = mediator_row[3],
    p_value = mediator_row[4],
    n_obs = nobs(model),
    n_subjects = length(unique(pdq8$SUBJID))
  )
}

# Run for all mediators
results <- map_df(mediators, test_mediator)
results$p_value <- 2 * pt(abs(results$t_value), df = results$n_obs - 2, lower.tail = FALSE)

#            mediator coefficient   std_error   t_value      p_value n_obs n_subjects
# 1             MDS68 -0.82406880 0.507044891 -1.625238 1.045210e-01   772        206
# 2             MDS26  0.90653575 0.289140754  3.135275 1.782143e-03   772        206
# 3             MDS27  1.61420010 0.410652691  3.930816 9.228077e-05   772        206
# 4        EFNM_total  0.31315508 0.059272296  5.283330 1.652792e-07   772        206
# 5     NMF_cognitive  0.53649430 0.118775340  4.516883 7.259626e-06   772        206
# 6  NMF_dysautonomia  0.51339751 0.126863446  4.046851 5.715150e-05   772        206
# 7          NMF_pain  0.63318594 0.193697924  3.268935 1.127465e-03   772        206
# 8               pfs  0.12050972 0.009679235 12.450336 1.782870e-32   751        206
# 9            FREESC  0.37059947 0.033137061 11.183836 5.273690e-27   769        206
# 10         UDYSCTOT  0.07250908 0.045787553  1.583598 1.137085e-01   749        206
# 11           FREEZ3  2.55787407 0.308102204  8.302031 4.589172e-16   771        206

results$p_value <- round(results$p_value, 4)

results$sig_star <- ifelse(results$p_value < 0.001, "***",
                    ifelse(results$p_value < 0.01, "**",
                    ifelse(results$p_value < 0.05, "*", "")))

results$mediator <- factor(results$mediator, 
                           levels = results$mediator[order(results$coefficient)])


# Color by significance
forest_plot_color <- results %>%
  mutate(mediator=ifelse(mediator=="MDS68", "MDS-UPDRS IV Dysk",
                         ifelse(mediator=="MDS26", "MDS-UPDRS 2.12 Falls",
                                ifelse(mediator=="MDS27", "MDS-UPDRS 2.13 FOG",
                                       ifelse(mediator=="EFNM_total", "Non-motor Fluct. Score",
                                              ifelse(mediator=="NMF_cognitive", "Non-motor Fluct. Cognitive Score",
                                                     ifelse(mediator=="NMF_dysautonomia", "Non-motor Fluct. Dysautonomia Score",
                                                            ifelse(mediator=="NMF_pain", "Non-motor Fluct. Pain Score",
                                                                   ifelse(mediator=="pfs", "Fatigue Pain Score",
                                                                          ifelse(mediator=="FREESC", "Freezing Quest Score", 
                                                                                 ifelse(mediator=="UDYSCTOT", "Unified Dyskinesia Score", "FOG Q Item #3"))))))))))) %>%
  ggplot(aes(x = coefficient, 
                                y = reorder(mediator, coefficient),
                                color = p_value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_linerange(aes(xmin = coefficient - 1.96 * std_error, 
                     xmax = coefficient + 1.96 * std_error),
                 size = 2.5, alpha = 0.7, lineend = "round") +
  geom_point(size = 4, stroke = 2.5, shape = 1, alpha=0.8) +
  scale_color_manual(values = c("TRUE" = "#aa3951", "FALSE" = "#2f3941"),
                     name = "p < 0.05") +
  labs(title = "Association with PDQ-8 [Same Visit Mixed Model]\n",
       x = "\n Coefficient [95% CI]",
       y = "") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3,3)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 12, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(forest_plot_color)

ggsave(file = "../out/forest_plot.svg", plot = forest_plot_color, width = 8, height = 4)




library(tidyverse)

# Calculate deltas for each patient
delta_df <- pdq8 %>%
  # Filter to baseline and final visit only
  filter(VISIT %in% c(0, 18)) %>%
  # Ensure each patient has both visits
  group_by(SUBJID) %>%
  filter(n() == 2) %>%
  # Pivot to wide format to calculate differences
  select(SUBJID, VISIT, TREATMENT, pdq8, MDS68, MDS26, MDS27, FREEZ3, EFNM_total, 
         NMF_cognitive, NMF_dysautonomia, NMF_pain, pfs , FREESC, UDYSCTOT ) %>%
  pivot_wider(id_cols = c(SUBJID, TREATMENT),
              names_from = VISIT,
              values_from = c(pdq8, MDS68, MDS26, MDS27, FREEZ3 , EFNM_total, NMF_cognitive,
                              NMF_dysautonomia, NMF_pain, pfs , FREESC, UDYSCTOT ),
              names_prefix = "visit_") %>%
  # Calculate deltas (visit_18 - visit_0)
  mutate(
    delta_pdq8 = pdq8_visit_18 - pdq8_visit_0,
    delta_MDS68 = MDS68_visit_18 - MDS68_visit_0,
    delta_MDS26 = MDS26_visit_18 - MDS26_visit_0,
    delta_MDS27 = MDS27_visit_18 - MDS27_visit_0,
    delta_FREEZ3 = FREEZ3_visit_18 - FREEZ3_visit_0,
    delta_EFNM_total = EFNM_total_visit_18 - EFNM_total_visit_0,
    delta_NMF_cognitive = NMF_cognitive_visit_18 - NMF_cognitive_visit_0,
    delta_NMF_dysautonomia = NMF_dysautonomia_visit_18 - NMF_dysautonomia_visit_0,
    delta_NMF_pain = NMF_pain_visit_18 - NMF_pain_visit_0,
    delta_pfs = pfs_visit_18 - pfs_visit_0,
    delta_FREESC = FREESC_visit_18 - FREESC_visit_0,
    delta_UDYSCTOT = UDYSCTOT_visit_18 - UDYSCTOT_visit_0

  ) %>%
  # Select only delta columns and treatment
  select(SUBJID, TREATMENT, starts_with("delta_"))




# Then run your models on delta_df_scaled
# Check sample size
n_patients <- nrow(delta_df)
cat("Number of patients with both visits:", n_patients, "\n")



# Define mediators (excluding MDS68 if it's binary change -3 to +1)
mediators <- c("delta_EFNM_total", "delta_NMF_cognitive", "delta_NMF_dysautonomia",
               "delta_NMF_pain", "delta_pfs", "delta_FREESC",  "delta_UDYSCTOT")

# Add delta_MDS68 separately since it's different
mediators_all <- c(mediators, "delta_MDS68", "delta_MDS26", "delta_MDS27", "delta_FREEZ3")

# Function to test each mediator
test_delta_mediator <- function(mediator_name) {
  formula <- as.formula(paste("delta_pdq8 ~", mediator_name))
  model <- lm(formula, data = delta_df)
  
  coef_summary <- summary(model)$coefficients
  
  data.frame(
    mediator = gsub("delta_", "", mediator_name),
    coefficient = coef_summary[2, 1],
    std_error = coef_summary[2, 2],
    t_value = coef_summary[2, 3],
    p_value = coef_summary[2, 4],
    r_squared = summary(model)$r.squared,
    n = nobs(model)
  )
}

# Run for all mediators
delta_results <- map_df(mediators_all, test_delta_mediator) %>%
  mutate(across(c(coefficient, std_error, r_squared), ~ round(., 3)),
         p_value = round(p_value, 4)) %>%
  arrange(p_value)

# Display results
print(delta_results)





#            mediator coefficient std_error     t_value p_value r_squared   n
# 1               pfs       0.090     0.020  4.44679032  0.0000     0.103 175
# 2            FREESC       0.361     0.075  4.81688980  0.0000     0.116 179
# 3            FREEZ3       1.658     0.569  2.91369196  0.0040     0.046 180
# 4             MDS27       1.437     0.787  1.82553582  0.0696     0.018 181
# 5             MDS68      -0.861     0.790 -1.09026027  0.2771     0.007 181
# 6     NMF_cognitive       0.191     0.232  0.82076035  0.4129     0.004 181
# 7        EFNM_total       0.073     0.114  0.63879717  0.5238     0.002 181
# 8  NMF_dysautonomia       0.149     0.265  0.56330401  0.5739     0.002 181
# 9             MDS26      -0.174     0.555 -0.31378700  0.7540     0.001 181
# 10         UDYSCTOT       0.017     0.069  0.24739632  0.8049     0.000 171
# 11         NMF_pain      -0.006     0.367 -0.01640814  0.9869     0.000 181



delta_results$p_value <- round(delta_results$p_value, 4)

delta_results$sig_star <- ifelse(delta_results$p_value < 0.001, "***",
                    ifelse(delta_results$p_value < 0.01, "**",
                    ifelse(delta_results$p_value < 0.05, "*", "")))

delta_results$mediator <- factor(delta_results$mediator, 
                           levels = delta_results$mediator[order(delta_results$coefficient)])


# Color by significance
forest_plot_color <- delta_results %>%
  mutate(mediator=ifelse(mediator=="MDS68", "Δ MDS-UPDRS IV Dysk",
                         ifelse(mediator=="MDS26", "Δ MDS-UPDRS 2.12 Falls",
                                ifelse(mediator=="MDS27", "Δ MDS-UPDRS 2.13 FOG",
                                       ifelse(mediator=="EFNM_total", "Δ Non-motor Fluct. Score",
                                              ifelse(mediator=="NMF_cognitive", "Δ Non-motor Fluct. Cognitive Score",
                                                     ifelse(mediator=="NMF_dysautonomia", "Δ Non-motor Fluct. Dysautonomia Score",
                                                            ifelse(mediator=="NMF_pain", "Δ Non-motor Fluct. Pain Score",
                                                                   ifelse(mediator=="pfs", "Δ Fatigue Pain Score",
                                                                          ifelse(mediator=="FREESC", "Δ Freezing Quest Score", 
                                                                                 ifelse(mediator=="UDYSCTOT", "Δ Unified Dyskinesia Score", "Δ FOG Q Item #3"))))))))))) %>%
  ggplot(aes(x = coefficient, 
                                y = reorder(mediator, coefficient),
                                color = p_value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_linerange(aes(xmin = coefficient - 1.96 * std_error, 
                     xmax = coefficient + 1.96 * std_error),
                 size = 2.5, alpha = 0.7, lineend = "round") +
  geom_point(size = 4, stroke = 2.5, shape = 1, alpha=0.8) +
  scale_color_manual(values = c("TRUE" = "#aa3951", "FALSE" = "#2f3941"),
                     name = "p < 0.05") +
  labs(title = "Association with Delta PDQ-8 [Deltas Linear Model]\n",
       x = "\n Coefficient [95% CI]",
       y = "") +
  theme_minimal() +
  coord_cartesian(xlim=c(-3,3)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust=-0.1),
        axis.text.y = element_text(size = 12, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

print(forest_plot_color)

ggsave(file = "../out/forest_plot_2.svg", plot = forest_plot_color, width = 8, height = 4)






# Check if treatment affects change in PFS
t.test(delta_pfs ~ TREATMENT, data = delta_df)

# Check if treatment affects change in FREESC
t.test(delta_FREESC ~ TREATMENT, data = delta_df)

# Check if treatment affects change in FREESC
t.test(delta_MDS26 ~ TREATMENT, data = delta_df)

# Check if treatment affects change in FREESC
t.test(delta_MDS27 ~ TREATMENT, data = delta_df)

t.test(delta_UDYSCTOT  ~ TREATMENT, data = delta_df)

t.test(delta_FREEZ3  ~ TREATMENT, data = delta_df)



# Linear models for treatment → mediators
model_pfs <- lm(delta_pfs ~ TREATMENT, data = delta_df)
summary(model_pfs)

model_freesc <- lm(delta_FREESC ~ TREATMENT, data = delta_df)
summary(model_freesc)

# Also check treatment → PDQ-8 (total effect)
model_pdq8 <- lm(delta_pdq8 ~ TREATMENT, data = delta_df)
summary(model_pdq8)




plot <- delta_df %>% 
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = delta_UDYSCTOT, y = delta_pdq8 , fill = Treatment, colour = Treatment)) +
  geom_jitter(height=0.1, width=0.4, shape=1, size=2, stroke=2) +
  geom_smooth(method="lm") +
  theme_minimal() +
  coord_cartesian(xlim=c(-10,30)) +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "Unified Dyskinesia Scores vs. PDQ8 Score Deltas",
    x = "\n 18-month Unified Dyskinesia Scores delta",
    y = "18-month PDQ8 delta\n",
    fill = "Treatment", colour="Treatment"
  )

plot

ggsave(file = "../out/delta_UDYSCTOT.svg", plot = plot, width = 5, height = 5)




plot <- delta_df %>% 
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = delta_EFNM_total, y = delta_pdq8, fill = Treatment, colour = Treatment)) +
  geom_jitter(height=0.1, width=0.4, shape=1, size=2, stroke=2) +
  geom_smooth(method="lm") +
  theme_minimal() +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "EFNM vs. PDQ8 Score Deltas",
    x = "\n 18-month EFNM delta",
    y = "18-month PDQ8 delta\n",
    fill = "Treatment", colour="Treatment"
  )

plot

ggsave(file = "../out/delta_efnm.svg", plot = plot, width = 5, height = 5)


plot <- delta_df %>% 
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = delta_FREESC , y = delta_pdq8, fill = Treatment, colour = Treatment)) +
  geom_jitter(height=0.1, width=0.4, shape=1, size=2, stroke=2) +
  geom_smooth(method="lm") +
  theme_minimal() +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "Freezing Q vs. PDQ8 Score Deltas",
    x = "\n 18-month Freezing Q delta",
    y = "18-month PDQ8 delta\n",
    fill = "Treatment", colour="Treatment"
  )

plot

ggsave(file = "../out/delta_feez.svg", plot = plot, width = 5, height = 5)



plot <- delta_df %>% 
  rename("Treatment"="TREATMENT") %>%
  ggplot(aes(x = delta_pfs  , y = delta_pdq8, fill = Treatment, colour = Treatment)) +
  geom_jitter(height=0.1, width=0.4, shape=1, size=2, stroke=2) +
  geom_smooth(method="lm") +
  theme_minimal() +
  scale_fill_manual(values = c("#aa3951", "#2f3941")) +
    scale_colour_manual(values = c("#aa3951", "#2f3941")) +
   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "Fatigue vs. PDQ8 Score Deltas",
    x = "\n 18-month Fatigue delta",
    y = "18-month PDQ8 delta\n",
    fill = "Treatment", colour="Treatment"
  )

plot

ggsave(file = "../out/delta_fatigue.svg", plot = plot, width = 5, height = 5)












library(mediation)

# Prepare data with complete cases
mediation_data <- delta_df %>%
  drop_na(delta_pdq8, delta_FREESC, delta_pfs, delta_UDYSCTOT , TREATMENT)

# MEDIATION FOR FREESC
# Step 1: Mediator model (Treatment → ΔFREESC)
med_pfs <- lm(delta_pfs  ~ TREATMENT, data = mediation_data)

summary(med_pfs)
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       -0.3827     0.4160   -0.92   0.3589  
# TREATMENTPlacebo   1.4262     0.5705    2.50   0.0134 *
#   
# Step 2: Outcome model (Treatment + Mediator → ΔPDQ8)
out_pfs <- lm(delta_pdq8 ~ TREATMENT + delta_pfs , data = mediation_data)

summary(out_pfs)


# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.70622    0.42344  -1.668   0.0972 .  
# TREATMENTPlacebo  0.42871    0.58972   0.727   0.4682    
# delta_FREESC      0.38053    0.07765   4.901 2.21e-06 ***
  
# Step 3: Run mediation
mediation_freesc <- mediate(med_pfs, out_pfs, 
                            treat = "TREATMENT", 
                            mediator = "delta_pfs ",
                            sims = 1000)

summary(mediation_freesc)

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value  
# ACME             0.5356       0.0953         1.06   0.026 *
# ADE              0.4221      -0.7053         1.55   0.446  
# Total Effect     0.9577      -0.2888         2.20   0.140  
# Prop. Mediated   0.5012      -3.5113         3.05   0.150  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Sample Size Used: 173 
# 
# 
# Simulations: 1000 



# MEDIATION FOR PFS
med_pfs <- lm(delta_pfs ~ TREATMENT, data = mediation_data)
out_pfs <- lm(delta_pdq8 ~ TREATMENT + delta_pfs, data = mediation_data)

mediation_pfs <- mediate(med_pfs, out_pfs, 
                         treat = "TREATMENT", 
                         mediator = "delta_pfs",
                         sims = 1000)

summary(mediation_pfs)

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME             0.3083      -0.0653         0.76    0.11
# ADE              0.6434      -0.5876         1.83    0.28
# Total Effect     0.9518      -0.2384         2.23    0.13
# Prop. Mediated   0.2809      -1.5480         2.25    0.21
# 
# Sample Size Used: 173 
# 
# 
# Simulations: 1000 




# MEDIATION FOR UDYSCTOT
med_UDYSCTOT  <- lm(delta_UDYSCTOT  ~ TREATMENT, data = mediation_data)
out_pfs <- lm(delta_pdq8 ~ TREATMENT + delta_pfs, data = mediation_data)

mediation_UDYSCTOT <- mediate(med_UDYSCTOT, out_pfs, 
                         treat = "TREATMENT", 
                         mediator = "delta_UDYSCTOT",
                         sims = 1000)

summary(mediation_pfs)

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME             0.3083      -0.0653         0.76    0.11
# ADE              0.6434      -0.5876         1.83    0.28
# Total Effect     0.9518      -0.2384         2.23    0.13
# Prop. Mediated   0.2809      -1.5480         2.25    0.21
# 
# Sample Size Used: 173 
# 
# 
# Simulations: 1000 




# Prepare data with complete cases
mediation_data <- delta_df %>%
  drop_na(delta_pdq8, delta_FREESC, delta_pfs, TREATMENT)



# MEDIATION FOR FREESC
# Step 1: Mediator model (Treatment → ΔFREESC)
med_freesc <- lm(delta_FREESC ~ TREATMENT, data = mediation_data)

summary(med_freesc)
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       -0.3827     0.4160   -0.92   0.3589  
# TREATMENTPlacebo   1.4262     0.5705    2.50   0.0134 *
#   
# Step 2: Outcome model (Treatment + Mediator → ΔPDQ8)
out_freesc <- lm(delta_pdq8 ~ TREATMENT + delta_FREESC, data = mediation_data)

summary(out_freesc)


# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.70622    0.42344  -1.668   0.0972 .  
# TREATMENTPlacebo  0.42871    0.58972   0.727   0.4682    
# delta_FREESC      0.38053    0.07765   4.901 2.21e-06 ***
  
# Step 3: Run mediation
mediation_freesc <- mediate(med_freesc, out_freesc, 
                            treat = "TREATMENT", 
                            mediator = "delta_FREESC",
                            sims = 1000)

summary(mediation_freesc)

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value  
# ACME             0.5356       0.0953         1.06   0.026 *
# ADE              0.4221      -0.7053         1.55   0.446  
# Total Effect     0.9577      -0.2888         2.20   0.140  
# Prop. Mediated   0.5012      -3.5113         3.05   0.150  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Sample Size Used: 173 
# 
# 
# Simulations: 1000 



# MEDIATION FOR PFS
med_pfs <- lm(delta_pfs ~ TREATMENT, data = mediation_data)
out_pfs <- lm(delta_pdq8 ~ TREATMENT + delta_pfs, data = mediation_data)

mediation_pfs <- mediate(med_pfs, out_pfs, 
                         treat = "TREATMENT", 
                         mediator = "delta_pfs",
                         sims = 1000)

summary(mediation_pfs)

# Causal Mediation Analysis 
# 
# Quasi-Bayesian Confidence Intervals
# 
#                Estimate 95% CI Lower 95% CI Upper p-value
# ACME             0.3083      -0.0653         0.76    0.11
# ADE              0.6434      -0.5876         1.83    0.28
# Total Effect     0.9518      -0.2384         2.23    0.13
# Prop. Mediated   0.2809      -1.5480         2.25    0.21
# 
# Sample Size Used: 173 
# 
# 
# Simulations: 1000 



# Test interaction between treatment and freezing
model_interaction <- lm(delta_pdq8 ~ TREATMENT * delta_FREESC, data = mediation_data)
summary(model_interaction)



# Extract simple slopes for each group
library(emmeans)
emtrends(model_interaction, ~ TREATMENT, var = "delta_FREESC")



library(ggplot2)

ggplot(mediation_data, aes(x = delta_FREESC, y = delta_pdq8, color = TREATMENT)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Amantadine" = "#aa3951", "Placebo" = "#2f3941")) +
  labs(
    title = "Relationship between ΔFREESC and ΔPDQ-8 by Treatment Group",
    x = "ΔFREESC (Change in Freezing)",
    y = "ΔPDQ-8 (Change in Quality of Life)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
