
library(haven)
library(tidyverse)
library(data.table)
library(lubridate)

admin <- read_sas("../data/admin.sas7bdat")

rando <- read_sas("../data/rando.sas7bdat")

treat_pats_groups <- admin %>% select(SUBJID, ADMREMNU) %>% drop_na() %>% distinct() %>%
  left_join(rando %>% select(RDNUM, GRP) %>% distinct(), by=c("ADMREMNU"="RDNUM")) %>%
  mutate(TREATMENT=ifelse(GRP=="A", "Amantadine", "Placebo"))

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

ecmp <- ecmp %>%   mutate(ecmp_nmf = ECMP8 + ECMP9) %>%
  select(SUBJID, VISIT, ecmp_hypodopa, ecmp_icd_binary, ecmp_icd_severity, ecmp_icb_binary, ecmp_icb_severity, ecmp_nmf)

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

efnm_2 <- efnm_2 %>% select(SUBJID, VISIT, EFNM_total, NMF_bin, NMF_cognitive, NMF_cognitive_bin,
                            NMF_dysautonomia, NMF_dysautonomia_bin, NMF_pain, NMF_pain_bin)

efnm_2$NMF_bin <- as.numeric(efnm_2$NMF_bin)
efnm_2$NMF_cognitive_bin  <- as.numeric(efnm_2$NMF_cognitive_bin )
efnm_2$NMF_dysautonomia_bin  <- as.numeric(efnm_2$NMF_dysautonomia_bin )
efnm_2$NMF_pain_bin <- as.numeric(efnm_2$NMF_pain_bin)


global_df <- ecmp %>% left_join(efnm_2)


global_df %>% group_by(SUBJID, VISIT) %>% count() %>% filter(n!=1)


global_df <- treat_pats_groups %>% select(SUBJID, TREATMENT) %>%
  left_join(global_df)


lars <- read_sas("../data/lars.sas7bdat")

lars <- lars %>% select(SUBJID, VISIT, LARSCORE)

lars <- lars %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))

lars <- lars %>% drop_na() %>%
  mutate(VISIT=ifelse(VISIT==2,0,
                      ifelse(VISIT==3, 3, 
                             ifelse(VISIT==5,9,18))))

lars <- lars %>% mutate(LARS_apathetic = ifelse(LARSCORE >= -21, 1, 0))

lars <- lars %>% group_by(SUBJID, VISIT) %>% summarise(LARS_apathetic=max(LARS_apathetic), LARSCORE =min(LARSCORE ))

global_df <- global_df %>% left_join(lars)


mds <- read_sas("../data/mds.sas7bdat")

global_df <- global_df %>% left_join( 
  mds %>% select(SUBJID, VISIT, MDS68) %>% 
    filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
    mutate(VISIT=ifelse(VISIT==2,0,
                        ifelse(VISIT==3, 3, 
                               ifelse(VISIT==5,9,18)))) %>%
    drop_na() %>%
    group_by(SUBJID, VISIT) %>% summarise(MDS68=max(MDS68)) %>%
    mutate(MDS68=ifelse(MDS68>0,1,0)) 
)

global_df <- global_df %>% left_join( 
  mds %>% filter(VISIT<=8|VISIT==16) %>% filter(VISIT>=2) %>% mutate(VISIT=ifelse(VISIT==16,8,VISIT))  %>%
    mutate(VISIT=ifelse(VISIT==2,0,
                        ifelse(VISIT==3, 3, 
                               ifelse(VISIT==5,9,18)))) %>%
    select(SUBJID, VISIT, MDS32:MDS64) %>% drop_na() %>%
    mutate(mds_III_total = rowSums(select(.,MDS32:MDS64))) %>%
    select(SUBJID, VISIT, mds_III_total) %>%
    group_by(SUBJID, VISIT) %>% summarise(mds_III_total=max(mds_III_total))
)


global_df %>% group_by(VISIT, TREATMENT) %>% count()



demog <- read_sas("../data/demog.sas7bdat")
demog$SEX <- as.factor(demog$SEX)


ecmp <- read_sas("../data/ecmp.sas7bdat")

demog <- demog %>% left_join(ecmp %>% select(SUBJID, VISIT, ECMPDT) %>% filter(VISIT==1)) %>%
  select(SUBJID, SEX, BIRTHDT, ECMPDT) 

demog$years <- as.numeric(
  difftime(demog$ECMPDT, demog$BIRTHDT, 
           unit="weeks")
)/52.25

global_df <- global_df %>% left_join(demog %>% select(SUBJID, SEX, years))


visig <- read_sas("../data/visig.sas7bdat")
visig <- visig %>% filter(VISIT==1) %>% select(SUBJID, WEIG, TAILLE)
visig <- visig %>% mutate(imc = WEIG/ ((TAILLE/100)**2) )

global_df <- global_df %>% left_join(visig %>% select(SUBJID, imc))                     


concmed <- read_sas("../data/concmed.sas7bdat")


names(concmed)
concmed <- concmed %>% filter(ATC2DEC=="ANTI-PARKINSON DRUGS") 

concmed <- concmed %>% select(SUBJID, CMLBL, WHOCD, WHODEC, PNDEC, ATCDEC, ATC3DEC, ATC2DEC,  ATC1DEC, CMPOS, CMSTDT, CMENDT, CMONGYN)
concmed <- concmed %>% select(SUBJID, CMLBL, WHOCD, WHODEC, PNDEC, ATCDEC, ATC3DEC,  CMPOS, CMSTDT, CMENDT, CMONGYN)

temp <- concmed %>% select(-SUBJID, -CMPOS, -CMSTDT, -CMENDT, -CMONGYN, -WHOCD) %>% distinct()

#fwrite(temp, "temp.csv")
#fwrite(concmed, "concmed.csv")

concmed <- concmed %>% arrange(CMPOS)

concmed <- fread("concmed.csv")

temp <- fread("temp.csv")

concmed <- concmed %>% left_join(temp)

dopa_agos <- concmed %>% select(SUBJID, ATCDEC )  %>% distinct()


dopa_agos <- dopa_agos %>% filter(ATCDEC=="DOPAMINE AGONISTS") %>% mutate(DopaAges=1) %>% select(-ATCDEC)

length(unique(dopa_agos$SUBJID)) 





concmed <- concmed %>% mutate(ledd_total=LEDD*DOSE) %>% select(SUBJID, ledd_total, CMSTDT, CMENDT, CMONGYN )

concmed %>% filter(CMONGYN==1) %>% group_by(SUBJID) %>% summarise(tot=sum(ledd_total, na.rm=T)) %>%
  ungroup() %>% summarise(mean=mean(tot, na.rm=T), sd=sd(tot, na.rm=T)) 

global_df %>% filter(VISIT==0) %>% select(SUBJID, TREATMENT)


ecmp <- read_sas("../data/ecmp.sas7bdat")

ecmp <- ecmp %>% filter(VISIT==2)  %>% select(SUBJID, ECMPDT)

concmed <- concmed %>%
  mutate(
    # Convert CMSTDT from DD/MM/YYYY to Date
    CMSTDT = dmy(CMSTDT),
    # Convert CMENDT from DD/MM/YYYY to Date
    CMENDT = dmy(CMENDT)
  )

concmed <- concmed %>% left_join(ecmp %>% mutate(SUBJID=as.numeric(SUBJID))) %>%
  filter(CMSTDT <= ECMPDT & CMENDT >= ECMPDT) 


global_df <- global_df %>% mutate(SUBJID=as.numeric(SUBJID)) %>%
  left_join(
    concmed %>%  group_by(SUBJID) %>% 
      summarise(ledd_total=sum(ledd_total, na.rm=T))
  )

global_df <- global_df %>% mutate(ledd_total=ifelse(is.na(ledd_total),0,ledd_total))



global_df <- global_df %>% mutate(SUBJID=as.numeric(SUBJID)) %>%
  left_join(
    concmed %>%  group_by(SUBJID) %>% 
      summarise(ledd_total=sum(ledd_total, na.rm=T))
  )


global_df <- global_df %>% left_join(dopa_agos) %>% mutate(DopaAges =ifelse(is.na(DopaAges ),0,DopaAges ))

fwrite(global_df, "global_df.csv")

global_df <- fread("global_df.csv")

table(global_df$DopaAges)








global_df <- global_df %>% mutate(SUBJID=as.factor(SUBJID))
names(global_df)

sum(is.na(global_df$MDS68))



# Fix NA issues - replace -Inf with NA and handle missing values
patient_df <- global_df %>%
  group_by(SUBJID, TREATMENT, SEX, years, imc) %>%
  summarise(
    MDS68_ever = ifelse(all(is.na(MDS68)), NA, max(MDS68, na.rm = TRUE)),
    
    # Add na.rm = TRUE and handle cases with all NAs
    ecmp_hypodopa_max = ifelse(all(is.na(ecmp_hypodopa)), NA, max(ecmp_hypodopa, na.rm = TRUE)),
    ecmp_icd_binary_max = ifelse(all(is.na(ecmp_icd_binary)), NA, max(ecmp_icd_binary, na.rm = TRUE)),
    ecmp_icd_severity_max = ifelse(all(is.na(ecmp_icd_severity)), NA, max(ecmp_icd_severity, na.rm = TRUE)),
    ecmp_icb_binary_max = ifelse(all(is.na(ecmp_icb_binary)), NA, max(ecmp_icb_binary, na.rm = TRUE)),
    ecmp_icb_severity_max = ifelse(all(is.na(ecmp_icb_severity)), NA, max(ecmp_icb_severity, na.rm = TRUE)),
    ecmp_nmf_max = ifelse(all(is.na(ecmp_nmf)), NA, max(ecmp_nmf, na.rm = TRUE)),
    EFNM_total_max = ifelse(all(is.na(EFNM_total)), NA, max(EFNM_total, na.rm = TRUE)),
    NMF_bin_max = ifelse(all(is.na(NMF_bin)), NA, max(NMF_bin, na.rm = TRUE)),
    NMF_cognitive_max = ifelse(all(is.na(NMF_cognitive)), NA, max(NMF_cognitive, na.rm = TRUE)),
    NMF_cognitive_bin_max = ifelse(all(is.na(NMF_cognitive_bin)), NA, max(NMF_cognitive_bin, na.rm = TRUE)),
    NMF_dysautonomia_max = ifelse(all(is.na(NMF_dysautonomia)), NA, max(NMF_dysautonomia, na.rm = TRUE)),
    NMF_dysautonomia_bin_max = ifelse(all(is.na(NMF_dysautonomia_bin)), NA, max(NMF_dysautonomia_bin, na.rm = TRUE)),
    NMF_pain_max = ifelse(all(is.na(NMF_pain)), NA, max(NMF_pain, na.rm = TRUE)),
    NMF_pain_bin_max = ifelse(all(is.na(NMF_pain_bin)), NA, max(NMF_pain_bin, na.rm = TRUE)),
    LARS_apathetic_max = ifelse(all(is.na(LARS_apathetic)), NA, max(LARS_apathetic, na.rm = TRUE)),
    LARSCORE_max = ifelse(all(is.na(LARSCORE)), NA, max(LARSCORE, na.rm = TRUE)),
    mds_III_total_max = ifelse(all(is.na(mds_III_total)), NA, max(mds_III_total, na.rm = TRUE)),
    ledd_total_max = ifelse(all(is.na(ledd_total)), NA, max(ledd_total, na.rm = TRUE)),
    DopaAges_max = ifelse(all(is.na(DopaAges)), NA, max(DopaAges, na.rm = TRUE)),
    
    .groups = "drop"
  )

# Check outcome distribution
table(patient_df$MDS68_ever)

sum(is.na(patient_df))




# Univariate logistic regression for each predictor
univariate_results <- data.frame()




# List of predictors to test
predictors <- c("TREATMENT", "SEX", "years", "imc", 
                "ecmp_hypodopa_max", "ecmp_icd_binary_max", 
                "ecmp_icd_severity_max", "ecmp_icb_binary_max", 
                "ecmp_icb_severity_max", "ecmp_nmf_max", "EFNM_total_max",
                "NMF_bin_max", "NMF_cognitive_max", "NMF_cognitive_bin_max",
                "NMF_dysautonomia_max", "NMF_dysautonomia_bin_max",
                "NMF_pain_max", "NMF_pain_bin_max", "LARS_apathetic_max",
                "LARSCORE_max", "mds_III_total_max", "ledd_total_max", "DopaAges_max")


names(patient_df)


# Check the structure of patient_df to understand what predictors we have
str(patient_df)

# Check for constant variables (no variation)
constant_vars <- sapply(patient_df, function(x) {
  if(is.numeric(x)) length(unique(na.omit(x))) == 1
  else if(is.factor(x)) length(unique(na.omit(x))) == 1
  else FALSE
})

print("Constant variables (no variation):")
names(patient_df)[constant_vars]

# Check number of events
print(paste("Number of events (MDS68_ever=1):", sum(patient_df$MDS68_ever, na.rm = TRUE)))
print(paste("Number of non-events:", sum(patient_df$MDS68_ever == 0, na.rm = TRUE)))

# Check each predictor's variation
for(pred in predictors) {
  if(pred %in% names(patient_df)) {
    unique_vals <- length(unique(na.omit(patient_df[[pred]])))
    if(unique_vals > 1) {
      print(paste(pred, ":", unique_vals, "unique values"))
    } else {
      print(paste(pred, "CONSTANT - skipping"))
    }
  } else {
    print(paste(pred, "NOT IN DATAFRAME"))
  }
}




# Univariate logistic regression with error handling
univariate_results <- data.frame()








# List of predictors to test
predictors <- c("TREATMENT", "SEX", "years", "imc", 
                "ecmp_hypodopa_max", "ecmp_icd_binary_max", 
                "ecmp_icd_severity_max", "ecmp_icb_binary_max", 
                "ecmp_icb_severity_max", "ecmp_nmf_max", "EFNM_total_max",
                "NMF_bin_max", "NMF_cognitive_max", "NMF_cognitive_bin_max",
                "NMF_dysautonomia_max", "NMF_dysautonomia_bin_max",
                "NMF_pain_max", "NMF_pain_bin_max", "LARS_apathetic_max",
                "LARSCORE_max", "mds_III_total_max", "ledd_total_max", "DopaAges_max")

# Run univariate analysis for each predictor
for(pred in predictors) {
  # Skip if predictor has no variation or is all NA
  if(pred %in% names(patient_df)) {
    if(all(is.na(patient_df[[pred]]))) {
      next
    }
    if(length(unique(na.omit(patient_df[[pred]]))) < 2) {
      next
    }
  }
  
  formula <- as.formula(paste("MDS68_ever ~", pred))
  
  # Try to fit the model
  model <- tryCatch(
    glm(formula, data = patient_df, family = binomial),
    error = function(e) NULL
  )
  
  if(is.null(model)) next
  
  # Check if model converged
  if(!model$converged) next
  
  coef_summary <- summary(model)$coefficients
  
  # For categorical variables with multiple levels
  if(pred %in% c("TREATMENT", "SEX") && nrow(coef_summary) > 1) {
    for(i in 2:nrow(coef_summary)) {
      univariate_results <- rbind(univariate_results, 
                                  data.frame(
                                    predictor = paste(pred, rownames(coef_summary)[i], sep = "_"),
                                    OR = exp(coef_summary[i, 1]),
                                    CI_lower = exp(coef_summary[i, 1] - 1.96 * coef_summary[i, 2]),
                                    CI_upper = exp(coef_summary[i, 1] + 1.96 * coef_summary[i, 2]),
                                    p_value = coef_summary[i, 4],
                                    n_obs = nobs(model),
                                    stringsAsFactors = FALSE
                                  ))
    }
  } 
  # For continuous variables or binary categorical
  else if(nrow(coef_summary) >= 2) {
    univariate_results <- rbind(univariate_results,
                                data.frame(
                                  predictor = pred,
                                  OR = exp(coef_summary[2, 1]),
                                  CI_lower = exp(coef_summary[2, 1] - 1.96 * coef_summary[2, 2]),
                                  CI_upper = exp(coef_summary[2, 1] + 1.96 * coef_summary[2, 2]),
                                  p_value = coef_summary[2, 4],
                                  n_obs = nobs(model),
                                  stringsAsFactors = FALSE
                                ))
  }
}

# Round results
univariate_results <- univariate_results %>%
  mutate(across(c(OR, CI_lower, CI_upper), ~ round(., 3)),
         p_value = round(p_value, 4)) %>%
  arrange(p_value)

# Display results
print("=== UNIVARIATE LOGISTIC REGRESSION RESULTS ===")
print(univariate_results)






# Significant predictors (p < 0.05)
significant_results <- univariate_results %>% 
  filter(p_value < 0.05) %>%
  arrange(p_value)

if(nrow(significant_results) > 0) {
  print("=== SIGNIFICANT PREDICTORS (p < 0.05) ===")
  print(significant_results)
} else {
  print("No significant predictors at p < 0.05")
  # Show predictors with p < 0.1
  marginal_results <- univariate_results %>% 
    filter(p_value < 0.1) %>%
    arrange(p_value)
  print("=== MARGINALLY SIGNIFICANT PREDICTORS (p < 0.1) ===")
  print(marginal_results)
}

patient_df <- patient_df %>% filter(!is.na(MDS68_ever))




plot <- ggplot(patient_df, aes(x = imc, y = MDS68_ever)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "#2f3941", fill = "#2f3941") +
  labs(x = "\n BMI (Body Mass Index)", 
       y = "Probability of Dyskinesia \n",
       title = "Probability of Dyskinesia ~ BMI") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave(file = "../out/bmi_dysk_prob.svg", plot = plot, width = 4, height = 4)


# Check if the relationship differs by treatment
plot <- ggplot(patient_df, aes(x = imc, y = MDS68_ever, color = TREATMENT, fill=TREATMENT)) +
  #geom_jitter(height = 0.05, width = 0, alpha = 0.5, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, level = 0.75) +
  scale_color_manual(values = c("Amantadine" = "#aa3951", "Placebo" = "#2f3941")) +
  scale_fill_manual(values = c("Amantadine" = "#aa3951", "Placebo" = "#2f3941")) +
  labs(x = "\n BMI", 
       y = "Probability of Dyskinesia \n",
       title = "Probability of Dyskinesia ~ \nBMI + Treatment") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

plot
ggsave(file = "../out/bmi_dysk_prob.svg", plot = plot, width = 4, height = 4)







