

library(haven)
library(tidyverse)
library(data.table)
library(lubridate)

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

efnm_2 <- efnm_2 %>% select(SUBJID, VISIT, EFNM_total, NMF_bin, 
                            NMF_cognitive, NMF_cognitive_bin, 
                            NMF_dysautonomia, NMF_dysautonomia_bin, 
                            NMF_pain, NMF_pain_bin)


dysk_pats <- fread("../out/dysk_pats.txt")


dysk_pats <- dysk_pats %>% select(-ADMREMNU, GRP)




df <- efnm_2 %>% mutate(SUBJID=as.numeric(SUBJID)) %>% inner_join(dysk_pats )  


df <- df %>% select(SUBJID, VISIT, EFNM_total  , MDS68, TREATMENT) %>%
  mutate(EFNM_total=as.numeric(EFNM_total) )
 
df_long <- df %>%
  arrange(SUBJID, VISIT) %>%
  group_by(SUBJID) %>%
  mutate(
    start = lag(VISIT, default = 0),
    stop  = VISIT,
    # Events (occur at END of interval)
    NMF_event  = EFNM_total ,
    dysk_event = MDS68,
    # Lagged exposures (state during interval)
    NMF_lag  = lag(EFNM_total , default = 0),
    dysk_lag = lag(MDS68, default = 0)
  ) %>%
  ungroup()



df_long <- df_long %>% filter(stop > 0)


library(survival)

# Does NMF bin increase risk of later dyskinesia?

cox_dysk <- coxph(
  Surv(start, stop, dysk_event) ~ NMF_lag ,
  data = df_long
)

summary(cox_dysk)


