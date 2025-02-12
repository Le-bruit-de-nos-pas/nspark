library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)

PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "PPMI_Curated_Data_Cut_Public_20241211.xlsx")

names(PPMI_Curated_Data_Cut_Public_20241211)

# PPMI patients 

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 3866

PPMI_Curated_Data_Cut_Public_20241211$PATNO <- as.numeric(PPMI_Curated_Data_Cut_Public_20241211$PATNO)

#  PD only
PPMI_Curated_Data_Cut_Public_20241211 %>% select(COHORT) %>% distinct()

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 
  
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1441

#  Known H&Y >0

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(hy_on!="." & hy_on!="0" ) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1422

# Known disease duration <5 at baseline

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(YEAR==0) %>%
  filter(duration_yrs<=5) %>% select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1345

# >1 visit

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  group_by(PATNO) %>% count() %>% filter(n>1) %>% ungroup() %>%
  select(PATNO) %>% distinct() %>%
  left_join(PPMI_Curated_Data_Cut_Public_20241211)

length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO)) # 1086

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  select(PATNO, visit_date, YEAR, duration_yrs, hy_on)





# Levodopa status

LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

Levodopa_lookups <- LEDD_Concomitant_Medication_Log_12Feb2025 %>% select(LEDTRT) %>% distinct()

fwrite(Levodopa_lookups, "Levodopa_lookups.csv")

# Freezing

Determination_of_Freezing_and_Falls_12Feb2025 <- fread("Medical/Determination_of_Freezing_and_Falls_12Feb2025.csv")

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  select(PATNO, PATNO, EVENT_ID,  INFODT , FRZGT12M , FRZGT1W)

Determination_of_Freezing_and_Falls_12Feb2025$PATNO <- as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$PATNO)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT12M), na.rm=T)

mean(as.numeric(Determination_of_Freezing_and_Falls_12Feb2025$FRZGT1W), na.rm=T)

Determination_of_Freezing_and_Falls_12Feb2025 %>% group_by(FRZGT1W) %>% count() 



Determination_of_Freezing_and_Falls_12Feb2025 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  left_join(Determination_of_Freezing_and_Falls_12Feb2025, by=c("PATNO"="PATNO", "visit_date"="INFODT"))

Determination_of_Freezing_and_Falls_12Feb2025 <- Determination_of_Freezing_and_Falls_12Feb2025 %>% 
  mutate(disease_duration=duration_yrs+YEAR)

Determination_of_Freezing_and_Falls_12Feb2025 %>%
  mutate(FRZGT1W=ifelse(FRZGT1W==0,0,1)) %>%
  # filter(YEAR == 0 | YEAR == 1) %>%
  group_by(YEAR) %>% summarise(mean=mean(FRZGT1W, na.rm=T))


Determination_of_Freezing_and_Falls_12Feb2025 %>%
  ggplot(aes(disease_duration, FRZGT1W)) +
  geom_smooth()
