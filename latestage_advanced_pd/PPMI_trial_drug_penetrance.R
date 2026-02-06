library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


LEDD_Concomitant_Medication_Log_12Feb2025 <- fread("Medical/LEDD_Concomitant_Medication_Log_12Feb2025.csv")

uniques <- data.frame(unique(LEDD_Concomitant_Medication_Log_12Feb2025$LEDTRT))

fwrite(uniques, "unique_LEDD_Concomitant_Medication_v2.txt")


# Read data
drug_data <- read.table("unique_LEDD_Concomitant_Medication_v2.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

colnames(drug_data) <- "Therapy"










drug_class_map <- list(
  # Levodopa and combinations
  "levodopa" = "Levodopa",
  "carbidopa" = "Levodopa",
  "sinemet" = "Levodopa",
  "sinement" = "Levodopa",
  "rytary" = "Levodopa",
  "madopar" = "Levodopa",
  "inbrija" = "Levodopa",
  "duopa" = "Intestinal levodopa Gel",
  "duodopa" = "Intestinal levodopa Gel",
  "isicom" = "Levodopa",
  "levocomp" = "Levodopa",
  "prolopa" = "Levodopa",
  "nacom" = "Levodopa",
  "dopicar" = "Levodopa",
  "levopar" = "Levodopa",
  "co-careldopa" = "Levodopa",
  "l-dopa" = "Levodopa",
  "levodapa" = "Levodopa",
  "levidopa" = "Levodopa",
  "cabidopa" = "Levodopa",
  "carb/levo" = "Levodopa",
  "ledopoda" = "Levodopa",
  "levodop" = "Levodopa",
  "co-carledopa" = "Levodopa",
  "levodpa" = "Levodopa",
  "azilekt" = "MAO Inhibitor",

  # MAO Inhibitors
  "selegiline" = "MAO Inhibitor",
  "selegline" = "MAO Inhibitor",
  "rasagiline" = "MAO Inhibitor",
  "rasigiline" = "MAO Inhibitor",
  "azilect" = "MAO Inhibitor",
  "safinamide" = "MAO Inhibitor",
  "xadago" = "MAO Inhibitor",
  "eldepryl" = "MAO Inhibitor",
  "rasagilina" = "MAO Inhibitor",
  "selegelina" = "MAO Inhibitor",
  "rasagilima" = "MAO Inhibitor",
  "rasagilene" = "MAO Inhibitor",
  "rasagalin" = "MAO Inhibitor",
  "rasalgiline" = "MAO Inhibitor",
  "rasageline" = "MAO Inhibitor",
  "selegelin" = "MAO Inhibitor",
  "selegelene" = "MAO Inhibitor",
  "selegeline" = "MAO Inhibitor",
  "selegrine" = "MAO Inhibitor",

  # Dopamine Agonists
  "ropinirole" = "Dopamine Agonist",
  "ropinirol" = "Dopamine Agonist",
  "pramipexole" = "Dopamine Agonist",
  "mirapex" = "Dopamine Agonist",
  "requip" = "Dopamine Agonist",
  "rotigotine" = "Dopamine Agonist",
  "neupro" = "Dopamine Agonist",
  "sifrol" = "Dopamine Agonist",
  "ropinerole" = "Dopamine Agonist",
  "ropinirone" = "Dopamine Agonist",
  "pramiprexole" = "Dopamine Agonist",
  "pramiprexol" = "Dopamine Agonist",
  "ropirinol" = "Dopamine Agonist",
  "ropinrole" = "Dopamine Agonist",
  "ropinirone" = "Dopamine Agonist",
  "rotigotina" = "Dopamine Agonist",
  "rotigine" = "Dopamine Agonist",
  "leganto" = "Dopamine Agonist",
  "clarium" = "Dopamine Agonist",
  "piribedil" = "Dopamine Agonist",
  "pramipexol" = "Dopamine Agonist",
  "prampexole" = "Dopamine Agonist",
  "neurpro" = "Dopamine Agonist",
  "nupro" = "Dopamine Agonist",

  # Amantadine
  "amantadine" = "Amantadine",
  "amantdine" = "Amantadine",
  "gocovri" = "Amantadine",
  "osmolex" = "Amantadine",
  "pk-merz" = "Amantadine",
  "pk merz" = "Amantadine",
  "amandtadine" = "Amantadine",
  "amandatine" = "Amantadine",
  "amandatin" = "Amantadine",
  "amantadin" = "Amantadine",
  "amandadina" = "Amantadine",
  "amandadin" = "Amantadine",

  # COMT Inhibitors
  "entacapone" = "COMT inhibitor",
  "comtan" = "COMT inhibitor",
  "ongentys" = "COMT inhibitor",
  "opicapone" = "COMT inhibitor",
  "opicapon" = "COMT inhibitor",
  "entacapon" = "COMT inhibitor",
  "entacapona" = "COMT inhibitor",
  "entacapon" = "COMT inhibitor",
  "entacaopne" = "COMT inhibitor",
  "entacaprone" = "COMT inhibitor",
  "entcapone" = "COMT inhibitor",
  "tasmar" = "COMT inhibitor",
  "tolcapone" = "COMT inhibitor",
  "stalevo" = "Levodopa/COMT inhibitor",

  # Subcutaneous Apomorphine
  "apomorphine" = "Subcutaneous Apomorphine",
  "apokyn" = "Subcutaneous Apomorphine",
  "apo-go" = "Subcutaneous Apomorphine",
  "kynmobi" = "Subcutaneous Apomorphine",
  "apomorfin" = "Subcutaneous Apomorphine",

  # Other
  "istradefylline" = "Other",
  "zonegran" = "Other"
)


drug_class_map


# Map function
map_therapy_to_class <- function(therapy) {
  hits <- sapply(names(drug_class_map), function(keyword) {
    if (str_detect(tolower(therapy), keyword)) return(drug_class_map[[keyword]]) else return(NA)
  })
  hits <- na.omit(hits)
  if (length(hits) == 0) return(NA)
  return(paste(unique(hits), collapse = "/"))
}


# Apply mapping
drug_data$Class <- sapply(drug_data$Therapy, map_therapy_to_class)


LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  select(REC_ID, PATNO, EVENT_ID, LEDTRT, STARTDT, STOPDT, ORIG_ENTRY) %>%
  left_join(drug_data, by=c("LEDTRT"="Therapy"))

LEDD_Concomitant_Medication_Log_12Feb2025 %>% group_by(Class) %>% count() %>%
  arrange(-n)

LEDD_Concomitant_Medication_Log_12Feb2025 <- LEDD_Concomitant_Medication_Log_12Feb2025 %>%
  mutate(ORIG_ENTRY=as.Date(paste("01/", as.character(ORIG_ENTRY)), "%d/%m/%Y"))  %>%
  mutate(STARTDT=as.Date(paste("01/", as.character(STARTDT)), "%d/%m/%Y"))  %>%
  mutate(STOPDT=as.Date(paste("01/", as.character(STOPDT)), "%d/%m/%Y")) 


unique(LEDD_Concomitant_Medication_Log_12Feb2025$Class)


PPMI_Curated_Data_Cut_Public_20241211 <- read_excel(path = "ppmi_docs_zips/PPMI_Curated_Data_Cut_Public_20241211.xlsx")

unique(PPMI_Curated_Data_Cut_Public_20241211$subgroup)
length(unique(PPMI_Curated_Data_Cut_Public_20241211$PATNO))

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(COHORT==1) 

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>% select(duration_yrs, hy_on, PATNO, visit_date)

PPMI_Curated_Data_Cut_Public_20241211 <- PPMI_Curated_Data_Cut_Public_20241211 %>%
  mutate(visit_date=as.Date(paste("01/", as.character(visit_date)), "%d/%m/%Y")) 


data <- PPMI_Curated_Data_Cut_Public_20241211 %>% filter(duration_yrs>=7 & hy_on>=4 & hy_on<=5) %>%
  select(PATNO, visit_date) %>%
  select(PATNO) %>% distinct() %>% count() # 77
  inner_join(LEDD_Concomitant_Medication_Log_12Feb2025 %>% mutate(PATNO=as.character(PATNO))) 

data %>% filter(grepl("Agonist", Class)) %>% filter(visit_date>=STARTDT) %>%
  filter(is.na(STOPDT) | visit_date<=STOPDT) %>% select(PATNO) %>% distinct() 
