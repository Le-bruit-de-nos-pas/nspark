library(tidyverse) 
library(data.table)
library(readxl)

# Map individual drugs to drug classes for all patient-visits -----

data_i <- read_excel(path = "Inclusion_20250106.xlsx")

unique(data_i$diag) 

data_i <- data_i %>% filter(diag=="MP") 

length(unique(data_i$anonyme_id)) # 30330

names(data_i)

data_i <- data_i %>% select(anonyme_id,pat_ddn_a ,pat_sexe,diag_date_a)




data_v <- read_excel(path = "Consultation_20250106.xlsx")

data_v <- data_v %>% inner_join(data_i %>% select(anonyme_id))

data_v$ttt_ache <- ifelse(data_v$ttt_ache == "Non", "0", "1")

data_v <- data_v %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, ttt_novo___1:pompe_date_arret_2)


num_col <- colnames(data_v)

num_col <- as.data.frame(num_col)

num_col$num <- seq(1:251)

names_df <- data.frame(names(data_v))





# ttt_modopar_62_5_gel_yn___yes (10) -> every 4  -> until ttt_quetiapine_400_yn___yes (235)

                       
var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,
                       225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,
                       294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,
                       358,362,366,370,374,378,382,386,390,394,398,402) -  (167)

length(unique(data_v$anonyme_id)) # 25602


data_v <- data_v %>% select(anonyme_id, cible___2:cible___3, pompe_date:pompe_date_arret_2, all_of(var_traitement_yn))

names(data_v)


new_traitement <- data_v





# si le patient a pris le traitement A
IMAOB <- c("ttt_azil_rasag1_yn___yes","ttt_safinamide_50_yn___yes","ttt_safinamide_100_yn___yes"  ,"ttt_selegiline_yn___yes")
new_traitement$A <- rowSums(new_traitement[, names(new_traitement) %in% IMAOB], na.rm = TRUE)
new_traitement$A <- ifelse(new_traitement$A != 0, 1, 0)

# si le patient a pris le traitement B
Levodopa <- c("ttt_modopar_125_cpr_yn___yes","ttt_modopar_62_5_gel_yn___yes", "ttt_modopar_125_gel_yn___yes" , "ttt_modopar_250_gel_yn___yes","ttt_modopar_lp125_gel_yn___yes","ttt_sinemet_100_cpr_yn___yes" ,"ttt_sinemet_250_cpr_yn___yes" ,"ttt_sinemet_lp100_cpr_yn___yes" ,"ttt_sinemet_lp200_cpr_yn___yes" ,"ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
new_traitement$B <- rowSums(new_traitement[, names(new_traitement) %in% Levodopa], na.rm = TRUE)
new_traitement$B <- ifelse(new_traitement$B != 0, 1, 0)

# si le patient a pris le traitement C
Agonistes <- c( "ttt_neu_rot2_yn___yes","ttt_neu_rot4_yn___yes"       ,"ttt_neu_rot6_yn___yes","ttt_neu_rot8_yn___yes","ttt_ral_brom5_yn___yes"        ,"ttt_ral_brom10_yn___yes"    ,"ttt_ral_brom2_5m_yn___yes","req_rop2_yn___yes","ttt_req_rop4_yn___yes"         ,"ttt_req_rop8_yn___yes" ,"ttt_req_rop025_yn___yes","ttt_req_rop050_yn___yes","ttt_req_rop1_yn___yes","ttt_req_rop2_yn___yes","ttt_req_rop5_yn___yes", "ttt_sif_pram026_yn___yes", "ttt_sif_pram052_yn___yes"     ,"ttt_sif_pram105_yn___yes","ttt_sif_pram210_yn___yes","ttt_sif_pram018_yn___yes", "ttt_sif_pram070_yn___yes","ttt_triv_prim20_yn___yes"     ,"ttt_triv_per_lp50_yn___yes","ttt_apo_stylo_yn___yes")
new_traitement$C <- rowSums(new_traitement[, names(new_traitement) %in% Agonistes], na.rm = TRUE)
new_traitement$C <- ifelse(new_traitement$C != 0, 1, 0)

# si le patient a pris le traitement D
new_traitement$D <- new_traitement$ttt_amantadine_yn___yes

# si le patient a pris le traitement E
ICOMB <- c("ttt_comptan_entac_yn___yes","ttt_tasm_talc100_yn___yes","ttt_stalevo_50_cpr_yn___yes" , "ttt_stalevo_75_cpr_yn___yes" , "ttt_stalevo_100_cpr_yn___yes" ,"ttt_stalevo_125_cpr_yn___yes"  ,"ttt_stalevo_150_cpr_yn___yes" , "ttt_stalevo_175_cpr_yn___yes" ,"ttt_stalevo_200_cpr_yn___yes")
new_traitement$E <- rowSums(new_traitement[, names(new_traitement) %in% ICOMB], na.rm = TRUE)
new_traitement$E <- ifelse(new_traitement$E != 0, 1, 0)

# si le patient a pris le traitement Traitements Oraux
new_traitement$pompe <- ifelse(is.na(new_traitement$pompe_dose), 0,  ifelse((new_traitement$pompe_dose == "DM" & is.na(new_traitement$pompe_date)), 0,ifelse(!is.na(new_traitement$pompe_dose) & new_traitement$pompe_dose != 0, 1, 0)))
new_traitement$pompe_2 <- ifelse(is.na(new_traitement$pompe_dose_2), 0,  ifelse((new_traitement$pompe_dose_2 == "DM" & is.na(new_traitement$pompe_date_2)), 0,ifelse(!is.na(new_traitement$pompe_dose_2) & new_traitement$pompe_dose_2 != 0, 1, 0)))
new_traitement$TO <- ifelse(new_traitement$A == 1 | new_traitement$B == 1 | new_traitement$C == 1 | 
                              new_traitement$D == 1 | new_traitement$E == 1,1,0)

# si le patient a pris le traitement Stimulation Cérébrale Profonde
SCP <- c("cible___2","cible___1", "cible___3")
new_traitement$SCP <- rowSums(new_traitement[, names(new_traitement) %in% SCP], na.rm = TRUE)
new_traitement$SCP <- ifelse(new_traitement$SCP != 0, 1, 0)

# si le patient a pris le traitement Lévodopa Gel Intestinal
LGI <- c("pompe_2")
new_traitement$LGI <- new_traitement$pompe_2

# si le patient a pris le traitement Apomorphine Sous Cutanée
ASC <- c("pompe")
new_traitement$ASC <- new_traitement$pompe

# si le patient a pris le traitement psychotique
Antipsychotique <- c("ttt_leponex_100_yn___yes","ttt_quetiapine_50_yn___yes","ttt_quetiapine_300_yn___yes","ttt_quetiapine_400_yn___yes")
Anticholinestherasique <- c("ttt_ache_yn___yes","ttt_exelon_yn___yes")
new_traitement$Antipsychotique <- rowSums(new_traitement[, names(new_traitement) %in% Antipsychotique], na.rm = TRUE)
new_traitement$Antipsychotique <- ifelse(new_traitement$Antipsychotique != 0, 1, 0)
new_traitement$Anticholinestherasique <- rowSums(new_traitement[, names(new_traitement) %in% Anticholinestherasique], na.rm = TRUE)
new_traitement$Anticholinestherasique <- ifelse(new_traitement$Anticholinestherasique != 0, 1, 0)

mean(new_traitement$B)

mean(new_traitement$TO)





Consultation_20250106 <- read_excel(path = "Consultation_20250106.xlsx")


Consultation_20250106 <- Consultation_20250106 %>% inner_join(data_i %>% select(anonyme_id))


liste_apo <- grepl("\\bAPOKINON\\b", Consultation_20250106$ttt_autre_ldopa,ignore.case = TRUE)


df_ldopa <- Consultation_20250106

df_ldopa <- df_ldopa[,names(df_ldopa) %in% c("ttt_autre_ldopa","redcap_repeat_instance", "anonyme_id")]


count_words_in_list <- function(sentence, word_list) {
  count <- sum(sapply(word_list, function(word) grepl(paste0("\\b", word, "\\b"), sentence, ignore.case = TRUE)))
  return(count)
}





liste_anesthesique <- c("ANTASOL")
df_ldopa$N_anesthesique <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_anesthesique))


liste_anticholinergiques <- c("ARTANE","PARKINANE")
df_ldopa$F <- ifelse(grepl(paste(liste_anticholinergiques, collapse = "|"), 
                           df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_analgesiques <- c("ASPEGIC","CODOLIPRANE","DAFALGAN","DOLIPRANE","DUROGESIC","EFFERALGAN","GABAPENTINE","IXPRIM","LAMALINE","LYRICA","NEURONTIN","OXYCONTIN","PARACETAMOL","SKENAN","TRAMADOL")
df_ldopa$N_analgesique <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_analgesiques))

liste_antiepileptique <- c("DEPAKINE","DEPAKOTE","DEPAMIDE","EPITOMAX","KEPPRA","LAMICTAL","LAMOTRIGINE","LEVETIRACETAM","RIVOTRIL","TEGRETOL")
df_ldopa$N_antiepileptique <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_antiepileptique))

liste_psy <- c("ALPRAZOLAM","ATARAX","BROMAZEPAM","BUSPIRONE","CIRCADIN","DIAZEPAM","IMOVANE","LEXOMIL","LORAZEPAM","LYSANXIA","MELATONINE","NOCTAMIDE","PRAZEPAM","STILNOX","STRESAM","TEMESTA","URBANYL","VALIUM","VERATRAN","XANAX","ZOLPIDEM","ZOPICLONE","ANAFRANIL","DEROXAT","DONEPEZIL","MODIODAL")
df_ldopa$N_autre_psy <- ifelse(grepl(paste(liste_psy, collapse = "|"), 
                                     df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_psychotique <- c("ABILIFY","CLOZAPINE","RISPERDAL","SERESTA","TIAPRIDAL","XEROQUEL","LOXAPRAC","mélatonine","Oxazepam")
df_ldopa$N_psychotique <- ifelse(grepl(paste(liste_psychotique, collapse = "|"), 
                                       df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_antidep <- c("ANAFRANIL","BRINTELLIX","CITALOPRAM","CLOMIPRAMINE","CYMBALTA","EFFEXOR","ESCITALOPRAM","FLUOXETINE","LAROXYL","MIANSERINE","MILNACIPRAN","MIRTAZAPINE","MOCLAMINE","NORSET","PAROXETINE","PROZAC","QUITAXON","SEROPLEX","SEROPRAM","SERTRALINE","SURMONTIL","VENLAFAXINE","ZOLOFT","Duloxetine")
df_ldopa$N_antidep <- ifelse(grepl(paste(liste_antidep, collapse = "|"), 
                                   df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_demence <- c("ARICEPT","DONEPEZIL","EBIXA","EXELON","RIVASTIGMINE")
df_ldopa$N_demence <- ifelse(grepl(paste(liste_demence, collapse = "|"), 
                                   df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_autres <- c("FAMPYRA","REVIA")
df_ldopa$N_autres <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_autres))

liste_digest <- c("AMAREL","ARTISIAL","ATROPINE","CALCIDOSE","CALTRATE","DEBRIDAT","DIAMICRON","DICETEL","DIFFU-K","DOMPERIDONE","DULCOLAX","DUPHALAC","DUPHALAC","EDUCTYL","ESOMEPRAZOLE","EUCREAS","EUPANTOL","FORLAX","GAVISCON","GLICLAZIDE","GLUCOPHAGE","IDEOS","INEXIUM","INIPOMP","JANUVIA","LACTULOSE","LANSOPRAZOLE","LANSOYL","LANTUS","MACROGOL","MELAXOSE","METEOSPASMYL","METFORMINE","MOPRAL","MOTILIUM","MOVICOL","MOXYDAR","NORMACOL","NOVOMIX","NOVORAPID","OMEPRAZOLE","PANTOPRAZOLE","PARIET","PENTASA","PHOSPHONEUROS","PSYLIA","RABEPRAZOLE","REPAGLINIDE","SMECTA","SPASFON","STAGID","SULFARLEM","TRANSIPEG","UVEDOSE","VICTOZA","MOVICOL")
df_ldopa$voies_digestives <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_digest))

liste_sang <- c("CLOPIDOGREL","COUMADINE","ELIQUIS","FUMAFER","KARDEGIC","PLAVIX","PRADAXA","PREVISCAN","SPECIAFOLDINE","XARELTO")
df_ldopa$sang_organes <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_sang))

liste_cardio <- c("ACEBUTOLOL","ALDACTAZINE","ALDACTONE","AMIODARONE","AMLODIPINE","AMLOR","APROVEL","ATENOLOL","ATORVASTATINE","AVLOCARDYL","BIPRETERAX","BISOCE","BISOPROLOL","CAPTOPRIL","CELIPROLOL","COAPROVEL","CORDARONE","COVERSYL","CRESTOR","ETIOVEN","FENOFIBRATE","FLECAINE","FLUVASTATINE","FUROSEMIDE","GUTRON","HYTACAND","IKOREL","IRBESARTAN","ISOPTINE","LASILIX","LERCANIDIPINE","LESCOL","LOSARTAN","LOXEN","NEBIVOLOL","PERINDOPRIL","PRAVASTATINE","PROCORALAN","PROPRANOLOL","RAMIPRIL","SECTRAL","SELOKEN","SIMVASTATINE","SOTALEX","SOTALOL","SPIRONOLACTONE","TAHOR","TEMERIT","TENORMINE","VALSARTAN","VERAPAMIL","ALTIZIDE","ENDOTELON","PROPANOLOL")
df_ldopa$cardiovasculaire <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_cardio))

liste_urine <- c("ALFUZOSINE","AVODART","CERIS","CHIBRO-PROSCAR","CIALIS","COMBODART","DRIPTANE","DUTASTERIDE","FESOTERODINE","FINASTERIDE","JOSIR","MECIR","OMEXEL","OMIX","OXYBUTYNINE","PARLODEL","PERMIXON","SILODYX","TADENAN","TAMSULOSINE","TROSPIPHARM","VESICARE","XATRAL","TADALAFIL","TOVIAZ")
df_ldopa$urine_hormsex <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_urine))

liste_horm <- c("FLUCORTAC","FLUDROCORTISONE","KETOCONAZOLE","LEVOTHYROX","LEVOTHYROXINE","FLUCOTAC","FLUCOTEC")
df_ldopa$hormsys_sex <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_horm))

liste_resp <- c("AERIUS","LIORESAL","SERETIDE","VENTOLINE")
df_ldopa$respiratoire <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_resp))

liste_muscle <- c("ALLOPURINOL","BACLOFENE","BOTULIQUE")
df_ldopa$muscle_squel <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_muscle))

liste_levodopa_oral <- c("SINEMET")
df_ldopa$N_park_dop_levodopa <- ifelse(grepl(paste(liste_levodopa_oral, collapse = "|"), 
                                             df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_levodopa <- c("DUODOPA")
df_ldopa$N_park_dop_duodopa <- ifelse(grepl(paste(liste_levodopa, collapse = "|"), 
                                            df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_N_park_dop_agonistes <- c("APOMORPHINE CHABRE","PARLODEL")
df_ldopa$N_park_dop_agonistes <- ifelse(grepl(paste(liste_N_park_dop_agonistes, collapse = "|"), 
                                              df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_N_park_ASC <- c("Pompe à Apomorphine","APOMORPHINE MRM")
df_ldopa$N_park_ASC <- ifelse(grepl(paste(liste_N_park_ASC, collapse = "|"), 
                                    df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_N_park_A <- c("OTRASEL")
df_ldopa$N_park_A <- ifelse(grepl(paste(liste_N_park_A, collapse = "|"), 
                                  df_ldopa$ttt_autre_ldopa,ignore.case = TRUE), 1, 0)

liste_apo <- grepl("\\bAPOKINON\\b", Consultation_20250106$ttt_autre_ldopa,ignore.case = TRUE)

df_apokinon <- df_ldopa[liste_apo,]

liste_apo_stylo <- df_apokinon[grepl("\\bSTYLO\\b", df_apokinon$ttt_autre_ldopa,ignore.case = TRUE),]$anonyme_id
liste_apo_sc <- df_apokinon[!df_apokinon$anonyme_id %in% liste_apo_stylo,]$anonyme_id

df_ldopa[df_ldopa$anonyme_id %in% liste_apo_stylo,]$N_park_dop_agonistes <- 1
df_ldopa[df_ldopa$anonyme_id %in% liste_apo_sc,]$N_park_ASC <- 1

liste_senso <- c("ARTELAC","ECOVITAMINE","MONOPROST","NAABAK","VOLTARENE")
df_ldopa$org_senso <- sapply(df_ldopa$ttt_autre_ldopa,function(x) count_words_in_list(x,liste_senso))


df_ldopa$Nb_autres <- rowSums(df_ldopa[,names(df_ldopa) %in% c("org_senso","muscle_squel","respiratoire","hormsys_sex","urine_hormsex",
                                                               "cardiovasculaire","sang_organes","voies_digestives",
                                                               "N_autres","N_antiepileptique","N_analgesique","N_anesthesique","N_autre_psy")])


df_ldopa$TO <- 0

# Anticholinergiques sont tous par voie orale
df_ldopa[df_ldopa$F == 1,]$TO <- 1
df_ldopa[df_ldopa$N_park_dop_levodopa == 1,]$TO <- 1
df_ldopa[df_ldopa$N_park_dop_agonistes == 1,]$TO <- 1
df_ldopa[df_ldopa$N_park_A == 1,]$TO <- 1

df_complet <- new_traitement

df_complet$A <- df_complet$A + df_ldopa$N_park_A
df_complet$A <- ifelse(df_complet$A != 0,1,0)

df_complet$B <- df_complet$B + df_ldopa$N_park_dop_levodopa
df_complet$B <- ifelse(df_complet$B != 0,1,0)

df_complet$C <- df_complet$C + df_ldopa$N_park_dop_agonistes
df_complet$C <- ifelse(df_complet$C != 0,1,0)

df_complet$TO <- df_complet$TO + df_ldopa$TO
df_complet$TO <- ifelse(df_complet$TO != 0,1,0)

df_complet$Anticholinesterasique <- df_complet$Anticholinestherasique + df_ldopa$N_demence
df_complet$Anticholinesterasique <- ifelse(df_complet$Anticholinesterasique != 0,1,0)

df_complet$Antipsychotique <- df_complet$Antipsychotique + df_ldopa$N_psychotique
df_complet$Antipsychotique <- ifelse(df_complet$Antipsychotique != 0,1,0)

df_complet$Antidepresseur <- df_ldopa$N_antidep

df_complet$ASC <- df_complet$ASC + df_ldopa$N_park_ASC
df_complet$ASC <- ifelse(df_complet$ASC != 0,1,0)

df_complet$LGI <- df_complet$LGI + df_ldopa$N_park_dop_duodopa
df_complet$LGI <- ifelse(df_complet$LGI != 0,1,0)

df_complet$F <- df_ldopa$F

df_complet$Nb_Autre <- 0
df_complet$Nb_Autre <- df_ldopa$Nb_autres
df_complet$Nb_Autre <- df_complet$Nb_Autre + new_traitement$ttt_gutron_yn___yes

df_complet <- df_complet[ ,names(df_complet) %in% c("anonyme_id ","A","B","C","D","E","F","TO","SCP","LGI","ASC","Antipsychotique","Anticholinestherasique","Antidepresseur","Nb_Autre")]

df_complet <- df_complet %>% bind_cols(Consultation_20250106 %>% select(anonyme_id,redcap_repeat_instance,act_datedeb ))

names(df_complet)

Consultation_20250106$hoehn_yahr_on

data_i <- data_i %>% inner_join(Consultation_20250106 %>% select(anonyme_id , act_datedeb, freezing, hoehn_yahr_on )) %>%
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>% 
  mutate(disease_duration=as.numeric(year)-as.numeric(diag_date_a)) %>%
  mutate(age=as.numeric(year)-as.numeric(pat_ddn_a) ) 


df_complet <- data_i %>% bind_cols(df_complet)


fwrite(df_complet, "df_complet.txt")

# ---------

# Initial distributions overall all patient-visits -------------

df_complet <- fread( "df_complet.txt")

df_complet <- df_complet %>% select(-c(anonyme_id...1, act_datedeb...5))

test <- df_complet %>% select(B, freezing, disease_duration, hoehn_yahr_on) %>%
    mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  mutate(freezing=ifelse(freezing==">=2",2,freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na() 

summary(lm(freezing ~ as.factor(B) + disease_duration + hoehn_yahr_on, data=test))


summary(lm(freezing ~ as.factor(B) , data=test))


library(mediation)

# Step 1: Total Effect
model_total <- lm(freezing ~ as.factor(B),  data=test)

# Step 2: Mediators Path
model_mediator1 <- lm(hoehn_yahr_on ~ as.factor(B), data=test)
model_mediator2 <- lm(disease_duration  ~ as.factor(B), data=test)

# Step 3: Direct and Indirect Effects
model_direct <- lm(freezing ~ as.factor(B) +hoehn_yahr_on + disease_duration,  data=test)

# Mediation Analysis
# Mediation Analysis for Each Mediator
mediate_result1 <- mediate(model_mediator1, model_direct, treat = "as.factor(B)", mediator = "hoehn_yahr_on")
mediate_result2 <- mediate(model_mediator2, model_direct, treat = "as.factor(B)", mediator = "disease_duration")

# Summary of Mediation Results
summary(mediate_result1)
summary(mediate_result2)



test %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>%
  ggplot(aes(disease_duration, freezing)) +
  geom_smooth()


test %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>%
  ggplot(aes(hoehn_yahr_on, freezing)) +
  geom_jitter()

test %>% group_by(hoehn_yahr_on, freezing) %>% count() %>%
  spread(key=freezing, value=n) %>%
  mutate(tot=`0`+`1`+`2`+`3`+`4`) %>%
  mutate(`0`=`0`/tot) %>%
  mutate(`1`=`1`/tot) %>%
  mutate(`2`=`2`/tot) %>%
  mutate(`3`=`3`/tot) %>%
  mutate(`4`=`4`/tot) 


test %>% group_by(disease_duration , freezing) %>% count() %>%
  spread(key=freezing, value=n) %>%
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>%
  mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
  mutate(`2`=ifelse(is.na(`2`),0,`2`)) %>%
  mutate(`3`=ifelse(is.na(`3`),0,`3`)) %>%
  mutate(`4`=ifelse(is.na(`4`),0,`4`)) %>%
  mutate(tot=`0`+`1`+`2`+`3`+`4`) %>%
  mutate(`0`=`0`/tot) %>%
  mutate(`1`=`1`/tot) %>%
  mutate(`2`=`2`/tot) %>%
  mutate(`3`=`3`/tot) %>%
  mutate(`4`=`4`/tot)  %>% ungroup() %>%
  gather(FOG, value, `0`:`4`) %>%
  filter(disease_duration<40) %>% filter(disease_duration>=0) %>%
  ggplot(aes(disease_duration, value, colour=FOG, fill=FOG)) +
  geom_smooth(se=F, size=2, alpha=0.5) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  +
  ylab("Proportion of patient-visits \n") + xlab("\n Disease duration [years]") +
  scale_colour_manual(values=c("#F2F2F2", "#83CBEB", "#104862", "#F6C6AD", "#CD3333")) 




test %>% group_by(B, freezing) %>% count() %>%
  spread(key=freezing, value=n) %>%
  mutate(tot=`0`+`1`+`2`+`3`+`4`) %>%
  mutate(`0`=`0`/tot) %>%
  mutate(`1`=`1`/tot) %>%
  mutate(`2`=`2`/tot) %>%
  mutate(`3`=`3`/tot) %>%
  mutate(`4`=`4`/tot) 


test %>% group_by(B, disease_duration, hoehn_yahr_on) %>% summarise(freezing=mean(freezing)) %>%
  filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) %>%
  mutate(B=ifelse(B==0, "No Levodopa", "ON Levodopa")) %>%
  ggplot(aes(x = disease_duration, y = hoehn_yahr_on, fill = freezing)) +
  geom_tile() +  # Heatmap
  scale_fill_gradient(low = "#F2F2F2", high = "#104862") +  # Color gradient
  facet_wrap(~B) +  # Separate plots for B=0 and B=1
  labs(
   # title = "Heatmap of Disease Duration vs Hoehn-Yahr Stage",
    x = "\n Disease Duration",
    y = "Hoehn & Yahr ON \n",
    fill = "Freezing"
  ) +
  theme_minimal() +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
       # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))  

# ---------
# Create the for ealy PD drug groups --------


df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`, `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
  ) %>% mutate( `act_datedeb...5`=as.Date( `act_datedeb...5`)) %>%
  mutate(`v2_act_datedeb...5`=as.Date(v2_act_datedeb...5)) %>%
  mutate(elapsed=lubridate::interval(act_datedeb...5, v2_act_datedeb...5  ) %/% months(1)) %>%
  filter(elapsed>=6 & elapsed<=24) %>%
  select(anonyme_id...1) %>% distinct() # 2389

second_visit <- first_visit %>% ungroup() %>% mutate(act_datedeb...5 =as.Date(act_datedeb...5 )) %>% 
  left_join(
  df_complet %>% select(`anonyme_id...1`, `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
  ) %>% mutate( `act_datedeb...5`=as.Date( `act_datedeb...5`)) %>%
  mutate(`v2_act_datedeb...5`=as.Date(v2_act_datedeb...5)) %>%
  mutate(elapsed=lubridate::interval(act_datedeb...5, v2_act_datedeb...5  ) %/% months(1)) %>%
  filter(elapsed>=6 & elapsed<=24) %>%
  select(anonyme_id...1, v2_act_datedeb...5) %>% distinct()



first_to_second_visit <- first_visit %>% mutate(act_datedeb...5=as.Date(act_datedeb...5)) %>%
  inner_join(second_visit ) %>% 
  mutate( `act_datedeb...5`=as.Date( `act_datedeb...5`)) %>%
  mutate(`v2_act_datedeb...5`=as.Date(v2_act_datedeb...5)) %>%
  group_by(anonyme_id...1) %>% 
  filter(v2_act_datedeb...5==min(v2_act_datedeb...5)) %>%
  filter(act_datedeb...5==min(act_datedeb...5)) %>% distinct()


names(df_complet)



Groups <- df_complet %>%  mutate(Groups=ifelse(B==1&D==1, "1- LD+Amantadine",
                                     ifelse(B==1, "2- LD (combo)",
                                            ifelse(A==1|C==1|ASC==1,"3- Agonists",
                                                   ifelse(A==0&B==0&C==0&D==0&E==0&TO==0&SCP==0&LGI==0&ASC==0, "4- None", "Remove"))))) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) %>%
  filter(Groups!="Remove")

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)


first_to_second_visit %>% group_by(Groups, v2_Groups) %>% count()


first_to_second_visit <- first_to_second_visit %>% left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing)) %>% 
  left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing) %>% rename("v2_freezing"="freezing"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

unique(first_to_second_visit$freezing)

first_to_second_visit %>% group_by(freezing) %>% count()

first_to_second_visit <- first_to_second_visit %>% mutate(freezing=ifelse(freezing==1,0,freezing))

unique(first_to_second_visit$v2_freezing)


first_to_second_visit <- first_to_second_visit %>% 
  mutate(v2_freezing=ifelse(v2_freezing==">=2","2",v2_freezing)) %>%
  mutate(v2_freezing=as.numeric(v2_freezing))


first_to_second_visit %>% ungroup() %>%
  group_by(Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T)) 


first_to_second_visit %>% group_by(Groups, freezing, v2_freezing) %>% count()


first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==0,0,1)) %>%
  group_by(Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T))



tes# ---------