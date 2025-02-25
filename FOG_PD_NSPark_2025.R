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


test # 35256 visits
length(unique(df_complet$anonyme_id...25)) # 25602

summary(lm(freezing ~ as.factor(B) + disease_duration + hoehn_yahr_on, data=test))


summary(lm(freezing ~ as.factor(B) , data=test))


# Step 1: Total Effect
model_total <- lm(freezing ~ as.factor(B),  data=test)

# Step 2: Mediators Path
model_mediator1 <- lm(hoehn_yahr_on ~ as.factor(B), data=test)
model_mediator2 <- lm(disease_duration  ~ as.factor(B), data=test)

# Step 3: Direct and Indirect Effects
model_direct <- lm(freezing ~ as.factor(B) +hoehn_yahr_on + disease_duration,  data=test)

# Mediation Analysis
# Mediation Analysis for Each Mediator
mediate_result1 <- mediation::mediate(model_mediator1, model_direct, treat = "as.factor(B)", mediator = "hoehn_yahr_on")
mediate_result2 <- mediation::mediate(model_mediator2, model_direct, treat = "as.factor(B)", mediator = "disease_duration")

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




test

model <- ordinal::clm(as.factor(freezing) ~ as.factor(B) , data = test)

summary(model)

model <- ordinal::clm(as.factor(freezing) ~ as.factor(hoehn_yahr_on) , data = test)

summary(model)



model <- ordinal::clm(as.factor(freezing) ~ disease_duration , data = test)

summary(model)


# ---------
# Initial distributions overall all patient-visits STARTING WITH FOG = 0 -------------

df_complet <- fread( "df_complet.txt")

df_complet <- df_complet %>% select(-c(anonyme_id...1, act_datedeb...5))


df_complet <- df_complet %>% group_by(anonyme_id...25) %>% filter(act_datedeb...27==min(act_datedeb...27)) %>%
  filter(freezing=="0") %>% select(anonyme_id...25) %>% distinct() %>% ungroup() %>%
  left_join(df_complet)


test <- df_complet %>% select(B, freezing, disease_duration, hoehn_yahr_on) %>%
    mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  mutate(freezing=ifelse(freezing==">=2",2,freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na() 


test # 22987 visits
length(unique(df_complet$anonyme_id...25)) # 13538

summary(lm(freezing ~ as.factor(B) + disease_duration + hoehn_yahr_on, data=test))

summary(lm(freezing ~ as.factor(B) , data=test))


# Step 1: Total Effect
model_total <- lm(freezing ~ as.factor(B),  data=test)

# Step 2: Mediators Path
model_mediator1 <- lm(hoehn_yahr_on ~ as.factor(B), data=test)
model_mediator2 <- lm(disease_duration  ~ as.factor(B), data=test)

# Step 3: Direct and Indirect Effects
model_direct <- lm(freezing ~ as.factor(B) +hoehn_yahr_on + disease_duration,  data=test)

# Mediation Analysis
# Mediation Analysis for Each Mediator
mediate_result1 <- mediation::mediate(model_mediator1, model_direct, treat = "as.factor(B)", mediator = "hoehn_yahr_on")
mediate_result2 <- mediation::mediate(model_mediator2, model_direct, treat = "as.factor(B)", mediator = "disease_duration")

# Summary of Mediation Results
summary(mediate_result1)
summary(mediate_result2)


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



test

model <- ordinal::clm(as.factor(freezing) ~ as.factor(B) , data = test)

summary(model)


model <- ordinal::clm(as.factor(freezing) ~ as.factor(hoehn_yahr_on) , data = test)

summary(model)


model <- ordinal::clm(as.factor(freezing) ~ disease_duration , data = test)

summary(model)


# ---------
# Create the patient funel for early PD drug groups --------


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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit %>% group_by(Groups) %>% count() %>% mutate(n=n/2389)

#   Groups       n
# 1 1- LD     1004
# 2 2- Other  1385
first_to_second_visit %>% group_by(v2_Groups) %>% count() %>% mutate(n=n/2389)

first_to_second_visit %>% group_by(Groups, v2_Groups) %>% count()

#   Groups   v2_Groups     n
# 1 1- LD    1- LD       982
# 2 1- LD    2- Other     22
# 3 2- Other 1- LD       443
# 4 2- Other 2- Other    942


mcnemar_table <- table(first_to_second_visit$Groups == "1- LD", first_to_second_visit$v2_Groups == "1- LD")

print(mcnemar_table)

mcnemar.test(mcnemar_table)

# 	McNemar's Chi-squared test with continuity correction
# 
# data:  mcnemar_table
# McNemar's chi-squared = 379.35, df = 1, p-value < 2.2e-16

first_to_second_visit <- first_to_second_visit %>% left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing)) %>% 
  left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing) %>% rename("v2_freezing"="freezing"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

unique(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% group_by(anonyme_id...1, act_datedeb...5, v2_act_datedeb...5, Groups, v2_Groups) %>% 
  mutate(freezing=min(freezing)) %>% mutate(v2_freezing=min(v2_freezing))  %>% distinct() %>% ungroup()

first_to_second_visit %>% group_by(freezing) %>% count()

#   freezing     n
# 1 0         2389
  

first_to_second_visit <- first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==">=2", "2", v2_freezing)) %>% 
  mutate(v2_freezing=as.numeric(v2_freezing)) %>% mutate(v2_freezing=ifelse(is.na(v2_freezing), 0, v2_freezing))

first_to_second_visit %>% group_by(v2_freezing) %>% count()

#   v2_freezing     n
# 1           0  2246
# 2           1   119
# 3           2    21
# 4           3     3


test <- first_to_second_visit %>% select(freezing, v2_freezing) %>%
  gather(eval, score, freezing:v2_freezing) %>% mutate(score=as.factor(score)) %>%
   mutate(eval=as.factor(eval))

unique(test$score)

model <- ordinal::clm(score ~ eval , data = test)

summary(model)
# (1) Hessian is numerically singular: parameters are not uniquely determined 
# In addition: Absolute convergence criterion was met, but relative criterion was not met 


first_to_second_visit %>% ungroup() %>%
  group_by(Groups) %>% summarise(mean=mean(freezing, na.rm=T)) 

#   Groups    mean
# 1 1- LD        0
# 2 2- Other     0

first_to_second_visit %>% ungroup() %>%
  group_by(Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T)) 

#   Groups     mean
# 1 1- LD    0.0966
# 2 2- Other 0.0527


first_to_second_visit %>% group_by(Groups, freezing, v2_freezing) %>% count()

#   Groups   freezing v2_freezing     n
# 1 1- LD           0           0   927
# 2 1- LD           0           1    59
# 3 1- LD           0           2    16
# 4 1- LD           0           3     2
# 5 2- Other        0           0  1319
# 6 2- Other        0           1    60
# 7 2- Other        0           2     5
# 8 2- Other        0           3     1

first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==0,0,1)) %>%
  group_by(Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T))

#   Groups     mean
# 1 1- LD    0.0767
# 2 2- Other 0.0477


first_to_second_visit$freezing <- as.numeric(first_to_second_visit$freezing)

first_to_second_visit$freezing <- first_to_second_visit$freezing - 1

mean(first_to_second_visit$freezing)


first_to_second_visit$v2_freezing <- as.numeric(first_to_second_visit$v2_freezing)

first_to_second_visit$v2_freezing <- first_to_second_visit$v2_freezing - 1

mean(first_to_second_visit$v2_freezing)

first_to_second_visit <- first_to_second_visit %>%
  mutate(freezing_diff = v2_freezing - freezing)


wilcox.test(freezing_diff ~ Groups, data = first_to_second_visit)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  freezing_diff by Groups
# W = 715821, p-value = 0.00266
# alternative hypothesis: true location shift is not equal to 0




# ---------
# Create the patient funel for early PD drug groups NO LEVODOPA AT BASELINE --------


df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit %>% group_by(Groups) %>% count() %>% mutate(n=n/1409)


first_to_second_visit %>% group_by(v2_Groups) %>% count() %>% mutate(n=n/1409)

#   v2_Groups     n
#   <chr>     <dbl>
# 1 1- LD     0.329
# 2 2- Other  0.671

first_to_second_visit %>% group_by(Groups, v2_Groups) %>% count()

#  Groups   v2_Groups     n
#   <chr>    <chr>     <int>
# 1 2- Other 1- LD       463
# 2 2- Other 2- Other    946

mcnemar_table <- table(first_to_second_visit$Groups == "1- LD", first_to_second_visit$v2_Groups == "1- LD")

print(mcnemar_table)

mcnemar.test(mcnemar_table)


first_to_second_visit <- first_to_second_visit %>% left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing)) %>% 
  left_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, freezing) %>% rename("v2_freezing"="freezing"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

unique(first_to_second_visit$freezing)
unique(first_to_second_visit$v2_freezing)

first_to_second_visit <- first_to_second_visit %>% group_by(anonyme_id...1, act_datedeb...5, v2_act_datedeb...5, Groups, v2_Groups) %>% 
  mutate(freezing=min(freezing)) %>% mutate(v2_freezing=min(v2_freezing))  %>% distinct() %>% ungroup()

first_to_second_visit %>% group_by(freezing) %>% count()

#   freezing     n
# 1 0         1409
  

first_to_second_visit <- first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==">=2", "2", v2_freezing)) %>% 
  mutate(v2_freezing=as.numeric(v2_freezing)) %>% mutate(v2_freezing=ifelse(is.na(v2_freezing), 0, v2_freezing))

first_to_second_visit %>% group_by(v2_freezing) %>% count()

#   v2_freezing     n
# 1           0  1338
# 2           1    65
# 3           2     5
# 4           3     1



test <- first_to_second_visit %>% select(freezing, v2_freezing) %>%
  gather(eval, score, freezing:v2_freezing) %>% mutate(score=as.factor(score)) %>%
   mutate(eval=as.factor(eval))

unique(test$score)

model <- ordinal::clm(score ~ eval , data = test)

summary(model)

# (1) Hessian is numerically singular: parameters are not uniquely determined 
# In addition: Absolute convergence criterion was met, but relative criterion was not met 


first_to_second_visit %>% ungroup() %>%
  group_by(Groups) %>% summarise(mean=mean(freezing, na.rm=T)) 

#   Groups    mean
# 1 1- LD        0
# 2 2- Other     0

first_to_second_visit %>% ungroup() %>%
  group_by(Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T)) 

#   Groups     mean
# 1 1- LD    0.0966
# 2 2- Other 0.0527

first_to_second_visit %>% ungroup() %>%
  group_by(v2_Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T)) 



first_to_second_visit %>% group_by(v2_Groups, freezing, v2_freezing) %>% count()

#  v2_Groups freezing v2_freezing     n
#   <chr>     <chr>          <dbl> <int>
# 1 1- LD     0                  0   437
# 2 1- LD     0                  1    25
# 3 1- LD     0                  3     1
# 4 2- Other  0                  0   901
# 5 2- Other  0                  1    40
# 6 2- Other  0                  2     5

first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==0,0,1)) %>%
  group_by(v2_Groups) %>% summarise(mean=mean(v2_freezing, na.rm=T))

#  1- LD     0.0562
# 2 2- Other  0.0476

first_to_second_visit %>% mutate(v2_freezing=ifelse(v2_freezing==0,0,1)) %>%
  group_by(v2_Groups) %>% summarise(sd=sd(v2_freezing, na.rm=T))

# 1 1- LD     0.226
# 2 2- Other  0.201

first_to_second_visit$freezing <- as.numeric(first_to_second_visit$freezing)

first_to_second_visit$freezing <- first_to_second_visit$freezing - 1

mean(first_to_second_visit$freezing)


first_to_second_visit$v2_freezing <- as.numeric(first_to_second_visit$v2_freezing)

first_to_second_visit$v2_freezing <- first_to_second_visit$v2_freezing - 1

mean(first_to_second_visit$v2_freezing)

first_to_second_visit <- first_to_second_visit %>%
  mutate(freezing_diff = v2_freezing - freezing)


wilcox.test(freezing_diff ~ v2_Groups, data = first_to_second_visit)

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  freezing_diff by v2_Groups
# W = 220840, p-value = 0.4985
# alternative hypothesis: true location shift is not equal to 0

first_to_second_visit %>% group_by(v2_Groups, freezing_diff) %>% count() %>%
  ungroup() %>% group_by(v2_Groups) %>% mutate(tot=sum(n)) %>% mutate(tot=n/tot)

#   v2_Groups freezing_diff     n     tot
#   <chr>             <dbl> <int>   <dbl>
# 1 1- LD                 0   437 0.944  
# 2 1- LD                 1    25 0.0540 
# 3 1- LD                 3     1 0.00216
# 4 2- Other              0   901 0.952  
# 5 2- Other              1    40 0.0423 
# 6 2- Other              2     5 0.00529
 

# ---------
# KM Cumulative Freezing Incidence Starting ON Levodopa vs none LD --------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()


mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))

filtered_data <- first_to_second_visit %>%
  group_by(anonyme_id...1) %>%
  mutate(first_freezing = which(freezing == 1)[1]) %>%
  filter(row_number() <= first_freezing | is.na(first_freezing)) %>%
  ungroup()

# View result
filtered_data

filtered_data <- filtered_data %>% select(-first_freezing)


library(survival)
library(survminer)



filtered_data <- filtered_data %>% group_by(anonyme_id...1) %>% filter(elapsed==max(elapsed)) %>% distinct()

data <- filtered_data %>% ungroup() %>% select(-anonyme_id...1) 

data <- data %>% mutate(Groups=ifelse(Groups=="1- LD", "Levodopa Baseline", "no LD baseline"))

km_fit <- survfit(Surv(elapsed, freezing ) ~ Groups   , data = data)

summary(km_fit)

km_fit



# Step 3: Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = data, 
           pval = TRUE,          # Add p-value for log-rank test
           conf.int = TRUE,      # Add confidence interval
           #risk.table = TRUE,    # Add risk table to the plot
           palette = c("#CD3333", "#83CBEB"), # Example color palette
           ggtheme = theme_minimal(),
           xlab=("\n Number of Days From Baseline"),
           ylab=("Proportion FOG-free \n")) # Clean theme

# Step 4: Summary and log-rank test to compare the groups
summary(km_fit)

# Optional: If you want to do a formal comparison
log_rank_test <- survdiff(Surv(elapsed, freezing) ~ Groups, data = data)

log_rank_test

# survdiff(formula = Surv(elapsed, freezing) ~ Groups, data = data)
# 
#                             N Observed Expected (O-E)^2/E (O-E)^2/V
# Groups=Levodopa Baseline 1005      198      155     12.15        20
# Groups=no LD baseline    1385      212      255      7.36        20
# 
#  Chisq= 20  on 1 degrees of freedom, p= 8e-06 

# --------
# KM Cumulative Freezing Incidence NO Levodopa Baseline: Eventualy Experienced vs Always Naive  --------


df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()


mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))

filtered_data <- first_to_second_visit %>%
  group_by(anonyme_id...1) %>%
  mutate(first_freezing = which(freezing == 1)[1]) %>%
  filter(row_number() <= first_freezing | is.na(first_freezing)) %>%
  ungroup()

# View result
filtered_data

filtered_data <- filtered_data %>% select(-first_freezing)


library(survival)
library(survminer)

filtered_data

filtered_data <- filtered_data %>% group_by(anonyme_id...1) %>% filter(elapsed==max(elapsed)) %>% distinct()

data <- filtered_data %>% ungroup() %>% select(-anonyme_id...1) 

data <- data %>% mutate(B=ifelse(B=="1", "Levodopa-experienced", "Levodopa-naive"))

km_fit <- survfit(Surv(elapsed, freezing ) ~ B   , data = data)

summary(km_fit)

km_fit

data %>% group_by(B) %>% count()

# Step 3: Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = data, 
           pval = TRUE,          # Add p-value for log-rank test
           conf.int = TRUE,      # Add confidence interval
           #risk.table = TRUE,    # Add risk table to the plot
           palette = c("#CD3333", "#83CBEB"), # Example color palette
           ggtheme = theme_minimal(),
           xlab=("\n Number of Days From Baseline"),
           ylab=("Proportion FOG-free \n")) # Clean theme

# Step 4: Summary and log-rank test to compare the groups
summary(km_fit)

# Optional: If you want to do a formal comparison
log_rank_test <- survdiff(Surv(elapsed, freezing) ~ Groups, data = data)

log_rank_test


# Call:
# survdiff(formula = Surv(elapsed, freezing) ~ Groups, data = data)
# 
#                             N Observed Expected (O-E)^2/E (O-E)^2/V
# Groups=Levodopa Baseline 1005      198      155     12.15        20
# Groups=no LD baseline    1385      212      255      7.36        20
# 
#  Chisq= 20  on 1 degrees of freedom, p= 8e-06 

survdiff(Surv(elapsed, freezing) ~ B, data = data, rho = 1)  # Peto-Peto test

coxph(Surv(elapsed, freezing) ~ B, data = data)


# ---------
# Linear Mixed-model and time-varying Cox regression - Starting ON Levodopa vs not H&Y ------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, hoehn_yahr_on )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$hoehn_yahr_on)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
      mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(Groups=="1- LD", 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365



data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(hoehn_yahr_on ==max(hoehn_yahr_on )) %>% distinct()


library(nlme)

# lme_model <- nlme::lme(fixed = hoehn_yahr_on ~ elapsed_scaled + Groups, 
#                  random = ~ elapsed_scaled | anonyme_id...1, 
#                  data = data, 
#                  method = "REML")
# 
# summary(lme_model)
# 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC     BIC    logLik
#   13032.44 13081.7 -6509.221
# 
# Random effects:
#  Formula: ~elapsed_scaled | anonyme_id...1
#  Structure: General positive-definite, Log-Cholesky parametrization
#                StdDev    Corr  
# (Intercept)    0.5245166 (Intr)
# elapsed_scaled 0.1134661 0.287 
# Residual       0.3815857       
# 
# Fixed effects:  hoehn_yahr_on ~ elapsed_scaled + Groups 
#                    Value   Std.Error   DF   t-value p-value
# (Intercept)    1.7751650 0.015978444 6099 111.09749       0
# elapsed_scaled 0.0983330 0.004644928 6099  21.16997       0
# Groups         0.2453974 0.023382512 2312  10.49491       0
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.268       
# Groups         -0.633  0.005
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -5.47135994 -0.40416260 -0.01407664  0.33813479  4.99219869 
# 
# Number of Observations: 8414
# Number of Groups: 2314 



data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(hoehn_yahr_on ==max(hoehn_yahr_on )) %>% ungroup()




# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, hy_stage = hoehn_yahr_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + hy_stage + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# > cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + hy_stage + cluster(ID), data = data)
# > # Show results
# > summary(cox_model_td)
# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + hy_stage, 
#     data = data, cluster = ID)
# 
#   n= 6168, number of events= 920 
# 
#             coef exp(coef) se(coef) robust se     z Pr(>|z|)    
# Groups   0.29993   1.34977  0.06961   0.11149  2.69  0.00714 ** 
# hy_stage 0.59371   1.81069  0.03669   0.05167 11.49  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#          exp(coef) exp(-coef) lower .95 upper .95
# Groups       1.350     0.7409     1.085     1.679
# hy_stage     1.811     0.5523     1.636     2.004
# 
# Concordance= 0.688  (se = 0.015 )
# Likelihood ratio test= 271.7  on 2 df,   p=<2e-16
# Wald test            = 159.8  on 2 df,   p=<2e-16
# Score (logrank) test = 314.9  on 2 df,   p=<2e-16,   Robust = 102.9  p=<2e-16
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


# Coefficients Interpretation
# Variable	Coefficient (coef)	Hazard Ratio (exp(coef))	p-value
# Groups	0.29993	1.35 (34.9% increased risk)	0.007 (significant)
# hy_stage	0.59371	1.81 (81.1% increased risk)	<2e-16 (highly significant)
# Key Takeaways:
# Groups (HR = 1.35, p = 0.007)
# → Belonging to a particular group increases the risk of freezing by 35%.
# Hoehn & Yahr stage (HR = 1.81, p < 2e-16)
# → A higher Parkinson’s disease stage is strongly associated with an 81% increased risk of freezing.

survminer::ggforest(cox_model_td, data = data)


# Fit survival curves based on the Cox model
surv_fit <- survival::survfit(cox_model_td, newdata = data.frame(Groups = c(0,1), hy_stage = mean(data$hy_stage)))

# Plot survival curves
survminer::ggsurvplot(surv_fit, data = data,
           conf.int = TRUE, # Show confidence intervals
           pval = TRUE, # Show p-value
           risk.table = TRUE, # Add risk table below plot
           legend.labs = c("Group 0", "Group 1"),
           xlab = "Time (Days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())


# -------
# Linear Mixed-model and time-varying Cox regression - Levodopa-experienced vs naive H&Y ------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")




names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, hoehn_yahr_on )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$hoehn_yahr_on)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
      mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(B==1, 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365

data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(hoehn_yahr_on ==max(hoehn_yahr_on )) %>% distinct()


library(nlme)

lme_model <- nlme::lme(fixed = hoehn_yahr_on ~ elapsed_scaled + Groups,
                 random = ~ elapsed_scaled | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)
 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC   logLik
#   7595.299 7640.745 -3790.65
# 
# Random effects:
#  Formula: ~elapsed_scaled | anonyme_id...1
#  Structure: General positive-definite, Log-Cholesky parametrization
#                StdDev     Corr  
# (Intercept)    0.50148114 (Intr)
# elapsed_scaled 0.09962077 0.231 
# Residual       0.39079009       
# 
# Fixed effects:  hoehn_yahr_on ~ elapsed_scaled + Groups 
#                    Value   Std.Error   DF  t-value p-value
# (Intercept)    1.7742005 0.025019034 3539 70.91403  0.0000
# elapsed_scaled 0.0961055 0.005679733 3539 16.92077  0.0000
# Groups         0.0201149 0.030748281 1338  0.65418  0.5131
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.263       
# Groups         -0.777 -0.074
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -5.34981170 -0.45432504 -0.01560714  0.37211675  4.51636354 
# 
# Number of Observations: 4880
# Number of Groups: 1340 






data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(hoehn_yahr_on ==max(hoehn_yahr_on )) %>% ungroup()



# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, hy_stage = hoehn_yahr_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + hy_stage + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + hy_stage, 
#     data = data, cluster = ID)
# 
#   n= 3598, number of events= 487 
# 
#              coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Groups   -0.15985   0.85227  0.14742   0.19895 -0.803    0.422    
# hy_stage  0.66165   1.93799  0.04939   0.07147  9.258   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#          exp(coef) exp(-coef) lower .95 upper .95
# Groups      0.8523      1.173    0.5771     1.259
# hy_stage    1.9380      0.516    1.6847     2.229
# 
# Concordance= 0.695  (se = 0.021 )
# Likelihood ratio test= 146.8  on 2 df,   p=<2e-16
# Wald test            = 85.71  on 2 df,   p=<2e-16
# Score (logrank) test = 176.8  on 2 df,   p=<2e-16,   Robust = 46.25  p=9e-11
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).



survminer::ggforest(cox_model_td, data = data)


# Fit survival curves based on the Cox model
surv_fit <- survival::survfit(cox_model_td, newdata = data.frame(Groups = c(0,1), hy_stage = mean(data$hy_stage)))

# Plot survival curves
survminer::ggsurvplot(surv_fit, data = data,
           conf.int = TRUE, # Show confidence intervals
           pval = TRUE, # Show p-value
           risk.table = TRUE, # Add risk table below plot
           legend.labs = c("Group 0", "Group 1"),
           xlab = "Time (Days)",
           ylab = "Survival Probability",
           ggtheme = theme_minimal())



# --------



# Linear Mixed-model and time-varying Cox regression - Starting ON Levodopa vs not MDS-UPDRS IIII ------

Echellesmdsupdrs_20250106 <- read_excel(path = "Echellesmdsupdrs_20250106.xlsx")

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% filter(!is.na(mds3_tot_on))

Echellesmdsupdrs_20250106 %>% group_by(anonyme_id) %>% count() %>%
  ungroup() %>% summarise(mean=mean(n))

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% select(anonyme_id, redcap_repeat_instance, mds3_tot_on)





df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, redcap_repeat_instance, freezing )

data_v <- data_v %>% inner_join(Echellesmdsupdrs_20250106) 

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$mds3_tot_on)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb, redcap_repeat_instance)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(Groups=="1- LD", 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365


data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(mds3_tot_on  ==max(mds3_tot_on  )) %>% distinct()

data <- data %>% ungroup()

summary(data)

sum(is.na(data))

library(nlme)

lme_model <- nlme::lme(fixed = mds3_tot_on  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC     BIC    logLik
#   13032.44 13081.7 -6509.221
# 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   6454.216 6478.093 -3222.108
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:    8.400174 6.446673
# 
# Fixed effects:  mds3_tot_on ~ elapsed_scaled + Groups 
#                    Value Std.Error  DF  t-value p-value
# (Intercept)    19.739770 0.5492394 508 35.94019  0.0000
# elapsed_scaled  0.247910 0.1824291 368  1.35894  0.1750
# Groups          0.825667 0.9119464 508  0.90539  0.3657
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled -0.061       
# Groups         -0.605  0.085
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.28798412 -0.53697896 -0.07288492  0.46514121  3.20709280 
# 
# Number of Observations: 879
# Number of Groups: 510 



data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(mds3_tot_on ==max(mds3_tot_on )) %>% ungroup()




# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, mds3_tot_on = mds3_tot_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(mds3_tot_on=mds3_tot_on/10)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + mds3_tot_on + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + mds3_tot_on, 
#     data = data, cluster = ID)
# 
#   n= 632, number of events= 67 
# 
#                 coef exp(coef) se(coef) robust se     z Pr(>|z|)  
# Groups      0.301347  1.351678 0.266627  0.318122 0.947    0.344  
# mds3_tot_on 0.028161  1.028561 0.008571  0.011206 2.513    0.012 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# Groups          1.352     0.7398    0.7246     2.522
# mds3_tot_on     1.029     0.9722    1.0062     1.051
# 
# Concordance= 0.694  (se = 0.043 )
# Likelihood ratio test= 12.04  on 2 df,   p=0.002
# Wald test            = 9.05  on 2 df,   p=0.01
# Score (logrank) test = 13.81  on 2 df,   p=0.001,   Robust = 8.52  p=0.01
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)


# Fit survival curves based on the Cox model
surv_fit <- survival::survfit(cox_model_td, newdata = data.frame(Groups = c(0,1), hy_stage = mean(data$hy_stage)))


# -------
# Linear Mixed-model and time-varying Cox regression - Levodopa-experienced vs naive  MDS-UPDRS IIII ------

Echellesmdsupdrs_20250106 <- read_excel(path = "Echellesmdsupdrs_20250106.xlsx")

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% filter(!is.na(mds3_tot_on))

Echellesmdsupdrs_20250106 %>% group_by(anonyme_id) %>% count() %>%
  ungroup() %>% summarise(mean=mean(n))

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% select(anonyme_id, redcap_repeat_instance, mds3_tot_on)


df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")




names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, redcap_repeat_instance )

data_v <- data_v %>% inner_join(Echellesmdsupdrs_20250106) 


data_v$act_datedeb <- as.Date(data_v$act_datedeb)


first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(B==1, 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365

data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(mds3_tot_on ==max(mds3_tot_on )) %>% distinct()




library(nlme)

lme_model <- nlme::lme(fixed = mds3_tot_on ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)
 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   4219.737 4241.509 -2104.868
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:    8.041853 6.396301
# 
# Fixed effects:  mds3_tot_on ~ elapsed_scaled + Groups 
#                    Value Std.Error  DF   t-value p-value
# (Intercept)    19.449546 0.9069333 336 21.445399  0.0000
# elapsed_scaled  0.235714 0.2173995 239  1.084243  0.2793
# Groups          0.582621 1.1308903 336  0.515188  0.6068
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.194       
# Groups         -0.819 -0.244
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.30343568 -0.53717460 -0.09293278  0.49372614  2.34664523 
# 
# Number of Observations: 578
# Number of Groups: 338 




data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(mds3_tot_on ==max(mds3_tot_on )) %>% ungroup()



# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, mds3_tot_on = mds3_tot_on)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(mds3_tot_on=mds3_tot_on/10)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + mds3_tot_on + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + mds3_tot_on, 
#     data = data, cluster = ID)
# 
#   n= 437, number of events= 47 
# 
#                coef exp(coef) se(coef) robust se     z Pr(>|z|)  
# Groups      0.01717   1.01732  0.45111   0.64090 0.027    0.979  
# mds3_tot_on 0.33586   1.39914  0.11584   0.13805 2.433    0.015 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#             exp(coef) exp(-coef) lower .95 upper .95
# Groups          1.017     0.9830    0.2897     3.573
# mds3_tot_on     1.399     0.7147    1.0675     1.834
# 
# Concordance= 0.735  (se = 0.045 )
# Likelihood ratio test= 7.78  on 2 df,   p=0.02
# Wald test            = 5.97  on 2 df,   p=0.05
# Score (logrank) test = 8.56  on 2 df,   p=0.01,   Robust = 4.64  p=0.1
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).



survminer::ggforest(cox_model_td, data = data)



# --------



# Linear Mixed-model and time-varying Cox regression - Starting ON Levodopa vs not LEDD ------

LEDD <- read_excel(path = "Consultation_20250106.xlsx")


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale, ttt_ledd_amantadine, ttt_ledd_rasagiline, ttt_ledd_ago ) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago), ttt_ledd_rasagiline=as.numeric(ttt_ledd_rasagiline), 
         ttt_ledd_amantadine=as.numeric(ttt_ledd_amantadine), ttt_ledd_totale=as.numeric(ttt_ledd_totale)) %>%
  filter(!is.na(ttt_ledd_totale)) %>% 
  mutate(ttt_ledd_amantadine=ifelse(is.na(ttt_ledd_amantadine),0,ttt_ledd_amantadine)) %>%
  mutate(ttt_ledd_rasagiline=ifelse(is.na(ttt_ledd_rasagiline),0,ttt_ledd_rasagiline)) %>%
    mutate(ttt_ledd_ago=ifelse(is.na(ttt_ledd_ago),0,ttt_ledd_ago)) %>%
  mutate(ttt_ledd_totale=ttt_ledd_totale-ttt_ledd_amantadine-ttt_ledd_rasagiline-ttt_ledd_ago) %>%
  select(anonyme_id, act_datedeb, ttt_ledd_totale)


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale)



df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, redcap_repeat_instance, freezing )

data_v <- data_v %>% inner_join(LEDD %>% drop_na(), by=c("anonyme_id"="anonyme_id", "act_datedeb"="act_datedeb")) 

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$ttt_ledd_totale)

data_v$ttt_ledd_totale <- as.numeric(data_v$ttt_ledd_totale)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb, redcap_repeat_instance)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(Groups=="1- LD", 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365


data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% distinct()

data <- data %>% ungroup()

summary(data)

sum(is.na(data))

library(nlme)

lme_model <- nlme::lme(fixed = ttt_ledd_totale  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#       AIC      BIC    logLik
#   88308.5 88341.28 -44149.25
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:     1896.58 745.3125
# 
# Fixed effects:  ttt_ledd_totale ~ elapsed_scaled + Groups 
#                    Value Std.Error   DF   t-value p-value
# (Intercept)    287.45582  63.45066 3503  4.530383  0.0000
# elapsed_scaled  86.39223   6.92133 3503 12.482019  0.0000
# Groups          20.09533  96.34780 1690  0.208571  0.8348
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.006       
# Groups         -0.658  0.043
# 
# Standardized Within-Group Residuals:
#           Min            Q1           Med            Q3           Max 
# -36.935155692  -0.089379208  -0.008008115   0.076687434  42.723338652 
# 
# Number of Observations: 5196
# Number of Groups: 1692 



data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale ==max(ttt_ledd_totale )) %>% ungroup()




# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, ttt_ledd_totale = ttt_ledd_totale)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(ttt_ledd_totale=ttt_ledd_totale/100)

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale + cluster(ID), data = data)

# Show results
summary(cox_model_td)
# 
# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale, 
#     data = data, cluster = ID)
# 
#   n= 4240, number of events= 654 
# 
#                     coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Groups           0.79609   2.21686  0.08605   0.13864  5.742 9.35e-09 ***
# ttt_ledd_totale -0.04233   0.95856  0.01069   0.01508 -2.807  0.00501 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# Groups             2.2169     0.4511    1.6894    2.9090
# ttt_ledd_totale    0.9586     1.0432    0.9306    0.9873
# 
# Concordance= 0.624  (se = 0.022 )
# Likelihood ratio test= 88.43  on 2 df,   p=<2e-16
# Wald test            = 33.28  on 2 df,   p=6e-08
# Score (logrank) test = 75.72  on 2 df,   p=<2e-16,   Robust = 28.96  p=5e-07
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# -------
# Linear Mixed-model and time-varying Cox regression - Levodopa-experienced vs naive  LEDD ------

LEDD <- read_excel(path = "Consultation_20250106.xlsx")

LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale, ttt_ledd_amantadine, ttt_ledd_rasagiline, ttt_ledd_ago ) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago), ttt_ledd_rasagiline=as.numeric(ttt_ledd_rasagiline), 
         ttt_ledd_amantadine=as.numeric(ttt_ledd_amantadine), ttt_ledd_totale=as.numeric(ttt_ledd_totale)) %>%
  filter(!is.na(ttt_ledd_totale)) %>% 
  mutate(ttt_ledd_amantadine=ifelse(is.na(ttt_ledd_amantadine),0,ttt_ledd_amantadine)) %>%
  mutate(ttt_ledd_rasagiline=ifelse(is.na(ttt_ledd_rasagiline),0,ttt_ledd_rasagiline)) %>%
    mutate(ttt_ledd_ago=ifelse(is.na(ttt_ledd_ago),0,ttt_ledd_ago)) %>%
  mutate(ttt_ledd_totale=ttt_ledd_totale-ttt_ledd_amantadine-ttt_ledd_rasagiline-ttt_ledd_ago) %>%
  select(anonyme_id, act_datedeb, ttt_ledd_totale)


LEDD <- LEDD %>% drop_na()

LEDD$ttt_ledd_totale <- as.numeric(LEDD$ttt_ledd_totale)

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")




names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, redcap_repeat_instance )

data_v <- data_v %>% inner_join(LEDD %>% drop_na(), by=c("anonyme_id"="anonyme_id", "act_datedeb"="act_datedeb" )) 


data_v$act_datedeb <- as.Date(data_v$act_datedeb)


first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(B==1, 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365

data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% distinct()


library(nlme)

lme_model <- nlme::lme(fixed = ttt_ledd_totale  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)
 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC   logLik
#   50891.39 50921.24 -25440.7
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:    2471.634 990.9657
# 
# Fixed effects:  ttt_ledd_totale ~ elapsed_scaled + Groups 
#                    Value Std.Error   DF  t-value p-value
# (Intercept)    117.01632 161.79016 1910 0.723260  0.4696
# elapsed_scaled  78.37471  11.92421 1910 6.572740  0.0000
# Groups         275.67837 187.62850  984 1.469278  0.1421
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.108       
# Groups         -0.862 -0.092
# 
# Standardized Within-Group Residuals:
#           Min            Q1           Med            Q3           Max 
# -27.707193279  -0.066608698  -0.004945572   0.048647213  32.211985378 
# 
# Number of Observations: 2897
# Number of Groups: 986 

data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% ungroup()



# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, ttt_ledd_totale  = ttt_ledd_totale )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(ttt_ledd_totale =ttt_ledd_totale /100)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale  + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale, 
#     data = data, cluster = ID)
# 
#   n= 2516, number of events= 337 
# 
#                      coef exp(coef)  se(coef) robust se      z Pr(>|z|)  
# Groups           0.939165  2.557845  0.362923  0.461674  2.034   0.0419 *
# ttt_ledd_totale -0.003717  0.996289  0.010253  0.009124 -0.407   0.6837  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# Groups             2.5578      0.391    1.0349     6.322
# ttt_ledd_totale    0.9963      1.004    0.9786     1.014
# 
# Concordance= 0.502  (se = 0.024 )
# Likelihood ratio test= 9.14  on 2 df,   p=0.01
# Wald test            = 4.27  on 2 df,   p=0.1
# Score (logrank) test = 7.24  on 2 df,   p=0.03,   Robust = 6.54  p=0.04
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# --------



# Linear Mixed-model and time-varying Cox regression - Starting ON Levodopa vs not LD % of LEDD ------

LEDD <- read_excel(path = "Consultation_20250106.xlsx")


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale, ttt_ledd_amantadine, ttt_ledd_rasagiline, ttt_ledd_ago ) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago), ttt_ledd_rasagiline=as.numeric(ttt_ledd_rasagiline), 
         ttt_ledd_amantadine=as.numeric(ttt_ledd_amantadine), ttt_ledd_totale=as.numeric(ttt_ledd_totale)) %>%
  filter(!is.na(ttt_ledd_totale)) %>% 
  mutate(ttt_ledd_amantadine=ifelse(is.na(ttt_ledd_amantadine),0,ttt_ledd_amantadine)) %>%
  mutate(ttt_ledd_rasagiline=ifelse(is.na(ttt_ledd_rasagiline),0,ttt_ledd_rasagiline)) %>%
    mutate(ttt_ledd_ago=ifelse(is.na(ttt_ledd_ago),0,ttt_ledd_ago)) %>%
  mutate(ttt_ledd_totale=(ttt_ledd_totale-ttt_ledd_amantadine-ttt_ledd_rasagiline-ttt_ledd_ago)/ttt_ledd_totale) %>%
  select(anonyme_id, act_datedeb, ttt_ledd_totale)


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale) %>% drop_na()

range(LEDD$ttt_ledd_totale)


LEDD <- LEDD %>% filter(ttt_ledd_totale>=0)

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing, disease_duration))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, redcap_repeat_instance, freezing )

data_v <- data_v %>% left_join(LEDD %>% drop_na(), by=c("anonyme_id"="anonyme_id", "act_datedeb"="act_datedeb")) 

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$ttt_ledd_totale)

data_v$ttt_ledd_totale <- as.numeric(data_v$ttt_ledd_totale)

data_v <- data_v %>% mutate(ttt_ledd_totale=ifelse(is.na(ttt_ledd_totale),0,ttt_ledd_totale))


first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb, redcap_repeat_instance)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(Groups=="1- LD", 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365


data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% distinct()

data <- data %>% ungroup()

summary(data)

sum(is.na(data))

library(nlme)

lme_model <- nlme::lme(fixed = ttt_ledd_totale  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#       AIC      BIC    logLik
#   88308.5 88341.28 -44149.25
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:     1896.58 745.3125
# 
# Fixed effects:  ttt_ledd_totale ~ elapsed_scaled + Groups 
#                    Value Std.Error   DF   t-value p-value
# (Intercept)    287.45582  63.45066 3503  4.530383  0.0000
# elapsed_scaled  86.39223   6.92133 3503 12.482019  0.0000
# Groups          20.09533  96.34780 1690  0.208571  0.8348
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.006       
# Groups         -0.658  0.043
# 
# Standardized Within-Group Residuals:
#           Min            Q1           Med            Q3           Max 
# -36.935155692  -0.089379208  -0.008008115   0.076687434  42.723338652 
# 
# Number of Observations: 5196
# Number of Groups: 1692 



data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale ==max(ttt_ledd_totale )) %>% ungroup()




# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, ttt_ledd_totale = ttt_ledd_totale)

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(ttt_ledd_totale=ttt_ledd_totale)

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale + cluster(ID), data = data)

# Show results
summary(cox_model_td)
# 
# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale, 
#     data = data, cluster = ID)
# 
#   n= 4240, number of events= 654 
# 
#                     coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Groups           0.79609   2.21686  0.08605   0.13864  5.742 9.35e-09 ***
# ttt_ledd_totale -0.04233   0.95856  0.01069   0.01508 -2.807  0.00501 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# Groups             2.2169     0.4511    1.6894    2.9090
# ttt_ledd_totale    0.9586     1.0432    0.9306    0.9873
# 
# Concordance= 0.624  (se = 0.022 )
# Likelihood ratio test= 88.43  on 2 df,   p=<2e-16
# Wald test            = 33.28  on 2 df,   p=6e-08
# Score (logrank) test = 75.72  on 2 df,   p=<2e-16,   Robust = 28.96  p=5e-07
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# -------
# Linear Mixed-model and time-varying Cox regression - Levodopa-experienced vs naive  LD % of LEDD ------

LEDD <- read_excel(path = "Consultation_20250106.xlsx")


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale, ttt_ledd_amantadine, ttt_ledd_rasagiline, ttt_ledd_ago ) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago), ttt_ledd_rasagiline=as.numeric(ttt_ledd_rasagiline), 
         ttt_ledd_amantadine=as.numeric(ttt_ledd_amantadine), ttt_ledd_totale=as.numeric(ttt_ledd_totale)) %>%
  filter(!is.na(ttt_ledd_totale)) %>% 
  mutate(ttt_ledd_amantadine=ifelse(is.na(ttt_ledd_amantadine),0,ttt_ledd_amantadine)) %>%
  mutate(ttt_ledd_rasagiline=ifelse(is.na(ttt_ledd_rasagiline),0,ttt_ledd_rasagiline)) %>%
    mutate(ttt_ledd_ago=ifelse(is.na(ttt_ledd_ago),0,ttt_ledd_ago)) %>%
  mutate(ttt_ledd_totale=(ttt_ledd_totale-ttt_ledd_amantadine-ttt_ledd_rasagiline-ttt_ledd_ago)/ttt_ledd_totale) %>%
  select(anonyme_id, act_datedeb, ttt_ledd_totale)


LEDD <- LEDD %>% select(anonyme_id, act_datedeb, ttt_ledd_totale) %>% drop_na()

range(LEDD$ttt_ledd_totale)


LEDD <- LEDD %>% filter(ttt_ledd_totale>=0)


LEDD <- LEDD %>% drop_na()

LEDD$ttt_ledd_totale <- as.numeric(LEDD$ttt_ledd_totale)

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()



first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")




names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, redcap_repeat_instance )


data_v <- data_v %>% left_join(LEDD %>% drop_na(), by=c("anonyme_id"="anonyme_id", "act_datedeb"="act_datedeb")) 

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

unique(data_v$ttt_ledd_totale)

data_v$ttt_ledd_totale <- as.numeric(data_v$ttt_ledd_totale)

data_v <- data_v %>% mutate(ttt_ledd_totale=ifelse(is.na(ttt_ledd_totale),0,ttt_ledd_totale))



first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(B==1, 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365

data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% distinct()


library(nlme)

lme_model <- nlme::lme(fixed = ttt_ledd_totale  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)
 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC   logLik
#   50891.39 50921.24 -25440.7
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept) Residual
# StdDev:    2471.634 990.9657
# 
# Fixed effects:  ttt_ledd_totale ~ elapsed_scaled + Groups 
#                    Value Std.Error   DF  t-value p-value
# (Intercept)    117.01632 161.79016 1910 0.723260  0.4696
# elapsed_scaled  78.37471  11.92421 1910 6.572740  0.0000
# Groups         275.67837 187.62850  984 1.469278  0.1421
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.108       
# Groups         -0.862 -0.092
# 
# Standardized Within-Group Residuals:
#           Min            Q1           Med            Q3           Max 
# -27.707193279  -0.066608698  -0.004945572   0.048647213  32.211985378 
# 
# Number of Observations: 2897
# Number of Groups: 986 

data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(ttt_ledd_totale  ==max(ttt_ledd_totale  )) %>% ungroup()



# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, ttt_ledd_totale  = ttt_ledd_totale )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(ttt_ledd_totale =ttt_ledd_totale /100)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale  + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + ttt_ledd_totale, 
#     data = data, cluster = ID)
# 
#   n= 2516, number of events= 337 
# 
#                      coef exp(coef)  se(coef) robust se      z Pr(>|z|)  
# Groups           0.939165  2.557845  0.362923  0.461674  2.034   0.0419 *
# ttt_ledd_totale -0.003717  0.996289  0.010253  0.009124 -0.407   0.6837  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                 exp(coef) exp(-coef) lower .95 upper .95
# Groups             2.5578      0.391    1.0349     6.322
# ttt_ledd_totale    0.9963      1.004    0.9786     1.014
# 
# Concordance= 0.502  (se = 0.024 )
# Likelihood ratio test= 9.14  on 2 df,   p=0.01
# Wald test            = 4.27  on 2 df,   p=0.1
# Score (logrank) test = 7.24  on 2 df,   p=0.03,   Robust = 6.54  p=0.04
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# --------



# Linear Mixed-model and time-varying Cox regression - Starting ON Levodopa vs not LD % of Disease Duration ------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

first_visit <- df_complet  %>%  select(`anonyme_id...1`, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>% select(-c(freezing))

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

Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)

first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()

first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)


data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, redcap_repeat_instance, freezing )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)


first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
    mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)


first_to_second_visit <- first_to_second_visit %>% mutate(act_datedeb...5=as.Date(act_datedeb...5)) %>%
  mutate(act_datedeb=as.Date(act_datedeb)) %>%
  inner_join(
    df_complet %>% select(anonyme_id...1, act_datedeb...5, disease_duration) %>%
  rename("anonyme_id...1"="anonyme_id...1") %>%
  rename("act_datedeb"="act_datedeb...5")  %>%
  mutate(act_datedeb=as.Date(act_datedeb))
  )



first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb, redcap_repeat_instance)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(Groups=="1- LD", 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit




data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365


data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(disease_duration  ==max(disease_duration  )) %>%  distinct()

data <- data %>% ungroup()

summary(data)

sum(is.na(data))

library(nlme)

lme_model <- nlme::lme(fixed = disease_duration  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)

# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   13584.12 13619.58 -6787.062
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept)  Residual
# StdDev:    1.573914 0.2773958
# 
# Fixed effects:  disease_duration ~ elapsed_scaled + Groups 
#                   Value  Std.Error   DF  t-value p-value
# (Intercept)    3.465683 0.04252550 6486  81.4966       0
# elapsed_scaled 1.001368 0.00186603 6486 536.6295       0
# Groups         0.842526 0.06558476 2387  12.8464       0
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.018       
# Groups         -0.648  0.003
# 
# Standardized Within-Group Residuals:
#           Min            Q1           Med            Q3           Max 
# -2.6968641900 -0.6237339938 -0.0001680127  0.6220827767  2.7052043364 
# 
# Number of Observations: 8876
# Number of Groups: 2389 



data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(disease_duration  ==max(disease_duration  )) %>% ungroup()




# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, disease_duration  = disease_duration )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)

data <- data %>% mutate(disease_duration =disease_duration )

library(survival)

# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + disease_duration  + cluster(ID), data = data)

# Show results
summary(cox_model_td)

# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + disease_duration, 
#     data = data, cluster = ID)
# 
#   n= 6487, number of events= 1044 
# 
#                      coef exp(coef) se(coef) robust se      z Pr(>|z|)    
# Groups            0.62272   1.86400  0.06694   0.11430  5.448 5.09e-08 ***
# disease_duration -0.13410   0.87451  0.01822   0.02929 -4.578 4.70e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                  exp(coef) exp(-coef) lower .95 upper .95
# Groups              1.8640     0.5365    1.4899    2.3320
# disease_duration    0.8745     1.1435    0.8257    0.9262
# 
# Concordance= 0.591  (se = 0.02 )
# Likelihood ratio test= 113.8  on 2 df,   p=<2e-16
# Wald test            = 39.77  on 2 df,   p=2e-09
# Score (logrank) test = 112.3  on 2 df,   p=<2e-16,   Robust = 36.29  p=1e-08
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).

survminer::ggforest(cox_model_td, data = data)



# -------
# Linear Mixed-model and time-varying Cox regression - Levodopa-experienced vs naive  LD % of Disease Duration ------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()


first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")


names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing, redcap_repeat_instance )



data_v$act_datedeb <- as.Date(data_v$act_datedeb)


first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()

mean(first_to_second_visit$freezing)


first_to_second_visit


first_to_second_visit <- first_to_second_visit %>%
  inner_join(df_complet %>% select(anonyme_id...1, act_datedeb...5, disease_duration) %>%
  rename("act_datedeb"="act_datedeb...5")
)

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$act_datedeb <- as.Date(first_to_second_visit$act_datedeb)


first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))


first_to_second_visit <- first_to_second_visit %>% mutate(Groups=ifelse(B==1, 1, 0))

first_to_second_visit <- first_to_second_visit %>% ungroup()

data <- first_to_second_visit

df_complet


data$elapsed_scaled <- (data$elapsed - mean(data$elapsed)) / 365

data <- data %>% select(-elapsed) %>% distinct()

data <- data %>% group_by(anonyme_id...1, elapsed_scaled) %>%
  filter(Groups==max(Groups)) %>% 
  filter(freezing ==max(freezing )) %>%
  filter(disease_duration  ==max(disease_duration  )) %>% distinct()


library(nlme)

lme_model <- nlme::lme(fixed = disease_duration  ~ elapsed_scaled + Groups,
                 random = ~ 1 | anonyme_id...1,
                 data = data,
                 method = "REML")

summary(lme_model)
 
# Linear mixed-effects model fit by REML
#   Data: data 
#        AIC      BIC    logLik
#   7925.807 7958.623 -3957.904
# 
# Random effects:
#  Formula: ~1 | anonyme_id...1
#         (Intercept)  Residual
# StdDev:    1.588907 0.2731608
# 
# Fixed effects:  disease_duration ~ elapsed_scaled + Groups 
#                   Value  Std.Error   DF  t-value p-value
# (Intercept)    3.460536 0.06586525 3828  52.5396  0.0000
# elapsed_scaled 1.001138 0.00228523 3828 438.0907  0.0000
# Groups         0.220644 0.08629279 1407   2.5569  0.0107
#  Correlation: 
#                (Intr) elpsd_
# elapsed_scaled  0.037       
# Groups         -0.763 -0.026
# 
# Standardized Within-Group Residuals:
#          Min           Q1          Med           Q3          Max 
# -2.557983354 -0.621816402 -0.008816379  0.608896838  2.745929972 
# 
# Number of Observations: 5238
# Number of Groups: 1409 

data <- first_to_second_visit

data <- data %>% group_by(anonyme_id...1, elapsed) %>% 
  filter(Groups==max(Groups)) %>%
  filter(freezing ==max(freezing )) %>%
  filter(disease_duration  ==max(disease_duration  )) %>% ungroup()



# Rename columns for clarity
data <- data %>%
  rename(ID = anonyme_id...1, time = elapsed, disease_duration  = disease_duration )

# Sort data by patient ID and time
data <- data %>% arrange(ID, time)

# Create start-stop format
data <- data %>%
  group_by(ID) %>%
  mutate(Start = lag(time, default = 0),  # Start at 0 for first entry
         Stop = time) %>%
  ungroup()


data <- data %>% filter(Start < Stop)




# Fit the time-dependent Cox model
cox_model_td <- coxph(Surv(Start, Stop, freezing) ~ Groups + disease_duration  + cluster(ID), data = data)

# Show results
summary(cox_model_td)


# Call:
# coxph(formula = Surv(Start, Stop, freezing) ~ Groups + disease_duration, 
#     data = data, cluster = ID)
# 
#   n= 3814, number of events= 501 
# 
#                      coef exp(coef) se(coef) robust se      z Pr(>|z|)  
# Groups           -0.08225   0.92104  0.14126   0.20610 -0.399   0.6898  
# disease_duration -0.11523   0.89116  0.02629   0.04671 -2.467   0.0136 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                  exp(coef) exp(-coef) lower .95 upper .95
# Groups              0.9210      1.086    0.6150    1.3795
# disease_duration    0.8912      1.122    0.8132    0.9766
# 
# Concordance= 0.506  (se = 0.028 )
# Likelihood ratio test= 20.9  on 2 df,   p=3e-05
# Wald test            = 6.21  on 2 df,   p=0.04
# Score (logrank) test = 19.78  on 2 df,   p=5e-05,   Robust = 6.49  p=0.04
# 
#   (Note: the likelihood ratio and score tests assume independence of
#      observations within a cluster, the Wald and robust score tests do not).


survminer::ggforest(cox_model_td, data = data)



# --------



# Summary table ---------------

df_complet <- fread( "df_complet.txt")

length(unique(df_complet$anonyme_id...1)) # 25602

df_complet <- df_complet %>% filter(disease_duration<40) %>% filter(disease_duration>=0) %>% ungroup() %>%
  filter(hoehn_yahr_on>0) 

length(unique(df_complet$anonyme_id...1)) # 17319

first_visit <- df_complet  %>%  select(`anonyme_id...1`, B, `act_datedeb...5`,disease_duration, freezing) %>%
  filter(disease_duration<=5) %>% filter(freezing==0) %>% group_by(anonyme_id...1) %>%
  filter(B==0) %>%
  filter(`act_datedeb...5`==min(`act_datedeb...5`)) %>%  select(-c(freezing, disease_duration, B))


first_visit %>% ungroup() %>% left_join(
  df_complet %>% select(`anonyme_id...1`,  `act_datedeb...5`) %>% rename("v2_act_datedeb...5"="act_datedeb...5")
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




Groups <- df_complet %>%  mutate(Groups=ifelse(B==1, "1- LD", "2- Other")) %>%
  select(anonyme_id...1, act_datedeb...5, Groups)


Groups %>% group_by(Groups) %>% count()





first_to_second_visit <- first_to_second_visit %>% left_join(Groups) %>% 
  left_join(Groups %>% rename("v2_Groups"="Groups"), by=c("v2_act_datedeb...5"="act_datedeb...5", "anonyme_id...1"="anonyme_id...1")) 

first_to_second_visit <- first_to_second_visit %>% 
  group_by(anonyme_id...1) %>% filter(Groups==min(Groups)) %>% filter(v2_Groups==min(v2_Groups))

first_to_second_visit$act_datedeb...5 <- as.Date(first_to_second_visit$act_datedeb...5)
first_to_second_visit$v2_act_datedeb...5 <- as.Date(first_to_second_visit$v2_act_datedeb...5)

first_to_second_visit <- first_to_second_visit %>% distinct()

first_to_second_visit <- first_to_second_visit %>% ungroup()




first_to_second_visit <- first_to_second_visit %>% select(anonyme_id...1, act_datedeb...5, Groups)



B_groups <- df_complet %>% select(anonyme_id...1, B) %>% distinct() %>% group_by(anonyme_id...1) %>%
  summarise(B=max(B))


first_to_second_visit <- first_to_second_visit %>% left_join(B_groups) %>% ungroup()

unique(first_to_second_visit$B)

data_v <- read_excel(path = "Consultation_20250106.xlsx")

names(data_v)

data_v <- data_v %>% select(anonyme_id, act_datedeb, freezing )

data_v$act_datedeb <- as.Date(data_v$act_datedeb)

first_to_second_visit <- first_to_second_visit %>% left_join(data_v, by=c("anonyme_id...1"="anonyme_id")) %>%
  filter(act_datedeb>=act_datedeb...5) %>% mutate(freezing=ifelse(freezing==">=2", "2", freezing)) %>%
  mutate(freezing=as.numeric(freezing)) %>% drop_na()


mean(first_to_second_visit$freezing)

first_to_second_visit <- first_to_second_visit %>% mutate(elapsed=as.numeric(act_datedeb-act_datedeb...5)) %>%
  select(-c(act_datedeb...5, act_datedeb)) %>%
  arrange(anonyme_id...1, elapsed) %>% group_by(anonyme_id...1) %>%
  mutate(freezing=cumsum(freezing)) %>% mutate(freezing=ifelse(freezing==0,0,1))

filtered_data <- first_to_second_visit %>%
  group_by(anonyme_id...1) %>%
  mutate(first_freezing = which(freezing == 1)[1]) %>%
  filter(row_number() <= first_freezing | is.na(first_freezing)) %>%
  ungroup()

# View result
filtered_data

filtered_data <- filtered_data %>% select(-first_freezing)

length(unique(filtered_data$anonyme_id...1)) # 1409

pats_to_summary <- filtered_data %>% select(anonyme_id...1) %>% rename("anonyme_id"="anonyme_id...1")

data_v <- read_excel(path = "Consultation_20250106.xlsx")

data_v <- pats_to_summary %>% inner_join(data_v)

data_v <- data_v %>% distinct() 

data_v <- data_v %>% group_by(anonyme_id) %>% filter(redcap_repeat_instance==min(redcap_repeat_instance))

Inclusion_20241028 <- read_excel(path = "Inclusion_20250106.xlsx")

Inclusion_20241028 <- Inclusion_20241028 %>% inner_join(data_v %>% select(anonyme_id ) %>% distinct())

names(Inclusion_20241028)

Inclusion_20241028 %>% group_by(pat_sexe) %>% count()

Inclusion_20241028 %>% select(anonyme_id, pat_ddn_a, diag_date_a) %>%
  inner_join(data_v) %>% 
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year)) %>%
  select(anonyme_id, age, disease_dur) %>% drop_na() %>%
  summarise(mean=mean(age), 
            se=sd(age),
            median=median(age),
            q25=quantile(age, 0.25),
            q75=quantile(age, 0.75))


Inclusion_20241028 %>% select(anonyme_id, pat_ddn_a, diag_date_a) %>%
  inner_join(data_v) %>% 
  mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=12*(year-diag_date_a)) %>% select(-c(diag_date_a, year)) %>%
  select(anonyme_id, age, disease_dur) %>% drop_na() %>%
  summarise(mean=mean(disease_dur), 
            se=sd(disease_dur),
            median=median(disease_dur),
            q25=quantile(disease_dur, 0.25),
            q75=quantile(disease_dur, 0.75))


data_v %>% select(anonyme_id, hoehn_yahr_on) %>% 
  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  group_by(hoehn_yahr_on) %>% count() %>% drop_na() %>%
  filter(hoehn_yahr_on<=5&hoehn_yahr_on>0) %>% ungroup() %>%
  mutate(tot=sum(n)) %>% mutate(perc=n/tot)


data_v %>% ungroup() %>%
  filter(freezing=="0") %>% 
  mutate(fluct_motrice=ifelse(fluct_motrice==">=2",2,fluct_motrice)) %>%
  select(anonyme_id, fluct_motrice) %>% 
  mutate(fluct_motrice=as.numeric(fluct_motrice)) %>% drop_na() %>%
  group_by(fluct_motrice) %>% count() %>% drop_na() %>%
  ungroup() %>%
  mutate(tot=sum(n)) %>% mutate(perc=n/tot)


names(data_v)

data_v %>% ungroup() %>%
  mutate(chute_instab=ifelse(chute_instab==">=2",2,chute_instab)) %>%
  select(anonyme_id, chute_instab) %>% 
  mutate(chute_instab=as.numeric(chute_instab)) %>% drop_na() %>%
  group_by(chute_instab) %>% count() %>% drop_na() %>%
  ungroup() %>%
  mutate(tot=sum(n)) %>% mutate(perc=n/tot)



Echellesmdsupdrs_20250106 <- read_excel(path = "Echellesmdsupdrs_20250106.xlsx")

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% filter(!is.na(mds3_tot_on))

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% select(anonyme_id, redcap_repeat_instance, mds3_tot_on)

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% filter(redcap_repeat_instance==1) %>% select(-redcap_repeat_instance)

Echellesmdsupdrs_20250106 <- data_v %>%inner_join(Echellesmdsupdrs_20250106) 

Echellesmdsupdrs_20250106 %>% ungroup() %>%
    summarise(mean=mean(mds3_tot_on), 
            se=sd(mds3_tot_on),
            median=median(mds3_tot_on),
            q25=quantile(mds3_tot_on, 0.25),
            q75=quantile(mds3_tot_on, 0.75))



Echellesmdsupdrs_20250106 <- read_excel(path = "Echellesmdsupdrs_20250106.xlsx")

names(Echellesmdsupdrs_20250106)

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% select(anonyme_id, redcap_repeat_instance, mds3_rigd_cou_on:mds3_presence_dyskm)

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% filter(!is.na(mds3_tot_on))

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% select(anonyme_id, redcap_repeat_instance, mds3_rigd_cou_on:mds3_tot_on)

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% group_by(anonyme_id) %>% filter(redcap_repeat_instance==min(redcap_repeat_instance))

Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% inner_join(data_v %>% select(anonyme_id) %>% distinct())

# 347
Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>% ungroup() %>% select(-c(anonyme_id, redcap_repeat_instance))

Echellesmdsupdrs_20250106 <- as.numeric(Echellesmdsupdrs_20250106)


Echellesmdsupdrs_20250106 <- Echellesmdsupdrs_20250106 %>%  mutate_all(as.numeric)

names(Echellesmdsupdrs_20250106)

Echellesmdsupdrs_20250106 %>% ungroup() %>%  
  select(contains("rigd")) %>%
  ungroup() %>%
  mutate(rigd_sum = rowSums(select(., contains("rigd")), na.rm = TRUE)) %>%
  filter(rigd_sum!=0) %>%
  count() %>% mutate(n=n/347)


Echellesmdsupdrs_20250106 %>% ungroup() %>%  
  select(contains("trem_")) %>%
  ungroup() %>%
  mutate(rigd_sum = rowSums(select(., contains("trem_")), na.rm = TRUE)) %>%
  filter(rigd_sum!=0) %>%
  count() %>% mutate(n=n/347)


Echellesmdsupdrs_20250106 %>% ungroup() %>%  
  select(mds3_tap_doigtd_on, mds3_tap_doigtg_on, mds3_mvm_maind_on, mds3_mvm_maing_on, mds3_pron_maind_on, mds3_pron_maing_on,
         mds3_tap_oreild_on, mds3_tap_oreilg_on, mds3_agilite_jambed_on, mds3_agilite_jambeg_on) %>%
  ungroup() %>%
  mutate(rigd_sum = rowSums(select(., contains("mds")), na.rm = TRUE)) %>%
  filter(rigd_sum!=0) %>%
  count() %>% mutate(n=n/347)
 
 
Echellesmdsupdrs_20250106 %>%
  select(mds3_blocage_on, mds3_stab_posturale_on, mds3_posture_on) %>%
    ungroup() %>%
  mutate(rigd_sum = rowSums(select(., contains("mds")), na.rm = TRUE)) %>%
  filter(rigd_sum!=0) %>%
  count() %>% mutate(n=n/347)
 

data_v <- read_excel(path = "Consultation_20250106.xlsx")


data_v %>% group_by(anonyme_id) %>% count() %>%
  inner_join(pats_to_summary %>% select(anonyme_id) %>% distinct()) %>%
  ungroup() %>%
  summarise(mean=mean(n), sd=sd(n), median=median(n), q25=quantile(n,0.25), q75=quantile(n,0.75))



# ----------------



# Centers ------------

df_complet <- fread( "df_complet.txt")

data_v <- read_excel(path = "Consultation_20250106.xlsx")


length(unique(data_v$redcap_data_access_group))

data.frame(data_v %>% select(redcap_data_access_group, anonyme_id) %>% distinct() %>%
  inner_join(df_complet %>% rename("anonyme_id"="anonyme_id...1") %>% select(anonyme_id) %>% distinct()) %>%
  group_by(redcap_data_access_group) %>% count() %>% arrange(-n))


# ---------