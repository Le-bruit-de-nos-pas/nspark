
library(tidyverse) 
library(data.table)
library(readxl)

data_v <- read.csv("../../Données/Visites_v2.csv",sep=";",header=TRUE)

data_i <- read.csv("../../Données/Inclusion_20240610.csv",sep=";",header=TRUE)

unique(data_i$diag) 

data_i <- data_i %>% filter(diag=="MP") 

data_v <- data_v %>% inner_join(data_i %>% select(anonyme_id) %>% distinct())

data_v$ttt_ache <- ifelse(data_v$ttt_ache == "Non", "0", "1")

num_col <- colnames(data_v)
num_col <- as.data.frame(num_col)
num_col$num <- seq(1:1020)

data_v$pompe_date <- factor(data_v$pompe_date)
data_v$pompe_date_arret <- factor(data_v$pompe_date_arret)    
data_v$pompe_date_2 <- factor(data_v$pompe_date_2) 
data_v$pompe_date_arret_2 <- factor(data_v$pompe_date_arret_2)

names_df <- data.frame(names(data_v))

data_v <- data_v %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, ttt_novo___1:pompe_date_arret_2)

names(data_v)

# ttt_modopar_62_5_gel_yn___yes every 4 until ttt_quetiapine_400_yn___yes

                       
var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,
                       225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,
                       294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,
                       358,362,366,370,374,378,382,386,390,394,398,402) - 167

var_traitement_val <- var_traitement_yn + 3

sum(is.na(data_v$anonyme_id))

data_v <- data_v %>% filter(!is.na(anonyme_id))

length(unique(data_v$anonyme_id))

i <- 0
## On met oui si le patient a une valeur pour un médicament mais il a coché non 
for (pat in data_v$anonyme_id){
  i <- i+1
  print(i)
  ss_df <- data_v[data_v$anonyme_id == pat,]
  ss_df_yn <- subset(ss_df,select=var_traitement_yn)
  ss_df_val <- subset(ss_df,select=var_traitement_val)
  colnames(ss_df_val)<-colnames(ss_df_yn)
  ss_df_verif <- rbind(ss_df_yn,ss_df_val)
  ss_df_verif <- sapply(ss_df_verif,as.numeric)
  ss_df_verif <- as.data.frame(ss_df_verif)
  ss_df_corr <- colSums(ss_df_verif,na.rm=TRUE)
  data_v[data_v$anonyme_id == pat,var_traitement_yn] <- ifelse(ss_df_corr != 0, 1, 0)
}

write.csv(data_v,"../Fichiers csv/paulo_all_data_pd_drugs.csv",row.names=FALSE)

var_traitement_yn2 <- c(177,181,185,189,193,197,201,205,209,213,217,221,225,
                        229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,
                        302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,
                        366,370,374,378,382,386,390,394,398,402) -167

var_traitement_yn2 <- append( c(1, 239:241, 246:251), var_traitement_yn2) 


# anonyme id
# cible___2 until cible___3
# pompe_date until pompe_date_arret_2

ind_traitement <- subset(data_v, select=var_traitement_yn2)
new_traitement <- ind_traitement


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



liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)


df_ldopa <- data_v
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

liste_apo <- grepl("\\bAPOKINON\\b", data_v$ttt_autre_ldopa,ignore.case = TRUE)
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

df_complet <- df_complet %>% bind_cols(data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb ))

df_complet %>% group_by(B) %>% count()

write.csv(df_complet,"../Fichiers csv/paulo_all_data_pd_drugs_v2.csv",row.names=FALSE)

df_complet <- read.csv("../Fichiers csv/paulo_all_data_pd_drugs_v2.csv",sep=",",header=TRUE)

data_v <- read.csv("../../Données/Visites_v2.csv",sep=";",header=TRUE)

data.frame(df_complet %>% left_join(data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on )) %>%
  filter(hoehn_yahr_on %in% c("0", "1", "2", ">=2", "3", "4","5")) %>%
  # mutate(hoehn_yahr_on=parse_number(hoehn_yahr_on)) %>%
  mutate(hoehn_yahr_on=ifelse(hoehn_yahr_on==">=2","2", hoehn_yahr_on)) %>%
  filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
           grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
           grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
  filter(!is.na(hoehn_yahr_on)) %>%
  select(hoehn_yahr_on, A:F) %>%
  gather(Drug, ON, A:F) %>%
   group_by(hoehn_yahr_on, Drug, ON) %>% count() %>%
  spread(key=ON, value=n) %>%
  mutate(perc=`1`/(`1`+`0`))) 


data.frame(df_complet %>% left_join(data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on )) %>%
             filter(hoehn_yahr_on %in% c("0", "1", "2", ">=2", "3", "4","5")) %>%
             # mutate(hoehn_yahr_on=parse_number(hoehn_yahr_on)) %>%
             mutate(hoehn_yahr_on=ifelse(hoehn_yahr_on==">=2","2", hoehn_yahr_on)) %>%
             filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
                      grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
                      grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
             filter(!is.na(hoehn_yahr_on)) %>%
             select(hoehn_yahr_on, A:F) %>%
             gather(Drug, ON, A:F) %>%
             group_by(hoehn_yahr_on, Drug) %>% summarise(mean=mean(ON))) 


data.frame(df_complet %>% left_join(data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on )) %>%
             filter(hoehn_yahr_on %in% c("0", "1", "2", ">=2", "3", "4","5")) %>%
             mutate(hoehn_yahr_on=parse_number(hoehn_yahr_on)) %>%
             mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
             filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
                      grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
                      grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
             filter(!is.na(hoehn_yahr_on)) %>%
             select(hoehn_yahr_on, A:F) %>%
             gather(Drug, ON, A:F) %>%
             group_by(hoehn_yahr_on, Drug, ON)) %>%
  filter(Drug!="Anticholinestherasique") %>%
  mutate(Drug=ifelse(Drug=="A", "MAO Inhibitor",
                     ifelse(Drug=="B", "Levodopa",
                            ifelse(Drug=="C", "Dopamine Agonists",
                                   ifelse(Drug=="D", "Amantadine",
                                          ifelse(Drug=="E", "COMT inhibitor",
                                                 ifelse(Drug=="F", "Anticholinergics",
                                                        ifelse(Drug=="TO", "Oral Treatment",
                                                               ifelse(Drug=="SCP", "DBS",
                                                                      ifelse(Drug=="ASC", "Subcutaneous Apo",
                                                                             ifelse(Drug=="LGI", "Intestinal Gel",
                                                                                    ifelse(Drug=="Antideresseur", "Antidepressant", "Antipsychotic")))))))))))) %>%
  mutate(Drug=factor(Drug, levels=c("MAO Inhibitor","Levodopa","Dopamine Agonists","Amantadine","COMT inhibitor", "Oral Treatment",
                                    "DBS","Subcutaneous Apo","Intestinal Gel","Anticholinergics","Antidepressant", "Antipsychotic"))) %>%
  ggplot(aes(hoehn_yahr_on , ON, colour=Drug, fill=Drug)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, size=1.5, alpha=0.5) +
  theme_minimal() +
  xlab("\n Hoehn & Yarh") + ylab("Probability of being ON \n") +
  scale_colour_manual(values=c(
    "#1f78b4",  # MAO Inhibitor
    "#33a02c",  # Levodopa
    "#e31a1c",  # Dopamine Agonists
    "#ff7f00",  # Amantadine
    "#6a3d9a",  # COMT inhibitor
    "#b15928",  # Oral PD Treatment
    "#a6cee3",  # Deep Brain Stimulation
    "#b2df8a",  # Intestinal Levodopa Gel
    "#fb9a99",  # Subcutaneous Apomorphine
    "#fdbf6f",  # Anticholinergics
    "#cab2d6",  # Antidepresseur
    "#ffff99"   # Antipsychotique
  ))




data.frame(df_complet %>% left_join(data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on )) %>%
             filter(hoehn_yahr_on %in% c("1", "2", ">=2", "3", "4","5")) %>%
             mutate(hoehn_yahr_on=parse_number(hoehn_yahr_on)) %>%
             mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
             filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
                      grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
                      grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
             filter(!is.na(hoehn_yahr_on)) %>%
             select(hoehn_yahr_on, A:F) %>%
             gather(Drug, ON, A:F) %>%
             group_by(hoehn_yahr_on, Drug, ON)) %>%
  filter(Drug!="Anticholinestherasique") %>%
  mutate(Drug=ifelse(Drug=="A", "MAO Inhibitor",
                     ifelse(Drug=="B", "Levodopa",
                            ifelse(Drug=="C", "Dopamine Agonists",
                                   ifelse(Drug=="D", "Amantadine",
                                          ifelse(Drug=="E", "COMT inhibitor",
                                                 ifelse(Drug=="F", "Anticholinergics",
                                                        ifelse(Drug=="TO", "Oral Treatment",
                                                               ifelse(Drug=="SCP", "DBS",
                                                                      ifelse(Drug=="ASC", "Subcutaneous Apo",
                                                                             ifelse(Drug=="LGI", "Intestinal Gel",
                                                                                    ifelse(Drug=="Antideresseur", "Antidepressant", "Antipsychotic")))))))))))) %>%
  mutate(Drug=factor(Drug, levels=c("MAO Inhibitor","Levodopa","Dopamine Agonists","Amantadine","COMT inhibitor", "Oral Treatment",
                                    "DBS","Subcutaneous Apo","Intestinal Gel","Anticholinergics","Antidepressant", "Antipsychotic"))) %>%
  ggplot(aes(hoehn_yahr_on , ON, colour=Drug, fill=Drug)) +
  geom_smooth(method = "gam",   formula = y ~ s(x, k = 5), se = FALSE, size=1.5, alpha=0.5) +
  theme_minimal() +
  xlab("\n Hoehn & Yarh") + ylab("Probability of being ON \n") +
  scale_colour_manual(values=c(
    "#1f78b4",  # MAO Inhibitor
    "#33a02c",  # Levodopa
    "#e31a1c",  # Dopamine Agonists
    "#ff7f00",  # Amantadine
    "#6a3d9a",  # COMT inhibitor
    "#b15928",  # Oral PD Treatment
    "#a6cee3",  # Deep Brain Stimulation
    "#b2df8a",  # Intestinal Levodopa Gel
    "#fb9a99",  # Subcutaneous Apomorphine
    "#fdbf6f",  # Anticholinergics
    "#cab2d6",  # Antidepresseur
    "#ffff99"   # Antipsychotique
  ))



data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on) %>%
  mutate(act_datedeb=substr(act_datedeb, 7, 10)) %>%
  inner_join(data_i %>% select(anonyme_id, diag_date_a)) %>%
  mutate(elapsed=as.numeric(act_datedeb)-as.numeric(diag_date_a)) %>%
  filter(elapsed>=0&elapsed<=40) %>%
  inner_join(df_complet %>% select(-act_datedeb)) %>%
  filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
           grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
           grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
  select(elapsed, A:F) %>%
  gather(Drug, ON, A:F) %>%
  group_by(elapsed, Drug, ON) %>%
  filter(Drug!="Anticholinestherasique") %>%
  mutate(Drug=ifelse(Drug=="A", "MAO Inhibitor",
                     ifelse(Drug=="B", "Levodopa",
                            ifelse(Drug=="C", "Dopamine Agonists",
                                   ifelse(Drug=="D", "Amantadine",
                                          ifelse(Drug=="E", "COMT inhibitor",
                                                 ifelse(Drug=="F", "Anticholinergics",
                                                        ifelse(Drug=="TO", "Oral Treatment",
                                                               ifelse(Drug=="SCP", "DBS",
                                                                      ifelse(Drug=="ASC", "Subcutaneous Apo",
                                                                             ifelse(Drug=="LGI", "Intestinal Gel",
                                                                                    ifelse(Drug=="Antideresseur", "Antidepressant", "Antipsychotic")))))))))))) %>%
  mutate(Drug=factor(Drug, levels=c("MAO Inhibitor","Levodopa","Dopamine Agonists","Amantadine","COMT inhibitor", "Oral Treatment",
                                    "DBS","Subcutaneous Apo","Intestinal Gel","Anticholinergics","Antidepressant", "Antipsychotic"))) %>%
  ggplot(aes(elapsed , ON, colour=Drug, fill=Drug)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, size=1.5, alpha=0.5) +
  theme_minimal() +
  xlab("\n Disease duration (years)") + ylab("Probability of being ON \n") +
  scale_colour_manual(values=c(
    "#1f78b4",  # MAO Inhibitor
    "#33a02c",  # Levodopa
    "#e31a1c",  # Dopamine Agonists
    "#ff7f00",  # Amantadine
    "#6a3d9a",  # COMT inhibitor
    "#b15928",  # Oral PD Treatment
    "#a6cee3",  # Deep Brain Stimulation
    "#b2df8a",  # Intestinal Levodopa Gel
    "#fb9a99",  # Subcutaneous Apomorphine
    "#fdbf6f",  # Anticholinergics
    "#cab2d6",  # Antidepresseur
    "#ffff99"   # Antipsychotique
  ))





data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on) %>%
  mutate(act_datedeb=substr(act_datedeb, 7, 10)) %>%
  inner_join(data_i %>% select(anonyme_id, diag_date_a)) %>%
  mutate(elapsed=as.numeric(act_datedeb)-as.numeric(diag_date_a)) %>%
  filter(elapsed>=0&elapsed<=40) %>%
  inner_join(df_complet %>% select(-act_datedeb)) %>%
  filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
           grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
           grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
  select(elapsed, A:F) %>%
  gather(Drug, ON, A:F) %>%
  group_by(elapsed, Drug, ON) %>%
  filter(Drug!="Anticholinestherasique") %>%
  mutate(Drug=ifelse(Drug=="A", "MAO Inhibitor",
                     ifelse(Drug=="B", "Levodopa",
                            ifelse(Drug=="C", "Dopamine Agonists",
                                   ifelse(Drug=="D", "Amantadine",
                                          ifelse(Drug=="E", "COMT inhibitor",
                                                 ifelse(Drug=="F", "Anticholinergics",
                                                        ifelse(Drug=="TO", "Oral Treatment",
                                                               ifelse(Drug=="SCP", "DBS",
                                                                      ifelse(Drug=="ASC", "Subcutaneous Apo",
                                                                             ifelse(Drug=="LGI", "Intestinal Gel",
                                                                                    ifelse(Drug=="Antideresseur", "Antidepressant", "Antipsychotic")))))))))))) %>%
  mutate(Drug=factor(Drug, levels=c("MAO Inhibitor","Levodopa","Dopamine Agonists","Amantadine","COMT inhibitor", "Oral Treatment",
                                    "DBS","Subcutaneous Apo","Intestinal Gel","Anticholinergics","Antidepressant", "Antipsychotic"))) %>%
  ggplot(aes(elapsed , ON, colour=Drug, fill=Drug)) +
  geom_smooth(method = "gam", size=1.5, alpha=0.5, se=F) +
  theme_minimal() +
  xlab("\n Disease duration (years)") + ylab("Probability of being ON \n") +
  scale_colour_manual(values=c(
    "#1f78b4",  # MAO Inhibitor
    "#33a02c",  # Levodopa
    "#e31a1c",  # Dopamine Agonists
    "#ff7f00",  # Amantadine
    "#6a3d9a",  # COMT inhibitor
    "#b15928",  # Oral PD Treatment
    "#a6cee3",  # Deep Brain Stimulation
    "#b2df8a",  # Intestinal Levodopa Gel
    "#fb9a99",  # Subcutaneous Apomorphine
    "#fdbf6f",  # Anticholinergics
    "#cab2d6",  # Antidepresseur
    "#ffff99"   # Antipsychotique
  ))







data_v %>% select(anonyme_id,redcap_repeat_instance,act_datedeb, hoehn_yahr_on) %>%
  mutate(act_datedeb=substr(act_datedeb, 7, 10)) %>%
  inner_join(data_i %>% select(anonyme_id, diag_date_a)) %>%
  mutate(elapsed=as.numeric(act_datedeb)-as.numeric(diag_date_a)) %>%
  filter(elapsed>=0&elapsed<=20) %>%
  filter(hoehn_yahr_on %in% c("0", "1", "2", ">=2", "3", "4","5")) %>%
  mutate(hoehn_yahr_on=parse_number(hoehn_yahr_on)) %>%
  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) %>%
  filter(!is.na(hoehn_yahr_on)) %>%
  inner_join(df_complet %>% select(-act_datedeb)) %>%
  filter(grepl("2016",act_datedeb)|grepl("2017",act_datedeb)|grepl("2018",act_datedeb)|
           grepl("2019",act_datedeb)|grepl("2020",act_datedeb)|grepl("2021",act_datedeb)|
           grepl("2022",act_datedeb)|grepl("2023",act_datedeb)|grepl("2024",act_datedeb)) %>%
  select(elapsed, hoehn_yahr_on, A:F) %>%
  gather(Drug, ON, A:F) %>%
  filter(Drug=="B") %>%
  group_by(elapsed, hoehn_yahr_on) %>%
  summarise(mean=mean(ON)) %>%
  ggplot(aes(x = elapsed , y = hoehn_yahr_on, fill = mean)) +
  geom_raster() +
  scale_fill_gradient(low = "white", high = "midnightblue") +
  labs(
    title = "Levodopa",
    x = "\n Disease Duration",
    y = "Hoehn & Yarh \n",
    fill = "Probability"
  ) +
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
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 
