
library(tidyverse) 
library(data.table)
library(readxl)




# Merge visit data with demographics info -----------------

data_i <- read.csv("../../Données/Inclusion_20240610.csv",sep=";",header=TRUE)
data_v <- read.csv("../../Données/Visites_v2.csv",sep=";",header=TRUE)
resultat <- merge(data_i, data_v, by = "anonyme_id", all.x = TRUE)
# write.csv(resultat,"../Fichiers csv/data_merge.csv",row.names=FALSE)

# -----------

# Flowchart / cohort definition -------------------------

data <- read.table("../Fichiers csv/data_merge.csv",sep=",",header=TRUE)
data <- rename(data, pat_code_anonyme = anonyme_id)
data$X <- seq(1:nrow(data))

data <- data[,-c(16)] # suppression de la variable extraction date d'extraction de la base
data$pat_code_anonyme <- as.factor(data$pat_code_anonyme)

df_visite <- data[,c("X","pat_code_anonyme","act_datedeb")]
df_visite_na <- df_visite[is.na(df_visite$act_datedeb),]
df_visite_na$pat_code_anonyme <- factor(df_visite_na$pat_code_anonyme)
df_visite <- df_visite[!is.na(df_visite$act_datedeb),]
df_visite$Annee <- substr(df_visite$act_datedeb,7,10)
df_visite$Mois <- substr(df_visite$act_datedeb,4,5)
df_visite$Jour <- substr(df_visite$act_datedeb,1,2)

liste_pat_na <- levels(df_visite_na$pat_code_anonyme)
df_visite$pat_code_anonyme <- factor(df_visite$pat_code_anonyme)
liste_pat <- levels(df_visite$pat_code_anonyme)
pat_communs = liste_pat_na[liste_pat_na %in% liste_pat]
nombre_pat_communs = length(pat_communs)
print(nombre_pat_communs)

df_visite$Annee <- as.numeric(df_visite$Annee)
summary(df_visite$Annee)
visite_annee_err <- df_visite[is.na(df_visite$Annee),]
print(visite_annee_err)
visite_annee_err$Annee <- substr(visite_annee_err$act_datedeb,1,4)
visite_annee_err$Mois <- substr(visite_annee_err$act_datedeb,6,7)
visite_annee_err$Jour <- substr(visite_annee_err$act_datedeb,9,10)
visite_annee_err$Annee <- as.numeric(visite_annee_err$Annee)
df_visite[is.na(df_visite$Annee),] <- visite_annee_err


df_sup_24 <- df_visite[df_visite$Annee > 2024,]
df_inf_16 <- df_visite[df_visite$Annee < 2016,]
df_sup_24$pat_code_anonyme <- factor(df_sup_24$pat_code_anonyme)
df_inf_16$pat_code_anonyme <- factor(df_inf_16$pat_code_anonyme)

df_visite <- df_visite[df_visite$Annee <= 2024,]
df_visite <- df_visite[df_visite$Annee >= 2016,]
df_visite$pat_code_anonyme <- factor(df_visite$pat_code_anonyme)
liste_pat_24 <- levels(df_sup_24$pat_code_anonyme)
liste_pat_16 <- levels(df_inf_16$pat_code_anonyme)
pat_communs = liste_pat_16[liste_pat_16 %in% liste_pat_24]

liste_pat_24 <- levels(df_sup_24$pat_code_anonyme)
liste_pat_16 <- levels(df_inf_16$pat_code_anonyme)
liste_pat <- levels(df_visite$pat_code_anonyme)
pat_communs = liste_pat_16[liste_pat_16 %in% liste_pat]
pat_communs_2 = liste_pat_24[liste_pat_24 %in% liste_pat]
nombre_pat_communs = length(pat_communs) + length(pat_communs_2)


df_visite$Mois <- as.numeric(df_visite$Mois)
df_visite$Jour <- as.numeric(df_visite$Jour)
df_visite_24 <- df_visite[df_visite$Annee == 2024,]
df_visite_24_futur <- df_visite_24[df_visite_24$Mois > 6,] 
df_visite_24_6 <- df_visite_24[df_visite_24$Mois == 6 & df_visite_24$Jour > 10,]

df_visite_24_futur$X <- as.factor(df_visite_24_futur$X)
df_visite_etude <- subset(df_visite,!X %in% df_visite_24_futur$X) 
df_visite_etude$X <- as.factor(df_visite_etude$X)

df_visite_24_futur$pat_code_anonyme <- factor(df_visite_24_futur$pat_code_anonyme)
liste_pat_24 <- levels(df_visite_24_futur$pat_code_anonyme)
df_visite_etude$pat_code_anonyme <- factor(df_visite_etude$pat_code_anonyme)
liste_pat <- levels(df_visite_etude$pat_code_anonyme)
pat_communs_2 = liste_pat_24[liste_pat_24 %in% liste_pat]
nombre_pat_communs =length(pat_communs_2)
print(nombre_pat_communs)

data <- subset(data,X %in% df_visite_etude$X)
data$pat_code_anonyme <- factor(data$pat_code_anonyme)

df_visite_etude$pat_code_anonyme <- as.factor(df_visite_etude$pat_code_anonyme)


length(unique(data$pat_code_anonyme)) # 28705
data$diag <- as.factor(data$diag)
levels(data$diag)
data_MP <- data[data$diag =="MP",]
length(unique(data_MP$pat_code_anonyme)) # 22896

df_diag <- data[!is.na(data$diag),]
df_MP <- df_diag[df_diag$diag == "MP",]
length(unique(df_MP$pat_code_anonyme)) # 22895

df_MP$diag <- factor(df_MP$diag)

test_date = subset(df_MP,select=c("X","pat_code_anonyme","diag_date_a","act_datedeb"))
test_date$act_datedeb <- substr(test_date$act_datedeb, 7, 10)
test_date$act_datedeb <- as.numeric(test_date$act_datedeb)


date_diag_na <- test_date[is.na(test_date$diag_date_a), ]
date_diag_na <- rbind(test_date[test_date$diag_date_a == "DM",],date_diag_na)
date_diag_na$pat_code_anonyme <- factor(date_diag_na$pat_code_anonyme)

date_diag <- test_date[!is.na(test_date$diag_date_a), ]
date_diag <- date_diag[date_diag$diag_date_a != "DM",]
date_diag$diag_date_a <- as.numeric(date_diag$diag_date_a)
date_diag_0 <- date_diag[date_diag$diag_date_a == 0,]
date_diag_0$pat_code_anonyme <- factor(date_diag_0$pat_code_anonyme)
date_diag <- date_diag[date_diag$diag_date_a != 0,]
date_diag_na$pat_code_anonyme <- factor(date_diag_na$pat_code_anonyme)
date_diag$pat_code_anonyme <- factor(date_diag$pat_code_anonyme)

test_date <- test_date[!is.na(test_date$diag_date_a), ]
test_date <- test_date[test_date$diag_date_a != "DM",]
test_date <- test_date[test_date$diag_date_a != "0",]
test_date$diag_date_a <- as.numeric(test_date$diag_date_a)

test_date$diff <- test_date$act_datedeb - test_date$diag_date_a

code_7 = test_date[test_date$diff >= 7,]
code_7$pat_code_anonyme <- factor(code_7$pat_code_anonyme)

pat_5 <- levels(code_7$pat_code_anonyme)

data_hy <- subset(df_MP,X %in% code_7$X)

length(unique(data_hy$pat_code_anonyme)) # 

unique(data_hy$hoehn_yahr_on)

data_hy <- data_hy %>% filter(hoehn_yahr_on %in% c("4", "5"))

length(unique(data_hy$pat_code_anonyme)) # 1531

df_visite_hy <- data_hy[,names(data_hy) %in% c("X","pat_code_anonyme","act_datedeb")]
df_visite_hy$Annee <- substr(df_visite_hy$act_datedeb,7,10)
df_visite_hy$Mois <- substr(df_visite_hy$act_datedeb,4,5)
df_visite_hy$Jour <- substr(df_visite_hy$act_datedeb,1,2)
df_visite_hy$Annee <- as.numeric(df_visite_hy$Annee)
df_visite_hy$Mois <- as.numeric(df_visite_hy$Mois)
df_visite_hy$Jour <- as.numeric(df_visite_hy$Jour)
df_visite_hy$pat_code_anonyme <- as.factor(df_visite_hy$pat_code_anonyme)
df_visite_hy$act_datedeb <- as.Date(df_visite_hy$act_datedeb, format = "%d/%m/%Y")
permutation <- order(df_visite_hy$act_datedeb)
df_ord_visite <- df_visite_hy[permutation, ]
df_ord_visite$ecart <- 0

setDT(data_hy)

nb_suivi <- 0 
df_ord_visite$Num_visite <- 1

niveaux_pat_hy <- levels(factor(data_hy$pat_code_anonyme))


# Calcul de l'écart entre chaque visite (en mois)
for (pat in niveaux_pat_hy){
  ss_df <- df_ord_visite[df_ord_visite$pat_code_anonyme == pat,]
  if (nrow(ss_df) != 1){
    ss_df$Num_visite <- seq(1:nrow(ss_df))
    nb_suivi <- nb_suivi + 1
    for (i in 2:nrow(ss_df)) {
      if (ss_df$Annee[i] == ss_df$Annee[i-1]){
        ss_df$ecart[i] <- ss_df$Mois[i] - ss_df$Mois[i-1]
      } else {
        an <- ss_df$Annee[i] - ss_df$Annee[i-1]
        mois <- ss_df$Mois[i] - ss_df$Mois[i-1]
        ss_df$ecart[i] <- 12*an + mois
      }
    }
  }
  df_ord_visite[df_ord_visite$pat_code_anonyme == pat,] <- ss_df
}


data_late <- data_hy

data_late <- data_late[data_late$pat_ddn_a <= data_late$diag_date_a,]
data_late <- data_late[!is.na(data_late$pat_code_anonyme),]

length(unique(data_late$pat_code_anonyme)) # 1526


# ----------------

# Medications combinations ---------------

# !!!!!!!!!!!!!!!!!!!!!!!!!
# Rerun code above to get "data_late" new version on advanced PD
# !!!!!!!!!!!!!!!!!!!!!!!!!


  
  
Pats_2_visits <- data_late %>% select(pat_code_anonyme, act_datedeb) %>% distinct() %>%
  group_by(pat_code_anonyme) %>% count() %>% filter(n>1) %>% ungroup() %>% 
  select(pat_code_anonyme) %>%
  left_join(data_late %>% select(pat_code_anonyme, act_datedeb) %>% distinct()) %>%
  mutate(act_datedeb=as.Date(act_datedeb, format="%d/%m/%Y")) %>%
  arrange(pat_code_anonyme, act_datedeb) %>%
  group_by(pat_code_anonyme) %>% 
  mutate(
    Elapsed_Time_Months = if_else(
      is.na(lag(act_datedeb)), 
      NA_real_, 
      lubridate::interval(lag(act_datedeb), act_datedeb) %/% months(1)
    )
  ) %>% ungroup() %>% 
  mutate(Elapsed_Time_Months=ifelse(is.na(Elapsed_Time_Months),0,Elapsed_Time_Months)) %>%
  group_by(pat_code_anonyme) %>%
  mutate(cumElapsed=cumsum(Elapsed_Time_Months)) %>%
  filter(cumElapsed==0| (cumElapsed>6&cumElapsed<24) ) %>% slice(1:2) %>%
  select(pat_code_anonyme, act_datedeb) %>% distinct()

Pats_2_visits <- Pats_2_visits %>% count() %>% filter(n==2) %>% select(pat_code_anonyme) %>%
  left_join(Pats_2_visits)

data_late <- data_late %>% mutate(act_datedeb=as.Date(act_datedeb, format="%d/%m/%Y")) 

data_late <- Pats_2_visits %>% ungroup() %>% left_join(data_late)


 
# data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>%
#   filter(act_datedeb==min(act_datedeb)) %>% slice(1) %>% ungroup()

data_late$ttt_ache <- ifelse(data_late$ttt_ache == "Non", "0", "1")

length(unique(data_late$pat_code_anonyme))

num_col <- colnames(data_late)
num_col <- as.data.frame(num_col)

num_col$num <- seq(1:1128)

data_late$pompe_date <- factor(data_late$pompe_date)
data_late$pompe_date_arret <- factor(data_late$pompe_date_arret)    
data_late$pompe_date_2 <- factor(data_late$pompe_date_2) 
data_late$pompe_date_arret_2 <- factor(data_late$pompe_date_arret_2)


var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402)

var_traitement_val <- var_traitement_yn + 3


for (pat in data_late$X){
  ss_df <- data_late[data_late$X == pat,]
  ss_df_yn <- subset(ss_df,select=var_traitement_yn)
  ss_df_val <- subset(ss_df,select=var_traitement_val)
  colnames(ss_df_val)<-colnames(ss_df_yn)
  ss_df_verif <- rbind(ss_df_yn,ss_df_val)
  ss_df_verif <- sapply(ss_df_verif,as.numeric)
  ss_df_verif <- as.data.frame(ss_df_verif)
  ss_df_corr <- colSums(ss_df_verif,na.rm=TRUE)
  data_late[data_late$X == pat,var_traitement_yn] <-  as.data.frame(t(ifelse(ss_df_corr != 0, 1, 0)))
}



var_traitement_yn2 <- c(1,1128,177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402,406:408,413:418)
ind_traitement <- subset(data_late, select=var_traitement_yn2)
new_traitement <- ind_traitement

typeof(new_traitement)

new_traitement <- data.frame(new_traitement)

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


library(data.table)
library(tidyverse)

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)

# data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>% filter(act_datedeb==min(act_datedeb)) %>% ungroup()

df_ldopa <- data_late

df_ldopa <- df_ldopa %>% select(ttt_autre_ldopa, X,pat_code_anonyme )


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

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)
df_apokinon <- df_ldopa[liste_apo,]
liste_apo_stylo <- df_apokinon[grepl("\\bSTYLO\\b", df_apokinon$ttt_autre_ldopa,ignore.case = TRUE),]$pat_code_anonyme
liste_apo_sc <- df_apokinon[!df_apokinon$pat_code_anonyme %in% liste_apo_stylo,]$pat_code_anonyme
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_stylo,]$N_park_dop_agonistes <- 1
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_sc,]$N_park_ASC <- 1

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


df_ldopa$Erreur <- rowSums(df_ldopa[,-c(1:3)],na.rm = TRUE )
print(df_ldopa[df_ldopa$Erreur == 0 & !is.na(df_ldopa$ttt_autre_ldopa),])


df_complet <- new_traitement

new_traitement_all <- new_traitement

df_complet[is.na(df_complet)] <- 0

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




df_erreur <- df_complet
df_erreur$Somme <- rowSums(df_erreur[,c(3:11,15)])
df_erreur[df_erreur$Somme == 0,]


new_traitement <- new_traitement[ ,names(new_traitement) %in% c("X","pat_code_anonyme","A","B","C","D","E","TO","SCP","LGI","ASC","Psy","ttt_gutron_yn___yes")]

df_complet <- df_complet[ ,names(df_complet) %in% c("X","pat_code_anonyme","A","B","C","D","E","F","TO","SCP","LGI","ASC","Antipsychotique","Anticholinestherasique","Antidepresseur","Nb_Autre")]


df_complet

length(unique(df_complet$pat_code_anonyme)) 







ignore <- Pats_2_visits %>%
  left_join(data_hy %>% mutate(act_datedeb=as.Date(act_datedeb, format=("%d/%m/%Y")))) %>%
  select(pat_code_anonyme, dyskinesie, douleur, dysarthrie, chute_instab,
         freezing, somnolence, deform_post, tr_degl, chute, fatigue, rbd,sas,sjsr,
         hypotension, apathie, depression, anxiete, halluc_psy, tci, punding, tr_cognitif)


ignore <- ignore %>%
  mutate(across(everything(), ~ ifelse(. == ">=2", "2", .))) %>%
  mutate(across(everything(), ~ ifelse(. == "Oui", "1", .))) %>%
  mutate(across(everything(), ~ ifelse(. == "Non", "0", .)))

ignore <- ignore %>% mutate(across(dyskinesie:tr_cognitif , as.numeric))

ignore <- df_complet %>% group_by(pat_code_anonyme) %>%
  mutate(X=row_number()) %>% ungroup() %>% bind_cols(ignore) 


ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17)) %>%
  gather(Drug, ON, A:F) %>%
  arrange(pat_code_anonyme...1) 


ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17)) %>%
  filter(X ==1 | X==2) %>%
  select(pat_code_anonyme...1, X, A:F) %>%
  gather(Drug, ON, A:F) %>%
  group_by(X, Drug) %>% summarise(ON=mean(ON)) %>%
  spread(key=X, value=ON)


ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17)) %>%
  gather(Drug, ON, A:F) %>%
  arrange(pat_code_anonyme...1) %>%
  gather(symptom, YES, dyskinesie :tr_cognitif ) %>%
  group_by(X, Drug, ON, symptom) %>%
  filter(X ==1 | X==2) %>%
  summarise(YES=mean(YES, na.rm=T)) %>%
  filter(Drug=="A") %>%
  spread(key=X, value=YES) %>%
  arrange(Drug, symptom)

exportignore <- data.frame(ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
                             select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
                             group_by(pat_code_anonyme...1) %>%
                             mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
                             ungroup() %>%
                             select(pat_code_anonyme...1, starts_with("diff_")) %>%
                             bind_cols(ignore %>% select(pat_code_anonyme...1, X:F)) %>%
                             filter(X ==1 | X==2) %>% ungroup() %>%
                             gather(Drug, ON, A:F) %>%
                             gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
                             filter(X==1) %>%
                             select(pat_code_anonyme...1,Drug, ON) %>%
                             bind_cols(
                               ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
                                 select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
                                 group_by(pat_code_anonyme...1) %>%
                                 mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
                                 ungroup() %>%
                                 select(pat_code_anonyme...1, starts_with("diff_")) %>%
                                 bind_cols(ignore %>% select(pat_code_anonyme...1, X:F)) %>%
                                 filter(X ==1 | X==2) %>% ungroup() %>%
                                 gather(Drug, ON, A:F) %>%
                                 gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
                                 filter(X==2) %>%
                                 select(pat_code_anonyme...1,symptom, YES) 
                             ) %>%
                             group_by(Drug, ON, symptom) %>%
                             summarise(YES=mean(YES, na.rm=T)) %>%
                             filter(Drug!="Anticholinestherasique") %>%
                             spread(key=Drug, value=YES) %>%
                             arrange(symptom))

fwrite(exportignore, "exportignore.csv")




# ignore <- ignore %>% 
#   mutate(G1=ifelse(B==1,1,0)) %>%
#   mutate(G4=ifelse(TO==1&SCP==1,1,0)) %>%
#   mutate(G5=ifelse(TO==1&(LGI==1|ASC==1),1,0)) %>%
#   mutate(G2=ifelse( (B==1&D==1)|
#                       (B==1&C==1&E==1)|
#                       (B==1&D==1&E==1)|
#                       (B==1&C==1&D==1&E==1)|
#                       (A==1&B==1&C==1&D==1)|
#                       (A==1&B==1&C==1&D==1&E==1)|
#                       (A==1&B==1&D==1)|
#                       (B==1&D==1&F==1)|
#                       (B==1&D==1&C==1&F==1)|
#                       (B==1&D==1&E==1&F==1)|
#                       (B==1&A==1&D==1&E==1),1,0)) %>%
#   mutate(G3=ifelse( (B==1&C==1)|
#                       (B==1&E==1)|
#                       (B==1&D==1&E==1)|
#                       (B==1&A==1)|
#                       (E==1&B==1&C==1)|
#                       (A==1&B==1&C==1)|
#                       (A==1&B==1&E==1)|
#                       (B==1&A==1&C==1&E==1)|
#                       (B==1&C==1&F==1)|
#                       (B==1&F==1)|
#                       (B==1&E==1&F==1),1,0)) %>%
#   select(-c(A,B,C,D,E,F,TO,SCP,LGI,ASC,Antipsychotique,Anticholinestherasique,Antidepresseur))
#   
# 
# exportignore <- data.frame(ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
#                              select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
#                              group_by(pat_code_anonyme...1) %>%
#                              mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
#                              ungroup() %>%
#                              select(pat_code_anonyme...1, starts_with("diff_")) %>%
#                              bind_cols(ignore %>% select(pat_code_anonyme...1, X, G1, G2, G3, G4, G5)) %>%
#                              filter(X ==1 | X==2) %>% ungroup() %>%
#                              gather(Drug, ON, G1, G2, G3, G4, G5) %>%
#                              gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
#                              filter(X==1) %>%
#                              select(pat_code_anonyme...1,Drug, ON) %>%
#                              bind_cols(
#                                ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
#                                  select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
#                                  group_by(pat_code_anonyme...1) %>%
#                                  mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
#                                  ungroup() %>%
#                                  select(pat_code_anonyme...1, starts_with("diff_")) %>%
#                                  bind_cols(ignore %>% select(pat_code_anonyme...1, X, G1, G2, G3, G4, G5)) %>%
#                                  filter(X ==1 | X==2) %>% ungroup() %>%
#                                  gather(Drug, ON, G1, G2, G3, G4, G5) %>%
#                                  gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
#                                  filter(X==2) %>%
#                                  select(pat_code_anonyme...1,symptom, YES) 
#                              ) %>%
#                              group_by(Drug, ON, symptom) %>%
#                              summarise(YES=mean(YES, na.rm=T)) %>%
#                              spread(key=Drug, value=YES) %>%
#                              arrange(symptom))
# 
# fwrite(exportignore, "exportignore.csv")




ignore2 <- ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
  select(pat_code_anonyme...1, X, dyskinesie:tr_cognitif) %>%
  filter(X==1 | X==2) %>%
  group_by(X) %>%
  summarize(across(dyskinesie:tr_cognitif, ~ mean(., na.rm = TRUE), .names = "mean_{.col}")) 

fwrite(ignore2, "ignore2.csv")






to_model <- ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
  select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
  group_by(pat_code_anonyme...1) %>%
  mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
  ungroup() %>%
  select(pat_code_anonyme...1, starts_with("diff_")) %>%
  bind_cols(ignore %>% select(pat_code_anonyme...1, X:F)) %>%
  filter(X ==1 | X==2) %>% ungroup() %>%
  gather(Drug, ON, A:F) %>%
  gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
  filter(X==1) %>%
  select(pat_code_anonyme...1,Drug, ON) %>%
  bind_cols(
    ignore %>% select(-c(Nb_Autre, pat_code_anonyme...17))  %>%
      select(pat_code_anonyme...1, dyskinesie:tr_cognitif) %>%
      group_by(pat_code_anonyme...1) %>%
      mutate(across(dyskinesie:tr_cognitif, ~ . - lag(.), .names = "diff_{.col}")) %>%
      ungroup() %>%
      select(pat_code_anonyme...1, starts_with("diff_")) %>%
      bind_cols(ignore %>% select(pat_code_anonyme...1, X:F)) %>%
      filter(X ==1 | X==2) %>% ungroup() %>%
      gather(Drug, ON, A:F) %>%
      gather(symptom, YES, diff_dyskinesie:diff_tr_cognitif ) %>%
      filter(X==2) %>%
      select(pat_code_anonyme...1,symptom, YES) 
  ) %>%
  select(pat_code_anonyme...1, Drug, ON, symptom, YES) %>%
  spread(key=Drug, value=ON)



unique(to_model$symptom)

to_model_2 <- to_model %>% filter(symptom=="diff_anxiete") %>% 
  select(-c(pat_code_anonyme...1, symptom)) %>%
  drop_na()

to_model_2$YES <- as.factor(to_model_2$YES)

library("randomForest")
modelAll_1_randomForest <- randomForest(YES ~ ., data = to_model_2)
summary(modelAll_1_randomForest)



data.frame(predict(modelAll_1_randomForest, to_model_2)) %>%
  bind_cols(to_model_2) %>%
  ggplot(aes(YES, predict.modelAll_1_randomForest..to_model_2.)) +
  geom_smooth(fill="deepskyblue4", colour="firebrick",method="lm") +
  xlim(-3,3) + ylim(-3,3) +
  theme_minimal() +
  xlab("\nObeserved Change Anxiety Score")  +
  ylab("Predicted Change in Anxiety Score\n")




data.frame(predict(modelAll_1_randomForest, to_model_2)) %>%
  bind_cols(to_model_2) %>%
  filter(YES!=0) %>%
  select(YES, predict.modelAll_1_randomForest..to_model_2.) %>%
  gather(Which, score, YES:predict.modelAll_1_randomForest..to_model_2.) %>%
  mutate(Which=ifelse(Which=="predict.modelAll_1_randomForest..to_model_2.", "Predicted", "Observed")) %>%
  ggplot(aes(score, fill=Which, colour=Which)) +
  geom_density(alpha=0.5)  +
  theme_minimal() +
  xlab("\nObeserved|Predicted Score")  + ylab("Patient density\n") +
  scale_fill_manual(values=c("deepskyblue4","firebrick")) +
  scale_colour_manual(values=c("deepskyblue4","firebrick")) 


ignore <- Pats_2_visits %>%
  left_join(data_hy %>% mutate(act_datedeb=as.Date(act_datedeb, format=("%d/%m/%Y"))))

names(ignore)




med <- df_complet[,-c(1,2,12:14,16)]

med[is.na(med)] <- 0

combinations <- apply(med, 1, paste, collapse = "")

freq <- table(combinations)

freq_sorted <- sort(freq, decreasing = TRUE)

top_combinations <- head(freq_sorted, 10)

# Ajustement des marges
old_par <- par(mar = c(5, 10, 4, 2) + 0.1)  # Augmente la marge gauche (second argument) pour les étiquettes

# Création du barplot transposé avec étiquettes verticales
barplot(top_combinations, main = "Top 10 des Combinaisons les Plus Fréquentes",
        xlab = "Fréquence", col = "skyblue", las = 1, horiz = TRUE)

# Réinitialisation des paramètres graphiques
par(old_par)


med$combinations <- combinations
med$X <- df_complet$X
med$Nb_Autres <- df_complet$Nb_Autre
med$Antipsychotique <- df_complet$Antipsychotique
med$Anticholinestherasique <- df_complet$Anticholinestherasique
med$Antidepresseur <- df_complet$Antidepresseur
head(med)

#write.csv(med,"../Fichiers csv/Obj_Principal/merge_tab_croise_ind.csv",row.names=FALSE)

head(df_complet)
df_complet[is.na(df_complet)] <- 0

# on regarde les combinaisons à la baseline
med <- df_complet[,-c(1,2,12:14,16)]


combinations <- apply(med, 1, paste, collapse = "")
freq <- table(combinations)

freq_sorted <- sort(freq, decreasing = TRUE)

top_combinations <- head(freq_sorted, 10)
# Ajustement des marges
old_par <- par(mar = c(5, 10, 4, 2) + 0.1)  # Augmente la marge gauche (second argument) pour les étiquettes

# Création du barplot transposé avec étiquettes verticales
barplot(top_combinations, main = "Top 10 des Combinaisons les Plus Fréquentes",
        xlab = "Fréquence", col = "skyblue", las = 1, horiz = TRUE)

# Réinitialisation des paramètres graphiques
par(old_par)


med$combinations <- combinations
med$pat_code_anonyme <- df_complet[,]$pat_code_anonyme
med$Nb_Autres <- df_complet[,]$Nb_Autre
med$Antipsychotique <- df_complet[,]$Antipsychotique
med$Anticholinestherasique <- df_complet[,]$Anticholinestherasique
med$Antidepresseur <- df_complet[,]$Antidepresseur
head(med)


med$combinations <- as.factor(med$combinations)
nb_combi <- levels(med$combinations)

df <- med[0,c(1:11)]
head(df)

for (combi in nb_combi){
  ss_df <- med[med$combinations == combi,c(1:11)]
  ss_df$effectif <- nrow(ss_df)
  ss_df$pourcentage <- ss_df$effectif/nrow(med)
  df <- rbind(df,ss_df[1,])
}

rownames(df) <- df$combinations


importance_comb <- order(-df$effectif)
df_ord <- df[importance_comb,]

names(med)

sum(med$A)/1526
sum(med$B)/1526
sum(med$C)/1526
sum(med$D)/1526
sum(med$E)/1526
sum(med$TO)/1526
sum(med$SCP)/1526
sum(med$LGI)/1526
sum(med$ASC)/1526
sum(med$F)/1526
sum(med$Antipsychotique)/1526
sum(med$Antidepresseur)/1526



data.frame(med %>% group_by(combinations) %>% count() %>% arrange(-n) %>%
             mutate(n=n/1526) %>% mutate(nn=cumsum(n)))

dataframe <- med %>% select(A, B, C, D, E )

names(dataframe) <- c("MAO Inhibitor","Levodopa","Dopamine Agonist",
                      "Amantadine", "COMT Inhibitor")

setDT(dataframe)

library(UpSetR)

# 500 x 300
upset(dataframe, 
      sets=colnames(dataframe),
      keep.order = T,
      nsets = length(colnames(dataframe)), nintersects = NA,
      matrix.dot.alpha = 0.7,
      mainbar.y.label = "Patient Count", point.size = 1,text.scale = 1,
      line.size = 0.5, mb.ratio = c(0.7, 0.3),
      sets.bar.color = "black", 
      main.bar.color = "black",
      shade.color = "white")


dataframe <- med %>% select(TO, SCP, LGI, ASC)


names(dataframe) <- c("Oral PD Treatment","Deep Brain Stim","Intestinal Gel",
                      "Subcutaneous Apomorphine")

setDT(dataframe)

library(UpSetR)


upset(dataframe, 
      sets=colnames(dataframe),
      keep.order = T,
      nsets = length(colnames(dataframe)), nintersects = NA,
      matrix.dot.alpha = 0.7,
      mainbar.y.label = "Patient Count", point.size = 1,text.scale = 1,
      line.size = 0.5, mb.ratio = c(0.7, 0.3),
      sets.bar.color = "black", 
      main.bar.color = "black",
      shade.color = "white")

# ------------

# Indivial drug level, to cluster/neural net - run both flocharts beforehand --------------

data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>%
  filter(act_datedeb==min(act_datedeb)) %>% slice(1) %>% ungroup()

data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>%
  filter(act_datedeb==min(act_datedeb)) %>% slice(1) %>% ungroup()

data_late$ttt_ache <- ifelse(data_late$ttt_ache == "Non", "0", "1")

length(unique(data_late$pat_code_anonyme))

num_col <- colnames(data_late)
num_col <- as.data.frame(num_col)

num_col$num <- seq(1:1128)

data_late$pompe_date <- factor(data_late$pompe_date)
data_late$pompe_date_arret <- factor(data_late$pompe_date_arret)    
data_late$pompe_date_2 <- factor(data_late$pompe_date_2) 
data_late$pompe_date_arret_2 <- factor(data_late$pompe_date_arret_2)


var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402)

var_traitement_val <- var_traitement_yn + 3


for (pat in data_late$X){
  ss_df <- data_late[data_late$X == pat,]
  ss_df_yn <- subset(ss_df,select=var_traitement_yn)
  ss_df_val <- subset(ss_df,select=var_traitement_val)
  colnames(ss_df_val)<-colnames(ss_df_yn)
  ss_df_verif <- rbind(ss_df_yn,ss_df_val)
  ss_df_verif <- sapply(ss_df_verif,as.numeric)
  ss_df_verif <- as.data.frame(ss_df_verif)
  ss_df_corr <- colSums(ss_df_verif,na.rm=TRUE)
  data_late[data_late$X == pat,var_traitement_yn] <-  as.data.frame(t(ifelse(ss_df_corr != 0, 1, 0)))
}



var_traitement_yn2 <- c(1,1128,177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402,406:408,413:418)
ind_traitement <- subset(data_late, select=var_traitement_yn2)
new_traitement <- ind_traitement

typeof(new_traitement)

new_traitement <- data.frame(new_traitement)

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


library(data.table)
library(tidyverse)

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)

data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>% filter(act_datedeb==min(act_datedeb)) %>% ungroup()

df_ldopa <- data_late

df_ldopa <- df_ldopa %>% select(ttt_autre_ldopa, X,pat_code_anonyme )


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

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)
df_apokinon <- df_ldopa[liste_apo,]
liste_apo_stylo <- df_apokinon[grepl("\\bSTYLO\\b", df_apokinon$ttt_autre_ldopa,ignore.case = TRUE),]$pat_code_anonyme
liste_apo_sc <- df_apokinon[!df_apokinon$pat_code_anonyme %in% liste_apo_stylo,]$pat_code_anonyme
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_stylo,]$N_park_dop_agonistes <- 1
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_sc,]$N_park_ASC <- 1

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


df_ldopa$Erreur <- rowSums(df_ldopa[,-c(1:3)],na.rm = TRUE )
print(df_ldopa[df_ldopa$Erreur == 0 & !is.na(df_ldopa$ttt_autre_ldopa),])


df_complet <- new_traitement
dim(df_complet)
names(df_complet)


df_complet_late_stage <- df_complet %>% group_by(pat_code_anonyme) %>%
  filter(X==min(X)) %>% ungroup() %>%
  select(pat_code_anonyme, ttt_modopar_62_5_gel_yn___yes:ttt_quetiapine_400_yn___yes, LGI, SCP, ASC)







data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>%
  filter(act_datedeb==min(act_datedeb)) %>% slice(1) %>% ungroup()

data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>%
  filter(act_datedeb==min(act_datedeb)) %>% slice(1) %>% ungroup()

data_late$ttt_ache <- ifelse(data_late$ttt_ache == "Non", "0", "1")

length(unique(data_late$pat_code_anonyme))

num_col <- colnames(data_late)
num_col <- as.data.frame(num_col)

num_col$num <- seq(1:1128)

data_late$pompe_date <- factor(data_late$pompe_date)
data_late$pompe_date_arret <- factor(data_late$pompe_date_arret)    
data_late$pompe_date_2 <- factor(data_late$pompe_date_2) 
data_late$pompe_date_arret_2 <- factor(data_late$pompe_date_arret_2)


var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402)

var_traitement_val <- var_traitement_yn + 3


for (pat in data_late$X){
  ss_df <- data_late[data_late$X == pat,]
  ss_df_yn <- subset(ss_df,select=var_traitement_yn)
  ss_df_val <- subset(ss_df,select=var_traitement_val)
  colnames(ss_df_val)<-colnames(ss_df_yn)
  ss_df_verif <- rbind(ss_df_yn,ss_df_val)
  ss_df_verif <- sapply(ss_df_verif,as.numeric)
  ss_df_verif <- as.data.frame(ss_df_verif)
  ss_df_corr <- colSums(ss_df_verif,na.rm=TRUE)
  data_late[data_late$X == pat,var_traitement_yn] <-  as.data.frame(t(ifelse(ss_df_corr != 0, 1, 0)))
}



var_traitement_yn2 <- c(1,1128,177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402,406:408,413:418)
ind_traitement <- subset(data_late, select=var_traitement_yn2)
new_traitement <- ind_traitement

typeof(new_traitement)

new_traitement <- data.frame(new_traitement)

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


library(data.table)
library(tidyverse)

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)

data_late <- data_late %>% group_by(pat_code_anonyme) %>% mutate(act_datedeb=as.Date(act_datedeb)) %>% filter(act_datedeb==min(act_datedeb)) %>% ungroup()

df_ldopa <- data_late

df_ldopa <- df_ldopa %>% select(ttt_autre_ldopa, X,pat_code_anonyme )


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

liste_apo <- grepl("\\bAPOKINON\\b", data_late$ttt_autre_ldopa,ignore.case = TRUE)
df_apokinon <- df_ldopa[liste_apo,]
liste_apo_stylo <- df_apokinon[grepl("\\bSTYLO\\b", df_apokinon$ttt_autre_ldopa,ignore.case = TRUE),]$pat_code_anonyme
liste_apo_sc <- df_apokinon[!df_apokinon$pat_code_anonyme %in% liste_apo_stylo,]$pat_code_anonyme
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_stylo,]$N_park_dop_agonistes <- 1
df_ldopa[df_ldopa$pat_code_anonyme %in% liste_apo_sc,]$N_park_ASC <- 1

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


df_ldopa$Erreur <- rowSums(df_ldopa[,-c(1:3)],na.rm = TRUE )
print(df_ldopa[df_ldopa$Erreur == 0 & !is.na(df_ldopa$ttt_autre_ldopa),])


df_complet <- new_traitement
dim(df_complet)
names(df_complet)


df_complet_advanced <- df_complet %>% group_by(pat_code_anonyme) %>%
  filter(X==min(X)) %>% ungroup() %>%
  select(pat_code_anonyme, ttt_modopar_62_5_gel_yn___yes:ttt_quetiapine_400_yn___yes, LGI, SCP, ASC)



to_clustr <- df_complet_late_stage %>% mutate(group=1) %>% select(-pat_code_anonyme) %>%
  bind_rows(df_complet_advanced %>% mutate(group=0) %>% select(-pat_code_anonyme))

fwrite(to_clustr, "to_clustr.csv")

to_clustr$ttt_quetiapine_300_yn___yes

# ---------