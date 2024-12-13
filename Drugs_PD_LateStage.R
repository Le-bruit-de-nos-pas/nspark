
library(tidyverse) 
library(data.table)
library(readxl)

library(corrplot)
library(FactoMineR)
library(MASS)
library(factoextra)
library(gridExtra)
library(pheatmap)
library(dplyr)
library(VIM)



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
code_inf_7 = test_date[test_date$diff < 7,]
code_inf_7$pat_code_anonyme <- factor(code_inf_7$pat_code_anonyme)

code_7 = test_date[test_date$diff >= 7,]
code_7$pat_code_anonyme <- factor(code_7$pat_code_anonyme)

pat_inf_7 <- levels(code_inf_7$pat_code_anonyme)
pat_7 <- levels(code_7$pat_code_anonyme)
pat_communs = pat_inf_7[pat_inf_7 %in% pat_7]
nombre_pat_communs =length(pat_communs)
print(nombre_pat_communs)

data_hy <- subset(df_MP,X %in% code_7$X)
length(unique(data_hy$pat_code_anonyme)) # 11666



data_hy$hoehn_yahr_on <- as.factor(data_hy$hoehn_yahr_on)
data_hy_45 <- data_hy[data_hy$hoehn_yahr_on %in% c("4","5"), ]
data_hy_45 <- data_hy_45[is.na(data_hy_45$hoehn_yahr_off) |  data_hy_45$hoehn_yahr_off %in% c("4","5","ND"),]
data_hy_45$hoehn_yahr_off <- as.factor(data_hy_45$hoehn_yahr_off)
data_hy_45$pat_code_anonyme <- factor(data_hy_45$pat_code_anonyme)
niveaux_pat_hy <- levels(data_hy_45$pat_code_anonyme)
data_hy_autre <- data_hy[!data_hy$hoehn_yahr_on %in% c("4","5"),]
length(unique(data_hy_45$pat_code_anonyme)) # 1509


df_visite_hy <- data_hy_45[,names(data_hy_45) %in% c("X","pat_code_anonyme","act_datedeb")]
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

nb_suivi <- 0 
df_ord_visite$Num_visite <- 1

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


data_late <- data_hy_45
data_late$ecart <- 0
data_late$Num_visite <- 0
for (x in df_ord_visite$X) {
  data_late[data_late$X == x,]$ecart <- df_ord_visite[df_ord_visite$X ==x,]$ecart
  data_late[data_late$X == x,]$Num_visite <- df_ord_visite[df_ord_visite$X ==x,]$Num_visite
}


data_late <- data_late[data_late$pat_ddn_a <= data_late$diag_date_a,]
data_late <- data_late[!is.na(data_late$pat_code_anonyme),]

length(unique(data_late$pat_code_anonyme)) #1504

#write.csv(data_late, file = "../Fichiers csv/data_late_baseline_f.csv", row.names = FALSE)


# ----------------

# Medications combinations ---------------
data_late <- read.table("../Fichiers csv/data_late_baseline_f.csv",sep=",",header=TRUE)

data_late$ttt_ache <- ifelse(data_late$ttt_ache == "Non", "0", "1")

num_col <- colnames(data_late)
num_col <- as.data.frame(num_col)
num_col$num <- seq(1:1130)

data_late$pompe_date <- factor(data_late$pompe_date)
data_late$pompe_date_arret <- factor(data_late$pompe_date_arret)    
data_late$pompe_date_2 <- factor(data_late$pompe_date_2) 
data_late$pompe_date_arret_2 <- factor(data_late$pompe_date_arret_2)


var_traitement_yn <- c(177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402)

var_traitement_val <- var_traitement_yn + 3

## On met oui si le patient a une valeur pour un médicament mais il a coché non 
for (pat in data_late$X){
  ss_df <- data_late[data_late$X == pat,]
  ss_df_yn <- subset(ss_df,select=var_traitement_yn)
  ss_df_val <- subset(ss_df,select=var_traitement_val)
  colnames(ss_df_val)<-colnames(ss_df_yn)
  ss_df_verif <- rbind(ss_df_yn,ss_df_val)
  ss_df_verif <- sapply(ss_df_verif,as.numeric)
  ss_df_verif <- as.data.frame(ss_df_verif)
  ss_df_corr <- colSums(ss_df_verif,na.rm=TRUE)
  data_late[data_late$X == pat,var_traitement_yn] <- ifelse(ss_df_corr != 0, 1, 0)
}

var_traitement_yn2 <- c(1,1128,177,181,185,189,193,197,201,205,209,213,217,221,225,229,233,237,250,254,258,262,266,270,274,278,282,286,290,294,298,302,306,310,314,318,322,326,330,334,338,342,346,350,354,358,362,366,370,374,378,382,386,390,394,398,402,406:408,413:418)
ind_traitement <- subset(data_late, select=var_traitement_yn2)
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
df_ldopa <- data_late
df_ldopa <- df_ldopa[,names(df_ldopa) %in% c("ttt_autre_ldopa","X","pat_code_anonyme")]


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

df_complet$Num_visite <- data_late$Num_visite


df_erreur <- df_complet
df_erreur$Somme <- rowSums(df_erreur[,c(3:11,15)])
df_erreur[df_erreur$Somme == 0,]


new_traitement <- new_traitement[ ,names(new_traitement) %in% c("X","pat_code_anonyme","A","B","C","D","E","TO","SCP","LGI","ASC","Psy","ttt_gutron_yn___yes")]

df_complet <- df_complet[ ,names(df_complet) %in% c("X","pat_code_anonyme","A","B","C","D","E","F","TO","SCP","LGI","ASC","Antipsychotique","Anticholinestherasique","Antidepresseur","Nb_Autre")]

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
med$X <- df_complet$X
med$Nb_Autres <- df_complet$Nb_Autre
med$Antipsychotique <- df_complet$Antipsychotique
med$Anticholinestherasique <- df_complet$Anticholinestherasique
med$Antidepresseur <- df_complet$Antidepresseur
head(med)




#write.csv(med,"../Fichiers csv/Obj_Principal/merge_tab_croise_ind.csv",row.names=FALSE)

df_complet$Num_visite <- data_late$Num_visite
head(df_complet)
# on regarde les combinaisons à la baseline
med <- df_complet[df_complet$Num_visite == 1,-c(1,2,12:14,16,17)]



dataframe <- med %>% select(A, B, C, D, E, F )

names(dataframe) <- c("MAO Inhibitor","Levodopa","Dopamine Agonist",
                      "Amantadine", "COMT Inhibitor", "Anticholinergics")

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
med$pat_code_anonyme <- df_complet[df_complet$Num_visite == 1,]$pat_code_anonyme
med$Nb_Autres <- df_complet[df_complet$Num_visite == 1,]$Nb_Autre
med$Antipsychotique <- df_complet[df_complet$Num_visite == 1,]$Antipsychotique
med$Anticholinestherasique <- df_complet[df_complet$Num_visite == 1,]$Anticholinestherasique
med$Antidepresseur <- df_complet[df_complet$Num_visite == 1,]$Antidepresseur
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


#write.csv(df_ord,"../Fichiers csv/Obj_Principal/merge_tableau_croise_baseline.csv",row.names=FALSE)

med <- read.table("../Fichiers csv/Obj_Principal/merge_tab_croise_ind.csv",sep=",",header=TRUE)

dim(med)


# ------------
# Clean up data - IGNORE FOR NOW -----------------

data_late <- read.csv("../Fichiers csv/data_late_baseline_f.csv",header=TRUE)
taux_na <- sapply(data_late, function(x) mean(is.na(x)))
na_val_all <- as.data.frame(taux_na)
na_val_all$Var <- seq(1,1130)
var_a_supp <- na_val_all[na_val_all$taux_na == 1, , drop = FALSE]
col_a_supp <- rownames(var_a_supp)
var_a_1 <- na_val_all[na_val_all$taux_na == 0, , drop = FALSE]
Var_a_1 <- data_late[, var_a_1$Var]
Var_a_1 <- as.data.frame(lapply(Var_a_1,as.factor))
single_level_vars <- names(Var_a_1)[sapply(Var_a_1, function(x) length(levels(x)) == 1)]
data_corr <- data_late
data_corr <- data_corr[,!names(data_corr) %in% col_a_supp]
redcap <- grep("redcap",colnames(data_corr),value=TRUE)
redcap_data <- data_corr[,names(data_corr) %in% redcap]
redcap_data <- lapply(redcap_data, as.factor)
redcap_data <- as.data.frame(redcap_data)
data_corr <- data_corr[,!names(data_corr) %in% c("redcap_repeat_instance.y","redcap_data_access_group.y","redcap_event_name.x","redcap_event_name.y")]
data_corr <- rename(data_corr, redcap_data_access_group = redcap_data_access_group.x)
data_corr$pat_code_anonyme <- as.factor(data_corr$pat_code_anonyme)
data_corr$redcap_data_access_group <- as.factor(data_corr$redcap_data_access_group)
data_corr$pat_sexe <- as.factor(data_corr$pat_sexe)
data_corr$pat_ethnie <- as.factor(data_corr$pat_ethnie)
data_corr$park_sd_nivetudes <- as.factor(data_corr$park_sd_nivetudes)
data_corr$identification_complete <- as.factor(data_corr$identification_complete)
data_id <- data_corr[,names(data_corr) %in% c("pat_code_anonyme","redcap_data_access_group","pat_ddn_m","pat_ddn_a","pat_sexe","pat_ethnie","park_sd_nivetudes","dece_a","dece_cause","identification_complete")]
data_id$dece_cause <- as.factor(data_id$dece_cause)
data_corr$dece_cause <- ifelse(data_corr$dece_cause == "non connue" | data_corr$dece_cause == "Non connu","Non connue",data_corr$dece_cause)
data_corr$dece_cause <- ifelse(data_corr$dece_cause == "COVID" ,"Covid19",data_corr$dece_cause)
data_corr$dece_cause <- as.factor(data_corr$dece_cause)
data_consent <- data_corr[,names(data_corr) %in% c("nspark_cohort","nspark_consentexp___1",
                                                   "nspark_consentexp_d","nspark_consentexp_date",
                                                   "nspark_consentexp_sign","nspark_consentecr___1",
                                                   "nspark_consentecr___na","nspark_consentecr___dm",
                                                   "nspark_consentecr_date","nspark_consentecr_sign",
                                                   "nspark_consent_maj","consent_ancill_autre___1",
                                                   "consent_snds","consent_util_ult","consent_util_ult_bio",
                                                   "consent_util_gen","consent_util_bio_gen","consentement_complete")]
data_consent <- lapply(data_consent,as.factor)
data_consent <- as.data.frame(data_consent)
data_diag <- data_corr[,names(data_corr) %in% c("diag_date_a","diag_autre","diag_lateralite",
                                                "diag_symptc1","diag_sympt_pred___1","diag_sympt_pred___2",
                                                "diag_sympt_pred___9","diag_introlevo_a","atcd_fam_pt_mut",
                                                "atcd_fam_pt_mut_nom","atcd_fam_yn","atcd_fam_mbre___1",
                                                "atcd_fam_mbre___2","atcd_fam_mbre___3","atcd_fam_mbre___4",
                                                "atcd_fam_mbre___8","atcd_fam_autre","fam_prel","fam_prel_code",
                                                "arbre_gene","diagnostic_complete")]

data_corr[,names(data_corr) %in% c("diag_autre","diag_lateralite","diag_symptc1","diag_sympt_pred___1",
                                   "diag_sympt_pred___2","diag_sympt_pred___9","diag_introlevo_a","atcd_fam_pt_mut",
                                   "atcd_fam_pt_mut_nom","atcd_fam_yn","atcd_fam_mbre___1","atcd_fam_mbre___2",
                                   "atcd_fam_mbre___3","atcd_fam_mbre___4","atcd_fam_mbre___8","fam_prel",
                                   "fam_prel_code","arbre_gene","diagnostic_complete")] <- lapply(
                                     data_corr[,names(data_corr) %in% c("diag_autre","diag_lateralite","diag_symptc1",
                                                                        "diag_sympt_pred___1","diag_sympt_pred___2",
                                                                        "diag_sympt_pred___9","diag_introlevo_a",
                                                                        "atcd_fam_pt_mut","atcd_fam_pt_mut_nom","atcd_fam_yn",
                                                                        "atcd_fam_mbre___1","atcd_fam_mbre___2","atcd_fam_mbre___3",
                                                                        "atcd_fam_mbre___4","atcd_fam_mbre___8","fam_prel",
                                                                        "fam_prel_code","arbre_gene","diagnostic_complete")],as.factor)

data_diag[,-c(1)] <- lapply(data_diag[,-c(1)],as.factor)
data_diag <- as.data.frame(data_diag)
data_corr <- data_corr[,!names(data_corr) %in% c("arbre_gene")]
data_corr$poids <- ifelse(data_corr$poids =="DM",NA,data_corr$poids)
data_corr$poids <- as.numeric(data_corr$poids)
data_corr <- as.data.frame(data_corr)

data_corr[,names(data_corr) %in% c("visite_suivi_yn","visite_suivi_cumul","visite_suivi_affich",
                                   "visite_suivi_affich_2","act_datedeb","fluct_motrice","dyskinesie",
                                   "douleur","nociceptive","neuropathique","dysarthrie","freezing",
                                   "chute_instab","deform_post","tr_degl","chute","somnolence",
                                   "insomnie","fatigue","rbd","sas","sjsr","hypotension","digestif",
                                   "urine","apathie","depression","anxiete","halluc_psy","tci",
                                   "add_ldopa","punding","tr_cognitif","hoehn_yahr_on","hoehn_yahr_off",
                                   "prel_sang","prel_sang_date","prel_yn","prel_cumul","prel_cumul_yn",
                                   "prel_cumul_date","prel_sang_date_envoi","prel_salive","prel_urine",
                                   "prel_peau","prel_lcr","prel_gs")] <- lapply(
                                     data_corr[,names(data_corr) %in% c("visite_suivi_yn","visite_suivi_cumul",
                                                                        "visite_suivi_affich","visite_suivi_affich_2",
                                                                        "act_datedeb","fluct_motrice","dyskinesie",
                                                                        "douleur","nociceptive","neuropathique","dysarthrie",
                                                                        "freezing","chute_instab","deform_post","tr_degl","chute",
                                                                        "somnolence","insomnie","fatigue","rbd","sas","sjsr","hypotension",
                                                                        "digestif","urine","apathie","depression","anxiete","halluc_psy","tci",
                                                                        "add_ldopa","punding","tr_cognitif","hoehn_yahr_on","hoehn_yahr_off",
                                                                        "prel_sang","prel_sang_date","prel_yn","prel_cumul","prel_cumul_yn",
                                                                        "prel_cumul_date","prel_sang_date_envoi","prel_salive","prel_urine",
                                                                        "prel_peau","prel_lcr","prel_gs")],as.factor)


data_sym <- data_corr[,names(data_corr) %in% c("visite_suivi_yn","visite_suivi_cumul",
                                               "visite_suivi_affich","visite_suivi_affich_2","act_datedeb",
                                               "fluct_motrice","dyskinesie","douleur","nociceptive","neuropathique",
                                               "dysarthrie","freezing","chute_instab","deform_post","tr_degl","chute",
                                               "somnolence","insomnie","fatigue","rbd","sas","sjsr","hypotension",
                                               "digestif","urine","poids","apathie","depression","anxiete","halluc_psy",
                                               "tci","add_ldopa","punding","tr_cognitif","hoehn_yahr_on","hoehn_yahr_off",
                                               "prel_sang","prel_sang_date","prel_yn","prel_cumul","prel_cumul_yn",
                                               "prel_cumul_date","prel_sang_date_envoi","prel_salive","prel_urine",
                                               "prel_peau","prel_lcr","prel_gs")]

data_corr <- data_corr[,!names(data_corr) %in% c("prel_salive","prel_urine","prel_peau","prel_lcr",
                                                 "prel_gs","visite_suivi_affich","visite_suivi_affich_2")]


mds_total <- data_corr[,c(350:466)]
mds_na <- is.na(mds_total)
mds_na_melted <- melt(mds_na)
colnames(mds_na_melted) <- c("Observation", "Variable", "Missing")
taux_na_mds <- sapply(mds_total, function(x) mean(is.na(x)))
na_mds_all <- as.data.frame(taux_na_mds)
data_mds <- data_corr[,names(data_corr) %in% c("mds3_trait_mp_st___1","mds3_trait_mp_st___2",
                                               "mds3_trait_mp_st___dm","echelles_mds_updrs_complete")]
data_mds <- as.data.frame(lapply(data_mds,as.factor))

moca_total <- data_corr[,c(467:496)]
moca_na <- is.na(moca_total)
moca_na_melted <- melt(moca_na)
colnames(moca_na_melted) <- c("Observation", "Variable", "Missing")
moca_total$total_moca <- as.factor(moca_total$total_moca)

comp_total <- data_corr[,c(497:572)]
comp_na <- is.na(comp_total)
comp_na_melted <- melt(comp_na)
colnames(comp_na_melted) <- c("Observation", "Variable", "Missing")
taux_na_comport <- sapply(comp_total, function(x) mean(is.na(x)))
na_comport_all <- as.data.frame(taux_na_comport)
comp_total$comport_troub_source___1 <- as.factor(comp_total$comport_troub_source___1)
comp_total$comport_troub_source___2 <- as.factor(comp_total$comport_troub_source___2)

print(colnames(data_corr[,c(573,581)]))
qv_total <- data_corr[,c(573:581)]
qv_na <- is.na(qv_total)
qv_na_melted <- melt(qv_na)
colnames(qv_na_melted) <- c("Observation", "Variable", "Missing")
qv_total$qualit_de_vie_pdq8_complete <- as.factor(qv_total$qualit_de_vie_pdq8_complete)
liste1 <- colnames(data_corr)




data_corr$age_diag <- data_corr$diag_date_a - data_corr$pat_ddn_a
data_corr$visite_date_a <- substr(data_corr$act_datedeb,7,10)
data_corr$visite_date_a <- as.numeric(data_corr$visite_date_a)
data_corr$age_baseline <- data_corr$visite_date_a - data_corr$pat_ddn_a
data_corr$age_deces <- 0
df_deces <- data_corr[!is.na(data_corr$dece_a),]
df_deces$age_deces <- df_deces$dece_a - df_deces$pat_ddn_a
data_corr[!is.na(data_corr$dece_a),] <- df_deces
data_corr$age_deces <- ifelse(data_corr$age_deces == 0,NA,data_corr$age_deces)
data_corr$diag_introlevo_a <- as.numeric(data_corr$diag_introlevo_a)
data_corr$age_introlevo_a <- data_corr$age_diag + data_corr$diag_introlevo_a
data_corr$date_impl_a <- substr(data_corr$date_impl,7,10)
data_corr$date_impl_a <- as.numeric(data_corr$date_impl_a)
data_corr$age_impl_scp <- data_corr$date_impl_a - data_corr$pat_ddn_a
data_corr$date_arret_a <- substr(data_corr$date_arret,7,10)
data_corr$date_arret_a <- as.numeric(data_corr$date_arret_a)
data_corr$age_arret_scp <- data_corr$date_arret_a - data_corr$pat_ddn_a
data_corr$date_deb_pompe <- substr(data_corr$pompe_date,7,10)
data_corr$date_deb_pompe <- as.numeric(data_corr$date_deb_pompe)
data_corr$age_deb_pompe <- data_corr$date_deb_pompe - data_corr$pat_ddn_a
data_corr$date_arr_pompe <- substr(data_corr$pompe_date_arret,7,10)
data_corr$date_arr_pompe <- as.numeric(data_corr$date_arr_pompe)
data_corr$age_arr_pompe <- data_corr$date_arr_pompe - data_corr$pat_ddn_a
data_corr$date_deb_pompe2 <- substr(data_corr$pompe_date_2,7,10)
data_corr$date_deb_pompe2 <- as.numeric(data_corr$date_deb_pompe2)
data_corr$age_deb_pompe2 <- data_corr$date_deb_pompe2 - data_corr$pat_ddn_a
data_corr$date_arr_pompe2 <- substr(data_corr$pompe_date_arret_2,7,10)
data_corr$date_arr_pompe2 <- as.numeric(data_corr$date_arr_pompe2)
data_corr$age_arr_pompe2 <- data_corr$date_arr_pompe2 - data_corr$pat_ddn_a
data_corr$maladie_tps_baseline <- data_corr$visite_date_a - data_corr$diag_date_a
data_corr$maladie_tps_mort <- data_corr$age_deces - data_corr$age_diag



## Variables de médicament (liés à ttt_autre ldopa)

### A
data_corr$ttt_otrasel <- 0
data_corr[grepl("OTRASEL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_otrasel <- 1

### LGI
data_corr$ttt_duodopa_gel_intestinal <- 0
data_corr[grepl("DUODOPA",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_duodopa_gel_intestinal <- 1
data_corr$ttt_duodopa_pompe <- ifelse(is.na(data_corr$pompe_dose_2), 0,  ifelse((data_corr$pompe_dose_2 == "DM" & is.na(data_corr$pompe_date_2)), 0,ifelse(!is.na(data_corr$pompe_dose_2) & data_corr$pompe_dose_2 != 0, 1, 0)))

### C
liste_N_park_dop_agonistes <- c("APOMORPHINE CHABRE","PARLODEL")
data_corr$ttt_apomorphine <- 0
data_corr[grepl("APOMORPHINE CHABRE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_apomorphine <- 1
data_corr$ttt_parlodel <- 0
data_corr[grepl("PARLODEL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_parlodel <- 1
liste_apo <- grepl("\\bAPOKINON\\b", data_corr$ttt_autre_ldopa,ignore.case = TRUE)
df_apokinon <- data_corr[liste_apo,]
liste_apo_stylo <- df_apokinon[grepl("\\bSTYLO\\b", df_apokinon$ttt_autre_ldopa,ignore.case = TRUE),]$pat_code_anonyme
liste_apo_sc <- df_apokinon[!df_apokinon$pat_code_anonyme %in% liste_apo_stylo,]$pat_code_anonyme
data_corr$ttt_apokinon <- 0
data_corr[data_corr$pat_code_anonyme %in% liste_apo_stylo,]$ttt_apokinon <- 1
data_corr$ttt_apokinon_asc <- 0
data_corr[data_corr$pat_code_anonyme %in% liste_apo_sc,]$ttt_apokinon_asc <- 1

### Anticholinesthérasique/Démence
liste_demence <- c("ARICEPT","DONEPEZIL","EBIXA","EXELON","RIVASTIGMINE")
data_corr$ttt_aricept <- 0
data_corr[grepl("ARICEPT",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_aricept <- 1
data_corr$ttt_donepezil <- 0
data_corr[grepl("DONEPEZIL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_donepezil <- 1
data_corr$ttt_ebixa <- 0
data_corr[grepl("EBIXA",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_ebixa <- 1
data_corr$ttt_exelon <- 0
data_corr[grepl("EXELON",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_exelon <- 1
data_corr$ttt_rivastigmine <- 0
data_corr[grepl("RIVASTIGMINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_rivastigmine <- 1

### Antipsychotiques
liste_psychotique <- c("ABILIFY","CLOZAPINE","RISPERDAL","SERESTA","TIAPRIDAL","XEROQUEL","LOXAPRAC")
data_corr$ttt_abilify <- 0
data_corr[grepl("ABILIFY",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_abilify <- 1
data_corr$ttt_clozapine <- 0
data_corr[grepl("CLOZAPINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_clozapine <- 1
data_corr$ttt_risperdal <- 0
data_corr[grepl("RISPERDAL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_risperdal <- 1
data_corr$ttt_seresta <- 0
data_corr[grepl("SERESTA",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_seresta <- 1
data_corr$ttt_tiapridal <- 0
data_corr[grepl("TIAPRIDAL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_tiapridal <- 1
data_corr$ttt_xeroquel <- 0
data_corr[grepl("XEROQUEL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_xeroquel <- 1
data_corr$ttt_loxaprac <- 0
data_corr[grepl("LOXAPRAC",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_loxaprac <- 1

### Antidépresseur
liste_antidep <- c("ANAFRANIL","BRINTELLIX","CITALOPRAM","CLOMIPRAMINE","CYMBALTA","EFFEXOR","ESCITALOPRAM","FLUOXETINE","LAROXYL","MIANSERINE","MILNACIPRAN","MIRTAZAPINE","MOCLAMINE","NORSET","PAROXETINE","PROZAC","QUITAXON","SEROPLEX","SEROPRAM","SERTRALINE","SURMONTIL","VENLAFAXINE","ZOLOFT")
data_corr$ttt_anafranil <- 0
data_corr[grepl("ANAFRANIL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_anafranil <- 1
data_corr$ttt_brintellix <- 0
data_corr[grepl("BRINTELLIX",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_brintellix <- 1
data_corr$ttt_citalopram <- 0
data_corr[grepl("CITALOPRAM",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_citalopram <- 1
data_corr$ttt_clomipramine <- 0
data_corr[grepl("CLOMIPRAMINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_clomipramine <- 1
data_corr$ttt_cymbalta <- 0
data_corr[grepl("CYMBALTA",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_cymbalta <- 1
data_corr$ttt_effexor <- 0
data_corr[grepl("EFFEXOR",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_effexor <- 1
data_corr$ttt_fluoxetine <- 0
data_corr[grepl("FLUOXETINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_fluoxetine <- 1
data_corr$ttt_laroxyl <- 0
data_corr[grepl("LAROXYL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_laroxyl <- 1
data_corr$ttt_mianserine <- 0
data_corr[grepl("MIANSERINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_mianserine <- 1
data_corr$ttt_milnacipran <- 0
data_corr[grepl("MILNACIPRAN",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_milnacipran <- 1
data_corr$ttt_mirtazapine <- 0
data_corr[grepl("MIRTAZAPINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_mirtazapine <- 1
data_corr$ttt_moclamine <- 0
data_corr[grepl("MOCLAMINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_moclamine <- 1
data_corr$ttt_norset <- 0
data_corr[grepl("NORSET",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_norset <- 1
data_corr$ttt_parox <- 0
data_corr[grepl("PAROXETINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_parox <- 1
data_corr$ttt_prozac <- 0
data_corr[grepl("PROZAC",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_prozac <- 1
data_corr$ttt_quitaxon <- 0
data_corr[grepl("QUITAXON",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_quitaxon <- 1
data_corr$ttt_seroplex <- 0
data_corr[grepl("SEROPLEX",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_seroplex <- 1
data_corr$ttt_seropram <- 0
data_corr[grepl("SEROPRAM",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_seropram <- 1
data_corr$ttt_sertraline <- 0
data_corr[grepl("SERTRALINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_sertraline <- 1
data_corr$ttt_surmontil <- 0
data_corr[grepl("SURMONTIL",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_surmontil <- 1
data_corr$ttt_venlafaxine <- 0
data_corr[grepl("VENLAFAXINE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_venlafaxine <- 1
data_corr$ttt_zoloft <- 0
data_corr[grepl("ZOLOFT",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_zoloft <- 1

### ASC
liste_N_park_ASC <- c("Apomorphine MRM","Pompe à Apomorphine")
data_corr$ttt_apomorphine_sc <- 0
data_corr[grepl("APOMORPHINE MRM",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_apomorphine_sc <- 1
data_corr$ttt_apomorphine_pompe <- 0
data_corr[grepl("Pompe à Apomorphine",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_apomorphine_pompe <- 1
data_corr$ttt_apomorphine_pompe <- data_corr$ttt_apomorphine_pompe + ifelse(is.na(data_corr$pompe_dose), 0,  ifelse((data_corr$pompe_dose == "DM" & is.na(data_corr$pompe_date)), 0,ifelse(!is.na(data_corr$pompe_dose) & data_corr$pompe_dose != 0, 1, 0)))


### B
data_corr[grepl("Sinemet 250",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_sinemet_250_cpr_yn___yes <- 1

### F (Anticholinergiques)
data_corr$ttt_artane <- 0
data_corr[grepl("ARTANE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_artane <- 1
data_corr$ttt_parkinane <- 0
data_corr[grepl("PARKINANE",data_corr$ttt_autre_ldopa,ignore.case = TRUE),]$ttt_parkinane <- 1


data_med <- read.csv("../Fichiers csv/Obj_Principal/merge_tab_croise_ind.csv",sep=",",header=TRUE)
data_combi <- data_med[,-c(11,13:16)]
data_autre <- data_med[,c(11,12)]
data_corr <- cbind(data_corr,data_med[,names(data_med) %in% c("A","B","C","D","E","F","TO","SCP","LGI","ASC")])

df <- data_combi[,-c(11)]

for (col in names(data_combi[,-c(11)])) {
  df[[col]] <- ifelse(data_combi[[col]] == 1, col, NA)
}

df$combinaison <- apply(df, 1, function(row) {
  paste(na.omit(row), collapse = "+")
})

data_combi$combinaison <- df$combinaison
data_corr$Traitement <- "Autre"
data_corr[data_corr$X %in% data_combi[data_combi$A == 0 & data_combi$B == 1 & data_combi$C == 0 & data_combi$D == 0 & data_combi$E == 0 & data_combi$F == 0 & data_combi$TO == 1 & data_combi$SCP == 0 & data_combi$LGI == 0 & data_combi$ASC == 0,]$X,]$Traitement <- "Lévodopa"
data_corr[data_corr$X %in% data_combi[data_combi$B == 1 & data_combi$D == 1 & data_combi$SCP == 0 & data_combi$LGI == 0 & data_combi$ASC == 0,]$X,]$Traitement <- "Lévodopa-Amantadine-Autre"
data_corr[data_corr$X %in% data_combi[data_combi$B == 1 & data_combi$D == 0 & data_combi$SCP == 0 & data_combi$LGI == 0 & data_combi$ASC == 0 & (data_combi$A == 1 |  data_combi$C == 1 | data_combi$E == 1 | data_combi$F == 1),]$X,]$Traitement <- "Lévodopa-Autre"
data_corr[data_corr$X %in% data_combi[data_combi$TO==1 & data_combi$SCP == 1 & data_combi$LGI == 0 & data_combi$ASC == 0,]$X,]$Traitement <- "TO-Stimulation"
data_corr[data_corr$X %in% data_combi[data_combi$TO==1 & data_combi$SCP == 0 & (data_combi$LGI == 1 | data_combi$ASC == 1),]$X,]$Traitement <- "TO-Pompe"
data_corr$Nb_autres_ttt <- data_med$Nb_Autres
data_corr$Antipsychotique <- data_med$Antipsychotique
data_corr$Demence <- data_med$Anticholinestherasique
data_corr$Antidepresseur <- data_med$Antidepresseur

data_corr <- data_corr[,!names(data_corr) %in% c("identification_complete","consentement_complete","diagnostic_complete","consultation_complete","echelles_mds_updrs_complete","tests_neuropsy_complete","qualit_de_vie_pdq8_complete")]

#write.csv(df_baseline, file = "../Fichiers csv/merge_late_baseline_corr.csv", row.names = FALSE)

df_suivi_6 <- data_corr[data_corr$ecart >= 6 & data_corr$ecart <= 24 & data_corr$Num_visite==2,]
df_suivi_6$pat_code_anonyme <- factor(df_suivi_6$pat_code_anonyme)
df_obj_f <- data_corr[data_corr$pat_code_anonyme %in% df_suivi_6$pat_code_anonyme,]
# write.csv(df_obj_f, file = "../Fichiers csv/merge_late_obj_f.csv", row.names = FALSE)

# ---------