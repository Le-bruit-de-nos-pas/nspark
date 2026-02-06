
library(tidyverse) 
library(data.table)
library(readxl)
library(lubridate)

# PÄIN Before vs After Using Paired Samples --------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342






names(Consultation_20241028)
data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, douleur, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(act_datedeb==pompe_date) # 19 with visit same date as pump introduced / 18 with douleur 5.5%

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(!is.na(pompe_date)) %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>% #617 visits
  # select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<1) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<2) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<3) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<4) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<5) %>% select(anonyme_id) %>% distinct() # 
  filter(abs(elapsed)<6) %>% select(anonyme_id) %>% distinct() # 


data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, douleur))

unique(first_apo$douleur)

first_apo %>% ungroup() %>% filter(douleur %in% c("1","0",">=2", "2", "3", "4")) %>%
  select(anonyme_id) %>% distinct() # 250 known pain status at some point

first_apo %>% group_by(douleur) %>% count()

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(douleur %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(douleur=parse_number(douleur))  



# All 129 available

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, douleur, elapsed) %>% rename("douleur_before"="douleur") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, douleur, elapsed) %>% rename("douleur_after"="douleur") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after %>% group_by(douleur_before) %>% count() %>% mutate(n=n/129)
Before_vs_after %>% group_by(douleur_after) %>% count()  %>% mutate(n=n/129)

mean(Before_vs_after$douleur_before) 
sd(Before_vs_after$douleur_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$douleur_after) 
sd(Before_vs_after$douleur_before) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$douleur_before, Before_vs_after$douleur_after, paired = TRUE, alternative = "two.sided")



Before_vs_after %>% 
  mutate(douleur_before=ifelse(douleur_before>=1,1,0)) %>%
  mutate(douleur_after=ifelse(douleur_after>=1,1,0)) %>%
  group_by(douleur_before, douleur_after) %>% count()

data <- matrix(c(41, 22, 20, 46), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(douleur_before, douleur_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "douleur"
  ) %>%
  mutate(
    time = ifelse(time == "douleur_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$douleur)

data_long$douleur <- factor(data_long$douleur, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(douleur ~ time , data = data_long)

summary(model)


# All 124 within 60 months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, douleur, elapsed) %>% rename("douleur_before"="douleur") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, douleur, elapsed) %>% rename("douleur_after"="douleur") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-60))
  )

Before_vs_after %>% group_by(douleur_before) %>% count()  %>% mutate(n=n/124)
Before_vs_after %>% group_by(douleur_after) %>% count()  %>% mutate(n=n/124)

mean(Before_vs_after$douleur_before) 
sd(Before_vs_after$douleur_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$douleur_after) 
sd(Before_vs_after$douleur_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$douleur_before, Before_vs_after$douleur_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(douleur_before=ifelse(douleur_before>=1,1,0)) %>%
  mutate(douleur_after=ifelse(douleur_after>=1,1,0)) %>%
  group_by(douleur_before, douleur_after) %>% count()

data <- matrix(c(39, 22, 19, 44), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(douleur_before, douleur_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "douleur"
  ) %>%
  mutate(
    time = ifelse(time == "douleur_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$douleur)

data_long$douleur <- factor(data_long$douleur, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(douleur ~ time , data = data_long)

summary(model)



# All 54 within 24months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, douleur, elapsed) %>% rename("douleur_before"="douleur") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, douleur, elapsed) %>% rename("douleur_after"="douleur") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-24))
  )

Before_vs_after %>% group_by(douleur_before) %>% count()  %>% mutate(n=n/54)
Before_vs_after %>% group_by(douleur_after) %>% count()  %>% mutate(n=n/54)

mean(Before_vs_after$douleur_before) 
sd(Before_vs_after$douleur_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$douleur_after) 
sd(Before_vs_after$douleur_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$douleur_before, Before_vs_after$douleur_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(douleur_before=ifelse(douleur_before>=1,1,0)) %>%
  mutate(douleur_after=ifelse(douleur_after>=1,1,0)) %>%
  group_by(douleur_before, douleur_after) %>% count()


data <- matrix(c(25, 7, 3, 19), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(douleur_before, douleur_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "douleur"
  ) %>%
  mutate(
    time = ifelse(time == "douleur_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$douleur)

data_long$douleur <- factor(data_long$douleur, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(douleur ~ time , data = data_long)

summary(model)



# All 30 within 12months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, douleur, elapsed) %>% rename("douleur_before"="douleur") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, douleur, elapsed) %>% rename("douleur_after"="douleur") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-12))
  )

Before_vs_after <- Before_vs_after %>% distinct()

Before_vs_after %>% group_by(douleur_before) %>% count()  %>% mutate(n=n/30)
Before_vs_after %>% group_by(douleur_after) %>% count()  %>% mutate(n=n/30)

mean(Before_vs_after$douleur_before) 
sd(Before_vs_after$douleur_before) #
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$douleur_after) 
sd(Before_vs_after$douleur_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$douleur_before, Before_vs_after$douleur_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(douleur_before=ifelse(douleur_before>=1,1,0)) %>%
  mutate(douleur_after=ifelse(douleur_after>=1,1,0)) %>%
  group_by(douleur_before, douleur_after) %>% count()


data <- matrix(c(14, 3, 1, 12), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(douleur_before, douleur_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "douleur"
  ) %>%
  mutate(
    time = ifelse(time == "douleur_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$douleur)

data_long$douleur <- factor(data_long$douleur, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(douleur ~ time , data = data_long)

summary(model)




# All 13 within 6months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=6) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-6))
  )

Before_vs_after %>% group_by(tci_before) %>% count()
Before_vs_after %>% group_by(tci_after) %>% count()

mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) 
median(Before_vs_after$elapsed_before) 

mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 

wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()

data <- matrix(c(1, 2, 0, 10), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)



# -----------------
# NEUROPATHIQUE VS NOCICEPTIVE Before vs After Using Paired Samples --------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342




# NEUROPHATIQUE PAIN

names(Consultation_20241028)
data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, neuropathique, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(act_datedeb==pompe_date) # 19 with visit same date as pump introduced / 18 with douleur 5.5%

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(!is.na(pompe_date)) %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>% #617 visits
  # select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<1) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<2) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<3) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<4) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<5) %>% select(anonyme_id) %>% distinct() # 
  filter(abs(elapsed)<6) %>% select(anonyme_id) %>% distinct() # 


data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, neuropathique))

unique(first_apo$neuropathique)


first_apo %>% ungroup() %>% filter(neuropathique %in% c("OUI","NON")) %>%
  select(anonyme_id) %>% distinct() # 132 known pain status at some point

first_apo %>% group_by(neuropathique) %>% count()

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(neuropathique %in% c("OUI","NON")) %>%
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) 



# All 40 available

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, neuropathique, elapsed) %>% rename("neuropathique_before"="neuropathique") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, neuropathique, elapsed) %>% rename("neuropathique_after"="neuropathique") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after %>% group_by(neuropathique_before) %>% count() %>% mutate(n=n/40) # 0.3 0.7
Before_vs_after %>% group_by(neuropathique_after) %>% count()  %>% mutate(n=n/40) # 0.475 0.525

median(Before_vs_after$elapsed_before)  #15
quantile(Before_vs_after$elapsed_before, 0.25) #8
quantile(Before_vs_after$elapsed_before, 0.75) #43

median(Before_vs_after$elapsed_after) #7
quantile(Before_vs_after$elapsed_after, 0.25) #12
quantile(Before_vs_after$elapsed_after, 0.75) #4

Before_vs_after %>% 
  group_by(neuropathique_before, neuropathique_after) %>% count()


data <- matrix(c(18, 10, 3, 9), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data) # 0.09609



# NOCICEPTIVE PAIN

names(Consultation_20241028)
data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, nociceptive, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(act_datedeb==pompe_date) # 19 with visit same date as pump introduced / 18 with douleur 5.5%

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(!is.na(pompe_date)) %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>% #617 visits
  # select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<1) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<2) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<3) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<4) %>% select(anonyme_id) %>% distinct() # 
  #filter(abs(elapsed)<5) %>% select(anonyme_id) %>% distinct() # 
  filter(abs(elapsed)<6) %>% select(anonyme_id) %>% distinct() # 


data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, nociceptive))

unique(first_apo$nociceptive)


first_apo %>% ungroup() %>% filter(nociceptive %in% c("OUI","NON")) %>%
  select(anonyme_id) %>% distinct() # 156 known pain status at some point

first_apo %>% group_by(nociceptive) %>% count()

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(nociceptive %in% c("OUI","NON")) %>%
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) 



# All 52 available

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, nociceptive, elapsed) %>% rename("nociceptive_before"="nociceptive") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, nociceptive, elapsed) %>% rename("nociceptive_after"="nociceptive") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after %>% group_by(nociceptive_before) %>% count() %>% mutate(n=n/52) # 0.173 0.827
Before_vs_after %>% group_by(nociceptive_after) %>% count()  %>% mutate(n=n/52) # 0.0577 0.942

median(Before_vs_after$elapsed_before)  #18
quantile(Before_vs_after$elapsed_before, 0.25) #8
quantile(Before_vs_after$elapsed_before, 0.75) #45

median(Before_vs_after$elapsed_after) #9
quantile(Before_vs_after$elapsed_after, 0.25) #14
quantile(Before_vs_after$elapsed_after, 0.75) #4

Before_vs_after %>% 
  group_by(nociceptive_before, nociceptive_after) %>% count()


data <- matrix(c(42, 1, 7, 2), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data) # 0.0771



z_alpha <- qnorm(1 - alpha / 2) 
z_beta <- qnorm(power)

# Calculate minimum sample size for McNemar's test
n <- ((z_alpha + z_beta)^2) / (2 * (0.16 - 0.0)^2)
print(n)

# -----------------
# TCI  Before vs After Using Paired Samples -------------------

Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342







data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(act_datedeb==pompe_date) # 19 with visit same date as pump introduced / 18 with tci 5.5%

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(!is.na(pompe_date)) %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>% #617 visits
  # select(anonyme_id) %>% distinct() # 330
  #filter(abs(elapsed)<1) %>% select(anonyme_id) %>% distinct() # 35
  #filter(abs(elapsed)<2) %>% select(anonyme_id) %>% distinct() # 58
  #filter(abs(elapsed)<3) %>% select(anonyme_id) %>% distinct() # 71
  #filter(abs(elapsed)<4) %>% select(anonyme_id) %>% distinct() # 82
  #filter(abs(elapsed)<5) %>% select(anonyme_id) %>% distinct() # 86
  filter(abs(elapsed)<6) %>% select(anonyme_id) %>% distinct() # 93



data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

# data <- data %>% arrange(anonyme_id, redcap_repeat_instance) %>% group_by(anonyme_id) %>%
#   fill(pompe_date, .direction = "down") %>% fill(pompe_date, .direction = "up") %>%
#   fill(tci, .direction = "down") %>% fill(tci, .direction = "up") 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo %>% ungroup() %>% filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>%
  select(anonyme_id) %>% distinct() # 253 known tci status at some point

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  



# All 149 available

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after %>% group_by(tci_before) %>% count() %>% mutate(n=n/149)
Before_vs_after %>% group_by(tci_after) %>% count()  %>% mutate(n=n/149)

Before_vs_after %>% mutate(tci_before=ifelse(tci_before==0,0,1)) %>%
  mutate(tci_after=ifelse(tci_after==0,0,1)) %>%
  group_by(tci_before, tci_after) %>% count()


Before_vs_after %>%  group_by(tci_before, tci_after) %>% count()


mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)




mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)




wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()

data <- matrix(c(10, 23, 10, 106), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tci_before, tci_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tci"
  ) %>%
  mutate(
    time = ifelse(time == "tci_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tci)

data_long$tci <- factor(data_long$tci, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tci ~ time , data = data_long)

summary(model)


# All 124 within 60 months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-60))
  )


Before_vs_after <- Before_vs_after %>% distinct()

Before_vs_after %>% group_by(tci_before) %>% count()  %>% mutate(n=n/124)
Before_vs_after %>% group_by(tci_after) %>% count()  %>% mutate(n=n/124)

mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()

Before_vs_after %>% 
  group_by(tci_before, tci_after) %>% count()

data <- matrix(c(10, 18, 8, 88), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tci_before, tci_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tci"
  ) %>%
  mutate(
    time = ifelse(time == "tci_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tci)

data_long$tci <- factor(data_long$tci, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tci ~ time , data = data_long)

summary(model)

# All 59 within 24months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-24))
  )
 
Before_vs_after %>% group_by(tci_before) %>% count()  %>% mutate(n=n/59)
Before_vs_after %>% group_by(tci_after) %>% count()  %>% mutate(n=n/59)

mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) 
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()


Before_vs_after %>% 
  group_by(tci_before, tci_after) %>% count()

data <- matrix(c(7, 8, 1, 43), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tci_before, tci_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tci"
  ) %>%
  mutate(
    time = ifelse(time == "tci_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tci)

data_long$tci <- factor(data_long$tci, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tci ~ time , data = data_long)

summary(model)





# All 37 within 12months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-12))
  )

Before_vs_after <- Before_vs_after %>% distinct()

Before_vs_after %>% group_by(tci_before) %>% count()  %>% mutate(n=n/37)
Before_vs_after %>% group_by(tci_after) %>% count()  %>% mutate(n=n/37)
 
mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) #
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25)
quantile(Before_vs_after$elapsed_before, 0.75)

mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25)
quantile(Before_vs_after$elapsed_after, 0.75)

wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()

Before_vs_after %>% 
  group_by(tci_before, tci_after) %>% count()


data <- matrix(c(5, 6, 0, 26), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))


mcnemar.test(data)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tci_before, tci_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tci"
  ) %>%
  mutate(
    time = ifelse(time == "tci_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tci)

data_long$tci <- factor(data_long$tci, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tci ~ time , data = data_long)

summary(model)


# All 13 within 6months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=6) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>%
      filter(elapsed_after>=(-6))
  )

Before_vs_after %>% group_by(tci_before) %>% count()
Before_vs_after %>% group_by(tci_after) %>% count()

mean(Before_vs_after$tci_before) 
sd(Before_vs_after$tci_before) 
median(Before_vs_after$elapsed_before) 

mean(Before_vs_after$tci_after) 
sd(Before_vs_after$tci_after) 
median(Before_vs_after$elapsed_after) 

wilcox.test(Before_vs_after$tci_before, Before_vs_after$tci_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% 
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()

data <- matrix(c(1, 2, 0, 10), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)


# ------------------


# LEDD  Before vs After Using Paired Samples -------------------

Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342





data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, ttt_ledd_ago, ttt_ledd_totale,  pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(act_datedeb==pompe_date) # 19 with visit same date as pump introduced / 18 with tci 5.5%

apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  filter(!is.na(pompe_date)) %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>% #617 visits
  # select(anonyme_id) %>% distinct() # 330
  #filter(abs(elapsed)<1) %>% select(anonyme_id) %>% distinct() # 35
  #filter(abs(elapsed)<2) %>% select(anonyme_id) %>% distinct() # 58
  #filter(abs(elapsed)<3) %>% select(anonyme_id) %>% distinct() # 71
  #filter(abs(elapsed)<4) %>% select(anonyme_id) %>% distinct() # 82
  #filter(abs(elapsed)<5) %>% select(anonyme_id) %>% distinct() # 86
  filter(abs(elapsed)<6) %>% select(anonyme_id) %>% distinct() # 93



data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

# data <- data %>% arrange(anonyme_id, redcap_repeat_instance) %>% group_by(anonyme_id) %>%
#   fill(pompe_date, .direction = "down") %>% fill(pompe_date, .direction = "up") %>%
#   fill(tci, .direction = "down") %>% fill(tci, .direction = "up") 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, ttt_ledd_ago, ttt_ledd_totale))

unique(first_apo$ttt_ledd_ago)

first_apo %>% ungroup() %>% filter(!is.na(ttt_ledd_totale)) %>%
  select(anonyme_id) %>% distinct() # 258 known ledd status at some point

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(!is.na(ttt_ledd_totale)) %>%
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))



# All 88 available

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_before"="ttt_ledd_ago") %>% rename("ledd_totale_before"="ttt_ledd_totale") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_after"="ttt_ledd_ago") %>% rename("ledd_totale_after"="ttt_ledd_totale") %>%  rename("elapsed_after"="elapsed") 
  )



Before_vs_after$ledd_ago_before <- as.numeric(Before_vs_after$ledd_ago_before)
Before_vs_after$ledd_ago_after <- as.numeric(Before_vs_after$ledd_ago_after)
Before_vs_after$ledd_totale_before <- as.numeric(Before_vs_after$ledd_totale_before)
Before_vs_after$ledd_totale_after <- as.numeric(Before_vs_after$ledd_totale_after)


range(Before_vs_after$ledd_ago_before, na.rm=T)
range(Before_vs_after$ledd_ago_after, na.rm=T)
range(Before_vs_after$ledd_totale_before, na.rm=T)
range(Before_vs_after$ledd_totale_after, na.rm=T)


Before_vs_after <- Before_vs_after %>% filter(ledd_ago_before<10000&
                                                ledd_ago_after<10000&
                                                ledd_totale_before<10000&
                                                ledd_totale_after<10000)


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ledd_ago_before=max(ledd_ago_before)) %>%
  mutate(ledd_totale_before =max(ledd_totale_before )) %>%  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ledd_ago_after=max(ledd_ago_after)) %>%
  mutate(ledd_totale_after =max(ledd_totale_after )) %>%  ungroup()  %>% distinct()


# 88

mean(Before_vs_after$ledd_ago_before, na.rm=T) 
sd(Before_vs_after$ledd_ago_before, na.rm=T)  
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25) 
quantile(Before_vs_after$elapsed_before, 0.75) 

mean(Before_vs_after$ledd_ago_after, na.rm=T) 
sd(Before_vs_after$ledd_ago_after, na.rm=T)  
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25) 
quantile(Before_vs_after$elapsed_after, 0.75) 

median(Before_vs_after$ledd_ago_before) 
quantile(Before_vs_after$ledd_ago_before, 0.25) 
quantile(Before_vs_after$ledd_ago_before, 0.75) 

median(Before_vs_after$ledd_ago_after) 
quantile(Before_vs_after$ledd_ago_after, 0.25) 
quantile(Before_vs_after$ledd_ago_after, 0.75) 

wilcox.test(Before_vs_after$ledd_ago_before, Before_vs_after$ledd_ago_after, paired = TRUE, alternative = "two.sided")



mean(Before_vs_after$ledd_totale_before, na.rm=T) 
sd(Before_vs_after$ledd_totale_before, na.rm=T)  
median(Before_vs_after$elapsed_before) 
quantile(Before_vs_after$elapsed_before, 0.25) 
quantile(Before_vs_after$elapsed_before, 0.75) 

mean(Before_vs_after$ledd_totale_after, na.rm=T) 
sd(Before_vs_after$ledd_ago_after, na.rm=T)  
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25) 
quantile(Before_vs_after$elapsed_after, 0.75) 

wilcox.test(Before_vs_after$ledd_totale_before, Before_vs_after$ledd_totale_after, paired = TRUE, alternative = "two.sided")

median(Before_vs_after$ledd_totale_before) 
quantile(Before_vs_after$ledd_totale_before, 0.25) 
quantile(Before_vs_after$ledd_totale_before, 0.75) 

median(Before_vs_after$ledd_totale_after) 
quantile(Before_vs_after$ledd_totale_after, 0.25) 
quantile(Before_vs_after$ledd_totale_after, 0.75) 

image <- Before_vs_after %>% select(ledd_totale_before, ledd_totale_after) %>%
  gather(Eval, LEDD, ledd_totale_before:ledd_totale_after) %>%
  mutate(Eval=ifelse(Eval=="ledd_totale_before", "A) Before", "B) After")) %>%
  ggplot(aes(Eval, LEDD, colour=Eval, fill=Eval)) +
  geom_boxplot(alpha=0.5, notch = T,width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=1.5 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) +
  xlab("\n Evaluation [Pre vs Post Pump]") +
  ylab("LEDD [Overall Dopa] \n")

ggsave(file="p88.svg", plot=image, width=4, height=4)

# All 73 within 60 months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_before"="ttt_ledd_ago") %>% 
  rename("ledd_totale_before"="ttt_ledd_totale") %>% rename("elapsed_before"="elapsed") %>% 
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_after"="ttt_ledd_ago") %>% 
      rename("ledd_totale_after"="ttt_ledd_totale") %>%  rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60))
  )



Before_vs_after$ledd_ago_before <- as.numeric(Before_vs_after$ledd_ago_before)
Before_vs_after$ledd_ago_after <- as.numeric(Before_vs_after$ledd_ago_after)
Before_vs_after$ledd_totale_before <- as.numeric(Before_vs_after$ledd_totale_before)
Before_vs_after$ledd_totale_after <- as.numeric(Before_vs_after$ledd_totale_after)


range(Before_vs_after$ledd_ago_before, na.rm=T)
range(Before_vs_after$ledd_ago_after, na.rm=T)


Before_vs_after <- Before_vs_after %>% filter(ledd_ago_before<10000&
                                                ledd_ago_after<10000&
                                                ledd_totale_before<10000&
                                                ledd_totale_after<10000)


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ledd_ago_before=max(ledd_ago_before)) %>%
  mutate(ledd_totale_before =max(ledd_totale_before )) %>%  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ledd_ago_after=max(ledd_ago_after)) %>%
  mutate(ledd_totale_after =max(ledd_totale_after )) %>%  ungroup()  %>% distinct()

# 73

mean(Before_vs_after$ledd_totale_before,na.rm=T) 
sd(Before_vs_after$ledd_totale_before, na.rm=T)  
median(Before_vs_after$elapsed_before)  
quantile(Before_vs_after$elapsed_before, 0.25) 
quantile(Before_vs_after$elapsed_before, 0.75) 

mean(Before_vs_after$ledd_totale_after, na.rm=T)  
sd(Before_vs_after$ledd_totale_after, na.rm=T)  
median(Before_vs_after$elapsed_after)  
quantile(Before_vs_after$elapsed_after, 0.25) 
quantile(Before_vs_after$elapsed_after, 0.75) 

median(Before_vs_after$ledd_ago_before)  
quantile(Before_vs_after$ledd_ago_before, 0.25) 
quantile(Before_vs_after$ledd_ago_before, 0.75) 


median(Before_vs_after$ledd_ago_after)  
quantile(Before_vs_after$ledd_ago_after, 0.25) 
quantile(Before_vs_after$ledd_ago_after, 0.75) 


wilcox.test(Before_vs_after$ledd_totale_before, Before_vs_after$ledd_totale_after, paired = TRUE, alternative = "two.sided")


median(Before_vs_after$ledd_totale_before)  
quantile(Before_vs_after$ledd_totale_before, 0.25) 
quantile(Before_vs_after$ledd_totale_before, 0.75) 


median(Before_vs_after$ledd_totale_after)  
quantile(Before_vs_after$ledd_totale_after, 0.25) 
quantile(Before_vs_after$ledd_totale_after, 0.75) 


image <- Before_vs_after %>% select(ledd_totale_before, ledd_totale_after) %>%
  gather(Eval, LEDD, ledd_totale_before:ledd_totale_after) %>%
  mutate(Eval=ifelse(Eval=="ledd_totale_before", "A) Before", "B) After")) %>%
  ggplot(aes(Eval, LEDD, colour=Eval, fill=Eval)) +
  geom_boxplot(alpha=0.5, notch = T,width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=1.5 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) +
  xlab("\n Evaluation [Pre vs Post Pump]") +
  ylab("LEDD [Overall Dopa] \n")

ggsave(file="p73.svg", plot=image, width=4, height=4)


# All 38 within 24months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_before"="ttt_ledd_ago") %>% 
  rename("ledd_totale_before"="ttt_ledd_totale") %>% rename("elapsed_before"="elapsed") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_after"="ttt_ledd_ago") %>% 
      rename("ledd_totale_after"="ttt_ledd_totale") %>%  rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24))
  )



Before_vs_after$ledd_ago_before <- as.numeric(Before_vs_after$ledd_ago_before)
Before_vs_after$ledd_ago_after <- as.numeric(Before_vs_after$ledd_ago_after)
Before_vs_after$ledd_totale_before <- as.numeric(Before_vs_after$ledd_totale_before)
Before_vs_after$ledd_totale_after <- as.numeric(Before_vs_after$ledd_totale_after)


range(Before_vs_after$ledd_ago_before, na.rm=T)
range(Before_vs_after$ledd_ago_after, na.rm=T)


Before_vs_after <- Before_vs_after %>% filter(ledd_ago_before<10000&
                                                ledd_ago_after<10000&
                                                ledd_totale_before<10000&
                                                ledd_totale_after<10000)


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ledd_ago_before=max(ledd_ago_before)) %>%
  mutate(ledd_totale_before =max(ledd_totale_before )) %>%  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ledd_ago_after=max(ledd_ago_after)) %>%
  mutate(ledd_totale_after =max(ledd_totale_after )) %>%  ungroup()  %>% distinct()

# 38

mean(Before_vs_after$ledd_totale_before, na.rm=T)  
sd(Before_vs_after$ledd_totale_before, na.rm=T)   
median(Before_vs_after$elapsed_before)  
quantile(Before_vs_after$elapsed_before, 0.25) 
quantile(Before_vs_after$elapsed_before, 0.75) 

mean(Before_vs_after$ledd_totale_after, na.rm=T)  
sd(Before_vs_after$ledd_totale_after, na.rm=T)  
median(Before_vs_after$elapsed_after) 
quantile(Before_vs_after$elapsed_after, 0.25) 
quantile(Before_vs_after$elapsed_after, 0.75) 

median(Before_vs_after$ledd_ago_before) 
quantile(Before_vs_after$ledd_ago_before, 0.25) 
quantile(Before_vs_after$ledd_ago_before, 0.75) 


median(Before_vs_after$ledd_ago_after) 
quantile(Before_vs_after$ledd_ago_after, 0.25) 
quantile(Before_vs_after$ledd_ago_after, 0.75) 


wilcox.test(Before_vs_after$ledd_totale_before, Before_vs_after$ledd_totale_after, paired = TRUE, alternative = "two.sided")


median(Before_vs_after$ledd_totale_before) 
quantile(Before_vs_after$ledd_totale_before, 0.25) 
quantile(Before_vs_after$ledd_totale_before, 0.75) 


median(Before_vs_after$ledd_totale_after) 
quantile(Before_vs_after$ledd_totale_after, 0.25) 
quantile(Before_vs_after$ledd_totale_after, 0.75) 


image <- Before_vs_after %>% select(ledd_totale_before, ledd_totale_after) %>%
  gather(Eval, LEDD, ledd_totale_before:ledd_totale_after) %>%
  mutate(Eval=ifelse(Eval=="ledd_totale_before", "A) Before", "B) After")) %>%
  ggplot(aes(Eval, LEDD, colour=Eval, fill=Eval)) +
  geom_boxplot(alpha=0.5, notch = T,width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=1.5 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) +
  xlab("\n Evaluation [Pre vs Post Pump]") +
  ylab("LEDD [Overall Dopa] \n")

ggsave(file="p38.svg", plot=image, width=4, height=4)

# All 28 within 12months

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_before"="ttt_ledd_ago") %>% 
  rename("ledd_totale_before"="ttt_ledd_totale") %>% rename("elapsed_before"="elapsed") %>% 
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, ttt_ledd_ago, ttt_ledd_totale, elapsed) %>% rename("ledd_ago_after"="ttt_ledd_ago") %>% 
      rename("ledd_totale_after"="ttt_ledd_totale") %>%  rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12))
  )



Before_vs_after$ledd_ago_before <- as.numeric(Before_vs_after$ledd_ago_before)
Before_vs_after$ledd_ago_after <- as.numeric(Before_vs_after$ledd_ago_after)
Before_vs_after$ledd_totale_before <- as.numeric(Before_vs_after$ledd_totale_before)
Before_vs_after$ledd_totale_after <- as.numeric(Before_vs_after$ledd_totale_after)


range(Before_vs_after$ledd_ago_before, na.rm=T)
range(Before_vs_after$ledd_ago_after, na.rm=T)


Before_vs_after <- Before_vs_after %>% filter(ledd_ago_before<10000&
                                                ledd_ago_after<10000&
                                                ledd_totale_before<10000&
                                                ledd_totale_after<10000)


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ledd_ago_before=max(ledd_ago_before)) %>%
  mutate(ledd_totale_before =max(ledd_totale_before )) %>%  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ledd_ago_after=max(ledd_ago_after)) %>%
  mutate(ledd_totale_after =max(ledd_totale_after )) %>%  ungroup()  %>% distinct()

# 28

mean(Before_vs_after$ledd_totale_before, na.rm=T)  
sd(Before_vs_after$ledd_totale_before, na.rm=T)   
median(Before_vs_after$elapsed_before)  
quantile(Before_vs_after$elapsed_before, 0.25) 
quantile(Before_vs_after$elapsed_before, 0.75) 

mean(Before_vs_after$ledd_totale_after, na.rm=T) 
sd(Before_vs_after$ledd_totale_after, na.rm=T)  
median(Before_vs_after$elapsed_after)  
quantile(Before_vs_after$elapsed_after, 0.25) 
quantile(Before_vs_after$elapsed_after, 0.75)  

median(Before_vs_after$ledd_ago_before)  
quantile(Before_vs_after$ledd_ago_before, 0.25) 
quantile(Before_vs_after$ledd_ago_before, 0.75)  

median(Before_vs_after$ledd_ago_after)  
quantile(Before_vs_after$ledd_ago_after, 0.25) 
quantile(Before_vs_after$ledd_ago_after, 0.75)  


wilcox.test(Before_vs_after$ledd_totale_before, Before_vs_after$ledd_totale_after, paired = TRUE, alternative = "two.sided")

median(Before_vs_after$ledd_totale_before)  
quantile(Before_vs_after$ledd_totale_before, 0.25) 
quantile(Before_vs_after$ledd_totale_before, 0.75)  

median(Before_vs_after$ledd_totale_after)  
quantile(Before_vs_after$ledd_totale_after, 0.25) 
quantile(Before_vs_after$ledd_totale_after, 0.75)  


image <- Before_vs_after %>% select(ledd_totale_before, ledd_totale_after) %>%
  gather(Eval, LEDD, ledd_totale_before:ledd_totale_after) %>%
  mutate(Eval=ifelse(Eval=="ledd_totale_before", "A) Before", "B) After")) %>%
  ggplot(aes(Eval, LEDD, colour=Eval, fill=Eval)) +
  geom_boxplot(alpha=0.5, notch = T,width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=1.5 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) +
  xlab("\n Evaluation [Pre vs Post Pump]") +
  ylab("LEDD [Overall Dopa] \n")

ggsave(file="p28.svg", plot=image, width=4, height=4)

# ------------------


# Doses for each individual dopamine agonist ----------

DAs <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date, ttt_ledd_ago,
                                        ttt_neu_rot2_yn___yes:ttt_apo_stylo)

DAs <- DAs %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() %>%
  left_join(DAs)

fwrite(DAs, "DAs.csv")

names(DAs)

DAs$act_datedeb <- as.Date(DAs$act_datedeb)
DAs$pompe_date <- as.Date(DAs$pompe_date)


DAs <- DAs %>% mutate(ttt_neu_rot2=ttt_neu_rot2_yn___yes*as.numeric(ttt_neu_rot2),
                      ttt_neu_rot4=ttt_neu_rot4_yn___yes*as.numeric(ttt_neu_rot4),
                      ttt_neu_rot6=ttt_neu_rot6_yn___yes*as.numeric(ttt_neu_rot6),
                      ttt_neu_rot8=ttt_neu_rot8_yn___yes*as.numeric(ttt_neu_rot8),
                      ttt_neu_rot2=ifelse(is.na(ttt_neu_rot2),0,ttt_neu_rot2),
                      ttt_neu_rot4=ifelse(is.na(ttt_neu_rot4),0,ttt_neu_rot4),
                      ttt_neu_rot6=ifelse(is.na(ttt_neu_rot6),0,ttt_neu_rot6),
                      ttt_neu_rot8=ifelse(is.na(ttt_neu_rot8),0,ttt_neu_rot8),
                      ttt_neu_rot=ttt_neu_rot2*2+ttt_neu_rot4*4+ttt_neu_rot6*6+ttt_neu_rot8*8) %>%
  select(-c(ttt_neu_rot2_yn___yes:ttt_neu_rot8))


DAs <- DAs %>% mutate(ttt_ral_brom5=ttt_ral_brom5_yn___yes*as.numeric(ttt_ral_brom5),
                      ttt_ral_brom10=ttt_ral_brom10_yn___yes*as.numeric(ttt_ral_brom10),
                      ttt_ral_brom2_5m=ttt_ral_brom2_5m_yn___yes*as.numeric(ttt_ral_brom2_5m),
                      ttt_ral_brom5=ifelse(is.na(ttt_ral_brom5),0,ttt_ral_brom5),
                      ttt_ral_brom10=ifelse(is.na(ttt_ral_brom10),0,ttt_ral_brom10),
                      ttt_ral_brom2_5m=ifelse(is.na(ttt_ral_brom2_5m),0,ttt_ral_brom2_5m),
                      ttt_ral_brom=ttt_ral_brom5*5+ttt_ral_brom10*10+ttt_ral_brom2_5m*2.5) %>%
  select(-c(ttt_ral_brom5_yn___yes:ttt_ral_brom2_5m))


DAs <- DAs %>% mutate(ttt_req_rop4=ttt_req_rop4_yn___yes*as.numeric(ttt_req_rop4),
                      ttt_req_rop8=ttt_req_rop8_yn___yes*as.numeric(ttt_req_rop8),
                      ttt_req_rop025=ttt_req_rop025_yn___yes*as.numeric(ttt_req_rop025),
                      ttt_req_rop050=ttt_req_rop050_yn___yes*as.numeric(ttt_req_rop050),
                      ttt_req_rop1=ttt_req_rop1_yn___yes*as.numeric(ttt_req_rop1),
                      ttt_req_rop2=ttt_req_rop2_yn___yes*as.numeric(ttt_req_rop2),
                      ttt_req_rop5=ttt_req_rop5_yn___yes*as.numeric(ttt_req_rop5),   
                      ttt_req_rop4=ifelse(is.na(ttt_req_rop4),0,ttt_req_rop4),
                      ttt_req_rop8=ifelse(is.na(ttt_req_rop8),0,ttt_req_rop8),
                      ttt_req_rop025=ifelse(is.na(ttt_req_rop025),0,ttt_req_rop025),
                      ttt_req_rop050=ifelse(is.na(ttt_req_rop050),0,ttt_req_rop050),
                      ttt_req_rop1=ifelse(is.na(ttt_req_rop1),0,ttt_req_rop1),
                      ttt_req_rop2=ifelse(is.na(ttt_req_rop2),0,ttt_req_rop2),
                      ttt_req_rop5=ifelse(is.na(ttt_req_rop5),0,ttt_req_rop5),
                      ttt_ral_brom=ttt_req_rop4*4+ttt_req_rop8*8+ttt_req_rop025*0.25+ttt_req_rop050*0.5+ttt_req_rop1*1+ttt_req_rop2*2+ttt_req_rop5*5) %>%
  select(-c(req_rop2_yn___yes:ttt_req_rop5))




DAs <- DAs %>% mutate(ttt_sif_pram026=ttt_sif_pram026_yn___yes*as.numeric(ttt_sif_pram026),
                      ttt_sif_pram052=ttt_sif_pram052_yn___yes*as.numeric(ttt_sif_pram052),
                      ttt_sif_pram105=ttt_sif_pram105_yn___yes*as.numeric(ttt_sif_pram105),
                      ttt_sif_pram050=ttt_sif_pram210_yn___yes*as.numeric(ttt_sif_pram210),
                      ttt_sif_pram018=ttt_sif_pram018_yn___yes*as.numeric(ttt_sif_pram018),
                      ttt_sif_pram070=ttt_sif_pram070_yn___yes*as.numeric(ttt_sif_pram070),
                      ttt_sif_pram026=ifelse(is.na(ttt_sif_pram026),0,ttt_sif_pram026),
                      ttt_sif_pram052=ifelse(is.na(ttt_sif_pram052),0,ttt_sif_pram052),
                      ttt_sif_pram105=ifelse(is.na(ttt_sif_pram105),0,ttt_sif_pram105),
                      ttt_sif_pram050=ifelse(is.na(ttt_sif_pram050),0,ttt_sif_pram050),
                      ttt_sif_pram018=ifelse(is.na(ttt_sif_pram018),0,ttt_sif_pram018),
                      ttt_sif_pram070=ifelse(is.na(ttt_sif_pram070),0,ttt_sif_pram070),
                      ttt_ral_brom=ttt_sif_pram026*0.26+ttt_sif_pram052*0.52+ttt_sif_pram105*1.05+ttt_sif_pram050*0.5+ttt_sif_pram018*0.18+ttt_sif_pram070*0.7) %>%
  select(-c(ttt_sif_pram026_yn___yes:ttt_sif_pram070))




DAs <- DAs %>% mutate(ttt_triv_prim20=ttt_triv_prim20_yn___yes*as.numeric(ttt_triv_prim20),
                      ttt_triv_prim20=ifelse(is.na(ttt_triv_prim20),0,ttt_triv_prim20),
                      ttt_triv_prim=ttt_triv_prim20*20) %>%
  select(-c(ttt_triv_prim20_yn___yes:ttt_triv_prim20))

DAs <- DAs %>% mutate(ttt_triv_per_lp50=ttt_triv_per_lp50_yn___yes*as.numeric(ttt_triv_per_lp50),
                      ttt_triv_per_lp50=ifelse(is.na(ttt_triv_per_lp50),0,ttt_triv_per_lp50),
                      ttt_triv_per=ttt_triv_per_lp50*50) %>%
  select(-c(ttt_triv_per_lp50_yn___yes:ttt_triv_per_lp50))


DAs <- DAs %>% mutate(ttt_apo_stylo=ttt_apo_stylo_yn___yes*as.numeric(ttt_apo_stylo),
                      ttt_apo_stylo=ifelse(is.na(ttt_apo_stylo),0,ttt_apo_stylo)) %>%
  select(-c(ttt_apo_stylo_yn___yes:ttt_apo_stylo_yn___dm))








data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))
first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  


first_apo %>% left_join(DAs %>% select(anonyme_id, act_datedeb, ttt_neu_rot ) %>% distinct()) %>%
  
  
  first_apo %>% left_join(DAs %>% select(anonyme_id, act_datedeb, ttt_ral_brom ) %>% distinct()) %>% 
  filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, elapsed, ttt_ral_brom ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("ttt_ral_brom_before"="ttt_ral_brom") %>%
  inner_join(
    first_apo %>% left_join(DAs %>% select(anonyme_id, act_datedeb, ttt_ral_brom ) %>% distinct())  %>% 
      filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, elapsed, ttt_ral_brom ) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>% rename("ttt_ral_brom_after"="ttt_ral_brom") 
  ) %>% ungroup() %>%
  summarise(mean_bef=mean(ttt_ral_brom_before ), mean_aft=mean(ttt_ral_brom_after))



first_apo %>% left_join(DAs %>% select(anonyme_id, act_datedeb, ttt_ral_brom ) %>% distinct()) %>% 
  filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, elapsed, ttt_ral_brom ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("ttt_ral_brom_before"="ttt_neu_rot") %>%
  inner_join(
    first_apo %>% left_join(DAs %>% select(anonyme_id, act_datedeb, ttt_ral_brom ) %>% distinct())  %>% 
      filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, elapsed, ttt_ral_brom ) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>% rename("ttt_neu_rot_after"="ttt_neu_rot") 
  ) %>% ungroup() %>%
  summarise(mean_bef=mean(ttt_neu_rot_before ), mean_aft=mean(ttt_neu_rot_after))


# ------------
# Motor Fluctuations  Before vs After Using Paired Samples -------------------

Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342





data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, fluct_motrice,  pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, fluct_motrice))

unique(first_apo$fluct_motrice)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(fluct_motrice %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(fluct_motrice=ifelse(fluct_motrice==">=2","2",fluct_motrice)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(fluct_motrice=as.numeric(fluct_motrice))





# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_before"="fluct_motrice") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_after"="fluct_motrice") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(fluct_motrice_before=max(fluct_motrice_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(fluct_motrice_after=max(fluct_motrice_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(fluct_motrice_before=ifelse(fluct_motrice_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(fluct_motrice_after=ifelse(fluct_motrice_after>=1,1,0)) %>%
  group_by(tci_before, fluct_motrice_before, 
           tci_after, fluct_motrice_after) %>% count()

# tci_before fluct_motrice_before tci_after fluct_motrice_after     n
# 1          0                    0         0                   0     9
# 2          0                    0         0                   1    15
# 3          0                    0         1                   1     3
# 4          0                    1         0                   0     7
# 5          0                    1         0                   1    72
# 6          0                    1         1                   0     1
# 7          0                    1         1                   1     6
# 8          1                    0         0                   1     4
# 9          1                    0         1                   1     1
# 10         1                    1         0                   0     2
# 11         1                    1         0                   1    16
# 12         1                    1         1                   0     1
# 13         1                    1         1                   1     8

mean(Before_vs_after$fluct_motrice_before,na.rm=T) 
sd(Before_vs_after$fluct_motrice_before, na.rm=T)  

mean(Before_vs_after$fluct_motrice_after, na.rm=T)  
sd(Before_vs_after$fluct_motrice_after, na.rm=T)  

wilcox.test(Before_vs_after$fluct_motrice_before, Before_vs_after$fluct_motrice_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(fluct_motrice_before) %>% count() %>% mutate(n=n/145)
Before_vs_after %>% group_by(fluct_motrice_after) %>% count() %>% mutate(n=n/145)







data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(fluct_motrice_before, fluct_motrice_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "fluct_motrice"
  ) %>%
  mutate(
    time = ifelse(time == "fluct_motrice_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$fluct_motrice)

data_long$fluct_motrice <- factor(data_long$fluct_motrice, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(fluct_motrice ~ time , data = data_long)

summary(model)




# All 120 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_before"="fluct_motrice") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_after"="fluct_motrice") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(fluct_motrice_before=max(fluct_motrice_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(fluct_motrice_after=max(fluct_motrice_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(fluct_motrice_before=ifelse(fluct_motrice_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(fluct_motrice_after=ifelse(fluct_motrice_after>=1,1,0)) %>%
  group_by(tci_before, fluct_motrice_before, 
           tci_after, fluct_motrice_after) %>% count()

# tci_before fluct_motrice_before tci_after fluct_motrice_after     n
# 1          0                    0         0                   0     6
# 2          0                    0         0                   1    11
# 3          0                    0         1                   1     2
# 4          0                    1         0                   0     6
# 5          0                    1         0                   1    62
# 6          0                    1         1                   0     1
# 7          0                    1         1                   1     5
# 8          1                    0         0                   1     3
# 9          1                    0         1                   1     1
# 10         1                    1         0                   0     1
# 11         1                    1         0                   1    13
# 12         1                    1         1                   0     1
# 13         1                    1         1                   1     8

mean(Before_vs_after$fluct_motrice_before,na.rm=T) 
sd(Before_vs_after$fluct_motrice_before, na.rm=T)  

mean(Before_vs_after$fluct_motrice_after, na.rm=T)  
sd(Before_vs_after$fluct_motrice_after, na.rm=T)  

wilcox.test(Before_vs_after$fluct_motrice_before, Before_vs_after$fluct_motrice_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(fluct_motrice_before) %>% count() %>% mutate(n=n/120)
Before_vs_after %>% group_by(fluct_motrice_after) %>% count() %>% mutate(n=n/120)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(fluct_motrice_before, fluct_motrice_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "fluct_motrice"
  ) %>%
  mutate(
    time = ifelse(time == "fluct_motrice_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$fluct_motrice)

data_long$fluct_motrice <- factor(data_long$fluct_motrice, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(fluct_motrice ~ time , data = data_long)

summary(model)


# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_before"="fluct_motrice") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_after"="fluct_motrice") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(fluct_motrice_before=max(fluct_motrice_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(fluct_motrice_after=max(fluct_motrice_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(fluct_motrice_before=ifelse(fluct_motrice_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(fluct_motrice_after=ifelse(fluct_motrice_after>=1,1,0)) %>%
  group_by(tci_before, fluct_motrice_before, 
           tci_after, fluct_motrice_after) %>% count()

# tci_before fluct_motrice_before tci_after fluct_motrice_after     n
# 1          0                    0         0                   0     1
# 2          0                    0         0                   1     2
# 3          0                    1         0                   0     3
# 4          0                    1         0                   1    36
# 5          0                    1         1                   1     1
# 6          1                    0         0                   1     1
# 7          1                    1         0                   0     1
# 8          1                    1         0                   1     5
# 9          1                    1         1                   0     1
# 10         1                    1         1                   1     6

mean(Before_vs_after$fluct_motrice_before,na.rm=T) 
sd(Before_vs_after$fluct_motrice_before, na.rm=T)  

mean(Before_vs_after$fluct_motrice_after, na.rm=T)  
sd(Before_vs_after$fluct_motrice_after, na.rm=T)  

wilcox.test(Before_vs_after$fluct_motrice_before, Before_vs_after$fluct_motrice_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(fluct_motrice_before) %>% count() %>% mutate(n=n/57)
Before_vs_after %>% group_by(fluct_motrice_after) %>% count() %>% mutate(n=n/57)







data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(fluct_motrice_before, fluct_motrice_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "fluct_motrice"
  ) %>%
  mutate(
    time = ifelse(time == "fluct_motrice_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$fluct_motrice)

data_long$fluct_motrice <- factor(data_long$fluct_motrice, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(fluct_motrice ~ time , data = data_long)

summary(model)


# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_before"="fluct_motrice") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, fluct_motrice, elapsed) %>% rename("fluct_motrice_after"="fluct_motrice") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(fluct_motrice_before=max(fluct_motrice_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(fluct_motrice_after=max(fluct_motrice_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(fluct_motrice_before=ifelse(fluct_motrice_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(fluct_motrice_after=ifelse(fluct_motrice_after>=1,1,0)) %>%
  group_by(tci_before, fluct_motrice_before, 
           tci_after, fluct_motrice_after) %>% count()



Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(fluct_motrice_before=ifelse(fluct_motrice_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(fluct_motrice_after=ifelse(fluct_motrice_after>=1,1,0)) %>%
  group_by(tci_before, tci_after) %>% count()



# tci_before fluct_motrice_before tci_after fluct_motrice_after     n
# 1          0                    0         0                   0     1
# 2          0                    0         0                   1     1
# 3          0                    1         0                   0     2
# 4          0                    1         0                   1    22
# 5          1                    1         0                   0     1
# 6          1                    1         0                   1     4
# 7          1                    1         1                   1     5

mean(Before_vs_after$fluct_motrice_before,na.rm=T) 
sd(Before_vs_after$fluct_motrice_before, na.rm=T)  

mean(Before_vs_after$fluct_motrice_after, na.rm=T)  
sd(Before_vs_after$fluct_motrice_after, na.rm=T)  

wilcox.test(Before_vs_after$fluct_motrice_before, Before_vs_after$fluct_motrice_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(fluct_motrice_before) %>% count() %>% mutate(n=n/36)
Before_vs_after %>% group_by(fluct_motrice_after) %>% count() %>% mutate(n=n/36)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(fluct_motrice_before, fluct_motrice_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "fluct_motrice"
  ) %>%
  mutate(
    time = ifelse(time == "fluct_motrice_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$fluct_motrice)

data_long$fluct_motrice <- factor(data_long$fluct_motrice, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(fluct_motrice ~ time , data = data_long)

summary(model)

# ------------------


# Dyskinesias Before vs After Using Paired Samples -------------------

Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342





data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, dyskinesie,  pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

data %>% select(anonyme_id, pompe_date) %>% distinct() %>% drop_na() %>%
  group_by(anonyme_id) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, dyskinesie))

unique(first_apo$dyskinesie)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(dyskinesie %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(dyskinesie=ifelse(dyskinesie==">=2","2",dyskinesie)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(dyskinesie=as.numeric(dyskinesie))





# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_before"="dyskinesie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_after"="dyskinesie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dyskinesie_before=max(dyskinesie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dyskinesie_after=max(dyskinesie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dyskinesie_before=ifelse(dyskinesie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dyskinesie_after=ifelse(dyskinesie_after>=1,1,0)) %>%
  group_by(tci_before, dyskinesie_before, 
           tci_after, dyskinesie_after) %>% count()

# tci_before dyskinesie_before tci_after dyskinesie_after     n
# 1          0                 0         0                0    21
# 2          0                 0         0                1    23
# 3          0                 0         1                0     2
# 4          0                 0         1                1     5
# 5          0                 1         0                0     8
# 6          0                 1         0                1    51
# 7          0                 1         1                0     1
# 8          0                 1         1                1     2
# 9          1                 0         0                0     1
# 10         1                 0         0                1     5
# 11         1                 0         1                0     2
# 12         1                 0         1                1     2
# 13         1                 1         0                0     2
# 14         1                 1         0                1    14
# 15         1                 1         1                1     6

mean(Before_vs_after$dyskinesie_before,na.rm=T) 
sd(Before_vs_after$dyskinesie_before, na.rm=T)  

mean(Before_vs_after$dyskinesie_after, na.rm=T)  
sd(Before_vs_after$dyskinesie_after, na.rm=T)  

wilcox.test(Before_vs_after$dyskinesie_before, Before_vs_after$dyskinesie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dyskinesie_before) %>% count() %>% mutate(n=n/145)
Before_vs_after %>% group_by(dyskinesie_after) %>% count() %>% mutate(n=n/145)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dyskinesie_before, dyskinesie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dyskinesie"
  ) %>%
  mutate(
    time = ifelse(time == "dyskinesie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dyskinesie)

data_long$dyskinesie <- factor(data_long$dyskinesie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dyskinesie ~ time , data = data_long)

summary(model)



# All 120 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_before"="dyskinesie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_after"="dyskinesie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dyskinesie_before=max(dyskinesie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dyskinesie_after=max(dyskinesie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dyskinesie_before=ifelse(dyskinesie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dyskinesie_after=ifelse(dyskinesie_after>=1,1,0)) %>%
  group_by(tci_before, dyskinesie_before, 
           tci_after, dyskinesie_after) %>% count()

# tci_before dyskinesie_before tci_after dyskinesie_after     n
#   1          0                 0         0                0    18
# 2          0                 0         0                1    17
# 3          0                 0         1                0     1
# 4          0                 0         1                1     5
# 5          0                 1         0                0     6
# 6          0                 1         0                1    44
# 7          0                 1         1                0     1
# 8          0                 1         1                1     1
# 9          1                 0         0                0     1
# 10          1                 0         0                1     3
# 11          1                 0         1                0     2
# 12          1                 0         1                1     2
# 13          1                 1         0                0     1
# 14          1                 1         0                1    12
# 15          1                 1         1                1     6

mean(Before_vs_after$dyskinesie_before,na.rm=T) 
sd(Before_vs_after$dyskinesie_before, na.rm=T)  

mean(Before_vs_after$dyskinesie_after, na.rm=T)  
sd(Before_vs_after$dyskinesie_after, na.rm=T)  

wilcox.test(Before_vs_after$dyskinesie_before, Before_vs_after$dyskinesie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dyskinesie_before) %>% count() %>% mutate(n=n/120)
Before_vs_after %>% group_by(dyskinesie_after) %>% count() %>% mutate(n=n/120)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dyskinesie_before, dyskinesie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dyskinesie"
  ) %>%
  mutate(
    time = ifelse(time == "dyskinesie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dyskinesie)

data_long$dyskinesie <- factor(data_long$dyskinesie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dyskinesie ~ time , data = data_long)

summary(model)



# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_before"="dyskinesie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_after"="dyskinesie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dyskinesie_before=max(dyskinesie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dyskinesie_after=max(dyskinesie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dyskinesie_before=ifelse(dyskinesie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dyskinesie_after=ifelse(dyskinesie_after>=1,1,0)) %>%
  group_by(tci_before, dyskinesie_before, 
           tci_after, dyskinesie_after) %>% count()

# tci_before dyskinesie_before tci_after dyskinesie_after     n
#   1          0                 0         0                0     5
# 2          0                 0         0                1     5
# 3          0                 0         1                1     1
# 4          0                 1         0                0     5
# 5          0                 1         0                1    25
# 6          1                 0         0                1     1
# 7          1                 0         1                0     2
# 8          1                 0         1                1     1
# 9          1                 1         0                0     1
# 10          1                 1         0                1     5
# 11          1                 1         1                1     4

mean(Before_vs_after$dyskinesie_before,na.rm=T) 
sd(Before_vs_after$dyskinesie_before, na.rm=T)  

mean(Before_vs_after$dyskinesie_after, na.rm=T)  
sd(Before_vs_after$dyskinesie_after, na.rm=T)  

wilcox.test(Before_vs_after$dyskinesie_before, Before_vs_after$dyskinesie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dyskinesie_before) %>% count() %>% mutate(n=n/55)
Before_vs_after %>% group_by(dyskinesie_after) %>% count() %>% mutate(n=n/55)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dyskinesie_before, dyskinesie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dyskinesie"
  ) %>%
  mutate(
    time = ifelse(time == "dyskinesie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dyskinesie)

data_long$dyskinesie <- factor(data_long$dyskinesie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dyskinesie ~ time , data = data_long)

summary(model)


# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_before"="dyskinesie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dyskinesie, elapsed) %>% rename("dyskinesie_after"="dyskinesie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dyskinesie_before=max(dyskinesie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dyskinesie_after=max(dyskinesie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dyskinesie_before=ifelse(dyskinesie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dyskinesie_after=ifelse(dyskinesie_after>=1,1,0)) %>%
  group_by(tci_before, dyskinesie_before, 
           tci_after, dyskinesie_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dyskinesie_before=ifelse(dyskinesie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dyskinesie_after=ifelse(dyskinesie_after>=1,1,0)) %>%
  group_by(tci_before, dyskinesie_before) %>% count()



# tci_before dyskinesie_before tci_after dyskinesie_after     n
# 1          0                 0         0                0     5
# 2          0                 1         0                0     3
# 3          0                 1         0                1    18
# 4          1                 0         1                0     2
# 5          1                 0         1                1     1
# 6          1                 1         0                0     1
# 7          1                 1         0                1     4
# 8          1                 1         1                1     2

mean(Before_vs_after$dyskinesie_before,na.rm=T) 
sd(Before_vs_after$dyskinesie_before, na.rm=T)  

mean(Before_vs_after$dyskinesie_after, na.rm=T)  
sd(Before_vs_after$dyskinesie_after, na.rm=T)  

wilcox.test(Before_vs_after$dyskinesie_before, Before_vs_after$dyskinesie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dyskinesie_before) %>% count() %>% mutate(n=n/36)
Before_vs_after %>% group_by(dyskinesie_after) %>% count() %>% mutate(n=n/36)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dyskinesie_before, dyskinesie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dyskinesie"
  ) %>%
  mutate(
    time = ifelse(time == "dyskinesie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dyskinesie)

data_long$dyskinesie <- factor(data_long$dyskinesie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dyskinesie ~ time , data = data_long)

summary(model)


# ------------------


# Adjust Pompe Dose to to ttt_led_ago diff ------------
ignore <- Consultation_20241028 %>% select(anonyme_id, pompe_dose, ttt_ledd_ago)
ignore <- ignore %>% filter(!is.na(pompe_dose)) %>% filter(!is.na(ttt_ledd_ago))
ignore <- ignore %>% mutate(pompe_dose=parse_number(pompe_dose)) %>% filter(pompe_dose>0)

ignore$ttt_ledd_ago <- as.numeric(ignore$ttt_ledd_ago)
ignore <- ignore %>% filter(pompe_dose<=250)
ignore <- ignore %>% filter(pompe_dose>=5)


range(ignore$ttt_ledd_ago)
mean(ignore$ttt_ledd_ago)

ignore %>% mutate(diff=ttt_ledd_ago -( pompe_dose*10)) %>%
   summarise(mean=mean(diff), sd=sd(diff)) #  42.3  95.8

image <- ignore %>% mutate(pompe_dose=pompe_dose*10) %>%
  gather(Therapy, dose, pompe_dose:ttt_ledd_ago ) %>%
  mutate(Therapy=ifelse(Therapy=="pompe_dose", "Apomorphine_Pump", "All Dopa Agonists [Pump-inc]")) %>%
  ggplot(aes(dose, fill=Therapy, colour=Therapy))  + 
  geom_density(adjust = 0.4,alpha=0.6) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) +
  xlab("\n LEDD Dose") +
  ylab("Patient density \n")

ggsave(file="ledds.svg", plot=image, width=7, height=4)



# ------------------

# dysarthrie Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, dysarthrie,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, dysarthrie))

unique(first_apo$dysarthrie)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(dysarthrie %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(dysarthrie=ifelse(dysarthrie==">=2","2",dysarthrie)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(dysarthrie=as.numeric(dysarthrie))





# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_before"="dysarthrie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_after"="dysarthrie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dysarthrie_before=max(dysarthrie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dysarthrie_after=max(dysarthrie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dysarthrie_before=ifelse(dysarthrie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dysarthrie_after=ifelse(dysarthrie_after>=1,1,0)) %>%
  group_by(tci_before, dysarthrie_before, 
           tci_after, dysarthrie_after) %>% count()

# tci_before dysarthrie_before tci_after dysarthrie_after     n
#   1          0                 0         0                0    33
# 2          0                 0         0                1    30
# 3          0                 0         1                0     2
# 4          0                 0         1                1     4
# 5          0                 1         0                0     8
# 6          0                 1         0                1    31
# 7          0                 1         1                1     3
# 8          1                 0         0                0     7
# 9          1                 0         0                1     6
# 10          1                 0         1                0     6
# 11          1                 1         0                0     2
# 12          1                 1         0                1     6
# 13          1                 1         1                1     4

mean(Before_vs_after$dysarthrie_before,na.rm=T) 
sd(Before_vs_after$dysarthrie_before, na.rm=T)  

mean(Before_vs_after$dysarthrie_after, na.rm=T)  
sd(Before_vs_after$dysarthrie_after, na.rm=T)  

wilcox.test(Before_vs_after$dysarthrie_before, Before_vs_after$dysarthrie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dysarthrie_before) %>% count() %>% mutate(n=n/142)
Before_vs_after %>% group_by(dysarthrie_after) %>% count() %>% mutate(n=n/142)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dysarthrie_before, dysarthrie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dysarthrie"
  ) %>%
  mutate(
    time = ifelse(time == "dysarthrie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dysarthrie)

data_long$dysarthrie <- factor(data_long$dysarthrie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dysarthrie ~ time , data = data_long)

summary(model)


# All 117 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_before"="dysarthrie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_after"="dysarthrie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dysarthrie_before=max(dysarthrie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dysarthrie_after=max(dysarthrie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dysarthrie_before=ifelse(dysarthrie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dysarthrie_after=ifelse(dysarthrie_after>=1,1,0)) %>%
  group_by(tci_before, dysarthrie_before, 
           tci_after, dysarthrie_after) %>% count()

# tci_before dysarthrie_before tci_after dysarthrie_after     n
#   1          0                 0         0                0    28
# 2          0                 0         0                1    20
# 3          0                 0         1                0     1
# 4          0                 0         1                1     3
# 5          0                 1         0                0     7
# 6          0                 1         0                1    29
# 7          0                 1         1                1     3
# 8          1                 0         0                0     6
# 9          1                 0         0                1     2
# 10          1                 0         1                0     6
# 11          1                 1         0                0     2
# 12          1                 1         0                1     6
# 13          1                 1         1                1     4

mean(Before_vs_after$dysarthrie_before,na.rm=T) 
sd(Before_vs_after$dysarthrie_before, na.rm=T)  

mean(Before_vs_after$dysarthrie_after, na.rm=T)  
sd(Before_vs_after$dysarthrie_after, na.rm=T)  

wilcox.test(Before_vs_after$dysarthrie_before, Before_vs_after$dysarthrie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dysarthrie_before) %>% count() %>% mutate(n=n/117)
Before_vs_after %>% group_by(dysarthrie_after) %>% count() %>% mutate(n=n/117)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dysarthrie_before, dysarthrie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dysarthrie"
  ) %>%
  mutate(
    time = ifelse(time == "dysarthrie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dysarthrie)

data_long$dysarthrie <- factor(data_long$dysarthrie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dysarthrie ~ time , data = data_long)

summary(model)

# All 54 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_before"="dysarthrie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_after"="dysarthrie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dysarthrie_before=max(dysarthrie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dysarthrie_after=max(dysarthrie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dysarthrie_before=ifelse(dysarthrie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dysarthrie_after=ifelse(dysarthrie_after>=1,1,0)) %>%
  group_by(tci_before, dysarthrie_before, 
           tci_after, dysarthrie_after) %>% count()

# tci_before dysarthrie_before tci_after dysarthrie_after     n
#   1          0                 0         0                0    14
# 2          0                 0         0                1     4
# 3          0                 0         1                1     1
# 4          0                 1         0                0     6
# 5          0                 1         0                1    15
# 6          1                 0         0                0     4
# 7          1                 0         1                0     4
# 8          1                 1         0                1     3
# 9          1                 1         1                1     3

mean(Before_vs_after$dysarthrie_before,na.rm=T) 
sd(Before_vs_after$dysarthrie_before, na.rm=T)  

mean(Before_vs_after$dysarthrie_after, na.rm=T)  
sd(Before_vs_after$dysarthrie_after, na.rm=T)  

wilcox.test(Before_vs_after$dysarthrie_before, Before_vs_after$dysarthrie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dysarthrie_before) %>% count() %>% mutate(n=n/54)
Before_vs_after %>% group_by(dysarthrie_after) %>% count() %>% mutate(n=n/54)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dysarthrie_before, dysarthrie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dysarthrie"
  ) %>%
  mutate(
    time = ifelse(time == "dysarthrie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dysarthrie)

data_long$dysarthrie <- factor(data_long$dysarthrie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dysarthrie ~ time , data = data_long)

summary(model)


# All 35 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_before"="dysarthrie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, dysarthrie, elapsed) %>% rename("dysarthrie_after"="dysarthrie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(dysarthrie_before=max(dysarthrie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(dysarthrie_after=max(dysarthrie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dysarthrie_before=ifelse(dysarthrie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dysarthrie_after=ifelse(dysarthrie_after>=1,1,0)) %>%
  group_by(tci_before, dysarthrie_before, 
           tci_after, dysarthrie_after) %>% count()

# tci_before dysarthrie_before tci_after dysarthrie_after     n
# <dbl>             <dbl>     <dbl>            <dbl> <int>
#   1          0                 0         0                0     8
# 2          0                 0         0                1     2
# 3          0                 1         0                0     3
# 4          0                 1         0                1    11
# 5          1                 0         0                0     3
# 6          1                 0         1                0     3
# 7          1                 1         0                1     3
# 8          1                 1         1                1     2

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(dysarthrie_before=ifelse(dysarthrie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(dysarthrie_after=ifelse(dysarthrie_after>=1,1,0)) %>%
  group_by(tci_after, dysarthrie_after) %>% count()




mean(Before_vs_after$dysarthrie_before,na.rm=T) 
sd(Before_vs_after$dysarthrie_before, na.rm=T)  

mean(Before_vs_after$dysarthrie_after, na.rm=T)  
sd(Before_vs_after$dysarthrie_after, na.rm=T)  

wilcox.test(Before_vs_after$dysarthrie_before, Before_vs_after$dysarthrie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(dysarthrie_before) %>% count() %>% mutate(n=n/35)
Before_vs_after %>% group_by(dysarthrie_after) %>% count() %>% mutate(n=n/35)


cor(Before_vs_after$tci_before, Before_vs_after$dysarthrie_before)
cor(Before_vs_after$tci_after, Before_vs_after$dysarthrie_after)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(dysarthrie_before, dysarthrie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "dysarthrie"
  ) %>%
  mutate(
    time = ifelse(time == "dysarthrie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$dysarthrie)

data_long$dysarthrie <- factor(data_long$dysarthrie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(dysarthrie ~ time , data = data_long)

summary(model)

# ------------------


# freezing Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, freezing,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, freezing))

unique(first_apo$freezing)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(freezing %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(freezing=ifelse(freezing==">=2","2",freezing)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(freezing=as.numeric(freezing))





# All 144 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_before"="freezing") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_after"="freezing") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(freezing_before=max(freezing_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(freezing_after=max(freezing_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(freezing_before=ifelse(freezing_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(freezing_after=ifelse(freezing_after>=1,1,0)) %>%
  group_by(tci_before, freezing_before, 
           tci_after, freezing_after) %>% count()


mean(Before_vs_after$freezing_before,na.rm=T) 
sd(Before_vs_after$freezing_before, na.rm=T)  

mean(Before_vs_after$freezing_after, na.rm=T)  
sd(Before_vs_after$freezing_after, na.rm=T)  

wilcox.test(Before_vs_after$freezing_before, Before_vs_after$freezing_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(freezing_before) %>% count() %>% mutate(n=n/144)
Before_vs_after %>% group_by(freezing_after) %>% count() %>% mutate(n=n/144)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(freezing_before, freezing_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "freezing"
  ) %>%
  mutate(
    time = ifelse(time == "freezing_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$freezing)

data_long$freezing <- factor(data_long$freezing, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(freezing ~ time , data = data_long)

summary(model)


# All 117 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_before"="freezing") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_after"="freezing") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(freezing_before=max(freezing_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(freezing_after=max(freezing_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(freezing_before=ifelse(freezing_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(freezing_after=ifelse(freezing_after>=1,1,0)) %>%
  group_by(tci_before, freezing_before, 
           tci_after, freezing_after) %>% count()


mean(Before_vs_after$freezing_before,na.rm=T) 
sd(Before_vs_after$freezing_before, na.rm=T)  

mean(Before_vs_after$freezing_after, na.rm=T)  
sd(Before_vs_after$freezing_after, na.rm=T)  

wilcox.test(Before_vs_after$freezing_before, Before_vs_after$freezing_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(freezing_before) %>% count() %>% mutate(n=n/117)
Before_vs_after %>% group_by(freezing_after) %>% count() %>% mutate(n=n/117)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(freezing_before, freezing_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "freezing"
  ) %>%
  mutate(
    time = ifelse(time == "freezing_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$freezing)

data_long$freezing <- factor(data_long$freezing, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(freezing ~ time , data = data_long)

summary(model)


# All 54 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_before"="freezing") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_after"="freezing") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(freezing_before=max(freezing_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(freezing_after=max(freezing_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(freezing_before=ifelse(freezing_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(freezing_after=ifelse(freezing_after>=1,1,0)) %>%
  group_by(tci_before, freezing_before, 
           tci_after, freezing_after) %>% count()

mean(Before_vs_after$freezing_before,na.rm=T) 
sd(Before_vs_after$freezing_before, na.rm=T)  

mean(Before_vs_after$freezing_after, na.rm=T)  
sd(Before_vs_after$freezing_after, na.rm=T)  

wilcox.test(Before_vs_after$freezing_before, Before_vs_after$freezing_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(freezing_before) %>% count() %>% mutate(n=n/54)
Before_vs_after %>% group_by(freezing_after) %>% count() %>% mutate(n=n/54)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(freezing_before, freezing_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "freezing"
  ) %>%
  mutate(
    time = ifelse(time == "freezing_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$freezing)

data_long$freezing <- factor(data_long$freezing, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(freezing ~ time , data = data_long)

summary(model)




# All 34 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_before"="freezing") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, freezing, elapsed) %>% rename("freezing_after"="freezing") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(freezing_before=max(freezing_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(freezing_after=max(freezing_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(freezing_before=ifelse(freezing_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(freezing_after=ifelse(freezing_after>=1,1,0)) %>%
  group_by(tci_before, freezing_before, 
           tci_after, freezing_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(freezing_before=ifelse(freezing_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(freezing_after=ifelse(freezing_after>=1,1,0)) %>%
  group_by(tci_after, freezing_after) %>% count()




mean(Before_vs_after$freezing_before,na.rm=T) 
sd(Before_vs_after$freezing_before, na.rm=T)  

mean(Before_vs_after$freezing_after, na.rm=T)  
sd(Before_vs_after$freezing_after, na.rm=T)  

wilcox.test(Before_vs_after$freezing_before, Before_vs_after$freezing_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(freezing_before) %>% count() %>% mutate(n=n/34)
Before_vs_after %>% group_by(freezing_after) %>% count() %>% mutate(n=n/34)


cor(Before_vs_after$tci_before, Before_vs_after$freezing_before)
cor(Before_vs_after$tci_after, Before_vs_after$freezing_after)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(freezing_before, freezing_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "freezing"
  ) %>%
  mutate(
    time = ifelse(time == "freezing_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$freezing)

data_long$freezing <- factor(data_long$freezing, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(freezing ~ time , data = data_long)

summary(model)

# ------------------


# chute_instab Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, chute_instab,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, chute_instab))

unique(first_apo$chute_instab)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(chute_instab %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(chute_instab=ifelse(chute_instab==">=2","2",chute_instab)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(chute_instab=as.numeric(chute_instab))





# All 142 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_before"="chute_instab") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_after"="chute_instab") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_instab_before=max(chute_instab_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_instab_after=max(chute_instab_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_instab_before=ifelse(chute_instab_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_instab_after=ifelse(chute_instab_after>=1,1,0)) %>%
  group_by(tci_before, chute_instab_before, 
           tci_after, chute_instab_after) %>% count()


mean(Before_vs_after$chute_instab_before,na.rm=T) 
sd(Before_vs_after$chute_instab_before, na.rm=T)  

mean(Before_vs_after$chute_instab_after, na.rm=T)  
sd(Before_vs_after$chute_instab_after, na.rm=T)  

wilcox.test(Before_vs_after$chute_instab_before, Before_vs_after$chute_instab_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(chute_instab_before) %>% count() %>% mutate(n=n/142)
Before_vs_after %>% group_by(chute_instab_after) %>% count() %>% mutate(n=n/142)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_instab_before, chute_instab_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute_instab"
  ) %>%
  mutate(
    time = ifelse(time == "chute_instab_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute_instab)

data_long$chute_instab <- factor(data_long$chute_instab, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute_instab ~ time , data = data_long)

summary(model)




# All 117 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_before"="chute_instab") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_after"="chute_instab") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_instab_before=max(chute_instab_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_instab_after=max(chute_instab_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_instab_before=ifelse(chute_instab_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_instab_after=ifelse(chute_instab_after>=1,1,0)) %>%
  group_by(tci_before, chute_instab_before, 
           tci_after, chute_instab_after) %>% count()


mean(Before_vs_after$chute_instab_before,na.rm=T) 
sd(Before_vs_after$chute_instab_before, na.rm=T)  

mean(Before_vs_after$chute_instab_after, na.rm=T)  
sd(Before_vs_after$chute_instab_after, na.rm=T)  

wilcox.test(Before_vs_after$chute_instab_before, Before_vs_after$chute_instab_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(chute_instab_before) %>% count() %>% mutate(n=n/117)
Before_vs_after %>% group_by(chute_instab_after) %>% count() %>% mutate(n=n/117)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_instab_before, chute_instab_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute_instab"
  ) %>%
  mutate(
    time = ifelse(time == "chute_instab_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute_instab)

data_long$chute_instab <- factor(data_long$chute_instab, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute_instab ~ time , data = data_long)

summary(model)

# All 54 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_before"="chute_instab") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_after"="chute_instab") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_instab_before=max(chute_instab_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_instab_after=max(chute_instab_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_instab_before=ifelse(chute_instab_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_instab_after=ifelse(chute_instab_after>=1,1,0)) %>%
  group_by(tci_before, chute_instab_before, 
           tci_after, chute_instab_after) %>% count()

mean(Before_vs_after$chute_instab_before,na.rm=T) 
sd(Before_vs_after$chute_instab_before, na.rm=T)  

mean(Before_vs_after$chute_instab_after, na.rm=T)  
sd(Before_vs_after$chute_instab_after, na.rm=T)  

wilcox.test(Before_vs_after$chute_instab_before, Before_vs_after$chute_instab_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(chute_instab_before) %>% count() %>% mutate(n=n/54)
Before_vs_after %>% group_by(chute_instab_after) %>% count() %>% mutate(n=n/54)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_instab_before, chute_instab_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute_instab"
  ) %>%
  mutate(
    time = ifelse(time == "chute_instab_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute_instab)

data_long$chute_instab <- factor(data_long$chute_instab, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute_instab ~ time , data = data_long)

summary(model)


# All 34 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_before"="chute_instab") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute_instab, elapsed) %>% rename("chute_instab_after"="chute_instab") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_instab_before=max(chute_instab_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_instab_after=max(chute_instab_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_instab_before=ifelse(chute_instab_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_instab_after=ifelse(chute_instab_after>=1,1,0)) %>%
  group_by(tci_before, chute_instab_before, 
           tci_after, chute_instab_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_instab_before=ifelse(chute_instab_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_instab_after=ifelse(chute_instab_after>=1,1,0)) %>%
  group_by(tci_after, chute_instab_after) %>% count()




mean(Before_vs_after$chute_instab_before,na.rm=T) 
sd(Before_vs_after$chute_instab_before, na.rm=T)  

mean(Before_vs_after$chute_instab_after, na.rm=T)  
sd(Before_vs_after$chute_instab_after, na.rm=T)  

wilcox.test(Before_vs_after$chute_instab_before, Before_vs_after$chute_instab_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(chute_instab_before) %>% count() %>% mutate(n=n/34)
Before_vs_after %>% group_by(chute_instab_after) %>% count() %>% mutate(n=n/34)


cor(Before_vs_after$tci_before, Before_vs_after$chute_instab_before)
cor(Before_vs_after$tci_after, Before_vs_after$chute_instab_after)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_instab_before, chute_instab_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute_instab"
  ) %>%
  mutate(
    time = ifelse(time == "chute_instab_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute_instab)

data_long$chute_instab <- factor(data_long$chute_instab, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute_instab ~ time , data = data_long)

summary(model)

# ------------------


# deform_post Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, deform_post,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, deform_post))

unique(first_apo$deform_post)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(deform_post %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(deform_post=ifelse(deform_post==">=2","2",deform_post)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(deform_post=as.numeric(deform_post))





# All 136 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_before"="deform_post") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_after"="deform_post") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(deform_post_before=max(deform_post_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(deform_post_after=max(deform_post_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(deform_post_before=ifelse(deform_post_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(deform_post_after=ifelse(deform_post_after>=1,1,0)) %>%
  group_by(tci_before, deform_post_before, 
           tci_after, deform_post_after) %>% count()


mean(Before_vs_after$deform_post_before,na.rm=T) 
sd(Before_vs_after$deform_post_before, na.rm=T)  

mean(Before_vs_after$deform_post_after, na.rm=T)  
sd(Before_vs_after$deform_post_after, na.rm=T)  

wilcox.test(Before_vs_after$deform_post_before, Before_vs_after$deform_post_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(deform_post_before) %>% count() %>% mutate(n=n/136)
Before_vs_after %>% group_by(deform_post_after) %>% count() %>% mutate(n=n/136)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(deform_post_before, deform_post_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "deform"
  ) %>%
  mutate(
    time = ifelse(time == "deform_post_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$deform)

data_long$deform <- factor(data_long$deform, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(deform ~ time , data = data_long)

summary(model)



# All 117 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_before"="deform_post") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_after"="deform_post") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(deform_post_before=max(deform_post_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(deform_post_after=max(deform_post_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(deform_post_before=ifelse(deform_post_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(deform_post_after=ifelse(deform_post_after>=1,1,0)) %>%
  group_by(tci_before, deform_post_before, 
           tci_after, deform_post_after) %>% count()


mean(Before_vs_after$deform_post_before,na.rm=T) 
sd(Before_vs_after$deform_post_before, na.rm=T)  

mean(Before_vs_after$deform_post_after, na.rm=T)  
sd(Before_vs_after$deform_post_after, na.rm=T)  

wilcox.test(Before_vs_after$deform_post_before, Before_vs_after$deform_post_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(deform_post_before) %>% count() %>% mutate(n=n/117)
Before_vs_after %>% group_by(deform_post_after) %>% count() %>% mutate(n=n/117)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(deform_post_before, deform_post_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "deform"
  ) %>%
  mutate(
    time = ifelse(time == "deform_post_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$deform)

data_long$deform <- factor(data_long$deform, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(deform ~ time , data = data_long)

summary(model)


# All 54 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_before"="deform_post") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_after"="deform_post") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(deform_post_before=max(deform_post_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(deform_post_after=max(deform_post_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(deform_post_before=ifelse(deform_post_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(deform_post_after=ifelse(deform_post_after>=1,1,0)) %>%
  group_by(tci_before, deform_post_before, 
           tci_after, deform_post_after) %>% count()

mean(Before_vs_after$deform_post_before,na.rm=T) 
sd(Before_vs_after$deform_post_before, na.rm=T)  

mean(Before_vs_after$deform_post_after, na.rm=T)  
sd(Before_vs_after$deform_post_after, na.rm=T)  

wilcox.test(Before_vs_after$deform_post_before, Before_vs_after$deform_post_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(deform_post_before) %>% count() %>% mutate(n=n/51)
Before_vs_after %>% group_by(deform_post_after) %>% count() %>% mutate(n=n/51)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(deform_post_before, deform_post_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "deform"
  ) %>%
  mutate(
    time = ifelse(time == "deform_post_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$deform)

data_long$deform <- factor(data_long$deform, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(deform ~ time , data = data_long)

summary(model)



# All 31 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_before"="deform_post") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, deform_post, elapsed) %>% rename("deform_post_after"="deform_post") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(deform_post_before=max(deform_post_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(deform_post_after=max(deform_post_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(deform_post_before=ifelse(deform_post_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(deform_post_after=ifelse(deform_post_after>=1,1,0)) %>%
  group_by(tci_before, deform_post_before, 
           tci_after, deform_post_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(deform_post_before=ifelse(deform_post_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(deform_post_after=ifelse(deform_post_after>=1,1,0)) %>%
  group_by(tci_after, deform_post_after) %>% count()




mean(Before_vs_after$deform_post_before,na.rm=T) 
sd(Before_vs_after$deform_post_before, na.rm=T)  

mean(Before_vs_after$deform_post_after, na.rm=T)  
sd(Before_vs_after$deform_post_after, na.rm=T)  

wilcox.test(Before_vs_after$deform_post_before, Before_vs_after$deform_post_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(deform_post_before) %>% count() %>% mutate(n=n/31)
Before_vs_after %>% group_by(deform_post_after) %>% count() %>% mutate(n=n/31)


cor(Before_vs_after$tci_before, Before_vs_after$deform_post_before)
cor(Before_vs_after$tci_after, Before_vs_after$deform_post_after)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(deform_post_before, deform_post_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "deform"
  ) %>%
  mutate(
    time = ifelse(time == "deform_post_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$deform)

data_long$deform <- factor(data_long$deform, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(deform ~ time , data = data_long)

summary(model)





# ------------------


# tr_degl Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, tr_degl,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, tr_degl))

unique(first_apo$tr_degl)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(tr_degl %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tr_degl=ifelse(tr_degl==">=2","2",tr_degl)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(tr_degl=as.numeric(tr_degl))





# All 141 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_before"="tr_degl") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_after"="tr_degl") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_degl_before=max(tr_degl_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_degl_after=max(tr_degl_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_degl_before=ifelse(tr_degl_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_degl_after=ifelse(tr_degl_after>=1,1,0)) %>%
  group_by(tci_before, tr_degl_before, 
           tci_after, tr_degl_after) %>% count()


mean(Before_vs_after$tr_degl_before,na.rm=T) 
sd(Before_vs_after$tr_degl_before, na.rm=T)  

mean(Before_vs_after$tr_degl_after, na.rm=T)  
sd(Before_vs_after$tr_degl_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_degl_before, Before_vs_after$tr_degl_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_degl_before) %>% count() %>% mutate(n=n/141)
Before_vs_after %>% group_by(tr_degl_after) %>% count() %>% mutate(n=n/141)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_degl_before, tr_degl_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_degl"
  ) %>%
  mutate(
    time = ifelse(time == "tr_degl_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_degl)

data_long$tr_degl <- factor(data_long$tr_degl, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_degl ~ time , data = data_long)

summary(model)




# All 116 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_before"="tr_degl") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_after"="tr_degl") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_degl_before=max(tr_degl_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_degl_after=max(tr_degl_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_degl_before=ifelse(tr_degl_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_degl_after=ifelse(tr_degl_after>=1,1,0)) %>%
  group_by(tci_before, tr_degl_before, 
           tci_after, tr_degl_after) %>% count()


mean(Before_vs_after$tr_degl_before,na.rm=T) 
sd(Before_vs_after$tr_degl_before, na.rm=T)  

mean(Before_vs_after$tr_degl_after, na.rm=T)  
sd(Before_vs_after$tr_degl_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_degl_before, Before_vs_after$tr_degl_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_degl_before) %>% count() %>% mutate(n=n/116)
Before_vs_after %>% group_by(tr_degl_after) %>% count() %>% mutate(n=n/116)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_degl_before, tr_degl_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_degl"
  ) %>%
  mutate(
    time = ifelse(time == "tr_degl_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_degl)

data_long$tr_degl <- factor(data_long$tr_degl, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_degl ~ time , data = data_long)

summary(model)




# All 51 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_before"="tr_degl") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_after"="tr_degl") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_degl_before=max(tr_degl_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_degl_after=max(tr_degl_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_degl_before=ifelse(tr_degl_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_degl_after=ifelse(tr_degl_after>=1,1,0)) %>%
  group_by(tci_before, tr_degl_before, 
           tci_after, tr_degl_after) %>% count()

mean(Before_vs_after$tr_degl_before,na.rm=T) 
sd(Before_vs_after$tr_degl_before, na.rm=T)  

mean(Before_vs_after$tr_degl_after, na.rm=T)  
sd(Before_vs_after$tr_degl_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_degl_before, Before_vs_after$tr_degl_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_degl_before) %>% count() %>% mutate(n=n/51)
Before_vs_after %>% group_by(tr_degl_after) %>% count() %>% mutate(n=n/51)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_degl_before, tr_degl_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_degl"
  ) %>%
  mutate(
    time = ifelse(time == "tr_degl_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_degl)

data_long$tr_degl <- factor(data_long$tr_degl, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_degl ~ time , data = data_long)

summary(model)



# All 33 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_before"="tr_degl") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_degl, elapsed) %>% rename("tr_degl_after"="tr_degl") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_degl_before=max(tr_degl_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_degl_after=max(tr_degl_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_degl_before=ifelse(tr_degl_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_degl_after=ifelse(tr_degl_after>=1,1,0)) %>%
  group_by(tci_before, tr_degl_before, 
           tci_after, tr_degl_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_degl_before=ifelse(tr_degl_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_degl_after=ifelse(tr_degl_after>=1,1,0)) %>%
  group_by(tci_after, tr_degl_after) %>% count()




mean(Before_vs_after$tr_degl_before,na.rm=T) 
sd(Before_vs_after$tr_degl_before, na.rm=T)  

mean(Before_vs_after$tr_degl_after, na.rm=T)  
sd(Before_vs_after$tr_degl_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_degl_before, Before_vs_after$tr_degl_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_degl_before) %>% count() %>% mutate(n=n/33)
Before_vs_after %>% group_by(tr_degl_after) %>% count() %>% mutate(n=n/33)


cor(Before_vs_after$tci_before, Before_vs_after$tr_degl_before)
cor(Before_vs_after$tci_after, Before_vs_after$tr_degl_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_degl_before, tr_degl_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_degl"
  ) %>%
  mutate(
    time = ifelse(time == "tr_degl_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_degl)

data_long$tr_degl <- factor(data_long$tr_degl, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_degl ~ time , data = data_long)

summary(model)



# ------------------


# chute Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, chute,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, chute))

unique(first_apo$chute)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(chute %in% c("Non","Oui")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(chute=ifelse(chute=="Non",0,1))





# All 119 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute, elapsed) %>% rename("chute_before"="chute") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute, elapsed) %>% rename("chute_after"="chute") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_before=max(chute_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_after=max(chute_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(tci_before, chute_before, 
           tci_after, chute_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(chute_before, chute_after) %>% count()


data <- matrix(c(20, 8, 19, 72), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)

Before_vs_after %>% group_by(chute_before) %>% count() %>% mutate(n=n/114)
Before_vs_after %>% group_by(chute_after) %>% count() %>% mutate(n=n/114)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_before, chute_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute"
  ) %>%
  mutate(
    time = ifelse(time == "chute_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute)

data_long$chute <- factor(data_long$chute, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute ~ time , data = data_long)

summary(model)



# All 114 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute, elapsed) %>% rename("chute_before"="chute") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute, elapsed) %>% rename("chute_after"="chute") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_before=max(chute_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_after=max(chute_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(chute_before, chute_after) %>% count()


data <- matrix(c(19, 8, 19, 68), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)

Before_vs_after %>% group_by(chute_before) %>% count() %>% mutate(n=n/114)
Before_vs_after %>% group_by(chute_after) %>% count() %>% mutate(n=n/114)







data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_before, chute_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute"
  ) %>%
  mutate(
    time = ifelse(time == "chute_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute)

data_long$chute <- factor(data_long$chute, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute ~ time , data = data_long)

summary(model)




# All 51 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute, elapsed) %>% rename("chute_before"="chute") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute, elapsed) %>% rename("chute_after"="chute") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_before=max(chute_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_after=max(chute_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(chute_before, chute_after) %>% count()


data <- matrix(c(11, 5, 6, 29), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)

Before_vs_after %>% group_by(chute_before) %>% count() %>% mutate(n=n/51)
Before_vs_after %>% group_by(chute_after) %>% count() %>% mutate(n=n/51)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_before, chute_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute"
  ) %>%
  mutate(
    time = ifelse(time == "chute_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute)

data_long$chute <- factor(data_long$chute, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute ~ time , data = data_long)

summary(model)




# All 33 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, chute, elapsed) %>% rename("chute_before"="chute") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, chute, elapsed) %>% rename("chute_after"="chute") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(chute_before=max(chute_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(chute_after=max(chute_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(tci_before, chute_before, 
           tci_after, chute_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(chute_before=ifelse(chute_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(chute_after=ifelse(chute_after>=1,1,0)) %>%
  group_by(chute_before, chute_after) %>% count()


data <- matrix(c(8, 5, 3, 17), nrow = 2, byrow = TRUE,
               dimnames = list("Before" = c("Positive", "Negative"),
                               "After" = c("Positive", "Negative")))

mcnemar.test(data)

Before_vs_after %>% group_by(chute_before) %>% count() %>% mutate(n=n/33)
Before_vs_after %>% group_by(chute_after) %>% count() %>% mutate(n=n/33)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(chute_before, chute_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "chute"
  ) %>%
  mutate(
    time = ifelse(time == "chute_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$chute)

data_long$chute <- factor(data_long$chute, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(chute ~ time , data = data_long)

summary(model)


# ------------------


# ------------------


# digestif Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, digestif,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, digestif))

unique(first_apo$digestif)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(digestif %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(digestif=ifelse(digestif==">=2","2",digestif)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(digestif=as.numeric(digestif))





# All 138 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_before"="digestif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_after"="digestif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(digestif_before=max(digestif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(digestif_after=max(digestif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(digestif_before=ifelse(digestif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(digestif_after=ifelse(digestif_after>=1,1,0)) %>%
  group_by(tci_before, digestif_before, 
           tci_after, digestif_after) %>% count()


mean(Before_vs_after$digestif_before,na.rm=T) 
sd(Before_vs_after$digestif_before, na.rm=T)  

mean(Before_vs_after$digestif_after, na.rm=T)  
sd(Before_vs_after$digestif_after, na.rm=T)  

wilcox.test(Before_vs_after$digestif_before, Before_vs_after$digestif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(digestif_before) %>% count() %>% mutate(n=n/138)
Before_vs_after %>% group_by(digestif_after) %>% count() %>% mutate(n=n/138)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(digestif_before, digestif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "digestif"
  ) %>%
  mutate(
    time = ifelse(time == "digestif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$digestif)

data_long$digestif <- factor(data_long$digestif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(digestif ~ time , data = data_long)

summary(model)




# All 114 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_before"="digestif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_after"="digestif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(digestif_before=max(digestif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(digestif_after=max(digestif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(digestif_before=ifelse(digestif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(digestif_after=ifelse(digestif_after>=1,1,0)) %>%
  group_by(tci_before, digestif_before, 
           tci_after, digestif_after) %>% count()


mean(Before_vs_after$digestif_before,na.rm=T) 
sd(Before_vs_after$digestif_before, na.rm=T)  

mean(Before_vs_after$digestif_after, na.rm=T)  
sd(Before_vs_after$digestif_after, na.rm=T)  

wilcox.test(Before_vs_after$digestif_before, Before_vs_after$digestif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(digestif_before) %>% count() %>% mutate(n=n/114)
Before_vs_after %>% group_by(digestif_after) %>% count() %>% mutate(n=n/114)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(digestif_before, digestif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "digestif"
  ) %>%
  mutate(
    time = ifelse(time == "digestif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$digestif)

data_long$digestif <- factor(data_long$digestif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(digestif ~ time , data = data_long)

summary(model)


# All 54 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_before"="digestif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_after"="digestif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(digestif_before=max(digestif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(digestif_after=max(digestif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(digestif_before=ifelse(digestif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(digestif_after=ifelse(digestif_after>=1,1,0)) %>%
  group_by(tci_before, digestif_before, 
           tci_after, digestif_after) %>% count()

mean(Before_vs_after$digestif_before,na.rm=T) 
sd(Before_vs_after$digestif_before, na.rm=T)  

mean(Before_vs_after$digestif_after, na.rm=T)  
sd(Before_vs_after$digestif_after, na.rm=T)  

wilcox.test(Before_vs_after$digestif_before, Before_vs_after$digestif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(digestif_before) %>% count() %>% mutate(n=n/54)
Before_vs_after %>% group_by(digestif_after) %>% count() %>% mutate(n=n/54)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(digestif_before, digestif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "digestif"
  ) %>%
  mutate(
    time = ifelse(time == "digestif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$digestif)

data_long$digestif <- factor(data_long$digestif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(digestif ~ time , data = data_long)

summary(model)


# All 34 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_before"="digestif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, digestif, elapsed) %>% rename("digestif_after"="digestif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(digestif_before=max(digestif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(digestif_after=max(digestif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(digestif_before=ifelse(digestif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(digestif_after=ifelse(digestif_after>=1,1,0)) %>%
  group_by(tci_before, digestif_before, 
           tci_after, digestif_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(digestif_before=ifelse(digestif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(digestif_after=ifelse(digestif_after>=1,1,0)) %>%
  group_by(tci_after, digestif_after) %>% count()




mean(Before_vs_after$digestif_before,na.rm=T) 
sd(Before_vs_after$digestif_before, na.rm=T)  

mean(Before_vs_after$digestif_after, na.rm=T)  
sd(Before_vs_after$digestif_after, na.rm=T)  

wilcox.test(Before_vs_after$digestif_before, Before_vs_after$digestif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(digestif_before) %>% count() %>% mutate(n=n/34)
Before_vs_after %>% group_by(digestif_after) %>% count() %>% mutate(n=n/34)


cor(Before_vs_after$tci_before, Before_vs_after$digestif_before)
cor(Before_vs_after$tci_after, Before_vs_after$digestif_after)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(digestif_before, digestif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "digestif"
  ) %>%
  mutate(
    time = ifelse(time == "digestif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$digestif)

data_long$digestif <- factor(data_long$digestif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(digestif ~ time , data = data_long)

summary(model)



# ------------------


# urine Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, urine,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, urine))

unique(first_apo$urine)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(urine %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(urine=ifelse(urine==">=2","2",urine)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(urine=as.numeric(urine))





# All 139 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, urine, elapsed) %>% rename("urine_before"="urine") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, urine, elapsed) %>% rename("urine_after"="urine") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(urine_before=max(urine_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(urine_after=max(urine_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(urine_before=ifelse(urine_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(urine_after=ifelse(urine_after>=1,1,0)) %>%
  group_by(tci_before, urine_before, 
           tci_after, urine_after) %>% count()


mean(Before_vs_after$urine_before,na.rm=T) 
sd(Before_vs_after$urine_before, na.rm=T)  

mean(Before_vs_after$urine_after, na.rm=T)  
sd(Before_vs_after$urine_after, na.rm=T)  

wilcox.test(Before_vs_after$urine_before, Before_vs_after$urine_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(urine_before) %>% count() %>% mutate(n=n/139)
Before_vs_after %>% group_by(urine_after) %>% count() %>% mutate(n=n/139)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(urine_before, urine_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "urine"
  ) %>%
  mutate(
    time = ifelse(time == "urine_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$urine)

data_long$urine <- factor(data_long$urine, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(urine ~ time , data = data_long)

summary(model)





# All 114 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, urine, elapsed) %>% rename("urine_before"="urine") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, urine, elapsed) %>% rename("urine_after"="urine") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(urine_before=max(urine_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(urine_after=max(urine_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(urine_before=ifelse(urine_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(urine_after=ifelse(urine_after>=1,1,0)) %>%
  group_by(tci_before, urine_before, 
           tci_after, urine_after) %>% count()


mean(Before_vs_after$urine_before,na.rm=T) 
sd(Before_vs_after$urine_before, na.rm=T)  

mean(Before_vs_after$urine_after, na.rm=T)  
sd(Before_vs_after$urine_after, na.rm=T)  

wilcox.test(Before_vs_after$urine_before, Before_vs_after$urine_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(urine_before) %>% count() %>% mutate(n=n/114)
Before_vs_after %>% group_by(urine_after) %>% count() %>% mutate(n=n/114)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(urine_before, urine_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "urine"
  ) %>%
  mutate(
    time = ifelse(time == "urine_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$urine)

data_long$urine <- factor(data_long$urine, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(urine ~ time , data = data_long)

summary(model)


# All 53 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, urine, elapsed) %>% rename("urine_before"="urine") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, urine, elapsed) %>% rename("urine_after"="urine") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(urine_before=max(urine_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(urine_after=max(urine_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(urine_before=ifelse(urine_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(urine_after=ifelse(urine_after>=1,1,0)) %>%
  group_by(tci_before, urine_before, 
           tci_after, urine_after) %>% count()

mean(Before_vs_after$urine_before,na.rm=T) 
sd(Before_vs_after$urine_before, na.rm=T)  

mean(Before_vs_after$urine_after, na.rm=T)  
sd(Before_vs_after$urine_after, na.rm=T)  

wilcox.test(Before_vs_after$urine_before, Before_vs_after$urine_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(urine_before) %>% count() %>% mutate(n=n/53)
Before_vs_after %>% group_by(urine_after) %>% count() %>% mutate(n=n/53)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(urine_before, urine_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "urine"
  ) %>%
  mutate(
    time = ifelse(time == "urine_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$urine)

data_long$urine <- factor(data_long$urine, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(urine ~ time , data = data_long)

summary(model)



# All 34 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, urine, elapsed) %>% rename("urine_before"="urine") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, urine, elapsed) %>% rename("urine_after"="urine") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(urine_before=max(urine_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(urine_after=max(urine_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(urine_before=ifelse(urine_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(urine_after=ifelse(urine_after>=1,1,0)) %>%
  group_by(tci_before, urine_before, 
           tci_after, urine_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(urine_before=ifelse(urine_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(urine_after=ifelse(urine_after>=1,1,0)) %>%
  group_by(tci_after, urine_after) %>% count()




mean(Before_vs_after$urine_before,na.rm=T) 
sd(Before_vs_after$urine_before, na.rm=T)  

mean(Before_vs_after$urine_after, na.rm=T)  
sd(Before_vs_after$urine_after, na.rm=T)  

wilcox.test(Before_vs_after$urine_before, Before_vs_after$urine_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(urine_before) %>% count() %>% mutate(n=n/34)
Before_vs_after %>% group_by(urine_after) %>% count() %>% mutate(n=n/34)


cor(Before_vs_after$tci_before, Before_vs_after$urine_before)
cor(Before_vs_after$tci_after, Before_vs_after$urine_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(urine_before, urine_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "urine"
  ) %>%
  mutate(
    time = ifelse(time == "urine_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$urine)

data_long$urine <- factor(data_long$urine, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(urine ~ time , data = data_long)

summary(model)


# -------------
# hypotension Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342


names(Consultation_20241028)
Consultation_20241028$hypotension

Consultation_20241028

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hypotension,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, hypotension))

unique(first_apo$hypotension)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(hypotension %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(hypotension=ifelse(hypotension==">=2","2",hypotension)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(hypotension=as.numeric(hypotension))





# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_before"="hypotension") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_after"="hypotension") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(hypotension_before=max(hypotension_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(hypotension_after=max(hypotension_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(hypotension_before=ifelse(hypotension_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(hypotension_after=ifelse(hypotension_after>=1,1,0)) %>%
  group_by(tci_before, hypotension_before, 
           tci_after, hypotension_after) %>% count()


mean(Before_vs_after$hypotension_before,na.rm=T) 
sd(Before_vs_after$hypotension_before, na.rm=T)  

mean(Before_vs_after$hypotension_after, na.rm=T)  
sd(Before_vs_after$hypotension_after, na.rm=T)  

wilcox.test(Before_vs_after$hypotension_before, Before_vs_after$hypotension_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(hypotension_before) %>% count() %>% mutate(n=n/145)
Before_vs_after %>% group_by(hypotension_after) %>% count() %>% mutate(n=n/145)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(hypotension_before, hypotension_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "hypotension"
  ) %>%
  mutate(
    time = ifelse(time == "hypotension_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$hypotension)

data_long$hypotension <- factor(data_long$hypotension, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(hypotension ~ time , data = data_long)

summary(model)



# All 120 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_before"="hypotension") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_after"="hypotension") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(hypotension_before=max(hypotension_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(hypotension_after=max(hypotension_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(hypotension_before=ifelse(hypotension_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(hypotension_after=ifelse(hypotension_after>=1,1,0)) %>%
  group_by(tci_before, hypotension_before, 
           tci_after, hypotension_after) %>% count()


mean(Before_vs_after$hypotension_before,na.rm=T) 
sd(Before_vs_after$hypotension_before, na.rm=T)  

mean(Before_vs_after$hypotension_after, na.rm=T)  
sd(Before_vs_after$hypotension_after, na.rm=T)  

wilcox.test(Before_vs_after$hypotension_before, Before_vs_after$hypotension_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(hypotension_before) %>% count() %>% mutate(n=n/120)
Before_vs_after %>% group_by(hypotension_after) %>% count() %>% mutate(n=n/120)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(hypotension_before, hypotension_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "hypotension"
  ) %>%
  mutate(
    time = ifelse(time == "hypotension_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$hypotension)

data_long$hypotension <- factor(data_long$hypotension, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(hypotension ~ time , data = data_long)

summary(model)



# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_before"="hypotension") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_after"="hypotension") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(hypotension_before=max(hypotension_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(hypotension_after=max(hypotension_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(hypotension_before=ifelse(hypotension_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(hypotension_after=ifelse(hypotension_after>=1,1,0)) %>%
  group_by(tci_before, hypotension_before, 
           tci_after, hypotension_after) %>% count()

mean(Before_vs_after$hypotension_before,na.rm=T) 
sd(Before_vs_after$hypotension_before, na.rm=T)  

mean(Before_vs_after$hypotension_after, na.rm=T)  
sd(Before_vs_after$hypotension_after, na.rm=T)  

wilcox.test(Before_vs_after$hypotension_before, Before_vs_after$hypotension_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(hypotension_before) %>% count() %>% mutate(n=n/57)
Before_vs_after %>% group_by(hypotension_after) %>% count() %>% mutate(n=n/57)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(hypotension_before, hypotension_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "hypotension"
  ) %>%
  mutate(
    time = ifelse(time == "hypotension_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$hypotension)

data_long$hypotension <- factor(data_long$hypotension, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(hypotension ~ time , data = data_long)

summary(model)




# All 35 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_before"="hypotension") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, hypotension, elapsed) %>% rename("hypotension_after"="hypotension") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(hypotension_before=max(hypotension_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(hypotension_after=max(hypotension_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(hypotension_before=ifelse(hypotension_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(hypotension_after=ifelse(hypotension_after>=1,1,0)) %>%
  group_by(tci_before, hypotension_before, 
           tci_after, hypotension_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(hypotension_before=ifelse(hypotension_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(hypotension_after=ifelse(hypotension_after>=1,1,0)) %>%
  group_by(tci_after, hypotension_after) %>% count()




mean(Before_vs_after$hypotension_before,na.rm=T) 
sd(Before_vs_after$hypotension_before, na.rm=T)  

mean(Before_vs_after$hypotension_after, na.rm=T)  
sd(Before_vs_after$hypotension_after, na.rm=T)  

wilcox.test(Before_vs_after$hypotension_before, Before_vs_after$hypotension_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(hypotension_before) %>% count() %>% mutate(n=n/35)
Before_vs_after %>% group_by(hypotension_after) %>% count() %>% mutate(n=n/35)


cor(Before_vs_after$tci_before, Before_vs_after$hypotension_before)
cor(Before_vs_after$tci_after, Before_vs_after$hypotension_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(hypotension_before, hypotension_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "hypotension"
  ) %>%
  mutate(
    time = ifelse(time == "hypotension_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$hypotension)

data_long$hypotension <- factor(data_long$hypotension, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(hypotension ~ time , data = data_long)

summary(model)



# ------------------



# ------------------


# apathie Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, apathie,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, apathie))

unique(first_apo$apathie)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(apathie %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(apathie=ifelse(apathie==">=2","2",apathie)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(apathie=as.numeric(apathie))





# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_before"="apathie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_after"="apathie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(apathie_before=max(apathie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(apathie_after=max(apathie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(apathie_before=ifelse(apathie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(apathie_after=ifelse(apathie_after>=1,1,0)) %>%
  group_by(tci_before, apathie_before, 
           tci_after, apathie_after) %>% count()


mean(Before_vs_after$apathie_before,na.rm=T) 
sd(Before_vs_after$apathie_before, na.rm=T)  

mean(Before_vs_after$apathie_after, na.rm=T)  
sd(Before_vs_after$apathie_after, na.rm=T)  

wilcox.test(Before_vs_after$apathie_before, Before_vs_after$apathie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(apathie_before) %>% count() %>% mutate(n=n/145)
Before_vs_after %>% group_by(apathie_after) %>% count() %>% mutate(n=n/145)







data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(apathie_before, apathie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "apathie"
  ) %>%
  mutate(
    time = ifelse(time == "apathie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$apathie)

data_long$apathie <- factor(data_long$apathie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(apathie ~ time , data = data_long)

summary(model)



# All 120 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_before"="apathie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_after"="apathie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(apathie_before=max(apathie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(apathie_after=max(apathie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(apathie_before=ifelse(apathie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(apathie_after=ifelse(apathie_after>=1,1,0)) %>%
  group_by(tci_before, apathie_before, 
           tci_after, apathie_after) %>% count()


mean(Before_vs_after$apathie_before,na.rm=T) 
sd(Before_vs_after$apathie_before, na.rm=T)  

mean(Before_vs_after$apathie_after, na.rm=T)  
sd(Before_vs_after$apathie_after, na.rm=T)  

wilcox.test(Before_vs_after$apathie_before, Before_vs_after$apathie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(apathie_before) %>% count() %>% mutate(n=n/120)
Before_vs_after %>% group_by(apathie_after) %>% count() %>% mutate(n=n/120)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(apathie_before, apathie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "apathie"
  ) %>%
  mutate(
    time = ifelse(time == "apathie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$apathie)

data_long$apathie <- factor(data_long$apathie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(apathie ~ time , data = data_long)

summary(model)



# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_before"="apathie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_after"="apathie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(apathie_before=max(apathie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(apathie_after=max(apathie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(apathie_before=ifelse(apathie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(apathie_after=ifelse(apathie_after>=1,1,0)) %>%
  group_by(tci_before, apathie_before, 
           tci_after, apathie_after) %>% count()

mean(Before_vs_after$apathie_before,na.rm=T) 
sd(Before_vs_after$apathie_before, na.rm=T)  

mean(Before_vs_after$apathie_after, na.rm=T)  
sd(Before_vs_after$apathie_after, na.rm=T)  

wilcox.test(Before_vs_after$apathie_before, Before_vs_after$apathie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(apathie_before) %>% count() %>% mutate(n=n/57)
Before_vs_after %>% group_by(apathie_after) %>% count() %>% mutate(n=n/57)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(apathie_before, apathie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "apathie"
  ) %>%
  mutate(
    time = ifelse(time == "apathie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$apathie)

data_long$apathie <- factor(data_long$apathie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(apathie ~ time , data = data_long)

summary(model)



# All 35 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_before"="apathie") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, apathie, elapsed) %>% rename("apathie_after"="apathie") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(apathie_before=max(apathie_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(apathie_after=max(apathie_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(apathie_before=ifelse(apathie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(apathie_after=ifelse(apathie_after>=1,1,0)) %>%
  group_by(tci_before, apathie_before, 
           tci_after, apathie_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(apathie_before=ifelse(apathie_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(apathie_after=ifelse(apathie_after>=1,1,0)) %>%
  group_by(tci_after, apathie_after) %>% count()




mean(Before_vs_after$apathie_before,na.rm=T) 
sd(Before_vs_after$apathie_before, na.rm=T)  

mean(Before_vs_after$apathie_after, na.rm=T)  
sd(Before_vs_after$apathie_after, na.rm=T)  

wilcox.test(Before_vs_after$apathie_before, Before_vs_after$apathie_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(apathie_before) %>% count() %>% mutate(n=n/35)
Before_vs_after %>% group_by(apathie_after) %>% count() %>% mutate(n=n/35)


cor(Before_vs_after$tci_before, Before_vs_after$apathie_before)
cor(Before_vs_after$tci_after, Before_vs_after$apathie_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(apathie_before, apathie_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "apathie"
  ) %>%
  mutate(
    time = ifelse(time == "apathie_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$apathie)

data_long$apathie <- factor(data_long$apathie, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(apathie ~ time , data = data_long)

summary(model)




# ------------------


# depression Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, depression,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, depression))

unique(first_apo$depression)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(depression %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(depression=ifelse(depression==">=2","2",depression)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(depression=as.numeric(depression))






# All 146 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, depression, elapsed) %>% rename("depression_before"="depression") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, depression, elapsed) %>% rename("depression_after"="depression") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(depression_before=max(depression_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(depression_after=max(depression_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(depression_before=ifelse(depression_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(depression_after=ifelse(depression_after>=1,1,0)) %>%
  group_by(tci_before, depression_before, 
           tci_after, depression_after) %>% count()


mean(Before_vs_after$depression_before,na.rm=T) 
sd(Before_vs_after$depression_before, na.rm=T)  

mean(Before_vs_after$depression_after, na.rm=T)  
sd(Before_vs_after$depression_after, na.rm=T)  

wilcox.test(Before_vs_after$depression_before, Before_vs_after$depression_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(depression_before) %>% count() %>% mutate(n=n/146)
Before_vs_after %>% group_by(depression_after) %>% count() %>% mutate(n=n/146)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(depression_before, depression_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "depression"
  ) %>%
  mutate(
    time = ifelse(time == "depression_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$depression)

data_long$depression <- factor(data_long$depression, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(depression ~ time , data = data_long)

summary(model)




# All 121 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, depression, elapsed) %>% rename("depression_before"="depression") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, depression, elapsed) %>% rename("depression_after"="depression") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(depression_before=max(depression_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(depression_after=max(depression_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(depression_before=ifelse(depression_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(depression_after=ifelse(depression_after>=1,1,0)) %>%
  group_by(tci_before, depression_before, 
           tci_after, depression_after) %>% count()


mean(Before_vs_after$depression_before,na.rm=T) 
sd(Before_vs_after$depression_before, na.rm=T)  

mean(Before_vs_after$depression_after, na.rm=T)  
sd(Before_vs_after$depression_after, na.rm=T)  

wilcox.test(Before_vs_after$depression_before, Before_vs_after$depression_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(depression_before) %>% count() %>% mutate(n=n/121)
Before_vs_after %>% group_by(depression_after) %>% count() %>% mutate(n=n/121)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(depression_before, depression_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "depression"
  ) %>%
  mutate(
    time = ifelse(time == "depression_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$depression)

data_long$depression <- factor(data_long$depression, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(depression ~ time , data = data_long)

summary(model)




# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, depression, elapsed) %>% rename("depression_before"="depression") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, depression, elapsed) %>% rename("depression_after"="depression") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(depression_before=max(depression_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(depression_after=max(depression_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(depression_before=ifelse(depression_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(depression_after=ifelse(depression_after>=1,1,0)) %>%
  group_by(tci_before, depression_before, 
           tci_after, depression_after) %>% count()

mean(Before_vs_after$depression_before,na.rm=T) 
sd(Before_vs_after$depression_before, na.rm=T)  

mean(Before_vs_after$depression_after, na.rm=T)  
sd(Before_vs_after$depression_after, na.rm=T)  

wilcox.test(Before_vs_after$depression_before, Before_vs_after$depression_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(depression_before) %>% count() %>% mutate(n=n/57)
Before_vs_after %>% group_by(depression_after) %>% count() %>% mutate(n=n/57)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(depression_before, depression_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "depression"
  ) %>%
  mutate(
    time = ifelse(time == "depression_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$depression)

data_long$depression <- factor(data_long$depression, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(depression ~ time , data = data_long)

summary(model)



# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, depression, elapsed) %>% rename("depression_before"="depression") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, depression, elapsed) %>% rename("depression_after"="depression") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(depression_before=max(depression_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(depression_after=max(depression_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(depression_before=ifelse(depression_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(depression_after=ifelse(depression_after>=1,1,0)) %>%
  group_by(tci_before, depression_before, 
           tci_after, depression_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(depression_before=ifelse(depression_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(depression_after=ifelse(depression_after>=1,1,0)) %>%
  group_by(tci_after, depression_after) %>% count()




mean(Before_vs_after$depression_before,na.rm=T) 
sd(Before_vs_after$depression_before, na.rm=T)  

mean(Before_vs_after$depression_after, na.rm=T)  
sd(Before_vs_after$depression_after, na.rm=T)  

wilcox.test(Before_vs_after$depression_before, Before_vs_after$depression_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(depression_before) %>% count() %>% mutate(n=n/36)
Before_vs_after %>% group_by(depression_after) %>% count() %>% mutate(n=n/36)


cor(Before_vs_after$tci_before, Before_vs_after$depression_before)
cor(Before_vs_after$tci_after, Before_vs_after$depression_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(depression_before, depression_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "depression"
  ) %>%
  mutate(
    time = ifelse(time == "depression_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$depression)

data_long$depression <- factor(data_long$depression, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(depression ~ time , data = data_long)

summary(model)




# ------------------


# anxiete Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, anxiete,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, anxiete))

unique(first_apo$anxiete)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(anxiete %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(anxiete=ifelse(anxiete==">=2","2",anxiete)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(anxiete=as.numeric(anxiete))





# All 147 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_before"="anxiete") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_after"="anxiete") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(anxiete_before=max(anxiete_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(anxiete_after=max(anxiete_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(anxiete_before=ifelse(anxiete_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(anxiete_after=ifelse(anxiete_after>=1,1,0)) %>%
  group_by(tci_before, anxiete_before, 
           tci_after, anxiete_after) %>% count()


mean(Before_vs_after$anxiete_before,na.rm=T) 
sd(Before_vs_after$anxiete_before, na.rm=T)  

mean(Before_vs_after$anxiete_after, na.rm=T)  
sd(Before_vs_after$anxiete_after, na.rm=T)  

wilcox.test(Before_vs_after$anxiete_before, Before_vs_after$anxiete_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(anxiete_before) %>% count() %>% mutate(n=n/147)
Before_vs_after %>% group_by(anxiete_after) %>% count() %>% mutate(n=n/147)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(anxiete_before, anxiete_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "anxiete"
  ) %>%
  mutate(
    time = ifelse(time == "anxiete_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$anxiete)

data_long$anxiete <- factor(data_long$anxiete, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(anxiete ~ time , data = data_long)

summary(model)



# All 122 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_before"="anxiete") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_after"="anxiete") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(anxiete_before=max(anxiete_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(anxiete_after=max(anxiete_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(anxiete_before=ifelse(anxiete_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(anxiete_after=ifelse(anxiete_after>=1,1,0)) %>%
  group_by(tci_before, anxiete_before, 
           tci_after, anxiete_after) %>% count()


mean(Before_vs_after$anxiete_before,na.rm=T) 
sd(Before_vs_after$anxiete_before, na.rm=T)  

mean(Before_vs_after$anxiete_after, na.rm=T)  
sd(Before_vs_after$anxiete_after, na.rm=T)  

wilcox.test(Before_vs_after$anxiete_before, Before_vs_after$anxiete_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(anxiete_before) %>% count() %>% mutate(n=n/122)
Before_vs_after %>% group_by(anxiete_after) %>% count() %>% mutate(n=n/122)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(anxiete_before, anxiete_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "anxiete"
  ) %>%
  mutate(
    time = ifelse(time == "anxiete_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$anxiete)

data_long$anxiete <- factor(data_long$anxiete, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(anxiete ~ time , data = data_long)

summary(model)



# All 58 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_before"="anxiete") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_after"="anxiete") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(anxiete_before=max(anxiete_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(anxiete_after=max(anxiete_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(anxiete_before=ifelse(anxiete_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(anxiete_after=ifelse(anxiete_after>=1,1,0)) %>%
  group_by(tci_before, anxiete_before, 
           tci_after, anxiete_after) %>% count()

mean(Before_vs_after$anxiete_before,na.rm=T) 
sd(Before_vs_after$anxiete_before, na.rm=T)  

mean(Before_vs_after$anxiete_after, na.rm=T)  
sd(Before_vs_after$anxiete_after, na.rm=T)  

wilcox.test(Before_vs_after$anxiete_before, Before_vs_after$anxiete_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(anxiete_before) %>% count() %>% mutate(n=n/58)
Before_vs_after %>% group_by(anxiete_after) %>% count() %>% mutate(n=n/58)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(anxiete_before, anxiete_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "anxiete"
  ) %>%
  mutate(
    time = ifelse(time == "anxiete_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$anxiete)

data_long$anxiete <- factor(data_long$anxiete, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(anxiete ~ time , data = data_long)

summary(model)



# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_before"="anxiete") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, anxiete, elapsed) %>% rename("anxiete_after"="anxiete") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(anxiete_before=max(anxiete_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(anxiete_after=max(anxiete_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(anxiete_before=ifelse(anxiete_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(anxiete_after=ifelse(anxiete_after>=1,1,0)) %>%
  group_by(tci_before, anxiete_before, 
           tci_after, anxiete_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(anxiete_before=ifelse(anxiete_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(anxiete_after=ifelse(anxiete_after>=1,1,0)) %>%
  group_by(tci_after, anxiete_after) %>% count()




mean(Before_vs_after$anxiete_before,na.rm=T) 
sd(Before_vs_after$anxiete_before, na.rm=T)  

mean(Before_vs_after$anxiete_after, na.rm=T)  
sd(Before_vs_after$anxiete_after, na.rm=T)  

wilcox.test(Before_vs_after$anxiete_before, Before_vs_after$anxiete_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(anxiete_before) %>% count() %>% mutate(n=n/36)
Before_vs_after %>% group_by(anxiete_after) %>% count() %>% mutate(n=n/36)


cor(Before_vs_after$tci_before, Before_vs_after$anxiete_before)
cor(Before_vs_after$tci_after, Before_vs_after$anxiete_after)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(anxiete_before, anxiete_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "anxiete"
  ) %>%
  mutate(
    time = ifelse(time == "anxiete_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$anxiete)

data_long$anxiete <- factor(data_long$anxiete, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(anxiete ~ time , data = data_long)

summary(model)



# ------------------


# halluc_psy Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, halluc_psy,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, halluc_psy))

unique(first_apo$halluc_psy)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(halluc_psy %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(halluc_psy=ifelse(halluc_psy==">=2","2",halluc_psy)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(halluc_psy=as.numeric(halluc_psy))





# All 143 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_before"="halluc_psy") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_after"="halluc_psy") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(halluc_psy_before=max(halluc_psy_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(halluc_psy_after=max(halluc_psy_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(halluc_psy_before=ifelse(halluc_psy_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(halluc_psy_after=ifelse(halluc_psy_after>=1,1,0)) %>%
  group_by(tci_before, halluc_psy_before, 
           tci_after, halluc_psy_after) %>% count()


mean(Before_vs_after$halluc_psy_before,na.rm=T) 
sd(Before_vs_after$halluc_psy_before, na.rm=T)  

mean(Before_vs_after$halluc_psy_after, na.rm=T)  
sd(Before_vs_after$halluc_psy_after, na.rm=T)  

wilcox.test(Before_vs_after$halluc_psy_before, Before_vs_after$halluc_psy_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(halluc_psy_before) %>% count() %>% mutate(n=n/143)
Before_vs_after %>% group_by(halluc_psy_after) %>% count() %>% mutate(n=n/143)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(halluc_psy_before, halluc_psy_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "halluc_psy"
  ) %>%
  mutate(
    time = ifelse(time == "halluc_psy_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$halluc_psy)

data_long$halluc_psy <- factor(data_long$halluc_psy, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(halluc_psy ~ time , data = data_long)

summary(model)



# All 119 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_before"="halluc_psy") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_after"="halluc_psy") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(halluc_psy_before=max(halluc_psy_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(halluc_psy_after=max(halluc_psy_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(halluc_psy_before=ifelse(halluc_psy_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(halluc_psy_after=ifelse(halluc_psy_after>=1,1,0)) %>%
  group_by(tci_before, halluc_psy_before, 
           tci_after, halluc_psy_after) %>% count()


mean(Before_vs_after$halluc_psy_before,na.rm=T) 
sd(Before_vs_after$halluc_psy_before, na.rm=T)  

mean(Before_vs_after$halluc_psy_after, na.rm=T)  
sd(Before_vs_after$halluc_psy_after, na.rm=T)  

wilcox.test(Before_vs_after$halluc_psy_before, Before_vs_after$halluc_psy_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(halluc_psy_before) %>% count() %>% mutate(n=n/119)
Before_vs_after %>% group_by(halluc_psy_after) %>% count() %>% mutate(n=n/119)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(halluc_psy_before, halluc_psy_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "halluc_psy"
  ) %>%
  mutate(
    time = ifelse(time == "halluc_psy_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$halluc_psy)

data_long$halluc_psy <- factor(data_long$halluc_psy, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(halluc_psy ~ time , data = data_long)

summary(model)




# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_before"="halluc_psy") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_after"="halluc_psy") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(halluc_psy_before=max(halluc_psy_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(halluc_psy_after=max(halluc_psy_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(halluc_psy_before=ifelse(halluc_psy_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(halluc_psy_after=ifelse(halluc_psy_after>=1,1,0)) %>%
  group_by(tci_before, halluc_psy_before, 
           tci_after, halluc_psy_after) %>% count()

mean(Before_vs_after$halluc_psy_before,na.rm=T) 
sd(Before_vs_after$halluc_psy_before, na.rm=T)  

mean(Before_vs_after$halluc_psy_after, na.rm=T)  
sd(Before_vs_after$halluc_psy_after, na.rm=T)  

wilcox.test(Before_vs_after$halluc_psy_before, Before_vs_after$halluc_psy_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(halluc_psy_before) %>% count() %>% mutate(n=n/57)
Before_vs_after %>% group_by(halluc_psy_after) %>% count() %>% mutate(n=n/57)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(halluc_psy_before, halluc_psy_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "halluc_psy"
  ) %>%
  mutate(
    time = ifelse(time == "halluc_psy_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$halluc_psy)

data_long$halluc_psy <- factor(data_long$halluc_psy, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(halluc_psy ~ time , data = data_long)

summary(model)



# All 35 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_before"="halluc_psy") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, halluc_psy, elapsed) %>% rename("halluc_psy_after"="halluc_psy") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(halluc_psy_before=max(halluc_psy_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(halluc_psy_after=max(halluc_psy_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(halluc_psy_before=ifelse(halluc_psy_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(halluc_psy_after=ifelse(halluc_psy_after>=1,1,0)) %>%
  group_by(tci_before, halluc_psy_before, 
           tci_after, halluc_psy_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(halluc_psy_before=ifelse(halluc_psy_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(halluc_psy_after=ifelse(halluc_psy_after>=1,1,0)) %>%
  group_by(tci_after, halluc_psy_after) %>% count()




mean(Before_vs_after$halluc_psy_before,na.rm=T) 
sd(Before_vs_after$halluc_psy_before, na.rm=T)  

mean(Before_vs_after$halluc_psy_after, na.rm=T)  
sd(Before_vs_after$halluc_psy_after, na.rm=T)  

wilcox.test(Before_vs_after$halluc_psy_before, Before_vs_after$halluc_psy_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(halluc_psy_before) %>% count() %>% mutate(n=n/35)
Before_vs_after %>% group_by(halluc_psy_after) %>% count() %>% mutate(n=n/35)


cor(Before_vs_after$tci_before, Before_vs_after$halluc_psy_before)
cor(Before_vs_after$tci_after, Before_vs_after$halluc_psy_after)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(halluc_psy_before, halluc_psy_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "halluc_psy"
  ) %>%
  mutate(
    time = ifelse(time == "halluc_psy_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$halluc_psy)

data_long$halluc_psy <- factor(data_long$halluc_psy, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(halluc_psy ~ time , data = data_long)

summary(model)




# ------------------


# somnolence Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, somnolence,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, somnolence))

unique(first_apo$somnolence)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(somnolence %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(somnolence=ifelse(somnolence==">=2","2",somnolence)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(somnolence=as.numeric(somnolence))





# All 144 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_before"="somnolence") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_after"="somnolence") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(somnolence_before=max(somnolence_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(somnolence_after=max(somnolence_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(somnolence_before=ifelse(somnolence_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(somnolence_after=ifelse(somnolence_after>=1,1,0)) %>%
  group_by(tci_before, somnolence_before, 
           tci_after, somnolence_after) %>% count()


mean(Before_vs_after$somnolence_before,na.rm=T) 
sd(Before_vs_after$somnolence_before, na.rm=T)  

mean(Before_vs_after$somnolence_after, na.rm=T)  
sd(Before_vs_after$somnolence_after, na.rm=T)  

wilcox.test(Before_vs_after$somnolence_before, Before_vs_after$somnolence_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(somnolence_before) %>% count() %>% mutate(n=n/144)
Before_vs_after %>% group_by(somnolence_after) %>% count() %>% mutate(n=n/144)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(somnolence_before, somnolence_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "somnolence"
  ) %>%
  mutate(
    time = ifelse(time == "somnolence_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$somnolence)

data_long$somnolence <- factor(data_long$somnolence, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(somnolence ~ time , data = data_long)

summary(model)






# All 119 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_before"="somnolence") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_after"="somnolence") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(somnolence_before=max(somnolence_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(somnolence_after=max(somnolence_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(somnolence_before=ifelse(somnolence_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(somnolence_after=ifelse(somnolence_after>=1,1,0)) %>%
  group_by(tci_before, somnolence_before, 
           tci_after, somnolence_after) %>% count()


mean(Before_vs_after$somnolence_before,na.rm=T) 
sd(Before_vs_after$somnolence_before, na.rm=T)  

mean(Before_vs_after$somnolence_after, na.rm=T)  
sd(Before_vs_after$somnolence_after, na.rm=T)  

wilcox.test(Before_vs_after$somnolence_before, Before_vs_after$somnolence_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(somnolence_before) %>% count() %>% mutate(n=n/119)
Before_vs_after %>% group_by(somnolence_after) %>% count() %>% mutate(n=n/119)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(somnolence_before, somnolence_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "somnolence"
  ) %>%
  mutate(
    time = ifelse(time == "somnolence_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$somnolence)

data_long$somnolence <- factor(data_long$somnolence, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(somnolence ~ time , data = data_long)

summary(model)





# All 56 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_before"="somnolence") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_after"="somnolence") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(somnolence_before=max(somnolence_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(somnolence_after=max(somnolence_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(somnolence_before=ifelse(somnolence_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(somnolence_after=ifelse(somnolence_after>=1,1,0)) %>%
  group_by(tci_before, somnolence_before, 
           tci_after, somnolence_after) %>% count()

mean(Before_vs_after$somnolence_before,na.rm=T) 
sd(Before_vs_after$somnolence_before, na.rm=T)  

mean(Before_vs_after$somnolence_after, na.rm=T)  
sd(Before_vs_after$somnolence_after, na.rm=T)  

wilcox.test(Before_vs_after$somnolence_before, Before_vs_after$somnolence_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(somnolence_before) %>% count() %>% mutate(n=n/56)
Before_vs_after %>% group_by(somnolence_after) %>% count() %>% mutate(n=n/56)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(somnolence_before, somnolence_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "somnolence"
  ) %>%
  mutate(
    time = ifelse(time == "somnolence_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$somnolence)

data_long$somnolence <- factor(data_long$somnolence, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(somnolence ~ time , data = data_long)

summary(model)





# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_before"="somnolence") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, somnolence, elapsed) %>% rename("somnolence_after"="somnolence") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(somnolence_before=max(somnolence_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(somnolence_after=max(somnolence_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(somnolence_before=ifelse(somnolence_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(somnolence_after=ifelse(somnolence_after>=1,1,0)) %>%
  group_by(tci_before, somnolence_before, 
           tci_after, somnolence_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(somnolence_before=ifelse(somnolence_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(somnolence_after=ifelse(somnolence_after>=1,1,0)) %>%
  group_by(tci_after, somnolence_after) %>% count()




mean(Before_vs_after$somnolence_before,na.rm=T) 
sd(Before_vs_after$somnolence_before, na.rm=T)  

mean(Before_vs_after$somnolence_after, na.rm=T)  
sd(Before_vs_after$somnolence_after, na.rm=T)  

wilcox.test(Before_vs_after$somnolence_before, Before_vs_after$somnolence_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(somnolence_before) %>% count() %>% mutate(n=n/36)
Before_vs_after %>% group_by(somnolence_after) %>% count() %>% mutate(n=n/36)


cor(Before_vs_after$tci_before, Before_vs_after$somnolence_before)
cor(Before_vs_after$tci_after, Before_vs_after$somnolence_after)



data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(somnolence_before, somnolence_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "somnolence"
  ) %>%
  mutate(
    time = ifelse(time == "somnolence_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$somnolence)

data_long$somnolence <- factor(data_long$somnolence, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(somnolence ~ time , data = data_long)

summary(model)


# ------------------


# tr_cognitif Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

names(Consultation_20241028)

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, tr_cognitif,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, tr_cognitif))

unique(first_apo$tr_cognitif)
unique(first_apo$tci)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  filter(tr_cognitif %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tr_cognitif=ifelse(tr_cognitif==">=2","2",tr_cognitif)) %>%
  mutate(tci=as.numeric(tci)) %>%
  mutate(tr_cognitif=as.numeric(tr_cognitif))





# All 144 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_before"="tr_cognitif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_after"="tr_cognitif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_cognitif_before=max(tr_cognitif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_cognitif_after=max(tr_cognitif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_cognitif_before=ifelse(tr_cognitif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_cognitif_after=ifelse(tr_cognitif_after>=1,1,0)) %>%
  group_by(tci_before, tr_cognitif_before, 
           tci_after, tr_cognitif_after) %>% count()


mean(Before_vs_after$tr_cognitif_before,na.rm=T) 
sd(Before_vs_after$tr_cognitif_before, na.rm=T)  

mean(Before_vs_after$tr_cognitif_after, na.rm=T)  
sd(Before_vs_after$tr_cognitif_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_cognitif_before, Before_vs_after$tr_cognitif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_cognitif_before) %>% count() %>% mutate(n=n/137)
Before_vs_after %>% group_by(tr_cognitif_after) %>% count() %>% mutate(n=n/137)




data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_cognitif_before, tr_cognitif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_cognitif"
  ) %>%
  mutate(
    time = ifelse(time == "tr_cognitif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_cognitif)

data_long$tr_cognitif <- factor(data_long$tr_cognitif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_cognitif ~ time , data = data_long)

summary(model)




# All 119 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_before"="tr_cognitif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_after"="tr_cognitif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_cognitif_before=max(tr_cognitif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_cognitif_after=max(tr_cognitif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_cognitif_before=ifelse(tr_cognitif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_cognitif_after=ifelse(tr_cognitif_after>=1,1,0)) %>%
  group_by(tci_before, tr_cognitif_before, 
           tci_after, tr_cognitif_after) %>% count()


mean(Before_vs_after$tr_cognitif_before,na.rm=T) 
sd(Before_vs_after$tr_cognitif_before, na.rm=T)  

mean(Before_vs_after$tr_cognitif_after, na.rm=T)  
sd(Before_vs_after$tr_cognitif_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_cognitif_before, Before_vs_after$tr_cognitif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_cognitif_before) %>% count() %>% mutate(n=n/113)
Before_vs_after %>% group_by(tr_cognitif_after) %>% count() %>% mutate(n=n/113)






data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_cognitif_before, tr_cognitif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_cognitif"
  ) %>%
  mutate(
    time = ifelse(time == "tr_cognitif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_cognitif)

data_long$tr_cognitif <- factor(data_long$tr_cognitif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_cognitif ~ time , data = data_long)

summary(model)



# All 56 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_before"="tr_cognitif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_after"="tr_cognitif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_cognitif_before=max(tr_cognitif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_cognitif_after=max(tr_cognitif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_cognitif_before=ifelse(tr_cognitif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_cognitif_after=ifelse(tr_cognitif_after>=1,1,0)) %>%
  group_by(tci_before, tr_cognitif_before, 
           tci_after, tr_cognitif_after) %>% count()

mean(Before_vs_after$tr_cognitif_before,na.rm=T) 
sd(Before_vs_after$tr_cognitif_before, na.rm=T)  

mean(Before_vs_after$tr_cognitif_after, na.rm=T)  
sd(Before_vs_after$tr_cognitif_after, na.rm=T)  

wilcox.test(Before_vs_after$tr_cognitif_before, Before_vs_after$tr_cognitif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_cognitif_before) %>% count() %>% mutate(n=n/52)
Before_vs_after %>% group_by(tr_cognitif_after) %>% count() %>% mutate(n=n/52)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_cognitif_before, tr_cognitif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_cognitif"
  ) %>%
  mutate(
    time = ifelse(time == "tr_cognitif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_cognitif)

data_long$tr_cognitif <- factor(data_long$tr_cognitif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_cognitif ~ time , data = data_long)

summary(model)




# All 36 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_before"="tr_cognitif") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, tr_cognitif, elapsed) %>% rename("tr_cognitif_after"="tr_cognitif") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(tr_cognitif_before=max(tr_cognitif_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(tr_cognitif_after=max(tr_cognitif_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_cognitif_before=ifelse(tr_cognitif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_cognitif_after=ifelse(tr_cognitif_after>=1,1,0)) %>%
  group_by(tci_before, tr_cognitif_before, 
           tci_after, tr_cognitif_after) %>% count()


Before_vs_after %>%
  mutate(tci_before=ifelse(tci_before>=1,1,0)) %>%
  mutate(tr_cognitif_before=ifelse(tr_cognitif_before>=1,1,0)) %>%
  mutate(tci_after=ifelse(tci_after>=1,1,0)) %>%
  mutate(tr_cognitif_after=ifelse(tr_cognitif_after>=1,1,0)) %>%
  group_by(tci_after, tr_cognitif_after) %>% count()




mean(Before_vs_after$tr_cognitif_before,na.rm=T) 
sd(Before_vs_after$tr_cognitif_before, na.rm=T)  

mean(Before_vs_after$tr_cognitif_after, na.rm=T)  
sd(Before_vs_after$tr_cognitif_after, na.rm=T)   

wilcox.test(Before_vs_after$tr_cognitif_before, Before_vs_after$tr_cognitif_after, paired = TRUE, alternative = "two.sided")

Before_vs_after %>% group_by(tr_cognitif_before) %>% count() %>% mutate(n=n/35)
Before_vs_after %>% group_by(tr_cognitif_after) %>% count() %>% mutate(n=n/35)


cor(Before_vs_after$tci_before, Before_vs_after$tr_cognitif_before)
cor(Before_vs_after$tci_after, Before_vs_after$tr_cognitif_after)





data_long <- Before_vs_after %>%
  pivot_longer(
    cols = c(tr_cognitif_before, tr_cognitif_after),
    names_to = "time",  # Create a new column to store "before" or "after"
    values_to = "tr_cognitif"
  ) %>%
  mutate(
    time = ifelse(time == "tr_cognitif_before", "before", "after"), # Simplify names
    time = factor(time, levels = c("before", "after")) # Ensure the correct order
  )

library(ordinal)

unique(data_long$tr_cognitif)

data_long$tr_cognitif <- factor(data_long$tr_cognitif, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clm(tr_cognitif ~ time , data = data_long)

summary(model)




# ------------------


# try to predict -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, ttt_ledd_ago, hoehn_yahr_on, dyskinesie, fluct_motrice,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)


data <- data %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))


apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data)

unique(first_apo$fluct_motrice)
unique(first_apo$hoehn_yahr_on)
unique(first_apo$dyskinesie)


first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 

first_apo <- first_apo %>% mutate(pat_sexe=ifelse(pat_sexe=="H",1,0))

first_apo <- first_apo %>% 
  mutate(tci=ifelse(fluct_motrice==">=2","2",fluct_motrice)) %>%
  mutate(tci=as.numeric(tci))  %>%
  mutate(dyskinesie=ifelse(dyskinesie==">=2","2",dyskinesie)) %>%
  mutate(dyskinesie=as.numeric(dyskinesie)) 



first_apo <- first_apo %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))




Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, ttt_ledd_ago, hoehn_yahr_on, dyskinesie,fluct_motrice,pat_sexe, age,disease_dur, elapsed) %>%   rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, elapsed) %>%  rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )


Before_vs_after <- Before_vs_after %>% mutate(delta=tci_before-tci_after) # the more positiv; the highr reduction


test <- Before_vs_after %>% ungroup() %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>%
  select(delta, ttt_ledd_ago) %>%
  drop_na()

cor.test(test$ttt_ledd_ago,test$delta)

# ---------------

# Pair patients who did not receive apo pump and see their tci over time -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  



Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )

Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 


Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hoehn_yahr_on, pompe_date)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107

data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 

data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 

data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 

data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id, act_datedeb_before) %>%
  left_join(data_all, by=c("anonyme_id"="anonyme_id","act_datedeb_before"="act_datedeb")) %>%
  select(-pompe_date) %>% ungroup() %>%
  drop_na()  %>% select(-c( redcap_repeat_instance))


data_all <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id))
data_all <- data_all %>% select(-pompe_date) %>% drop_na()
patients_to_pair <- data_all %>% select(-c(redcap_repeat_instance))


pump_patients <- pats_apo_to_pair
control_patients <- patients_to_pair

pump_patients$act_datedeb_before <- as.numeric(pump_patients$act_datedeb_before)
control_patients$act_datedeb <- as.numeric(control_patients$act_datedeb)

pump_patients <- pump_patients %>% rename("act_datedeb"="act_datedeb_before")



# Define Tolerance for Numerical Variables
tolerances <- c(age = 5, disease_dur = 5, act_datedeb=365)

# Initialize List to Store Matches
matches <- list()

# Matching Loop
for (i in 1:nrow(pump_patients)) {
  pump <- pump_patients[i, ]
  
  # Filter for Exact Matches (Row-Specific Matching)
  potential_controls <- control_patients %>%
    filter(
      pat_sexe == pump$pat_sexe,      # Exact match on sex
      tci == pump$tci,               # Exact match on TCI
      hoehn_yahr_on == pump$hoehn_yahr_on, # Exact match on disease stage
      abs(age - pump$age) <= tolerances["age"],      # Age within ±5
      abs(act_datedeb - pump$act_datedeb) <= tolerances["act_datedeb"],    # A visit within ±1 year of the pump patients
      abs(disease_dur - pump$disease_dur) <= tolerances["disease_dur"] # Disease duration within ±5
    )
  
  # If there are matches, record them
  if (nrow(potential_controls) > 0) {
    potential_controls$matched_to <- pump$anonyme_id  # Track matched pump patient
    matches[[as.character(pump$anonyme_id)]] <- potential_controls
  }
}

# Combine Matches into a Single Data Frame
match_results <- do.call(rbind, matches)


first_visit_controls <- match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>% mutate(act_datedeb=as.numeric(act_datedeb)) %>%
  inner_join(match_results) %>%
  group_by(anonyme_id, matched_to) %>% summarise(act_datedeb=min(act_datedeb))


pump_patients  %>%
  ungroup() %>%
  summarise(mean=mean(tci))

first_visit_controls %>% select(-matched_to) %>% distinct() %>%
  left_join(data_all %>% mutate(act_datedeb=as.numeric(act_datedeb))) %>%
  ungroup() %>%
  summarise(mean=mean(tci))


first_visit_controls %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  select(anonyme_id, ttt_ledd_ago) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>%
  summarise(mean=mean(ttt_ledd_ago, na.rm=T))


pump_patients %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  group_by(anonyme_id) %>% mutate(pompe_date=as.Date(pompe_date)) %>% summarise(pompe_date=min(pompe_date, na.rm=T)) %>%
  rename("firstpompe"="pompe_date") %>%
  left_join(Consultation_20241028 %>% mutate(act_datedeb =as.Date(act_datedeb))) %>% filter(act_datedeb<firstpompe) %>%
  select(anonyme_id, ttt_ledd_ago) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>%
  summarise(range=range(ttt_ledd_ago, na.rm=T))



match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>%
  ggplot(aes(redcap_repeat_instance , tci)) +
  geom_smooth(se=F)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.5)
  )

match_results %>% select(matched_to) %>% distinct() %>%
  left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
  select(redcap_repeat_instance , tci) %>%
  mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
  mutate(tci=as.numeric(tci)) %>%
  ggplot(aes(redcap_repeat_instance , tci)) +
  geom_smooth(se=F)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.5)
  )



match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(redcap_repeat_instance , tci) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(redcap_repeat_instance , tci) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
  ) %>%
  ggplot(aes(redcap_repeat_instance , tci, colour=Group, fill=Group)) +
  geom_smooth(se=F, method = "loess", size=2, alpha=0.5)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.5)
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Evaluation Number") +
  ylab("Average ICD Score \n")



Consultation_20241028 %>% select(anonyme_id, pompe_date) %>% 
  mutate(pompe_date=ifelse(is.na(pompe_date),0,1)) %>% group_by(anonyme_id ) %>% 
  summarise(pompe_date=max(pompe_date)) %>%
  left_join(Consultation_20241028  %>% select(anonyme_id, tci)) %>%
  mutate(tci=ifelse(tci==">=2","2",tci)) %>% 
  mutate(tci=as.numeric(tci)) %>% ungroup() %>% 
  group_by(pompe_date) %>% summarise(tci=mean(tci, na.rm=T))

# -------------
# Pair patients who did not receive apo pump and see their tci over time V2 Stricter -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



# Apomorphine patients

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )

Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 


# All patients all data, inc apo
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hoehn_yahr_on, pompe_date)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107
data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 
data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 
data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 
data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id, act_datedeb_before) %>%
  left_join(data_all, by=c("anonyme_id"="anonyme_id","act_datedeb_before"="act_datedeb")) %>%
  select(-pompe_date) %>% ungroup() %>%
  drop_na() #  %>% select(-c( redcap_repeat_instance))


data_all <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id))
data_all <- data_all %>% select(-pompe_date) %>% drop_na()

patients_to_pair <- data_all # %>% select(-c(redcap_repeat_instance))

pump_patients <- pats_apo_to_pair
control_patients <- patients_to_pair

pump_patients$act_datedeb_before <- as.numeric(pump_patients$act_datedeb_before)
control_patients$act_datedeb <- as.numeric(control_patients$act_datedeb)

pump_patients <- pump_patients %>% rename("act_datedeb"="act_datedeb_before")

# Define Tolerance for Numerical Variables
tolerances <- c(age = 5, disease_dur = 5, act_datedeb=31, redcap_repeat_instance=2)

# Initialize List to Store Matches
matches <- list()

# Matching Loop
for (i in 1:nrow(pump_patients)) {
  pump <- pump_patients[i, ]
  
  # Filter for Exact Matches (Row-Specific Matching)
  potential_controls <- control_patients %>%
    filter(
      pat_sexe == pump$pat_sexe,      # Exact match on sex
      tci == pump$tci,               # Exact match on TCI
      hoehn_yahr_on == pump$hoehn_yahr_on, # Exact match on disease stage
      abs(age - pump$age) <= tolerances["age"],      # Age within ±5
      abs(act_datedeb - pump$act_datedeb) <= tolerances["act_datedeb"],    # A visit within ±1 year of the pump patients
      abs(disease_dur - pump$disease_dur) <= tolerances["disease_dur"], # Disease duration within ±5
      abs(redcap_repeat_instance - pump$redcap_repeat_instance) <= tolerances["redcap_repeat_instance"] # redcap_repeat_instance within ±2
      
    )
  
  # If there are matches, record them
  if (nrow(potential_controls) > 0) {
    potential_controls$matched_to <- pump$anonyme_id  # Track matched pump patient
    matches[[as.character(pump$anonyme_id)]] <- potential_controls
  }
}

# Combine Matches into a Single Data Frame
match_results <- do.call(rbind, matches)


match_results <- match_results %>% select(anonyme_id, matched_to) %>%
  distinct() %>% group_by(matched_to) %>% slice(1:5)


first_visit_controls <- match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>% mutate(act_datedeb=as.numeric(act_datedeb)) %>%
  inner_join(match_results) %>%
  group_by(anonyme_id, matched_to) %>% summarise(act_datedeb=min(act_datedeb))


pump_patients  %>%
  ungroup() %>%
  summarise(mean=mean(tci))

first_visit_controls %>% select(-matched_to) %>% distinct() %>%
  left_join(data_all %>% mutate(act_datedeb=as.numeric(act_datedeb))) %>%
  ungroup() %>%
  summarise(mean=mean(tci))


first_visit_controls %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  select(anonyme_id, ttt_ledd_ago) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>% filter(ttt_ledd_ago<4000) %>%
  summarise(mean=mean(ttt_ledd_ago, na.rm=T), sd=sd(ttt_ledd_ago, na.rm=T))

# mean    sd
# <dbl> <dbl>
#   1  168.  158.

pump_patients %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  group_by(anonyme_id) %>% mutate(pompe_date=as.Date(pompe_date)) %>% summarise(pompe_date=min(pompe_date, na.rm=T)) %>%
  rename("firstpompe"="pompe_date") %>%
  left_join(Consultation_20241028 %>% mutate(act_datedeb =as.Date(act_datedeb))) %>% filter(act_datedeb<firstpompe) %>%
  select(anonyme_id, ttt_ledd_ago) %>% 
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>% filter(ttt_ledd_ago<4000) %>%
  summarise(mean=mean(ttt_ledd_ago, na.rm=T), sd=sd(ttt_ledd_ago, na.rm=T))

# mean    sd
# <dbl> <dbl>
# 1  206.  146.

match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>%
  ggplot(aes(redcap_repeat_instance , tci)) +
  geom_smooth(se=F)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.5)
  )

match_results %>% select(matched_to) %>% distinct() %>%
  left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
  select(act_datedeb  , tci) %>% mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
  mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
  mutate(tci=as.numeric(tci)) %>% drop_na() %>%
  ggplot(aes(act_datedeb , tci)) +
  geom_smooth()


match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(redcap_repeat_instance , tci) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(redcap_repeat_instance , tci) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
  ) %>%
  ggplot(aes(redcap_repeat_instance , tci, colour=Group, fill=Group)) +
  geom_smooth(se=F, method = "gam", size=2, alpha=0.5)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.35)
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Evaluation Number") +
  ylab("Average ICD Score \n")



test_ignore <- match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(redcap_repeat_instance , tci) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(redcap_repeat_instance , tci) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
    )
    


model <- lm(tci ~ redcap_repeat_instance * Group, data = test_ignore)
summary(model)



match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(act_datedeb , tci) %>%
  mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(act_datedeb , tci) %>%
      mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
  ) %>%
  ggplot(aes(act_datedeb , tci, colour=Group, fill=Group)) +
  geom_smooth(se=F, method = "gam", size=2, alpha=0.3)+
  coord_cartesian(
    xlim = c(16000,20000),
    ylim = c(0,0.3)
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Exact Date") +
  ylab("Average ICD Score \n")



Consultation_20241028 %>% select(anonyme_id, pompe_date) %>% 
  mutate(pompe_date=ifelse(is.na(pompe_date),0,1)) %>% group_by(anonyme_id ) %>% 
  summarise(pompe_date=max(pompe_date)) %>%
  left_join(Consultation_20241028  %>% select(anonyme_id, tci)) %>%
  mutate(tci=ifelse(tci==">=2","2",tci)) %>% 
  mutate(tci=as.numeric(tci)) %>% ungroup() %>% 
  group_by(pompe_date) %>% summarise(tci=mean(tci, na.rm=T))


Consultation_20241028 %>% select(anonyme_id, pompe_date) %>% 
  mutate(pompe_date=ifelse(is.na(pompe_date),0,1)) %>% group_by(anonyme_id ) %>% 
  summarise(pompe_date=max(pompe_date)) %>%
  left_join(Consultation_20241028  %>% select(anonyme_id, ttt_leponex_100_yn___yes)) %>%
  mutate(ttt_leponex_100_yn___yes=as.numeric(ttt_leponex_100_yn___yes)) %>% ungroup() %>% 
  group_by(pompe_date) %>% summarise(ttt_leponex_100_yn___yes=mean(ttt_leponex_100_yn___yes, na.rm=T))




# -------------

# Pair patients who did not receive apo pump and see their tci over time V2 Stricter check Apo AGO LEDD -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



# Apomorphine patients

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )

Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 


# All patients all data, inc apo
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, ttt_ledd_ago, hoehn_yahr_on, pompe_date)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107
data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 
data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 
data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 
data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id, act_datedeb_before) %>%
  left_join(data_all, by=c("anonyme_id"="anonyme_id","act_datedeb_before"="act_datedeb")) %>%
  select(-pompe_date) %>% ungroup() %>%
  drop_na() #  %>% select(-c( redcap_repeat_instance))


data_all <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id))
data_all <- data_all %>% select(-pompe_date) %>% drop_na()

patients_to_pair <- data_all # %>% select(-c(redcap_repeat_instance))

pump_patients <- pats_apo_to_pair
control_patients <- patients_to_pair

pump_patients$act_datedeb_before <- as.numeric(pump_patients$act_datedeb_before)
control_patients$act_datedeb <- as.numeric(control_patients$act_datedeb)

pump_patients <- pump_patients %>% rename("act_datedeb"="act_datedeb_before")

# Define Tolerance for Numerical Variables
tolerances <- c(age = 5, disease_dur = 5, act_datedeb=31, redcap_repeat_instance=2)

# Initialize List to Store Matches
matches <- list()

# Matching Loop
for (i in 1:nrow(pump_patients)) {
  pump <- pump_patients[i, ]
  
  # Filter for Exact Matches (Row-Specific Matching)
  potential_controls <- control_patients %>%
    filter(
      pat_sexe == pump$pat_sexe,      # Exact match on sex
      tci == pump$tci,               # Exact match on TCI
      hoehn_yahr_on == pump$hoehn_yahr_on, # Exact match on disease stage
      abs(age - pump$age) <= tolerances["age"],      # Age within ±5
      abs(act_datedeb - pump$act_datedeb) <= tolerances["act_datedeb"],    # A visit within ±1 year of the pump patients
      abs(disease_dur - pump$disease_dur) <= tolerances["disease_dur"], # Disease duration within ±5
      abs(redcap_repeat_instance - pump$redcap_repeat_instance) <= tolerances["redcap_repeat_instance"] # redcap_repeat_instance within ±2
      
    )
  
  # If there are matches, record them
  if (nrow(potential_controls) > 0) {
    potential_controls$matched_to <- pump$anonyme_id  # Track matched pump patient
    matches[[as.character(pump$anonyme_id)]] <- potential_controls
  }
}

# Combine Matches into a Single Data Frame
match_results <- do.call(rbind, matches)


match_results <- match_results %>% select(anonyme_id, matched_to) %>%
  distinct() %>% group_by(matched_to) %>% slice(1:5)


first_visit_controls <- match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>% mutate(act_datedeb=as.numeric(act_datedeb)) %>%
  inner_join(match_results) %>%
  group_by(anonyme_id, matched_to) %>% summarise(act_datedeb=min(act_datedeb))


pump_patients  %>%
  ungroup() %>%
  summarise(mean=mean(tci))

first_visit_controls %>% select(-matched_to) %>% distinct() %>%
  left_join(data_all %>% mutate(act_datedeb=as.numeric(act_datedeb))) %>%
  ungroup() %>%
  summarise(mean=mean(tci))


first_visit_controls %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  select(anonyme_id, ttt_ledd_ago) %>%
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>% filter(ttt_ledd_ago<4000) %>%
  summarise(mean=mean(ttt_ledd_ago, na.rm=T), sd=sd(ttt_ledd_ago, na.rm=T))

# mean    sd
# <dbl> <dbl>
#   1  168.  158.


pump_patients %>% select(anonyme_id) %>% distinct() %>% ungroup() %>%
  inner_join(Consultation_20241028)  %>%
  group_by(anonyme_id) %>% mutate(pompe_date=as.Date(pompe_date)) %>% summarise(pompe_date=min(pompe_date, na.rm=T)) %>%
  rename("firstpompe"="pompe_date") %>%
  left_join(Consultation_20241028 %>% mutate(act_datedeb =as.Date(act_datedeb))) %>% filter(act_datedeb==firstpompe) %>%
  select(anonyme_id, ttt_ledd_ago) %>% 
  mutate(ttt_ledd_ago=as.numeric(ttt_ledd_ago)) %>% filter(ttt_ledd_ago<4000) %>%
  summarise(mean=mean(ttt_ledd_ago, na.rm=T), sd=sd(ttt_ledd_ago, na.rm=T))

# mean    sd
# <dbl> <dbl>
# 1  206.  146.

match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all) %>%
  ggplot(aes(redcap_repeat_instance , tci)) +
  geom_smooth(se=F)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.5)
  )

match_results %>% select(matched_to) %>% distinct() %>%
  left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
  select(act_datedeb  , tci) %>% mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
  mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
  mutate(tci=as.numeric(tci)) %>% drop_na() %>%
  ggplot(aes(act_datedeb , tci)) +
  geom_smooth()


match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(redcap_repeat_instance , tci) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(redcap_repeat_instance , tci) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
  ) %>%
  ggplot(aes(redcap_repeat_instance , tci, colour=Group, fill=Group)) +
  geom_smooth(se=F, method = "gam", size=2, alpha=0.5)+
  coord_cartesian(
    xlim = c(0,10),
    ylim = c(0,0.35)
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Evaluation Number") +
  ylab("Average ICD Score \n")




match_results %>% select(anonyme_id) %>%
  distinct() %>%
  left_join(data_all) %>%
  group_by(anonyme_id) %>% count() %>%
  filter(n>1) %>% select(-n) %>%
  left_join(data_all)  %>% ungroup() %>%
  select(act_datedeb , tci) %>%
  mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
  mutate(Group="Control (no pump)") %>%
  bind_rows(
    match_results %>% select(matched_to) %>% distinct() %>%
      left_join(Consultation_20241028, by=c("matched_to"="anonyme_id")) %>%
      select(act_datedeb , tci) %>%
      mutate(act_datedeb=as.numeric(as.Date(act_datedeb) )) %>%
      mutate(tci=ifelse(tci==">=2", "2", tci)) %>%
      mutate(tci=as.numeric(tci)) %>% drop_na() %>%
      mutate(Group="Apomorphine Pump")
  ) %>%
  ggplot(aes(act_datedeb , tci, colour=Group, fill=Group)) +
  geom_smooth(se=F, method = "gam", size=2, alpha=0.3)+
  coord_cartesian(
    xlim = c(16000,20000),
    ylim = c(0,0.3)
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Exact Date") +
  ylab("Average ICD Score \n")



Consultation_20241028 %>% select(anonyme_id, pompe_date) %>% 
  mutate(pompe_date=ifelse(is.na(pompe_date),0,1)) %>% group_by(anonyme_id ) %>% 
  summarise(pompe_date=max(pompe_date)) %>%
  left_join(Consultation_20241028  %>% select(anonyme_id, tci)) %>%
  mutate(tci=ifelse(tci==">=2","2",tci)) %>% 
  mutate(tci=as.numeric(tci)) %>% ungroup() %>% 
  group_by(pompe_date) %>% summarise(tci=mean(tci, na.rm=T))


Consultation_20241028 %>% select(anonyme_id, pompe_date) %>% 
  mutate(pompe_date=ifelse(is.na(pompe_date),0,1)) %>% group_by(anonyme_id ) %>% 
  summarise(pompe_date=max(pompe_date)) %>%
  left_join(Consultation_20241028  %>% select(anonyme_id, ttt_leponex_100_yn___yes)) %>%
  mutate(ttt_leponex_100_yn___yes=as.numeric(ttt_leponex_100_yn___yes)) %>% ungroup() %>% 
  group_by(pompe_date) %>% summarise(ttt_leponex_100_yn___yes=mean(ttt_leponex_100_yn___yes, na.rm=T))




# -------------

# How many patients per center -----------------
pats_to_track <- fread("pats8to8track.txt")


Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

Cohorte_NS_PARK_Correspondance_No_centre <- read_excel(path = "Cohorte NS-PARK_Correspondance No centre.xlsx")

names(Consultation_20241028)

Consultation_20241028 %>% select(redcap_data_access_group, anonyme_id) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(redcap_data_access_group) %>% count() %>% arrange(-n)

redcap_data_access_group     n
1 paris                       27
2 poitiers                    23
3 nice                        14
4 toulouse                    14
5 nmes                        13
6 reims                       13
7 creteil                     10
8 caen                         8
9 lille                        8
10 limoges                      6
11 lyon                         6
12 bordeaux                     2
13 marseille                    1
14 nancy                        1
15 nantes                       1
16 rennes                       1
17 rouen                        1

# -----------

# Pair patients who did not receive apo pump and see their tci over time Propensity matching -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



# Apomorphine patients

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )

Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 




# All patients all data, inc apo
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hoehn_yahr_on, pompe_date, fluct_motrice, dyskinesie)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107
data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 
data_all <- data_all %>% mutate(fluct_motrice=ifelse(fluct_motrice==">=2","2",fluct_motrice)) %>%
  mutate(fluct_motrice=as.numeric(fluct_motrice)) 
data_all <- data_all %>% mutate(dyskinesie=ifelse(dyskinesie==">=2","2",dyskinesie)) %>%
  mutate(dyskinesie=as.numeric(dyskinesie)) 


data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 
data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 
data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


v2_controls <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct()) %>% select(-pompe_date)

v2_pumps <- data_all %>% inner_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct()) %>% select(-pompe_date)



combined_data <- bind_rows(
  v2_pumps %>% mutate(group = 1),
  v2_controls %>% mutate(group = 0)
)


library(missMDA)


act_datedeb <- combined_data %>% select(act_datedeb)

combined_data <- combined_data %>% select(-act_datedeb)

combined_data_imputed <- imputePCA(combined_data,ncp=2, scale = T)

combined_data_imputed <- data.frame(combined_data_imputed$completeObs)

combined_data_imputed <- combined_data_imputed %>% bind_cols(act_datedeb)

names(combined_data_imputed)





library(MatchIt)

# Fit Propensity Score Model
ps_model <- matchit(
  group ~ pat_sexe + age + disease_dur + hoehn_yahr_on + fluct_motrice + dyskinesie ,
  data = combined_data_imputed,
  method = "nearest",  # Nearest neighbor matching
  distance = "logit",  # Logistic regression for propensity score
  ratio = 5            # 5 controls per PUMP
)



matched_data <- match.data(ps_model)
head(matched_data)



library(cobalt)

# Plot Covariate Balance Before and After Matching
love.plot(ps_model, threshold = 0.1)  # Add threshold line at |SMD| = 0.1





# Add propensity scores to the original dataset
combined_data_imputed$ps <- ps_model$distance


# Plot Propensity Score Distribution
ggplot(combined_data_imputed, aes(x = ps, fill = as.factor(group))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Propensity Score Distribution",
    x = "Propensity Score",
    fill = "Group"
  ) +
  theme_minimal() +
  xlim(0,0.1)



combined_data_imputed %>%
  filter(act_datedeb>="2016-01-01" & act_datedeb<="2024-12-31") %>% 
  filter(redcap_repeat_instance<=10) %>%
  mutate(group=ifelse(group==1, "Apo pump", "Control")) %>%
  ggplot(aes(redcap_repeat_instance, tci, color=group, fill=group )) +
  geom_smooth(method="gam", se=F) +
  coord_cartesian(ylim=c(0,0.3)) +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Evaluation Number") +
  ylab("ICD Fit Score \n")


combined_data_imputed %>%
  filter(act_datedeb>="2016-01-01" & act_datedeb<="2024-12-31") %>% 
  filter(redcap_repeat_instance<=10) %>%
  mutate(group=ifelse(group==1, "Apo pump", "Control")) %>%
  ggplot(aes(act_datedeb, tci, color=group, fill=group )) +
  geom_smooth(method="gam", se=F) +
  coord_cartesian(ylim=c(0,0.3)) +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#558EA9", "#A955A7")) +
  scale_fill_manual(values=c("#558EA9", "#A955A7")) +
  xlab("\n Evaluation Number") +
  ylab("Average ICD Score \n")


# -------------

# ttt_nb_levodopa Before vs After Using Paired Samples -------------------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, ttt_nb_levodopa,  pompe_date)
data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)
names(Consultation_20241028)
data <- data %>% arrange(anonyme_id, redcap_repeat_instance)
apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo
data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 
first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258
first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci, ttt_nb_levodopa))

unique(first_apo$ttt_nb_levodopa)
unique(first_apo$tci)

first_apo$ttt_nb_levodopa <- as.numeric(first_apo$ttt_nb_levodopa)

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before"))

first_apo <- first_apo %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci))




# All 145 available
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_before"="ttt_nb_levodopa") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_after"="ttt_nb_levodopa") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ttt_nb_levodopa_before=max(ttt_nb_levodopa_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ttt_nb_levodopa_after=max(ttt_nb_levodopa_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()

mean(Before_vs_after$ttt_nb_levodopa_before,na.rm=T) 
sd(Before_vs_after$ttt_nb_levodopa_before, na.rm=T)  

mean(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  
sd(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  

wilcox.test(Before_vs_after$ttt_nb_levodopa_before, Before_vs_after$ttt_nb_levodopa_after, paired = TRUE, alternative = "two.sided")



# All 120 within 60 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_before"="ttt_nb_levodopa") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=60) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_after"="ttt_nb_levodopa") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-60)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ttt_nb_levodopa_before=max(ttt_nb_levodopa_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ttt_nb_levodopa_after=max(ttt_nb_levodopa_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()



mean(Before_vs_after$ttt_nb_levodopa_before,na.rm=T) 
sd(Before_vs_after$ttt_nb_levodopa_before, na.rm=T)  

mean(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  
sd(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  

wilcox.test(Before_vs_after$ttt_nb_levodopa_before, Before_vs_after$ttt_nb_levodopa_after, paired = TRUE, alternative = "two.sided")



# All 57 within 24 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_before"="ttt_nb_levodopa") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_after"="ttt_nb_levodopa") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-24)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ttt_nb_levodopa_before=max(ttt_nb_levodopa_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ttt_nb_levodopa_after=max(ttt_nb_levodopa_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()


mean(Before_vs_after$ttt_nb_levodopa_before,na.rm=T) 
sd(Before_vs_after$ttt_nb_levodopa_before, na.rm=T)  

mean(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  
sd(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  

wilcox.test(Before_vs_after$ttt_nb_levodopa_before, Before_vs_after$ttt_nb_levodopa_after, paired = TRUE, alternative = "two.sided")



# All 35 within 12 months
Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
  select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_before"="ttt_nb_levodopa") %>%  rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>%
  filter(elapsed_before<=12) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>% 
      select(anonyme_id, tci, ttt_nb_levodopa, elapsed) %>% rename("ttt_nb_levodopa_after"="ttt_nb_levodopa") %>%   rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed")  %>%
      filter(elapsed_after>=(-12)) 
  )

Before_vs_after <- Before_vs_after %>% group_by(anonyme_id, elapsed_before) %>% 
  mutate(ttt_nb_levodopa_before=max(ttt_nb_levodopa_before)) %>%
  mutate(tci_before=max(tci_before)) %>%
  ungroup() %>%
  group_by(anonyme_id, elapsed_after) %>% 
  mutate(ttt_nb_levodopa_after=max(ttt_nb_levodopa_after)) %>% 
  mutate(tci_after=max(tci_after)) %>% ungroup()  %>% distinct()


mean(Before_vs_after$ttt_nb_levodopa_before,na.rm=T) 
sd(Before_vs_after$ttt_nb_levodopa_before, na.rm=T)  

mean(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  
sd(Before_vs_after$ttt_nb_levodopa_after, na.rm=T)  

wilcox.test(Before_vs_after$ttt_nb_levodopa_before, Before_vs_after$ttt_nb_levodopa_after, paired = TRUE, alternative = "two.sided")


# ------------------


# Pair patients who did not receive apo pump and see their tci over time Propensity matching check longitudinal model -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



# Apomorphine patients

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )

Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 




# All patients all data, inc apo
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hoehn_yahr_on, pompe_date, fluct_motrice, dyskinesie)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107
data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 
data_all <- data_all %>% mutate(fluct_motrice=ifelse(fluct_motrice==">=2","2",fluct_motrice)) %>%
  mutate(fluct_motrice=as.numeric(fluct_motrice)) 
data_all <- data_all %>% mutate(dyskinesie=ifelse(dyskinesie==">=2","2",dyskinesie)) %>%
  mutate(dyskinesie=as.numeric(dyskinesie)) 


data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 
data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 
data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


v2_controls <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct()) %>% select(-pompe_date)

v2_pumps <- data_all %>% inner_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct()) %>% select(-pompe_date)



combined_data <- bind_rows(
  v2_pumps %>% mutate(group = 1),
  v2_controls %>% mutate(group = 0)
)


library(missMDA)


act_datedeb <- combined_data %>% select(act_datedeb)

combined_data <- combined_data %>% select(-act_datedeb)

combined_data_imputed <- imputePCA(combined_data,ncp=2, scale = T)

combined_data_imputed <- data.frame(combined_data_imputed$completeObs)

combined_data_imputed <- combined_data_imputed %>% bind_cols(act_datedeb)

names(combined_data_imputed)



library(MatchIt)

# Fit Propensity Score Model
ps_model <- matchit(
  group ~ pat_sexe + age + disease_dur + hoehn_yahr_on + fluct_motrice + dyskinesie ,
  data = combined_data_imputed,
  method = "nearest",  # Nearest neighbor matching
  distance = "logit",  # Logistic regression for propensity score
  ratio = 5            # 5 controls per PUMP
)


matched_data <- match.data(ps_model)
head(matched_data)


library(cobalt)

# Plot Covariate Balance Before and After Matching
love.plot(ps_model, threshold = 0.1)  # Add threshold line at |SMD| = 0.1



# Add propensity scores to the original dataset
combined_data_imputed$ps <- ps_model$distance

library(lme4)

v2_combined_data_imputed <- combined_data_imputed %>% mutate(fluct_motrice=ifelse(fluct_motrice<=0,0,1))


v2_combined_data_imputed %>% group_by(group) %>% summarise(mean=mean(fluct_motrice))


model <- glm(fluct_motrice  ~ redcap_repeat_instance * group , 
             data = v2_combined_data_imputed, family = binomial)

summary(model)

# -------------

# Propensity matching  Visit before vs after 5:1 for each visit -----------
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")

Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")

MPs <- Inclusion_20241028 <- Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()

length(unique(Consultation_20241028$anonyme_id)) # 31988

Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)

length(unique(Consultation_20241028$anonyme_id)) # 25449

Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342



# Apomorphine patients

data <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, pompe_date)

data$act_datedeb <- as.Date(data$act_datedeb)
data$pompe_date <- as.Date(data$pompe_date)

data <- data %>% arrange(anonyme_id, redcap_repeat_instance)

apo_pats <- data %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342 with apo

data <- apo_pats %>% left_join(data) %>% filter(!is.na(act_datedeb)) %>%
  group_by(anonyme_id) %>% count() %>% filter(n>1) %>% # 262 > 1 visit with known date
  select(anonyme_id) %>% left_join(data) %>% ungroup() %>% filter(!is.na(act_datedeb)) 

first_apo <- data %>% filter(!is.na(pompe_date)) %>% group_by(anonyme_id) %>%
  summarise(pompe_date=min(pompe_date)) %>% distinct()  # 258

first_apo <- first_apo %>% left_join(data %>% select(anonyme_id, act_datedeb, tci))

first_apo <- first_apo %>% mutate(elapsed=interval(act_datedeb,pompe_date ) %/% months(1)) %>%
  filter(tci %in% c("1","0",">=2", "2", "3", "4")) %>% 
  mutate(before_after=ifelse(act_datedeb>pompe_date, "after", "before")) %>%
  mutate(tci=parse_number(tci))  

Before_vs_after <- first_apo %>% filter(before_after=="before") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
  select(anonyme_id, tci, elapsed,act_datedeb ) %>% rename("tci_before"="tci") %>% rename("elapsed_before"="elapsed") %>% rename("act_datedeb_before"="act_datedeb") %>% 
  filter(elapsed_before<=24) %>%
  inner_join(
    first_apo %>% filter(before_after=="after") %>% group_by(anonyme_id) %>% filter(elapsed==max(elapsed)) %>%
      select(anonyme_id, tci, elapsed, act_datedeb) %>% rename("tci_after"="tci") %>% rename("elapsed_after"="elapsed") %>% rename("act_datedeb_after"="act_datedeb") %>% 
      filter(elapsed_after>=(-24))
  )




Before_vs_after <- Before_vs_after %>% distinct()

mean(Before_vs_after$tci_before)
mean(Before_vs_after$tci_after)

pats_apo_to_pair <- Before_vs_after %>% select(anonyme_id) 














# All patients all data, inc apo
Consultation_20241028 <- read_excel(path = "Consultation_20241028.xlsx")
Inclusion_20241028 <- read_excel(path = "Inclusion_20241028.xlsx")
MPs <-  Inclusion_20241028 %>% filter(diag=="MP") %>% select(anonyme_id) %>% distinct()
length(unique(Consultation_20241028$anonyme_id)) # 31988
Consultation_20241028 <- Consultation_20241028 %>% inner_join(MPs)
length(unique(Consultation_20241028$anonyme_id)) # 25449
Consultation_20241028 %>% filter(!is.na(pompe_date)) %>% select(anonyme_id) %>% distinct() # 342

data_all <- Consultation_20241028 %>% select(anonyme_id, redcap_repeat_instance, act_datedeb, tci, hoehn_yahr_on, pompe_date, fluct_motrice, dyskinesie)
data_all$act_datedeb <- as.Date(data_all$act_datedeb)
data_all$pompe_date <- as.Date(data_all$pompe_date)
data_all <- data_all %>% arrange(anonyme_id, redcap_repeat_instance)
data_all <- data_all %>% left_join(Inclusion_20241028 %>% select(anonyme_id, pat_sexe, pat_ddn_a, diag_date_a))
length(unique(data_all$anonyme_id))  # 25107
data_all <- data_all %>% mutate(tci=ifelse(tci==">=2","2",tci)) %>%
  mutate(tci=as.numeric(tci)) 
data_all <- data_all %>% mutate(fluct_motrice=ifelse(fluct_motrice==">=2","2",fluct_motrice)) %>%
  mutate(fluct_motrice=as.numeric(fluct_motrice)) 
data_all <- data_all %>% mutate(dyskinesie=ifelse(dyskinesie==">=2","2",dyskinesie)) %>%
  mutate(dyskinesie=as.numeric(dyskinesie)) 


data_all <- data_all %>%  mutate(hoehn_yahr_on=as.numeric(hoehn_yahr_on)) 
data_all <- data_all %>%  mutate(pat_sexe =ifelse(pat_sexe =="H",1,0)) 
data_all <- data_all %>% mutate(year=str_sub(as.character(act_datedeb), 1L, 4L)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(pat_ddn_a=as.numeric(pat_ddn_a)) %>%
  mutate(diag_date_a=as.numeric(diag_date_a)) %>%
  mutate(age=year-pat_ddn_a) %>% select(-c(pat_ddn_a)) %>%
  mutate(disease_dur=year-diag_date_a) %>% select(-c(diag_date_a, year))


Before_vs_after <- Before_vs_after %>% group_by(anonyme_id) %>% 
  filter(act_datedeb_before==max(act_datedeb_before)) %>% 
  filter(act_datedeb_after==min(act_datedeb_after))

pats_to_macth <- Before_vs_after %>% select(anonyme_id, act_datedeb_before) %>% rename("act_datedeb"="act_datedeb_before")  %>%
  bind_rows(Before_vs_after %>% select(anonyme_id, act_datedeb_after) %>% rename("act_datedeb"="act_datedeb_after"))

group1 <- pats_to_macth %>% ungroup() %>% mutate(group=1) %>% inner_join(data_all) 




group0 <- data_all %>% anti_join(data_all %>% filter(!is.na(pompe_date)) %>% select(anonyme_id)) %>% mutate(group=0)


group0 <- group0 %>% select(anonyme_id, act_datedeb, group, redcap_repeat_instance:disease_dur)



groups <- group1 %>% bind_rows(group0)


groups <- groups %>% select(-pompe_date)

library(missMDA)


act_datedeb <- groups %>% select(act_datedeb)

combined_data <- groups %>% select(-act_datedeb)

combined_data_imputed <- imputePCA(combined_data,ncp=2, scale = T)

combined_data_imputed <- data.frame(combined_data_imputed$completeObs)

combined_data_imputed <- combined_data_imputed %>% bind_cols(act_datedeb)

names(combined_data_imputed)


combined_data_imputed$act_datedeb <- as.numeric(as.Date(combined_data_imputed$act_datedeb, format="%Y-%m-%d"))

combined_data_imputed <- combined_data_imputed %>% filter(act_datedeb>0)

ps_model <- MatchIt::matchit(
  group ~ pat_sexe + age + disease_dur + hoehn_yahr_on + fluct_motrice + dyskinesie + act_datedeb,
  data = combined_data_imputed,
  method = "nearest",   # Nearest neighbor matching
  distance = "logit",   # Logistic regression for propensity score
  ratio = 5             # 5 controls per treated
)



# Extract matched dataset
matched_data <- MatchIt::match.data(ps_model)

length(unique(matched_data$subclass))
length(unique(matched_data$anonyme_id))



unique(Consultation_20241028$date_impl)

dbs_pats <- Consultation_20241028 %>% filter(!is.na(date_impl)) %>% select(anonyme_id) %>% distinct()


matched_data %>% select(anonyme_id) %>% distinct() %>%
  inner_join(dbs_pats)


# Check balance after matching
summary(ps_model)


length(unique(matched_data$anonyme_id))


matched_data %>% group_by(anonyme_id) %>% 
  mutate(act_datedeb=ifelse(act_datedeb==min(act_datedeb),1,2)) %>%
  group_by(group, act_datedeb) %>%
  summarise(n=mean(tci), sd=sd(tci))



# Calculate mean and standard error
data_summary <- matched_data %>% group_by(anonyme_id) %>% 
  mutate(act_datedeb=ifelse(act_datedeb==min(act_datedeb),1,2)) %>%
  ungroup() %>%
  group_by(group, act_datedeb) %>%
  summarise(
    mean_tci = mean(tci, na.rm = TRUE),
    se_tci = sd(tci, na.rm = TRUE) / sqrt(n())  # Standard error
  )


data_summary <- data_summary %>% mutate(group=ifelse(group==0, "A) Never Apo Pump [CSAI]", "B) Apo Pump Experienced [CSAI]"))

data_summary <- data_summary %>% mutate(act_datedeb=ifelse(act_datedeb==1, "1-BEFORE Apo Pump [CSAI]", "2-AFTER Apo Pump [CSAI]"))

data_summary <- data_summary %>% 
  mutate(act_datedeb=ifelse(act_datedeb=="1-BEFORE Apo Pump [CSAI]" & group=="A) Never Apo Pump [CSAI]", "1-BEFORE (Matched)",
                            ifelse(act_datedeb=="2-AFTER Apo Pump [CSAI]" & group=="A) Never Apo Pump [CSAI]","2-AFTER (Matched)", act_datedeb)))


# Ensure act_datedeb is a factor with a specific order
data_summary <- data_summary %>%
  mutate(act_datedeb = factor(act_datedeb, 
                              levels = c("1-BEFORE (Matched)", "2-AFTER (Matched)", 
                                         "1-BEFORE Apo Pump [CSAI]", "2-AFTER Apo Pump [CSAI]")))

# Create the bar plot with correct ordering
ggplot(data_summary, aes(x = act_datedeb, y = mean_tci, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6, alpha=0.5) +  # Create bar plot
  geom_errorbar(aes(ymin = mean_tci - se_tci, ymax = mean_tci + se_tci), 
                width = 0.2, position = position_dodge(0.6)) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    x = "\n Visit - Closest Before|After CSAI \n [Or date-matched for controls]",
    y = "Mean TCI [SEM] \n",
    fill = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability +
    scale_color_manual(values=c("#A955A7", "#558EA9")) +
  scale_fill_manual(values=c("#A955A7", "#558EA9")) 


test <- matched_data %>% group_by(anonyme_id) %>% 
  mutate(act_datedeb=ifelse(act_datedeb==min(act_datedeb),1,2)) %>%
  ungroup() %>% select(anonyme_id, group, act_datedeb, tci) %>% distinct() %>%
  arrange(anonyme_id)

test %>% filter(group==1) 
test %>% filter(group==0) 

# Split data by group and act_datedeb
group_0_before <- test %>% filter(group == 0, act_datedeb == 1)
group_0_after  <- test %>% filter(group == 0, act_datedeb == 2)

group_1_before <- test %>% filter(group == 1, act_datedeb == 1)
group_1_after  <- test %>% filter(group == 1, act_datedeb == 2)

# Perform Wilcoxon rank-sum test (unpaired version)
wilcox_test_group_0 <- wilcox.test(group_0_before$tci, group_0_after$tci, paired =T)

wilcox_test_group_1 <- wilcox.test(group_1_before$tci, group_1_after$tci, paired =T)

3# Print results
wilcox_test_group_0
wilcox_test_group_1



# ------


