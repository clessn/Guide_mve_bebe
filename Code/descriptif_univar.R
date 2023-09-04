# Packages ----------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)


# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

table(Data$info)
RawData <- read.csv("_SharedFolder_Guide_mve/data/INSPQ-guide-mve_RAW.csv",
                    encoding = "UTF-8")


# SES -------------------------------------------------------------------

# ses_link_kid
table(Data$ses_link_kidmom)
table(Data$ses_link_kiddad)
table(Data$ses_link_kidstepdad)
table(Data$ses_link_kidstepmom)

# Reshape the data
link_counts <- data.frame(
  link_Group = c("Mère", "Père", "Partenaire de la mère", "Partenaire du père"),
  Count = c(sum(Data$ses_link_kidmom, na.rm = TRUE),
            sum(Data$ses_link_kiddad, na.rm = TRUE),
            sum(Data$ses_link_kidstepdad, na.rm = TRUE),
            sum(Data$ses_link_kidstepmom, na.rm = TRUE)))

# Plot bar chart 
ggplot(link_counts, aes(x=link_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,800), breaks=seq(0, 800, by=200)) +
  labs(title="Nombre de répondants par lien avec l'enfant", x="Type de lien", y="Nombre de répondants") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#_______________________________________________________________________________

# AGE (aller corriger la variable Data$ses_age50 dans cleaning)
#Data$ses_age1829, Data$ses_age3039, Data$ses_age4049, Data$ses_age50

table(Data$ses_age3039)

# Reshape the data
age_counts <- data.frame(
  Age_Group = c("18-29", "30-39", "40-49", "50 ou plus"),
  Count = c(sum(Data$ses_age1829, na.rm = TRUE),
            sum(Data$ses_age3039, na.rm = TRUE),
            sum(Data$ses_age4049, na.rm = TRUE),
            sum(Data$ses_age50, na.rm = TRUE)))

# Plot bar chart
ggplot(age_counts, aes(x=Age_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,600), breaks=seq(0, 600, by=200)) +
  labs(title="Nombre de répondants par groupe d'âge", x="Groupes d'âge", y="Nombre de répondants") +
  theme_clean()

#_______________________________________________________________________________

# ses_sex_ori ----> aurait du être ""situation parentale"

# Reshape the data
couple_counts <- data.frame(
  Couple_Group = c("Autre", "Couple", "Famille monoparentale"),
  Count = c(sum(Data$ses_coupleautre, na.rm = TRUE),
            sum(Data$ses_couple, na.rm = TRUE),
            sum(Data$ses_monoparental, na.rm = TRUE)))

# Plot bar chart
ggplot(couple_counts, aes(x=Couple_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,900), breaks=seq(0, 900, by=200)) +
  labs(title="Nombre de répondants par situation parentale", x="Situation parentale", y="Nombre de répondants") +
  theme_clean()

#_______________________________________________________________________________

# ses_immigrant + ses_immigrant year

# Reshape the data
canada_counts <- data.frame(
  Canada_Group = c("Oui", "Non"),
  Count = c(sum(Data$ses_canada == 1, na.rm = TRUE),
            sum(Data$ses_canada == 0, na.rm = TRUE)))

# Plot bar chart
ggplot(canada_counts, aes(x=Canada_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,900), breaks=seq(0, 900, by=200)) +
  labs(title="Nombre de répondants nés au Canada", x="né au Canada?", y="Nombre de répondants") +
  theme_clean()

# REVOIR J'AI ESSAYÉ MAIS ÇA PEUT ÊTRE MIEUX
# Reshape the data
table(Data$ses_immigrant_year)

Data$ses_immigrant_year[Data$ses_immigrant_year == 1] <- "1"
Data$ses_immigrant_year[Data$ses_immigrant_year == 2] <- "2"
Data$ses_immigrant_year[Data$ses_immigrant_year == 3] <- "3"
Data$ses_immigrant_year[Data$ses_immigrant_year == 4] <- "4"
Data$ses_immigrant_year[Data$ses_immigrant_year == 5] <- "5"
Data$ses_immigrant_year[Data$ses_immigrant_year >= 6 & Data$ses_immigrant_year <= 9] <- "6-9"
Data$ses_immigrant_year[Data$ses_immigrant_year == 10] <- "10"
Data$ses_immigrant_year[Data$ses_immigrant_year >= 11 & Data$ses_immigrant_year <= 14] <- "11-14"
Data$ses_immigrant_year[Data$ses_immigrant_year >= 15 & Data$ses_immigrant_year <= 19] <- "15-19"
Data$ses_immigrant_year[Data$ses_immigrant_year >= 20 & Data$ses_immigrant_year <= 24] <- "20-24"
Data$ses_immigrant_year[Data$ses_immigrant_year >= 25 & Data$ses_immigrant_year <= 29] <- "25-29"

ggplot(Data, aes(x=ses_immigrant_year)) +
  geom_bar(fill="black", width=0.3) +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6-9", "10", "11-14", "15-19", "20-24", "25-29", "30+")) +
  scale_y_continuous(limits=c(0,60), breaks=seq(0, 60, by=10)) +
  labs(title="Années depuis l'arrivée au Québec", x="Nombre d'années", y="Nombre de répondants") +
  theme_clean()
# REVOIR J'AI ESSAYÉ MAIS ÇA PEUT ÊTRE MIEUX

#_______________________________________________________________________________

# ses_language  -----> je n'ai pas fait de graph car presque juste francophone

table(Data$ses_languageboth)

# Reshape the data
language_counts <- data.frame(
  Language_Group = c("Both", "English", "French", "Arab"),
  Count = c(
    sum(Data$ses_languageboth, na.rm = TRUE),
    sum(Data$ses_languageeng, na.rm = TRUE),
    sum(Data$ses_languagefr, na.rm = TRUE),
    sum(Data$ses_languagearab, na.rm = TRUE)))

#_______________________________________________________________________________

# NOMBRE DE KIDS

table(RawData$ses_kids_1)
table(Data$ses_kids)

Data$ses_kids_2[Data$ses_kids == 0] <- "0"
Data$ses_kids_2[Data$ses_kids == 1] <- "1"
Data$ses_kids_2[Data$ses_kids == 2] <- "2"
Data$ses_kids_2[Data$ses_kids == 3] <- "3"
Data$ses_kids_2[Data$ses_kids == 4] <- "4"
Data$ses_kids_2[Data$ses_kids >= 5] <- "5+"

table(Data$ses_kids_2)

ggplot(Data, aes(x=ses_kids_2)) +
  geom_bar(fill="black", width=0.3) +
  scale_x_discrete(limits=c("0", "1", "2", "3", "4", "5+")) +
  scale_y_continuous(limits=c(0,400), breaks=seq(0, 400, by=50)) +
  labs(title="Nombre d'enfants par foyer", x="Nombre d'enfants", y="Nombre de répondants") +
  theme_clean()
#_______________________________________________________________________________

# ses_region_3

# Reshape the data
region_counts <- data.frame(
  Region_Group = c("Nord-du-Québec", "Mauricie", "Lanaudière", 
                   "En dehors de Québec", "Centre-du-Québec", 
                   "Abitibi-Témiscamingue", "Bas-Saint-Laurent", "Chaudière-Appalaches",
                   "Estrie", "Laurentides", "Montérégie", "Outaouais", "Saguenay-Lac-Saint-Jean",
                   "Montréal", "Laval", "Gaspésie-Îles-de-la-Madeleine", "Côte-Nord", "Capitale-Nationale"),
  Count = c(
    sum(Data$ses_regionabitibi, na.rm = TRUE),
    sum(Data$ses_regionchaudiere, na.rm = TRUE),
    sum(Data$ses_regioniles, na.rm = TRUE),
    sum(Data$ses_regionmauricie, na.rm = TRUE),
    sum(Data$ses_regionoutaouais, na.rm = TRUE),
    sum(Data$ses_regionbasstlaur, na.rm = TRUE),
    sum(Data$ses_regioncotenord, na.rm = TRUE),
    sum(Data$ses_regionlanaudiere, na.rm = TRUE),
    sum(Data$ses_regionmonteregie, na.rm = TRUE),
    sum(Data$ses_regionsaguenay, na.rm = TRUE),
    sum(Data$ses_regionquebec, na.rm = TRUE),
    sum(Data$ses_regiondehors, na.rm = TRUE),   
    sum(Data$ses_regionlaurentides, na.rm = TRUE),
    sum(Data$ses_regionmtl, na.rm = TRUE),   
    sum(Data$ses_regioncentreduqc, na.rm = TRUE),
    sum(Data$ses_regionestrie, na.rm = TRUE),   
    sum(Data$ses_regionlaval, na.rm = TRUE), 
    sum(Data$ses_regionnordduqc, na.rm = TRUE)))   


ggplot(region_counts, aes(x=Region_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,180), breaks=seq(0, 180, by=20)) +
  labs(title="Nombre de répondants par région", x="Région", y="Nombre de répondants") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))        
#_______________________________________________________________________________

# ses_education

education_counts <- data.frame(
  Education_Group = c("Secondaire sans diplôme de secondaire V", "Universitaire maîtrise",
                      "Autre formation -spécifier-", "Primaire", "Universitaire baccalauréat",
                      "Collégial -préciser-", "Secondaire avec diplôme de secondaire V", "Universitaire doctorat"),
  Count = c(
    sum(Data$ses_educationsans5, na.rm = TRUE),
    sum(Data$ses_educationmaitrise, na.rm = TRUE),
    sum(Data$ses_educationautre, na.rm = TRUE),
    sum(Data$ses_educationprimaire, na.rm = TRUE),
    sum(Data$ses_educationbacc, na.rm = TRUE),
    sum(Data$ses_educationcollegial, na.rm = TRUE),
    sum(Data$ses_educationavec5, na.rm = TRUE),
    sum(Data$ses_educationphd, na.rm = TRUE)))


ggplot(education_counts, aes(x=Education_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,350), breaks=seq(0, 350, by=50)) +
  labs(title="Nombre de répondants par niveau d'éducation", x="niveau d'éducation", y="Nombre de répondants") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
#_______________________________________________________________________________

# ses_job

job_counts <- data.frame(
  Job_Group = c("Sans emploi", "Employé à temps partiel", "Autre -spécifier-", 
                "Travailleur autonome", "Employé à temps plein", "Étudiant"),
  Count = c(
    sum(Data$ses_jobautre, na.rm = TRUE),
    sum(Data$ses_jobnojob, na.rm = TRUE),
    sum(Data$ses_jobpartiel, na.rm = TRUE),
    sum(Data$ses_jobautonome, na.rm = TRUE),
    sum(Data$ses_jobfulltime, na.rm = TRUE),
    sum(Data$ses_jobstudent, na.rm = TRUE)))


ggplot(job_counts, aes(x=Job_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,700), breaks=seq(0, 700, by=100)) +
  labs(title="Nombre de répondants par occupation", x="occupation", y="Nombre de répondants") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#_______________________________________________________________________________

# INCOME
# House income = table(Data$ses_houseincome), table(RawData$ses_income_2)
# personal income = table(Data$ses_persoincome), table(RawData$ses_income_1)

table(Data$ses_houseincome)

income_df <- Data %>%
  select(ses_houseincome, ses_persoincome) %>%
  gather(key="Income_Type", value="Value") %>%
  drop_na(Value)

ggplot(income_df, aes(x=as.factor(Value), fill=Income_Type)) +
  geom_bar(position="dodge", width=0.4) +
  scale_x_discrete(labels=c("Bas", "Moyen", "Élevé"), breaks=c(0, 0.5, 1)) +
  labs(title="Revenus personnels et par foyer", x="Catégorie de revenu", y="Nombre de répondants") +
  theme_clean() +
  scale_fill_manual(values = c("black", "gray"), name="Type de revenus", labels = c("Revenu par foyer", "Revenu personnel"))