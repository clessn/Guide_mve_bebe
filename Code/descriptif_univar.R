# Packages ----------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)


# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

table(Data$info)
RawData <- read.csv("_SharedFolder_Guide_mve/data/INSPQ-guide-mve_RAW.csv")


# SES -------------------------------------------------------------------

# AGE (aller corriger la variable Data$ses_age50 dans cleaning)
#Data$ses_age1829, Data$ses_age3039, Data$ses_age4049, Data$ses_age50

table(Data$ses_age3039)

# Reshape the data
age_counts <- data.frame(
  Age_Group = c("18-29", "30-39", "40-49", "50+"),
  Count = c(sum(Data$ses_age1829, na.rm = TRUE),
            sum(Data$ses_age3039, na.rm = TRUE),
            sum(Data$ses_age4049, na.rm = TRUE),
            sum(Data$ses_age50, na.rm = TRUE))
)

# Plot bar chart
ggplot(age_counts, aes(x=Age_Group, y=Count)) +
  geom_bar(stat="identity", fill="black", width=0.4) +
  scale_y_continuous(limits=c(0,600), breaks=seq(0, 600, by=200)) +
  labs(title="Nombre de répondants par groupe d'âge", x="Groupes d'âge", y="Nombre de répondants") +
  theme_clean()


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

table(Data$ses_houseincome)

table(Data$ses)

# LANGUAGE
# pas clean


# INCOME
# House income = table(Data$ses_houseincome), table(RawData$ses_income_2)
# personal income = table(Data$ses_persoincome), table(RawData$ses_income_1)

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
