# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

## guide_format_change - Le remplacement du format papier du guide « Mieux vivre avec notre enfant » par le format web serait-il avantageux pour vous?

table(Data$guide_format_change)

Data %>% 
  group_by(guide_format_change) %>% 
  summarise(n = n()) %>% 
  mutate(guide_format_change = ifelse(is.na(guide_format_change), "NA", guide_format_change)) %>% 
  ggplot(aes(x = guide_format_change, y = n)) +
  geom_bar(stat = "identity",
           aes(fill = guide_format_change),
           show.legend = F,
           color = NA) +
  scale_fill_manual(values = c("0" = "#FF6B6B",
                               "1" = "#95E08E",
                               "NA" = "grey")) +
  scale_x_discrete(labels = c("0" = "Non",
                              "1" = "Oui",
                              "NA" = "N'a pas répondu")) +
  scale_y_continuous(limits = c(0,950)) +
  clessnverse::theme_clean_light(base_size = 15) +
  geom_text(aes(y = n + 15, label = paste0("n = ", n))) +
  ylab("Nombre de répondants") +
  xlab("") +
  labs(caption = "Les 929 répondants du sondage sont représentés sur ce graphique.",
       title = "Le remplacement du format papier du guide\n« Mieux vivre avec notre enfant » par le format web\nserait-il avantageux pour vous?")

ggsave("_SharedFolder_Guide_mve/graphs/11guide_format_change.png",
       width = 10, height = 8)

## Quelle proportion du guide « Mieux vivre avec notre enfant » avez-vous lu?

table(Data$guide_paper_prop)
table(Data$guide_web_prop)

map_to_category <- function(value) {
  if(value == 1.00) {
    return("100% (complet)")
  } else if(0.75 <= value && value < 1.00) {
    return("75-99%")
  } else if(0.50 <= value && value < 0.75) {
    return("50-75%")
  } else if(0.25 <= value && value < 0.50) {
    return("25-50%")
  } else if(0.01 <= value && value < 0.25) {
    return("1-25% (quelques pages)")
  } else if(value == 0.00) {
    return("0% (rien)")
  } else {
    return("Autre")
  }
}

category_order <- c("0% (rien)", "1-25% (quelques pages)", "25-50%", "50-75%", "75-99%", "100% (complet)")

# Utilisation directe des données et transformation en format long
data_long <- Data %>%
  select(guide_paper_prop, guide_web_prop) %>%
  gather(Format, Proportion, everything()) %>%
  filter(!is.na(Proportion)) %>%
  mutate(Proportion = ifelse(Proportion == 0.50, 0.66, Proportion)) %>%
  group_by(Format, Proportion) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Total = ifelse(Format == "guide_paper_prop", sum(Frequency[Format == "guide_paper_prop"]), 
                        sum(Frequency[Format == "guide_web_prop"]))) %>%
  mutate(Proportion_Percent = Frequency / Total * 100) %>%
  mutate(Category = sapply(Proportion, map_to_category)) %>% 
  mutate(Category = factor(Category, levels = category_order)) 

# Création du graphique
ggplot(data_long, aes(x = Category, y = Proportion_Percent, fill = Format)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Proportion lue du guide selon le format",
    x = "Proportion lue",
    y = "Proportion des répondants par format consulté"
  ) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/6guide_prop.png", width = 10, height = 8)

## density

data_density_read <- Data %>%
  select(guide_paper_prop, guide_web_prop) %>%
  gather(Format, Proportion_Read, everything()) %>%
  filter(!is.na(Proportion_Read))

# Création du graphique à crêtes
ggplot(data_density_read, aes(x = Proportion_Read, y = factor(Format, levels = c("guide_web_prop", "guide_paper_prop"), labels = c("Web", "Papier")), fill = Format, color = Format)) +
  ggridges::geom_density_ridges(
    scale = 0.5,
    rel_min_height = 0.01,
    alpha = 0.6,
    quantile_lines = TRUE
  ) +
  labs(
    title = "Distribution de la proportion lue du guide selon le format",
    x = "Proportion lue",
    y = ""
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent(seq(0, 1, by = 0.1))) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  scale_color_manual(values = c("#FFA07A", "#6699CC"), 
                     labels = c("Format papier", "Format web")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_Guide_mve/graphs/6guide_prop_density.png", 
       width = 10, height = 8)

## Satisfaction du guide -------------------------------------------------------


Data$guide_satisfaction_web <- ifelse(!is.na(Data$guide_satisfaction_2webpdf), 
                                      Data$guide_satisfaction_2webpdf, 
                                      Data$guide_satisfaction_3webhtml)

# Utilisation directe des données et transformation en format long
data_satisfaction <- drop_na(Data %>%
  select(guide_satisfaction_1paper, guide_satisfaction_web) %>%
  gather(Format, Satisfaction, everything()) %>%
  filter(!is.na(Satisfaction)) %>%
  group_by(Format, Satisfaction) %>%
  summarize(Frequency = n()) %>%
  ungroup() %>%
  mutate(Total = ifelse(Format == "guide_satisfaction_1paper", sum(Frequency[Format == "guide_satisfaction_1paper"]), 
                        sum(Frequency[Format == "guide_satisfaction_web"]))) %>%
  mutate(Proportion_Percent = Frequency / Total * 100) %>% 
  mutate(Satisfaction = factor(Satisfaction, levels = 0:10)))

# Création du graphique
ggplot(data_satisfaction, aes(x = Satisfaction, y = Proportion_Percent, fill = Format)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Satisfaction des répondants en fonction du format consulté",
    x = "Niveau de satisfaction",
    y = "Proportion des répondants par format consulté"
  ) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/7guide_satisfaction.png", width = 10, height = 8)

## ridges

# Utilisation directe des données et transformation en format long
data_density <- Data %>%
  select(guide_satisfaction_1paper, guide_satisfaction_web) %>%
  gather(Format, Satisfaction, everything()) %>%
  mutate(Format = ifelse(Format == "guide_satisfaction_1paper", "Paper", "Web"))

# Création du graphique à crêtes avec affichage des lignes de quartile
ggplot(data_density, aes(x = as.numeric(Satisfaction), y = factor(Format, levels = c("Web", "Paper"), labels = c("Web", "Papier")), fill = Format, color = Format)) +
  ggridges::geom_density_ridges(
    scale = 0.5,
    rel_min_height = 0.01,
    alpha = 0.6,
    quantile_lines = TRUE,
    trim = TRUE
  ) +
  labs(
    title = "Distribution de la satisfaction des répondants selon le format",
    x = "Niveau de satisfaction",
    y = ""
  ) +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  scale_color_manual(values = c("#FFA07A", "#6699CC"), 
                     labels = c("Format papier", "Format web")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_Guide_mve/graphs/7guide_satisfaction'density.png", 
       width = 10, height = 8)
## pratique dans le guide ------------------------------------------------------

# 1. Transformation des données en format long
data_practical <- Data %>%
  select(starts_with("guide_paper_prac"), starts_with("guide_pdf_prac"), starts_with("guide_html_prac")) %>%
  gather(Format_Practicality, Value, everything())

# 2. Définissez le format et la praticité
data_practical$Format <- case_when(
  grepl("guide_paper", data_practical$Format_Practicality) ~ "Paper",
  grepl("guide_pdf|guide_html", data_practical$Format_Practicality) ~ "Web"
)

data_practical$Practicality <- case_when(
  grepl("pracuse", data_practical$Format_Practicality) ~ "Facile à consulter",
  grepl("practransport", data_practical$Format_Practicality) ~ "Facile à transporter",
  grepl("pracacess", data_practical$Format_Practicality) ~ "Accessible en tout temps"
)

# 3. Filtrage
data_practical_filtered <- data_practical %>%
  filter(Value == 1)

# 4. Calcul des fréquences
data_summary_filtered <- data_practical_filtered %>%
  group_by(Format, Practicality) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  group_by(Format) %>%
  mutate(Total = sum(Frequency)) %>%
  ungroup() %>%
  mutate(Proportion_Percent = Frequency / Total * 100)

# 5. Création du graphique
ggplot(data_summary_filtered, aes(x = Practicality, y = Proportion_Percent, fill = Format)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Avantages par format",
    x = "",
    y = "Proportion des répondants par format"
  ) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/9guide_pratique.png", width = 10, height = 8)

## désavantages du guide -------------------------------------------------------

data_disadvantages <- Data %>%
  select(starts_with("guide_paper_dis"), starts_with("guide_pdf_dis"), starts_with("guide_html_dis")) %>%
  gather(Format_Disadvantage, Value, everything())

# 2. Définissez le format et le désavantage
data_disadvantages$Format <- case_when(
  grepl("guide_paper", data_disadvantages$Format_Disadvantage) ~ "Paper",
  grepl("guide_pdf|guide_html", data_disadvantages$Format_Disadvantage) ~ "Web"
)

data_disadvantages$Disadvantage <- case_when(
  grepl("disuse", data_disadvantages$Format_Disadvantage) ~ "Difficile à utiliser",
  grepl("distransport", data_disadvantages$Format_Disadvantage) ~ "Difficile à transporter",
  grepl("disacess", data_disadvantages$Format_Disadvantage) ~ "Accès difficile"
)

# 3. Filtrage pour les désavantages
data_disadvantages_filtered <- data_disadvantages %>%
  filter(Value == 1)

# 4. Calcul des fréquences pour les désavantages
data_summary_disadvantages <- data_disadvantages_filtered %>%
  group_by(Format, Disadvantage) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  group_by(Format) %>%
  mutate(Total = sum(Frequency)) %>%
  ungroup() %>%
  mutate(Proportion_Percent = Frequency / Total * 100)

# 5. Création du graphique pour les désavantages
ggplot(data_summary_disadvantages, aes(x = Disadvantage, y = Proportion_Percent, fill = Format)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Désavantages par format",
    x = "",
    y = "Proportion des répondants par format"
  ) +
  scale_fill_manual(values = c("#FFA07A", "#6699CC"), 
                    labels = c("Format papier", "Format web")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/10guide_desavantages.png", 
       width = 10, height = 8)

## Connaissance vs SES ---------------------------------------------------------


## Age
table(Data$guide_connaitre)

data_age_filtered <- Data %>%
  filter(ses_age1829 == 1 | ses_age3039 == 1 | ses_age4049 == 1 | ses_age50 == 1)

# Transforming the data to a long format
data_age_long <- data_age_filtered %>%
  select(guide_connaitre, ses_age1829, ses_age3039, ses_age4049, ses_age50) %>%
  gather("Age_Group", "Value", -guide_connaitre) %>%
  filter(Value == 1)

data_age_summary <- data_age_long %>%
  group_by(Age_Group, guide_connaitre) %>%
  summarise(Frequency = n()) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Proportion = Frequency / Total * 100) %>% 
  filter(Frequency != 1)

data_age_summary$Age_Group <- recode(data_age_summary$Age_Group, 
                                     "ses_age1829" = "18 à 29 ans",
                                     "ses_age3039" = "30 à 39 ans",
                                     "ses_age4049" = "40 à 49 ans",
                                     "ses_age50" = "50 ans et plus")
# Création du graphique à barres
ggplot(data_age_summary, aes(x = Age_Group, y = Proportion, fill = as.factor(guide_connaitre))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), size = 6,
            vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Connaissance du guide par tranche d'âge",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Connaît le guide"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("Ne connaît pas le guide", "Connaît le guide")) +
  clessnverse::theme_clean_light(base_size = 16) +
  theme(axis.text.x = element_text(hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xconnaitre_age.png", 
       width = 10, height = 8)

## Région

data_region_filtered <- Data %>%
  filter(ses_regionabitibi == 1 | ses_regionchaudiere == 1 | ses_regioniles == 1
         | ses_regionmauricie == 1 | ses_regionoutaouais == 1 | ses_regionbasstlaur == 1
         | ses_regioncotenord == 1 | ses_regionlanaudiere == 1 | ses_regionmonteregie == 1
         | ses_regionsaguenay == 1 | ses_regionquebec == 1 | ses_regiondehors == 1
         | ses_regionlaurentides == 1 | ses_regionmtl == 1 | ses_regioncentreduqc == 1
         | ses_regionestrie == 1 | ses_regionlaval == 1 | ses_regionnordduqc == 1)

# Transforming the data to a long format
data_region_long <- data_region_filtered %>%
  select(guide_connaitre, starts_with("ses_region")) %>%
  gather("region_Group", "Value", -guide_connaitre) %>%
  filter(Value == 1)

data_region_summary <- data_region_long %>%
  group_by(region_Group, guide_connaitre) %>%
  summarise(Frequency = n()) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Proportion = Frequency / Total * 100) %>% 
  filter(Frequency != 1)

data_region_summary <- rbind(data_region_summary, 
                             data.frame(region_Group = "Côte-Nord", 
                                        guide_connaitre = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))


data_region_summary <- rbind(data_region_summary, 
                             data.frame(region_Group = "Gaspésie-Îles-de-la-Madeleine", 
                                        guide_connaitre = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))

data_region_summary <- rbind(data_region_summary, 
                             data.frame(region_Group = "Nord-du-Québec", 
                                        guide_connaitre = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))


data_region_summary$region_Group <- recode(data_region_summary$region_Group, 
                                     "ses_regionabitibi" = "Abitibi",
                                     "ses_regionchaudiere" = "Chaudière-Appalaches",
                                     "ses_regioniles" = "Gaspésie-Îles-de-la-Madeleine",
                                     "ses_regionmauricie" = "Mauricie", 
                                     "ses_regionoutaouais" = "Outaouais", 
                                     "ses_regionbasstlaur" = "Bas-Saint-Laurent", 
                                    "ses_regioncotenord"  = "Côte-Nord", 
                                    "ses_regionlanaudiere" = "Lanaudière", 
                                    "ses_regionmonteregie" = "Montérégie", 
                                    "ses_regionsaguenay" = "Saguenay-Lac-Saint-Jean", 
                                    "ses_regionquebec" = "Capitale-Nationale", 
                                    "ses_regionlaurentides" = "Laurentides", 
                                    "ses_regionmtl" = "Montréal", 
                                    "ses_regioncentreduqc" = "Centre-du-Québec", 
                                    "ses_regionestrie" = "Estrie", 
                                    "ses_regionlaval" = "Laval", 
                                    "ses_regionnordduqc" = "Nord-du-Québec")

# 1. Calcul de la proportion moyenne de ceux qui connaissent le guide pour chaque région
order_region <- data_region_summary %>%
  filter(guide_connaitre == 1) %>% 
  arrange(Proportion) %>%
  pull(region_Group)

# 2. Réorganisation des niveaux de la variable region_Group
data_region_summary$region_Group <- factor(data_region_summary$region_Group, levels = order_region)

# 3. Création du graphique à barres
ggplot(data_region_summary, aes(x = region_Group, y = Proportion, fill = as.factor(guide_connaitre))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency),
                y = Proportion + 2),
            position = position_dodge(0.9), hjust = 0,
            angle = 90) +
  labs(
    title = "Connaissance du guide par région",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Connaît le guide"
  ) +
  scale_y_continuous(limits = c(0, 105)) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("Ne connaît pas le guide", "Connaît le guide")) +
  clessnverse::theme_clean_light(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xconnaitre_region.png", 
       width = 10, height = 8)

## Éducation

data_educ_filtered <- Data %>% 
  filter(ses_educationprimaire == 1 | ses_educationsans5 == 1 | ses_educationavec5 == 1
         | ses_educationcollegial == 1 | ses_educationbacc == 1
         | ses_educationmaitrise == 1 | ses_educationphd == 1)

data_educ_long <- data_educ_filtered %>%
  select(guide_connaitre, starts_with("ses_education")) %>%
  gather("educ_Group", "Value", -guide_connaitre) %>%
  filter(Value == 1)

data_educ_summary <- drop_na(data_educ_long %>%
  group_by(educ_Group, guide_connaitre) %>%
  summarise(Frequency = n()) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Proportion = Frequency / Total * 100)) 

education_order <- c(
  "Primaire", 
  "Secondaire non-complété", 
  "Secondaire complété",
  "Collégial",
  "Baccalauréat",
  "Maîtrise",
  "Doctorat"
)

# Recoder les niveaux d'éducation pour data_educ_summary
data_educ_summary$educ_Group <- recode(data_educ_summary$educ_Group, 
                                       "ses_educationprimaire" = "Primaire", 
                                       "ses_educationsans5" = "Secondaire non-complété", 
                                       "ses_educationavec5" = "Secondaire complété", 
                                       "ses_educationcollegial" = "Collégial", 
                                       "ses_educationbacc" = "Baccalauréat", 
                                       "ses_educationmaitrise" = "Maîtrise", 
                                       "ses_educationphd" = "Doctorat")

# Convertir la colonne 'educ_Group' de data_educ_summary en facteur avec l'ordre spécifié
data_educ_summary$educ_Group <- factor(data_educ_summary$educ_Group, levels = education_order)



ggplot(data_educ_summary, aes(x = educ_Group, y = Proportion, fill = as.factor(guide_connaitre))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Connaissance du guide par niveau d'éducation",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Connaît le guide"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("Ne connaît pas le guide", "Connaît le guide")) +
  clessnverse::theme_clean_light(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xconnaitre_educ.png", 
       width = 10, height = 8)

## Utilisation vs SES ----------------------------------------------------------
## Age
table(Data$guide_use_1)

data_age_filtered <- Data %>%
  filter(ses_age1829 == 1 | ses_age3039 == 1 | ses_age4049 == 1 | ses_age50 == 1)

# Transforming the data to a long format
data_age_long1 <- data_age_filtered %>%
  select(guide_use_1, ses_age1829, ses_age3039, ses_age4049, ses_age50) %>%
  gather("Age_Group", "Value", -guide_use_1) %>%
  filter(Value == 1)

data_age_summary1 <- drop_na(data_age_long1 %>%
  group_by(Age_Group, guide_use_1) %>%
  summarise(Frequency = n()) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Proportion = Frequency / Total * 100))

data_age_summary1$Age_Group <- recode(data_age_summary1$Age_Group, 
                                     "ses_age1829" = "18 à 29 ans",
                                     "ses_age3039" = "30 à 39 ans",
                                     "ses_age4049" = "40 à 49 ans",
                                     "ses_age50" = "50 ans et plus")
# Création du graphique à barres
ggplot(data_age_summary1, aes(x = Age_Group, y = Proportion, fill = as.factor(guide_use_1))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Utilisation du guide par tranche d'âge",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Utilise le guide"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("N'utilise pas le guide", "Utilise le guide")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xutilise_age.png", 
       width = 10, height = 8)

## region

## Région

data_region_filtered <- Data %>%
  filter(ses_regionabitibi == 1 | ses_regionchaudiere == 1 | ses_regioniles == 1
         | ses_regionmauricie == 1 | ses_regionoutaouais == 1 | ses_regionbasstlaur == 1
         | ses_regioncotenord == 1 | ses_regionlanaudiere == 1 | ses_regionmonteregie == 1
         | ses_regionsaguenay == 1 | ses_regionquebec == 1 | ses_regiondehors == 1
         | ses_regionlaurentides == 1 | ses_regionmtl == 1 | ses_regioncentreduqc == 1
         | ses_regionestrie == 1 | ses_regionlaval == 1 | ses_regionnordduqc == 1)

# Transforming the data to a long format
data_region_long1 <- data_region_filtered %>%
  select(guide_use_1, starts_with("ses_region")) %>%
  gather("region_Group", "Value", -guide_use_1) %>%
  filter(Value == 1)

data_region_summary1 <- drop_na(data_region_long1 %>%
  group_by(region_Group, guide_use_1) %>%
  summarise(Frequency = n()) %>%
  mutate(Total = sum(Frequency)) %>%
  mutate(Proportion = Frequency / Total * 100))

data_region_summary1 <- rbind(data_region_summary1, 
                             data.frame(region_Group = "Côte-Nord", 
                                        guide_use_1 = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))


data_region_summary1 <- rbind(data_region_summary1, 
                             data.frame(region_Group = "Gaspésie-Îles-de-la-Madeleine", 
                                        guide_use_1 = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))

data_region_summary1 <- rbind(data_region_summary1, 
                             data.frame(region_Group = "Nord-du-Québec", 
                                        guide_use_1 = 0, 
                                        Frequency = 0, 
                                        Total = 0, 
                                        Proportion = 0))


data_region_summary1$region_Group <- recode(data_region_summary1$region_Group, 
                                           "ses_regionabitibi" = "Abitibi-Témiscamingue",
                                           "ses_regionchaudiere" = "Chaudière-Appalaches",
                                           "ses_regioniles" = "Gaspésie-Îles-de-la-Madeleine",
                                           "ses_regionmauricie" = "Mauricie", 
                                           "ses_regionoutaouais" = "Outaouais", 
                                           "ses_regionbasstlaur" = "Bas-Saint-Laurent", 
                                           "ses_regioncotenord"  = "Côte-Nord", 
                                           "ses_regionlanaudiere" = "Lanaudière", 
                                           "ses_regionmonteregie" = "Montérégie", 
                                           "ses_regionsaguenay" = "Saguenay-Lac-Saint-Jean", 
                                           "ses_regionquebec" = "Capitale-Nationale", 
                                           "ses_regionlaurentides" = "Laurentides", 
                                           "ses_regionmtl" = "Montréal", 
                                           "ses_regioncentreduqc" = "Centre-du-Québec", 
                                           "ses_regionestrie" = "Estrie", 
                                           "ses_regionlaval" = "Laval", 
                                           "ses_regionnordduqc" = "Nord-du-Québec")

# 1. Calcul de la proportion moyenne de ceux qui connaissent le guide pour chaque région
order_region1 <- data_region_summary1 %>%
  filter(guide_use_1 == 1) %>% 
  arrange(Proportion) %>%
  pull(region_Group)

# 2. Réorganisation des niveaux de la variable region_Group
data_region_summary1$region_Group <- factor(data_region_summary1$region_Group, levels = order_region1)

# 3. Création du graphique à barres
ggplot(data_region_summary1, aes(x = region_Group, y = Proportion, fill = as.factor(guide_use_1))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Utilisation du guide par région",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Utilise le guide"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("N'utilise pas le guide", "Utilise le guide")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xutilise_region.png", 
       width = 10, height = 8)

## education

data_educ_filtered <- Data %>% 
  filter(ses_educationprimaire == 1 | ses_educationsans5 == 1 | ses_educationavec5 == 1
         | ses_educationcollegial == 1 | ses_educationbacc == 1
         | ses_educationmaitrise == 1 | ses_educationphd == 1)

data_educ_long1 <- data_educ_filtered %>%
  select(guide_use_1, starts_with("ses_education")) %>%
  gather("educ_Group", "Value", -guide_use_1) %>%
  filter(Value == 1)

data_educ_summary1 <- drop_na(data_educ_long1 %>%
                               group_by(educ_Group, guide_use_1) %>%
                               summarise(Frequency = n()) %>%
                               mutate(Total = sum(Frequency)) %>%
                               mutate(Proportion = Frequency / Total * 100)) 

data_educ_summary1 <- rbind(data_educ_summary1, 
                              data.frame(educ_Group = "Doctorat", 
                                         guide_use_1 = 0, 
                                         Frequency = 0, 
                                         Total = 0, 
                                         Proportion = 0))

education_order <- c(
  "Primaire", 
  "Secondaire non-complété", 
  "Secondaire complété",
  "Collégial",
  "Baccalauréat",
  "Maîtrise",
  "Doctorat"
)

# Recoder les niveaux d'éducation pour data_educ_summary
data_educ_summary1$educ_Group <- recode(data_educ_summary1$educ_Group, 
                                       "ses_educationprimaire" = "Primaire", 
                                       "ses_educationsans5" = "Secondaire non-complété", 
                                       "ses_educationavec5" = "Secondaire complété", 
                                       "ses_educationcollegial" = "Collégial", 
                                       "ses_educationbacc" = "Baccalauréat", 
                                       "ses_educationmaitrise" = "Maîtrise", 
                                       "ses_educationphd" = "Doctorat")

# Convertir la colonne 'educ_Group' de data_educ_summary en facteur avec l'ordre spécifié
data_educ_summary1$educ_Group <- factor(data_educ_summary1$educ_Group, levels = education_order)



ggplot(data_educ_summary1, aes(x = educ_Group, y = Proportion, fill = as.factor(guide_use_1))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("n = ", Frequency)), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = "Utilisation du guide par niveau d'éducation",
    x = "",
    y = "Proportion des répondants (%)",
    fill = "Utilise le guide"
  ) +
  scale_fill_manual(values = c("#FF6B6B", "#95E08E"), 
                    labels = c("N'utilise pas le guide", "Utilise le guide")) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("_SharedFolder_Guide_mve/graphs/Xutilise_educ.png", 
       width = 10, height = 8)
