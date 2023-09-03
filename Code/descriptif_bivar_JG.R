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
  } else if(value == 0.50) {
    return("50% (moitié)")
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

category_order <- c("0% (rien)", "1-25% (quelques pages)", "25-50%", "50% (moitié)", "50-75%", "75-99%", "100% (complet)")


# Utilisation directe des données et transformation en format long
data_long <- Data %>%
  select(guide_paper_prop, guide_web_prop) %>%
  gather(Format, Proportion, everything()) %>%
  filter(!is.na(Proportion)) %>%
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
