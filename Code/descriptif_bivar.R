# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

# Connaissez vous le guide mve --------------------------------------------

table(Data$guide_connaitre)

Data %>% 
  group_by(guide_connaitre) %>% 
  summarise(n = n()) %>% 
  mutate(guide_connaitre = ifelse(is.na(guide_connaitre), "NA", guide_connaitre)) %>% 
  ggplot(aes(x = guide_connaitre, y = n)) +
  geom_bar(stat = "identity",
           aes(fill = guide_connaitre),
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
  labs(caption = "Les 926 répondants du sondage sont représentés sur ce graphique.",
       title = "Connaissez-vous le guide MVE?")

ggsave("_SharedFolder_Guide_mve/graphs/1guide_connaitre.png",
       width = 10, height = 8)

# Avez vous consulté le guide mve -----------------------------------------

table(Data$guide_use_1)
table(Data$guide_use_1, Data$guide_connaitre)
sum(table(Data$guide_use_1))

Graph <- Data %>% 
  filter(guide_connaitre==1) %>% 
  group_by(guide_use_1) %>% 
  summarise(n = n()) %>% 
  mutate(guide_use_1 = ifelse(is.na(guide_use_1), "NA", guide_use_1))

ggplot(Graph, aes(x = guide_use_1, y = n)) +
  geom_bar(stat = "identity",
           aes(fill = guide_use_1),
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
  labs(caption = paste0("Les ", sum(Graph$n), " répondants qui connaissent le guide MVE sont représentés sur ce graphique."),
       title = "Avez-vous consulté le guide MVE?")

ggsave("_SharedFolder_Guide_mve/graphs/2guide_consulte.png",
       width = 10, height = 8)

# Guide use format --------------------------------------------------------

table(Data$guide_use_format)
table(Data$guide_use_format, Data$guide_use_1,
      useNA = "always")
table <- table(Data$guide_use_format, Data$guide_use_1, Data$guide_connaitre,
      useNA = "always")
table
sum(table[1:5,2,2])

Graph <- Data %>% 
  filter(guide_use_1 == 1) %>% 
  group_by(guide_use_format) %>% 
  summarise(n = n()) %>%
  drop_na(guide_use_format) %>% 
  mutate(guide_use_format = factor(guide_use_format,
                                   levels = c("0", "0.25",
                                              "0.5", "0.75", "1"),
                                   labels = c("Uniquement papier",
                                              "Principalement papier",
                                              "Deux formats également",
                                              "Principalement web",
                                              "Uniquement web")))

ggplot(Graph, aes(x = guide_use_format, y = n)) +
  geom_bar(stat = "identity",
           aes(fill = guide_use_format),
           show.legend = F,
           color = NA) +
  scale_fill_manual(
    values = c(
      "Uniquement papier" = "#FFA07A",
      "Principalement papier" = "#FFDAB9",
      "Deux formats également" = "#d7d8d5",
      "Principalement web" = "#AED6F1",
      "Uniquement web" = "#6699CC"
    )
  ) +
  clessnverse::theme_clean_light(base_size = 15) +
  geom_text(aes(y = n + 10, label = paste0("n = ", n))) +
  ylab("Nombre de répondants") +
  xlab("") +
  labs(caption = "715 des 716 répondants qui ont consulté le guide MVE sont représentés sur ce graphique.\nUn répondant n'a pas répondu à cette question.",
       title = "Format utilisé pour consulter le guide MVE") +
  theme(axis.text.x = element_text(angle = 15))

### Un répondant n'a pas répondu
ggsave("_SharedFolder_Guide_mve/graphs/3guide_format.png",
       width = 10, height = 8)



## comparer guide_paperfrequency et guide_web_freq

## comparer guide_paper_prop et guide_web_prop

## comparer satisfaction_papier et satisfaction_papier

## guide_change pour les en ligne

## guide_papier_prac VS guide_web_prac

## guide_papier_dis VS guide_web_dis

## guide_format_change



