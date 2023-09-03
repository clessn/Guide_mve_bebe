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
  clessnverse::theme_clean_light(base_size = 15) +
  geom_text(aes(y = n + 10, label = paste0("n = ", n))) +
  ylab("Nombre de répondants") +
  xlab("") +
  labs(caption = "Les 926 répondants du sondage sont représentés sur ce graphique.",
       title = "Connaissez-vous le guide MVE?")

ggsave("_SharedFolder_Guide_mve/graphs/1guide_connaitre.png",
       width = 10, height = 8)

# Avez vous consulté le guide mve -----------------------------------------

table(Data$guide_use_1)
sum(table(Data$guide_use_1))

# Guide use format --------------------------------------------------------

Graph <- Data %>% 
  group_by(guide_use_format) %>% 
  summarise(n = n())



## comparer guide_use_format_why selon papier ou web

## comparer guide_paperfrequency et guide_web_freq

## comparer guide_paper_prop et guide_web_prop

## comparer satisfaction_papier et satisfaction_papier

## guide_change pour les en ligne

## guide_papier_prac VS guide_web_prac

## guide_papier_dis VS guide_web_dis

## guide_format_change



