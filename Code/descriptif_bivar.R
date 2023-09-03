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

table(Data$guide_use_format, useNA = "always")
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

ggsave("_SharedFolder_Guide_mve/graphs/3guide_format.png",
       width = 10, height = 8)


## Stacked -----------------------------------------------------------------

Graph2 <- Graph %>% 
  mutate(papierweb = ifelse(guide_use_format %in% c("Uniquement papier",
                                                    "Principalement papier"), "Papier", "Deux formats également"),
         papierweb = ifelse(guide_use_format %in% c("Uniquement web",
                                                    "Principalement web"), "Web", papierweb),
         papierweb = factor(papierweb, levels = c("Papier", "Deux formats également", "Web")),
         intensity = ifelse(guide_use_format %in% c("Uniquement papier",
                                                    "Uniquement web"), "Uniquement", NA),
         intensity = ifelse(guide_use_format %in% c("Principalement papier",
                                                    "Principalement web"), "Principalement", intensity)) %>% 
  group_by(papierweb) %>% 
  mutate(nformat = sum(n))

ggplot(Graph2, aes(x = papierweb, y = n)) +
  geom_bar(stat = "identity",
           aes(fill = papierweb,
               alpha = intensity),
           show.legend = T,
           color = NA) +
  scale_fill_manual(
    values = c(
      "Papier" = "#FFA07A",
      "Deux formats également" = "#d7d8d5",
      "Web" = "#6699CC"
    )
  ) +
  scale_alpha_manual(values = c("Uniquement" = 1,
                                "Principalement" = 0.4),
                     name = "") +
  guides(fill = FALSE) +
  clessnverse::theme_clean_light(base_size = 15) +
  geom_text(aes(y = nformat + 10, label = paste0("n = ", nformat))) +
  ylab("Nombre de répondants") +
  xlab("") +
  labs(caption = "715 des 716 répondants qui ont consulté le guide MVE sont représentés sur ce graphique.\nUn répondant n'a pas répondu à cette question.",
       title = "Format utilisé pour consulter le guide MVE") +
  theme(axis.text.x = element_text())

ggsave("_SharedFolder_Guide_mve/graphs/3guide_format2.png",
       width = 10, height = 8)


# Frequency -------------------------------------------------------------------------

table(Data$guide_paperfrequency, Data$guide_use_format,
      useNA = "always")
table(Data$guide_web_freq, Data$guide_use_format,
      useNA = "always")

Data %>% 
  pivot_longer(cols = c(guide_paperfrequency, guide_web_freq)) %>%
  mutate(value = factor(value, labels = c("0" = "Jamais",
                                          "0.25" = "Rarement",
                                          "0.5" = "Quelques fois",
                                          "0.75" = "Souvent",
                                          "1" = "Très souvent"))) %>% 
  group_by(name, value, .drop = FALSE) %>%
  summarise(n = n()) %>%
  drop_na() %>% 
  group_by(name) %>% 
  mutate(n_format = sum(n),
         prop = n/n_format) %>% 
  ggplot(aes(x = value, y = prop*100)) +
  geom_bar(stat = "identity",
           aes(fill = name,
               group = name),
           position = position_dodge2()) +
  geom_text(aes(y = prop*100 + 1.5,
                  label = paste0("n = ", n),
                group = name),
            position = position_dodge(width = 0.9)) +
  clessnverse::theme_clean_light() +
  scale_fill_manual(values = 
                      c(
                        "guide_paperfrequency" = "#FFA07A",
                        "guide_web_freq" = "#6699CC"
                      ),
                    labels = 
                      c(
                        "guide_paperfrequency" = "Papier",
                        "guide_web_freq" = "Web"
                      )) +
  ylab("Proportion des répondants par format consulté (%)") +
  xlab("") +
  labs(caption = "712 des 715 répondants qui ont indiqué leur format d'utilisation sont sur ce graphique.\nTrois répondants n'ont pas répondu à cette question.",
       title = "Fréquence de consultation selon le format utilisé")

ggsave("_SharedFolder_Guide_mve/graphs/5freq.png",
       width = 10, height = 6)


# Who does not know the guide? --------------------------------------------

table(Data$guide_connaitre)

# Wrangling ---------------------------------------------------------------

## age
Data$ses_age <- 1 * Data$ses_age1829 + 2 * Data$ses_age3039 + 
  3*Data$ses_age4049
Data$ses_age <- case_when(
  Data$ses_age == 1 ~ "18_29",
  Data$ses_age == 2 ~ "30_39",
  Data$ses_age == 3 ~ "40_49"
)
Data$ses_age <- factor(Data$ses_age, ordered = TRUE)
unique(Data$ses_age)

