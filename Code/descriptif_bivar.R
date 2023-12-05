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
  clessnverse::theme_clean_light(base_size = 21) +
  geom_text(aes(y = n + 19, label = paste0("n = ", n)),
            size = 6) +
  ylab("Nombre de répondants") +
  xlab("") +
  labs(caption = "Les 926 répondants du sondage sont représentés sur ce graphique.",
       title = "Connaissez-vous le guide MVE?\n")

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
  clessnverse::theme_clean_light(base_size = 21) +
  geom_text(aes(y = n + 19, label = paste0("n = ", n)),
            size = 6) +
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



# Connait X ses_week ------------------------------------------------------

Data %>% 
  mutate(ses_week = as.numeric(ses_week),
         ses_week_group = cut(ses_week,
                              breaks = seq(0, 40, by = 5),
                              ordered_result = TRUE,
                              right = FALSE)) %>% 
  group_by(ses_week_group, guide_connaitre) %>%
  summarise(n = n()) %>%
  group_by(ses_week_group) %>% 
  mutate(n_group = sum(n),
         prop = n/n_group) %>% 
  drop_na() %>% 
  filter(guide_connaitre == 1) %>% 
  ggplot(aes(x = ses_week_group, y = prop*100)) +
  geom_bar(stat = "identity",
           fill = "#FFC300",
           width = 0.75) +
  geom_text(aes(label = paste0("n = ", n_group),
                y = prop * 100 + 1.5)) +
  clessnverse::theme_clean_light() +
  ylab("Proportion qui connait\nle guide MVE (%)") +
  xlab("Nombre de semaines de grossesse") +
  ggtitle("Connaissance du guide MVE selon l'avancement de la grossesse")

ggsave("_SharedFolder_Guide_mve/graphs/12_connaitreXweeks.png",
       width = 10, height = 6)


Data %>% 
  mutate(ses_week = as.numeric(ses_week),
         ses_week_group = cut(ses_week,
                              breaks = seq(0, 40, by = 5),
                              ordered_result = TRUE,
                              right = FALSE),
         filt_1stkid = case_when(
           filt_1stkid == 1 ~ "Premier enfant",
           filt_1stkid == 0 ~ "Pas le premier enfant"
         )) %>% 
  group_by(ses_week_group, filt_1stkid, guide_connaitre) %>%
  summarise(n = n()) %>%
  group_by(ses_week_group, filt_1stkid) %>% 
  mutate(n_group = sum(n),
         prop = n/n_group) %>% 
  drop_na() %>% 
  filter(guide_connaitre == 1) %>% 
  ggplot(aes(x = ses_week_group, y = prop*100)) +
  geom_bar(stat = "identity",
           fill = "#FFC300",
           width = 0.6) +
  facet_wrap(~filt_1stkid,
             ncol = 1) +
  geom_text(aes(label = paste0("n = ", n_group),
                y = prop * 100 + 2.5)) +
  clessnverse::theme_clean_light() +
  ylab("Proportion qui connait\nle guide MVE (%)") +
  xlab("Nombre de semaines de grossesse") +
  ggtitle("Connaissance du guide MVE selon l'avancement de la grossesse") +
  theme(axis.title.y = element_text(hjust = 0.5, size = 12),
        strip.text.x = element_text(size = 18))

ggsave("_SharedFolder_Guide_mve/graphs/12_connaitreXweeksX1stkid.png",
       width = 8, height = 10)


# n years immi X connaissance  --------------------------------------------

Data$ses_immigrant_year2[Data$ses_immigrant_year >= 1 & Data$ses_immigrant_year <= 5] <- "1-5" 
Data$ses_immigrant_year2[Data$ses_immigrant_year == 6] <- "6-10"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 7] <- "6-10"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 8] <- "6-10"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 9] <- "6-10"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 10] <- "6-10"
Data$ses_immigrant_year2[Data$ses_immigrant_year >= 11 & Data$ses_immigrant_year <= 15] <- "11-15" 
Data$ses_immigrant_year2[Data$ses_immigrant_year >= 16 & Data$ses_immigrant_year <= 20] <- "16-20" 
Data$ses_immigrant_year2[Data$ses_immigrant_year >= 21 & Data$ses_immigrant_year <= 25] <- "21-25" 
Data$ses_immigrant_year2[Data$ses_immigrant_year >= 26 & Data$ses_immigrant_year <= 30] <- "26-30"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 33] <- "30+"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 34] <- "30+"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 35] <- "30+"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 38] <- "30+"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 41] <- "30+"
Data$ses_immigrant_year2[Data$ses_immigrant_year == 42] <- "30+"

table(Data$ses_immigrant_year2)

Graph3 <- Data %>% 
  group_by(ses_immigrant_year2, guide_connaitre) %>% 
  summarise(n = n()) %>% 
  group_by(ses_immigrant_year2) %>% 
  mutate(total = sum(n),
         prop = n/total*100,
         guide_connaitre = factor(guide_connaitre, levels = c("0", "1"))) %>% 
  ungroup() %>% 
  mutate(prop = ifelse(guide_connaitre == 1, prop, NA)) %>% 
  drop_na(ses_immigrant_year2)

ggplot(Graph3, aes(x = ses_immigrant_year2, y = n)) +
  geom_bar(aes(alpha = guide_connaitre),
           stat = "identity",
           fill = "#FFC300") +
  scale_x_discrete(limits=c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+")) +
  clessnverse::theme_clean_light() +
  scale_alpha_discrete(labels = c("0" = "Ne connait pas le guide",
                                  "1" = "Connait le guide"),
                       range = c(0.2, 1)) +
  labs(title="Connaissance du guide selon le nombre\nd'années depuis l'arrivée au Canada",
       x="Nombre d'années",
       y="Nombre de répondants") +
  scale_y_continuous(limits=c(0,30), breaks=seq(0, 30, by=5)) +
  geom_text(data = Graph3 %>% drop_na(prop),
            aes(y = total + 1, label = paste0(round(prop), "%")))

ggsave("_SharedFolder_Guide_mve/graphs/ses_immigrant_yearXconnaissance.png", 
       width = 10, height = 8)

Data %>% 
  group_by(guide_connaitre, ses_immigrant_year2) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(guide_connaitre) %>% 
  mutate(total = sum(n),
         prop = n/total*100,
         guide_connaitre = factor(guide_connaitre, levels = c("0", "1")))
