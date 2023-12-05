# Packages -----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean_2023-12-05.rds")

## ses_houseincome2
table(Data$ses_houseincome)

## ses_age
table(Data$ses_age)

## guide_format_ess_papier, html, pdf
table(Data$guide_format_ess_papier)
table(Data$guide_format_ess_html)
table(Data$guide_format_ess_pdf)


# Univarié ----------------------------------------------------------------

Graph1 <- Data %>% 
  pivot_longer(., cols = starts_with("guide_format_ess"),
               names_to = "format",
               names_prefix = "guide_format_ess_") %>% 
  mutate(value2 = case_when(
    value == "all_other" ~ 0,
    value == "paper_and_web" ~ 1,
    value == "mostly" ~ 2,
    value == "only" ~ 3
  ))

Graph1 %>% 
  drop_na(value) %>% 
  mutate(format = as.character(format),
         format = ifelse(value == "paper_and_web", "", format),
         value = ifelse(value == "paper_and_web", "Papier et\nweb également", value),
         format = case_when(
           format == "" ~ "",
           format == "html" ~ "HTML",
           format == "papier" ~ "Papier",
           format == "pdf" ~ "PDF"
         ),
         format = factor(format, levels = c("", "Papier", "HTML", "PDF"))) %>% 
  group_by(format, value) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = value, y = n)) +
  facet_grid(cols = vars(format),
             scales = "free_x",
             space = "free",
             switch = "x") +
  geom_bar(stat = "identity",
           aes(fill = value), color = NA,
           show.legend = FALSE) +
  scale_fill_manual(values = c("Papier et\nweb également" = "#7D7D7D",  # Gris
                               "1" = "#C34A36",  # Rouge
                               "3" = "#E1C340",  # Jaune
                               "4" = "#4DAF7C")) +
  scale_x_discrete(labels = c("1" = "Autre format", "3" = "Principalement", "4" = "Uniquement")) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("\nNombre de répondants\n") +
  labs(title = "Quel format du guide « Mieux vivre avec notre enfant »\n(papier ou web) est plus essentielle pour vous?") +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

ggsave("_SharedFolder_Guide_mve/graphs/apres_vente/univarie.png",
       width = 9, height = 7)

# essentiel X income ------------------------------------------------------

Graph2 <- Data %>% 
  pivot_longer(., cols = starts_with("guide_format_ess"),
               names_to = "format",
               names_prefix = "guide_format_ess_") %>% 
  mutate(value2 = case_when(
    value == "all_other" ~ 0,
    value == "paper_and_web" ~ 1,
    value == "mostly" ~ 2,
    value == "only" ~ 3
  ))

Graph2 %>% 
  drop_na(value, ses_houseincome) %>% 
  mutate(format = as.character(format),
         format = ifelse(value == "paper_and_web", "", format),
         value = ifelse(value == "paper_and_web", "Papier et\nweb également", value),
         format = case_when(
           format == "" ~ "",
           format == "html" ~ "HTML",
           format == "papier" ~ "Papier",
           format == "pdf" ~ "PDF"
         ),
         format = factor(format, levels = c("", "Papier", "HTML", "PDF")),
         ses_houseincome = case_when(
           ses_houseincome == 0 ~   "Faible\n(40 000$ et moins)",
           ses_houseincome == 0.5 ~ "Moyen\n(40 000$ - 99 999$)",
           ses_houseincome == 1 ~   "Élevé\n(100 000$ et plus)"
         ),
         ses_houseincome = factor(ses_houseincome,
                                  levels = c("Faible\n(40 000$ et moins)",
                                             "Moyen\n(40 000$ - 99 999$)",
                                             "Élevé\n(100 000$ et plus)"))) %>% 
  filter(value != "1") %>% 
  group_by(format, value, ses_houseincome) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = value, y = n)) +
  facet_grid(cols = vars(format),
             rows = vars(ses_houseincome),
             scales = "free",
             space = "free_x",
             switch = "both") +
  geom_bar(stat = "identity",
           aes(fill = value), color = NA,
           show.legend = FALSE) +
  scale_fill_manual(values = c("Papier et\nweb également" = "#7D7D7D",  # Gris
                               "1" = "#C34A36",  # Rouge
                               "3" = "#E1C340",  # Jaune
                               "4" = "#4DAF7C")) +
  scale_x_discrete(labels = c("1" = "Autre format", "3" = "Principalement", "4" = "Uniquement")) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("\nNombre de répondants\n") +
  labs(title = "Quel format du guide « Mieux vivre avec notre enfant »\n(papier ou web) est plus essentielle pour vous?",
       subtitle = "Selon le revenu du ménage") +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.border = element_rect(fill = NA, color = "lightgrey"),
        strip.placement.y = "outside")

ggsave("_SharedFolder_Guide_mve/graphs/apres_vente/essXincome.png",
       width = 9, height = 7)


# age X income ------------------------------------------------------

Graph3 <- Data %>% 
  pivot_longer(., cols = starts_with("guide_format_ess"),
               names_to = "format",
               names_prefix = "guide_format_ess_") %>% 
  mutate(value2 = case_when(
    value == "all_other" ~ 0,
    value == "paper_and_web" ~ 1,
    value == "mostly" ~ 2,
    value == "only" ~ 3
  ))

Graph3 %>% 
  drop_na(value, ses_age) %>% 
  mutate(format = as.character(format),
         format = ifelse(value == "paper_and_web", "", format),
         value = ifelse(value == "paper_and_web", "Papier et\nweb également", value),
         format = case_when(
           format == "" ~ "",
           format == "html" ~ "HTML",
           format == "papier" ~ "Papier",
           format == "pdf" ~ "PDF"
         ),
         format = factor(format, levels = c("", "Papier", "HTML", "PDF"))) %>% 
  filter(value != "1") %>% 
  group_by(format, value, ses_age) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = value, y = n)) +
  facet_grid(cols = vars(format),
             rows = vars(ses_age),
             scales = "free",
             space = "free_x",
             switch = "both") +
  geom_bar(stat = "identity",
           aes(fill = value), color = NA,
           show.legend = FALSE) +
  scale_fill_manual(values = c("Papier et\nweb également" = "#7D7D7D",  # Gris
                               "1" = "#C34A36",  # Rouge
                               "3" = "#E1C340",  # Jaune
                               "4" = "#4DAF7C")) +
  scale_x_discrete(labels = c("1" = "Autre format", "3" = "Principalement", "4" = "Uniquement")) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("") +
  ylab("\nNombre de répondants\n") +
  labs(title = "Quel format du guide « Mieux vivre avec notre enfant »\n(papier ou web) est plus essentielle pour vous?",
       subtitle = "Selon l'âge du répondant") +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.border = element_rect(fill = NA, color = "lightgrey"),
        strip.placement.y = "outside")

ggsave("_SharedFolder_Guide_mve/graphs/apres_vente/essXage.png",
       width = 9, height = 7)

