# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

# Regression models -------------------------------------------------------

## Wrangling ---------------------------------------------------------------

### young_parent_status ####

Data$yparent_status <- NA
## young_parent_not_preg
Data$yparent_status[Data$filt_preg == 0 &
                      Data$filt_parent == 1] <- "parent_not_preg"
## young parent pregnant
Data$yparent_status[Data$filt_preg == 1 &
                      Data$filt_1stkid == 0] <- "parent_preg"
## pregnant first kid
Data$yparent_status[Data$filt_preg == 1 &
                       Data$filt_1stkid == 1] <- "firstkid"
table(Data$yparent_status)
Data$yparent_status <- factor(Data$yparent_status)
Data$yparent_status <- relevel(Data$yparent_status,
                               ref = "parent_not_preg")


table(Data$info_1, useNA = "always")

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

## nb of kids
table(Data$ses_kids)
Data$ses_kids2 <- as.numeric(Data$ses_kids)
Data$ses_kids2[Data$ses_kids2 > 2] <- 3
Data$ses_kids2 <- factor(Data$ses_kids2, ordered = TRUE)
#Data$ses_kids2 <- Data$ses_kids2/3
table(Data$ses_kids2)
unique(Data$ses_kids2)

## education
Data$ses_educ <- NA
Data$ses_educ[Data$ses_educationsans5 == 1 |
                Data$ses_educationavec5 == 1 |
                Data$ses_educationprimaire == 1] <- "bhs"
Data$ses_educ[Data$ses_educationcollegial == 1] <- "college"
Data$ses_educ[Data$ses_educationbacc == 1 |
                Data$ses_educationmaitrise == 1 |
                Data$ses_educationphd == 1] <- "univ"
Data$ses_educ <- factor(Data$ses_educ, levels = c("bhs", "college", "univ"))
table(Data$ses_educ)





## Who does not know the guide? --------------------------------------------

table(Data$guide_connaitre)

### Model -------------------------------------------------------------------

model <- glm(guide_connaitre ~ ses_age + ses_couple +
               ses_kids2 +
               ses_canada*ses_age +
               yparent_status + ses_languagefr +
               ses_educ + ses_houseincome +
               info_1,
             family = binomial(),
             data = Data)
summary(model)

# Predict on ses_kids2 ----------------------------------------------------

### Repeat Data for the number of categories in VI
choices <- names(table(Data$ses_kids2))
df_kids <- Data[rep(1:nrow(Data),
                    each = length(choices)),]
df_kids$ses_kids2 <- rep(choices, times = nrow(Data))
df_kids$prob <- predict(model, newdata = df_kids, type = "response")

### graph
ggplot(df_kids, aes(x = prob*100, y = ses_kids2)) +
  ggridges::geom_density_ridges(
    quantile_lines = c(0.25, 0.5, 0.75),
    scale = 1,
    fill = "#FFA7B7",
    color = "#FFA7B7",
    alpha = 0.4
  ) +
  xlim(60, 100) +
  scale_y_discrete(labels = c("0" = "0",
                              "1" = "1",
                              "2" = "2",
                              "3" = "3 et +")) +
  clessnverse::theme_clean_light(base_size = 15) +
  xlab("Probabilité de connaître le guide MVE (%)") +
  ylab("Nombre d'enfants") +
  labs(title = "Relation entre le nombre d'enfants\net la connaissance du guide MVE",
       caption = "Probabilités prédites d'un modèle de régression logistique.")

ggsave("_SharedFolder_Guide_mve/graphs/13_connaitreXkids.png",
       width = 10, height = 8)  


# Predict on ses_canada ----------------------------------------------------

### Repeat Data for the number of categories in VI
choices <- names(table(Data$ses_canada))
df_can <- Data[rep(1:nrow(Data),
                    each = length(choices)),]
df_can$ses_canada <- rep(as.numeric(choices), times = nrow(Data))
df_can$prob <- predict(model, newdata = df_can, type = "response")

### graph

graph <- df_can %>% 
  group_by(ses_canada, ses_age) %>% 
  summarise(
    mean = mean(prob, na.rm = T)*100,
    low = list(confint(lm(prob ~ 1)))[[1]][1]*100,
    high = list(confint(lm(prob ~ 1)))[[1]][2]*100,
  ) %>% 
  mutate(ses_canada = case_when(
    ses_canada == 1 ~ "Né au Canada",
    ses_canada == 0 ~ "Né hors-Canada"
  ))

ggplot(graph, aes(x = mean, y = ses_age)) +
  geom_pointrange(aes(xmin = low,
                    xmax = high,
                    group = ses_canada,
                    color = ses_canada),
                position = position_dodge(width = 0.5),
                linewidth = 5,
                lineend = "round") +
  geom_jitter(aes(color = ses_canada,
                  group = ses_canada),
             size = 12, alpha = 1,
             position = position_dodge(width = 0.5)) +
  geom_jitter(aes(color = ses_canada,
                 x = low,
                 group = ses_canada),
             size = 8, alpha = 0.5,
             position = position_dodge(width = 0.5)) +
  geom_jitter(aes(color = ses_canada,
                 x = high,
                 group = ses_canada),
             size = 8, alpha = 0.5,
             position = position_dodge(width = 0.5)) +
  xlim(60, 100) +
  scale_color_manual(values = c(
    "Né au Canada" = "#BDA0CB",
    "Né hors-Canada" = "#A8D5BA"
  )) +
  scale_y_discrete(labels = c("18_29" = "18-29 ans",
                              "30_39" = "30-39 ans",
                              "40_49" = "40-49 ans")) +
  clessnverse::theme_clean_light(base_size = 15) +
  xlab("Probabilité de connaître le guide MVE (%)") +
  ylab("Âge des parents") +
  labs(title = "Relation entre l'âge et la provenance des parents\navec la connaissance du guide MVE",
       caption = "Probabilités prédites d'un modèle de régression logistique.\nLes lignes horizontales indiquent l'intervalle de confiance à un niveau de confiance de 95%.")

ggsave("_SharedFolder_Guide_mve/graphs/13_connaitreXses_canada.png",
       width = 10, height = 8)  


# Predict on ses_canada ----------------------------------------------------

### Repeat Data for the number of categories in VI
choices <- names(table(Data$ses_houseincome))
df_inc <- Data[rep(1:nrow(Data),
                   each = length(choices)),]
df_inc$ses_houseincome <- rep(as.numeric(choices), times = nrow(Data))
df_inc$prob <- predict(model, newdata = df_inc, type = "response")



