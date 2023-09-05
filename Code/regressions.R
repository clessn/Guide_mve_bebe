# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

# Regression models -------------------------------------------------------

## Wrangling ---------------------------------------------------------------

### young_parent_status ####



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
               ses_kids2 + ses_canada + ses_languagefr +
               ses_educ + ses_houseincome,
             family = binomial(),
             data = Data)
summary(model)

vis <- attr(model$terms, "term.labels")

df_vis <- Data %>% 
  select(all_of(vis))

for (i in 1:length(vis)){
  vi <- vis[i]
  choices <- names(table(df_vis[[vi]]))
  dfi <- df_vis[rep(1:nrow(df_vis), each = length(choices)),]
  dfi[[vi]] <- rep(choices, times = nrow(df_vis))
  dfi$var <- vi
  if (i == 1){
    df <- dfi
  } else {
    df <- rbind(df, dfi) %>% 
      drop_na()
  }
}

df$ses_couple <- as.numeric(df$ses_couple)
df$ses_canada <- as.numeric(df$ses_canada)
df$ses_languagefr <- as.numeric(df$ses_languagefr)
df$ses_houseincome <- as.numeric(df$ses_houseincome)

df$prob <- predict(model, newdata = df, type = "response")

ggplot(df, aes(x = , y = ))