# Packages ----------------------------------------------------------------
library(tidyverse)
#library(NLP)
library(tm)
library(SnowballC)
library(stats)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_Guide_mve/data/clean.rds")

Papier <- Data %>% 
  filter(guide_use_format %in% c(0, 0.25),
         !(guide_use_format_why %in% c("", " ")))
writexl::write_xlsx(Papier %>% slice_sample(n = 100) %>% select(guide_use_format_why),
                    "_SharedFolder_Guide_mve/data/guise_format_why_sample.xlsx")

Neut <- Data %>% 
  filter(guide_use_format == 0.5,
         !(guide_use_format_why %in% c("", " ")))

Web <- Data %>% 
  filter(guide_use_format %in% c(0.75, 1),
         !(guide_use_format_why %in% c("", " ")))

# Papier ------------------------------------------------------------------


## Cleaning text -----------------------------------------------------------
# Créer un corpus de texte
text_corpus <- Corpus(VectorSource(Papier$guide_use_format_why),
                      readerControl = list(language = "fr"))
# Étape 1 : Conversion en minuscules
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
# Étape 2 : Suppression de la ponctuation
text_corpus <- tm_map(text_corpus, removePunctuation)
# Étape 3 : Suppression des mots vides (stop words)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("french"))
text_corpus <- tm_map(text_corpus, removeWords, c("'", "’", "’a"))
# Étape 4 : Suppression des espaces inutiles
text_corpus <- tm_map(text_corpus, stripWhitespace)
# Étape 5 : racine des mots
text_corpus <- tm_map(text_corpus, stemDocument, language = "french")
# Étape 6 : Création d'un nouveau corpus
cleaned_corpus <- Corpus(VectorSource(unlist(sapply(text_corpus, as.character))))



## Représentation numérique ------------------------------------------------
tfidf_transformer <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  tfidf <- weightTfIdf(dtm)
  return(tfidf)
}

tfidf_matrix <- tfidf_transformer(cleaned_corpus)

# Convertir la matrice TF-IDF en un data frame pour une meilleure manipulation
tfidf_df <- as.data.frame(as.matrix(tfidf_matrix))

## Clustering ------------------------------------------------------------

cl_result <- kmeans(tfidf_df, centers = 3)


