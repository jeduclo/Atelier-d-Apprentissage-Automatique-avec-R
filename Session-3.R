##### Session-3 : Classification en utilisant Naive Bayes --------------------

## Exemple : Filtrage des messages SMS indésirables ----
## Étape 2 : Exploration et préparation des données ----

# Lire les données SMS dans le data frame sms_raw
sms_raw <- read.csv("sms_spam.csv")

# Examiner la structure des données sms
str(sms_raw)

# Convertir spam/ham en facteur.
sms_raw$type <- factor(sms_raw$type)

# Examiner la variable type plus en détail
str(sms_raw$type)
table(sms_raw$type)

# Construire un corpus en utilisant le package text mining (tm)
install.packages("tm")
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# Examiner le corpus SMS
print(sms_corpus)
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

# Nettoyer le corpus en utilisant tm_map()
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# Montrer la différence entre sms_corpus et corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # Supprimer les nombres
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # Supprimer les mots vides
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # Supprimer la ponctuation

# Astuce : créer une fonction personnalisée pour remplacer (plutôt que supprimer) la ponctuation
removePunctuation("hello...world")
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
replacePunctuation("hello...world")

# Illustration de la racinisation des mots
install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # Éliminer les espaces inutiles

# Examiner le corpus propre final
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# Créer une matrice creuse document-term
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Solution alternative : créer directement une matrice creuse document-term à partir du corpus SMS
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# Solution alternative : utiliser une fonction personnalisée pour les mots vides pour assurer un résultat identique
sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x, stopwords()) },
  removePunctuation = TRUE,
  stemming = TRUE
))

# Comparer le résultat
sms_dtm
sms_dtm2
sms_dtm3

# Création d'ensembles de données d'entraînement et de test
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# Enregistrer également les étiquettes
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# Vérifier que la proportion de spam est similaire
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# Visualisation du nuage de mots
install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# Sous-ensemble des données d'entraînement en groupes spam et ham
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# Fonctionnalités indicatrices pour les mots fréquents
findFreqTerms(sms_dtm_train, 5)

# Enregistrer les termes fréquemment présents dans un vecteur de caractères
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# Créer des matrices document-term avec seulement les termes fréquents
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# Convertir les comptages en facteur
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Oui", "Non")
}

# Appliquer() convert_counts() aux colonnes des données d'entraînement/test
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

## Étape 3 : Entraînement d'un modèle sur les données ----
install.packages("naivebayes")
library(naivebayes)
sms_classifier <- naive_bayes(sms_train, sms_train_labels)

## Étape 4 : Évaluation de la performance du modèle ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('prédit', 'réel'))

## Étape 5 : Amélioration de la performance du modèle ----
sms_classifier2 <- naive_bayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('prédit', 'réel'))
