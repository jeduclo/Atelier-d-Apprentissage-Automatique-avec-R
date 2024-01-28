## Session-13:  Imputation des valeurs manquantes 

library(readr)
titanic_train <- read_csv("titanic_train.csv")

# Imputer des chaînes de texte arbitraires pour les données catégorielles manquantes
titanic_train <- titanic_train |>
  mutate(
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = if_else(is.na(Embarked), "Inconnu", Embarked)
  )

# Imputer la valeur moyenne et créer un indicateur de valeur manquante pour l'âge
titanic_train <- titanic_train |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age)
  )

## Stratégies simples pour rééquilibrer les données 

# Charger et préparer les données sur les médias sociaux des adolescents
library(tidyverse)

snsdata <- read_csv("snsdata.csv") |>
  mutate(
    genre = fct_recode(genre, Femme = "F", Homme = "M"),
    genre = fct_na_value_to_level(genre, level = "Inconnu"),
    age = ifelse(age < 13 | age > 20, NA, age) # Remplacer les valeurs aberrantes de l'âge
  ) |>
  group_by(gradyear) |>
  mutate(age_imp = if_else(is.na(age), median(age, na.rm = TRUE), age)) |>
  ungroup() |>
  select(genre, amis, gradyear, age_imp, basketball:drugs)

# Examiner le déséquilibre initial des classes
fct_count(snsdata$genre, prop = TRUE)

# Sous-échantillonner les classes majoritaires
library(caret)
sns_undersample <- downSample(x = snsdata[2:40], y = snsdata$genre, yname = "genre")
fct_count(sns_undersample$genre, prop = TRUE)

# Surexéchantillonner les classes minoritaires
library(caret)
sns_oversample <- upSample(x = snsdata[2:40], y = snsdata$genre, yname = "genre")
fct_count(sns_oversample$genre, prop = TRUE)

## Génération d'un ensemble de données équilibré synthétique avec SMOTE 

# Créer un ensemble de données équilibré en genre en utilisant SMOTE
install.packages("themis")
library(themis)
sns_balanced <- snsdata |> smote("genre") # syntaxe simple (sans normalisation)

# Vérifier que l'ensemble de données est maintenant équilibré en genre
table(sns_balanced$genre)

# Pour une meilleure utilisation de SMOTE, créer une fonction de normalisation (introduite au Chapitre 3)
normaliser <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Une fonction de dénormalisation ramène les données à l'échelle d'origine (introduite au Chapitre 7)
denormaliser <- function(valeurs_norm, nom_colonne) {
  anciennes_valeurs <- snsdata[nom_colonne]
  valeurs_denormalisées <- valeurs_norm * (max(anciennes_valeurs) - min(anciennes_valeurs)) + min(anciennes_valeurs)
  
  # Arrondir toutes les colonnes à des entiers sauf age_imp
  valeurs_arrondies <- if(nom_colonne != "age_imp") { round(valeurs_denormalisées) }
  else {valeurs_denormalisées}
  
  return (valeurs_arrondies)
}

# Processus SMOTE plus avancé avec des données normalisées
snsdata_equilibre <- snsdata |>
  mutate(across(where(is.numeric), normaliser)) |> # normaliser les données numériques
  smote("genre") |>
  mutate(across(where(is.numeric), ~denormaliser(.x, cur_column()))) # dénormaliser les données

# Confirmer que l'ensemble de données rééquilibré a fonctionné correctement
table(snsdata$genre)
table(snsdata_equilibre$genre)
