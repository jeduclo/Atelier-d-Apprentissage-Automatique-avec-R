##### Session-11 : Préparation avancée des données 

## Exploration de tidyverse de R 

install.packages("tidyverse")
library(tidyverse) # Charger tous les packages tidyverse

# Convertir l'ensemble de données d'apprentissage du Titanic en tibble
install.packages("tibble")
library(tibble) # Pas nécessaire si tidyverse est déjà chargé
titanic_csv <- read.csv("titanic_train.csv")
titanic_tbl <- as_tibble(titanic_csv)
titanic_tbl

# Lire l'ensemble de données d'apprentissage du Titanic à l'aide de readr
library(readr) # Pas nécessaire si tidyverse est déjà chargé
titanic_train <- read_csv("titanic_train.csv")

# Lire l'ensemble de données d'apprentissage du Titanic à l'aide de readxl
library(readxl)
titanic_train <- read_excel("titanic_train.xlsx")

# Préparation et gestion des données avec dplyr
library(dplyr)

# Filtrer uniquement les lignes féminines
titanic_train |> filter(Sex == "female")

# Sélectionner uniquement les colonnes Name, Sex et Age
titanic_train |> select(Name, Sex, Age)

# Combinez plusieurs verbes dplyr et enregistrez la sortie dans un tibble
titanic_women <- titanic_train |>
  filter(Sex == "female") |>
  select(Name, Sex, Age) |>
  arrange(Name)

# Créer une nouvelle caractéristique indiquant un âge avancé
titanic_train |>
  mutate(elderly = if_else(Age >= 65, 1, 0))

# Créez plusieurs caractéristiques dans la même commande mutate
titanic_train |>
  mutate(
    elderly = if_else(Age >= 65, 1, 0),
    child = if_else(Age < 18, 1, 0)
  )

# Calculer le taux de survie par genre
titanic_train |>
  group_by(Sex) |>
  summarize(survival_rate = mean(Survived))

# Calculer le taux de survie moyen des enfants par rapport aux non-enfants
titanic_train |>
  filter(!is.na(Age)) |>
  mutate(child = if_else(Age < 18, 1, 0)) |>
  group_by(child) |>
  summarize(survival_rate = mean(Survived))

# Transformer l'ensemble de données et le diriger vers un arbre de décision
library(rpart)
m_titanic <- titanic_train |>
  filter(!is.na(Age)) |>
  mutate(AgeGroup = if_else(Age < 18, "Enfant", "Adulte")) |>
  select(Survived, Pclass, Sex, AgeGroup) |>
  rpart(formula = Survived ~ ., data = _)

library(rpart.plot)
rpart.plot(m_titanic)

## Transformation de texte avec stringr ----
library(readr)
titanic_train <- read_csv("titanic_train.csv")

library(stringr)

# Examinez le code préfixe de la cabine
titanic_train <- titanic_train |>
  mutate(CabinCode = str_sub(Cabin, start = 1, end = 1))

# Comparez le préfixe de la cabine à la classe des passagers
table(titanic_train$Pclass, titanic_train$CabinCode,
      useNA = "ifany")

# Tracé de la probabilité de survie par code de cabine
library(ggplot2)
titanic_train |> ggplot() +
  geom_bar(aes(x = CabinCode, y = Survived),
           stat = "summary", fun = "mean") +
  ggtitle("Taux de survie du Titanic par code de cabine")

# Regardez les premiers noms de passagers
head(titanic_train$Name)

# Créez une fonctionnalité de titre / salutation
titanic_train <- titanic_train |>
  # Utilisez des expressions régulières pour trouver les caractères entre la virgule et le point
  mutate(Title = str_extract(Name, ", [A-z]+\\."))

# Regardez les premiers exemples
head(titanic_train$Title)

# Nettoyez la fonction de titre
titanic_train <- titanic_train |>
  mutate(Title = str_replace_all(Title, "[, \\.]", ""))

# Examinez la sortie
table(titanic_train$Title)

# Regroupez les titres dans des catégories connexes
titanic_train <- titanic_train |>
  mutate(TitleGroup = recode(Title,
                             # Les premiers restent les mêmes
                             "Mr" = "M.", "Mrs" = "Mme", "Master" = "Enfant",
                             "Miss" = "Mlle",
                             # Combinez les variantes de "Miss"
                             "Ms" = "Mlle", "Mlle" = "Mlle", "Mme" = "Mlle",
                             # Tout le reste sera "Autre"
                             .missing = "Autre",
                             .default = "Autre"
  )
  )

# Examinez la sortie
table(titanic_train$TitleGroup)

# Tracé de la probabilité de survie par groupe de titres
library(ggplot2)
titanic_train |> ggplot() +
  geom_bar(aes(x = TitleGroup, y = Survived),
           stat = "summary", fun = "mean") +
  ggtitle("Taux de survie du Titanic par salutation")

## Nettoyage des dates avec lubridate ----

library(lubridate)

# Lecture des dates de publication de Machine Learning avec R dans différents formats
mdy(c("25 octobre 2013", "10/25/2013"))
dmy(c("25 octobre 2013", "25.10.13"))
ymd("2013-10-25")

# Construire les dates de publication de MLwR
MLwR_1stEd <- mdy("25 octobre 2013")
MLwR_2ndEd <- mdy("31 juillet 2015")
MLwR_3rdEd <- mdy("15 avril 2019")

# Calcul des différences (renvoie un objet difftime)
MLwR_2ndEd - MLwR_1stEd
MLwR_3rdEd - MLwR_2ndEd

# Convertir les différences en durées
as.duration(MLwR_2ndEd - MLwR_1stEd)
as.duration(MLwR_3rdEd - MLwR_2ndEd)

# Convertir la durée en années
dyears()
as.duration(MLwR_2ndEd - MLwR_1stEd) / dyears()
as.duration(MLwR_3rdEd - MLwR_2ndEd) / dyears()

# Version plus facile à retenir de ce qui précède :
time_length(MLwR_2ndEd - MLwR_1stEd, unit = "years")
time_length(MLwR_3rdEd - MLwR_2ndEd, unit = "years")

# Calculer l'âge (en durée)
USA_DOB <- mdy("4 juillet 1776") # Date de naissance des États-Unis
time_length(mdy("3 juillet 2023") - USA_DOB, unit = "years")
time_length(mdy("5 juillet 2023") - USA_DOB, unit = "years")

# Calculer l'âge (en utilisant des intervalles)
interval(USA_DOB, mdy("3 juillet 2023")) / years()
interval(USA_DOB, mdy("5 juillet 2023")) / years()

# Calculer l'âge (en utilisant une division entière)
USA_DOB %--% mdy("3 juillet 2023") %/% years()
USA_DOB %--% mdy("5 juillet 2023") %/% years()

# Fonction pour calculer l'âge calendrier
age <- function(date_naissance) {
  date_naissance %--% today() %/% years()
}

# Calculer l'âge de célébrités
age(mdy("12 janvier 1964")) # Jeff Bezos
age(mdy("28 juin 1971")) # Elon Musk
age(mdy("28 octobre 1955")) # Bill Gates
