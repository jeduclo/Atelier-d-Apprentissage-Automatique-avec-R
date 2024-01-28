##### Session-12 : Données complexes 

## Régression pas à pas 

# Lire les données et effectuer une préparation simple des données
library(tidyverse)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age),
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = factor(if_else(is.na(Embarked), "X", Embarked)),
    Sex = factor(Sex)
  )

# Spécifier le modèle de régression logistique le plus simple
simple_model <- glm(Survived ~ 1, family = binomial, data = titanic_train)

# Spécifier le modèle de régression logistique complet
full_model <- glm(Survived ~ Age + Age_MVI + Embarked + Sex + Pclass + SibSp + Fare,
                  family = binomial, data = titanic_train)

# Régression pas à pas avant
sw_forward <- stats::step(simple_model, scope = formula(full_model),
                          direction = "forward")

# Obtenir la formule du modèle final
formula(sw_forward)

# Les coefficients de régression du modèle final
sw_forward$coefficients

# Régression pas à pas arrière
sw_backward <- stats::step(full_model, direction = "backward")

## Sélection de caractéristiques avec Boruta ----

set.seed(12345) # Définir la graine aléatoire pour assurer que les résultats correspondent
# Créer une caractéristique avec des valeurs aléatoires pour démontrer une caractéristique inutile
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100)

# Exécuter Boruta sur l'ensemble de données du Titanic (cela peut prendre beaucoup de temps pour les ensembles de données plus importants)
install.packages("Boruta")
library(Boruta)
titanic_boruta <- Boruta(Survived ~ PassengerId + Age + 
                           Sex + Pclass + SibSp + rand_vals,
                         data = titanic_train, doTrace = 1)
# Vérifier le résultat
titanic_boruta

# Tracer l'importance des caractéristiques
plot(titanic_boruta)

## Analyse en composantes principales (PCA) ----

library(tidyverse) # Charger l'ensemble de packages tidyverse

sns_data <- read_csv("snsdata.csv") # Lire les données sur les médias sociaux des adolescents

# Sélectionner uniquement les 36 colonnes de la colonne nommée 'basketball' à celle nommée 'drugs'
# Chaque colonne contient le nombre de fois où chaque profil de média social a utilisé le terme respectif
sns_terms <- sns_data |> select(basketball:drugs)

# La bibliothèque irlba fournit une fonction PCA plus efficace que prcomp() intégré à R
install.packages("irlba")
library(irlba)

# Exécuter la PCA - notez que nous centrons et re-échelonnons les données ici
set.seed(2023) # Pour garantir que les résultats correspondent au livre
sns_pca <- sns_terms |> 
  prcomp_irlba(n = 10, center = TRUE, scale = TRUE) # Trouver les dix premières composantes principales des données SNS

# Créer un graphique des valeurs propres de la PCA des données SNS
screeplot(sns_pca, npcs = 10, type = "lines",
          main = "Graphique des valeurs propres des composantes principales des données SNS")

# Utiliser un résumé pour voir les composantes et la proportion de variance expliquée
summary(sns_pca)

# Examiner l'objet PCA -- nous nous intéressons principalement aux composantes $x et $rotation
str(sns_pca)

# La composante $x est la version transformée des données d'origine
str(sns_pca$x)

# La composante $x est nos données d'origine transformées avec de nouvelles "caractéristiques" -- les composantes principales
nrow(sns_pca$x) # devrait avoir 30 000 lignes
head(sns_pca$x) # devrait avoir dix colonnes

# Créer une version "longue" de l'ensemble de données PCA pour la visualisation
sns_pca_long <- tibble(Terme_SNS = colnames(sns_terms), as_tibble(sns_pca$rotation)) |> # ajouter les étiquettes de ligne
  pivot_longer(PC1:PC10, names_to = "PC", values_to = "Contribution") # passer d'un ensemble de données large à un ensemble de données long

# Utiliser ggplot pour visualiser les termes importants pour PC4
library(ggplot2)

sns_pca_long |>
  filter(PC == "PC3") |>
  top_n(15, abs(Contribution)) |>
  mutate(Terme_SNS = reorder(Terme_SNS, Contribution)) |>
  ggplot(aes(Terme_SNS, Contribution, fill = Terme_SNS)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        axis.ticks.x = element_blank()) + 
  labs(x = "Terme des médias sociaux",
       y = "Importance relative pour la composante principale",
       title = "15 principaux contributeurs à la PC3")

# Créer une fonction pour visualiser les quatre autres composantes
plot_pca <- function(composante) {
  sns_pca_long |>
    filter(PC == composante) |>
    top_n(15, abs(Contribution)) |>
    mutate(Terme_SNS = reorder(Terme_SNS, Contribution)) |>
    ggplot(aes(Terme_SNS, Contribution, fill = Terme_SNS)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.ticks.x = element_blank()) + 
    labs(x = "Terme des médias sociaux",
         y = "Importance relative pour la composante principale",
         title = paste("15 principaux contributeurs à", composante))
}

# Utiliser la fonction
plot_pca("PC1")
plot_pca("PC2")
plot_pca("PC4")
plot_pca("PC5")

# Nous pouvons utiliser les composantes principales pour prédire le nombre d'amis
sns_data_pca <- cbind(sns_data[1:4], sns_pca$x) # joindre les composantes principales aux données d'origine

# Créer un modèle de régression linéaire prédisant les amis à partir des composantes principales
m <- lm(amis ~ PC1 + PC2 + PC3 + PC4 + PC5, data = sns_data_pca)

m # afficher les coefficients du modèle

## Remapping sparse categorical data ----

library(tidyverse)

# Lire l'ensemble de données du Titanic et créer la caractéristique de titre (du Chapitre 12)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(Titre = str_extract(Nom, ", [A-z]+\\.")) |>
  mutate(Titre = str_replace_all(Titre, "[, \\.]", ""))

# La caractéristique du titre a un grand nombre de catégories
table(titanic_train$Titre, useNA = "ifany")

# Regrouper les catégories ayant une signification réelle similaire
titanic_train <- titanic_train |>
  mutate(GroupeTitre = fct_collapse(Titre, 
                                    M. = "M.",
                                    Mme = "Mme",
                                    Maître = "Maître",
                                    Mlle_Miss = c("Mlle", "Miss"),
                                    Noble = c("Don", "Sir", "Jonkheer", "Lady"),
                                    Militaire = c("Capt", "Col", "Major"),
                                    Docteur = "Dr",
                                    Clergé = "Rev",
                                    other_level = "Autre")
  ) |>
  mutate(GroupeTitre = fct_na_value_to_level(GroupeTitre,
                                             level = "Inconnu"))

# Examiner le recodage
table(titanic_train$GroupeTitre)

# Regarder les comptes et les proportions de tous les niveaux, triés du plus grand au plus petit
fct_count(titanic_train$Titre, sort = TRUE, prop = TRUE)

# Regrouper tout ce qui n'est pas dans les trois niveaux supérieurs
table(fct_lump_n(titanic_train$Titre, n = 3))

# Regrouper tout ce qui représente moins de 1%
table(fct_lump_prop(titanic_train$Titre, prop = 0.01))

# Regrouper tout ce qui a moins de 5 observations
table(fct_lump_min(titanic_train$Titre, min = 5))

## Discrétisation de données numériques éparses ----

# Examiner les données du tarif du Titanic
head(titanic_train$Fare)
summary(titanic_train$Fare)

# Créer une variable binaire pour la première/deuxième classe
titanic_train <- titanic_train |> mutate(
  tarif_premiereclasse = if_else(Fare >= 31, 1, 0, missing = 0)
)

# Tabuler les valeurs binaires
table(titanic_train$tarif_premiereclasse)

# Créer une caractéristique à trois niveaux en utilisant case_when()
titanic_train <- titanic_train |>
  mutate(
    tarif_classe = case_when(
      Fare >= 31 ~ "1ère Classe",
      Fare >= 15 ~ "2ème Classe",
      TRUE ~ "3ème Classe"
    )
  )

# Examiner le résultat
table(titanic_train$tarif_classe)

# La fonction cut() peut accomplir la même chose que le case_when() ci-dessus
table(cut(titanic_train$Fare, breaks = c(-Inf, 15, 31, Inf),
          right = FALSE))

# Utiliser cut() avec seq() pour générer des points de rupture de taille égale
table(cut(titanic_train$Fare, right = FALSE,
          breaks = seq(from = 0, to = 550, by = 50)))

# Utiliser cut() avec quantiles() et seq() pour créer des bacs avec un nombre égal d'exemples
table(cut(titanic_train$Fare, right = FALSE,
          breaks = quantile(titanic_train$Fare,
                            probs = seq(0, 1, 0.20))))

# Utiliser la fonction ntile() du tidyverse pour créer cinq bacs
table(ntile(titanic_train$Fare, n = 5))

# Convertir les groupes ntile() en facteur
titanic_train <- titanic_train |>
  mutate(niveau_tarif = factor(ntile(Fare, n = 11)))

table(titanic_train$niveau_tarif)
