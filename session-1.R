##### Session-1 : Gestion et compréhension des données -------------------

##### Structures de données R --------------------

## Vecteurs -----

# Créer des vecteurs de données pour trois patients médicaux
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# Accéder au deuxième élément dans le vecteur de température corporelle
temperature[2]

## Exemples d'accès aux éléments d'un vecteur
# Inclure les éléments dans la plage 2 à 3
temperature[2:3]

# Exclure l'élément 2 en utilisant le signe moins
temperature[-2]

# Utiliser un vecteur pour indiquer les éléments à inclure
temperature[c(TRUE, TRUE, FALSE)]

# Créer un vecteur logique à partir d'une expression logique
fever <- temperature > 100

# Identifier les patients avec de la fièvre (les deux produisent des résultats identiques)
subject_name[fever]
subject_name[temperature > 100]

## Facteurs -----

# Ajouter un facteur pour le genre
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# Ajouter un facteur pour le groupe sanguin
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

# Ajouter un facteur ordonné
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)
symptoms

# Vérifier les symptômes supérieurs à "MODERATE"
symptoms > "MODERATE"

## Listes -----

# Afficher les informations d'un patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# Créer une liste pour un patient
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

# Afficher le patient
subject1

## Méthodes pour accéder à une liste

# Obtenir une seule valeur de la liste par position (retourne une sous-liste)
subject1[2]

# Obtenir une seule valeur de la liste par position (retourne un vecteur numérique)
subject1[[2]]

# Obtenir une seule valeur de la liste par nom
subject1$temperature

# Obtenir plusieurs éléments de la liste en spécifiant un vecteur de noms
subject1[c("temperature", "flu_status")]

## Accéder à une liste comme à un vecteur
# Obtenir les valeurs 2 et 3
subject1[2:3]

## Data frames -----

# Créer un data frame à partir des données des patients médicaux

pt_data <- data.frame(subject_name, temperature,
                      flu_status, gender, blood, symptoms)

# Afficher le data frame
pt_data

## Accès à un data frame

# Obtenir une seule colonne
pt_data$subject_name

# Obtenir plusieurs colonnes en spécifiant un vecteur de noms
pt_data[c("temperature", "flu_status")]

# C'est la même chose qu'au-dessus, en extrayant temperature et flu_status
pt_data[2:3]

# Accès par ligne et par colonne
pt_data[1, 2]

# Accès à plusieurs lignes et plusieurs colonnes en utilisant des vecteurs
pt_data[c(1, 3), c(2, 4)]

## Laissez une ligne ou une colonne vide pour extraire toutes les lignes ou toutes les colonnes

# Colonne 1, toutes les lignes
pt_data[, 1]
# Ligne 1, toutes les colonnes
pt_data[1, ]
# Toutes les lignes et toutes les colonnes
pt_data[ , ]

# Les suivants sont équivalents
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]

# Création d'une colonne de température en Celsius
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)

# Comparaison avant et après
pt_data[c("temperature", "temp_c")]

## Matrices -----

# Créer une matrice 2x2
m <- matrix(c(1, 2, 3, 4), nrow = 2)
m

# Équivalent à ce qui précède
m <- matrix(c(1, 2, 3, 4), ncol = 2)
m

# Créer une matrice 2x3
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m

# Créer une matrice 3x2
m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
m

# Extraire des valeurs des matrices
m[1, 1]
m[3, 2]

# Extraire des lignes
m[1, ]

# Extraire des colonnes
m[, 1]

##### Gestion des données avec R ------------

## Sauvegarde, chargement et suppression des structures de données R

# Afficher toutes les structures de données en mémoire
ls()

# Supprimer les objets m et subject1
rm(m, subject1)
ls()

rm(list=ls())

##### Importation et sauvegarde de jeux de données depuis des fichiers CSV ------------

# Lecture d'un fichier CSV
pt_data <- read.csv("pt_data.csv")

# Lecture d'un fichier CSV et conversion de toutes les colonnes de caractères en facteurs
pt_data <- read.csv("pt_data.csv", stringsAsFactors = TRUE)

##### Exploration et compréhension des données --------------------

## Exemple d'exploration de données en utilisant des données de voitures d'occasion
usedcars <- read.csv("usedcars.csv")

# Obtenir la structure des données de voitures d'occasion
str(usedcars)

## Exploration des variables numériques -----

# Résumer les variables numériques
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# Calculer la moyenne du revenu
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# La médiane du revenu
median(c(36000, 44000, 56000))

# Le min/max des prix des voitures d'occasion
range(usedcars$price)

# La différence de la plage
diff(range(usedcars$price))

# IQR pour les prix des voitures d'occasion
IQR(usedcars$price)

# Utiliser quantile pour calculer le résumé en cinq chiffres
quantile(usedcars$price)

# Le 99e percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# Quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# Boîte à moustaches des prix des voitures d'occasion et du kilométrage
boxplot(usedcars$price, main = "Boîte à moustaches des prix des voitures d'occasion",
        ylab = "Prix ($)")

boxplot(usedcars$mileage, main="Boîte à moustaches du kilométrage des voitures d'occasion",
        ylab = "Compteur (mi.)")

# Histogrammes des prix des voitures d'occasion et du kilométrage
hist(usedcars$price, main = "Histogramme des prix des voitures d'occasion",
     xlab = "Prix ($)")

hist(usedcars$mileage, main = "Histogramme du kilométrage des voitures d'occasion",
     xlab = "Compteur (mi.)")

# Variance et écart type des données sur les voitures d'occasion
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploration des variables catégorielles -----

# Tables unidirectionnelles pour les données sur les voitures d'occasion
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# Calculer les proportions de table
model_table <- table(usedcars$model)
prop.table(model_table)

# Arrondir les données
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploration des relations entre les variables -----

# Nuage de points du prix par rapport au kilométrage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Nuage de points du prix par rapport au kilométrage",
     xlab = "Compteur des voitures d'occasion (mi.)",
     ylab = "Prix des voitures d'occasion ($)")

# Nouvelle variable indiquant les couleurs conservatrices
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

# Vérification de notre variable
table(usedcars$conservative)

# Tableau croisé entre les couleurs conservatrices et les modèles
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)

# Calcul du test du chi carré manuellement
# Étape 1 : somme des contributions des cellules
0.009 + 0.004 + 0.086 + 0.044 + 0.007 + 0.004

# Étape 2 : calcul de la probabilité du chi carré
pchisq(0.154, df = 2, lower.tail = FALSE)

# Optionnel : vous pouvez également calculer le test du chi carré avec CrossTable
CrossTable(x = usedcars$model, y = usedcars$conservative, chisq = TRUE)
