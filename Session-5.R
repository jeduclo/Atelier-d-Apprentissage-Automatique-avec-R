##### Session-5 : Méthodes de régression 

#### Partie 1 : Régression linéaire 

## Compréhension de la régression 
## Exemple : Données de lancement de la navette spatiale 
launch <- read.csv("challenger.csv")

# Estimer bêta manuellement
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# Estimer alpha manuellement
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# Calculer la corrélation des données de lancement
r <- cov(launch$temperature, launch$distress_ct) /
  (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

# Création d'une fonction de régression linéaire simple
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimation"
  print(b)
}

# Examiner les données de lancement
str(launch)

# Tester le modèle de régression avec une régression linéaire simple
reg(y = launch$distress_ct, x = launch[2])

# Utiliser le modèle de régression avec une régression linéaire multiple
reg(y = launch$distress_ct, x = launch[2:4])

# Si désiré, vous pouvez confirmer que la fonction de régression multiple personnalisée fonctionne
# correctement en la comparant au résultat obtenu avec la fonction lm intégrée de R (non inclus dans le texte)
model <- lm(distress_ct ~ temperature + field_check_pressure + flight_num, data = launch)
model

## Exemple : Prédire les frais médicaux ----
## Étape 2 : Exploration et préparation des données ----
insurance <- read.csv("autoinsurance.csv", stringsAsFactors = TRUE)
str(insurance)

# Résumer la variable des frais médicaux
summary(insurance$expenses)

# Histogramme des frais d'assurance
hist(insurance$expenses)

# Tables pour les caractéristiques catégorielles
table(insurance$geo_area)
table(insurance$vehicle_type)

# Exploration des relations entre les caractéristiques : matrice de corrélation
cor(insurance[c("age", "est_value", "miles_driven", "expenses")])

# Visualisation des relations entre les caractéristiques : matrice de nuages de points
pairs(insurance[c("age", "est_value", "miles_driven",
                  "expenses")], pch = ".")

# Matrice de nuages de points plus informative
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "est_value", "miles_driven",
                         "expenses")], pch = ".")

## Étape 3 : Entraînement d'un modèle sur les données ----
ins_model <- lm(expenses ~ age + geo_area + vehicle_type +
                  est_value + miles_driven +
                  college_grad_ind + speeding_ticket_ind +
                  hard_braking_ind + late_driving_ind +
                  clean_driving_ind,
                data = insurance)

ins_model <- lm(expenses ~ ., data = insurance) # cela équivaut à ce qui précède

# Voir les coefficients bêta estimés
options(scipen = 999) # désactive la notation scientifique
ins_model

## Étape 4 : Évaluation de la performance du modèle ----
# Voir plus de détails sur les coefficients bêta estimés
summary(ins_model)

## Étape 5 : Amélioration de la performance du modèle ----
# Ajouter un terme "âge" d'ordre supérieur
insurance$age2 <- insurance$age^2

# Créer le modèle final
ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind,
                 data = insurance)

summary(ins_model2)

# Faire des prédictions avec le modèle de régression
insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 1))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 24000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

## Prédiction de la résiliation des titulaires de police d'assurance avec la régression logistique ----

churn_data <- read.csv("insurance_churn.csv")

prop.table(table(churn_data$churn)) # voir le % de résiliation

# Créer le modèle de régression logistique
churn_model <- glm(churn ~ . -member_id, data = churn_data,
                   family = binomial(link = "logit"))

# Examiner les estimations des paramètres du modèle de régression logistique
summary(churn_model)

# Lire l'ensemble de test
churn_test <- read.csv("insurance_churn_test.csv")

# Faire des prédictions sur l'ensemble de test
churn_test$churn_prob <- predict(churn_model, churn_test,
                                 type = "response")

# Examiner les valeurs prédites
summary(churn_test$churn_prob)

# Fournir les membres les plus susceptibles de résilier
churn_order <- order(churn_test$churn_prob, decreasing = TRUE)
head(churn_test[churn_order, c("member_id", "churn_prob")], n = 5)

#### Partie 2 : Arbres de régression et arbres de modèle -------------------

## Compréhension des arbres de régression et des arbres de modèle ----
## Exemple : Calcul de la SDR ----
# Configurer les données
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# Calculer la SDR
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# Comparer la SDR pour chaque division
sdr_a
sdr_b

## Exemple : Estimation de la qualité du vin ----
## Étape 2 : Exploration et préparation des données ----
wine <- read.csv("whitewines.csv")

# Examiner les données sur le vin
str(wine)

# Distribution des évaluations de la qualité
hist(wine$quality)

# Statistiques sommaires des données sur le vin
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Étape 3 : Entraînement d'un modèle sur les données ----
# Arbre de régression en utilisant rpart
install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# Obtenir des informations de base sur l'arbre
m.rpart

# Obtenir des informations plus détaillées sur l'arbre
summary(m.rpart)

# Utiliser le package rpart.plot pour créer une visualisation
install.packages("rpart.plot")
library(rpart.plot)

# Un diagramme d'arbre de décision de base
rpart.plot(m.rpart, digits = 3)

# Quelques ajustements au diagramme
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Étape 4 : Évaluer la performance du modèle ----

# Générer des prédictions pour l'ensemble de test
p.rpart <- predict(m.rpart, wine_test)

# Comparer la distribution des valeurs prédites avec les valeurs réelles
summary(p.rpart)
summary(wine_test$quality)

# Comparer la corrélation
cor(p.rpart, wine_test$quality)

# Fonction pour calculer l'erreur absolue moyenne
MAE <- function(réel, prédit) {
  mean(abs(réel - prédit))  
}

# Erreur absolue moyenne entre les valeurs prédites et réelles
MAE(p.rpart, wine_test$quality)

# Erreur absolue moyenne entre les valeurs réelles et la valeur moyenne
mean(wine_train$quality) # résultat = 5.87
MAE(5.87, wine_test$quality)

## Étape 5 : Amélioration de la performance du modèle ----
# Entraîner un modèle Cubist Tree
install.packages("Cubist")
library(Cubist)
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

# Afficher des informations de base sur l'arbre de modèle
m.cubist

# Afficher l'arbre lui-même
summary(m.cubist)

# Générer des prédictions pour le modèle
p.cubist <- predict(m.cubist, wine_test)

# Statistiques sommaires sur les prédictions
summary(p.cubist)

# Corrélation entre les valeurs prédites et les vraies valeurs
cor(p.cubist, wine_test$quality)

# Erreur absolue moyenne des valeurs prédites et des vraies valeurs
# (utilise une fonction personnalisée définie ci-dessus)
MAE(wine_test$quality, p.cubist)
