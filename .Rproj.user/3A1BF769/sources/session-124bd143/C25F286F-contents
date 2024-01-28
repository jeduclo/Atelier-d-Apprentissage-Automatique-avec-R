##### Session-9 : Évaluation de la performance du modèle 

## Matrices de confusion en R

## Créer les probabilités prédites à partir du classificateur SMS construit au Chapitre 4.
resultats_sms <- read.csv("sms_results.csv", stringsAsFactors = TRUE)

# Les premiers cas de test
head(resultats_sms)

# Cas de test où le modèle est moins confiant
head(subset(resultats_sms, prob_spam > 0.40 & prob_spam < 0.60))

# Cas de test où le modèle s'est trompé
head(subset(resultats_sms, actual_type != predict_type))

# Spécifier des vecteurs
table(resultats_sms$actual_type, resultats_sms$predict_type)

# Solution alternative en utilisant l'interface de formule (non montrée dans le livre)
xtabs(~ actual_type + predict_type, resultats_sms)

# Utilisation de la fonction CrossTable
library(gmodels)
CrossTable(resultats_sms$actual_type, resultats_sms$predict_type)

# Calcul de la précision et du taux d'erreur --
# Précision
(152 + 1203) / (152 + 1203 + 4 + 31)
# Taux d'erreur
(4 + 31) / (152 + 1203 + 4 + 31)
# Taux d'erreur = 1 - précision
1 - 0.9748201

## Au-delà de la précision : autres mesures de performance 
install.packages("caret")
library(caret)
confusionMatrix(resultats_sms$predict_type, resultats_sms$actual_type, positive = "spam")

# Statistique kappa
# Exemple en utilisant le classificateur SMS
pr_a <- 0.865 + 0.109
pr_a

pr_e <- 0.868 * 0.888 + 0.132 * 0.112
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

# Calculer le kappa avec le package vcd
install.packages("vcd")
library(vcd)
Kappa(table(resultats_sms$actual_type, resultats_sms$predict_type))

# Calculer le kappa avec le package irr
install.packages("irr")
library(irr)
kappa2(resultats_sms[1:2])

# Coefficient de corrélation de Matthews (MCC)
# Pour le classificateur SMS : TN = 1203, FP = 4, FN = 31, TP = 152

# MCC = (TP * TN - FP * FN) /
#         sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

# Calcul du MCC manuellement pour le classificateur SMS
(152 * 1203 - 4 * 31) /
  sqrt((152 + 4) * (152 + 31) * (1203 + 4) * (1203 + 31))

# Calcul du MCC en utilisant la fonction mcc() du package mltools
install.packages("mltools")
library(mltools)
mcc(resultats_sms$actual_type, resultats_sms$predict_type)

# Calcul du MCC en utilisant la corrélation de Pearson
cor(ifelse(resultats_sms$actual_type == "spam", 1, 0),
    ifelse(resultats_sms$predict_type == "spam", 1, 0))

# Sensibilité et spécificité
# Exemple en utilisant le classificateur SMS
sens <- 152 / (152 + 31)
sens

spec <- 1203 / (1203 + 4)
spec

# Exemple en utilisant le package caret
library(caret)
sensitivity(resultats_sms$predict_type, resultats_sms$actual_type, positive = "spam")
specificity(resultats_sms$predict_type, resultats_sms$actual_type, negative = "ham")

# Précision et rappel (recall)
prec <- 152 / (152 + 4)
prec

rec <- 152 / (152 + 31)
rec

# Exemple en utilisant le package caret
library(caret)
posPredValue(resultats_sms$predict_type, resultats_sms$actual_type, positive = "spam")
sensitivity(resultats_sms$predict_type, resultats_sms$actual_type, positive = "spam")

# F-mesure
f <- (2 * prec * rec) / (prec + rec)
f

f <- (2 * 152) / (2 * 152 + 4 + 31)
f

## Visualisation des compromis de performance 
install.packages("pROC")
library(pROC)
roc_sms <- roc(resultats_sms$actual_type, resultats_sms$prob_spam)

# Courbe ROC pour Naive Bayes
plot(roc_sms, main = "Courbe ROC pour le filtre anti-spam SMS",
     col = "blue", lwd = 2, grid = TRUE, legacy.axes = TRUE)

# Comparaison avec kNN 
resultats_sms_knn <- read.csv("sms_results_knn.csv")
roc_sms_knn <- roc(resultats_sms$actual_type, resultats_sms_knn$p_spam)
plot(roc_sms_knn, col = "red", lwd = 2, add = TRUE)

# Calcul de l'AUC pour Naive Bayes et kNN
auc(roc_sms)
auc(roc_sms_knn)

## Estimation de la performance future 

# Partitionnement des données
library(caret)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

# Méthode de réserve (Holdout)
# Utilisation d'identifiants aléatoires
ids_aléatoires <- order(runif(1000))
credit_entrainement <- credit[ids_aléatoires[1:500],]
credit_validation <- credit[ids_aléatoires[501:750], ]
credit_test <- credit[ids_aléatoires[751:1000], ]

# Utilisation de la fonction caret
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_entrainement <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# Validation croisée à 10 plis
set.seed(123) # pour garantir que les résultats correspondent
plis <- createFolds(credit$default, k = 10)
str(plis)
credit01_test <- credit[plis$Fold01, ]
credit01_entrainement <- credit[-plis$Fold01, ]

## Validation croisée à 10 plis automatisée pour un arbre de décision C5.0 en utilisant lapply() ----
library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

set.seed(123)
plis <- createFolds(credit$default, k = 10)

resultats_cv <- lapply(plis, function(x) {
  credit_entrainement <- credit[-x, ]
  credit_test <- credit[x, ]
  modèle_credit <- C5.0(default ~ ., data = credit_entrainement)
  prédiction_credit <- predict(modèle_credit, credit_test)
  réel_credit <- credit_test$default
  kappa <- kappa2(data.frame(réel_credit, prédiction_credit))$value
  return(kappa)
})

# Examiner les résultats des 10 essais
str(resultats_cv)

# Calculer la valeur moyenne du kappa sur les 10 essais
mean(unlist(resultats_cv))

# Calculer l'écart-type sur les 10 essais
sd(unlist(resultats_cv))

## Échantillonnage Bootstrap ----

# Calcul de la probabilité qu'un enregistrement ne soit pas sélectionné
(1 - (1/1000))^1000
(1 - (1/100000))^100000
1 / exp(1) # lorsque n approche l'infini
