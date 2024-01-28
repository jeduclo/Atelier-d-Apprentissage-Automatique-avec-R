##### Session-13: Construction de meilleurs modèles d'apprentissage 

# Charger l'ensemble de données de crédit
credit <- read.csv("credit.csv")
library(caret)

## Création d'un modèle simple ajusté 

# Rechercher les paramètres d'ajustement pour C5.0
modelLookup("C5.0")

# Ajustement automatique des paramètres du modèle d'arbre de décision C5.0
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# Résumé des résultats de l'ajustement
m

# Appliquer le meilleur modèle candidat C5.0 pour effectuer des prédictions
p <- predict(m, credit)
table(p, credit$default)

# Obtenir les classes prédites
head(predict(m, credit))

# Obtenir les probabilités prédites
head(predict(m, credit, type = "prob"))

## Personnalisation du processus d'ajustement 
# Utilisation de trainControl() pour modifier la stratégie de rééchantillonnage
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# Utilisation de expand.grid() pour créer une grille de paramètres d'ajustement
grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)

# Examiner le résultat de expand.grid()
grid

# Personnaliser train() avec la liste de contrôle et la grille de paramètres
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)

# Voir les résultats
m

## Bagging ----
# Utilisation des arbres de décision baggés ipred
install.packages("ipred")
library(ipred)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# Estimer les performances des arbres baggés ipred
library(caret)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

## Boosting ----

## Utilisation de l'arbre de décision C5.0 (non montré dans le livre)
install.packages("C50")
library(C50)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

# Création d'un modèle Adaboost.M1
install.packages("adabag")
library(adabag)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

# Création et évaluation d'un modèle Adaboost.M1 avec une validation croisée à 10 plis
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

# Calcul du kappa
install.packages("vcd")
library(vcd)
Kappa(adaboost_cv$confusion)

## Forêts aléatoires ----
# Forêt aléatoire avec les paramètres par défaut
install.packages("randomForest")
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

# Calcul du kappa sur l'estimation out-of-bag
library(vcd)
Kappa(rf$confusion[1:2,1:2])

# Ranger est une implémentation plus rapide de l'algorithme des forêts aléatoires
install.packages("ranger")
library(ranger)
set.seed(300)
m_ranger <- ranger(default ~ ., data = credit)
m_ranger

# Calcul du kappa
Kappa(m_ranger$confusion.matrix)

## Machines à gradient boosting (GBM) ----

# Préparer les données pour gbm()
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit$default <- ifelse(credit$default == "yes", 1, 0)

# Créer un échantillon d'entraînement et de test aléatoire
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# Créer un modèle GBM avec les paramètres par défaut
install.packages("gbm")
library(gbm)
set.seed(300)
m_gbm <- gbm(default ~ ., data = credit_train)
m_gbm

# Évaluer le modèle GBM simple
p_gbm <- predict(m_gbm, credit_test, type = "response")
p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)

# Calcul du kappa
library(vcd)
Kappa(table(credit_test$default, p_gbm_c))

# Créer un modèle gbm() ajusté en utilisant caret
# Commencez par créer la grille d'ajustement
grid_gbm <- expand.grid(
  n.trees = c(100, 150, 200),
  interaction.depth = c(1, 2, 3),
  shrinkage = c(0.01, 0.1, 0.3),
  n.minobsinnode = 10
)

# Définir les paramètres de l'expérience
library(caret)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")

# Exécuter l'expérience caret
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_gbm_c <- train(default ~ ., data = credit, method = "gbm",
                 trControl = ctrl, tuneGrid = grid_gbm,
                 metric = "Kappa",
                 verbose = FALSE)

# Voir les résultats
m_gbm_c

## Extreme Gradient Boosting (XGB) 

# Formater les données de crédit sous forme de matrice creuse
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
library(Matrix)
credit_matrix <- sparse.model.matrix(~ . -default, data = credit)

# Examiner la matrice creuse credit_matrix
dim(credit_matrix)
print(credit_matrix[1:5, 1:15])

# Supprimer l'intercept
credit_matrix <- credit_matrix[, -1] 

# Diviser la matrice en train et test
set.seed(12345)
train_ids <- sample(1000, 900)
credit_train <- credit_matrix[train_ids, ]
credit_test <- credit_matrix[-train_ids, ]

# Vérifier que les lignes sont 900 vs 100 et les colonnes sont 35 vs 35
dim(credit_train)
dim(credit_test)

# Créer des vecteurs 1/0 pour les données d'entraînement et de test indiquant le défaut de prêt
credit_train_labels <-
  ifelse(credit[train_ids, c("default")] == "yes", 1, 0)
credit_test_labels <-
  ifelse(credit[-train_ids, c("default")] == "yes", 1, 0)

# Construire le modèle xgboost
install.packages("xgboost")
library(xgboost)

# Définir les hyperparamètres XGB
params.xgb <- list(objective   = "binary:logistic",
                   max_depth   = 6,
                   eta         = 0.3,
                   gamma       = 0,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1)

set.seed(555)
xgb_credit <- xgboost(params  = params.xgb,
                      data    = credit_train,
                      label   = credit_train_labels, 
                      nrounds = 100,
                      print_every_n = 10,
                      verbose = 1)

# Faire des prédictions
prob_default <- predict(xgb_credit, credit_test)
pred_default <- ifelse(prob_default > 0.50, 1, 0)

# Créer une matrice de confusion
table(pred_default, credit_test_labels)

# Calculer le kappa
library(vcd)
Kappa(table(pred_default, credit_test_labels))

# Créer un modèle xgboost() ajusté en utilisant caret
# Commencer par créer la grille d'ajustement
grid_xgb <- expand.grid(
  eta = c(0.3, 0.4),
  max_depth = c(1, 2, 3),
  colsample_bytree = c(0.6, 0.8),
  subsample = c(0.50, 0.75, 1.00),
  nrounds = c(50, 100, 150),
  gamma = c(0, 1),
  min_child_weight = 1
)

# Définir l'objet de contrôle
library(caret)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")

# Exécuter l'expérience caret
set.seed(300)
m_xgb <- train(default ~ ., data = credit, method = "xgbTree",
               trControl = ctrl, tuneGrid = grid_xgb,
               metric = "Kappa", verbosity = 0)

# Voir les résultats de tous les modèles (non montré dans le livre en raison de la taille de la sortie)
m_xgb

# Voir les hyperparamètres pour le modèle le mieux performant
m_xgb$bestTune

# Obtenir le meilleur kappa parmi les 216 modèles testés
max(m_xgb$results["Kappa"])
