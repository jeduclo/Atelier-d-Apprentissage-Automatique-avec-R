##### Session-4 : Classification en utilisant les arbres de décision et les règles 

#### Partie 1 : Arbres de décision 

## Compréhension des arbres de décision 
# Calcul de l'entropie d'un segment à deux classes
-0.60 * log2(0.60) - 0.40 * log2(0.40)

curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "Entropie", lwd = 4)

## Exemple : Identification des prêts bancaires risqués 
## Étape 2 : Exploration et préparation des données 
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

# Regarder deux caractéristiques du demandeur
table(credit$checking_balance)
table(credit$savings_balance)

# Regarder deux caractéristiques du prêt
summary(credit$months_loan_duration)
summary(credit$amount)

# Regarder la variable de classe
table(credit$default)

# Créer un échantillon aléatoire pour les données d'entraînement et de test
# Utiliser set.seed pour utiliser la même séquence de nombres aléatoires que le tutoriel
set.seed(9829)
train_sample <- sample(1000, 900)

str(train_sample)

# Diviser les data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# Vérifier la proportion de la variable de classe
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Étape 3 : Entraînement d'un modèle sur les données 
# Construire l'arbre de décision le plus simple
install.packages("C50")
library(C50)
credit_model <- C5.0(default ~ ., data = credit_train)

# Afficher des informations simples sur l'arbre
credit_model

# Afficher des informations détaillées sur l'arbre
summary(credit_model)

## Étape 4 : Évaluation de la performance du modèle 
# Créer un vecteur de facteurs de prédictions sur les données de test
credit_pred <- predict(credit_model, credit_test)

# Tableau croisé des classes prédites par rapport aux classes réelles
install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('défaut réel', 'défaut prédit'))

## Étape 5 : Amélioration de la performance du modèle 

## Amélioration de la précision des arbres de décision
# Arbre de décision amélioré avec 10 essais
credit_boost10 <- C5.0(default ~ ., data = credit_train,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('défaut réel', 'défaut prédit'))

## Rendre certaines erreurs plus coûteuses que d'autres

# Créer des dimensions pour une matrice de coûts
matrix_dimensions <- list(c("non", "oui"), c("non", "oui"))
names(matrix_dimensions) <- c("prédit", "réel")
matrix_dimensions

# Construire la matrice
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# Appliquer la matrice de coûts à l'arbre
credit_cost <- C5.0(default ~ ., data = credit_train,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('défaut réel', 'défaut prédit'))

#### Partie 2 : Apprentissage de règles 

## Exemple : Identification des champignons vénéneux 
## Étape 2 : Exploration et préparation des données 
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

# Examiner la structure du data frame
str(mushrooms)

# Supprimer la caractéristique veil_type
mushrooms$veil_type <- NULL

# Examiner la distribution des classes
table(mushrooms$type)

## Étape 3 : Entraînement d'un modèle sur les données 
install.packages("OneR")
library(OneR)

# Former OneR() sur les données
mushroom_1R <- OneR(type ~ ., data = mushrooms)

## Étape 4 : Évaluation de la performance du modèle 
mushroom_1R

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(réel = mushrooms$type, prédit = mushroom_1R_pred)

## Étape 5 : Amélioration de la performance du modèle 
install.packages("RWeka")
library(RWeka)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

# Apprentissage de règles à l'aide des arbres de décision C5.0 (non inclus dans le texte)
library(C50)
mushroom_c5rules <- C5.0(type ~ ., data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)
