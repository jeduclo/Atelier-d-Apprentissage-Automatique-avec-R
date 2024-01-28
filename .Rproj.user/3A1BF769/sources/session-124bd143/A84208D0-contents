##### Session-6 : Réseaux de neurones et Machines à vecteurs de support (SVM)

##### Partie 1 : Réseaux de neurones 
## Exemple : Modélisation de la résistance du béton  

## Étape 2 : Exploration et préparation des données 
# Lire les données et examiner leur structure
béton <- read.csv("béton.csv")
str(béton)

# Fonction de normalisation personnalisée
normaliser <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Appliquer la normalisation à l'ensemble du dataframe
béton_norm <- as.data.frame(lapply(béton, normaliser))

# Confirmer que la plage est maintenant entre zéro et un
summary(béton_norm$résistance)

# Comparé au minimum et au maximum d'origine
summary(béton$résistance)

# Créer les données d'entraînement et de test
béton_train <- béton_norm[1:773, ]
béton_test <- béton_norm[774:1030, ]

## Étape 3 : Entraînement d'un modèle sur les données ----
# Entraîner le modèle neuralnet
install.packages("neuralnet")
library(neuralnet)

# Réseau de neurones simple avec un seul neurone caché
set.seed(12345) # pour garantir des résultats reproductibles
modèle_béton <- neuralnet(résistance ~ ciment + laitier +
                            cendres + eau + superplastifiant + 
                            granulatsGros + granulatsFins + âge,
                          data = béton_train)

# Visualiser la topologie du réseau
plot(modèle_béton)

## Étape 4 : Évaluation de la performance du modèle ----
# Obtenir les résultats du modèle
résultats_modèle <- compute(modèle_béton, béton_test[1:8])
# Obtenir les valeurs de résistance prédites
résistance_prédite <- résultats_modèle$net.result
# Examiner la corrélation entre les valeurs prédites et réelles
cor(résistance_prédite, béton_test$résistance)

## Étape 5 : Amélioration de la performance du modèle ----
# Une topologie de réseau de neurones plus complexe avec 5 neurones cachés
set.seed(12345) # pour garantir des résultats reproductibles
modèle_béton2 <- neuralnet(résistance ~ ciment + laitier +
                             cendres + eau + superplastifiant + 
                             granulatsGros + granulatsFins + âge,
                           data = béton_train, hidden = 5)

# Tracer le réseau
plot(modèle_béton2)

# Évaluer les résultats comme précédemment
résultats_modèle2 <- compute(modèle_béton2, béton_test[1:8])
résistance_prédite2 <- résultats_modèle2$net.result
cor(résistance_prédite2, béton_test$résistance)

# Une topologie de réseau de neurones encore plus complexe avec deux couches cachées et une fonction d'activation personnalisée

# Créer une fonction d'activation personnalisée "softplus"
softplus <- function(x) { log(1 + exp(x)) }

set.seed(12345) # pour garantir des résultats reproductibles
modèle_béton3 <- neuralnet(résistance ~ ciment + laitier +
                             cendres + eau + superplastifiant + 
                             granulatsGros + granulatsFins + âge,
                           data = béton_train, hidden = c(5, 5), act.fct = softplus)

# Tracer le réseau
plot(modèle_béton3)

# Évaluer les résultats comme précédemment
résultats_modèle3 <- compute(modèle_béton3, béton_test[1:8])
résistance_prédite3 <- résultats_modèle3$net.result
cor(résistance_prédite3, béton_test$résistance)

# Noter que les valeurs prédites et réelles sont à des échelles différentes
résistances <- data.frame(
  réelle = béton$résistance[774:1030],
  prédite = résistance_prédite3
)

head(résistances, n = 3)

# La corrélation n'est pas affectée par la normalisation...
# ... mais des mesures comme l'erreur en pourcentage seraient affectées par le changement d'échelle !
cor(résistances$prédite, résistances$réelle)
cor(résistances$prédite, béton_test$résistance)

# Créer une fonction de désnormalisation pour inverser la normalisation
désnormaliser <- function(x) { 
  return(x * (max(béton$résistance) -
                min(béton$résistance)) + min(béton$résistance))
}

résistances$prédite_nouvelle <- désnormaliser(résistances$prédite)
résistances$erreur_pct <- (résistances$prédite_nouvelle - résistances$réelle) / résistances$réelle

head(résistances, n = 3)

# La corrélation reste la même malgré la désnormalisation
cor(résistances$prédite_nouvelle, résistances$réelle)

##### Partie 2 : Machines à vecteurs de support (SVM) -------------------
## Exemple : Reconnaissance optique de caractères ----

## Étape 2 : Exploration et préparation des données ----
# Lire les données et examiner leur structure
lettres <- read.csv("lettres.csv", stringsAsFactors = TRUE)
str(lettres)

# Diviser en données d'entraînement et de test
lettres_entrainement <- lettres[1:16000, ]
lettres_test  <- lettres[16001:20000, ]

## Étape 3 : Entraînement d'un modèle sur les données ----
# Commencer par entraîner un SVM linéaire simple
install.packages("kernlab")
library(kernlab)
classifieur_lettres <- ksvm(lettre ~ ., data = lettres_entrainement,
                            kernel = "vanilladot")

# Examiner des informations de base sur le modèle
classifieur_lettres

## Étape 4 : Évaluation de la performance du modèle ----
# Prédictions sur l'ensemble de test
prédictions_lettres <- predict(classifieur_lettres, lettres_test)

head(prédictions_lettres)

table(prédictions_lettres, lettres_test$lettre)

# Examiner uniquement l'accord vs. le désaccord
# Construire un vecteur de TRUE/FALSE indiquant les prédictions correctes/incorrectes
accord <- prédictions_lettres == lettres_test$lettre
table(accord)
prop.table(table(accord))

## Étape 5 : Amélioration de la performance du modèle ----

# Passer à un noyau RBF
set.seed(12345)
classifieur_lettres_rbf <- ksvm(lettre ~ ., data = lettres_entrainement, kernel = "rbfdot")
prédictions_lettres_rbf <- predict(classifieur_lettres_rbf, lettres_test)

accord_rbf <- prédictions_lettres_rbf == lettres_test$lettre
table(accord_rbf)
prop.table(table(accord_rbf))

# Tester diverses valeurs du paramètre de coût
valeurs_coût <- c(1, seq(from = 5, to = 40, by = 5))

valeurs_précision <- sapply(valeurs_coût, function(x) {
  set.seed(12345)
  m <- ksvm(lettre ~ ., data = lettres_entrainement,
            kernel = "rbfdot", C = x)
  pred <- predict(m, lettres_test)
  accord <- ifelse(pred == lettres_test$lettre, 1, 0)
  précision <- sum(accord) / nrow(lettres_test)
  return (précision)
})

plot(valeurs_coût, valeurs_précision, type = "b")
