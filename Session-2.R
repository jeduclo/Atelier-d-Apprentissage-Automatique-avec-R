##### Session-2 : Classification en utilisant les voisins les plus proches --------------------

## Exemple : Classification des échantillons de cancer ----
## Étape 2 : Exploration et préparation des données ----

# Importer le fichier CSV
wbcd <- read.csv("wisc_bc_data.csv")

# Examiner la structure du data frame wbcd
str(wbcd)

# Supprimer la caractéristique "id"
wbcd <- wbcd[-1]

# Tableau du diagnostic
table(wbcd$diagnosis)

# Recoder le diagnostic en tant que facteur
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Bénin", "Maligne"))

# Tableau de proportions avec des étiquettes plus informatives
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Résumer trois caractéristiques numériques
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# Créer une fonction de normalisation
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Tester la fonction de normalisation - le résultat devrait être identique
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# Normaliser les données wbcd
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# Confirmer que la normalisation a fonctionné
summary(wbcd_n$area_mean)

# Créer des données d'entraînement et de test
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# Créer des étiquettes pour les données d'entraînement et de test

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Étape 3 : Entraînement d'un modèle sur les données ----

# Charger la bibliothèque "class"
install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Étape 4 : Évaluation de la performance du modèle ----

# Charger la bibliothèque "gmodels"
install.packages("gmodels")
library(gmodels)

# Créer le tableau croisé des prédictions par rapport aux valeurs réelles
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Étape 5 : Amélioration de la performance du modèle ----

# Utiliser la fonction scale() pour standardiser en z-score un data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# Confirmer que la transformation a été appliquée correctement
summary(wbcd_z$area_mean)

# Créer des ensembles de données d'entraînement et de test
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# Reclassifier les cas de test
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Créer le tableau croisé des prédictions par rapport aux valeurs réelles
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# Tester plusieurs valeurs différentes de k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

k_values <- c(1, 5, 11, 15, 21, 27)

# Boucle for pour tester toutes les valeurs de k dans le vecteur k_values
for (k_val in k_values) {
  wbcd_test_pred <- knn(train = wbcd_train,
                        test = wbcd_test,
                        cl = wbcd_train_labels,
                        k = k_val)
  CrossTable(x = wbcd_test_labels,
             y = wbcd_test_pred,
             prop.chisq = FALSE)
}
