##### Session-8 : Clustering avec k-means 

## Exemple : Identification des segments de marché pour les adolescents 
## Étape 2 : Exploration et préparation des données 
adolescents <- read.csv("snsdata.csv", stringsAsFactors = TRUE)
str(adolescents)

# Examiner les données manquantes pour la variable sexe (gender)
table(adolescents$gender)
table(adolescents$gender, useNA = "ifany")

# Examiner les données manquantes pour la variable âge
summary(adolescents$age)

# Éliminer les valeurs aberrantes pour l'âge
adolescents$age <- ifelse(adolescents$age >= 13 & adolescents$age < 20,
                          adolescents$age, NA)

summary(adolescents$age)

# Réaffecter les valeurs de sexe manquantes à "inconnu"
adolescents$féminin <- ifelse(adolescents$gender == "F" &
                                !is.na(adolescents$gender), 1, 0)
adolescents$sans_sexe <- ifelse(is.na(adolescents$gender), 1, 0)

# Vérifier notre travail de recodage
table(adolescents$gender, useNA = "ifany")
table(adolescents$féminin, useNA = "ifany")
table(adolescents$sans_sexe, useNA = "ifany")

# Trouver l'âge moyen par cohorte
mean(adolescents$age) # ne fonctionne pas
mean(adolescents$age, na.rm = TRUE) # fonctionne

# Âge par cohorte
aggregate(data = adolescents, age ~ gradyear, mean, na.rm = TRUE)

# Créer un vecteur avec l'âge moyen pour chaque année de promotion, répété par personne
âge_moyen <- ave(adolescents$age, adolescents$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))

adolescents$age <- ifelse(is.na(adolescents$age), âge_moyen, adolescents$age)

# Vérifier les résultats du résumé pour s'assurer que les valeurs manquantes sont éliminées
summary(adolescents$age)

## Étape 3 : Entraînement d'un modèle sur les données ----

# Créer un dataframe standardisé en z-score pour une interprétation plus facile
intérêts <- adolescents[5:40]
intérêts_z <- as.data.frame(lapply(intérêts, scale))

# Comparer les données avant et après la transformation
summary(intérêts$basketball)
summary(intérêts_z$basketball)

# Créer les clusters en utilisant k-means
set.seed(2345)
clusters_adolescents <- kmeans(intérêts_z, 5)

## Étape 4 : Évaluation de la performance du modèle ----
# Examiner la taille des clusters
clusters_adolescents$size

# Examiner les centres des clusters
clusters_adolescents$centers

# Optionnel : visualiser les clusters en utilisant le package factoextra
# (la sortie n'est pas affichée dans le manuel)
install.packages("factoextra")
library(factoextra)
fviz_cluster(clusters_adolescents, intérêts_z, geom = "point")

## Étape 5 : Amélioration de la performance du modèle ----
# Appliquer les identifiants de cluster au dataframe original
adolescents$cluster <- clusters_adolescents$cluster

# Examiner les cinq premiers enregistrements
adolescents[1:5, c("cluster", "gender", "age", "friends")]

# Âge moyen par cluster
aggregate(data = adolescents, age ~ cluster, mean)

# Proportion de femmes par cluster
aggregate(data = adolescents, féminin ~ cluster, mean)

# Nombre moyen d'amis par cluster
aggregate(data = adolescents, friends ~ cluster, mean)
