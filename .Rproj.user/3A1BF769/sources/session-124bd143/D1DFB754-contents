##### Session-7 : Règles d'association 

## Exemple : Identification des achats fréquents d'épicerie 
## Étape 2 : Exploration et préparation des données 

# Charger les données d'épicerie dans une matrice creuse
install.packages("arules")
library(arules)
épicerie <- read.transactions("épicerie.csv", sep = ",")
summary(épicerie)

# Examiner le format long (sans décodage) pour voir les identifiants bruts des articles
head(toLongFormat(épicerie, decode = FALSE), n = 7)

# Regarder les cinq premières transactions
inspect(épicerie[1:5])

# Examiner la fréquence des articles
itemFrequency(épicerie[, 1:3])

# Tracer la fréquence des articles
itemFrequencyPlot(épicerie, support = 0.1)
itemFrequencyPlot(épicerie, topN = 20)

# Une visualisation de la matrice creuse des cinq premières transactions
image(épicerie[1:5])

# Visualisation d'un échantillon aléatoire de 100 transactions
image(sample(épicerie, 100))

## Étape 3 : Entraînement d'un modèle sur les données ----
library(arules)

# Les paramètres par défaut ne donnent aucune règle apprise
apriori(épicerie)

# Définir un meilleur support et des niveaux de confiance pour apprendre plus de règles
règles_épicerie <- apriori(épicerie, parameter = list(support =
                                                        0.006, confidence = 0.25, minlen = 2))
règles_épicerie

## Étape 4 : Évaluation de la performance du modèle ----
# Résumé des règles d'association d'épicerie
summary(règles_épicerie)

# Examiner les trois premières règles
inspect(règles_épicerie[1:3])

## Étape 5 : Amélioration de la performance du modèle ----

# Tri des règles d'épicerie par le lift
inspect(sort(règles_épicerie, by = "lift")[1:5])

# Recherche de sous-ensembles de règles contenant des articles de baies
règles_baies <- subset(règles_épicerie, items %in% "baies")
inspect(règles_baies)

# Écriture des règles dans un fichier CSV
write(règles_épicerie, file = "règles_épicerie.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# Conversion de l'ensemble de règles en un dataframe
règles_épicerie_df <- as(règles_épicerie, "data.frame")
str(règles_épicerie_df)

# Utilisation de l'algorithme eclat pour de meilleures performances
# D'abord, générer les ensembles d'articles fréquents
ensembles_articles_épicerie_eclat <- eclat(épicerie, support = 0.006)
inspect(ensembles_articles_épicerie_eclat[1:5])
# Ensuite, générer les règles
règles_épicerie_eclat <- ruleInduction(ensembles_articles_épicerie_eclat, confidence = 0.25)
règles_épicerie_eclat
inspect(règles_épicerie_eclat[1:5])
