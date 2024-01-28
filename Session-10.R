##### Session-10 : Utilisation de ggplot2 pour l'exploration visuelle des données  

# Lire l'ensemble de données d'apprentissage du Titanic
titanic_train <- read.csv("titanic_train.csv")

# Examiner les caractéristiques
str(titanic_train)

# Charger le package ggplot2 et l'appliquer à l'ensemble de données du Titanic
library(ggplot2)
p <- ggplot(data = titanic_train)
p # crée un graphique vide en gris

# Comparer le boxplot intégré au boxplot ggplot2
boxplot(titanic_train$Age) # utilise boxplot() intégré à R
p + geom_boxplot(mapping = aes(y = Age)) # utilise boxplot ggplot2

# Boxplot examinant la relation entre l'âge et la survie
p + geom_boxplot(mapping = aes(x = Age, y = as.factor(Survived)))

# Comparer l'histogramme intégré à l'histogramme ggplot2
hist(titanic_train$Age) # utilise hist() intégré à R
p + geom_histogram(aes(x = Age)) # utilise l'histogramme ggplot2

# Histogrammes superposés
p + geom_histogram(aes(x = Age, fill = as.factor(Survived))) +
  ggtitle("Répartition de l'âge par statut de survie du Titanic")

# Histogrammes côte à côte
p + geom_histogram(aes(x = Age)) +
  facet_grid(cols = vars(Survived)) +
  ggtitle("Répartition de l'âge par statut de survie du Titanic")

# Tracés de densité superposés
p + geom_density(aes(x = Age,
                     color = as.factor(Survived),
                     fill = as.factor(Survived)),
                 alpha = 0.25) +
  ggtitle("Densité de l'âge par statut de survie du Titanic")

# Diagramme à barres du nombre de passagers par sexe
p + geom_bar(aes(x = Sex)) +
  ggtitle("Nombre de passagers du Titanic par sexe")

# Diagramme à barres de la probabilité de survie par sexe
p + geom_bar(aes(x = Sex, y = Survived),
             stat = "summary", fun = "mean") +
  ggtitle("Taux de survie du Titanic par sexe")

# Diagramme à barres de la probabilité de survie par classe de passagers
p + geom_bar(aes(x = Pclass, y = Survived),
             stat = "summary", fun = "mean") +
  ggtitle("Taux de survie du Titanic par classe de passagers")

# Diagramme à barres empilé de la survie par classe de passagers
p + geom_bar(aes(x = Pclass,
                 fill = factor(Survived,
                               labels = c("Non", "Oui")))) +
  labs(fill = "Survie") +
  ylab("Nombre de passagers") +
  ggtitle("Comptage de la survie du Titanic par classe de passagers")

# Statut de survie par classe de passagers
p + geom_bar(aes(x = Pclass,
                 fill = factor(Survived,
                               labels = c("Non", "Oui"))),
             position = "fill") +
  labs(fill = "Survie") +
  ylab("Proportion de passagers") +
  ggtitle("Survie du Titanic par classe de passagers")

# Diagramme à barres de l'interaction entre la classe et le sexe
p + geom_bar(aes(x = Pclass, y = Survived, fill = Sex),
             position = "dodge", stat = "summary", fun = "mean") +
  ylab("Proportion de survie") +
  ggtitle("Taux de survie du Titanic par classe et sexe")
