library(readxl)
library(corrplot)
library(psych)
library("FactoMineR")
library(factoextra)
library(cortest)


# Read CSV
data = read.csv2("C:/Users/Enzo/Desktop/Dossier/Enzo/ESGI/COURS - ESGI 4IABD 2/Mathématiques/Projet R/données.csv", row.names=1) #dec  =
data <- tail(data, 2)   
print(data)
str(data)

# 1)
data.matrice_corr <- cor(data) 
data.matrice_corr

data <- subset(data, select = -i2002)

data.matrice_corr <- cor(data) 
data.matrice_corr

# 2) 
corrplot(data.matrice_corr)

# 3) 
# Si Déterminant pas inférieur à 0.00001 elle n'est pas singulière // N'est pas une matrice identité car det != 1
det(data.matrice_corr)

# Calcul ACP
data.pca <- PCA(data, scale.unit = TRUE, graph = TRUE)
summary(data.pca)
print(data.pca)

data.valeurs_propres <- get_eigenvalue(data.pca)
data.valeurs_propres

# Seulement 1 axe car il représente à lui seul 100%% de l'inertie totale 
# Nul besoin de continuer l'ACP car il n'y a aucune réduction de dimension.
