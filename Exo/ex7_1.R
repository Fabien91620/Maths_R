library(readxl)
library(corrplot)
library(psych)
library("FactoMineR")
library(factoextra)
library(cortest)


# Read CSV
data = read.csv2("C:/Users/Enzo/Desktop/Dossier/Enzo/ESGI/COURS - ESGI 4IABD 2/Mathématiques/Projet R/données.csv", row.names=1) #dec  =
data <- head(data, - 2)    
print(data)
str(data)

# 1)
data.matrice_corr <- cor(data) 
data.matrice_corr

# 2) 
# x et y indépendantes =⇒ cov(x, y) = 0 ⇐⇒ cor(x, y) = 0
# Corrélation forte : |cor(x, y)| > 0.5
# Corrélation moyenne : 0.3 6 |cor(x, y)| 6 0.5

corrplot(data.matrice_corr)

# 3) 

#data.moy <- apply(data, 2, mean)
#data.centre <- sweep(data, 2, data.moy, "-")
#s <- apply(data, 2, sd) * sqrt(5/6) 
#data.centre_reduite <- sweep(data.centre, 2, s, "/")
#round(apply(data.centre_reduite, 2, sd) * sqrt(4/5), digit=1)
#print(data.centre_reduite, digits=2) 

# Test de Bartlett afin d'afirmer que matrice de corr n'est pas une matrice identité
bartlett_res <- cortest.bartlett(data.matrice_corr, n = NULL,diag=TRUE)
# bartlett_res <- bartlett.test(data.matrice_corr)
bartlett_res

# Si Déterminant pas inférieur à 0.00001 elle n'est pas singulière // N'est pas une matrice identité car det != 1
det(data.matrice_corr)

# Calcul ACP
data.pca <- PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE)
summary(data.pca)
print(data.pca)

data.valeurs_propres <- get_eigenvalue(data.pca)
data.valeurs_propres

fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 50))

# 2 axes principaux représentant 83.5% de l'inertie totale paraissent suffisant
# Ces 2 dimensions sont suffisament représentatifs et nous permettront d'avoir une représentation plus simple

# 4) Variables qui déterminent les deux premières composantes principales
data.pca_var <- get_pca_var(data.pca)
data.pca_var$contrib

fviz_contrib(data.pca, choice = "var", axes = 1, top = 10)
# c1999, c2000, c2002, c1998, c2001, c2004, c2003

fviz_contrib(data.pca, choice = "var", axes = 2, top = 10)
# i2001, i2003, i1999, i2002, i2000

fviz_contrib(data.pca, choice = "var", axes = 1:2, top = 10)
# c2002, c2001, c2000, c1999, c2003, c2004, c1998, i2003, i2002

# 5) Cercle des corrélations

fviz_pca_var(data.pca, col.var = "black")

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping,
)

# En interprétant le cercle des corrélations on se rend compte que : 
# Les variables les plus corrélées à Dim1 sont c1999, c2000, c2002, c2001, c1998, c2003, c2004 elles sont très corrélées positivement entre elles et forment un groupe 
# Les variables les plus corrélées à Dim2 sont i2001, i2003, i1999, i2002, i2000
# Aucune corrélation négatives pour Dim1 mais toutes les variables très positivement corrélées à Dim 1 sont négativement corrélées  à Dim2

# 6)

# La première composante principale capture un maximum d'informations -> chomage
# La seconde composante principale est orthogonale à la première, elle n'est pas linéairement corréelée à la première -> inflation

# 7)

data.pca_ind <- get_pca_ind(data.pca)
data.pca_ind$contrib

fviz_contrib(data.pca, choice = "ind", axes = 1)

fviz_contrib(data.pca, choice = "ind", axes = 2)

fviz_contrib(data.pca, choice = "ind", axes = 1:2)

fviz_pca_ind(data.pca, col.ind = "cos2", 
             gradient.cols = c("white", "#2E9FDF", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(data.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Les 4 individus qui déterminent le plus les deux axes principaux sont le Japon, l'Espagne, la Grèce et l'Irlande

# 8)

# L'ACP est très utile afin de synthétiser l'information, d'autant plus en compagnie de grands volumes de donneés
# De réduire le nombre de variables afin de rendre l'information moins redondante
# Grâce  à l'utilisation du package, nous pouvons aisément construires des représentations graphiques
# nous permettant de rendre le contenu plus facilement interprétable

data.pca$var$coord
