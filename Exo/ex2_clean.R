library(rootSolve)
library(calculus)
library(comprehenr)
library(scatterplot3d)
library(ggplot2)

# I)

funcF = function(x,y){
  return(exp(x*y) - exp(x) + 2)
}

funcToGraph = function(func, min , max , extremums){
  x <- c()
  y <- c()
  z <- c()
  
  plage_data = seq(min,max,by=0.01)
  
  for(i in plage_data)
    for(i2 in plage_data){
      y = c(y , i2)
      x = c(x , i)
      z = c(z , func(i,i2))
    }
  
  colors <- rep("blue", length(x))
  for(point in extremums){
    x = append(x , point[1])
    y = append(y , point[2])
    z = append(z , point[3])
    colors = append(colors , "red")
  }
  
  print(c(length(x) ,length(y),length(z)))
  scatterplot3d(x=x, y=y , z=z ,
                type="p", 
                main="Function plot sur [0,2]",
                color=colors,
                pch=16,
                zlab = "func(x,y)"
  )
}

extrems = list(c(1,1,1),c(2,2,2),c(3,3,3))
funcToGraph(funcF, 0 , 2 ,extrems)


f <- expression(exp(x*y) - exp(x) + 2)
f_deriv_x = stats::D(f, "x")
f_deriv_y = stats::D(f, "y")
f_model = function(e) {
  x = e[1]
  y = e[2]
  F1 <- eval(f_deriv_x )
  F2 <- eval(f_deriv_y )
  c(F1=F1, F2=F2)
}

# Vérifier que le package rootSolve est bien activé (cf Erreur : could not find function "multiroot")
f_rslt<- multiroot(f = f_model, start = c(1, 1))
f_pts_critiques <- f_rslt$root
f_pts_critiques <- round(f_pts_critiques, digits = 0)
f_pts_critiques

# On obtient le point critique (0,1)
# Calcul de la hessienne

f_vars <- c("x", "y")
# Get the symbolic hessian
calculus::hessian(f = f, var = f_vars)

# On vient remplacer les inconnues par les cordonnées de chaque point point critique

# Get the hessian evaluated at a specific point
f_hessian_matrix <- calculus::hessian(f = f, var = c('x' = f_pts_critiques[1], 'y' = f_pts_critiques[2]))
f_hessian_matrix

# On regarde les valeurs propres de la matrice hessienne obtenue

f_hessian_matrix_valeurs_propres = round(eigen(f_hessian_matrix)$values)
f_hessian_matrix_valeurs_propres

# La matrice hessienne admet deux valeurs propres de signes contraire, (0,1) est un point selle

# ---------------------------------------------------------------------------------------------

# II)
g = expression((x+z^2) * exp(x^(y^2 + z^2 +1)))
g_deriv_x = stats::D(g, "x")
g_deriv_y = stats::D(g, "y")
g_deriv_z = stats::D(g, "z")

g_model = function(e) {
  x = e[1]
  y = e[2]
  z = e[3]
  F1 <- eval(g_deriv_x)
  F2 <- eval(g_deriv_y)
  F3 <- eval(g_deriv_z)
  c(F1=F1, F2=F2, F3=F3)
}

g_rslt <- multiroot(f = g_model, start=c(1, 1, 1))
g_pts_critiques <- g_rslt$root
g_pts_critiques <- round(g_pts_critiques, digits = 0)
g_pts_critiques

g_rslt_bis <- multiroot(f = g_model, start=c(1, 1, 1)*2)
g_pts_critiques_bis <- g_rslt_bis$root
g_pts_critiques_bis <- round(g_pts_critiques_bis, digits = 0)
g_pts_critiques_bis

# On obtient le point critique (1, -435, 1)

g_vars <- c("x", "y", "z")
calculus::hessian(f = g, var = g_vars)
g_hessian_matrix <- calculus::hessian(f = g, var = c('x' = g_pts_critiques[1], 'y' = g_pts_critiques[2], 'z' = g_pts_critiques[3]))
g_hessian_matrix

g_hessian_matrix_valeurs_propres = round(eigen(g_hessian_matrix)$values)
g_hessian_matrix_valeurs_propres

# La matrice hessienne admet un minimum en (1, -435, 1) car toutes les valeurs propres sont de signes positif

g_hessian_matrix_bis <- calculus::hessian(f = g, var = c('x' = g_pts_critiques_bis[1], 'y' = g_pts_critiques_bis[2], 'z' = g_pts_critiques_bis[3]))
g_hessian_matrix_bis

g_hessian_matrix_bis_valeurs_propres = round(eigen(g_hessian_matrix_bis)$values)
g_hessian_matrix_bis_valeurs_propres

# La matrice hessienne admet des valeurs propres de signes contraire, (2, 2, 2) est un point selle

# ---------------------------------------------------------------------------------------------

# III)
h = expression(log(x*y*z) - log(x) * log(y) * log(z))
h_deriv_x = stats::D(h, "x")
h_deriv_y = stats::D(h, "y")
h_deriv_z = stats::D(h, "z")

h_model = function(e) {
  x = e[1]
  y = e[2]
  z = e[3]
  F1 <- eval(h_deriv_x)
  F2 <- eval(h_deriv_y)
  F3 <- eval(h_deriv_z )
  c(F1=F1, F2=F2, F3=F3)
}

h_rslt <- multiroot(f = h_model, start=c(1, 1, 1))
h_pts_critiques <- h_rslt$root
h_pts_critiques <- round(h_pts_critiques, digits = 0)
h_pts_critiques

h_rslt_bis <- multiroot(f = h_model, start=c(2, 1, 2)*2)
h_pts_critiques_bis <- h_rslt_bis$root
h_pts_critiques_bis <- round(h_pts_critiques_bis, digits = 0)
h_pts_critiques_bis

# On obtient le point critique (3, 3, 3)

h_vars <- c("x", "y", "z")
calculus::hessian(f = h, var = h_vars)
h_hessian_matrix <- calculus::hessian(f = h, var = c('x' = h_pts_critiques[1], 'y' = h_pts_critiques[2], 'z' = h_pts_critiques[3]))
h_hessian_matrix

h_hessian_matrix_valeurs_propres = round(eigen(h_hessian_matrix)$values)
h_hessian_matrix_valeurs_propres

# La matrice hessienne admet un minimum en (1, -435, 1) car toutes les valeurs propres sont de signes positif

h_hessian_matrix_bis <- calculus::hessian(f = h, var = c('x' = h_pts_critiques_bis[1], 'y' = h_pts_critiques_bis[2], 'z' = h_pts_critiques_bis[3]))
h_hessian_matrix_bis

h_hessian_matrix_bis_valeurs_propres = round(eigen(h_hessian_matrix_bis)$values)
h_hessian_matrix_bis_valeurs_propres

# La matrice hessienne admet des valeurs propres de signes contraire, (1, -435, 1) est un point selle
