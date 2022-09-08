# On considère trois suites (un), (vn), (wn) telles que : un+1 = un + 2wn, vn+1 = −vn, wn+1 = 2un + wn,
# avec u0 = 1, v0 = 2, w0 = 3
# En utilisant la diagonalisation, exprimer un, vn, wn en fonction de n.

install.packages("expm")
library(expm)

A = rbind(c(1, 0, 2), c(0, -1, 0), c(2, 0, 1))
X0 = c(1, 2, 3)
valeurs_propres = round(eigen(A)$values)
P = eigen(A)$vectors
D = diag(valeurs_propres)
inv_P = solve(P)

ex1=function(n){
  XN= round(P %*% D %^% n %*% inv_P%*% X0 , 2)
  XN
} 

ex1(10)

