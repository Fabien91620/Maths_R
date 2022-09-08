# Après lecture de l'énoncé on obtient : 
# P(X >= 75) = 0.1 et P(X <= 50) = 0.25

A <- matrix(c(qnorm(0.9), qnorm(0.25), 1, 1),nrow = 2)
A                          

B <- matrix(c(75, 50),nrow = 2)
B                         

solve(A, B)

# Le javelot parcourt en moyenne 58.62m et l'écart type est de 12.78m 


