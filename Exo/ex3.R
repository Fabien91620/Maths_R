library(Rsolnp)
library(alabama) 

# 1)
f1 <- function(e) {
  x = e[1]
  y = e[2]
  x * y
}

eqF1 <- function(e) {  
  x = e[1]
  y = e[2]
  (x - y + 6)
}

solonp_f1_rslt <- solnp(pars = c(0, 100), fun = f1, eqfun = eqF1, eqB = 0)
auglag_f1_rslt <- auglag(par = c(0, 100), fn = f1, heq = eqF1)

pt_critique <- auglag_f1_rslt$par
pt_critique
hessian <- auglag_f1_rslt$hessian
valeurs_propres <- round(eigen(hessian)$values)
valeurs_propres

# 2)

f2 <- function(e) {
  x = e[1]
  y = e[2]
  z = e[3]
  x^2 + y^2 + z^2
}

eqF1 <- function(e) {  
  x = e[1]
  y = e[2]
  (x + y + z - 1)
}

solnp(pars = c(0, 100), fun = f1, eqfun = eqF1, eqB = 0)
rslt <- auglag(par = c(0, 100), fn = f1, heq = eqF1)

resSOLNP <- solnp(pars   = c(P),                             
                  fun    = eval_f,                             
                  eqfun  = f_constr_1,                            
                  eqB    = rep(0,n),                             
                  LB     = rep(0,length(P)),                            
                  UB     = rep(1,length(P)),                            
                  L = list(z,t1,t2))

# 4)

f4 <- function(e) {
  x = e[1]
  y = e[2]
  x^2 * y
}

eqF4 <- function(e) {  
  x = e[1]
  y = e[2]
  (2 * x^2 + y^2 - 3)
}

solonp_f4_rslt <- solnp(pars = c(0, 100), fun = f4, eqfun = eqF4, eqB = 0)
auglag_f4_rslt <- auglag(par = c(0, 100), fn = f4, heq = eqF4)
solonp_f4_rslt$pars
auglag_f4_rslt$par

hessian_f4 <- auglag_f4_rslt$hessian
valeurs_propres_f4 <- round(eigen(hessian_f4)$values)
valeurs_propres_f4

# 5)

f5 <- function(e) {
  x = e[1]
  y = e[2]
  (x^2 * y^2)/(x^2 + y^2) + 2*x^2 + 2*y^2 + 4*x*y + 5*x + 5*y + 20
}

eqF5 <- function(e) {  
  x = e[1]
  y = e[2]
  2*x^2 + 2*y^2 + 4*x*y - 11
}

solonp_f5_rslt <- solnp(pars = c(0, 100), fun = f5, eqfun = eqF5, eqB = 0) # basé sur le solver SQP soit SLSQP ?
auglag_f5_rslt <- auglag(par = c(0, 100), fn = f5, heq = eqF5) #, localsolver="lbfgs", method="nlminb"
solonp_f5_rslt$pars
auglag_f5_rslt$par

hessian_f5 <- auglag_f5_rslt$hessian
valeurs_propres_f5 <- round(eigen(hessian_f5)$values)
valeurs_propres_f5
