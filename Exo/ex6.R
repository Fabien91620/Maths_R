# library(Ryacas)

#1
func <-function(x, n) 
{
  x^2*log(x)^n
}

#rslt <- sapply(1:10, function(n) {integrate(function(x) {func(x, n)}, lower = 1, upper = exp(1))$value})

rslt <- sapply(1:10, function(n) {integrate(func, lower=1, upper=exp(1), n=n)$value})
rslt


#2
pseudo_limit <- sapply(1:10000, function(n) {integrate(func, lower=1, upper=exp(1), n=n)$value})
pseudo_limit
pseudo_limit[10000]

# yac_str("Limit(x, Infinity) x")
# n <- ysym("n") 
# lim(n * integrate(func, lower=1, upper=exp(1)), n=n, Infinity)
# rsl <- yac_str("Limit(n, Infinity) n*integrate(func, lower=1, upper=exp(1), n=n)") 
# MaxEvalDepth(10000)
