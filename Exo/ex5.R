#Exercice 5
#1
library(stringi)

palindrome = function(x) {
  if(stri_reverse(x)==x){
    return('mot est un palindrome')
  }
  else{
    return('mot est pas un palindrome')
  }
}
x1 = "fabien"
palindrome(x1)

#2
mot = ""
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("erreur.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

palindrome = function(i) {
  mgsub(c("á","é", "è","ó", " "), c("a","e","e","o",""), i)
  if(stri_reverse(i)==i){
    return('mot est un palindrome')
  }
  else{
    return('mot est pas un palindrome')
  }
}
x1 = "fabien"
palindrome(x1)

#3
foo <- c("tic", "tac", "toe", "RADAR", "RESSASSER", "LAVALLOISE", "fabiennnnnnn")
exo3 = function(x) {
  for (i in x) {
    if(nchar(i) > 8){
      mgsub(c("á","é", "è","ó", " "), c("a","e","e","o",""), i)
      if(stri_reverse(i)==i){
        print(i)
      }
    }
  }
}
exo3(foo)