substitute.imp <- function(Sigma, k, newC, newD){
  Sigma <- add.imp.k(Sigma, newC, difference.sets(newD,newC), k)
  return(Sigma)
}#End substitute.imp
