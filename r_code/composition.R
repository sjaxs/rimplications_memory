union.eq <- function(Sigma,k,l){
  A <- read.left(Sigma,k)
  B <- read.right(Sigma,k) 
  C <- read.left(Sigma,l)
  D <- read.right(Sigma,l) 
  if(equals.sets(A,C) & !is.empty(B) & !is.empty(D)){
    BD <- union.sets(B,D)
    Sigma <- substitute.imp(Sigma,k,A,BD)
    Sigma <- remove.imp(Sigma,l)
  }
  return(Sigma)  
}#End union.eq
