included.left.right <- function(Sigma,k, l){
  uni1 <- union.sets(read.reft(v,l), read.right(Sigma,l))
  return(is.included(read.left(Sigma,k), uni1))
}#End included.left.right
