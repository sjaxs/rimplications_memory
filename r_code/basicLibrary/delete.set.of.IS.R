delete.set.of.IS <- function(Sigma){
  k <- seq(Sigma)  
  Index <- !is.empty(read.right(Sigma,k))
  return(Sigma[Index])
}#End delete.set.of.IS
