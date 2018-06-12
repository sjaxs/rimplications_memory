rsimplification <- function(Sigma,A,B,C,D,k,l){
  CD <- union.sets(C,D)
  if (is.included(A,CD)){
    DB <- difference.sets(D,B)
    if (is.empty(DB)){
      Sigma <- Sigma[-l] 
    }else{
      if (!equals.sets(DB,D)){
        Sigma <-substitute.imp(Sigma,l,C,DB)
      }
    }
  }
  return(Sigma)
}#End rsimplification