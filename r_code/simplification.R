simplification <- function(Sigma, A,B,C,D,k,l){
  if (is.included(A,C) ){
    CB <- difference.sets(C,B)
    DB <- difference.sets(D,B)
    if (is.empty(CB) | is.empty(DB)) { 
      Sigma <- Sigma[-l] 
    }else{
      if (!equals.sets(CB,C) | !equals.sets(DB,D)){
        Sigma <-substitute.imp(Sigma,l,CB,DB)
        if(equals.sets(CB,A)){
          Sigma <- composition.eq(Sigma,k,l)
          Sigma <- Sigma[-l]
        }
      }
    }
  }
  return(Sigma)
}#End simplification