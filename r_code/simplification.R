simplification <- function(Sigma, A,B,C,D,k,l){
  if (is.included(A,C) ){ #si A esta contenido en C
    CB <- difference.sets(C,B)
    DB <- difference.sets(D,B)
    if (is.empty(CB) | is.empty(DB)) { # si la regla es 
      # igual en alguno de los lados borro l
      Sigma <- Sigma[-l] #regla vacia, borro
    }else{
      if (!equals.sets(CB,C) | !equals.sets(DB,D)){
        Sigma <-substitute.imp(Sigma,l,CB,DB)
        if(equals.sets(CB,A)){
          Sigma <- composition.eq(Sigma,k,l)
          Sigma <- Sigma[-l]
        }
      }#END IF
    }#end else
  }#end if is.included
  return(Sigma)
}