simplify <- function(Sigma){
  fixpoint <- FALSE
  while (!fixpoint){
    fixpoint <- TRUE
    SigmaS <- Sigma
    Sigma <- Sigma[0]
    for (k in seq(SigmaS)){  
      Gamma <- SigmaS[0] 
      A <- read.left(SigmaS,k)
      B <- read.right(SigmaS,k) 
      for (l in seq(Sigma)){ 
        C <- read.left(Sigma,l)
        D <- read.right(Sigma,l) 
        if((is.included(C,A) & is.included(A,union.sets(C,D))) |
           (is.included(A,C) & is.included(C,union.sets(A,B)))){
          A <- intersection.sets(A,C)
          B <- union.sets(B,D)
        }else{
          if(is.included(A,C)){
            if(!is.included(D,B)){
              Gamma <- add.imp(Gamma, difference.sets(C,B), difference.sets(D,B))
            }
          }else{
            if(is.included(C,A)){
              A <- difference.sets(A,D)
              B <- difference.sets(B,D)
            }
            Gamma <- add.imp(Gamma, C, D)
          }
        }
      }
      if(is.empty(B)){
        Sigma <- Gamma
      }else{
        Gamma <- add.imp(Gamma, A, B)
        Sigma <- Gamma
      }
    }
    if(!equals.sets(Sigma, SigmaS)) fixpoint <- FALSE
  }
  return(SigmaS)
}#End simplify

