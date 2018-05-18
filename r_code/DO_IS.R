SLgetDO <- function(Sigma){
  fixpoint <- FALSE
  SigmaR <- Sigma[0]
  for ( k in seq(Sigma)){
    A <- read.left(Sigma,k)
    B <- read.right(Sigma,k)
    if(!is.included(B,A) & !is.empty(A) & !is.empty(B)){
      SigmaR <- add.imp(SigmaR, A, difference.sets(B,A))
    }
  }
  Sigma <- simplify(SigmaR)
  while(!fixpoint){ 
    fixpoint <- TRUE
    SigmaDO <- Sigma
    Sigma <- Sigma[0]
    for(k in seq(SigmaDO)){ # A -> B
      Gamma <- Sigma[0]
      A <- read.left(SigmaDO,k)
      B <- read.right(SigmaDO,k)
      for(l in seq(Sigma)){ # C -> D
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
            #Aplicamos la simplificacion fuerte
            aux <- add.sSimpBoth(A,B,C,D,Sigma)
            
            if(cardinality.set(aux) != 0){
              Gamma <- add.imp(Gamma, read.left(aux,1), read.right(aux,1))
              if(cardinality.set(aux) == 2){
                Gamma <- add.imp(Gamma, read.left(aux,2), read.right(aux,2))
              }
            }
          }
        }
      }#End for l
      if(is.empty(B)){
        Sigma <- Gamma
      }else{
        Sigma <- add.imp(Gamma, A, B)
      }
    }#End for k
    if(!equals.sets(SigmaDO, Sigma)){
      fixpoint <- FALSE 
    }
  }
  SigmaDO <- apply.composition.eq(SigmaDO)
  
  return(SigmaDO)
}#end function SLgetDO



simplify <- function(Sigma){
  fixpoint <- FALSE
  
  while (!fixpoint){
    fixpoint <- TRUE
    SigmaS <- Sigma
    Sigma <- Sigma[0] #Empty Sigma
    for (k in seq(SigmaS)){  # A -> B
      Gamma <- SigmaS[0] #Empty Gamma
      A <- read.left(SigmaS,k)
      B <- read.right(SigmaS,k) 
      for (l in seq(Sigma)){ # C -> D
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
          }#end if
        }#end if
      }#end for l
      if(is.empty(B)){
        Sigma <- Gamma
      }else{
        Gamma <- add.imp(Gamma, A, B)
        Sigma <- Gamma
      }
    }#end for k
    
    if(!equals.sets(Sigma, SigmaS)){
      fixpoint <- FALSE
    }
  }#end while
  
  return(SigmaS)
}#end function simplify

