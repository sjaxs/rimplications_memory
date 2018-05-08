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



add.sSimp <- function(A,B,C,D,Sigma){
  
  aux1 <- intersection.sets(B,C)
  aux2 <- difference.sets(D, union.sets(A,B))
  Sigma2 <- Sigma[0]
  
  if(!is.included(A,C) & !is.empty(aux1) & !is.empty(aux2) & !equals.sets(aux1,aux2)){
    E <- union.sets(A, difference.sets(C,B))
    G <- difference.sets(D, union.sets(A,B))
    for (k in seq(Sigma)){  # X -> Y
      X <- read.left(Sigma,k)
      Y <- read.right(Sigma,k)
      if(is.included(X,E)){
        if(!is.included(G,Y)){
          E <- difference.sets(E,Y)
          G <- difference.sets(G,Y)
        }
      }
    }#End for
    Sigma2 <- add.imp(Sigma2, E, G)
  }#end if
  
  return(Sigma2)
}#end function add.sSimp



add.sSimpBoth <- function(A,B,C,D,Sigma){
  aux1 <- intersection.sets(B,C)
  aux2 <- difference.sets(D, union.sets(A,B))
  aux3 <- intersection.sets(D,A)
  aux4 <- difference.sets(B, union.sets(C,D))
  Sigma2 <- Sigma[0]
  cont <- '0'
  
  if(!is.included(A,C) & !is.empty(aux1) & !is.empty(aux2) & !equals.sets(aux1,aux2)){
    cont <- '1'
  }
  if(!is.included(C,A) & !is.empty(aux3) & !is.empty(aux4) & !equals.sets(aux3,aux4)){
    if(cont == '1'){
      cont <- '3'
    }else{
      cont <- '2'
    }
  }
  switch(cont,
         '0'={ #If cont == 0
           #Do nothing
         },

         '1'={ #If cont == 1 then A->B, C->D
           E <- union.sets(A, difference.sets(C,B))
           G <- difference.sets(D, union.sets(A,B))
           k <- 1
           while (k <= length(Sigma) & !is.empty(E) & !is.empty(G)){  # X -> Y
             X <- read.left(Sigma,k)
             Y <- read.right(Sigma,k)
             if(is.included(X,E) & !is.included(G,Y)){
               E <- difference.sets(E,Y)
               G <- difference.sets(G,Y)
             }
             k <- k + 1
           }#End while
           Sigma2 <- add.imp(Sigma2, E, G)
         },

         '2'={ #If cont == 2 then C->D, A->B
           H <- union.sets(C, difference.sets(A,D))
           I <- difference.sets(B, union.sets(C,D))
           k <- 1
           while (k <= length(Sigma) & !is.empty(H) & !is.empty(I)){  # X -> Y
             X <- read.left(Sigma,k)
             Y <- read.right(Sigma,k)
             if(is.included(X,H) & !is.included(I,Y)){
               H <- difference.sets(H,Y)
               I <- difference.sets(I,Y)
             }
             k <- k + 1
           }#End while
           Sigma2 <- add.imp(Sigma2, H, I)
         },

         '3'={ #If cont == 3 then A->B, C->D & C->D, A->B
           E <- union.sets(A, difference.sets(C,B))
           G <- difference.sets(D, union.sets(A,B))
           H <- union.sets(C, difference.sets(A,D))
           I <- difference.sets(B, union.sets(C,D))
           empty1 <- FALSE
           empty2 <- FALSE
           k <- 1
           while(k <= length(Sigma) & (!empty1 | !empty2)){
           	 X <- read.left(Sigma,k)
             Y <- read.right(Sigma,k)
             if(!empty1 & !is.empty(E) & !is.empty(G)){
               if(is.included(X,E) & !is.included(G,Y)){
                 E <- difference.sets(E,Y)
                 G <- difference.sets(G,Y)
               }
             }else{
               empty1 <- TRUE
             }
             if(!empty2 & !is.empty(H) & !is.empty(I)){
               if(is.included(X,H) & !is.included(I,Y)){
                 H <- difference.sets(H,Y)
                 I <- difference.sets(I,Y)
               }
             }else{
               empty2 <- TRUE
             }
             k <- k + 1
           }#End while
           Sigma2 <- add.imp(Sigma2, E, G)
           Sigma2 <- add.imp(Sigma2, H, I)
         }#End case3
  )#End switch
 
  return(Sigma2)
}#end function add.sSimpBoth
