apply.reduction.eq <- function(Sigma){
  for (k in seq(Sigma))
    Sigma <- reduction.eq(Sigma,k)
  return(Sigma)
}#End apply.reduction.eq


apply.composition.eq.old <- function(Sigma){
  delete <- c()
  for (k in seq(Sigma)){
    for (l in seq(Sigma))
      if (k<l){ 
        Sigma <- composition.eq(Sigma,k,l)
      }
  }
  Sigma <- delete.set.of.IS(Sigma)
  return(Sigma)
}#End apply.composition.eq.old

apply.union.eq <- function(A,B,Sigma){
  for (k in seq(Sigma)){
    C <- Sigma[k]$lhs
    D <- Sigma[k]$rhs
    if(equals.sets(A,C) & !equal.sets(B,D)){
      BD <- union.sets(B,D)
      Sigma <- substitute.imp(Sigma,k,C,BD)
      return(Sigma)  
    }#end if
  }#end for
  return(Sigma)         
}#End apply.union.eq

simplification <- function(Sigma, A,B,C,D,k,l){
  N<-FALSE
  fuera <- FALSE
  fixpoint <- TRUE
  if (is.included(A,C) ){ #si A esta contenido en C
    CB <- difference.sets(C,B)
    DB <- difference.sets(D,B)
    if (is.empty(CB) | is.empty(DB)) { # si la regla es 
      # igual en alguno de los lados borro l
      Sigma <- Sigma[-l] #regla vacia, borro
      fixpoint <- FALSE
      N <- TRUE
    }else{
      if (!equals.sets(CB,C) | !equals.sets(DB,D)){
        Sigma <-substitute.imp(Sigma,l,CB,DB)
        if(equals.sets(CB,A)){
          Sigma <- composition.eq(Sigma,k,l)
          Sigma <- Sigma[-l]
          fixpoint <- FALSE
          N <- TRUE
        }
        fixpoint <- FALSE
      }#END IF
    }#end else
  }#end if is.included
  return(list("Sol" = Sigma, "Fixpoint" = fixpoint, "Fuera" = fuera, "Next" = N))
}

rsimplification <- function(Sigma, A,B,C,D,k,l){
  N<-FALSE
  fuera <- FALSE
  fixpoint <- TRUE
  CD <- union.sets(C,D)
  if (is.included(A,CD) ){
    DB <- difference.sets(D,B)
    if (is.empty(DB)) {
      Sigma <- Sigma[-l] #regla vacia, borro
      fixpoint <- FALSE
      N <- TRUE
    }else{
      if ( !equals.sets(DB,D)){
        Sigma <-substitute.imp(Sigma,l,C,DB)
        fixpoint <- FALSE
      }#END IF
    }#end else
  }#end if is.included
  return(list("Sol" = Sigma, "Fixpoint" = fixpoint, "Fuera" = fuera, "Next" = N))
}



composition.eq <- function(Sigma,k,l){
  A <- read.left(Sigma,k)
  B <- read.right(Sigma,k) 
  C <- read.left(Sigma,l)
  D <- read.right(Sigma,l) 
  if(equals.sets(A,C) & !is.empty(B) & !is.empty(D)){
    BD <- union.sets(B,D)
#    browser()
    Sigma <- substitute.imp(Sigma,k,A,BD)
    Sigma <- remove.imp(Sigma,l)
  }#end if
  return(Sigma)  
}#End composition.eq


reduction.eq <- function(Sigma,k){
  A <- read.left(Sigma,k)
  B <- read.right(Sigma,k) 
  BA <- difference.sets(B,A) 
  if (is.empty(BA)){ 
    Sigma <- remove.imp(Sigma,k)
    return(Sigma)
  }#end if
  if (!(equals.sets(BA,B))) 
        Sigma <- substitute.imp(Sigma,k,A,BA)
  return(Sigma)
}#End reduction.eq


sSimp <- function(Sigma,k,l){
  if(included.left(Sigma,k,l) | 
     intersection.sets(Sigma[k]$rhs[[1]],Sigma[l]$lhs[[1]]==FALSE))   {
    return()
  }#END IF  
  AB <- union.sets(read.left(Sigma,k),read.right(Sigma,k)) #AB
  D_AB <- difference.sets(read.right(Sigma,l),AB) #D-AB
  if (is.empty(D_AB)) return(Sigma)
  AC <- union.sets(read.left(Sigma,k),read.left(Sigma,l)) #AC
  AC_B <- difference.sets(AC,read.right(Sigma,k)) #AC_B
  Sigma <- new.imp(Sigma,AC_B,D_AB)
  return(Sigma)
}#End sSimp

### Reduction Method ###
apply.ssimp <- function(A,B,GammaP){
  newGammaP <- GammaP[0]
  for(l in seq(GammaP)){
    C <- read.left(GammaP, l)
    D <- read.right(GammaP, l)
    newA <- union.sets(A, difference.sets(C, B)) 
    newB <- difference.sets(D,union.sets(A, B)) 
    newGammaP <- add.imp(newGammaP, newA, newB)
  }
  newGammaP <- delete.set.of.IS(newGammaP)
  newGammaP <- apply.composition.eq(newGammaP)
  return(newGammaP)
}#End apply.ssimp


lSimp <- function(OmegaB, A, B){
  K_B <- difference.sets(OmegaB,B)
  newK <- union.sets(A,K_B)
  return(newK)
}#End lSimp

transG <- function(Sigma){
  fuera <- FALSE
  fuera2 <- FALSE
  fixpoint <- FALSE
  longitud <- length(Sigma)
  while(!fixpoint){
    fixpoint <- TRUE
    k <- 1
    while(k  <= longitud){ # V -> W
      if(fuera2){
        fuera2 <- FALSE
        next
      }
      if(k > length(Sigma)){
        break
      }
      V <- read.left(Sigma,k)
      W <- read.right(Sigma,k) 
      l <- 1
      while(l <= longitud){ # X -> Y
        if(fuera){
          fuera <- FALSE
          next
        }
        if(l > length(Sigma)){
          break
        }
        if (k==l){
          l <- l+1
          next
        }
        X <- read.left(Sigma,l)
        Y <- read.right(Sigma,l) 
        XY <- union.sets(X,Y)
        m <- 1
        while(m <= longitud){ # Z -> U
          if(m >length(Sigma)){
            break
          }
          if (k==m){
            m <- m + 1
            next
          }
          if (m==l){
            m <- m + 1
            next
          }
          Z <- read.left(Sigma,m) # Z
          if(is.included(Z,XY)){  # Z incluido en XY
            U <- read.right(Sigma,m) # U
            UV <- union.sets(U,V)
            if(is.included(X,V) & is.included(W,UV)){
              Sigma <- Sigma[-k]
              fixpoint <- FALSE
              fuera <- TRUE
              fuera2 <- TRUE
              next
            }
          }
          m <- m+1
        }
        l <- l+1
      }
      k <- k+1
    }
  }
  return(Sigma)
}#End transG
