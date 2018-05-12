apply.reduction.eq <- function(Sigma){
  for (k in seq(Sigma))
    Sigma <- reduction.eq(Sigma,k)
  return(Sigma)
}#End apply.reduction.eq


apply.composition.eq <- function(Sigma){
  for (k in seq(Sigma)){
    if(k > length(Sigma)){
      break
    }
    A <- read.left(Sigma,k)
    B <- read.right(Sigma,k) 
    l <- k + 1
    while (l <= length(Sigma)){
      if(l > length(Sigma)){
        break
      }
      C <- read.left(Sigma,l)
      D <- read.right(Sigma,l) 
      if(equals.sets(A,C) & !is.empty(B) & !is.empty(D)){
        B <- union.sets(B,D)
        Sigma <- substitute.imp(Sigma,k,A,B)
        Sigma <- Sigma[-l]
        l <- l - 1
      }#end if
      l <- l + 1
    }
  }
  return(Sigma)
}#End apply.composition.eq


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

simplification.eq <-function(Sigma,k,l){
# browser()
  mi.left <- read.left(Sigma,l)
  mi.right <- read.right(Sigma,k)
  if(included.left(Sigma,k,l)) {
    newL <- difference.sets(mi.left, mi.right)
    if (is.empty(newL)) {
      Impl <- remove.imp(Sigma,l)
      return(Sigma)
    } 
    else{
    newR <- difference.sets(read.right(Sigma,l), read.right(Sigma,k))
    if (is.empty(newR)) {
      Sigma <- remove.imp(Sigma,l)
      return(Sigma)
    }}#else
 #   browser()
    if (!(equals.sets(mi.left,newL) | equals.sets(mi.right,newR)) )
        Sigma <- substitute.imp(Sigma,l,newL,newR)
  }#end if
  return(Sigma)
}#End simplification.eq


rsimplification.eq <- function(Sigma,k,l){
  mi.left <- read.left(Sigma,l)
  mi.right <- read.right(Sigma,k)
  if(included.left.right(Sigma,k,l)) {
    newR <- difference.sets(mi.right,mi.left)
    if (is.empty(newR)) {
      Sigma <- remove.imp(Sigma,l)
      return(Sigma)
    }
    #   browser()
    if (!(equals.sets(mi.right,newR)) )
      Sigma <- substitute.imp(Sigma,l,mi.left,newR)
  }#end if
  return(Sigma)
}#End rsimplification.eq


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
