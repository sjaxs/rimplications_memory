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