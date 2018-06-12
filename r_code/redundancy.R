
apply.remove.redundancy <- function(Sigma){
  fixpoint <- FALSE
  fuera <- FALSE
  longitud <- length(Sigma)
  while (!fixpoint){
    fixpoint <- TRUE
    k <- 1
    while(k <= longitud){    
      if(fuera){
        fuera <- FALSE
        next
      }
      if(k > length(Sigma)){
        break
      }
      l <- k+1 
      while(l <= longitud){  
        if(l > length(Sigma)){
          break
        }
        A <- read.left(Sigma,k)
        B <- read.right(Sigma,k) 
        C <- read.left(Sigma,l)
        D <- read.right(Sigma,l)
        if(equals.sets(C,A)){
          Sigma <- composition.eq(Sigma,k,l)
          Sigma <- Sigma[-l]
          fixpoint <- FALSE
          next 
        }
        if (is.empty(B)){
          Sigma <- Sigma[-k] 
          fuera <- TRUE 
          next 
        }else if (is.empty(D)){
          Sigma <- Sigma[-l] 
          next 
        } 
        S <- simplification(Sigma, A,B,C,D,k,l)
        Sigma <- S$Sol
        fixpoint <- S$Fixpoint
        fuera <- S$Fuera
        N <- S$N
        if(N)next
        RS <- rsimplification(Sigma, A,B,C,D,k,l)
        Sigma <- RS$Sol
        fixpoint <- RS$Fixpoint
        fuera <- RS$Fuera
        N <- RS$N
        if(N)next
        l <- l+1
      }
      k <- k+1
    }
  }
  Sigma <- transG(Sigma)
  return(Sigma)
}#End apply.remove.redundancy