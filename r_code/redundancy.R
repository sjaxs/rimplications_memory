
apply.remove.redundancy <- function(Sigma){
  fixpoint <- FALSE
  fuera <- FALSE
  longitud <- length(Sigma)
  while (!fixpoint){
    fixpoint <- TRUE
    k <- 1
    # si comparamos A -> B con C -> D, no comparamos C -> D con A -> B
    while(k  <= longitud){    # A -> B
      if(fuera){
        fuera <- FALSE
        next
      }
      if(k > length(Sigma)){
        break
      }
      l <- k+1 # no repetimos comparaciones 
      while(l <= longitud){  # C -> D
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
          next #salgo
        }
        if (is.empty(B)){
          Sigma <- Sigma[-k] #regla vacia, borro
          fuera <- TRUE # tengo que salir dos veces
          next #salgo
        }else if (is.empty(D)){
          Sigma <- Sigma[-l] #regla vacia, borro
          next #salgo
        } 
        # SIMPLIFICACION
        S <- simplification(Sigma, A,B,C,D,k,l)
        Sigma <- S$Sol
        fixpoint <- S$Fixpoint
        fuera <- S$Fuera
        N <- S$N
        if(N)next
        # R-SIMPLIFICACION
        RS <- rsimplification(Sigma, A,B,C,D,k,l)
        Sigma <- RS$Sol
        fixpoint <- RS$Fixpoint
        fuera <- RS$Fuera
        N <- RS$N
        if(N)next
        l <- l+1
      }#end while l
      k <- k+1
    }#end while k
  }#end while
  # Aplicar transisitvidad generalizada al final
  Sigma <- transG(Sigma)
  return(Sigma)
}#End apply.remove.redundancy