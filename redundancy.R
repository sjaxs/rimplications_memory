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
          # fuera <- TRUE # tengo que salir dos veces
          next #salgo
        } 
        # SIMPLIFICACION
        if (is.included(A,C) ){ #si A esta contenido en C
          CB <- difference.sets(C,B)
          DB <- difference.sets(D,B)
          if (is.empty(CB) | is.empty(DB)) { 
            # si la regla es igual en alguno de los lados borro l
            Sigma <- Sigma[-l] #regla vacia, borro
            fixpoint <- FALSE
            next #salgo
          }else{
            if (!equals.sets(CB,C) | !equals.sets(DB,D)){
              #si es distinta la sustituyo por la nueva 
              Sigma <- substitute.imp(Sigma,l,CB,DB)
              if(equals.sets(CB,A)){
                Sigma <- composition.eq(Sigma,k,l)
                Sigma <- Sigma[-l]
                fixpoint <- FALSE
                next #salgo
              }
              fixpoint <- FALSE
            }#END IF
          }#end else
        }#end if is.included
        # R-SIMPLIFICACION
        CD <- union.sets(C,D)
        if (is.included(A,CD) ){
          DB <- difference.sets(D,B)
          if (is.empty(DB)) {
            Sigma <- Sigma[-l] #regla vacia, borro
            fixpoint <- FALSE
            next #salgo
          }else{
            if (!equals.sets(DB,D)){
              Sigma <- substitute.imp(Sigma,l,C,DB)
              fixpoint <- FALSE
            }#END IF
          }#end else
        }#end if is.included
        l <- l+1
      }#end while l
      k <- k+1
    }#end while k
  }#end while
  # Aplicar transisitvidad generalizada al final
  Sigma <- transG(Sigma)
  return(Sigma)
}#End apply.remove.redundancy

 