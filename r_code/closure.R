apply.closure <- function(Sigma,Xmas){
  fixpoint <- FALSE
  cont <- 0
  Xmas2 <- Xmas
  while (!fixpoint){ 
    fixpoint <- TRUE
    cont <- cont + 1
    if(cont == 2) Xmas <- difference.sets(Xmas, Xmas2)
    k <- 1
    l <- length(Sigma)
    while (k <= l){  # top -> Xmas,  A -> B 
      A <- read.left(Sigma,k)
      B <- read.right(Sigma,k) 
      if(is.empty(Xmas)) break
      newL <- difference.sets(A,Xmas) 
      if (is.empty(newL)) {
        Xmas  <- union.sets(B,Xmas)
        Sigma <- Sigma[-k]
        k <- k-1
        l <- l-1
        fixpoint <- FALSE
      }else{
        newR <- difference.sets(B,Xmas) 
        if (is.empty(newR)) {
          Sigma <- Sigma[-k]
          k <- k-1
          l <- l-1
          fixpoint <- FALSE
        }else{
          if (!equals.sets(newR,B) | !equals.sets(newL,A)){
            Sigma <- substitute.imp(Sigma,k,newL,newR)
            fixpoint <- FALSE
          }#END IF
        }#end else
      }#end else
      k <- k+1
    }#end while
  }#end while
  return(list("closure"=union.sets(Xmas,Xmas2), "implications"=Sigma))
}#End apply.closure