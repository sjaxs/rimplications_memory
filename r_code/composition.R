apply.union.eq <- function(Sigma){
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
}#End apply.union.eq
