new.imp <- function(Sigma,A,B){
  for (k in seq(Sigma)){
    C <- read.left(Sigma,k)
    D <- read.right(Sigma,k) 
    if (equals.sets(C,A)){
      if (equals.sets(D,B)){
        return(Sigma) 
      }else{
        BD <- union.sets(B,D)
        Sigma <- substitute.right(Sigma,k,A,BD)
        return(Sigma)  
  	  } 
   	} 
  }
  Sigma <- add.imp(Sigma,A,B)
  return(Sigma)  
}#End new.imp