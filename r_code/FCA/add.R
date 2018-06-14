
add <- function(Alpha, Beta){
  if(length(Alpha) == 0){
    Omega <- Beta
  }else if(length(Beta) == 0){
    Omega <- Alpha
  }else{
    Omega <- Beta[0]
    contOmega <- 1
    C <- Alpha[[1]]$label
    D <- Alpha[[1]]$set
    for(k in seq(Beta)){
      A <- Beta[[k]]$label
      B <- Beta[[k]]$set
      if(is.empty(A) || ((as(A, "list"))[[1]])=="empty"){
        newA <- C
      }else if(is.empty(C) || (as(C, "list"))[[1]]=="empty"){
        newA <- A
      }else{
        newA <- union.sets(A, C)
      }
      newB <- c()
      contB <- 1
      for(l in seq(B)){
        if(is.empty(B) || ((as(B, "list"))[[1]])=="empty"){
          newB <- D
        }else if(is.empty(D) || (as(D, "list"))[[1]]=="empty"){
          newB <- B
        }else{
          if(is.null(newB)){
            newB <- union.sets(B[l], D)
          }else{
            aux <- union.sets(B[l], D)
            newB <- c(newB, aux)
          }
        }
        contB <- contB+1
      }#End for l
      new <- list("label"=newA, "set"=newB)
      Omega[[contOmega]] <- new
      contOmega <- contOmega +1
    }#End for k
  }
  return(Omega)
}#End add