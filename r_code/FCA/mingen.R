minimal.generator <- function(M, Gamma, subsets = NULL){
  if(is.null(subsets)){
    all.subsets <- list()
  }else{
    all.subsets <- subsets
  }
  if(is.character(M[1])){
    new <- addMtoGamma(M,Gamma)
    M <- new$M
    Gamma <- new$Gamma
    all.subsets <- new$all.sub
  }
  if(length(Gamma)==0 ){
    Phi <- trv(M, Gamma, all.subsets)
  }else{
    Phi <- trv(M, Gamma, all.subsets)
    for(k in seq(Gamma)){
      A <- read.left(Gamma,k)
      B <- read.right(Gamma,k)
      cls <- apply.closure(Gamma,A)
      A.Plus <- cls$closure
      Gamma2 <- cls$implications
      M2 <- difference.sets(M, A.Plus)
      ASet <- list(list("label"=A.Plus, "set"=A)) 
      aux0 <- minimal.generator(M2, Gamma2, all.subsets)
      aux1 <- add(ASet, aux0)
      Phi <- suppressWarnings(union.minimal.sets(Phi, aux1))
    }
  } 
  return (Phi)
}#End minimal.generator