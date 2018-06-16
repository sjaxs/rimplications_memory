compact <- function(Omega,Gamma){
  newGamma <- Gamma[0]
  for(k in seq(Gamma)){
    A <- read.left(Gamma,k)
    B <- read.right(Gamma,k) 
    newA <- intersection.sets(A,Omega)
    newB <- intersection.sets(B,Omega)
    newGamma <- add.imp(newGamma, newA, newB)
  }
  newGamma <- delete.set.of.IS(newGamma)
  newGamma <- apply.composition.eq(newGamma)
  return(newGamma)
}#End compact

union.keys <- function(OmegaC,K){
  newK <- K[0]
  for(k in seq(K)){
    list <- as(K[k],"list")
    newKey <- encode(list[[1]],K@itemInfo$labels)
    newK <- c(newK, union.sets(newKey, OmegaC))
  }
  return(newK)
}#End union.keys
