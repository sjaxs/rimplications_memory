body <- function(Omega, Gamma){
  lpSet <- read.left(Gamma,1)
  for(k in seq(Gamma)){
    B <- read.left(Gamma,k)
    lpSet <- union.sets(lpSet,B)
  }
  OmegaC <- core(Omega,Gamma)
  OmegaClos <- apply.closure(Gamma,OmegaC)
  newSet <- difference.sets(lpSet, OmegaClos$closure)
  return(newSet)
}#End body
