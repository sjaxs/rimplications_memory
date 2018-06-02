core <- function(Omega, Gamma){
  rpSet <- read.right(Gamma,1)
  for(k in seq(Gamma)){
    B <- read.right(Gamma,k)
    rpSet <- union.sets(rpSet,B)
  }
  newOmega <- difference.sets(Omega, rpSet)
  return(newOmega)
}#End core
