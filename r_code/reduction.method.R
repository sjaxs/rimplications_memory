reduction.method <- function(Omega, Gamma, flag = TRUE){
  aotg <- addMtoGamma(Omega,Gamma,TRUE)
  Omega <- aotg$M
  Gamma <- aotg$Gamma
  OmegaB <- body(Omega,Gamma)
  GammaP <- compact(OmegaB,Gamma)
  K <- enumerate.keys(OmegaB, GammaP, flag)
  K <- minimals(K)
  OmegaC <- core(Omega,Gamma)
  newK <- union.keys(OmegaC,K)
  return(newK)
}#End reduction.method

minimals <- function(keys){
  keys <- unique(keys)
  fixpoint <- FALSE
  while(!fixpoint){
    fixpoint <- TRUE
    k <- 1)
    while(k <= length(keys)){
      a <- keys[k]
      l <- 1 + k
      while (l <= length(keys)) {
        b <- keys[l]
        if(is.included(b,a)){
          keys <- keys[-k]
          fixpoint <- FALSE
          break
        }else if(is.included(a,b)){
          keys <- keys[-l]
          fixpoint <- FALSE
          break
        }
        l <- l + 1
      }
      k <- k+1 
    }
  }
  return(keys)
}#End minimals


