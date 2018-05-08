reduction.method <- function(Omega, Gamma, flag = TRUE){
  aotg <- addMtoGamma(Omega,Gamma,TRUE)
  Omega <- aotg$M
  Gamma <- aotg$Gamma
  # browser()
  OmegaB <- body(Omega,Gamma)
  # browser()
  GammaP <- compact(OmegaB,Gamma)
  # browser()
  K <- enumerate.keys(OmegaB, GammaP, flag)
  # print(length(K))
  # browser()
  K <- minimals(K)
  # browser()
  OmegaC <- core(Omega,Gamma)
  # browser()
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



enumerate.keys <- function(OmegaB, GammaP, flag, visited = NULL){
  listKeys <- c()
  # browser()
  if(flag){
    if(is.null(visited)){
      visited <- GammaP[0]
    }
  }
 
  if(length(GammaP)>0){
    for(k in seq(GammaP)){
      
      if (flag){
        if(is.element(GammaP[k],visited)){
          next
        }else{
          visited <- c(visited, GammaP[k])
        }
      }
      
      A <- read.left(GammaP, k)
      B <- read.right(GammaP, k)
      newK <- lSimp(OmegaB, A, B) 
      newGammaP <- apply.ssimp(A,B,GammaP) )
      key <- enumerate.keys(newK, newGammaP, flag, visited))
      if(!is.null(key)){
        if(is.null(listKeys)){
          listKeys <- c(key)
        }else{
          listKeys <- c(listKeys, key)
        }
      }
    }
  }else{
    if(is.null(listKeys)){
      listKeys <- c(OmegaB)
    }else if (is.null(OmegaB)){
      listKeys <- c(listKeys)
    }else{
      listKeys <- c(listKeys, OmegaB)
    }
  }
  return(listKeys)
}#End enumerate.keys

apply.ssimp <- function(A,B,GammaP){
  newGammaP <- GammaP[0]
  for(l in seq(GammaP)){
    C <- read.left(GammaP, l)
    D <- read.right(GammaP, l)
    newA <- union.sets(A, difference.sets(C, B)) 
    newB <- difference.sets(D,union.sets(A, B)) 
    newGammaP <- add.imp(newGammaP, newA, newB)
  }
  newGammaP <- delete.set.of.IS(newGammaP)
  newGammaP <- apply.composition.eq(newGammaP)
  return(newGammaP)
}#End apply.ssimp

lSimp <- function(OmegaB, A, B){
  K_B <- difference.sets(OmegaB,B)
  newK <- union.sets(A,K_B)
  return(newK)
}#End lSimp