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
      # Aplicamos a lsimplificacion
      newK <- lSimp(OmegaB, A, B) 
      #Aplicamos la simplificacion fuerte
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


