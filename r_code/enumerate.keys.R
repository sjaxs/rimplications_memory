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


