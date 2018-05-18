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