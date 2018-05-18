lSimp <- function(OmegaB, A, B){
  K_B <- difference.sets(OmegaB,B)
  newK <- union.sets(A,K_B)
  return(newK)
}#End lSimp