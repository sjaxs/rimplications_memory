add.imp <- function(Sigma,A,B){
  rNJ <- new("rules", lhs=A, rhs=B,quality = data.frame(confidence = 1))
  SigmaNew <- c(Sigma,rNJ)
  return(SigmaNew)
}#End add.imp
