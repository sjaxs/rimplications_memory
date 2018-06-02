add.imp.k <- function(Sigma,A,B,k){
  rNJ <- new("rules", lhs=A, rhs=B, quality = data.frame(confidence = 1))
  SigmaNew <- c(Sigma[seq_len(k-1)], rNJ, Sigma[k+seq_len(length(Sigma)-k)])
  return(SigmaNew)
}#End add.imp.k
