remove.imp <- function(Sigma, k){
  rNJ <- new("rules", lhs=lhs(Sigma[k]), rhs=itemSetdiff(lhs(Sigma[k]), lhs(Sigma[k])), quality = data.frame(confidence = 1))
  return(c(Sigma[seq_len(k-1)], rNJ, Sigma[k+seq_len(length(Sigma)-k)]))
}#End remove.imp
