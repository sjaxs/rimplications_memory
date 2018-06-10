attribute.concept.fc <- function(context, m){
  A <- Mp(context, c(m))
  B <- Gp(context, A)
  newfc <- create.context(A, B)
  return(newfc)
}#End attribute.concept.fc