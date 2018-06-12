object.concept.fc <- function(context, g){
  B <- Gp(context, c(g))
  A <- Mp(context, B)
  newfc <- create.concept(A, B)
  return(newfc)
}#End object.concept.fc