supremum.fc <- function(context, fc1, fc2){
  A12 <- Gp2(context,union(fc1$g, fc2$g))
  B12 <- intersect(fc1$m, fc2$m)
  if (length(B12)==0) B12 <- NULL
  newfc <- create.fc(A12, B12)
  return(newfc)
}#End supremum.fc
