infimum.fc <- function(context, fc1, fc2){
  A12 <-intersect(fc1$g, fc2$g)
  if (length(A12)==0) A12 <- NULL
  B12 <- Mp2(context, union(fc1$m, fc2$m))
  newfc <- create.concept(A12, B12)
  return(newfc)
}#End infimum.fc