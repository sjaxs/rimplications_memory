supremum.set.fc <- function(context, set.fc){
  A <- set.fc[1]$g
  B <- set.fc[1]$m
  for(k in seq(set.fc)){
    A <- union(A, set.fc[k]$g)
    B <- intersect(B, set.fc[k]$m)
  }
  A <- Gp2(context, A)
  supremum <- create.concept(A, B)
  return(supremum)
}#End supremum.set.fc