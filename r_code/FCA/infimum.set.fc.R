infimum.set.fc <- function(context, set.fc){
  A <- set.fc[1]$g
  B <- set.fc[1]$m
  for(k in seq(set.fc)){
    A <- intersect(A, set.fc[k]$g)
    B <- union(B, set.fc[k]$m)
  }
  B <- Mp2(context, B)
  infimum <- create.concept(A, B)
  return(infimum)
}#End infimum.set.fc