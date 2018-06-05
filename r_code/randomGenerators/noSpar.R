context.no.sparness <- function(num.obj,num.attr) {
  mi.df <- data.frame(rbv(num.obj))
  for (k in 1:(num.attr-1)){
      col <- rbv(num.obj)
      mi.df <- cbind(mi.df,col)
  }
  return(mi.df)
}#End context.no.sparness
