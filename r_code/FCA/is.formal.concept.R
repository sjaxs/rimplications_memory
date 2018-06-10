is.formal.concept <- function(context, c){
  Ap <- Gp(context, c$g)
  Bp <- Mp(context, c$m)
  return(identical(Ap, c$m) & identical(Bp, c$g))
}#End is.formal.concept
