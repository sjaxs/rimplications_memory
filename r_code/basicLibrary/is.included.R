is.included <- function(X,Y){
  return(is.subset(X, Y, proper=FALSE, sparse=FALSE))
}#End is.included
