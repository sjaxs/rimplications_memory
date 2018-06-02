initialize.setOfAttributes <- function(X, ListAttributes){
  X <- unique(X)
  if(last(ListAttributes)=="empty"){
    l <- length(ListAttributes)
    iLabels <- sort(ListAttributes[-l])
    iLabels <- c(iLabels, "empty")
  }else{
    iLabels <- sort(ListAttributes)
  }
  X2 <- encode(list(X), iLabels)
  return(X2)
}#End initialize.setOfAttributes
