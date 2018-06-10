all.attribute.concept.fc <- function(context){
  attribute.names <-colnames(context)
  newfcs <- c()
  for (k in attribute.names){
    newfc <- attribute.concept.fc(context, c(k))
    newfcs <- c(newfcs, newfc)
  }
  return(newfcs)
}#End all.attribute.concept.fc