all.object.concept.fc <- function(context){
  object.names <- row.names(context)
  newfcs <- c()
  for (k in object.names){
    newfc <- object.concept.fc(context, c(k))
    newfcs <- c(newfcs, newfc)
  }
  return(newfcs)
}#End all.object.concept.fc