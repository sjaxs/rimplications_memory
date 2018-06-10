all.fc <- function(context){
  newformal.cs <- c()
  attribute.names <- colnames(context)
  object.names <- rownames(context)
  list.extents <- c()
  for (k in attribute.names){
    newextent <- Mp(context, c(k))
    list.extents <- add.extent(newextent, list.extents)
  }
  fixpoint <- FALSE
  while(!fixpoint){
    fixpoint <- TRUE
    all.list.extents <- list()
    le <- length(list.extents)
    k <- 1
    while(k <= le){
      l <- k + 1
      while(l <= le){
        inter.extents <- intersect(list.extents[[k]], list.extents[[l]])
        if ( !is.member(inter.extents, list.extents)
            & !is.member(inter.extents, all.list.extents)){
          all.list.extents <- add.extent(inter.extents, all.list.extents)
          fixpoint <- FALSE
        }
        l <- l + 1
      }
      k <- k + 1
    }
    if(length(all.list.extents)>0){
      list.extents <- c(list.extents, all.list.extents)
    }
  }
  list.extents <- add.extent(object.names, list.extents)
  for (k in 1:length(list.extents)){
    newfc <- object.concept.fc(context, c(list.extents[[k]]))
    newformal.cs <- c(newformal.cs, newfc)
  }
  return(newformal.cs)
}#End all.fc