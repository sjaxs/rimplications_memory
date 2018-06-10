Mp <- function(context, my.attr){
  if (is.null(my.attr)){
    return(rownames(context))
  }
  cols <- context[,my.attr]
  cols <- as.data.frame(cols)
  if (dim(cols)[1]==0){
    return(NULL)
  }else{
    Inters <- as.logical(cols[,1])
    for (k in seq_along(vector(mode="numeric", length=dim(cols)[2]-1))+1){
      Inters <- Inters & as.logical(cols[,k])
    }
  }
  if (sum(Inters)==0){
    return(NULL)
  }else{
    return(rownames(context)[Inters])
  }
}#End Mp
