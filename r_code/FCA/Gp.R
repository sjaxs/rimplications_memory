Gp <- function(context, my.objects){
  if (is.null(my.objects)){
    return(colnames(context))
  }
  rows <- context[my.objects,]
  rows <- as.data.frame(rows)
  if (dim(rows)[1]==0){ 
    return(NULL)
  }
  Inters <- as.logical(rows[1,])
  for (k in seq_along(vector(mode="numeric", length=dim(rows)[1]-1))+1){
    Inters <- Inters & as.logical(rows[k,])
  }
  if (sum(Inters)==0){
    return(NULL)
  }else{ 
    return(colnames(context)[Inters])
  }
}#End Gp
