context <- function(num.obj, num.attr, sparness=NULL) {
  if (num.obj < 1 || num.attr < 1){
    stop("The number of objects and the number of attributes must be greater than zero.")
  } 
  if (!is.null(sparness) && ((sparness < 0) || (sparness > 1))){
    stop("Sparness must be a number between 0 and 1.") 
  }
  if(is.null(sparness)){
    mi.df <- context.no.sparness(num.obj, num.attr)
  }else{
    totalN <- num.obj*num.attr
    ones <- totalN*sparness
    if(ones < totalN/2){
      mi.df <- putOnes(mi.df, totalN, ones, num.obj, num.attr)
    }else{
      mi.df <- putZeros(mi.df, totalN, (totalN-ones), num.obj, num.attr)
    }
  }
  colnames(mi.df) <- as.vector(sapply(1:num.attr,label.att))
  rownames(mi.df) <- as.vector(sapply(1:num.obj,label.obj))
  for(k in seq(dim(mi.df)[2])){
    mi.df[,k] <- as.logical(mi.df[,k])
  }
  return(mi.df)
}#End context
