next.closure <- function(FC, attributes){
  rn <- row.names(FC)
  cn <- colnames(FC)
  for (m in sort(cn, decreasing = TRUE)){
    if(any(attributes == m)){
      if (length(attributes) > 1){
        attributes <- attributes[1:(length(attributes)-1)]
      }else{
        attributes <- NULL
      }
    }else{
      temp <- attributes
      temp[(length(attributes) + 1)] <- m
      ClosedB <- Mp2(FC, temp)
      temp <- ClosedB[ClosedB < m]
      end <- TRUE
      for (i in temp){
        if (!any(i == attributes)){
          end <- FALSE
          break
        }
      }
      if(end){
        return(ClosedB)
      }
    }
  }
  return(cn)
}#End next.closure
