set.of.attributes <- function(ListAttributes){
  list <- c()
  for (k in seq(ListAttributes)){
    imp <- strsplit(str_trim(ListAttributes[k],side=c("both"))," ")
    list <- union(imp,list)
  }
  return(list)
}#End set.of.attributes
  