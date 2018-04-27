
#install.packages("data.table")  #SI NO LO TIENES
 # library(stringr)
 # library(data.table)

# read a set of implications y lo pasa a data.table
# w <- union(unlist(I[1,lhs]),unlist(I[3,lhs]))
# I[3,lhs:=list(list(c('5','8')))]
#I[3,lhs:=list(list(w))]
# Delete row I <- I[-2,]



##################################
#  ENTRADA - SALIDA  IMPLICATIONS
##################################

read.implications <- function(fileName){ 
  myString <- paste(readLines(fileName), collapse="\n") 
  list_imp <- str_split(myString, "\n") 
  impl <- sapply(list_imp, function(x) as.vector(str_split(x, "->")))
  impl2 <- lapply(impl, function(x) as.vector(x))
  impl3 <- t(as.data.table(impl2))
  lhs_impl3 <- impl3[,1]
  rhs_impl3 <- impl3[,2]
  listAttributes <- c()
  for (k in seq(lhs_impl3)){
     lhs_impl3[k] <- strsplit(str_trim(lhs_impl3[k],side=c("both"))," ")
     rhs_impl3[k] <- strsplit(str_trim(rhs_impl3[k],side=c("both"))," ")
     listAttributes <- union(lhs_impl3[[k]],listAttributes)
     listAttributes <- union(rhs_impl3[[k]],listAttributes)
  }
  impl4 <- data.table(lhs=lhs_impl3,rhs=rhs_impl3)
  listAttributes <- sort(listAttributes)
  return(list("implications"=impl4, "attributes"=listAttributes))
}#End read.implications



to.rules <- function(datatable.rules){
  flag <- FALSE
  if(datatable.rules$attributes[[1]]=="empty"){
    atr <- datatable.rules$attributes[-1]
    flag <- TRUE
  }else{
    atr <- datatable.rules$attributes
  }
  iLabels <- sort(atr)
  if(flag){
    iLabels <- c(iLabels, "empty")
  }
  Sigma <- c()
  for (k in seq(dim(datatable.rules$implications)[1])){
    impl1 <- list( datatable.rules$implications$lhs[k][[1]],
                   datatable.rules$implications$rhs[k][[1]])
    iM <- encode(impl1, iLabels)
    rNJ <- new("rules", lhs=iM[1], rhs=itemSetdiff(iM[2],iM[1]), quality = data.frame(confidence = 1))
    if (length(Sigma)==0){
      Sigma <- c(rNJ)
    }else{
      Sigma <- c(Sigma,rNJ)
    }  
  }#end for
  return(Sigma)
}#End to.rules
 


##################################
#  ENTRADA - SALIDA
##################################
###por aqui
# Para imprimir el resultado

write.set <- function(X){
  itemSep = ","
  setStart = "{"
  setEnd = "}"
  To = " --> "
  for (k in seq(X[[1]])){
    cat(setStart,X[k]$lhs[[1]],setEnd,To,setStart,X[k]$rhs[[1]],setEnd,'\n')
  }#end for
}#End write.set
