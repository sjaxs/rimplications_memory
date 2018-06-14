trv <- function(M, Gamma, all.subs){
  cont <- 1 
  subs2 <- list(list("label" = all.subs[[1]], "set"= all.subs[[1]]))
  k <- 1
  if(length(Gamma)!=0){
    while(k <= length(all.subs)){
      if(k > length(all.subs)){
        break
      }
      included <- FALSE
      for(l in seq(Gamma)){
        A <- read.left(Gamma, l)
        if(is.included(A, all.subs[[k]])){
          included <- TRUE
          k <- k + calculate.new.k(M, A)
          break
        }
      }
      if(!included && (is.included(all.subs[[k]], M) || equals.sets(all.subs[[k]], last(all.subs)))){
        subs2[[cont]] <- list("label" = all.subs[[k]], "set"= all.subs[[k]])
        cont <- cont + 1
      }
      if(!included){
        k <- k+1
      }
    }
  }else{ 
    e <- encode("empty", M@itemInfo$labels)
    if(is.empty(M)){
      M <- list(e) 
    }else{
      M <- list(M,e)
    }
    for(k in 1:length(M)){
      subs2[[cont]] <- list("label" = M[[k]],"set"= M[[k]])
      cont <- cont + 1
    }
  }
  return (subs2)
}#End trv