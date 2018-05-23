d.basis <- function(M,Sigma){
  mg <- minimal.generator0(M,Sigma)
  MG <- mg$minGen
  Sigma <- mg$Gamma
  mg.labels <- itemLabels(MG[[1]]$set)
  cont <- 1
  C <- list()
  for(k in seq(MG)){
    mgk <- MG[[k]]
    list.labels <- as(mgk$label, "list")
    for(l in seq(list.labels[[1]])){
      a <- encode(list.labels[[1]][l],mg.labels)
      a.mg <- list("label"=a, "set"=mgk$set)
      ad <-add.dbasis(a.mg,C)
      C[[cont]] <- c(ad)
      cont <- cont + 1
    }
  }
  cont <- length(C) + 1
  fixpoint <- FALSE
  while(!fixpoint){  
    fixpoint <- TRUE
    k <- 1
    while(k <= length(C)){
      if(k > length(C))break
      if(length(C[[k]]$set)==0){
        C <- C[-k]
        cont <- cont - 1
        next()
      }
      label <- C[[k]]$label
      l <- k+1
      while(l <= length(C)){
        if(l > length(C))break
        if(length(C[[l]]$set)==0){
          C <- C[-l]
          cont <- cont - 1
          next()
        }
        label2 <- C[[l]]$label
        if(is.included(label, label2)){
          fixpoint <- FALSE
          s <- c(C[[k]]$set,C[[l]]$set)
          lista <- list("label"=label, "set"=s)
          C[[cont]] <- c(lista)
          C <- C[-l]
          C <- C[-k]
          cont <- cont - 1
          break
        }
        l <- l+1
      }
      k<-k+1
    }
  } 
  Sigma.D <- Sigma[0]
  for(k in seq(C)){
    mga <- C[[k]]$set
    #Calculo de los minimal covers
    mga <- minimal.covers(mga,Sigma)
    for(l in seq(mga)){
      g <- mga[l]
      a <- C[[k]]$label
      Sigma.D <- add.imp(Sigma.D,g,a)
    }
  }
  Sigma.D <- apply.composition.eq(Sigma.D)
  return(Sigma.D)
}
add.dbasis <- function(a.mg,C){
  a <- a.mg$label
  mg <- a.mg$set
  g <- mg[0]
  for(k in seq(mg)){
    if(!is.included(a,mg[k])){
      g <- c(g,mg[k])
    }
  }
  return(list("label"=a,"set"=g))
}
