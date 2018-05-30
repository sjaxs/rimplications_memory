labeled.closed.set <- function(M, Gamma, Label = NULL, Cicerone = NULL, Flag = TRUE){
  if(is.null(Label)){
    Label <- Gamma[1]@lhs[0]
  }
  if(is.null(Cicerone)){
    Cicerone <- Gamma[1]@lhs[0]
  }
  fixpoint <- FALSE
  while(!fixpoint){
    fixpoint <- TRUE
    Sigma <- Gamma
    Gamma <- Gamma[0]
    for(k in seq(Sigma)){
      A <- read.left(Sigma,k)
      B <- read.right(Sigma,k)
      if(length(Cicerone) != 0 && is.included(A,Cicerone)){
        Cicerone <- union.sets(Cicerone,B)
      }else if(length(Cicerone) != 0 && !is.included(B,Cicerone)){
        newA <- difference.sets(A,Cicerone)
        newB <- difference.sets(B,Cicerone)
        Gamma <- add.imp(Gamma, newA, newB)
      }else if(length(Cicerone) == 0){
        Gamma <- add.imp(Gamma, A, B)
      }
    }
    if(!equals.sets(Gamma,Sigma)){
      fixpoint <- FALSE
    }
  }
  if(Flag){
    amtg <- addMtoGamma(M,Gamma,FALSE)
    M <- amtg$M
    Gamma <- amtg$Gamma
    subs <- amtg$all.sub
    newC <- Cicerone[0]
    labels <- M@itemInfo$labels
    list <- as(Cicerone,"list")
    if(length(Cicerone)!=0){
      Cicerone <- encode(list[[1]],labels) 
    }else{
      Cicerone <- encode("empty",labels) 
      Cicerone <- Cicerone[0]
    }
  }else{
    M <- difference.sets(M,Cicerone)
    if(!is.empty(M)){
      subs <- gen.all.subsets(M)$sub
    }else{ subs <- list() }
  }
  #A subconjunto de M que cumple que A -> B pertenece a Gamma
  Mnl <- mnl(Gamma)
  #Subconjuntos de M que no contienen a ningun A de Mnl
  NC <- nc(subs,Mnl)
  LCS <- list() 
  if(length(NC) != 0){
    for(k in seq(NC)){
      X <- NC[[k]]
      newLabeledX <- X[0]
      labels <- Cicerone@itemInfo$labels
      list <- as(X,"list")
      newLabeledX <- encode(list[[1]],labels) 
      X <- newLabeledX
      if(length(Cicerone)!=0){
        if(((as(X, "list"))[[1]])!="empty"){
          newX <- union.sets(Cicerone,X)
        }else{
          newX <- Cicerone
        }
      }else{
        newX <- X
      }
      if(length(Label)!=0){
        if(((as(X, "list"))[[1]])!="empty"){
          set <- union.sets(Label,X)
        }else{ set <- Label }
      }else{ set <- X }
      List <- list(list("label"=newX, "set"=set)) 
      LCS <- union.minimal.sets(LCS,List)
    }
  }else{
    List <- list(list("label"=Cicerone, "set"=Label)) 
    LCS <- union.minimal.sets(LCS,List)
  }
  for(k in seq(Mnl)){
    A <- Mnl[[k]]
    if(length(Label)!=0){
      newLabel <- union.sets(Label, A)
    }else{
      newLabel <- A
    }
    if(length(Cicerone)!=0){
      newCicerone <- union.sets(Cicerone, A)
    }else{
      newCicerone <- A
    }
    LCS2 <- labeled.closed.set(M,Gamma,newLabel,newCicerone,FALSE)
    LCS <- union.minimal.sets(LCS,LCS2)
  } 
  return(LCS) 
}#End labeled.closed.set