Gp2 <- function(context, my.obj){
  gp <- Gp(context, my.obj)
  if(!is.null(gp)){
    gp2 <- Mp(context, gp)
  }else{
    return(NULL)
  }
  return(gp2)
}#End Gp2
