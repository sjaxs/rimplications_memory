Mp2 <- function(context, my.attr){
  mp <- Mp(context, my.attr)
  if(!is.null(mp)){
    mp2 <- Gp(context, mp)
  }else{
    return(NULL)
  }
  return(mp2)
}#End Mp2