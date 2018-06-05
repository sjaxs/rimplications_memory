putOnes <- function(mi.df, totalN, ones, num.obj, num.attr){
  bin <- rep(0, totalN)
  contI <- 1
  contF <- num.obj
  mi.df <- data.frame(bin[contI:contF])
  for (k in seq(num.attr-1)){  
    contI <- contI+num.obj
    contF <- contF+num.obj
    col <- bin[contI:contF]
    mi.df <- cbind(mi.df,col)
  }
  numRow <- 0
  numCol <- 0
  sum <- 0
  while(sum < ones){
    numRow <- sample(1:num.obj,1)
    numCol <- sample(1:num.attr,1)
    if(mi.df[numRow,numCol] == 0){
      mi.df[numRow,numCol] <- 1
      sum <- sum + 1
    }
  }
  return (mi.df)
}#End putOnes
