random.implications.generator <- function(numAttributes, numDependencies, 
                             difference = NULL, 
                             percentage = NULL, 
                             maxLeftSize = NULL, maxRightSize = NULL,
                             nameExitFile, numFiles=1){
  
  if(is.correct.input(numAttributes, numDependencies, difference, percentage,
                       maxLeftSize, maxRightSize, nameExitFile, numFiles=1)){
  } # If something is incorrect, it will stop 

  numDep <- numDependencies
  for(numFile in 1:numFiles){
    correct.outPut <- FALSE
    salida <- c()
    cont <- 0
    N <- numAttributes
    if(numFiles==1){
      numFile <- NULL
    }else{
      numFile <- paste("_", numFile, sep="")
    }
    nameExit <- paste(nameExitFile,numFile, ".R", sep="")
    while(!correct.outPut){
      for(k in 1:numDep){
        if(!is.null(difference)){
          if(difference < 0){ # Negative ->  More attributes in right side
            num <- sample(1:(N+difference), 1)
            newDiff <- sample(0:difference, 1)
            numLeft <- num
            numRight <- numLeft-newDiff
          }else{ # Positive -> More attributes in left side
            num <- sample(1:(N-difference), 1)
            newDiff <- sample(0:difference, 1)
            numRight <- num
            numLeft <- numRight+newDiff
          }
        }else if(!is.null(percentage)){
          if(percentage < 0){ # Negative ->  More attributes in right side
            p <- (N * percentage) %/% 100
            newP <- sample(0:p,1)
            num <- sample(1:(N+p),1)
            numLeft <- num
            numRight <- numLeft - newP
          }else{ # Positive -> More attributes in left side
            p <- (N * percentage) %/% 100
            newP <- sample(0:p,1)
            num <- sample(1:(N-p),1)
            numRight <- num
            numLeft <- numRight + newP
          }
        }else if(!is.null(maxLeftSize) && !is.null(maxRightSize)){
          numLeft <- sample(1:maxLeftSize,1)
          numRight <- sample(1:maxRightSize,1)
        }
        A <- generate.string(N, numLeft)
        B <- generate.string(N, numRight)
        dep <- paste(A," -> ",B, sep ="")
        salida <- c(salida, dep)
      }
      write(salida, nameExit)
      I <- read.implications(nameExit)
      R1 <- I$rules
      R2 <- apply.remove.redundancy(R1)
      R3 <- apply.composition.eq(R2)
      if(length(R3) != numDependencies){
        numDep <- numDependencies - length(R3)
      }else{
        correct.outPut <- TRUE
        write(print.implications(R3), nameExit)
      }
    }#End while correct
  }#End for files
}#End random.implications.generator

