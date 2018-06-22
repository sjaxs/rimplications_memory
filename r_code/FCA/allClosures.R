all.closures <- function(FC){
  FC <- FC[order(colnames(FC))]
  cn <- colnames(FC)
  attributes <- NULL
  #Computing first closure
  attributes <- Mp2(FC, attributes)
  flag <- FALSE
  AllClosures <- c()
  repeat{
    concept <- create.concept(Mp(FC, attributes), attributes)
    AllClosures <- c(AllClosures, concept)
    if(flag){
      return(AllClosures)
    } 
    attributes <- next.closure(FC, attributes)
    if(all(attributes==cn)){
      concept <- create.concept(Mp(FC, attributes), attributes)
      AllClosures <- c(AllClosures, concept)
      flag <- TRUE
    }
  }
}#End all.closures