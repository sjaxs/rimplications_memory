read.fc <- function(filename, file, first.line.atrr, first.col.obj){
  if(file == "xls"){
    if(first.line.atrr & first.col.obj){
      df <- as.data.frame(read_excel(filename))
      row.names(df) <- df[,1]
      df[,1] <- NULL
    }else if (first.line.atrr){
      df <- as.data.frame(read_excel(filename))
      objs <- c()
      for(k in seq(dim(df)[1])){
        ob <- label.obj(k)
        objs <- c(objs, ob)
      }
      row.names(df) <- objs
    }else if (first.col.obj){
      df <- as.data.frame(read_excel(filename, col_names = FALSE))
      row.names(df) <- df[,1]
      df[,1] <- NULL
      attrs <- c()
      for(k in seq(dim(df)[2])){
        at <- label.att(k)
        attrs <- c(attrs, at)
      }
      names(df) <- attrs
    }else{
      df <- as.data.frame(read_excel(filename, col_names = FALSE))
      attrs <- c()
      for(k in seq(dim(df)[2])){
        at <- label.att(k)
        attrs <- c(attrs, at)
      }
      objs <- c()
      for(k in seq(dim(df)[1])){
        ob <- label.obj(k)
        objs <- c(objs, ob)
      }
      names(df) <- attrs
      row.names(df) <- objs
    }
  }else if((file == "txt") | (file == "r")){
    myString <- paste(readLines(filename), collapse="\n") 
    list_r <- str_split(myString, "\n")[[1]]
    if(first.line.atrr & first.col.obj){
      list_obj <- str_split(list_r, " ")[1][[1]]
      list_atrr <- str_split(list_r, " ")[2][[1]]
      df <- data.frame()
      for(k in seq(length(list_obj))){
        x <- str_split(list_r[k+2], " ")[[1]]
        x <- as.numeric(x)
        df <- rbind(df,as.logical(x))
      }
    }else if(first.line.atrr){
      list_atrr <- str_split(list_r, " ")[1][[1]]
      df <- data.frame()
      for(k in seq(length(list_r)-1)){
        x <- str_split(list_r[k+1], " ")[[1]]
        x <- as.numeric(x)
        df <- rbind(df,as.logical(x))
      }
      list_obj <- c()
      for(k in seq(dim(df)[1])){
        ob <- label.obj(k)
        list_obj <- c(list_obj, ob)
      }
    }else if(first.col.obj){
      list_obj <- str_split(list_r, " ")[1][[1]]
      df <- data.frame()
      for(k in seq(length(list_obj))){
        x <- str_split(list_r[k+1], " ")[[1]]
        x <- as.numeric(x)
        df <- rbind(df,as.logical(x))
      }
      list_atrr <- c()
      for(k in seq(dim(df)[2])){
        at <- label.att(k)
        list_atrr <- c(list_atrr, at)
      }
    }else{
      df <- data.frame()
      for(k in seq(length(list_r))){
        x <- str_split(list_r[k], " ")[[1]]
        x <- as.numeric(x)
        df <- rbind(df, as.logical(x))
      }
      list_atrr <- c()
      for(k in seq(dim(df)[2])){
        at <- label.att(k)
        list_atrr <- c(list_atrr, at)
      }
      list_obj <- c()
      for(k in seq(dim(df)[1])){
        ob <- label.obj(k)
        list_obj <- c(list_obj, ob)
      }
    }
    row.names(df) <- list_obj
    names(df) <- list_atrr
  }
  for(k in seq(dim(df)[2])){
    df[,k] <- as.logical(df[,k])
  }
  return(df)
}#End read.fc