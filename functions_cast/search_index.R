search_index <- function(pattern, dataframe) {
  index <- grep(paste(".",pattern,"|^",pattern,sep = ""), colnames(dataframe))
  return(index)
}