calcule_cost <- function(cost_matrix, cm, type = "smart") {
  x <- as.matrix(cost_matrix)
  if(type == "smart") {
    y <- matrix(cm$table)
  } else {
    positives <- sum(cm$table[,2])
    negatives <- sum(cm$table[,1])
    if(type == "random")
      y <- matrix( c(negatives/2, negatives/2, positives/2, positives/2) )
    if(type == "reactive")
      y <- matrix( c(negatives, 0, positives, 0) )
  }
  return( as.numeric(x %*% y) )
}