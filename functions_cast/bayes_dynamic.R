#' Title
#'
#' @param pa Priori probability of A
#' @param pb_a Probability of B given A (sensitiviy in the case of a classifier)
#' @param pb_na Probability of B given not A (false positive rate = fp/sum(true_negatives))
#' @param b Result of the test (B = TRUE or B = FALSE). If it is a vector, dynamic bayes updating is applied.
#'
#' @return posteriory probability given B. If b is a vector, the priori probability is updated with the posteriory result for each element in b.
#' @export
#'
#' @examples
#' bayes_dynamic()
#' [1] 0.32312925 0.02354282
bayes_dynamic <- function( pa = 0.005, pb_a = 0.95, pb_na = 0.01, b = c(TRUE, FALSE) ) {
    source('functions_cast/bayes_reasoning.R')
    ph <- rep(pa, (length(b)+1))
    for(i in 1:(length(ph)-1)) {
        ph[i+1] <- bayes_reasoning(ph[i], pb_a, pb_na, b[i])
    }
    ph <- ph[2:length(ph)]
    
    return(ph)
}