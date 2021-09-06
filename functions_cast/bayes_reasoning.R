#' bayes_reasoning estimate the probability of A given B using Bayes theorem.
#'
#' @param pa Priori probability of A
#' @param pb_a Probability of B given A (sensitiviy in the case of a classifier)
#' @param pb_na Probability of B given not A (false positive rate = fp/sum(true_negatives))
#' @param b Result of the test (B = TRUE or B = FALSE)
#'
#' @return Probability of A given B
#' @export
#'
#' @examples
#' bayes_reasoning(0.005, 0.95, 0.01)
#' [1] 0.3231293
bayes_reasoning <- function( pa = 0.01, pb_a, pb_na, b = TRUE ) {
    if(b)
        pa_b <- pa*pb_a / (pa*pb_a + (1 - pa)*pb_na)
    else
        pa_b <- pa*(1-pb_a) / (pa*(1-pb_a) + (1 - pa)*(1-pb_na))
    
    pa_b <- ifelse(pa_b < 0.001, 0.001, pa_b)
    pa_b <- ifelse(pa_b > 0.99, 0.99, pa_b)
    return(pa_b)
}