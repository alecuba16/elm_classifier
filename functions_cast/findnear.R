#' Find dates near to a reference date
#'
#' @param vectordate1: vector of reference dates 
#' @param vectordate2: vector of dates to search near to referecen dates vector
#' @param interval_up: margin inferior respect to reference date (in secods)
#' @param interval_low: margin superior respect to reference date (in secods) 
#'
#' @return a list of the indexes of vectordate2 that are near to vectordate1. 
#' Each list element belows to each date reference in vectordate1
#' @export
#'
#' @examples
findnear <- function(vectordate1, vectordate2, interval_up = 60*10, interval_low = 60*10) {
    
    f <- function(x, y, in_up, in_lo) {which( y < (x + in_up) & y > (x - in_lo) )}
    
    id <- lapply(X = as.list(vectordate1), FUN = f, vectordate2, interval_up, interval_low)
    
    return(id)
}