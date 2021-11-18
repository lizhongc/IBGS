#' sigmoid function to calculate the acceptance probability
#'
#' @param dx the difference of model selection criteria
#' @param k  the tuning parameter
#'
#' @return sigmoid value
#' @export
#'
sigmoid <- function(dx,k){
  p <- exp(k * dx)
  return(min(c(1,p)))
}
