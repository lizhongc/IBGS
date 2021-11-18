#' Calculate the weights based on the model selection criterion values
#'
#' @param sic a vector of model selection criterion values
#' @param k   the tuning parameter
#'
#' @return a sequence of model weights
#' @export
#'
#'
weight <- function(sic, k){
  return(exp(-k*(sic - min(sic)))/sum(exp(-k*(sic - min(sic)))))
}
