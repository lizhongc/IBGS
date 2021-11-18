#' The corrected AIC function
#'
#' @param model the selected model
#'
#' @return AICc value for a model
#' @export
#'
AICc <- function(model){
  k <- switch(model$family$family,
         gaussian = model$df.null - model$df.residual + 2,
         binomial = model$df.null - model$df.residual + 1,
         poisson  = model$df.null - model$df.residual + 1)
  n <- dim(model$model)[1]
  return(model$aic + 2*k*(k+1)/(n-k-1))
}
