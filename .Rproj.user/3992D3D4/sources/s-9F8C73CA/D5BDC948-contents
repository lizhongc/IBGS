#' extended BIC function
#'
#' @param model the selected model
#' @param gamma the tuning parameter to control the penalty
#' @param p0    the number of total predictors
#'
#' @return extended BIC value
#' @export
#'
exBIC <- function(model, gamma, p0){
  p <- model$df.null - model$df.residual
  return(BIC(model)+ 2* gamma * p * log(p0))
}
