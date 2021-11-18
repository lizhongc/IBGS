#' The prediction of the best model and model averaging over top models
#'
#' @param xnew      the new data
#' @param result    the result from IBGS
#' @param n.models  the number of the top models
#'
#' @return          the prediction values
#' @export

predicts.Gibbs <- function(xnew, result, n.models = 1){
  m.weight <- weight(result$m.sics[1:n.models], result$k)
  l.pred   <- vector()
  for(i in 1:n.models){
    fit    <- result$c.models$models[[i]]
    v.s    <- as.numeric(colnames(fit$model)[-1])
    l.pred <- cbind(l.pred, as.matrix(cbind(1,xnew[,v.s]))%*% fit$coefficients)
  }
  wl.pred  <- l.pred %*% m.weight

  y.pred   <- switch(result$c.models$models[[1]]$family$family,
                     gaussian = wl.pred,
                     binomial = exp(wl.pred)/(1+exp(wl.pred)),
                     poisson  = exp(wl.pred))
  return(y.pred)
}
