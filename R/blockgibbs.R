#' The iterated block Gibbs sampler algorithm
#'
#' @param y        the response variable
#' @param x        the predictors
#' @param H        the number of predictors in small groups, default is 30
#' @param kapp     the number of selected predictors in first step, default is 20
#' @param tau      the threshold to select the important predictors in second step, default is 0.9
#' @param perm     the permutation of Gibbs sampler, default is TRUE
#' @param len      the half number of generated samples, default is 250
#' @param k        the tuning parameter, default is 1
#' @param gamma    the parameter for extended BIC, default is 0.5
#' @param info     the selected model selection criterion from AIC, AICc, BIC and exBIC
#' @param family   the type of model from linear, logistic, poisson
#' @param n.iter   the number of iterations
#' @param n.models the number of top selected models
#'
#' @return a list contains a summary of final result
#' @export

BlockGibbsSampler <- function(y, x, n.iter = 3, n.models = 10, H = 30, kapp = 20,
                              tau = 0.9, perm = TRUE, len = 250, k = 1, gamma = 0.5,
                              info = c("AIC", "BIC", "AICc", "exBIC"),
                              family = c("gaussian","poisson", "binomial")){
  p <- dim(x)[2]
  x.predictors <- colnames(x)
  colnames(x)  <- 1:p

  x1 <- x
  x2 <- vector()

  if(n.iter < 2){
    result <- BlockGibbsSampler.step3(y, x1, x2, n.models, x.predictors, H,
                                      kapp, tau, perm, len, k, gamma, p, info, family)
  }
  else{
    j <- 1
    while(j < n.iter){
      v.select <- BlockGibbsSampler.step2(y, x1, x2, H, kapp, tau, perm, len, k,
                                          gamma, p, info, family)
      x1 <- x[, -v.select]
      x2 <- x[, v.select]

      j <- j+1
    }
    result <- BlockGibbsSampler.step3(y, x1, x2, n.models, x.predictors, H,
                                      kapp, tau, perm, len, k, gamma, p, info, family)
  }
  result$x.predictors <- x.predictors
  return(result)
}
