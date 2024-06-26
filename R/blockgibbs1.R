#' The first step in the block Gibbs sampler search algorithm
#'
#' @param y        the response variable
#' @param x1       the non-important predictors
#' @param x2       the important predictors
#' @param h        the number of small groups
#' @param len      the half number of generated samples
#' @param perm     the permutation of Gibbs sampler
#' @param k        the tuning parameter
#' @param gamma    the parameter for extended BIC
#' @param p0       the number of all predictors
#' @param info     the selected model selection criterion
#' @param family   the type of model

#' @return the marginal probability for each predictor
#' @export

BlockGibbsSampler.step1 <- function(y, x1, x2, h, perm, len, k, gamma, p0, info, family){
  index    <- sample(x = h, size = dim(x1)[2], replace = TRUE, prob = rep(1/h, h))

  v.freq   <- foreach(i = 1:h, .combine=c) %dopar% {
    xi            <- x1[, index == i]
    pi            <- dim(xi)[2]
    s.model       <- rep(1,pi)
    m.matrix      <- GibbsSamplerStep(y, xi, x2, s.model, perm, len, k, gamma, p0, info, family)
    v.prob        <- colSums(m.matrix)/len
    v.prob[2:(pi+1)]
  }
  return(r.index(v.freq, index, h))
}
