#' Title
#'
#' @param y        the response variable
#' @param x1       the non-important predictors
#' @param x2       the important predictors
#' @param n.models the number of top selected models
#' @param x.predictors the names of predictors
#' @param H        the number of predictors in small groups
#' @param kapp     the number of selected predictors in first step
#' @param tau      the threshold to select the important predictors in second step
#' @param perm     the permutation of Gibbs sampler
#' @param len      the half number of generated samples
#' @param k        the tuning parameter
#' @param gamma    the parameter for extended BIC
#' @param p0       the number of all predictors
#' @param info     the selected model selection criterion
#' @param family   the type of model
#'
#' @return a list containg all results
#' @export

BlockGibbsSampler.step3 <- function(y, x1, x2, n.models, x.predictors, H, kapp, tau, perm, len, k,
                                    gamma, p0, info, family){
  n  <- dim(x1)[1]
  p1 <- dim(x1)[2]
  if(is.null(dim(x2))){
    p2       <- 0
    h        <- ceiling(dim(x1)[2]/(min(H, n-p2)))
    v.freq   <- BlockGibbsSampler.step1(y, x1, x2, h, perm, len, k, gamma, p0, info, family)
    x.s      <- x1[,order(v.freq, decreasing = TRUE)[1:kapp]]
  }else{
    p2       <- dim(x2)[2]
    h        <- ceiling(dim(x1)[2]/(min(H, n-p2)))
    v.freq   <- BlockGibbsSampler.step1(y, x1, x2, h, perm, len, k, gamma, p0, info, family)
    x.s      <- cbind(x1[,order(v.freq, decreasing = TRUE)[1:kapp]], x2)
  }
  p.s       <- dim(x.s)[2]
  s.index   <- rep(1, p.s)
  m.matrix  <- GibbsSamplerStep(y, x.s, x2 = vector(), s.index, perm, 4*len, k, gamma, p0, info, family)

  result <- result.GibbsSampler(m.matrix, y, x.s, k, gamma, p0, n.models, info, family)

  v.prob    <- rep(0, p1+p2)
  v.prob[as.numeric(colnames(x.s))] <- colSums(m.matrix[,-1])/(4*len)
  v.select  <- x.predictors[v.prob > tau]

  result$v.prob   <- v.prob
  result$v.select <- v.select
  result$tau      <- tau

  return(result)
}
