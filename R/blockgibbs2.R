#' The second step in the block Gibbs sampler search algorithm in the iterations
#'
#' @param y        the response variable
#' @param x1       the non-important predictors
#' @param x2       the important predictors
#' @param H        the number of predictors in small groups
#' @param kapp     the number of selected predictors in first step
#' @param tau      the threshold to select the important predictors in second step
#' @param len      the half number of generated samples
#' @param k        the tuning parameter
#' @param gamma    the parameter for extended BIC
#' @param p0       the number of all predictors
#' @param info     the selected model selection criterion
#' @param family   the type of model

#' @return  the number of selected columns
#' @export

BlockGibbsSampler.step2 <- function(y, x1, x2, H, kapp, tau, len,
                                    k, gamma, p0, info, family){
  x  <- cbind(x1,x2)
  z  <- as.data.frame(cbind(y,x1,x2))
  colnames(z)[1] <- "y"

  n  <- dim(x1)[1]
  p1 <- dim(x1)[2]
  if(is.null(dim(x2))){
    p2 <- 0
  }else
    p2 <- dim(x2)[2]

  h  <- ceiling(dim(x1)[2]/(min(H, n-p2)))
  v.freq   <- BlockGibbsSampler.step1(y, x1, x2, h, len, k, gamma, p0, info, family)

  x.s      <- cbind(x1[,order(v.freq, decreasing = TRUE)[1:kapp]], x2)
  p.s      <- dim(x.s)[2]

  s.index   <- rep(1, p.s)
  m.matrix  <- GibbsSampler(y, x.s, x2 = vector(), s.index, len,
                            k, gamma, p0, info, family)
  v.freq2    <- colSums(m.matrix)/len

  if(sum(v.freq2[-1] > tau) > 1){
    v.select   <- as.numeric(colnames(x.s)[v.freq2[-1] > tau])
  }
  else{
    v.select   <- as.numeric(colnames(x.s))
  }
  return(sort(v.select))
}
