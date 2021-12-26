#' The Metropolized restricted Gibbs sampler
#'
#' @param y        the response variable
#' @param x        the predictors
#' @param n.vars   the number of maximal predictors included in the candidate model
#' @param perm     the permutation of Gibbs sampler, default TRUE
#' @param n.models the number of top selected models
#' @param tau      the threshold to select the important predictors in second step, default is 0.9
#' @param len      the half number of generated samples, default is 1000
#' @param k        the tuning parameter, default is 1
#' @param gamma    the parameter for extended BIC, default is 0.5
#' @param info     the selected model selection criterion from AIC, AICc, BIC and exBIC
#' @param family   the type of model from linear, logistic, poisson
#'
#' @return a list of summary
#' @export

#' @examples
#' x <- matrix(rnorm(1000), ncol = 10);
#' y <- rowSums(x[,1:5]) + rnorm(100)
#' m.s <- GibbsSampler(y,x,info = "BIC", family = "gaussian")
#'

GibbsSampler <- function(y, x, n.vars = ncol(x), perm = TRUE, n.models = 10,
                         tau = 0.9, len = 1000, k = 1, gamma = 0.5,
                         info = c("AIC", "BIC", "AICc", "exBIC"),
                         family = c("gaussian","poisson", "binomial")){

  n <- dim(x)[1]
  p <- dim(x)[2]

  x.predictors <- colnames(x)
  colnames(x)  <- 1:p

  z  <- as.data.frame(cbind(y,x), stringsAsFactors = TRUE)
  colnames(z)[1] <- "y"

  s.index  <- c(rep(1,n.vars), rep(0, p - n.vars))
  m.models <- c(1, s.index)
  m.temp   <- glm(y ~ .,family = family, data = z[,c(1,s.index)==1])

  j <- 0
  while(j < 2*len){
      for(i in 1:p){

      if(perm){
        s <- sample(p,p)
      }else s <- 1:p

      t.index    <- s.index
      d.index    <- t.index
      d.index[s[i]] <- 1 - t.index[s[i]]

      if(0 < sum(d.index) & sum(d.index) <= n.vars){
        m.curr <- m.temp
        m.next <- glm(y ~ .,family = family, data = z[,c(1,d.index)==1])

        A  <- ac.ratio(m.curr, m.next, k, gamma, p, info = info)
        mu <- runif(1)
        if(mu < A){
          t.index[s[i]] <- 1 - t.index[s[i]]
          m.temp     <- m.next
        }
        else{
          m.temp     <- m.curr
        }
        s.index      <- t.index
      }
      else{
        s.index      <- t.index
      }
    }

    #store the sequence in a matrix
    m.models <- rbind(m.models, c(1,s.index))
    j <- j + 1
  }
  m.matrix <- burn.seq(m.models, len)

  result <- result.GibbsSampler(m.matrix, y, x, k, gamma,
                                p, n.models, info, family)

  v.prob <- colSums(m.matrix[,-1])/len
  v.select  <- x.predictors[v.prob > tau]

  result$v.prob   <- v.prob
  result$v.select <- v.select
  result$tau      <- tau
  result$x.predictors <- x.predictors

  return(result)
}
