#' Gibbs sampler search algorithm
#'
#' @param y        the response variable
#' @param x1       the non-important predictors
#' @param x2       the important predictors
#' @param s.model  the start model
#' @param perm     the permutation of Gibbs sampler
#' @param len      the half number of generated samples
#' @param k        the tuning parameter
#' @param gamma    the parameter for extended BIC
#' @param p0       the number of all predictors
#' @param info     the selected model selection criterion
#' @param family   the type of model
#'
#' @return a matrix of generated candidate model samples
#' @export
#'
GibbsSamplerStep <- function(y, x1, x2, s.model, perm, len, k, gamma, p0, info, family){

  p1 <- dim(x1)[2]
  if(is.null(dim(x2))){
    z  <- as.data.frame(cbind(y,x1))
    p2 <- 0
  }else{
    z  <- as.data.frame(cbind(y,x1,x2))
    p2 <- dim(x2)[2]
  }
  colnames(z)[1] <- "y"

  s.index <- c(s.model, rep(1,p2))
  m.models <- c(1, s.index)
  m.temp <- glm(y ~ .,family = family, data = z[,c(1,s.index)==1])

  j <- 0
  while(j < 2*len){
      for(i in 1:p1){

      if(perm){
        s <- sample(p1,p1)
      }else s <- 1:p1

      t.index    <- s.index
      d.index    <- t.index
      d.index[s[i]] <- 1 - t.index[s[i]]

      if(0 < sum(d.index)){
        m.curr <- m.temp
        m.next <- glm(y ~ .,family = family, data = z[,c(1,d.index)==1])

        A  <- ac.ratio(m.curr, m.next, k, gamma, p0, info = info)
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
  return(burn.seq(m.models, len))
}
