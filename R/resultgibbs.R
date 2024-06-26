#' a summary of partial results
#'
#' @param m.matrix  a matrix of generated samples
#' @param y         the response variable
#' @param x         the predictors
#' @param n.models  the number of top selected models
#' @param info      the selected model selection criterion from AIC, AICc, BIC and exBIC
#' @param family    the type of model from linear, logistic, poisson
#' @param k         the tuning parameter
#' @param gamma     the tuning parameter to control the penalty
#' @param p0        the number of total predictors
#'
#' @return a list containing partial results
#' @export

result.GibbsSampler <- function(m.matrix, y, x, k, gamma, p0, n.models, info, family){
  z <- as.data.frame(cbind(y,x))
  colnames(z)[1] <- "y"

  n <- dim(m.matrix)[1]
  m.sic <- vector()
  for(i in 1:n){
    m.sic <- c(m.sic, v.sic(m.matrix[i,], y, x, gamma, p0, info, family))
  }

  m.sic.df <- as.data.frame(table(m.sic))
  m.sics <- as.numeric(levels(m.sic.df[,1]))

  if(length(m.sics) < n.models)
    n.models <- length(m.sics)

  m.order <- order(m.sic, decreasing = FALSE)
  m.index <- cumsum(m.sic.df[,2])
  m.models <- list()
  for (i in 1:n.models) {
    model <- glm(y~., z[,m.matrix[m.order[m.index[i]],]==1], family = family)
    m.models[[i]] <- model
  }
  m.sicc <- m.sics[1:n.models]
  m.weights <- weight(m.sicc, k)

  return(list(n.models = n.models,
              m.sics   = m.sics,
              m.sic    = m.sic,
              m.seq    = m.matrix[,-1],
              c.models = list(models  = m.models,
                              weights = m.weights),
              k        = k,
              info     = info,
              family   = family
  ))
}
