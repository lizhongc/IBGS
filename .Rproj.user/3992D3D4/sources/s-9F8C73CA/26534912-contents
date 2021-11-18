#' The function for model selection criterion value
#'
#' @param m.index  the model index
#' @param y        the response variable
#' @param x        the predictors
#' @param info     the selected model selection criterion from AIC, AICc, BIC and exBIC
#' @param family   the type of model from linear, logistic, poisson
#' @param gamma    the tuning parameter to control the penalty
#' @param p0       the number of total predictors
#'
#' @return model selection criterion value for selected model
#' @export

v.sic <- function(m.index, y, x, gamma, p0, info, family){
  z <- as.data.frame(cbind(y,x))
  colnames(z)[1] <- "y"
  SIC <- switch(info,
         AIC   = AIC(glm(y~., data = z[,m.index==1], family = family)),
         AICc  = AICc(glm(y~., data = z[,m.index==1], family = family)),
         BIC   = BIC(glm(y~., data = z[,m.index==1], family = family)),
         exBIC = exBIC(glm(y~., data = z[,m.index==1], family = family), gamma, p0),
         )
  return(SIC)
}
