#' The acceptance ratio of jumping from the current model to the next model
#'
#' @param m.curr The current model
#' @param m.next The next model
#' @param k     The tuning parameter
#' @param gamma The parameter for extended BIC
#' @param p0    The number of total predictors
#' @param info  The selected information criterion
#'
#' @return The acceptance ratio value
#' @export
#'
ac.ratio <- function(m.curr, m.next, k, gamma = 0, p0 = 0, info = c("AIC", "BIC", "AICc", "exBIC")){
  switch(info,
         AIC    = sigmoid(-AIC(m.next)   + AIC(m.curr), k),
         AICc   = sigmoid(-AICc(m.next)  + AICc(m.curr), k),
         BIC    = sigmoid(-BIC(m.next)   + BIC(m.curr), k),
         exBIC  = sigmoid(-exBIC(m.next, gamma, p0) + exBIC(m.curr, gamma, p0), k)
         )
}
