#' plot the marginal probability for top predictors
#'
#' @param result the result from IBGS
#' @param n.vars the number of top predictors

#' @export

plots.vr <- function(result, n.vars = 20){
  colors  <- rep(0,n.vars)
  v.order <- order(result$v.prob, decreasing = TRUE)
  v.freq  <- result$v.prob[v.order[1:n.vars]]

  colors[v.freq >  result$tau]    <- 2
  colors[v.freq <= result$tau ]   <- 1
  plot(1:n.vars, v.freq, xlab = "", ylab = "Marginal Probability",
       xaxt = "n", main = "", type = "h", col = colors, ylim = c(0,1))
  mtext(result$x.predictors[v.order[1:n.vars]], side = 1, line = 0.25,
        at = 1:n.vars, las = 2, cex = 1)
}
