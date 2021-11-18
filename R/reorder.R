#' re-order the predictor sequence
#'
#' @param x     the order of predictors in groups
#' @param index the index to divide predictors into small groups
#' @param H     the number of groups
#'
#' @return      the correct order of predictors
#' @export

r.index <- function(x, index, H){
  x.r <- rep(0, length(x))
  for (i in 1:H) {
    d <- sum(index == i)
    x.r[index == i] <- x[1:d]
    x <- x[-(1:d)]
  }
  return(x.r)
}
