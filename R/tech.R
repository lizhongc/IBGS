#' Remove highly correlation predictors
#'
#' @param x     data set
#' @param rho   threshold for correlation
#'
#' @return      a new data set
#' @export
remove.highly.cor <- function(x, rho){
  tmp <- cor(x)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0

  x1 <- x[, !apply(tmp, 2, function(x) any(abs(x) > rho, na.rm = TRUE))]
  x2 <- x[,  apply(tmp, 2, function(x) any(abs(x) > rho, na.rm = TRUE))]

  x0 <- list()
  x0$extracted <- x1
  x0$removed   <- x2

  return(x0)
}

#' Set up train and test sets
#'
#' @param x        data set
#' @param prob     proportion of division
#'
#' @return         a list containing train and test sets
#' @export

divide <- function(x, prob = c(0.8,0.2)){
  n <- dim(x)[1]
  index <- sample(x = 2, size = n, replace = TRUE, prob)

  x0 <- list()
  x0$train <- data[index == 1,]
  x0$test  <- data[index == 2,]
  return(x0)
}

#' Leave one out Cross-Validation in GLM
#'
#' @param glmfit a GLM model
#'
#' @return a vector of loocv prediction
#' @export

CV <- function(glmfit){
  data    <- glmfit$model
  n       <- nrow(data)
  seq_len <- 1:n
  y.cv    <- rep(0,n)
  for(i in 1:n) {
    j.out <- seq_len == i
    j.in  <- seq_len != i
    d.glm <- glm(y~., data =  data[j.in, , drop=FALSE], family = glmfit$family)
    y.cv[i] <- predict(d.glm, data[j.out, , drop=FALSE], type = "link")
  }
  return(y.cv)
}
