#' Binary table for imbalanced data
#'
#' @param y       observations in the data
#' @param y_pred  predictions of the model
#' @param type    response type
#'
#' @return        a binary table
#' @export
#'
binary.table <- function(y, y_pred, type = c("link", "response")){
  m <- matrix(rep(0,4), ncol = 2)
  rownames(m) <- c("Positive", "Negative")
  colnames(m) <- c("True", "False")

  if(type == "link"){
    m[1,1] <- sum(y == 1 & y_pred > 0) #TP
    m[1,2] <- sum(y == 0 & y_pred > 0) #FP
    m[2,1] <- sum(y == 1 & y_pred < 0) #FN
    m[2,2] <- sum(y == 0 & y_pred < 0) #TN
  }
  else{
    m[1,1] <- sum(y == 1 & y_pred > 0.5)
    m[1,2] <- sum(y == 0 & y_pred > 0.5)
    m[2,1] <- sum(y == 1 & y_pred < 0.5)
    m[2,2] <- sum(y == 0 & y_pred < 0.5)
  }
  return(m)
}

#' Imbalanced metrics
#'
#' @param m a binary table
#'
#' @return  a vector consisting of imbalanced metrics
#' @export
#'
imbalanced.metric <- function(m){
  precision <- m[1,1]/(m[1,1] + m[1,2])
  recall    <- m[1,1]/(m[1,1] + m[2,1])
  f.measure <- (2*precision*recall)/(precision + recall)
  return(c(precision, recall, f.measure))
}
