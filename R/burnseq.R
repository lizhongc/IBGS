#' the function to burn first samples out
#'
#' @param m the model sequence matrix
#' @param l the number of burned samples
#'
#' @return a matrix burning first l off
#' @export
#'
burn.seq <- function(m, l){
  return(m[(nrow(m)-l+1):nrow(m),])
}
