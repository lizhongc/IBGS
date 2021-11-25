#' Model frequency figure for model selection
#'
#' @param result a list of summary results
#'
#' @export
#'
plots.mf <- function(result){
  m.table <- table(round(result$m.sic,2))/length(result$m.sic)
  plot(m.table, xlab = paste(result$info, "values"), ylab = "Model frequency")
}
