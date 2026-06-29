# Small deterministic simulators and assertion helpers shared by the tests.
# Each simulator puts a strong signal in the first three columns so the true
# active set is unambiguous; the matrices have no column names, so IBGS labels
# the predictors V1, V2, ... and `truth_names()` reproduces those labels.
#
# The package tests use the traditional base-R mechanism (plain tests/*.R
# scripts checked against *.Rout.save), so the assertion verbs below replace
# the former testthat ones: each stops on failure and prints nothing on success.

sim_gauss <- function(n = 80, p = 40, truth = 1:3, b = c(3, -3, 3)) {
  x <- matrix(rnorm(n * p), n, p)
  y <- as.numeric(x[, truth, drop = FALSE] %*% b + rnorm(n))
  list(x = x, y = y, truth = truth)
}

sim_binom <- function(n = 100, p = 30, truth = 1:3, b = c(2.5, -2.5, 2.5)) {
  x   <- matrix(rnorm(n * p), n, p)
  eta <- as.numeric(x[, truth, drop = FALSE] %*% b)
  y   <- rbinom(n, 1, plogis(eta))
  list(x = x, y = y, truth = truth)
}

sim_pois <- function(n = 100, p = 30, truth = 1:3, b = c(1, -1, 1)) {
  x  <- matrix(rnorm(n * p), n, p)
  mu <- exp(as.numeric(x[, truth, drop = FALSE] %*% b))
  y  <- rpois(n, mu)
  list(x = x, y = y, truth = truth)
}

sim_cox <- function(n = 60, p = 20, truth = 1:3, b = c(2, -2, 2)) {
  x      <- matrix(rnorm(n * p), n, p)
  time   <- rexp(n, exp(as.numeric(x[, truth, drop = FALSE] %*% b)))
  status <- rbinom(n, 1, 0.8)
  list(x = x, time = time, status = status, truth = truth)
}

sim_lme <- function(n = 80, p = 24, g = 8, truth = 1:3, b = c(3, -3, 3)) {
  x   <- matrix(rnorm(n * p), n, p)
  grp <- gl(g, n / g)
  y   <- as.numeric(x[, truth, drop = FALSE] %*% b) + rnorm(g)[grp] + rnorm(n)
  list(x = x, y = y, group = grp, truth = truth)
}

# the predictor names IBGS assigns when x has no column names
truth_names <- function(truth) paste0("V", truth)

# expect `expr` to raise an error whose message matches the regexp `pat`
expect_err <- function(expr, pat) {
  e <- tryCatch(expr, error = function(e) e)
  stopifnot(inherits(e, "error"), grepl(pat, conditionMessage(e)))
}

# common structural checks for an "IBGS" result over p predictors
expect_ibgs <- function(fit, p) {
  stopifnot(inherits(fit, "IBGS"))
  need <- c("marginal.prob", "selected.vars", "var.names", "coef",
            "model.ic", "model.freq", "ic.trace", "criterion")
  stopifnot(all(need %in% names(fit)))
  stopifnot(length(fit$marginal.prob) == p)
  stopifnot(length(fit$var.names) == p)
}

# robust recovery check: the true predictors rank among the top few by marginal
# inclusion probability (tolerant to the exact tau threshold and to BLAS noise)
expect_recovers <- function(fit, truth) {
  top <- fit$var.names[order(fit$marginal.prob, decreasing = TRUE)]
  stopifnot(all(truth_names(truth) %in% top[seq_len(length(truth) + 2L)]))
}
