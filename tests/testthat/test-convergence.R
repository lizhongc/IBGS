# The convergence diagnostics are reimplemented in C (src/diag.c) so the package
# does not depend on coda.  These tests check the kernels against base stats
# (stats::ar Yule-Walker, stats::acf) and short inline-R reimplementations of the
# Geweke and split-chain Gelman-Rubin formulas, plus structural checks on a fit.

# spectrum0.ar reference: AR(aic) spectral density at zero frequency, exactly as
# coda::spectrum0.ar (and matching the C kernel).
spec0_ref <- function(z) {
  a <- ar(z, aic = TRUE, method = "yule-walker")
  a$var.pred / (1 - sum(a$ar))^2
}

# Geweke z reference, with coda's window indexing on the 1..N grid.
geweke_ref <- function(x, frac1 = 0.1, frac2 = 0.5) {
  N  <- length(x)
  n1 <- floor(frac1 * (N - 1)) + 1
  s2 <- ceiling(frac2 * (N - 1))          # 0-based start of window 2
  w1 <- x[seq_len(n1)]
  w2 <- x[(s2 + 1):N]
  (mean(w1) - mean(w2)) /
    sqrt(spec0_ref(w1) / length(w1) + spec0_ref(w2) / length(w2))
}

# Univariate split-chain Gelman-Rubin reference, matching coda::gelman.diag
# (transform = FALSE, autoburnin = FALSE, confidence = 0.95).
gelman_ref <- function(x, m = 4) {
  N <- length(x)
  L <- N %/% m
  segs  <- lapply(seq_len(m), function(j) x[((j - 1) * L + 1):(j * L)])
  xbar  <- vapply(segs, mean, numeric(1))
  s2    <- vapply(segs, var,  numeric(1))
  Niter <- L
  Nchain <- m
  w      <- mean(s2)
  muhat  <- mean(xbar)
  b      <- Niter * var(xbar)
  var.w  <- var(s2) / Nchain
  var.b  <- 2 * b^2 / (Nchain - 1)
  cov.wb <- (Niter / Nchain) *
    (cov(s2, xbar^2) - 2 * muhat * cov(s2, xbar))
  V      <- (Niter - 1) * w / Niter + (1 + 1 / Nchain) * b / Niter
  var.V  <- ((Niter - 1)^2 * var.w + (1 + 1 / Nchain)^2 * var.b +
               2 * (Niter - 1) * (1 + 1 / Nchain) * cov.wb) / Niter^2
  df.V   <- 2 * V^2 / var.V
  df.adj <- (df.V + 3) / (df.V + 1)
  B.df   <- Nchain - 1
  W.df   <- 2 * w^2 / var.w
  R2.fixed  <- (Niter - 1) / Niter
  R2.random <- (1 + 1 / Nchain) * (1 / Niter) * (b / w)
  R2.upper  <- R2.fixed + qf(0.975, B.df, W.df) * R2.random
  c(point = sqrt(df.adj * (R2.fixed + R2.random)),
    upper = sqrt(df.adj * R2.upper))
}

test_that("ibgs_diag matches base-stats and inline references", {
  set.seed(101)
  n <- 2000
  x <- as.numeric(arima.sim(n = n, list(ar = 0.6)))
  d <- .Call("ibgs_diag", as.double(x), 4L, 30L, 20L, PACKAGE = "IBGS")

  # autocorrelations vs stats::acf
  ac_ref <- as.numeric(acf(x, lag.max = 30, plot = FALSE)$acf)
  expect_equal(as.numeric(d$acf), ac_ref, tolerance = 1e-8)
  expect_equal(d$acf.lag, 0:30)

  # effective size vs n * var / spectrum0.ar
  ess_ref <- n * var(x) / spec0_ref(x)
  expect_equal(d$ess, ess_ref, tolerance = 1e-5)

  # Geweke z and Gelman-Rubin R-hat vs the inline references
  expect_equal(d$geweke, geweke_ref(x), tolerance = 1e-5)
  gr <- gelman_ref(x, 4)
  expect_equal(d$gelman[1], unname(gr["point"]), tolerance = 1e-6)
  expect_equal(d$gelman[2], unname(gr["upper"]), tolerance = 1e-6)
})

test_that("ibgs_diag shrink data grows and is sane", {
  set.seed(202)
  x  <- as.numeric(arima.sim(n = 1000, list(ar = 0.4)))
  d  <- .Call("ibgs_diag", as.double(x), 4L, 20L, 15L, PACKAGE = "IBGS")
  sh <- d$shrink
  expect_true(length(sh$iter) >= 1)
  expect_true(all(diff(sh$iter) > 0))                 # endpoints increase
  expect_equal(sh$iter[length(sh$iter)], 1000)        # last uses the whole chain
  expect_true(all(sh$median >= 1 - 1e-8, na.rm = TRUE))
})

test_that("an IBGS fit carries tidy convergence diagnostics", {
  set.seed(7)
  dat <- sim_gauss(n = 120, p = 30)
  fit <- glmIBGS(dat$y, dat$x, criterion = "BIC", n.draws = 120)

  cv <- fit$convergence
  expect_true(is.list(cv))
  expect_true(all(c("gelman", "geweke", "ess", "autocorr", "shrink") %in%
                    names(cv)))
  expect_named(cv$gelman, c("point", "upper"))
  expect_true(is.na(cv$gelman[["point"]]) || cv$gelman[["point"]] >= 1 - 1e-8)
  expect_true(is.na(cv$ess) || cv$ess > 0)
  expect_equal(unname(cv$autocorr[["0"]]), 1, tolerance = 1e-8)
  expect_s3_class(cv$shrink, "data.frame")
})

test_that("summary prints a convergence block and the new plots run", {
  set.seed(8)
  dat <- sim_gauss(n = 120, p = 30)
  fit <- glmIBGS(dat$y, dat$x, criterion = "BIC", n.draws = 120)

  expect_output(print(summary(fit)), "Convergence diagnostics")

  pdf(NULL)
  on.exit(dev.off())
  expect_s3_class(plot(fit, which = "gelman"), "IBGS")
  expect_s3_class(plot(fit, which = "autocorr"), "IBGS")
})
