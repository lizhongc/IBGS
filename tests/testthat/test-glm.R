# Short runs (small n.draws / n.refine) keep the suite fast; the strong signal
# means the chain still concentrates on the true model.

test_that("glmIBGS (gaussian) recovers the signal and has IBGS structure", {
  set.seed(1)
  d   <- sim_gauss()
  fit <- glmIBGS(d$y, d$x, criterion = "BIC", n.refine = 2, n.draws = 150,
                 n.cores = 1)
  expect_ibgs(fit, ncol(d$x))
  expect_equal(fit$criterion, "BIC")
  expect_true(all(truth_names(d$truth) %in% fit$selected.vars))
  expect_equal(nrow(fit$coef), ncol(d$x) + 1L)   # intercept + p rows
  expect_equal(fit$family, "gaussian")
})

test_that("glmGibbs (gaussian) runs and ranks the true predictors highly", {
  set.seed(2)
  d   <- sim_gauss(n = 100, p = 20)
  fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 120)
  expect_ibgs(fit, ncol(d$x))
  expect_recovers(fit, d$truth)
})

test_that("permute = TRUE (random permutation) and FALSE both recover the signal", {
  set.seed(2)
  d  <- sim_gauss(n = 100, p = 20)
  ft <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 120, permute = TRUE)
  ff <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 120, permute = FALSE)
  expect_recovers(ft, d$truth)
  expect_recovers(ff, d$truth)
  # permute = TRUE must be reproducible under a fixed seed
  set.seed(2); a <- glmGibbs(d$y, d$x, n.draws = 120, permute = TRUE)
  set.seed(2); b <- glmGibbs(d$y, d$x, n.draws = 120, permute = TRUE)
  expect_equal(a$marginal.prob, b$marginal.prob)
})

test_that("binomial and poisson families run via both samplers", {
  set.seed(3)
  db <- sim_binom()
  fb <- glmIBGS(db$y, db$x, family = "binomial", criterion = "BIC",
                n.refine = 2, n.draws = 150)
  expect_ibgs(fb, ncol(db$x))
  expect_equal(fb$family, "binomial")
  expect_recovers(fb, db$truth)

  set.seed(4)
  dp <- sim_pois()
  fp <- glmGibbs(dp$y, dp$x, family = "poisson", criterion = "AIC",
                 n.draws = 150)
  expect_ibgs(fp, ncol(dp$x))
  expect_equal(fp$family, "poisson")
  expect_recovers(fp, dp$truth)
})

test_that("the criterion choice is honoured", {
  set.seed(5)
  d <- sim_gauss(n = 60, p = 10)
  for (crit in c("AIC", "BIC", "AICc", "exBIC")) {
    fit <- glmGibbs(d$y, d$x, criterion = crit, n.draws = 60)
    expect_equal(fit$criterion, crit)
  }
})
