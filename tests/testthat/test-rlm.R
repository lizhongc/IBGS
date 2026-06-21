test_that("rlmIBGS / rlmGibbs with a grouping factor work and carry BLUPs", {
  set.seed(21)
  d  <- sim_rlm()
  fi <- rlmIBGS(d$y, d$x, group = d$group, criterion = "BIC",
                block.size = 15, n.keep = 10, n.refine = 2, n.draws = 150)
  expect_ibgs(fi, ncol(d$x))
  expect_true("re" %in% names(fi))             # random-effect BLUPs
  expect_recovers(fi, d$truth)

  fg <- rlmGibbs(d$y, d$x, group = d$group, criterion = "BIC", n.draws = 150)
  expect_ibgs(fg, ncol(d$x))
  expect_true("re" %in% names(fg))

  expect_length(fitted(fg), length(d$y))
  pc <- predict(fg, d$x[1:5, ], group.new = d$group[1:5])  # conditional prediction
  expect_length(pc, 5)
  expect_true(is.numeric(pc))
})

test_that("rlm accepts a general random part via Z + varcomp", {
  set.seed(22)
  d   <- sim_rlm(g = 5)
  Z   <- model.matrix(~ d$group - 1)            # group-indicator random-effects design
  fit <- rlmGibbs(d$y, d$x, Z = Z,
                  varcomp = list(sigma2b = 1, sigma2e = 1), criterion = "BIC",
                  n.draws = 60)
  expect_ibgs(fit, ncol(d$x))
  expect_true("re" %in% names(fit))
})

test_that("rlm accepts a directly supplied marginal covariance V (no BLUPs)", {
  set.seed(23)
  d   <- sim_rlm()
  V   <- diag(nrow(d$x))                         # identity => ordinary least squares
  fit <- rlmGibbs(d$y, d$x, V = V, criterion = "BIC", n.draws = 60)
  expect_ibgs(fit, ncol(d$x))
  expect_null(fit$re)                            # a bare V yields no random-effect BLUPs
})

test_that("rlm errors when no random part is specified", {
  set.seed(24)
  d <- sim_rlm()
  expect_error(rlmGibbs(d$y, d$x), "random part")
  expect_error(rlmIBGS(d$y, d$x), "random part")
})
