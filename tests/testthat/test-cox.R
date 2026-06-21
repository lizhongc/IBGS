test_that("coxIBGS / coxGibbs run and have the right (intercept-free) structure", {
  set.seed(11)
  d  <- sim_cox()
  fi <- coxIBGS(d$time, d$status, d$x, criterion = "BIC",
                block.size = 10, n.keep = 10, n.refine = 2, n.draws = 150)
  expect_ibgs(fi, ncol(d$x))
  expect_equal(fi$family, "cox")
  expect_false(fi$has.intercept)               # the Cox model has no intercept
  expect_equal(nrow(fi$coef), ncol(d$x))       # so no intercept row

  fg <- coxGibbs(d$time, d$status, d$x, criterion = "BIC", n.draws = 150)
  expect_ibgs(fg, ncol(d$x))
  expect_recovers(fg, d$truth)

  pr <- predict(fg, d$x[1:5, ])                # relative risk exp(x beta)
  expect_length(pr, 5)
  expect_true(all(pr > 0))
  expect_length(fitted(fg), length(d$time))
})

test_that("cox samplers reject a status of the wrong length", {
  set.seed(12)
  d <- sim_cox()
  expect_error(coxGibbs(d$time, d$status[-1], d$x), "length")
  expect_error(coxIBGS(d$time, d$status[-1], d$x), "length")
})
