test_that("coef returns named coefficient vectors (single and averaged)", {
  set.seed(31)
  d   <- sim_gauss(n = 100, p = 20)
  fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

  b1 <- coef(fit)
  expect_length(b1, ncol(d$x) + 1L)
  expect_equal(names(b1)[1], "(Intercept)")

  ba <- coef(fit, average = TRUE)
  expect_length(ba, ncol(d$x) + 1L)
  expect_true(is.numeric(ba))
})

test_that("predict / fitted give sensible values per family", {
  set.seed(32)
  dg <- sim_gauss(n = 100, p = 20)
  fg <- glmGibbs(dg$y, dg$x, criterion = "BIC", n.draws = 60)
  pg <- predict(fg, dg$x[1:5, ])
  expect_length(pg, 5)
  expect_true(is.numeric(pg))

  # fitted() are the stored training predictions; predict(newdata = NULL) agrees,
  # and recomputing on the training design matches as well
  fv <- fitted(fg)
  expect_length(fv, nrow(dg$x))
  expect_equal(fv, predict(fg))
  expect_equal(unname(predict(fg, dg$x)), unname(fv))

  # the single best model is available too
  expect_length(predict(fg, dg$x[1:5, ], average = FALSE), 5)

  set.seed(33)
  db <- sim_binom(n = 120, p = 20)
  fb <- glmGibbs(db$y, db$x, family = "binomial", criterion = "BIC",
                 n.draws = 60)
  pb <- predict(fb, db$x[1:10, ])
  expect_length(pb, 10)
  expect_true(all(pb >= 0 & pb <= 1))          # binomial response = probabilities
  # the link scale differs from the response scale for binomial
  expect_false(isTRUE(all.equal(predict(fb, db$x[1:10, ], type = "link"), pb)))
})

test_that("summary and print methods work", {
  set.seed(34)
  d   <- sim_gauss(n = 100, p = 20)
  fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

  s <- summary(fit)
  expect_s3_class(s, "summary.IBGS")
  expect_output(print(fit))                    # print emits text ...
  expect_invisible(print(fit))                 # ... and returns invisibly
})

test_that("the plot methods run without error", {
  set.seed(35)
  d   <- sim_gauss(n = 100, p = 20)
  fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

  pdf(tempfile())
  on.exit(dev.off())
  expect_no_error(plot(fit))
  expect_no_error(plotIchart(fit))
  expect_no_error(plotModelFreq(fit))
  expect_no_error(plotVarProb(fit))
})
