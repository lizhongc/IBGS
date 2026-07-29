library(IBGS)
source("helper-sim.R")

# cor.check stops on a duplicated covariate
set.seed(41)
d  <- sim_gauss(n = 80, p = 10)
xx <- cbind(d$x, d$x[, 1])                    # an exact duplicate column
expect_err(glmGibbs(d$y, xx, cor.check = 0.999), "highly correlated")

# cor.check = NULL (default) skips the scan
set.seed(42)
d  <- sim_gauss(n = 80, p = 10)
xx <- cbind(d$x, d$x[, 1])
stopifnot(inherits(glmGibbs(d$y, xx, criterion = "BIC", n.draws = 60), "IBGS"))

# invalid criterion and family are rejected
set.seed(43)
d <- sim_gauss(n = 80, p = 10)
expect_err(glmGibbs(d$y, d$x, criterion = "ZZZ"), ".")
expect_err(glmGibbs(d$y, d$x, family = "weibull"), ".")

# ebic.gamma defaults to 1, consistent for any p = O(n^kappa); start and fast are
# no longer arguments (the chains always start null, proposals always score fast)
for (f in list(glmGibbs, glmIBGS, coxGibbs, coxIBGS, lmeGibbs, lmeIBGS))
  stopifnot(formals(f)$ebic.gamma == 1,
            !any(c("start", "fast") %in% names(formals(f))))

# and is held in [max(0, 1 - log(n)/(2*log(p))), 1]
gfloor <- IBGS:::.ebic.gamma.floor
stopifnot(all.equal(suppressMessages(gfloor(0.25, "exBIC", 80, 200)),
                    1 - log(80) / (2 * log(200))),   # below the floor: raised
          gfloor(1, "exBIC", 80, 200) == 1,          # the default: untouched
          suppressMessages(gfloor(2, "exBIC", 80, 200)) == 1,   # above 1: capped
          suppressMessages(gfloor(-1, "exBIC", 1e6, 2)) == 0,   # negative: floored
          gfloor(0.25, "BIC", 80, 200) == 0.25,      # other criteria ignore it
          gfloor(0.25, "exBIC", 80, 1) == 0.25,      # p < 2: no log(1) divide
          gfloor(0.25, "exBIC", 1, 200) == 0.25)     # n < 2: likewise

# the sampler reports the change and still returns a fit
set.seed(44)
d    <- sim_gauss(n = 80, p = 200)
msgs <- character(0)
fit  <- withCallingHandlers(
  glmGibbs(d$y, d$x, criterion = "exBIC", ebic.gamma = 0.25, max.size = 10,
           n.draws = 60),
  message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
stopifnot(inherits(fit, "IBGS"),
          any(grepl("set to 0.58647 (from 0.25)", msgs, fixed = TRUE)))
