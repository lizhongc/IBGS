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
