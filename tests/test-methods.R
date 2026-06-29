library(IBGS)
source("helper-sim.R")

# coef returns named coefficient vectors (single and averaged)
set.seed(31)
d   <- sim_gauss(n = 100, p = 20)
fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

b1 <- coef(fit)
stopifnot(length(b1) == ncol(d$x) + 1L)
stopifnot(names(b1)[1] == "(Intercept)")

ba <- coef(fit, average = TRUE)
stopifnot(length(ba) == ncol(d$x) + 1L)
stopifnot(is.numeric(ba))

# predict / fitted give sensible values per family
set.seed(32)
dg <- sim_gauss(n = 100, p = 20)
fg <- glmGibbs(dg$y, dg$x, criterion = "BIC", n.draws = 60)
pg <- predict(fg, dg$x[1:5, ])
stopifnot(length(pg) == 5)
stopifnot(is.numeric(pg))

# fitted() are the stored training predictions; predict(newdata = NULL) agrees,
# and recomputing on the training design matches as well
fv <- fitted(fg)
stopifnot(length(fv) == nrow(dg$x))
stopifnot(isTRUE(all.equal(fv, predict(fg))))
stopifnot(isTRUE(all.equal(unname(predict(fg, dg$x)), unname(fv))))

# the single best model is available too
stopifnot(length(predict(fg, dg$x[1:5, ], average = FALSE)) == 5)

set.seed(33)
db <- sim_binom(n = 120, p = 20)
fb <- glmGibbs(db$y, db$x, family = "binomial", criterion = "BIC",
               n.draws = 60)
pb <- predict(fb, db$x[1:10, ])
stopifnot(length(pb) == 10)
stopifnot(all(pb >= 0 & pb <= 1))          # binomial response = probabilities
# the link scale differs from the response scale for binomial
stopifnot(!isTRUE(all.equal(predict(fb, db$x[1:10, ], type = "link"), pb)))

# summary and print methods work
set.seed(34)
d   <- sim_gauss(n = 100, p = 20)
fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

s <- summary(fit)
stopifnot(inherits(s, "summary.IBGS"))
out <- capture.output(wv <- withVisible(print(fit)))
stopifnot(length(out) > 0)      # print emits text ...
stopifnot(!wv$visible)          # ... and returns invisibly

# the plot methods run without error
set.seed(35)
d   <- sim_gauss(n = 100, p = 20)
fit <- glmGibbs(d$y, d$x, criterion = "BIC", n.draws = 60)

pdf(NULL)
plot(fit)
plotICtrace(fit)
plotModelFreq(fit)
plotMargProb(fit)
plotMargProb(fit, horizontal = TRUE)
plotModelFreq(fit, cumulative = TRUE, horizontal = TRUE)
invisible(dev.off())
