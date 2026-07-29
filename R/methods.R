# S3 methods, shared plotting palette, and internal helpers for the "IBGS"
# result object.
#
# Every sampler (glmIBGS / glmGibbs / coxIBGS / coxGibbs / lmeIBGS / lmeGibbs)
# returns a list with class "IBGS" (assembled by .ibgs.object).  These methods
# make the final output read well: a compact print(), a tabular summary(), and a
# plot() that dispatches to the three diagnostic plots.  The model-averaging
# methods -- coef(), predict() and fitted() -- and their weight/linear-predictor
# helpers live in averaging.R.  This file also holds the internal helpers the
# samplers call: .ibgs.object() (builds the "IBGS" object from the compact C
# summary) and .check.high.cor() (the optional pre-fit near-collinearity guard).

# Shared, colour-blind-safe palette (Okabe-Ito) used by all three plot functions
# so the diagnostics have a consistent look.
.ibgs.cols <- function() {
  list(trace     = "grey40",    # the criterion trace / below-threshold needles
       limit1    = "#D55E00",   # first-half control limit (vermillion)
       limit2    = "#0072B2",   # whole-run control limit (blue)
       highlight = "#E69F00",   # best value reached (orange)
       selected  = "#009E73",   # above-threshold / selected predictors (green)
       muted     = "grey75",    # de-emphasised elements
       burnin    = "grey92")    # shaded first-half region
}

# size (number of predictors) of each retained top model, from its coefficient
# column (excluding the intercept row when present).
.model.sizes <- function(object) {
  rows <- if (isTRUE(object$has.intercept)) -1L else TRUE
  apply(object$coef, 2, function(b) sum(b[rows] != 0))
}

# Print a concise summary of an IBGS fit
#
# Arguments:
#   x   an "IBGS" result from one of the samplers
#   ... ignored
# Value: x, invisibly
print.IBGS <- function(x, ...) {
  np  <- length(x$var.names)
  sel <- x$selected.vars
  i1  <- which.min(x$model.ic)          # best (lowest-criterion) model
  size1 <- .model.sizes(x)[i1]

  cat("Iterated Block Gibbs Sampler (IBGS)\n")
  cat(strrep("-", 35), "\n", sep = "")
  cat(sprintf("Family:     %s\n", x$family))
  cat(sprintf("Criterion:  %s\n", x$criterion))
  cat(sprintf("Predictors: %d   |   Selected (marginal prob > %.2g): %d\n",
              np, x$threshold, length(sel)))
  if (length(sel)) {
    shown <- if (length(sel) > 15) c(sel[1:15], "...") else sel
    cat("  ", paste(shown, collapse = ", "), "\n", sep = "")
  } else {
    cat("  (none above threshold)\n")
  }
  cat(sprintf("Best model: %s = %.4g,  size = %d,  visit freq = %.3f\n",
              x$criterion, x$model.ic[i1], size1, x$model.freq[i1]))
  cat(sprintf("Models retained: %d\n", x$n.models))
  cat("\nUse summary() for the variable/model tables and plot() for diagnostics.\n")
  invisible(x)
}

# Tabular summary of an IBGS fit
#
# Arguments:
#   object an "IBGS" result from one of the samplers
#   ...    ignored
# Value: invisibly, an object of class "summary.IBGS" with the selected
#   variable table and the top-model table
summary.IBGS <- function(object, ...) {
  # selected-variable table: marginal probability + best-model coefficient,
  # ordered by marginal probability (descending)
  sel <- object$selected.vars
  if (length(sel)) {
    idx  <- match(sel, object$var.names)
    brow <- if (isTRUE(object$has.intercept)) idx + 1L else idx
    bcol <- object$coef[brow, which.min(object$model.ic)]
    vtab <- data.frame(variable     = sel,
                       marginal.prob = object$marginal.prob[idx],
                       best.coef     = bcol,
                       row.names     = NULL,
                       stringsAsFactors = FALSE)
    vtab <- vtab[order(vtab$marginal.prob, decreasing = TRUE), , drop = FALSE]
    rownames(vtab) <- NULL
  } else {
    vtab <- data.frame()
  }

  # top-model table: rank, criterion value, visit frequency, model size.  Only
  # the first n.models entries of model.ic (which is the full ascending list of
  # distinct criterion values) are the retained models aligned with coef /
  # model.freq, so the table covers just those.
  nm   <- object$n.models
  mtab <- data.frame(rank = seq_len(nm),
                     ic   = object$model.ic[seq_len(nm)],
                     freq = object$model.freq,
                     size = .model.sizes(object),
                     row.names = NULL)

  structure(list(family = object$family, criterion = object$criterion,
                 threshold = object$threshold, n.pred = length(object$var.names),
                 vars = vtab, models = mtab, convergence = object$convergence),
            class = "summary.IBGS")
}

# Print method for the summary.IBGS table object
#
# Arguments:
#   x   a "summary.IBGS" object
#   ... ignored
print.summary.IBGS <- function(x, ...) {
  cat(sprintf("IBGS fit: family = %s, criterion = %s, predictors = %d\n",
              x$family, x$criterion, x$n.pred))
  cat(sprintf("\nSelected variables (marginal prob > %.2g):\n", x$threshold))
  if (nrow(x$vars)) {
    v <- x$vars
    v$marginal.prob <- formatC(v$marginal.prob, format = "f", digits = 3)
    v$best.coef     <- formatC(v$best.coef,     format = "g", digits = 4)
    print(v, row.names = FALSE)
  } else {
    cat("  (none above threshold)\n")
  }
  cat(sprintf("\nTop %d models (by %s):\n", nrow(x$models), x$criterion))
  m <- x$models
  m$ic   <- formatC(m$ic,   format = "f", digits = 4)
  m$freq <- formatC(m$freq, format = "f", digits = 3)
  print(m, row.names = FALSE)

  # convergence diagnostics of the criterion trace (see .ibgs.diag)
  cv <- x$convergence
  if (!is.null(cv)) {
    cat(sprintf("\nConvergence diagnostics (on the %s trace):\n", x$criterion))
    cat(sprintf("  Gelman-Rubin R-hat: %.3f (upper %.3f)\n",
                cv$gelman[["point"]], cv$gelman[["upper"]]))
    cat(sprintf("  Geweke z:           %.3f\n", cv$geweke))
    cat(sprintf("  Effective size:     %.1f\n", cv$ess))
    cat(sprintf("  Autocorrelation lag 1: %.3f\n", cv$autocorr[["1"]]))
  }
  invisible(x)
}

# Diagnostic plots for an IBGS fit
#
# Dispatches to the diagnostic plots: the trace of the criterion sequence
# (plotICtrace()), the marginal inclusion probabilities (plotMargProb()), the
# top-model visit frequencies (plotModelFreq()), the Gelman-Rubin shrink factor
# (plotGelman()), and the trace autocorrelation (plotAutocorr()).  Several may be
# requested at once, in which case they are drawn in a multi-panel layout.
#
# Arguments:
#   x     an "IBGS" result
#   which one or more of "ictrace", "margprob", "modelfreq", "gelman",
#         "autocorr"; default is all five
#   ...   further graphical parameters, forwarded only when a single panel
#         is requested
# Value: x, invisibly
plot.IBGS <- function(x, which = c("ictrace", "margprob", "modelfreq",
                                   "gelman", "autocorr"), ...) {
  which <- match.arg(which, c("ictrace", "margprob", "modelfreq",
                             "gelman", "autocorr"),
                     several.ok = TRUE)
  draw  <- list(ictrace   = plotICtrace,
                margprob  = plotMargProb,
                modelfreq = plotModelFreq,
                gelman    = plotGelman,
                autocorr  = plotAutocorr)
  if (length(which) > 1) {
    ncol <- min(length(which), 2L)
    nrow <- ceiling(length(which) / ncol)
    old  <- par(mfrow = c(nrow, ncol))
    on.exit(par(old))
    for (w in which) draw[[w]](x)
  } else {
    draw[[which]](x, ...)
  }
  invisible(x)
}

# Internal helper: assemble the "IBGS" result object from the compact C summary.
#
# The C sampler now returns the model-averaging summary directly (coef / model.ic
# / model.freq / ic.trace), so the former R-side .fit.summary -- which received
# the whole indicator matrix and refit each top model through the
# glm_coef/cox_coef/lme_coef .Call wrappers -- is gone.  The samplers attach
# marginal.prob, selected.vars, threshold, var.names (and, for lme, the
# random-effect block) and the averaged training linear.predictors around this.
.ibgs.object <- function(out, inv.temp, criterion, family, has.intercept) {
  obj <- structure(
    list(n.models      = ncol(out$coef),
         model.ic      = out$model.ic,    # criterion of each top model (ascending)
         model.freq    = out$model.freq,  # visit frequency of each top model
         ic.trace      = out$ic.trace,    # criterion at every generation (I-chart)
         coef          = out$coef,
         has.intercept = has.intercept,
         inv.temp      = inv.temp,
         criterion     = criterion,       # the model selection criterion name
         family        = family),
    class = "IBGS")
  obj$convergence <- .ibgs.diag(out$ic.trace)   # coda-style diagnostics of the trace
  obj
}

# Internal helper: convergence diagnostics of the information-criterion trace.
#
# Calls the self-contained C routine (no coda dependency) and tidies the result
# into the list stored as result$convergence.  All diagnostics are computed on
# ic.trace, the criterion value at each generation -- the same sequence the
# I-chart monitors -- treated as a univariate MCMC chain.  The Gelman-Rubin
# diagnostic splits that single chain into n.seg equal contiguous segments
# (split-Rhat).  Fields:
#   gelman   named c(point, upper): Gelman-Rubin potential scale reduction factor
#   geweke   the Geweke z-statistic (first 10% vs last 50% of the chain)
#   ess      the effective sample size
#   autocorr the lag autocorrelations, named by lag (0..lag.max)
#   shrink   data.frame(iter, median, upper): the evolving shrink factor for
#            plotGelman()
#
# Arguments:
#   ic.trace the criterion-at-each-generation vector from the C sampler
#   n.seg    number of contiguous segments for the split-chain Gelman-Rubin
#   lag.max  highest autocorrelation lag to report
#   n.bin    number of breakpoints for the evolving shrink factor
.ibgs.diag <- function(ic.trace, n.seg = 4L, lag.max = 40L, n.bin = 20L) {
  d <- .Call("ibgs_diag", as.double(ic.trace), as.integer(n.seg),
             as.integer(lag.max), as.integer(n.bin), PACKAGE = "IBGS")
  list(gelman   = stats::setNames(d$gelman, c("point", "upper")),
       geweke   = d$geweke,
       ess      = d$ess,
       autocorr = stats::setNames(d$acf, d$acf.lag),
       shrink   = data.frame(iter   = d$shrink$iter,
                             median = d$shrink$median,
                             upper  = d$shrink$upper))
}

# Internal helper: detect near-collinear covariate pairs and stop.
#
# Highly correlated covariates do not crash the IBGS fit -- a numerically
# singular candidate model is simply rejected -- but a pair with |cor| ~ 1 is
# essentially a duplicated column that carries no extra information and makes the
# affected fits unstable.  Rather than silently dropping columns (order-dependent
# and risky), the samplers can fail fast: when `cor.check` is a scalar threshold
# this routine scans for any pair exceeding it and stops, naming the offenders.
#
# The scan is memory-safe: it standardizes the columns once (so a correlation is
# an inner product divided by n-1) and then sweeps the pairwise correlations in
# column chunks via crossprod(), never materialising the full p x p matrix.
#
# Arguments:
#   x      the numeric design matrix (n x p)
#   names  predictor names (length p) used to label offending pairs
#   thresh the correlation threshold (|cor| > thresh triggers the stop)
.check.high.cor <- function(x, names, thresh) {
  n <- nrow(x)
  p <- ncol(x)
  if (n < 3 || p < 2) return(invisible(NULL))   # nothing to compare

  # standardize columns: cor(i, j) = crossprod(z)[i, j] / (n - 1).
  # constant columns become NaN under scale(); zero them so they never match.
  z <- scale(x)
  z[is.na(z)] <- 0
  denom <- n - 1

  chunk    <- 128L
  max.list <- 20L                  # cap how many pairs we name in the message
  pairs    <- character(0)
  ntrip    <- 0L                   # total number of offending pairs found

  for (start in seq.int(1L, p, by = chunk)) {
    idx <- start:min(start + chunk - 1L, p)
    # |idx| x p block of correlations; only keep upper pairs (global j > i)
    cc <- crossprod(z[, idx, drop = FALSE], z) / denom
    hits <- which(abs(cc) > thresh, arr.ind = TRUE)
    if (nrow(hits)) {
      gi <- idx[hits[, 1]]         # global row index i
      gj <- hits[, 2]              # global col index j
      keep <- gj > gi              # each pair once, skip the diagonal
      if (any(keep)) {
        gi <- gi[keep]; gj <- gj[keep]
        ntrip <- ntrip + length(gi)
        room <- max.list - length(pairs)
        if (room > 0) {
          take <- seq_len(min(room, length(gi)))
          pairs <- c(pairs, sprintf("'%s'~'%s'", names[gi[take]], names[gj[take]]))
        }
      }
    }
  }

  if (ntrip > 0) {
    more <- if (ntrip > length(pairs))
      sprintf(" (and %d more)", ntrip - length(pairs)) else ""
    stop(sprintf(paste0("highly correlated covariates (|cor| > %g): %s%s.\n",
                        "  Remove one column from each pair, or set ",
                        "cor.check = NULL to skip the check."),
                 thresh, paste(pairs, collapse = ", "), more),
         call. = FALSE)
  }
  invisible(NULL)
}

# Internal helper: hold ebic.gamma at the extended-BIC consistency floor.
#
# The exBIC penalty 2*gamma*npred*log(p0) is selection-consistent only for
# gamma > 1 - log(n)/(2*log(p)), equivalently gamma > 1 - 1/(2*kappa) when
# p = O(n^kappa) (Chen & Chen, 2012).  Below that floor the criterion is
# inconsistent in exactly the ultrahigh-dimensional regime the samplers target:
# gamma = 0.5, for instance, only qualifies while kappa < 1, i.e. p < n.  The
# prior construction gamma = 1 - xi with xi in [0, 1] also bounds gamma to
# [0, 1], and outside that range the penalty stops penalising -- a negative gamma
# rewards larger models.  This routine clamps the supplied value into
# [max(0, floor), 1] and reports any change.  The other three criteria never
# read gamma, so they are returned untouched.
#
# Arguments:
#   gamma     the supplied ebic.gamma
#   criterion the resolved criterion name; only "exBIC" uses gamma
#   n         the effective sample size the criterion itself penalises with:
#             nrow(x) for the glm and lme samplers, but the event count for the
#             Cox samplers, whose penalty uses log(d) (see coxicval() in cox.c)
#   p         the number of candidate predictors (the pool size p0)
#
# Value: the gamma to hand to the C layer.
.ebic.gamma.floor <- function(gamma, criterion, n, p) {
  if (criterion != "exBIC") return(gamma)             # gamma is unused otherwise
  if (n < 2 || p < 2) return(min(1, max(gamma, 0)))   # kappa undefined at log(1)

  g.min <- 1 - log(n) / (2 * log(p))
  out   <- min(1, max(gamma, g.min, 0))
  if (!isTRUE(all.equal(out, gamma)))
    message(sprintf(paste0("'ebic.gamma' set to %g (from %g): with effective sample ",
                           "size n = %d and p = %d predictors the extended BIC is ",
                           "selection-consistent only for 'ebic.gamma' in [%g, 1], ",
                           "the lower end being max(0, 1 - log(n)/(2*log(p)))."),
                    out, gamma, n, p, max(0, g.min)))
  out
}
