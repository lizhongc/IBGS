# S3 methods, shared plotting palette, and internal helpers for the "IBGS"
# result object.
#
# Every sampler (glmIBGS / glmGibbs / coxIBGS / coxGibbs / rlmIBGS / rlmGibbs)
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
                 vars = vtab, models = mtab),
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
  invisible(x)
}

# Diagnostic plots for an IBGS fit
#
# Dispatches to the diagnostic plots: the I-chart of the criterion trace
# (plotIchart()), the marginal inclusion probabilities
# (plotVarProb()), and the top-model visit frequencies
# (plotModelFreq()).  Several may be requested at once, in which case
# they are drawn in a multi-panel layout.
#
# Arguments:
#   x     an "IBGS" result
#   which one or more of "ichart", "varprob",
#         "modelfreq"; default is all three
#   ...   further graphical parameters, forwarded only when a single panel
#         is requested
# Value: x, invisibly
plot.IBGS <- function(x, which = c("ichart", "varprob", "modelfreq"), ...) {
  which <- match.arg(which, c("ichart", "varprob", "modelfreq"),
                     several.ok = TRUE)
  draw  <- list(ichart    = plotIchart,
                varprob   = plotVarProb,
                modelfreq = plotModelFreq)
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
# glm_coef/cox_coef/rlm_coef .Call wrappers -- is gone.  The samplers attach
# marginal.prob, selected.vars, threshold, var.names (and, for rlm, the
# random-effect block) and the averaged training linear.predictors around this.
.ibgs.object <- function(out, inv.temp, criterion, family, has.intercept) {
  structure(
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
