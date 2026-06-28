# Standalone diagnostic plots for an "IBGS" result: the trace of the criterion
# sequence, the visit frequency of the top models, the marginal inclusion
# probability of the top covariates, the Gelman-Rubin shrink factor, and the
# autocorrelation of the criterion trace.  (The plot.IBGS S3 method that
# dispatches to these lives in methods.R.)

# Trace plot of the generated information-criterion sequence
#
# Plots the model selection criterion at each Gibbs generation.  The best
# (lowest) criterion value reached is highlighted and a horizontal guide marks
# the best retained model's criterion, so the minimal IC stands out.
#
# Arguments:
#   result      an "IBGS" result from one of the samplers
#   col         colour of the criterion trace, default from the package
#               palette (grey)
#   highlight   colour marking the best (lowest) value, default orange
#   running.min add a best-so-far (running minimum) trace, default
#               FALSE
#   legend      draw an explanatory legend, default TRUE
#   ...         further graphical parameters passed to plot()
#
# Value: invisibly, a list with the best value and the generation that reached it
plotICtrace <- function(result, col = NULL, highlight = NULL,
                        running.min = FALSE, legend = TRUE, ...){
  pal <- .ibgs.cols()
  if (is.null(col))       col       <- pal$trace
  if (is.null(highlight)) highlight <- "blue"   # best draw + best-so-far line

  v   <- result$ic.trace      # information criterion of each generation
  n   <- length(v)
  crit <- result$criterion

  i.best   <- which.min(v)          # generation achieving the best criterion
  ic.best  <- if (!is.null(result$model.ic)) min(result$model.ic) else v[i.best]

  # default a robust y-range so early-iteration spikes do not flatten the settled
  # region into a flat line; a user-supplied ylim (via ...) still wins
  dots <- list(...)
  if (is.null(dots$ylim))
    dots$ylim <- c(min(v), max(stats::quantile(v, 0.98)))
  do.call(plot, c(list(seq_len(n), v, type = "l", col = col,
                       xlab = "Generation", ylab = sprintf("%s value", crit),
                       main = sprintf("Trace of the %s sequence", crit)),
                  dots))
  abline(h = ic.best, col = "red", lty = 1, lwd = 1)   # best model criterion
  if (isTRUE(running.min))
    lines(seq_len(n), cummin(v), col = highlight, lwd = 2)
  points(i.best, v[i.best], pch = 19, col = highlight, cex = 1.3)

  if (isTRUE(legend)) {
    items <- c(sprintf("%s trace", crit),
               sprintf("best model = %.2f", ic.best),
               sprintf("best draw = %.2f", v[i.best]))
    cols  <- c(col, "red", highlight)
    ltys  <- c(1, 1, NA)
    pchs  <- c(NA, NA, 19)
    lwds  <- c(1, 1, NA)
    if (isTRUE(running.min)) {
      items <- c(items, "best so far"); cols <- c(cols, highlight)
      ltys  <- c(ltys, 1); pchs <- c(pchs, NA); lwds <- c(lwds, 2)
    }
    # top-right corner inside the plotting region, on a white background
    legend("topright", legend = items, col = cols, lty = ltys, pch = pchs,
           lwd = lwds, cex = 0.8, bg = "white")
  }

  invisible(list(best = v[i.best], best.at = i.best))
}

# Plot the visit frequency of the top selected models
#
# Draws a bar for each of the top n.models models, showing how often the
# sampler settled on it: the relative frequency with which the Gibbs chain
# visited that model (result$model.freq).  The bars are ordered best-first
# (the model with the lowest information criterion on the left, highlighted), and
# each bar is annotated with its criterion value.  Tall leading bars indicate a
# search that concentrated sharply on a few models.
#
# Arguments:
#   result   an "IBGS" result from one of the samplers
#   n.models the number of top models to show, default is all retained
#   col      bar colour for the non-best models, default from the palette
#   annotate write each bar's criterion value above it, default TRUE
#   cumulative overlay the cumulative visit frequency, default FALSE
#   ...      further graphical parameters passed to barplot()
#
# Value: invisibly, the vector of plotted relative model frequencies
plotModelFreq <- function(result, n.models = result$n.models, col = NULL,
                          annotate = TRUE, cumulative = FALSE, ...){
  pal <- .ibgs.cols()
  if (is.null(col)) col <- pal$trace

  n.models <- min(n.models, length(result$model.freq))
  freq     <- result$model.freq[seq_len(n.models)]
  ic       <- result$model.ic[seq_len(n.models)]
  names(freq) <- seq_len(n.models)            # model rank (best = 1)

  # reserve room on the right for the cumulative axis and its title (on exit)
  if (isTRUE(cumulative)) {
    op <- par(mar = par("mar") + c(0, 0, 0, 3)); on.exit(par(op))
  }

  cols <- rep(col, n.models)
  cols[1] <- "red"                            # best model stands out
  bp <- barplot(freq, xlab = "Model (ranked by criterion)",
                ylab = "Visit frequency", col = cols, border = NA,
                ylim = c(0, max(freq) * 1.15), ...)

  if (isTRUE(annotate))
    text(bp, freq, labels = formatC(ic, format = "f", digits = 1),
         pos = 3, cex = 0.7, col = pal$trace, xpd = NA)

  if (isTRUE(cumulative)) {
    cum <- cumsum(freq) / sum(result$model.freq)   # share of all recorded visits
    lines(bp, cum * max(freq), col = "blue", lwd = 2)
    points(bp, cum * max(freq), pch = 19, col = "blue", cex = 0.7)
    axis(4, at = pretty(c(0, 1)) * max(freq), labels = pretty(c(0, 1)),
         col.axis = "blue", col = "blue")
    mtext("cumulative frequency", side = 4, line = 2.5, col = "blue",
          cex = 0.8)
  }

  invisible(freq)
}

# Plot the marginal inclusion probability of the top covariates
#
# Draws a lollipop plot of the n.vars covariates with the highest marginal
# inclusion probability, sorted descending.  A dashed line marks the selection
# threshold; covariates above it (the selected predictors) are
# highlighted and the rest are muted, so the selected set stands out.  The
# predictor names are written under the axis.
#
# Arguments:
#   result an "IBGS" result from one of the samplers
#   n.vars the number of top covariates to show, default 20
#   col    colour of the below-threshold stems, default from the palette
#   lwd    stem line width
#   side   margin side for the predictor labels (see mtext())
#   line   margin line for the labels
#   las    label orientation (2 = perpendicular)
#   cex    label character expansion
#
# Value: invisibly, the named vector of plotted inclusion probabilities
plotVarProb <- function(result, n.vars = 20, col = NULL, lwd = 2, side = 1,
                        line = 0.25, las = 2, cex = 1){
  pal <- .ibgs.cols()
  if (is.null(col)) col <- pal$muted

  n.vars  <- min(n.vars, length(result$marginal.prob))
  # rank predictors by marginal inclusion probability and take the top n.vars
  v.order <- order(result$marginal.prob, decreasing = TRUE)[seq_len(n.vars)]
  v.freq  <- result$marginal.prob[v.order]
  v.name  <- result$var.names[v.order]

  sel    <- v.freq > result$threshold              # selected (above threshold)
  colors <- ifelse(sel, "blue", col)
  xx     <- seq_len(n.vars)

  plot(xx, v.freq, xlab = "", ylab = "Marginal inclusion probability",
       xaxt = "n", main = "", type = "n", ylim = c(0, 1))
  segments(xx, 0, xx, v.freq, col = colors, lwd = lwd, lend = 1)   # stems
  points(xx, v.freq, pch = 19, col = colors, cex = 0.8)            # heads
  abline(h = result$threshold, col = "red", lty = 2, lwd = 1.5)    # threshold
  # threshold label on the right, just above the line
  text(n.vars + 0.4, result$threshold,
       labels = sprintf("threshold = %.2g", result$threshold),
       adj = c(1, -0.4), cex = 0.75, col = "red", xpd = NA)
  # predictor names under the axis; selected ones in blue
  mtext(v.name, side = side, line = line, at = xx, las = las, cex = cex,
        col = ifelse(sel, "blue", pal$trace))
  # colour key under the threshold line, right-aligned, on a white background
  legend(x = n.vars + 0.4, y = result$threshold-0.02, xjust = 1, yjust = 1,
         legend = c("selected", "not selected"),
         col = c("blue", col), pch = 19, cex = 0.8, bg = "white",bty = "n", xpd = NA)

  invisible(stats::setNames(v.freq, v.name))
}

# Gelman-Rubin shrink-factor plot for the criterion trace
#
# Plots the evolving Gelman-Rubin potential scale reduction factor (R-hat) of
# the criterion trace as the run lengthens, computed from the single chain split
# into equal contiguous segments (result$convergence$shrink).  The median and
# the 97.5% upper limit are drawn together with a horizontal guide at 1 (perfect
# mixing) and at 1.1 (the usual convergence rule of thumb): once both curves
# settle below ~1.1 the sampler has stabilised.
#
# Arguments:
#   result an "IBGS" result from one of the samplers
#   col    colour of the median R-hat curve, default from the package palette
#   legend draw an explanatory legend, default TRUE
#   ...    further graphical parameters passed to plot()
#
# Value: invisibly, the data.frame of shrink-factor values
plotGelman <- function(result, col = NULL, legend = TRUE, ...){
  pal <- .ibgs.cols()
  if (is.null(col)) col <- "blue"        # median shrink-factor curve

  sh <- result$convergence$shrink
  if (is.null(sh) || nrow(sh) == 0)
    stop("no Gelman shrink-factor data in this fit (chain too short)")

  ylim <- range(1, sh$median, sh$upper, na.rm = TRUE)
  plot(sh$iter, sh$median, type = "l", col = col, lwd = 2, ylim = ylim,
       xlab = "Generation", ylab = "shrink factor",
       main = "Gelman-Rubin shrink factor", ...)
  lines(sh$iter, sh$upper, col = "red", lty = 2, lwd = 2)   # 97.5% upper limit
  abline(h = 1,   col = pal$trace,     lty = 1)
  abline(h = 1.1, col = pal$highlight, lty = 3, lwd = 1.5)
  if (isTRUE(legend))
    legend("topright", legend = c("median", "97.5%", "R-hat = 1.1"),
           col = c(col, "red", pal$highlight), lty = c(1, 2, 3),
           lwd = c(2, 2, 1.5), cex = 0.8, bg = "white", bty = "n")

  invisible(sh)
}

# Autocorrelation plot for the criterion trace
#
# Draws the lagged autocorrelation of the criterion trace
# (result$convergence$autocorr) as a stick plot.  A trace that mixes well decays
# to zero quickly; persistent high autocorrelation signals a slowly-mixing chain
# and a small effective sample size.
#
# Arguments:
#   result an "IBGS" result from one of the samplers
#   col    colour of the sticks, default from the package palette
#   ...    further graphical parameters passed to plot()
#
# Value: invisibly, the named vector of autocorrelations
plotAutocorr <- function(result, col = NULL, ...){
  pal <- .ibgs.cols()
  if (is.null(col)) col <- pal$trace

  ac <- result$convergence$autocorr
  if (is.null(ac))
    stop("no autocorrelation data in this fit")
  lags <- as.integer(names(ac))

  plot(lags, ac, type = "h", col = col, lwd = 2,
       xlab = "Lag", ylab = "Autocorrelation",
       main = sprintf("Autocorrelation of the %s trace", result$criterion),
       ylim = range(0, ac, na.rm = TRUE), ...)
  abline(h = 0, col = pal$trace)

  invisible(ac)
}
