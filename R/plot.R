# Standalone diagnostic plots for an "IBGS" result: the I-chart of the criterion
# sequence, the visit frequency of the top models, and the marginal inclusion
# probability of the top covariates.  (The plot.IBGS S3 method that dispatches to
# these lives in methods.R.)

# I-chart for the generated information-criterion sequence
#
# Plots the model selection criterion at each Gibbs generation together with two
# upper control limits (UCLs): one estimated from the first half of the run
# (the shaded region) and one from the whole run.  Once the trace settles below
# the limits the sampler has stabilised.  The best (lowest) criterion value
# reached is highlighted, and a horizontal guide marks the best retained model's
# criterion.
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
# Value: invisibly, a list with the two control limits and the best value
plotIchart <- function(result, col = NULL, highlight = NULL,
                       running.min = FALSE, legend = TRUE, ...){
  pal <- .ibgs.cols()
  if (is.null(col))       col       <- pal$trace
  if (is.null(highlight)) highlight <- pal$highlight

  v   <- result$ic.trace      # information criterion of each generation
  n   <- length(v)
  h   <- floor(n / 2)         # first/second-half split
  crit <- result$criterion

  # Upper control limit (UCL) for the criterion trace.  It is anchored at the
  # best (lowest) value `lo` reached and reaches up by sqrt(10) times the
  # root-mean-square deviation of the trace from that anchor,
  #     UCL = lo + sqrt(10) * sqrt( var(z) + (mean(z) - lo)^2 ).
  # The bracket is E[(z - lo)^2] (spread about lo, not about the mean), so a
  # trace that has settled near its minimum gives a tight limit; sqrt(10) ~ 3.16
  # plays the role of a "3-sigma" control band.  Computing it on the first half
  # vs. the whole run shows whether the sampler has stabilised.
  ucl <- function(z){
    lo <- min(z)
    lo + sqrt(10) * sqrt(sd(z)^2 + (mean(z) - lo)^2)
  }
  ucl.half <- ucl(v[seq_len(h)])    # limit from the first half only
  ucl.full <- ucl(v)                # limit from the whole run
  i.best   <- which.min(v)          # generation achieving the best criterion
  ic.best  <- if (!is.null(result$model.ic)) min(result$model.ic) else v[i.best]

  # reserve room on the right for the outside legend (restored on exit)
  if (isTRUE(legend)) {
    op <- par(mar = par("mar") + c(0, 0, 0, 6)); on.exit(par(op))
  }

  # default a robust y-range so early-iteration spikes do not flatten the settled
  # region into a flat line; a user-supplied ylim (via ...) still wins
  dots <- list(...)
  if (is.null(dots$ylim))
    dots$ylim <- c(min(v), max(stats::quantile(v, 0.98), ucl.half))
  do.call(plot, c(list(seq_len(n), v, type = "l", col = col,
                       xlab = "Generation", ylab = sprintf("%s value", crit),
                       main = sprintf("I-chart of the %s sequence", crit)),
                  dots))
  # light shaded band over the first half, which the (first-half) UCL is built on
  usr <- par("usr")
  rect(usr[1], usr[3], h, usr[4], col = pal$burnin, border = NA)
  lines(seq_len(n), v, col = col)                      # redraw trace over the band
  abline(h = ucl.half, col = pal$limit1, lty = 3, lwd = 2)
  abline(h = ucl.full, col = pal$limit2, lty = 2, lwd = 2)
  abline(h = ic.best,  col = pal$selected, lty = 1, lwd = 1)  # best model criterion
  if (isTRUE(running.min))
    lines(seq_len(n), cummin(v), col = highlight, lwd = 2)
  points(i.best, v[i.best], pch = 19, col = highlight, cex = 1.3)

  if (isTRUE(legend)) {
    items <- c(sprintf("%s trace", crit), "UCL (first half)", "UCL (whole run)",
               "burn-in (first half)", sprintf("best model = %.2f", ic.best),
               sprintf("best draw = %.2f", v[i.best]))
    cols  <- c(col, pal$limit1, pal$limit2, "grey70", pal$selected, highlight)
    ltys  <- c(1, 3, 2, NA, 1, NA)
    pchs  <- c(NA, NA, NA, 22, NA, 19)
    lwds  <- c(1, 2, 2, NA, 1, NA)
    ptbg  <- c(NA, NA, NA, pal$burnin, NA, NA)
    if (isTRUE(running.min)) {
      items <- c(items, "best so far"); cols <- c(cols, highlight)
      ltys  <- c(ltys, 1); pchs <- c(pchs, NA); lwds <- c(lwds, 2)
      ptbg  <- c(ptbg, NA)
    }
    # placed just outside the right edge of the plotting region (xpd = NA)
    legend(x = usr[2] + 0.02 * diff(usr[1:2]), y = usr[4],
           legend = items, col = cols, lty = ltys, pch = pchs, lwd = lwds,
           pt.bg = ptbg, bty = "n", cex = 0.8, xpd = NA)
  }

  invisible(list(ucl.half = ucl.half, ucl.full = ucl.full,
                 best = v[i.best], best.at = i.best))
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
  cols[1] <- pal$highlight                    # best model stands out
  bp <- barplot(freq, xlab = "Model (ranked by criterion)",
                ylab = "Visit frequency", col = cols, border = NA,
                ylim = c(0, max(freq) * 1.15), ...)

  if (isTRUE(annotate))
    text(bp, freq, labels = formatC(ic, format = "f", digits = 1),
         pos = 3, cex = 0.7, col = pal$trace, xpd = NA)

  if (isTRUE(cumulative)) {
    cum <- cumsum(freq) / sum(result$model.freq)   # share of all recorded visits
    lines(bp, cum * max(freq), col = pal$limit2, lwd = 2)
    points(bp, cum * max(freq), pch = 19, col = pal$limit2, cex = 0.7)
    axis(4, at = pretty(c(0, 1)) * max(freq), labels = pretty(c(0, 1)),
         col.axis = pal$limit2, col = pal$limit2)
    mtext("cumulative frequency", side = 4, line = 2.5, col = pal$limit2,
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
  colors <- ifelse(sel, pal$selected, col)
  xx     <- seq_len(n.vars)

  # reserve room on the right for the colour key (restored on exit)
  op <- par(mar = par("mar") + c(0, 0, 0, 7)); on.exit(par(op))

  plot(xx, v.freq, xlab = "", ylab = "Marginal inclusion probability",
       xaxt = "n", main = "", type = "n", ylim = c(0, 1))
  segments(xx, 0, xx, v.freq, col = colors, lwd = lwd, lend = 1)   # stems
  points(xx, v.freq, pch = 19, col = colors, cex = 0.8)            # heads
  abline(h = result$threshold, col = pal$limit1, lty = 2, lwd = 1.5)  # threshold
  # right-justified just above the line so it never clips the panel edge
  text(n.vars + 0.4, result$threshold,
       labels = sprintf("threshold = %.2g", result$threshold),
       adj = c(1, -0.4), cex = 0.75, col = pal$limit1, xpd = NA)
  # predictor names under the axis; selected ones in the highlight colour
  mtext(v.name, side = side, line = line, at = xx, las = las, cex = cex,
        col = ifelse(sel, pal$selected, pal$trace))
  # colour key just outside the right edge of the plotting region
  usr <- par("usr")
  legend(x = usr[2] + 0.02 * diff(usr[1:2]), y = usr[4],
         legend = c("selected (> threshold)", "not selected"),
         col = c(pal$selected, col), pch = 19, bty = "n", cex = 0.8, xpd = NA)

  invisible(stats::setNames(v.freq, v.name))
}
