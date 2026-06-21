# Internal helpers for the held random part of the linear mixed model.
#
# Fixed-effect selection holds the random structure, so the marginal model is
# y ~ N(X beta, V) with V fixed.  Whitening by V = L L' (here we use the
# symmetric root W = V^{-1/2}) turns each candidate fit into ordinary least
# squares on the whitened data, which the C sampler then performs.  These helpers
# (a) estimate the variance ratio once by REML (random-intercept case),
# (b) whiten y / X / the intercept, and (c) form the random-effect BLUPs used for
# conditional prediction.  Base R only -- no lme4/nlme dependency.

# Whiten the columns of M by the random-intercept root W = V0^{-1/2}, where
# V0 = I + lambda Z Z' is block-diagonal by group.  Within a group of size n_g,
# W = I + c_g J with c_g = (-1 + 1/sqrt(1 + lambda n_g)) / n_g (the symmetric
# square root of V0^{-1}), so W M adds c_g * colSums(M_group) to each group row.
# Never forms an n x n matrix.
.ri.whiten <- function(M, lambda, gidx, gsz) {
  M  <- as.matrix(M)
  cg <- (-1 + 1 / sqrt(1 + lambda * gsz)) / gsz
  out <- M
  for (g in seq_along(gsz)) {
    rows <- gidx[[g]]
    s <- colSums(M[rows, , drop = FALSE])
    out[rows, ] <- M[rows, , drop = FALSE] +
      matrix(cg[g] * s, nrow = length(rows), ncol = length(s), byrow = TRUE)
  }
  out
}

# REML estimate of the variance ratio lambda = sigma_u^2 / sigma_e^2 for a
# random-intercept model, from the reference fixed design D (intercept + the
# fixed predictors when estimable, else intercept only).  Profiles beta and
# sigma_e^2; minimises -2*REML(lambda) over log(lambda) with the closed-form
# whitening above.  Returns the estimated lambda and residual variance.
.rlm.reml <- function(y, D, gidx, gsz, n) {
  pD  <- ncol(D)
  obj <- function(loglam) {
    lam <- exp(loglam)
    ys  <- .ri.whiten(y, lam, gidx, gsz)
    Ds  <- .ri.whiten(D, lam, gidx, gsz)
    qrD <- qr(Ds)
    if (qrD$rank < pD) return(1e10)
    rss <- sum(qr.resid(qrD, ys)^2)
    if (rss <= 0) return(1e10)
    logdetV0  <- sum(log(1 + lam * gsz))
    logdetXtX <- 2 * sum(log(abs(diag(qr.R(qrD)))))
    (n - pD) * log(rss) + logdetV0 + logdetXtX     # -2*REML up to constants
  }
  op  <- optimize(obj, interval = c(log(1e-4), log(1e4)))
  lam <- exp(op$minimum)
  res <- qr.resid(qr(.ri.whiten(D, lam, gidx, gsz)), .ri.whiten(y, lam, gidx, gsz))
  list(lambda = lam, sigma2e = sum(res^2) / (n - pD))
}

# Assemble the held random structure and whiten (y, X, intercept).
# Exactly one of `group`, (`Z`+`varcomp`), `V` specifies the random part.
# Returns ystar/xstar/istar (whitened), logdetV0 (= log|V0|), and an `re` list
# carrying what conditional prediction needs.
.rlm.whiten <- function(y, x, group = NULL, Z = NULL, varcomp = NULL, V = NULL) {
  n <- length(y)
  x <- as.matrix(x)

  if (!is.null(group)) {
    group <- as.factor(group)
    lv    <- levels(group)
    gidx  <- lapply(lv, function(l) which(group == l))
    gsz   <- lengths(gidx)
    # estimate lambda once: from the full model if it is estimable (p < n),
    # otherwise from the intercept-only (null) model.
    Dref  <- if (ncol(x) <= n - 2L) cbind(1, x) else matrix(1, n, 1)
    est   <- .rlm.reml(y, Dref, gidx, gsz, n)
    lam   <- est$lambda
    ystar <- .ri.whiten(y, lam, gidx, gsz)[, 1]
    xstar <- .ri.whiten(x, lam, gidx, gsz)
    istar <- .ri.whiten(matrix(1, n, 1), lam, gidx, gsz)[, 1]
    logdetV0 <- sum(log(1 + lam * gsz))
    re <- list(type = "group", levels = lv, gidx = gidx, gsz = gsz,
               lambda = lam, sigma2e = est$sigma2e)
  } else {
    if (is.null(V)) {
      if (is.null(Z) || is.null(varcomp))
        stop("specify the random part via 'group', or 'Z' + 'varcomp', or 'V'")
      Z   <- as.matrix(Z)
      s2b <- varcomp$sigma2b; s2e <- varcomp$sigma2e
      if (is.null(s2b) || is.null(s2e))
        stop("'varcomp' must be a list with 'sigma2b' and 'sigma2e'")
      V <- s2e * diag(n) + s2b * tcrossprod(Z)
    }
    U <- chol(V)                                   # V = U'U (U upper)
    ystar <- backsolve(U, y,          transpose = TRUE)
    xstar <- backsolve(U, x,          transpose = TRUE)
    istar <- backsolve(U, rep(1, n),  transpose = TRUE)
    logdetV0 <- 2 * sum(log(diag(U)))
    # `Z` is set only on the Z+varcomp path; a directly supplied `V` has no
    # random-effects design, so it whitens (selection) but yields no BLUPs.
    re <- if (is.null(Z)) list(type = "marginal", U = U)
          else            list(type = "general", Z = Z, U = U, varcomp = varcomp)
  }

  list(ystar = ystar, xstar = as.matrix(xstar), istar = as.numeric(istar),
       logdetV0 = logdetV0, re = re)
}

# Assemble the rlm result from the compact C sampler output `out`, the original
# (y, x), and the whitening `wh`.  The C call already returns the original-space
# coefficient summary (whitening leaves the GLS fixed effects unchanged); this
# adds the per-top-model random-effect BLUPs from the original-space residuals,
# the marginal-probability fields, and the averaged training linear predictors.
.rlm.result <- function(out, y, x, var.names, wh, inv.temp, info, threshold) {
  result <- .ibgs.object(out, inv.temp, info, "rlm", TRUE)

  # per-top-model BLUPs from original-space residuals r = y - cbind(1, x) %*% beta.
  # A directly-supplied marginal covariance V ("marginal") has no random-effects
  # design, so no BLUPs are available and `re` is left unset.
  if (wh$re$type != "marginal") {
    nm   <- result$n.models
    nre  <- if (wh$re$type == "group") length(wh$re$levels) else ncol(wh$re$Z)
    B.re <- matrix(0, nrow = nre, ncol = nm)
    Xd   <- cbind(1, x)
    for (m in seq_len(nm)) {
      r <- as.numeric(y - Xd %*% result$coef[, m])
      B.re[, m] <- .rlm.blup(r, wh$re)
    }
    result$re <- list(blup = B.re, type = wh$re$type, levels = wh$re$levels,
                      lambda = wh$re$lambda, sigma2e = wh$re$sigma2e)
  }

  result$marginal.prob     <- out$v.prob
  result$selected.vars     <- var.names[out$v.prob > threshold]
  result$threshold         <- threshold
  result$var.names         <- var.names
  result$linear.predictors <- .ibgs.lp(result, x)
  result
}

# Random-effect BLUPs b_hat = G Z' V^{-1} r from the ORIGINAL-space residual
# r = y - X beta_hat.  Random-intercept: b_i = (n_i lambda /(1 + n_i lambda)) *
# mean(r in group i).  General: b = sigma2b * Z' V^{-1} r.
.rlm.blup <- function(r, re) {
  if (re$type == "group") {
    vapply(seq_along(re$gsz), function(g) {
      ng <- re$gsz[g]
      (ng * re$lambda / (1 + ng * re$lambda)) * mean(r[re$gidx[[g]]])
    }, numeric(1))
  } else {
    Vinv_r <- backsolve(re$U, backsolve(re$U, r, transpose = TRUE))
    as.numeric(re$varcomp$sigma2b * crossprod(re$Z, Vinv_r))
  }
}
