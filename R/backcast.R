# Extend a statistics series back across a methodology break.
#
# When an office changes how it measures something (new base year, new classification, survey
# redesign), it publishes the new-method series from some period on, and the old-method series
# up to some period. Each method below fills the new-method series back over the periods where
# only the old one exists. They differ in where they put the gap between the two.
#
# `old` and `new` are named numeric vectors, names = periods (years), all values positive. They
# must overlap in at least one period, the link. Every method returns the new series extended
# back to the first old period, as a named vector in period order.


# Periods measured both ways, and periods before the new series starts
overlap <- function(old, new) intersect(names(old), names(new))
early <- function(old, new) names(old)[as.numeric(names(old)) < min(as.numeric(names(new)))]

extend <- function(backcast, new) {
  x <- c(backcast, new)
  x[order(as.numeric(names(x)))]
}

# One check for every entry point: numeric, named by numeric periods, no duplicates, no NA or
# non-finite values, all positive. Returns the series as doubles, sorted by period, with names
# in canonical form ("2002").
check_series <- function(x, what = "series", fn = "backcast") {
  bad <- function(msg) stop(fn, ": `", what, "` ", msg, call. = FALSE)
  if (!is.numeric(x)) bad("must be a numeric vector.")
  if (is.null(names(x))) bad("must be named by period (years).")
  p <- suppressWarnings(as.numeric(names(x)))
  if (anyNA(p)) bad(paste0("has a name that is not a numeric period: \"", names(x)[which(is.na(p))[1]], "\"."))
  if (anyDuplicated(p)) bad(paste0("has a duplicate period: ", p[anyDuplicated(p)], "."))
  if (any(!is.finite(x))) bad(paste0("has a missing or non-finite value in period ", p[which(!is.finite(x))[1]], "."))
  if (any(x <= 0)) bad(paste0("must be all positive; first non-positive value is in period ", p[which(x <= 0)[1]], "."))
  x <- stats::setNames(as.numeric(x), as.character(p))
  x[order(p)]
}

# Turn a link given as numbers or strings into the period names used in the series
as_periods <- function(p) as.character(as.numeric(p))

check_link <- function(old, new, link) {
  shared <- overlap(old, new)
  if (is.null(link)) link <- shared[which.min(as.numeric(shared))]
  link <- as_periods(link)
  if (length(link) == 0 || !all(link %in% shared)) {
    stop("The link must be one or more periods present in both series; they share: ",
         paste(shared, collapse = ", "), call. = FALSE)
  }
  if (length(early(old, new)) == 0) stop("`old` has no periods before `new` starts.", call. = FALSE)
  link
}

# Validate everything the splice functions share
prep <- function(old, new, link, fn) {
  old <- check_series(old, "old", fn)
  new <- check_series(new, "new", fn)
  list(old = old, new = new, link = check_link(old, new, link))
}

check_base <- function(base, link, fn) {
  if (length(base) != 1 || !is.finite(suppressWarnings(as.numeric(base)))) {
    stop(fn, ": `base` must be one period.", call. = FALSE)
  }
  if (!(as.numeric(base) < as.numeric(link))) stop("`base` must come before the link.", call. = FALSE)
  as.numeric(base)
}


#' Ratio splice (retropolation)
#'
#' Scales the old series by new/old at the link. Keeps the old growth rates and moves the whole
#' level gap to the start of the series.
#'
#' @param old,new Named numeric vectors, names = periods (years), all values positive, sharing
#'   at least one period.
#' @param link Period(s) where the two series are linked, as numbers or strings; the first
#'   shared period by default (`NULL`). With several periods the ratio splice uses
#'   sum(new) / sum(old) over them, the difference splice mean(new - old).
#' @return The new series extended back to the first old period, a named vector in period order.
#' @examples
#' old <- c("2000" = 90, "2001" = 93, "2002" = 96)
#' new <- c("2002" = 100, "2003" = 103)
#' splice_ratio(old, new)
#' @export
splice_ratio <- function(old, new, link = NULL) {
  p <- prep(old, new, link, "splice_ratio"); old <- p$old; new <- p$new; link <- p$link
  e <- early(old, new)
  extend(old[e] * (sum(new[link]) / sum(old[link])), new)
}

#' Difference splice
#'
#' Shifts the old series by new - old at the link. Keeps the old absolute changes instead of
#' its growth rates.
#'
#' @inheritParams splice_ratio
#' @inherit splice_ratio return
#' @export
splice_difference <- function(old, new, link = NULL) {
  p <- prep(old, new, link, "splice_difference"); old <- p$old; new <- p$new; link <- p$link
  e <- early(old, new)
  extend(old[e] + mean(new[link] - old[link]), new)
}

#' Interpolation (Prados de la Escosura)
#'
#' The old series is taken as right in its own base period `base`, and the gap with the new
#' series to have built up gradually from then to the link, as the old weights went out of
#' date. The ratio new/old grows geometrically from 1 at `base` to its value at the link; before
#' `base` it stays 1.
#'
#' @inheritParams splice_ratio
#' @param base Period in which the old series is taken to be right, such as its base year or a
#'   census year.
#' @param link A single period where the two series are linked.
#' @inherit splice_ratio return
#' @references Prados de la Escosura, L. (2016). Mismeasuring long-run growth: the bias from
#'   splicing national accounts - the case of Spain. *Cliometrica* 10(3), 251-275.
#' @export
splice_interpolate <- function(old, new, base, link = NULL) {
  p <- prep(old, new, link, "splice_interpolate"); old <- p$old; new <- p$new; link <- p$link
  if (length(link) != 1) stop("Interpolation takes a single link period.")
  t0 <- check_base(base, link, "splice_interpolate")
  t1 <- as.numeric(link)
  e <- early(old, new)
  t <- as.numeric(e)
  r <- (new[[link]] / old[[link]])^pmin(pmax((t - t0) / (t1 - t0), 0), 1)
  extend(old[e] * r, new)
}

#' Mixed splicing (de la Fuente)
#'
#' Between the ratio splice and interpolation. The old series is taken to be wrong already in
#' `base`, by a share `rho` of the gap at the link, and the error to grow geometrically from
#' there, as it would if it came from activities the old series missed that grow faster than the
#' rest. In logs the error is `d_T * rho^((T - t) / T)`, with `t` counted from `base`; `rho = 1`
#' is the ratio splice, and `rho` near 0 approaches interpolation. de la Fuente leaves `rho` to
#' judgement.
#'
#' @inheritParams splice_interpolate
#' @param base Period from which the error is taken to grow.
#' @param rho Share of the gap at the link already present in `base`, in (0, 1].
#' @inherit splice_ratio return
#' @references de la Fuente, A. (2013). A mixed splicing procedure for economic time series.
#'   BBVA Research Working Paper 13/02.
#' @export
splice_mixed <- function(old, new, base, rho, link = NULL) {
  p <- prep(old, new, link, "splice_mixed"); old <- p$old; new <- p$new; link <- p$link
  if (length(link) != 1) stop("Mixed splicing takes a single link period.")
  if (length(rho) != 1 || !is.finite(rho) || !(rho > 0 && rho <= 1)) stop("`rho` must be in (0, 1].")
  span <- as.numeric(link) - check_base(base, link, "splice_mixed")
  e <- early(old, new)
  d_link <- log(new[[link]] / old[[link]])
  t <- as.numeric(e) - as.numeric(base)
  extend(old[e] * exp(d_link * rho^((span - t) / span)), new)
}

#' Indicator splice
#'
#' Fits the new series on a related indicator over the periods both cover, then predicts the new
#' series back from the indicator. Ignores the old series, so it does not inherit its errors; it
#' needs an indicator measured the same way throughout and at least 3 shared periods.
#'
#' `spec = "levels"` regresses log(new) on log(indicator). On two trending series this can be a
#' spurious regression and it is extrapolated outside the fit window, so treat it as fragile: the
#' fit (elasticity, R2, residual SE, n, window) is returned as `attr(, "fit")`, and a warning is
#' given when the backcast spans more periods than the fit window or the elasticity is outside
#' between 0 and 3. `spec = "growth"` regresses yearly log changes (needs consecutive annual periods) and
#' rebuilds the level backwards from the first new period; it does not depend on a common trend.
#' The result is the median path (exp of the fitted log), not the mean.
#'
#' @inheritParams splice_ratio
#' @param indicator Named numeric vector of a related series measured the same way throughout.
#' @param spec `"levels"` (default) or `"growth"`.
#' @return The new series extended back over the old periods the indicator covers, with the fit
#'   in `attr(, "fit")`.
#' @export
splice_indicator <- function(old, new, indicator, spec = c("levels", "growth")) {
  spec <- match.arg(spec)
  old <- check_series(old, "old", "splice_indicator")
  new <- check_series(new, "new", "splice_indicator")
  indicator <- check_series(indicator, "indicator", "splice_indicator")
  fit_periods <- intersect(names(new), names(indicator))
  fit_periods <- fit_periods[order(as.numeric(fit_periods))]
  if (length(fit_periods) < 3) stop("The indicator needs at least 3 periods shared with `new`.")
  e <- intersect(early(old, new), names(indicator))
  if (length(e) == 0) stop("The indicator does not cover any period before `new` starts.")
  e <- e[order(as.numeric(e))]
  if (spec == "levels") {
    fit <- stats::lm(log(new[fit_periods]) ~ log(indicator[fit_periods]))
    b <- stats::coef(fit)
    est <- exp(b[[1]] + b[[2]] * log(indicator[e]))
    slope <- b[[2]]
  } else {
    pn <- as.numeric(fit_periods)
    if (any(diff(pn) != 1)) stop("spec = \"growth\" needs consecutive annual periods in the fit window.")
    # backward from the first new period: all old periods up to it must be covered, no gaps
    first <- fit_periods[1]
    chain <- as.character(seq(as.numeric(first), min(as.numeric(e)), by = -1))
    if (!all(chain %in% names(indicator)) || !all(e %in% chain)) {
      stop("spec = \"growth\" needs the indicator to cover every period from the first new period back.")
    }
    fit <- stats::lm(diff(log(new[fit_periods])) ~ diff(log(indicator[fit_periods])))
    b <- stats::coef(fit)
    if (anyNA(b)) stop("spec = \"growth\": growth of the indicator does not vary over the fit window.")
    chain <- chain[order(as.numeric(chain))]
    d <- diff(log(indicator[chain]))  # change into period chain[k+1]
    step <- b[[1]] + b[[2]] * d
    lvl <- log(new[[first]]) - rev(cumsum(rev(step)))
    est <- exp(stats::setNames(lvl, chain[-length(chain)]))[e]
    slope <- b[[2]]
  }
  sm <- summary(fit)
  info <- list(spec = spec, elasticity = slope, r2 = sm$r.squared, resid_se = sm$sigma,
               n = length(stats::residuals(fit)), fit_window = range(as.numeric(fit_periods)),
               backcast_periods = length(e),
               lognormal_correction_pct = 100 * (exp(sm$sigma^2 / 2) - 1))
  if (spec == "levels" && (info$backcast_periods > length(fit_periods) || slope < 0 || slope > 3)) {
    warning("splice_indicator: the backcast covers ", length(e), " periods from a fit on ",
            length(fit_periods), " (elasticity ", round(slope, 2), "). Levels regression on short ",
            "trending series is fragile; compare spec = \"growth\".", call. = FALSE)
  }
  out <- extend(stats::setNames(as.numeric(est), e), new)
  attr(out, "fit") <- info
  out
}


#' Find a break in a series
#'
#' Does a spliced series change level or trend somewhere, and where? Fits log(x) on a quadratic
#' time trend (so smooth slowing or speeding growth is not read as a break), takes the sup-F
#' statistic for one break in that model (Andrews), and gets its p-value from a parametric
#' bootstrap under the no-break null with AR(1) errors fitted to the residuals (the asymptotic
#' critical values reject far too often on short autocorrelated annual series). If the test
#' rejects at `level`, the break is dated by least squares (Bai-Perron, via strucchange). Use it
#' to check a documented break date against the data, not as proof of a cause. One break only.
#'
#' `level` is nominal: the actual false-break rate on short series is in `scripts/simulation.R`
#' and the README. The bootstrap uses `seed` and restores the caller's random state.
#'
#' @param x Named numeric vector, names = periods, all values positive.
#' @param h Minimum number of periods in each segment (at least 4, the model has 3 coefficients).
#' @param level Nominal significance level of the test.
#' @param B Number of bootstrap replications.
#' @param seed Seed for the bootstrap.
#' @return A list: `period`, the first period of the new segment (NA if none); `p_value`;
#'   `reason`, one of `"ok"` (a break was found), `"no_break"`, `"too_short"`, `"failed"`.
#' @references Andrews, D. W. K. (1993). Tests for parameter instability and structural change
#'   with unknown change point. *Econometrica* 61(4), 821-856. Bai, J. and Perron, P. (2003).
#'   Computation and analysis of multiple structural change models. *Journal of Applied
#'   Econometrics* 18(1), 1-22.
#' @export
# ponytail: one break only; BIC over several breaks overfits on short annual series.
find_break <- function(x, h = 4, level = 0.05, B = 499, seed = 1) {
  x <- check_series(x, "x", "find_break")
  if (h < 4) stop("find_break: `h` must be at least 4.", call. = FALSE)
  n <- length(x)
  out <- function(period, p, reason) list(period = period, p_value = p, reason = reason)
  if (n < 3 * h) return(out(NA_real_, NA_real_, "too_short"))
  t <- as.numeric(names(x)); t <- t - mean(t)
  y <- log(x)
  supf <- function(y) max(strucchange::Fstats(y ~ t + I(t^2), from = h)$Fstats)
  stat <- tryCatch(supf(y), error = function(e) NA_real_)
  if (!is.finite(stat)) return(out(NA_real_, NA_real_, "failed"))
  null <- stats::lm(y ~ t + I(t^2))
  res <- stats::residuals(null)
  phi <- sum(res[-1] * res[-n]) / sum(res^2)
  phi <- max(min(phi + (1 + 3 * phi) / n, 0.95), -0.95)  # Kendall's small-sample bias correction
  sd_e <- sqrt(sum(res^2) / (n - 3) * (1 - phi^2))
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  fitted <- stats::fitted(null)
  boot <- vapply(seq_len(B), function(b) {
    e <- numeric(n)
    e[1] <- stats::rnorm(1, 0, sd_e / sqrt(1 - phi^2))
    z <- stats::rnorm(n - 1, 0, sd_e)
    for (i in 2:n) e[i] <- phi * e[i - 1] + z[i - 1]
    tryCatch(supf(fitted + e), error = function(err) NA_real_)
  }, numeric(1))
  boot <- boot[is.finite(boot)]
  if (length(boot) < B / 2) return(out(NA_real_, NA_real_, "failed"))
  p <- (1 + sum(boot >= stat)) / (length(boot) + 1)
  if (p >= level) return(out(NA_real_, p, "no_break"))
  at <- tryCatch(t[strucchange::breakpoints(y ~ t + I(t^2), h = h, breaks = 1)$breakpoints + 1],
                 error = function(e) NA_real_)
  if (is.na(at)) return(out(NA_real_, p, "failed"))
  out(at + mean(as.numeric(names(x))), p, "ok")
}


#' Compare backcast methods side by side
#'
#' Runs every method that applies: `base` enables interpolation, `base` with `rho` the mixed
#' splice, `indicator` the indicator method.
#'
#' @inheritParams splice_mixed
#' @param base,rho,indicator Optional; see [splice_interpolate()], [splice_mixed()],
#'   [splice_indicator()].
#' @return A data frame with `period`, one column per method, `spread_pct`, the range across the
#'   methods available in that period as a percentage of their mean (a range of assumptions, not a
#'   confidence interval), and `note`, which names methods missing in a period (an indicator that
#'   does not reach back that far).
#' @examples
#' old <- c("2000" = 90, "2001" = 93, "2002" = 96)
#' new <- c("2002" = 100, "2003" = 103)
#' backcast_compare(old, new, base = 2000)
#' @export
backcast_compare <- function(old, new, base = NULL, indicator = NULL, rho = NULL, link = NULL) {
  out <- list(ratio = splice_ratio(old, new, link), difference = splice_difference(old, new, link))
  if (!is.null(base)) out$interpolation <- splice_interpolate(old, new, base, link)
  if (!is.null(base) && !is.null(rho)) out$mixed <- splice_mixed(old, new, base, rho, link)
  if (!is.null(indicator)) out$indicator <- splice_indicator(old, new, indicator)
  periods <- names(out$ratio)
  x <- data.frame(period = as.numeric(periods),
                  lapply(out, function(s) as.numeric(s[periods])), check.names = FALSE)
  m <- as.matrix(x[names(out)])
  rng <- function(f) apply(m, 1, function(r) f(r, na.rm = TRUE))
  x$spread_pct <- 100 * (rng(max) - rng(min)) / rowMeans(m, na.rm = TRUE)
  x$note <- apply(m, 1, function(r) {
    if (anyNA(r)) paste("no value from:", paste(colnames(m)[is.na(r)], collapse = ", ")) else ""
  })
  x
}

#' Average yearly growth under each method
#'
#' @param x A [backcast_compare()] table.
#' @param from,to Periods to measure growth between (different, both in the table).
#' @return A data frame with `method` and `growth_pct`, the average yearly growth in percent
#'   (NA for a method with no value in `from` or `to`).
#' @export
backcast_growth <- function(x, from = min(x$period), to = max(x$period)) {
  if (!(from %in% x$period && to %in% x$period)) stop("`from` and `to` must be periods in `x`.")
  if (!(from < to)) stop("`from` must be before `to`.")
  methods <- setdiff(names(x), c("period", "spread_pct", "note"))
  a <- x[x$period == from, methods]
  b <- x[x$period == to, methods]
  g <- 100 * ((unlist(b) / unlist(a))^(1 / (to - from)) - 1)
  data.frame(method = methods, growth_pct = unname(g))
}


#' Measure a method's error where both series were published
#'
#' Hides the new series for `hide` (its first periods), backcasts them from what is left, and
#' compares with the published values. Unit tests with exact recovery check consistency; this is
#' the accuracy check on real revisions.
#'
#' @inheritParams splice_ratio
#' @param hide The earliest periods of the new series, all published in both series.
#' @param method A function of (old, new) returning a backcast, such as [splice_ratio].
#' @return A data frame with `period`, `published`, `backcast` and `error_pct` (signed:
#'   positive = backcast too high). With two or more hidden periods, `attr(, "growth_error_pp")`
#'   is the average yearly growth over the hidden span in the backcast minus the published one,
#'   in percentage points.
#' @export
backcast_validate <- function(old, new, hide, method) {
  old <- check_series(old, "old", "backcast_validate")
  new <- check_series(new, "new", "backcast_validate")
  hide <- as_periods(hide)
  if (!all(hide %in% overlap(old, new))) stop("`hide` must be periods published both ways.")
  kept <- new[setdiff(names(new), hide)]
  if (max(as.numeric(hide)) >= min(as.numeric(names(kept)))) {
    stop("`hide` must be the earliest periods of the new series.")
  }
  hide <- hide[order(as.numeric(hide))]
  estimate <- method(old, kept)[hide]
  out <- data.frame(period = as.numeric(hide), published = unname(new[hide]),
                    backcast = unname(estimate),
                    error_pct = unname(100 * (estimate - new[hide]) / new[hide]))
  if (length(hide) >= 2) {
    k <- length(hide) - 1
    gr <- function(v) 100 * ((v[length(v)] / v[1])^(1 / k) - 1)
    attr(out, "growth_error_pp") <- unname(gr(out$backcast) - gr(out$published))
  }
  out
}
