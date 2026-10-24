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

check_link <- function(old, new, link) {
  if (length(link) == 0 || !all(link %in% overlap(old, new))) {
    stop("The link must be one or more periods present in both series; they share: ",
         paste(overlap(old, new), collapse = ", "))
  }
  if (length(early(old, new)) == 0) stop("`old` has no periods before `new` starts.")
}


# Ratio splice (retropolation): scale the old series by new/old at the link. Keeps the old
# growth rates and moves the whole level gap to the start of the series.
splice_ratio <- function(old, new, link = overlap(old, new)[1]) {
  check_link(old, new, link)
  e <- early(old, new)
  extend(old[e] * sum(new[link]) / sum(old[link]), new)
}

# Difference splice: shift the old series by new - old at the link. Keeps the old absolute
# changes instead of its growth rates.
splice_difference <- function(old, new, link = overlap(old, new)[1]) {
  check_link(old, new, link)
  e <- early(old, new)
  extend(old[e] + mean(new[link] - old[link]), new)
}

# Interpolation (Prados de la Escosura): the old series is taken as right in its own base
# period `base`, and the gap with the new series is taken to have built up gradually from then
# to the link, as the old weights went out of date. The ratio new/old grows geometrically from
# 1 at `base` to its value at the link; before `base` it stays 1.
splice_interpolate <- function(old, new, base, link = overlap(old, new)[1]) {
  check_link(old, new, link)
  if (length(link) != 1) stop("Interpolation takes a single link period.")
  t0 <- as.numeric(base)
  t1 <- as.numeric(link)
  if (!(t0 < t1)) stop("`base` must come before the link.")
  e <- early(old, new)
  t <- as.numeric(e)
  r <- (new[[link]] / old[[link]])^pmin(pmax((t - t0) / (t1 - t0), 0), 1)
  extend(old[e] * r, new)
}

# Indicator: fit log(new) on log(indicator) over the periods both cover, then predict the new
# series back from the indicator. Ignores the old series, so it does not inherit its errors;
# it needs an indicator measured the same way throughout and at least 3 shared periods.
# ponytail: exp(fitted log) gives the median, a slight underestimate of the mean level when the
# fit is loose; add the lognormal correction if residuals are large.
splice_indicator <- function(old, new, indicator) {
  fit_periods <- intersect(names(new), names(indicator))
  if (length(fit_periods) < 3) stop("The indicator needs at least 3 periods shared with `new`.")
  e <- intersect(early(old, new), names(indicator))
  if (length(e) == 0) stop("The indicator does not cover any period before `new` starts.")
  fit <- stats::lm(log(new[fit_periods]) ~ log(indicator[fit_periods]))
  b <- stats::coef(fit)
  extend(stats::setNames(exp(b[[1]] + b[[2]] * log(indicator[e])), e), new)
}


# Every applicable method side by side, one column each, plus the spread across them
#
# `base` enables the interpolation, `indicator` the indicator method. `spread_pct` is the range
# across methods as a percentage of their mean, per period.
backcast_compare <- function(old, new, base = NULL, indicator = NULL, link = overlap(old, new)[1]) {
  out <- list(ratio = splice_ratio(old, new, link), difference = splice_difference(old, new, link))
  if (!is.null(base)) out$interpolation <- splice_interpolate(old, new, base, link)
  if (!is.null(indicator)) out$indicator <- splice_indicator(old, new, indicator)
  periods <- names(out$ratio)
  x <- data.frame(period = as.numeric(periods),
                  lapply(out, function(s) unname(s[periods])), check.names = FALSE)
  m <- as.matrix(x[names(out)])
  x$spread_pct <- 100 * (apply(m, 1, max) - apply(m, 1, min)) / rowMeans(m)
  x
}

# Average yearly growth (%) from `from` to `to` under each method in a backcast_compare() table
backcast_growth <- function(x, from = min(x$period), to = max(x$period)) {
  methods <- setdiff(names(x), c("period", "spread_pct"))
  a <- x[x$period == from, methods]
  b <- x[x$period == to, methods]
  g <- 100 * ((unlist(b) / unlist(a))^(1 / (to - from)) - 1)
  data.frame(method = methods, growth_pct = unname(g))
}


# Test a method where the office published both series for several periods: hide the new
# series for `hide` (its first periods), backcast them from what is left, and compare with the
# published values. `method` is a function of (old, new) returning a backcast.
backcast_validate <- function(old, new, hide, method) {
  hide <- as.character(hide)
  if (!all(hide %in% overlap(old, new))) stop("`hide` must be periods published both ways.")
  kept <- new[setdiff(names(new), hide)]
  if (max(as.numeric(hide)) >= min(as.numeric(names(kept)))) {
    stop("`hide` must be the earliest periods of the new series.")
  }
  estimate <- method(old, kept)[hide]
  data.frame(period = as.numeric(hide), published = unname(new[hide]),
             backcast = unname(estimate),
             error_pct = unname(100 * (estimate - new[hide]) / new[hide]))
}
