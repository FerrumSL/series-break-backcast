# Every test builds the true new-method series, distorts it in a known way to make the old one,
# and checks which methods recover the truth.

years <- 2000:2020
truth <- setNames(100 * 1.03^(years - 2000), years)  # 3% a year
published_new <- truth[as.character(2015:2020)]       # new method from 2015
old_years <- as.character(2000:2016)                   # old method up to 2016

test_that("a constant level gap is recovered exactly by the ratio splice", {
  old <- truth[old_years] * 0.9
  b <- splice_ratio(old, published_new)
  expect_equal(b, truth)
  expect_false(isTRUE(all.equal(splice_difference(old, published_new), truth)))
})

test_that("a constant absolute gap is recovered exactly by the difference splice", {
  old <- truth[old_years] - 10
  expect_equal(splice_difference(old, published_new), truth)
  expect_false(isTRUE(all.equal(splice_ratio(old, published_new), truth)))
})

test_that("a gap that built up since the old base year is recovered by interpolation", {
  # The old series is right in its base year 2005 and drifts 20% low by 2015
  t <- as.numeric(old_years)
  drift <- 1.2^pmin(pmax((t - 2005) / 10, 0), 2)
  old <- truth[old_years] / drift
  b <- splice_interpolate(old, published_new, base = 2005)
  expect_equal(b, truth)
  # The ratio splice carries the 2015 gap back to 2000, so it understates early growth
  g <- backcast_growth(backcast_compare(old, published_new, base = 2005), from = 2000, to = 2015)
  expect_equal(g$growth_pct[g$method == "interpolation"], 3)
  expect_lt(g$growth_pct[g$method == "ratio"], 2)
})

test_that("an indicator measured the same way throughout gives the level back", {
  old <- truth[old_years] * 0.9
  expect_equal(suppressWarnings(splice_indicator(old, published_new, indicator = truth / 2)), truth,
               ignore_attr = TRUE)
  set.seed(1)
  noisy <- truth / 2 * exp(rnorm(length(truth), 0, 0.01))
  b <- suppressWarnings(splice_indicator(old, published_new, indicator = noisy))
  expect_lt(max(abs(b / truth - 1)), 0.05)
  expect_error(splice_indicator(old, published_new, indicator = truth[c("2015", "2016")]), "3 periods")
})

test_that("methods agree, with zero spread, when nothing changed", {
  x <- suppressWarnings(backcast_compare(truth[old_years], published_new, base = 2005, indicator = truth))
  expect_equal(names(x), c("period", "ratio", "difference", "interpolation", "indicator", "spread_pct", "note"))
  expect_equal(x$period, years)
  expect_equal(x$spread_pct, rep(0, length(years)))
  expect_equal(backcast_growth(x)$growth_pct, rep(3, 4))
})

test_that("the spread grows where methods disagree", {
  old <- truth[old_years] * 0.9
  x <- backcast_compare(old, published_new)
  expect_gt(x$spread_pct[x$period == 2000], x$spread_pct[x$period == 2014])
  expect_equal(x$spread_pct[x$period >= 2015], rep(0, 6))  # published, not backcast
})

test_that("validation on a published overlap measures each method's error", {
  # Published both ways for 2015 and 2016: hide 2015 and backcast it from the 2016 link
  old <- truth[old_years] * 0.9
  ratio <- backcast_validate(old, published_new, hide = 2015, method = splice_ratio)
  expect_equal(ratio$error_pct, 0)
  difference <- backcast_validate(old, published_new, hide = 2015, method = splice_difference)
  expect_gt(abs(difference$error_pct), 0)
  expect_error(backcast_validate(old, published_new, hide = 2016, method = splice_ratio), "earliest")
  expect_error(backcast_validate(old, published_new, hide = 2017, method = splice_ratio), "both ways")
})

test_that("bad inputs stop with a clear message", {
  expect_error(splice_ratio(truth[old_years], truth[as.character(2018:2020)]), "share")
  expect_error(splice_ratio(truth[as.character(2015:2016)], published_new), "no periods before")
  expect_error(splice_interpolate(truth[old_years], published_new, base = 2016), "before the link")
})

test_that("integer series of realistic size do not overflow", {
  # Populations are stored as integers; old * new would pass .Machine$integer.max
  old <- setNames(c(15982352L, 16203173L, 16440254L), 2009:2011)
  new <- setNames(c(16763173L, 17000000L), 2010:2011)
  b <- splice_ratio(old, new)
  expect_false(anyNA(b))
  expect_equal(b[["2009"]], 15982352 * 16763173 / 16203173)
})

test_that("an error that grows geometrically from a share rho at base is recovered by mixed splicing", {
  # de la Fuente's model: log error d_t = d_T * rho^((T - t) / T), t from base 2005, link 2015
  rho <- 0.4
  d_link <- log(1.2)
  t <- as.numeric(old_years) - 2005
  old <- truth[old_years] / exp(d_link * rho^((10 - t) / 10))
  expect_equal(splice_mixed(old, published_new, base = 2005, rho = rho), truth)
  expect_false(isTRUE(all.equal(splice_ratio(old, published_new), truth)))
  expect_false(isTRUE(all.equal(splice_interpolate(old, published_new, base = 2005), truth)))
})

test_that("mixed splicing with rho = 1 is the ratio splice", {
  old <- truth[old_years] * seq(0.8, 0.9, length.out = length(old_years))
  expect_equal(splice_mixed(old, published_new, base = 2005, rho = 1),
               splice_ratio(old, published_new))
})

test_that("find_break locates a level jump and finds none in a steady series", {
  set.seed(1)
  steady <- truth * exp(rnorm(length(years), sd = 0.002))
  jumped <- steady * ifelse(years >= 2012, 1.1, 1)
  b <- find_break(jumped, B = 199)
  expect_equal(b$period, 2012)
  expect_equal(b$reason, "ok")
  expect_true(is.na(find_break(steady, B = 199)$period))
})

test_that("find_break does not read smooth curvature plus autocorrelation as a break", {
  # The old sup-F on a linear trend flagged this kind of series in ~97% of runs
  set.seed(2)
  hits <- 0
  for (i in 1:20) {
    e <- as.numeric(arima.sim(list(ar = 0.7), 22, sd = 0.01))
    x <- setNames(exp(cumsum(0.03 - 0.0006 * (1:22)) + e + 4.6), 2000:2021)
    hits <- hits + !is.na(find_break(x, B = 99)$period)
  }
  expect_lte(hits, 3)  # nominal 1 of 20; allow Monte Carlo noise
})

test_that("find_break reports short series and does not touch the caller's random state", {
  expect_equal(find_break(truth[1:8])$reason, "too_short")
  set.seed(7); before <- runif(1); set.seed(7)
  find_break(truth, B = 20)
  expect_equal(runif(1), before)
})

# Hand-worked numbers: answers written as literals, not rebuilt with the code's formula
test_that("splices match numbers worked out by hand", {
  old <- c("2000" = 80, "2001" = 90, "2002" = 100)
  new <- c("2002" = 120, "2003" = 130)
  expect_equal(splice_ratio(old, new), c("2000" = 96, "2001" = 108, "2002" = 120, "2003" = 130))
  expect_equal(splice_difference(old, new), c("2000" = 100, "2001" = 110, "2002" = 120, "2003" = 130))
  # interpolation, base 2000, link 2002: ratio 1.2 grows geometrically, 1 at 2000
  expect_equal(splice_interpolate(old, new, base = 2000)[["2001"]], 90 * sqrt(1.2))
  expect_equal(splice_interpolate(old, new, base = 2000)[["2000"]], 80)
  # mixed, rho = 0.25, span 2: error share at 2001 is rho^(1/2) = 0.5 of the log gap
  expect_equal(splice_mixed(old, new, base = 2000, rho = 0.25)[["2001"]], 90 * 1.2^0.5)
  expect_equal(splice_mixed(old, new, base = 2000, rho = 0.25)[["2000"]], 80 * 1.2^0.25)
})

test_that("a link given as a number works in every splice", {
  old <- c("2000" = 90, "2001" = 93, "2002" = 96)
  new <- c("2002" = 100, "2003" = 103)
  expect_equal(splice_ratio(old, new, link = 2002), splice_ratio(old, new))
  expect_equal(splice_difference(old, new, link = 2002), splice_difference(old, new))
  expect_equal(splice_interpolate(old, new, base = 2000, link = 2002),
               splice_interpolate(old, new, base = 2000))
  expect_equal(splice_mixed(old, new, base = 2000, rho = 0.5, link = 2002),
               splice_mixed(old, new, base = 2000, rho = 0.5))
  expect_false(anyNA(splice_ratio(old, new, link = 2002)))
})

test_that("a two-year link uses sum ratio and mean difference", {
  old <- c("2000" = 90, "2001" = 93, "2002" = 96, "2003" = 99)
  new <- c("2002" = 100, "2003" = 106)
  expect_equal(splice_ratio(old, new, link = c(2002, 2003))[["2000"]], 90 * 206 / 195)
  expect_equal(splice_difference(old, new, link = c(2002, 2003))[["2000"]], 90 + 5.5)
})

test_that("bad series stop with the function and period named", {
  new <- c("2002" = 100, "2003" = 103)
  ok <- c("2000" = 90, "2001" = 93, "2002" = 96)
  expect_error(splice_ratio(replace(ok, 1, NA), new), "splice_ratio.*old.*2000")
  expect_error(splice_mixed(replace(ok, 1, 0), new, base = 2000, rho = 0.5), "positive.*2000")
  expect_error(splice_ratio(ok, c("2002" = -100, "2003" = 103)), "positive.*2002")
  expect_error(splice_ratio(c(ok, "2001" = 94), new), "duplicate period: 2001")
  expect_error(splice_ratio(setNames(ok, c("2000Q1", "2001", "2002")), new), "2000Q1")
  expect_error(splice_ratio(unname(ok), new), "named")
  expect_error(splice_indicator(ok, new, c("2002" = Inf, "2003" = 1, "2004" = 2)), "indicator")
  expect_error(find_break(replace(truth, 5, NA)), "find_break.*2004")
})

test_that("unsorted input gives the same answer as sorted", {
  old <- c("2000" = 90, "2001" = 93, "2002" = 96)
  new <- c("2002" = 100, "2003" = 103)
  expect_equal(splice_ratio(old[c(3, 1, 2)], new[2:1]), splice_ratio(old, new))
  expect_equal(find_break(rev(truth), B = 20)$p_value, find_break(truth, B = 20)$p_value)
})

test_that("compare reports an indicator that covers only part of the span", {
  old <- c("2000" = 90, "2001" = 93, "2002" = 96, "2003" = 99)
  new <- c("2002" = 100, "2003" = 103, "2004" = 107, "2005" = 110)
  ind <- c("2001" = 50, "2002" = 52, "2003" = 54, "2004" = 56)
  x <- backcast_compare(old, new, indicator = ind)
  expect_true(is.na(x$indicator[x$period == 2000]))
  expect_match(x$note[x$period == 2000], "indicator")
  expect_false(is.na(x$spread_pct[x$period == 2000]))
  expect_equal(x$note[x$period == 2002], "")
})

test_that("growth stops on equal or missing periods", {
  x <- backcast_compare(c("2000" = 90, "2001" = 93, "2002" = 96), c("2002" = 100, "2003" = 103))
  expect_error(backcast_growth(x, from = 2002, to = 2002), "before")
  expect_error(backcast_growth(x, from = 1999, to = 2002), "periods in")
})

test_that("the indicator fit is reported and a long extrapolation warns", {
  old <- truth[old_years] * 0.9
  ind <- truth / 2 * exp(c(0.01, -0.01, 0.02, -0.02, 0.01, 0, -0.01, 0.02, 0, -0.02, 0.01,
                           0, 0.01, -0.01, 0.02, 0, -0.01, 0.01, 0, 0.02, -0.01))
  expect_warning(b <- splice_indicator(old, published_new, indicator = ind), "fragile")
  fit <- attr(b, "fit")
  expect_equal(fit$elasticity, 1, tolerance = 0.2)
  expect_equal(fit$n, 6)
  expect_equal(fit$fit_window, c(2015, 2020))
  g <- splice_indicator(old, published_new, indicator = ind, spec = "growth")
  expect_lt(max(abs(g / truth - 1)), 0.25)
  expect_error(splice_indicator(old, published_new, indicator = truth / 2, spec = "growth"), "does not vary")
})

test_that("validation also reports the growth error over a multi-year hidden span", {
  old <- truth[old_years] * 0.9
  v <- backcast_validate(old, published_new, hide = 2015, method = splice_ratio)
  expect_null(attr(v, "growth_error_pp"))
  v2 <- backcast_validate(old, truth[as.character(2014:2020)], hide = c(2014, 2015),
                          method = splice_difference)
  expect_gt(abs(attr(v2, "growth_error_pp")), 0)
  expect_equal(backcast_validate(old, truth[as.character(2014:2020)], hide = c(2014, 2015),
                                 method = splice_ratio)$error_pct, c(0, 0))
})
