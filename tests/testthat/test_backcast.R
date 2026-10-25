# Run from the project root: Rscript -e 'testthat::test_dir("tests/testthat")'
#
# Every test builds the true new-method series, distorts it in a known way to make the old one,
# and checks which methods recover the truth.
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "R/backcast.R"), local = TRUE)

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
  expect_equal(splice_indicator(old, published_new, indicator = truth / 2), truth)
  set.seed(1)
  noisy <- truth / 2 * exp(rnorm(length(truth), 0, 0.01))
  b <- splice_indicator(old, published_new, indicator = noisy)
  expect_lt(max(abs(b / truth - 1)), 0.05)
  expect_error(splice_indicator(old, published_new, indicator = truth[c("2015", "2016")]), "3 periods")
})

test_that("methods agree, with zero spread, when nothing changed", {
  x <- backcast_compare(truth[old_years], published_new, base = 2005, indicator = truth)
  expect_equal(names(x), c("period", "ratio", "difference", "interpolation", "indicator", "spread_pct"))
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
  expect_equal(find_break(jumped)[["period"]], 2012)
  expect_true(is.na(find_break(steady)[["period"]]))
})
