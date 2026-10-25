# Validate the backcast methods on real Kazakhstan revisions.
# Run from the project root: Rscript scripts/kazakhstan.R
#
# Data: the World Bank's WDI Database Archives (API source 57) keep every past WDI release. When
# a later release revised a series' history, the earlier release is the old-method series and
# the later one the new-method series, both published for the same years. National accounts
# come from Kazakhstan's Bureau of National Statistics; population from the UN World Population
# Prospects.
#
# A new release extends its own method back only to its base year; before that it is the old
# series rescaled (the ratio new/old is constant there). So only years from the new base year on
# are a true test, and `hide` starts there.

source("R/backcast.R")

raw <- "data/raw/wdi_kaz_vintages.csv"

cases <- data.frame(
  case = c("industry_rebase_2014", "industry_revision_2025", "gdp_rebase_2014", "population_wpp2024"),
  series = c("NV.IND.TOTL.KN", "NV.IND.TOTL.KN", "NY.GDP.MKTP.KN", "SP.POP.TOTL"),
  old_vintage = c("201312", "202412", "201312", "202312"),
  new_vintage = c("201412", "202512", "201412", "202412"),
  # Year the old release is taken as right in, for interpolation. NA where the two releases are
  # in different units (constant prices of different base years): there new/old is never 1, so
  # the interpolation's premise does not hold.
  old_base = c(NA, 2005, NA, 2009),
  hide_from = c(2005, 2005, 2005, 2009),
  hide_to = c(2008, 2014, 2008, 2015),
  note = c("Industry value added, constant tenge: base year moved from 2000 to 2005",
           "Industry value added, constant tenge: history revised, mostly from 2009 on, same 2005 base",
           "GDP, constant tenge: same rebasing; growth rates were not revised (control)",
           "Population: UN WPP 2024 replaced WPP 2022")
)

fetch_wdi <- function(series, vintage) {
  url <- sprintf("https://api.worldbank.org/v2/sources/57/country/KAZ/series/%s/version/%s?format=json&per_page=500",
                 series, vintage)
  rows <- jsonlite::fromJSON(url, simplifyVector = FALSE)$source$data
  year <- vapply(rows, function(r) {
    v <- r$variable[[which(vapply(r$variable, `[[`, "", "concept") == "Time")]]
    as.integer(v$value)
  }, 1L)
  value <- vapply(rows, function(r) if (is.null(r$value)) NA_real_ else as.numeric(r$value), 1)
  data.frame(series = series, vintage = vintage, year = year, value = value)[!is.na(value), ]
}

if (!file.exists(raw)) {
  pairs <- unique(rbind(setNames(cases[c("series", "old_vintage")], c("series", "vintage")),
                        setNames(cases[c("series", "new_vintage")], c("series", "vintage"))))
  wdi <- do.call(rbind, Map(fetch_wdi, pairs$series, pairs$vintage))
  wdi <- wdi[order(wdi$series, wdi$vintage, wdi$year), ]
  dir.create(dirname(raw), recursive = TRUE, showWarnings = FALSE)
  write.csv(wdi, raw, row.names = FALSE)
}
wdi <- read.csv(raw, colClasses = c(vintage = "character"))

get_series <- function(series, vintage) {
  x <- wdi[wdi$series == series & wdi$vintage == vintage, ]
  setNames(x$value, x$year)
}

results <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  k <- cases[i, ]
  old <- get_series(k$series, k$old_vintage)
  new <- get_series(k$series, k$new_vintage)
  new <- new[as.numeric(names(new)) >= k$hide_from]
  hide <- k$hide_from:k$hide_to
  link <- k$hide_to + 1
  methods <- list(
    ratio = splice_ratio,
    difference = splice_difference
  )
  if (!is.na(k$old_base)) {
    methods$interpolation <- function(old, new) splice_interpolate(old, new, base = k$old_base)
  }
  do.call(rbind, lapply(names(methods), function(m) {
    v <- backcast_validate(old, new, hide, methods[[m]])
    growth <- function(first, last) 100 * ((last / first)^(1 / (link - k$hide_from)) - 1)
    data.frame(case = k$case, method = m, hidden = paste0(k$hide_from, "-", k$hide_to),
               mean_abs_error_pct = mean(abs(v$error_pct)),
               max_abs_error_pct = max(abs(v$error_pct)),
               growth_published_pct = growth(v$published[1], new[[as.character(link)]]),
               growth_backcast_pct = growth(v$backcast[1], new[[as.character(link)]]))
  }))
}))

num <- vapply(results, is.numeric, TRUE)
results[num] <- lapply(results[num], round, 3)
dir.create("results", showWarnings = FALSE)
write.csv(results, "results/kazakhstan.csv", row.names = FALSE)
write.csv(cases, "results/kazakhstan_cases.csv", row.names = FALSE)
print(results, row.names = FALSE)
