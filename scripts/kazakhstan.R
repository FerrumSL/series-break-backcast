# Validate the backcast methods on real Kazakhstan revisions, then backcast industry to 1995.
# Run from the project root: Rscript scripts/kazakhstan.R
#
# Data: the World Bank's WDI Database Archives (API source 57) keep every past WDI release. When
# a later release revised a series' history, the earlier release is the old-method series and
# the later one the new-method series, both published for the same years. National accounts
# come from Kazakhstan's Bureau of National Statistics; population estimates draw on the UN
# World Population Prospects and national sources.
#
# A new release extends its own method back only to its reference year; before that it is the
# old series rescaled (the ratio new/old is constant there). So only years from the reference
# year on are a true test, and `hide` starts there.

source("R/backcast.R")

raw <- "data/raw/wdi_kaz_vintages.csv"

cases <- data.frame(
  case = c("industry_rebase_2014", "industry_revision_2025", "gdp_rebase_2014", "population_2024"),
  series = c("NV.IND.TOTL.KN", "NV.IND.TOTL.KN", "NY.GDP.MKTP.KN", "SP.POP.TOTL"),
  old_vintage = c("201312", "202412", "201312", "202312"),
  new_vintage = c("201412", "202512", "201412", "202412"),
  # Year the old release is taken as right in, for interpolation. NA where the two releases are
  # in constant prices of different reference years: there new/old is never 1, so the
  # interpolation's premise does not hold.
  old_base = c(NA, 2005, NA, 2009),
  # Whether to run the indicator method (see `indicators` below)
  indicator = c(TRUE, FALSE, TRUE, FALSE),
  hide_from = c(2005, 2005, 2005, 2009),
  hide_to = c(2008, 2014, 2008, 2015),
  note = c("Industry value added, constant tenge: reference year moved from 2000 to 2005",
           "Industry value added, constant tenge: history revised, mostly from 2009 on, same 2005 reference year",
           "GDP, constant tenge: same change of reference year; growth rates were not revised (control)",
           "Population: every year back to 1995 revised in the December 2024 release")
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

needed <- unique(rbind(
  setNames(cases[c("series", "old_vintage")], c("series", "vintage")),
  setNames(cases[c("series", "new_vintage")], c("series", "vintage")),
  data.frame(series = "EG.ELC.PROD.KH", vintage = "201412")
))
wdi <- if (file.exists(raw)) read.csv(raw, colClasses = c(vintage = "character")) else NULL
missing <- needed[!paste(needed$series, needed$vintage) %in% paste(wdi$series, wdi$vintage), ]
if (nrow(missing) > 0) {
  wdi <- rbind(wdi, do.call(rbind, Map(fetch_wdi, missing$series, missing$vintage)))
  wdi <- wdi[order(wdi$series, wdi$vintage, wdi$year), ]
  dir.create(dirname(raw), recursive = TRUE, showWarnings = FALSE)
  write.csv(wdi, raw, row.names = FALSE)
}

get_series <- function(series, vintage) {
  x <- wdi[wdi$series == series & wdi$vintage == vintage, ]
  setNames(x$value, x$year)
}

# Oil production (TWh; crude, condensates and NGLs), Energy Institute Statistical Review of World
# Energy via Our World in Data. Oil dominates Kazakhstan's industry, which makes it the natural
# indicator; electricity is kept for comparison.
oil_raw <- "data/raw/owid_oil_production_kaz.csv"
if (!file.exists(oil_raw)) {
  oil <- read.csv("https://ourworldindata.org/grapher/oil-production-by-country.csv?useColumnShortNames=true")
  write.csv(oil[oil$code == "KAZ", c("year", "oil_production_twh")], oil_raw, row.names = FALSE)
}
oil <- read.csv(oil_raw)

# Related series measured the same way throughout, for the indicator method
indicators <- list(
  oil = setNames(oil$oil_production_twh, oil$year),
  electricity = get_series("EG.ELC.PROD.KH", "201412")  # kWh, from IEA energy balances
)

methods_for <- function(k) {
  m <- list(ratio = splice_ratio, difference = splice_difference)
  if (!is.na(k$old_base)) {
    m$interpolation <- function(old, new) splice_interpolate(old, new, base = k$old_base)
  }
  if (k$indicator) {
    for (i in names(indicators)) {
      m[[paste0("indicator_", i)]] <- local({
        ind <- indicators[[i]]
        function(old, new) splice_indicator(old, new, ind)
      })
    }
  }
  m
}

growth <- function(first, last, years) 100 * ((last / first)^(1 / years) - 1)


# 1. Validation: hide the first new-method years, backcast them, compare with the published values

validation <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  k <- cases[i, ]
  old <- get_series(k$series, k$old_vintage)
  new <- get_series(k$series, k$new_vintage)
  new <- new[as.numeric(names(new)) >= k$hide_from]
  hide <- k$hide_from:k$hide_to
  link <- as.character(k$hide_to + 1)
  methods <- methods_for(k)
  do.call(rbind, lapply(names(methods), function(m) {
    v <- backcast_validate(old, new, hide, methods[[m]])
    n <- k$hide_to + 1 - k$hide_from
    data.frame(case = k$case, method = m, hidden = paste0(k$hide_from, "-", k$hide_to),
               mean_abs_error_pct = mean(abs(v$error_pct)),
               max_abs_error_pct = max(abs(v$error_pct)),
               growth_published_pct = growth(v$published[1], new[[link]], n),
               growth_backcast_pct = growth(v$backcast[1], new[[link]], n))
  }))
}))


# 2. Backcast: industry value added on the 2005 reference year, from 2005 back to 1995

k <- cases[cases$case == "industry_rebase_2014", ]
old <- get_series(k$series, k$old_vintage)
published <- get_series(k$series, k$new_vintage)
new <- published[as.numeric(names(published)) >= 2005]
backcast <- backcast_compare(old, new, indicator = indicators$oil)
backcast$indicator_electricity <- unname(
  splice_indicator(old, new, indicators$electricity)[as.character(backcast$period)])
backcast$world_bank <- unname(published[as.character(backcast$period)])
backcast <- backcast[backcast$period >= 1995 & backcast$period <= 2013, ]
names(backcast)[names(backcast) == "indicator"] <- "indicator_oil"
long_run <- backcast_growth(backcast[c("period", "ratio", "difference", "indicator_oil",
                                       "indicator_electricity", "world_bank")],
                            from = 1995, to = 2013)


round_df <- function(x) {
  num <- vapply(x, is.numeric, TRUE)
  x[num] <- lapply(x[num], function(v) signif(round(v, 3), 10))
  x
}
dir.create("results", showWarnings = FALSE)
write.csv(round_df(validation), "results/kazakhstan.csv", row.names = FALSE)
write.csv(cases, "results/kazakhstan_cases.csv", row.names = FALSE)
write.csv(round_df(backcast), "results/industry_backcast.csv", row.names = FALSE)
write.csv(round_df(long_run), "results/industry_growth.csv", row.names = FALSE)

png("results/industry_backcast.png", width = 1600, height = 1000, res = 200)
cols <- c(ratio = "#1b6ca8", difference = "#c0392b", indicator_oil = "#27ae60",
          indicator_electricity = "#e67e22")
matplot(backcast$period, backcast[names(cols)] / 1e12, type = "l", lty = 1, lwd = 2, col = cols,
        xlab = "", ylab = "trillion tenge, 2005 prices", las = 1,
        main = "Kazakhstan industry value added, backcast from 2005 to 1995")
abline(v = 2005, lty = 3, col = "grey40")
mtext("new method from 2005", side = 3, at = 2005, line = 0.2, cex = 0.7)
legend("topleft", names(cols), col = cols, lwd = 2, bty = "n")
invisible(dev.off())

print(round_df(validation), row.names = FALSE)
print(round_df(long_run), row.names = FALSE)
