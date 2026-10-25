# Backcast Kazakhstan's population across the 2021 census.
# Run from the project root: Rscript scripts/bns_population.R
#
# The Bureau of National Statistics estimates the population at the start of each year from the
# last census, adding births and arrivals and subtracting deaths and departures (the method
# stated in its metadata). Its published series still holds these 2009-census-based estimates up
# to the start of 2021 and switches to 2021-census-based figures from the start of 2022; the
# census correction was not spread back over 2009-2021. So the series is old-method to 2021 and
# new-method from 2022, with no year published both ways.
#
# The overlap year is rebuilt with the Bureau's own method: the old-method population at the
# start of 2022 is the start of 2021 plus 2021's births, minus its deaths, plus its net migration.
# The script checks that this identity holds exactly in every other year.

source("R/backcast.R")

raw <- "data/raw/bns_population.csv"

# Bureau dynamic tables (stat.gov.kz, demography): population at the start of the year, births,
# deaths, net migration. Element ids are those of the download links on the tables page.
bns_files <- c(population = 6584, births = 6548, deaths = 6550, net_migration = 6567)

# National row of every year sheet in a Bureau dynamic table -> named vector (names = years).
# Year headers may carry a footnote mark ("2009*", "20091)").
read_bns <- function(path) {
  out <- c()
  for (sh in setdiff(readxl::excel_sheets(path), c("Метаданные", "Условные обозначения",
                                                    "по районам", "По районам"))) {
    x <- as.matrix(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text",
                                      .name_repair = "minimal"))
    header <- which(rowSums(matrix(grepl("^(19|20)[0-9]{2}", x), nrow(x))) >= 3)[1]
    row <- which(apply(x, 1, function(r) any(trimws(r) == "Республика Казахстан", na.rm = TRUE)))[1]
    if (is.na(header) || is.na(row)) next  # older sheets laid out differently; years before 2000 unused
    year <- sub("^((19|20)[0-9]{2}).*$", "\\1", x[header, ])
    keep <- grepl("^(19|20)[0-9]{2}$", year)
    out[year[keep]] <- as.numeric(x[row, keep])
  }
  out
}

if (!file.exists(raw)) {
  series <- lapply(bns_files, function(id) {
    f <- tempfile(fileext = ".xlsx")
    download.file(sprintf("https://stat.gov.kz/api/iblock/element/%d/file/ru/", id), f,
                  mode = "wb", quiet = TRUE)
    read_bns(f)
  })
  years <- as.character(2000:max(as.numeric(names(series$population))))
  bns <- data.frame(year = as.numeric(years),
                    lapply(series, function(s) unname(s[years])))
  dir.create(dirname(raw), recursive = TRUE, showWarnings = FALSE)
  write.csv(bns, raw, row.names = FALSE)
}
bns <- read.csv(raw)

# Change in population over each year against births - deaths + net migration
by_year <- function(col) setNames(bns[[col]], bns$year)
pop <- by_year("population")
y <- as.character(2009:(max(bns$year) - 1))
y_next <- as.character(as.numeric(y) + 1)
flows <- by_year("births")[y] - by_year("deaths")[y] + by_year("net_migration")[y]
residual <- setNames(pop[y_next] - (pop[y] + flows), y_next)
stopifnot(all(residual[names(residual) != "2022"] == 0))  # the identity holds except at the census

old <- c(pop[as.character(2009:2021)], "2022" = pop[["2021"]] + flows[["2021"]])
new <- pop[as.character(2022:max(bns$year))]

# Interpolation takes the 2009 census as right, so the gap builds up between the two censuses
backcast <- backcast_compare(old, new, base = 2009)
backcast$published <- unname(pop[as.character(backcast$period)])
long_run <- backcast_growth(backcast[c("period", "ratio", "difference", "interpolation", "published")],
                            from = 2009, to = 2021)

round_df <- function(x) {
  num <- vapply(x, is.numeric, TRUE)
  x[num] <- lapply(x[num], function(v) signif(round(v, 3), 10))
  x
}
dir.create("results", showWarnings = FALSE)
write.csv(round_df(backcast), "results/population_backcast.csv", row.names = FALSE)
write.csv(round_df(long_run), "results/population_growth.csv", row.names = FALSE)

cat(sprintf("Census correction at the start of 2022: %+.0f people (%+.2f%%)\n",
            residual[["2022"]], 100 * residual[["2022"]] / old[["2022"]]))
print(round_df(backcast[backcast$period %in% c(2009, 2015, 2021, 2022), ]), row.names = FALSE)
print(round_df(long_run), row.names = FALSE)
