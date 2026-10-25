# Series break backcaster

When a statistics office changes how it measures something (a new base year, a new industry
classification, a redesigned survey), the series jumps for a reason that has nothing to do with
the economy. The office publishes the new method from some year on, and users either glue the
old and new parts together or cut the history off. How the gluing is done matters: splicing
national accounts by carrying the level gap back unchanged biases long-run growth
([Prados de la Escosura, VoxEU](https://cepr.org/voxeu/columns/mismeasuring-long-run-growth-bias-spliced-national-accounts)),
and a 2025 Cambridge working paper argues that the ONS's spliced backcast of post-war investment
was wrong ([CBR WP543](https://www.jbs.cam.ac.uk/wp-content/uploads/2025/07/cbr-wp543.pdf)).
The UN handbook on backcasting national accounts has been in draft since 2018
([UNSD](https://unstats.un.org/unsd/nationalaccount/aeg/2018/M12_8iiib_Backcasting.pdf)), and
the Australian Bureau of Statistics' backcasting system is internal. On CRAN, packages exist for
benchmarking and temporal disaggregation (`tempdisagg`, `gseries`, `rjd3bench`) and for
chain-linking index numbers (`IndexNumberTools`), but none backcasts a series across a
methodology break or compares the ways of doing it.

This tool extends the new-method series back with several published methods side by side,
reports how far they disagree, and measures each method's error where the office published both
series for a few years.

**Status:** draft. Methods, synthetic tests, and a first validation on four real Kazakhstan
revisions (below).

## Methods

`old` and `new` are named numeric vectors (names are years) that share at least one year, the
link. Each method returns the new series extended back to the first old year.

| Function | Assumes | Keeps from the old series |
|---|---|---|
| `splice_ratio()` | the gap is a constant factor | growth rates |
| `splice_difference()` | the gap is a constant amount | absolute changes |
| `splice_interpolate(base = )` | the old series was right in its own base year and the gap built up gradually since, as its weights went out of date (Prados de la Escosura) | growth rates, corrected by a geometric drift |
| `splice_indicator(indicator = )` | a related series was measured the same way throughout | nothing; predicts from the indicator |

`backcast_compare()` runs every method that applies and adds `spread_pct`, the range across
methods as a share of their mean, per year. `backcast_growth()` gives each method's average
yearly growth over a span. `backcast_validate()` hides the first years of the new series where
both were published, backcasts them, and reports the error against the published values.

```r
source("R/backcast.R")
x <- backcast_compare(old, new, base = 2005)
backcast_growth(x, from = 2000, to = 2015)
backcast_validate(old, new, hide = 2015, method = splice_ratio)
```

## Tests

`tests/testthat/test_backcast.R` builds a true series growing 3% a year, distorts it in a known
way to make the old one, and checks which methods recover it:

- a constant level factor: the ratio splice is exact, the difference splice is not
- a constant amount: the difference splice is exact, the ratio splice is not
- a gap that builds up from the old base year (2005) to 20% at the link (2015): interpolation
  is exact, while the ratio splice gives 1.8% a year of growth for 2000-2015 instead of 3%
- an exact or slightly noisy indicator: the indicator method recovers the level
- with no distortion all methods agree and the spread is zero
- validation on a published overlap gives zero error for the right method

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Kazakhstan: four real revisions

The World Bank keeps every past release of its World Development Indicators (WDI Database
Archives, API source 57). Where a later release revised a series' history, the earlier release
is the old-method series and the later one the new-method series, published for the same years.
`scripts/kazakhstan.R` takes four such revisions for Kazakhstan, hides the first years of the new
series, backcasts them from the old one, and compares with what was published. A new release
carries its own method back only to its base year (before it, the new series is the old one
rescaled), so only years from that base year on are hidden.

| Case | Change | Hidden | Ratio | Difference | Interpolation |
|---|---|---|---|---|---|
| Industry value added, Dec 2014 release | base year 2000 to 2005 | 2005-2008 | 1.29 | 5.41 | n/a |
| Industry value added, Dec 2025 release | history revised, mostly from 2009 on, same 2005 base | 2005-2014 | 0.45 | 0.46 | 0.41 |
| GDP, Dec 2014 release (control) | base year 2000 to 2005, growth unchanged | 2005-2008 | 0.00 | 5.01 | n/a |
| Population, Dec 2024 release | UN WPP 2022 replaced by WPP 2024 | 2009-2015 | 0.11 | 0.28 | 1.69 |

Mean absolute error of the backcast, % of the published level. Full results, including the
average growth each method implies over the hidden years, are in `results/kazakhstan.csv`.

- The control works: the GDP rebasing changed only the level, so the ratio splice is exact.
- The difference splice is the wrong tool for growing series. Over 2005-2009 it gives industry
  growth of 3.3% a year against 6.2% published, and GDP 3.1% against 6.0%.
- On the industry rebasing, even the best method misses: the ratio splice gives 5.9% a year
  against 6.2%, because the new base year changed the industry's own growth rates.
- Interpolation does not apply to rebasings. The two releases are in constant prices of
  different years, so the ratio new/old is never 1, which interpolation assumes for the old base
  year. Where both releases are in the same units, it was best on the 2025 industry revision
  and worst on population, where the UN revised every year rather than letting a gap build up
  after the 2009 census. The causes in this table are read from the data; the next step is to confirm
each in the release notes.

Run `Rscript scripts/kazakhstan.R`. It downloads the releases once into
`data/raw/wdi_kaz_vintages.csv` and reruns offline after that.

## Next

- Confirm each revision's cause in the Bureau of National Statistics' own release notes.
- More cases, including series straight from the Bureau (OKED classification changes, labour
  force survey redesigns), and an indicator series for the indicator method.
- Break detection as a check on the documented break date (`strucchange::breakpoints`).

R 4.6.1; packages pinned in `renv.lock` (`renv::restore()`). Licence: GPL-2.
