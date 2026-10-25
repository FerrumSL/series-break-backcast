# Series break backcaster

When a statistics office changes how it measures something (a new base year, a new industry
classification, a redesigned survey), the series jumps for a reason that has nothing to do with
the economy. The office publishes the new method from some year on, and users either glue the
old and new parts together or cut the history off. How the gluing is done matters. Prados de la
Escosura shows that the usual splice, which carries the level gap back unchanged
(retropolation), tends to overstate past levels and understate growth, and proposes
interpolation instead
([CAGE WP 202, 2014](https://warwick.ac.uk/fac/soc/economics/research/centres/cage/manage/publications/202-2014_leandro_prados_de_la_escosura.pdf);
*Cliometrica* 2016). Bill Martin argues that the ONS's 2014 splice of post-war UK investment was
a mistake that is still in the official historic accounts
([Cambridge CBR WP543, 2025](https://www.jbs.cam.ac.uk/wp-content/uploads/2025/07/cbr-wp543.pdf)).
The UN handbook on backcasting national accounts, which describes retropolation, interpolation
and mixed splicing, is still a September 2018 draft
([UNSD](https://unstats.un.org/unsd/nationalaccount/aeg/2018/M12_8iiib_Backcasting.pdf)). The
Australian Bureau of Statistics added a backcasting facility to its own seasonal adjustment
software in 2006
([ABS Methodological News](https://abs.gov.au/ausstats/abs@.nsf/7d12b0f6763c78caca257061001cc588/371b091c80e5aa77ca25729d000f70f0!OpenDocument)).
On CRAN, packages exist for
benchmarking and temporal disaggregation (`tempdisagg`, `gseries`, `rjd3bench`) and for
chain-linking index numbers (`IndexNumberTools`), but none backcasts a series across a
methodology break or compares the ways of doing it.

This tool extends the new-method series back with several published methods side by side,
reports how far they disagree, and measures each method's error where the office published both
series for a few years.

It is the official-statistics twin of
[breakpoint-drift](https://github.com/FerrumSL/breakpoint-drift), which separates a change in
antibiotic breakpoints from a real change in resistance: in both, a change in how something is
measured has to be told apart from a change in the thing itself.

**Status:** v1.0.0, an installable R package. Five methods and a break test, synthetic tests,
a validation on four real Kazakhstan revisions, and two real backcasts: Kazakhstan's industry
value added to 1995, and its population across the 2021 census.

```r
# install.packages("remotes")
remotes::install_github("FerrumSL/series-break-backcast")
library(backcast)
```

## Methods

`old` and `new` are named numeric vectors (names are years) that share at least one year, the
link. Each method returns the new series extended back to the first old year.

| Function | Assumes | Keeps from the old series |
|---|---|---|
| `splice_ratio()` | the gap is a constant factor | growth rates |
| `splice_difference()` | the gap is a constant amount | absolute changes |
| `splice_interpolate(base = )` | the old series was right in its own base year and the gap built up gradually since, as its weights went out of date (Prados de la Escosura) | growth rates, corrected by a geometric drift |
| `splice_mixed(base = , rho = )` | the old series was already wrong in `base` by a share `rho` of the final gap, and the error grew geometrically since, as missed activities outgrew the rest (de la Fuente 2013) | growth rates, corrected by an accelerating drift |
| `splice_indicator(indicator = )` | a related series was measured the same way throughout | nothing; predicts from the indicator |

`splice_mixed()` sits between the two extremes: `rho = 1` is the ratio splice, and `rho` near 0
approaches interpolation. de la Fuente leaves `rho` to judgement
([BBVA Research WP 13/02](https://www.bbvaresearch.com/wp-content/uploads/migrados/WP_1302_tcm348-364162.pdf)).

`backcast_compare()` runs every method that applies and adds `spread_pct`, the range across
methods as a share of their mean, per year. `backcast_growth()` gives each method's average
yearly growth over a span. `backcast_validate()` hides the first years of the new series where
both were published, backcasts them, and reports the error against the published values.
`find_break()` tests whether a spliced series changes level or trend (sup-F test, then the
break dated by least squares, via `strucchange`), to check a documented break date against the
data or find an undocumented one.

```r
x <- backcast_compare(old, new, base = 2005, rho = 0.5)
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
- an error that grows geometrically from a share of the gap: mixed splicing is exact, and with
  `rho = 1` it equals the ratio splice
- with no distortion all methods agree and the spread is zero
- validation on a published overlap gives zero error for the right method
- `find_break()` dates a 10% level jump correctly and finds nothing in a steady noisy series
- integer series the size of a population do not overflow

```sh
R CMD build . && R CMD check --no-manual backcast_1.0.0.tar.gz
```

## Kazakhstan: four real revisions

The World Bank keeps every past release of its World Development Indicators (WDI Database
Archives, API source 57). Where a later release revised a series' history, the earlier release
is the old-method series and the later one the new-method series, published for the same years.
Kazakhstan's national accounts in WDI come from the Bureau of National Statistics as chained
volumes rescaled to a reference year (2005 today, per WDI's country notes). The reference year
in each release can be read off the data: it is the year where constant and current prices
match. It was 1995 in the October 2009 release, 2000 from December 2010 and 2005 from
December 2014.

`scripts/kazakhstan.R` takes four revisions, hides the first years of the new series,
backcasts them from the old one, and compares with what was published. A new release carries
its own method back only to its reference year (before it, the new series is the old one
rescaled by a constant), so only years from that reference year on are hidden.

| Case | Change | Hidden | Ratio | Difference | Interpolation | Indicator |
|---|---|---|---|---|---|---|
| Industry value added, Dec 2014 release | reference year 2000 to 2005, growth rates revised | 2005-2008 | **1.29** | 5.41 | n/a | 4.12 / 19.66 |
| Industry value added, Dec 2025 release | history revised, mostly from 2009 on | 2005-2014 | 0.45 | 0.46 | **0.41** | |
| GDP, Dec 2014 release (control) | reference year 2000 to 2005, growth rates unchanged | 2005-2008 | **0.00** | 5.01 | n/a | 1.63 / 33.04 |
| Population, Dec 2024 release | every year back to 1995 revised | 2009-2015 | **0.11** | 0.28 | 1.69 | |

Mean absolute error of the backcast, % of the published level; best method in bold. The
indicators are electricity production (kWh, IEA energy balances via WDI) / oil production (TWh,
Energy Institute Statistical Review via Our World in Data), fitted on the new-method years after
the hidden ones. Full results, including the average growth each method implies over
the hidden years, are in `results/kazakhstan.csv`; the cases are described in
`results/kazakhstan_cases.csv`.

- The control works: the GDP release changed only the reference year, so the ratio splice is
  exact.
- The difference splice is the wrong tool for growing series. Over 2005-2009 it gives industry
  growth of 3.3% a year against 6.2% published, and GDP 3.1% against 6.0%.
- On the industry change, even the best method misses: the ratio splice gives 5.9% a year
  against 6.2%, because the new release revised the industry's own growth rates.
- Interpolation does not apply to a change of reference year: the two releases are in prices of
  different years, so the ratio new/old is never 1, which interpolation assumes for the old base
  year. Where both releases are in the same units, it was best on the 2025 industry revision
  and worst on population, where every year was revised rather than a gap building up after the
  2009 census.
- The indicator method is fragile on a short fit. Oil should be the natural indicator for an
  oil-led industry, but over the 2009-2013 fit window oil output barely moved while industry
  grew, so the fitted elasticity is 2.2 and the backcast overshoots.

## Kazakhstan: industry value added back to 1995

The December 2014 release measures industry on the 2005 reference year. Taking it as published
from 2005 and backcasting 1995-2004 from the old release gives:

![Industry value added, backcast with four methods](results/industry_backcast.png)

| Method | Average growth 1995-2013 |
|---|---|
| ratio | 6.5% a year |
| difference | 4.4% a year |
| indicator: oil production | 9.4% a year |
| indicator: electricity production | 2.2% a year |
| as published by the World Bank | 6.5% a year |

The methods put long-run industry growth anywhere from 2.2% to 9.4% a year, and the 1995 level
anywhere from 0.85 to 2.86 trillion tenge (`results/industry_backcast.csv`). The World Bank's
published back years are the ratio splice to within rounding, and the validation above favours
the ratio splice for this kind of change. Both indicators fail in opposite directions: power
output fell by 29% from 1995 to 1999 while industry held level, and oil output rose fourfold
from 1995 to 2013, faster than the rest of industry.

Run `Rscript scripts/kazakhstan.R`. It downloads the releases it needs once into
`data/raw/` and reruns offline after that.

## Kazakhstan: population across the 2021 census

The Bureau of National Statistics estimates the population at the start of each year from the
last census plus births and arrivals, minus deaths and departures. Its published series
([stat.gov.kz dynamic tables](https://stat.gov.kz/ru/industries/social-statistics/demography/dynamic-tables/))
is still based on the 2009 census up to the start of 2021 and on the 2021 census from the start of
2022. The census correction was never spread back: the accounting identity holds to the person
in every year except 2021-2022, where the population jumps by 380,751 (2.0%) more than births,
deaths and migration explain. `find_break()` puts the break at 2022 (p < 0.001) from the
published series alone.

`scripts/bns_population.R` rebuilds the old-method population for 2022 with the Bureau's own
method, which gives the one year measured both ways, and backcasts 2009-2021:

| Method | 2009 | 2021 | Average growth 2009-2021 |
|---|---|---|---|
| as published (old method) | 15.98 m | 18.88 m | 1.40% a year |
| ratio | 16.30 m | 19.25 m | 1.40% a year |
| difference | 16.36 m | 19.26 m | 1.37% a year |
| interpolation (2009 census taken as right) | 15.98 m | 19.23 m | 1.55% a year |
| mixed, `rho = 0.5` | 16.14 m | 19.24 m | 1.47% a year |

Interpolation is the method the census logic points to, since the 2009 count is the old
series' own benchmark: it keeps 2009 and spreads the 380,751 over the twelve years, so growth
was 0.15 points a year faster than published. The ratio splice instead puts 2% more people in
Kazakhstan in 2009 than that census counted. For comparison, the UN's World Population
Prospects 2024 (via WDI) gives 1.45% a year for mid-2009 to mid-2021 and also raises 2009, which
is the mixed splice's assumption.

Run `Rscript scripts/bns_population.R`. It downloads the four Bureau tables once into
`data/raw/bns_population.csv`, checks the accounting identity, and reruns offline after that.

## Next

- More Bureau series with a documented break: OKED classification changes, labour force survey
  redesigns.
- An industry indicator built from several physical outputs (oil, metals, electricity) with a
  longer fit window.
- Several breaks per series in `find_break()`.

R 4.6.1; packages pinned in `renv.lock` (`renv::restore()`). The scripts `source("R/backcast.R")`
so they run from a clone without installing the package. Licence: GPL-2.
