# backcast 1.0.0 (not released)

No result in `results/` changes.

- `splice_indicator(spec = "growth")` stops with fewer than 4 periods shared with `new`. With 3
  it fitted a line through two yearly changes and reported R2 = 1 with no residual error.
- `splice_difference()` warns when the shift takes an early value to zero or below.
- `splice_mixed()` documentation corrected: a `rho` near 0 does not approach interpolation. It
  shares interpolation's starting point (no error in `base`) but puts most of the correction
  just before the link. Documented what happens before `base`, and the half-life form of `rho`
  (de la Fuente 2013, equation 24).

## Fixes
* A numeric `link` (such as `2002`) works in every splice; before, `splice_ratio()` returned NA and
  `splice_interpolate()` failed with "subscript out of bounds".
* All entry points check their input: numeric, named by year, no duplicates, no NA or infinite
  values, all positive. Errors name the function and the first bad period. Unsorted input is sorted.
* `backcast_growth()` stops when `from` equals `to` or a period is not in the table.
* `backcast_compare()` keeps a `note` column; an indicator that does not reach back far enough gives
  NA for those years and the spread uses the methods that do have a value.
* `find_break()` no longer flags smooth slowing growth as a break. It fits a quadratic trend and
  takes its p-value from an AR(1) bootstrap (before: sup-F on a linear trend with asymptotic
  critical values, which reported a break in 97.5% of curved, autocorrelated no-break series; now 8.5-11%).
  It now returns a list with `reason`. See `results/simulation_find_break.csv`.

## New
* `splice_indicator()` returns its fit (`attr(, "fit")`), warns when it extrapolates over more periods
  than it was fitted on, and has `spec = "growth"`.
* `backcast_validate()` reports the growth error over a multi-year hidden span.
* `scripts/simulation.R` (size and power of `find_break()`) and `scripts/simulation_splice.R`
  (bias and RMSE of each splice under five distortions).
* Validation by hide length and leave-one-case-out tables in `results/`.

## Documentation
* README: input rules, terms, data licences, claims reworded as "estimated" or "if ... is right".
