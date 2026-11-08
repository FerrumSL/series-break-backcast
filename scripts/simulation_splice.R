# Which splice wins under which distortion? Known truth, five distortion families, noise, overlap
# lengths 1/3/5. Reports bias and RMSE (percent) of the backcast level averaged over the backcast years (log, percent), and of
# average yearly growth over the backcast span, with Monte Carlo SE of the bias.
# Run from the repo root: Rscript scripts/simulation_splice.R   (writes results/simulation_splice.csv)
# Interpolation and mixed are told the TRUE onset of the error (base); the misspecified rows
# (distortion that those methods do not assume) are the point of the exercise.
source("R/backcast.R")
set.seed(2026)
B <- 500
n_back <- 15                       # periods before the new series starts
years <- 1:(n_back + 10)           # truth: 3% growth, noise only in what is observed
link_year <- n_back + 1
base <- 5                          # old series right in year 5
sd_noise <- 0.01

distortions <- list(
  constant_ratio    = function(t, truth) truth * exp(0.20),
  constant_difference = function(t, truth) truth + 0.2 * truth[link_year],
  gradual_drift     = function(t, truth) truth * exp(0.20 * pmin(pmax((t - base) / (link_year - base), 0), 1)),
  accelerating_drift = function(t, truth) truth * exp(0.20 * pmin(pmax((t - base) / (link_year - base), 0), 1)^2),
  level_shift_plus_drift = function(t, truth) truth * exp(0.10 + 0.10 * pmin(pmax((t - base) / (link_year - base), 0), 1))
)
# every distortion makes OLD LARGER than truth, sign is irrelevant for the ranking

methods <- list(
  ratio = function(o, n, link) splice_ratio(o, n, link),
  difference = function(o, n, link) splice_difference(o, n, link),
  interpolation = function(o, n, link) splice_interpolate(o, n, base = base, link = link[1]),
  mixed_rho_0.5 = function(o, n, link) splice_mixed(o, n, base = base, rho = 0.5, link = link[1])
)

rows <- list()
for (g in c(0.03, 0.01)) {
  truth0 <- 100 * exp(g * (years - 1))
  for (d in names(distortions)) for (ov in c(1, 3, 5)) for (noise in c(0, sd_noise)) {
    errs <- replicate(B, {
      obs_noise <- function() exp(rnorm(length(years), 0, noise))
      new_all <- truth0 * obs_noise()
      old_all <- distortions[[d]](years, truth0) * obs_noise()
      new <- setNames(new_all[years >= link_year], years[years >= link_year])
      old <- setNames(old_all[years <= link_year + ov - 1], years[years <= link_year + ov - 1])
      link <- as.character(link_year:(link_year + ov - 1))
      vapply(methods, function(m) {
        b <- m(old, new, link)
        yrs <- as.character(1:n_back)
        c(level = 100 * mean(log(b[yrs]) - log(truth0[1:n_back])),
          growth = 100 * ((b[[as.character(link_year)]] / b[["1"]])^(1 / n_back) - 1) - 100 * (exp(g) - 1))
      }, c(level = 0, growth = 0))
    })
    for (j in seq_along(methods)) {
      lv <- errs[1, j, ]; gr <- errs[2, j, ]
      rows[[length(rows) + 1]] <- data.frame(
        truth_growth_pct = 100 * g, distortion = d, overlap = ov, noise_sd = noise, method = names(methods)[j],
        level_bias_pct = mean(lv), level_rmse_pct = sqrt(mean(lv^2)), level_bias_mcse = sd(lv) / sqrt(B),
        growth_bias_pp = mean(gr), growth_rmse_pp = sqrt(mean(gr^2)), reps = B)
    }
  }
}
res <- do.call(rbind, rows)
dir.create("results", showWarnings = FALSE)
write.csv(transform(res, level_bias_pct = round(level_bias_pct, 3), level_rmse_pct = round(level_rmse_pct, 3),
                    level_bias_mcse = round(level_bias_mcse, 3), growth_bias_pp = round(growth_bias_pp, 3),
                    growth_rmse_pp = round(growth_rmse_pp, 3)),
          "results/simulation_splice.csv", row.names = FALSE)
sub <- res[res$truth_growth_pct == 3 & res$overlap == 3 & res$noise_sd > 0, ]
print(reshape(sub[c("distortion", "method", "level_rmse_pct")], idvar = "distortion", timevar = "method",
              direction = "wide"), row.names = FALSE)
