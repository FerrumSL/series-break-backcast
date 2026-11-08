# Size and power of find_break() on short annual series (n = 22), by simulation.
# Run from the repo root: Rscript scripts/simulation.R
# Writes results/simulation_find_break.csv. Monte Carlo SE = sqrt(p (1 - p) / reps).
source("R/backcast.R")
set.seed(2026)
reps <- 200
B <- 199          # bootstrap replications inside find_break (default 499)
n <- 22
years <- 2000:(1999 + n)

make <- function(ar, curve, jump) {
  e <- as.numeric(arima.sim(list(ar = ar), n, sd = 0.01))
  g <- 0.03 - curve * (1:n)
  lvl <- 4.6 + cumsum(g) + e + ifelse(years >= 2011, log(1 + jump), 0)
  setNames(exp(lvl), years)
}

grid <- expand.grid(ar = c(0, 0.7), curve = c(0, 0.0006), jump = c(0, 0.05, 0.10))
res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  out <- replicate(reps, {
    r <- find_break(make(g$ar, g$curve, g$jump), B = B)
    c(found = !is.na(r$period), at = r$period)
  })
  found <- out["found", ] == 1
  data.frame(g, reps = reps, rate = mean(found), mc_se = sqrt(mean(found) * (1 - mean(found)) / reps),
             median_error = if (g$jump > 0 && any(found)) median(out["at", found] - 2011) else NA)
}))
dir.create("results", showWarnings = FALSE)
write.csv(res, "results/simulation_find_break.csv", row.names = FALSE)
print(res)
