# Usage: Rscript scripts/fetch_fangraphs.R <first_season> <last_season>
# Full-season FanGraphs pitching benchmarks -> raw/fangraphs/pitching/<season>.parquet
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript scripts/fetch_fangraphs.R <first> <last>")
for (s in seq(as.integer(args[1]), as.integer(args[2]))) {
  f <- fetch_fangraphs(s)
  store_write(f, fangraphs_key(s))
  q <- f[f$ip_outs >= 150, ]
  message(s, ": ", nrow(f), " pitchers; Stuff+ present for ", round(100 * mean(!is.na(q$stuff_plus)), 1),
          "% of those with 50+ IP")
}
