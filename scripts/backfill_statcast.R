# Usage: Rscript scripts/backfill_statcast.R <MLB|AAA> <first_season> <last_season>
# Fetches every missing game day, checks each against the schedule, then compacts
# each season into one parquet file. Exits non-zero if any day could not be fetched
# completely (rerun to retry just those days).
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/backfill_statcast.R <MLB|AAA> <first_season> <last_season>")
level <- args[1]
seasons <- seq(as.integer(args[2]), as.integer(args[3]))
log <- backfill_statcast(level, seasons)
print(dplyr::count(log, status))
for (s in seasons) message("Compacted: ", compact_season(level, s))
if (backfill_failed(log)) {
  message("Some days failed; see the log in ", file.path(cfg()$data_dir, "logs"))
  quit(status = 1)
}
