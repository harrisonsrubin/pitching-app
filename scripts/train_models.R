# Usage:
#   Rscript scripts/train_models.R forward            # train MLB 2023-2025, test 2026; ablations
#   Rscript scripts/train_models.R oof [names...]     # 5 pitcher-held-out folds (default: all 8)
# Boosters go to <data_dir>/models/lgb/<name>/. Forward results go to
# data/reports/model_eval.csv and data/reports/calibration.csv.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !args[1] %in% c("forward", "oof")) stop("Usage: train_models.R forward|oof [names...]")
reports <- file.path(cfg()$data_dir, "reports")
dir.create(reports, recursive = TRUE, showWarnings = FALSE)

if (args[1] == "forward") {
  refs <- read_refs()
  names <- c("stuff", "location", "pitching", "stuff_physics", "pitching_noseq",
             "p_swing", "p_whiff", "p_cstrike", "xwoba_contact", "p_gb")
  res <- lapply(names, function(n) { message("forward: ", n); r <- forward_eval(n, refs); gc(); r })
  ev <- dplyr::bind_rows(lapply(res, `[[`, "eval"))
  readr::write_csv(ev, file.path(reports, "model_eval.csv"))
  readr::write_csv(dplyr::bind_rows(lapply(res, `[[`, "calibration")), file.path(reports, "calibration.csv"))
  print(as.data.frame(ev))
} else {
  names <- if (length(args) > 1) args[-1] else names(MODEL_SPECS)
  for (n in names) { message("oof: ", n); train_oof(n); gc() }
}
