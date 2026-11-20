# Usage: Rscript scripts/changed_seasons.R <before_sha> <after_sha>
# Prints the seasons whose corrections file changed between two commits, space-separated.
# A zero "before" sha (first push of a branch) means every corrections file.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript scripts/changed_seasons.R <before_sha> <after_sha>")
paths <- if (grepl("^0+$", args[1])) {
  file.path("corrections", list.files(file.path(repo_root(), "corrections")))
} else {
  system2("git", c("diff", "--name-only", args[1], args[2]), stdout = TRUE)
}
cat(changed_seasons(paths))
