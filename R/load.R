# Sources every file in R/ (except this one) in alphabetical order.
# Usage, from anywhere inside the repo:  source("R/load.R")
local({
  here <- normalizePath(getwd())
  root <- here
  while (!file.exists(file.path(root, "R", "load.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Could not find the repo root (R/load.R) above ", here)
    root <- parent
  }
  files <- sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))
  files <- files[basename(files) != "load.R"]
  for (f in files) sys.source(f, envir = globalenv())
  assign(".pv_root", root, envir = globalenv())
})
