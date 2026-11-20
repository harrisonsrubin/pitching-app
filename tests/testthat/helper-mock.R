# Temporarily replace functions defined in the global environment (this repo sources
# plain R files rather than building a package, so testthat's local_mocked_bindings
# does not apply). Restored automatically when the calling test ends.
local_global_binding <- function(..., .frame = parent.frame()) {
  new <- list(...)
  env <- globalenv()
  old <- mget(names(new), envir = env)
  for (nm in names(new)) assign(nm, new[[nm]], envir = env)
  withr::defer(for (nm in names(old)) assign(nm, old[[nm]], envir = env), envir = .frame)
  invisible(old)
}
