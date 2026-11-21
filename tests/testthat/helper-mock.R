# Swap a global function until the calling test ends (not a package, so no mocked bindings)
local_global_binding <- function(..., .frame = parent.frame()) {
  new <- list(...)
  env <- globalenv()
  old <- mget(names(new), envir = env)
  for (nm in names(new)) assign(nm, new[[nm]], envir = env)
  withr::defer(for (nm in names(old)) assign(nm, old[[nm]], envir = env), envir = .frame)
  invisible(old)
}
