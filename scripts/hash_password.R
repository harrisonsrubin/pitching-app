# Usage: Rscript scripts/hash_password.R
# Prints a bcrypt hash of the admin password to paste into the host's PV_ADMIN_HASH
# environment variable. The password itself is never stored anywhere.
pw <- if (interactive()) readline("Admin password: ") else {
  cat("Admin password (input is not hidden in a plain terminal): ")
  system("stty -echo", ignore.stderr = TRUE); on.exit(system("stty echo", ignore.stderr = TRUE))
  x <- readLines(file("stdin"), n = 1); cat("\n"); x
}
if (!nzchar(pw)) stop("Empty password.")
cat(bcrypt::hashpw(pw), "\n")
