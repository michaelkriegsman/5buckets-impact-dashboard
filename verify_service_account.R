#!/usr/bin/env Rscript
# verify_service_account.R
# One-off helper to confirm a Google service account JSON key can READ the three
# dashboard sheet targets before deploying to Connect Cloud (Step 2 verification).
#
# This script contains NO secrets: it reads the key location from the first CLI
# argument, or from the GOOGLE_SERVICE_ACCOUNT_KEY env var (a file path or raw JSON).
#
# Usage:
#   Rscript verify_service_account.R ~/.secrets/5buckets/service-account-key.json
#   # or, if GOOGLE_SERVICE_ACCOUNT_KEY is already exported:
#   Rscript verify_service_account.R

suppressPackageStartupMessages(library(googlesheets4))

args <- commandArgs(trailingOnly = TRUE)
key <- if (length(args) >= 1 && nzchar(args[1])) args[1] else Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY", "")

if (!nzchar(key)) {
  stop("No key provided. Pass the JSON key path as the first argument, or set GOOGLE_SERVICE_ACCOUNT_KEY.")
}

# Authenticate exactly like global.R does: accept a file path OR raw JSON content.
if (file.exists(key)) {
  cat("Using key file:", key, "\n")
  gs4_auth(path = key)
} else if (grepl("^\\s*\\{", key)) {
  cat("Using inline JSON key from env var\n")
  tmp <- tempfile(fileext = ".json")
  writeLines(key, tmp)
  gs4_auth(path = tmp)
} else {
  stop("Key is neither an existing file path nor JSON content starting with '{'.")
}

# Mirror the IDs/tabs from global.R (these IDs are public, not secrets).
targets <- list(
  list(label = "Master Pre",      id = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw", tab = "Pre Submissions"),
  list(label = "Master Post",     id = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw", tab = "Post Submissions"),
  list(label = "Program Manager", id = "1aefJFVQtfYx2UCd5u9q1pl0raJXVzXaW8pKlzZRMXuQ", tab = "Workshops")
)

cat("\nAuthenticated as:", tryCatch(gs4_user(), error = function(e) "<unknown>"), "\n")
cat(strrep("-", 60), "\n")

ok <- TRUE
for (t in targets) {
  res <- tryCatch({
    d <- read_sheet(t$id, sheet = t$tab, n_max = 5)
    sprintf("OK  | %-16s | tab '%s' | %d cols, read %d sample rows",
            t$label, t$tab, ncol(d), nrow(d))
  }, error = function(e) {
    ok <<- FALSE
    sprintf("FAIL| %-16s | tab '%s' | %s", t$label, t$tab, conditionMessage(e))
  })
  cat(res, "\n")
}

cat(strrep("-", 60), "\n")
if (ok) {
  cat("SUCCESS: service account can read all targets. Ready for Step 3.\n")
} else {
  cat("Some reads FAILED. Most common cause: the workbook is not shared with the\n")
  cat("service account email as Viewer, or the Sheets/Drive API is not enabled.\n")
  quit(status = 1)
}
