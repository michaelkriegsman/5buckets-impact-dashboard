#!/usr/bin/env Rscript
# Quick check: confirm we read from the correct Master workbook and print basic summary.
# Run from Impact Dashboard directory: Rscript scripts/check_master_data.R

# Run from Impact Dashboard directory: Rscript scripts/check_master_data.R
# Or from project root: Rscript "Pre-And-Post-Survey Flow/Impact Dashboard/scripts/check_master_data.R"
args <- commandArgs(trailingOnly = FALSE)
script_dir <- dirname(sub("^--file=", "", args[grep("^--file=", args)]))
if (length(script_dir) && nzchar(script_dir)) setwd(dirname(script_dir))
if (!file.exists("global.R")) {
  if (file.exists("Pre-And-Post-Survey Flow/Impact Dashboard/global.R"))
    setwd("Pre-And-Post-Survey Flow/Impact Dashboard")
  else if (file.exists("Impact Dashboard/global.R"))
    setwd("Impact Dashboard")
}
if (!file.exists("global.R")) stop("Run from Impact Dashboard directory (where global.R lives).")

cat("=== Master workbook data check ===\n\n")

# Source only what we need to avoid full app init
suppressPackageStartupMessages({
  library(googlesheets4)
})
# Sheet IDs and names (must match global.R)
MASTER_PRE_SHEET_ID <- Sys.getenv("MASTER_PRE_SHEET_ID", unset = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw")
MASTER_POST_SHEET_ID <- Sys.getenv("MASTER_POST_SHEET_ID", unset = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw")
MASTER_PRE_SHEET_NAME <- "Pre Submissions"
MASTER_POST_SHEET_NAME <- "Post Submissions"

expected_url <- "https://docs.google.com/spreadsheets/d/1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw/edit"
cat("Configured Master Pre/Post spreadsheet ID:", MASTER_PRE_SHEET_ID, "\n")
cat("Expected URL (from your link):           ", expected_url, "\n")
cat("Match:", identical(MASTER_PRE_SHEET_ID, "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw"), "\n\n")

cat("Sheet names we read by name (not gid):\n")
cat("  Pre: ", MASTER_PRE_SHEET_NAME, "\n")
cat("  Post:", MASTER_POST_SHEET_NAME, "\n\n")

# Load global helpers for normalization only (avoid full UI)
source("global.R", local = TRUE)

pre <- load_master_pre()
post <- load_master_post()

cat("--- Pre Submissions ---\n")
cat("Rows:", nrow(pre), "\n")
if (nrow(pre) > 0) {
  cat("Columns:", paste(colnames(pre), collapse = ", "), "\n")
  if ("org_name" %in% colnames(pre)) {
    orgs <- unique(trimws(as.character(pre$org_name)))
    orgs <- orgs[!is.na(orgs) & orgs != ""]
    cat("Unique org_name:", length(orgs), "\n")
    if (length(orgs) <= 20) cat("  ", paste(orgs, collapse = ", "), "\n") else cat("  (first 15)", paste(head(orgs, 15), collapse = ", "), "...\n")
  } else cat("(no org_name column after mapping)\n")
  if ("session_id" %in% colnames(pre)) {
    sid <- pre$session_id
    non_empty <- sum(!is.na(sid) & trimws(as.character(sid)) != "", na.rm = TRUE)
    cat("session_id: non-empty rows:", non_empty, "of", nrow(pre), "\n")
    if (non_empty > 0) cat("  Sample:", paste(head(trimws(as.character(sid)), 3), collapse = " | "), "\n")
  } else cat("(no session_id column after mapping)\n")
}

cat("\n--- Post Submissions ---\n")
cat("Rows:", nrow(post), "\n")
if (nrow(post) > 0) {
  cat("Columns:", paste(colnames(post), collapse = ", "), "\n")
  if ("org_name" %in% colnames(post)) {
    orgs <- unique(trimws(as.character(post$org_name)))
    orgs <- orgs[!is.na(orgs) & orgs != ""]
    cat("Unique org_name:", length(orgs), "\n")
  }
  if ("session_id" %in% colnames(post)) {
    sid <- post$session_id
    non_empty <- sum(!is.na(sid) & trimws(as.character(sid)) != "", na.rm = TRUE)
    cat("session_id: non-empty rows:", non_empty, "of", nrow(post), "\n")
  }
}

cat("\n--- Column mapping (global.R normalize_master_cols) ---\n")
cat("Sheet columns mapped to: org_name (from 'Organization' / 'Org name' / etc.), group (from 'Group'), session_id (from 'Session Link' / 'Session id')\n")

cat("\n--- v3 smoke checklist ---\n")
pct_parsed <- function(df) {
  if (!"timestamp_parsed" %in% colnames(df)) return(NA_real_)
  round(100 * mean(!is.na(df$timestamp_parsed)), 1)
}
cat("Pre timestamp_parsed %:", if (nrow(pre) > 0) pct_parsed(pre) else "n/a", "\n")
cat("Post timestamp_parsed %:", if (nrow(post) > 0) pct_parsed(post) else "n/a", "\n")
if ("language" %in% colnames(pre) || "language" %in% colnames(post)) {
  langs <- unique(c(
    if ("language" %in% colnames(pre)) trimws(as.character(pre$language)),
    if ("language" %in% colnames(post)) trimws(as.character(post$language))
  ))
  langs <- langs[!is.na(langs) & nzchar(langs)]
  cat("Distinct language values:", paste(langs, collapse = ", "), "\n")
}
source("data/process_data.R", local = TRUE)
source("data/big_pre_analysis.R", local = TRUE)
source("data/big_post_analysis.R", local = TRUE)
pm <- tryCatch(load_program_manager(), error = function(e) NULL)
if (!is.null(pm) && nrow(pm) > 0 && exists("identify_survey_type", mode = "function")) {
  pre_t <- identify_survey_type(pre, pm)
  post_t <- identify_survey_type(post, pm)
  cat("Big Pre rows:", sum(pre_t$is_big_pre %in% TRUE, na.rm = TRUE), "\n")
  cat("Big Post rows:", sum(post_t$is_big_post %in% TRUE, na.rm = TRUE), "\n")
}
if ("respondent_id" %in% colnames(pre) || "respondent_id" %in% colnames(post)) {
  source("server/respondent_pairing.R", local = TRUE)
  st <- compute_respondent_pairing_stats(pre, post)
  cat("Distinct respondent_id:", st$distinct_ids, "| paired:", st$paired_any, "\n")
}
cat("Done.\n")
