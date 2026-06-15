#!/usr/bin/env Rscript
# Trace app data flow (no Shiny): load Master, build session summary, apply filters, see where rows are lost.
# Run from Impact Dashboard dir: Rscript scripts/trace_data_flow.R

args <- commandArgs(trailingOnly = FALSE)
script_dir <- dirname(sub("^--file=", "", args[grep("^--file=", args)]))
if (length(script_dir) && nzchar(script_dir)) setwd(dirname(script_dir))
if (!file.exists("global.R")) {
  if (file.exists("Pre-And-Post-Survey Flow/Impact Dashboard/global.R"))
    setwd("Pre-And-Post-Survey Flow/Impact Dashboard")
  else if (file.exists("Impact Dashboard/global.R"))
    setwd("Impact Dashboard")
}
if (!file.exists("global.R")) stop("Run from Impact Dashboard directory.")
source("global.R", local = TRUE)
source("data/process_data.R", local = TRUE)

cat("=== Data flow trace (simulating app with All orgs, All groups) ===\n\n")

pre_data <- load_master_pre()
post_data <- load_master_post()
cat("1. After load_master_pre/post:\n")
cat("   Pre rows:", nrow(pre_data), "| Post rows:", nrow(post_data), "\n")
if (nrow(pre_data) > 0) cat("   Pre session_id sample:", head(trimws(as.character(pre_data$session_id)), 2), "\n\n")

ss_raw <- create_session_summary_from_master(pre_data, post_data, NULL)
cat("2. After create_session_summary_from_master (no filters):\n")
cat("   Session summary rows:", nrow(ss_raw), "\n")
if (nrow(ss_raw) > 0) {
  cat("   session_summary columns:", paste(colnames(ss_raw), collapse = ", "), "\n")
  if ("org_name" %in% colnames(ss_raw)) cat("   Unique org_name in summary:", length(unique(ss_raw$org_name)), "\n")
  cat("   session_id sample:", head(ss_raw$session_id, 2), "\n\n")
}

# App's session_summary_data: apply date range and org/group (All = no filter)
dr <- c(Sys.Date() - 1095, Sys.Date())
sel_org <- "All"
sel_grp <- "All"
ss <- ss_raw
if (nrow(ss) == 0) { cat("   [Session summary is empty - stopping]\n"); quit(save = "no", status = 1) }
date_ok <- length(dr) >= 2 && !is.na(dr[1]) && !is.na(dr[2])
if (date_ok) {
  keep_date <- is.na(ss$date) | (ss$date >= dr[1] & ss$date <= dr[2])
  if (sum(keep_date) > 0) ss <- ss[keep_date, , drop = FALSE]
}
if (nrow(ss) > 0 && sel_org != "All" && "org_name" %in% colnames(ss)) {
  ss_org <- trimws(as.character(ss$org_name))
  keep_org <- is.na(ss_org) | ss_org == "" | ss_org == trimws(sel_org)
  if (sum(keep_org) > 0) ss <- ss[keep_org, , drop = FALSE]
}
if (nrow(ss) > 0 && sel_grp != "All" && "group" %in% colnames(ss)) {
  ss_grp <- trimws(as.character(ss$group))
  keep_grp <- is.na(ss_grp) | ss_grp == "" | ss_grp == trimws(sel_grp)
  if (sum(keep_grp) > 0) ss <- ss[keep_grp, , drop = FALSE]
}

cat("3. After app date/org/group filter (All org, All group, default date range):\n")
cat("   Session summary rows:", nrow(ss), "\n\n")

# Simulate filtered_pre: restrict pre to session_ids in ss, then org/group
data <- pre_data
if (nrow(data) == 0) { cat("4. filtered_pre: 0 (no pre data)\n"); quit(save = "no", status = 0) }
if (nrow(ss) > 0 && "session_id" %in% colnames(ss) && "session_id" %in% colnames(data)) {
  data_sid <- trimws(as.character(data$session_id))
  ss_sid <- trimws(as.character(ss$session_id))
  match_pre <- data_sid %in% ss_sid & !is.na(data_sid) & data_sid != ""
  n_before <- nrow(data)
  if (any(match_pre)) data <- data[match_pre, , drop = FALSE]
  cat("4. filtered_pre (after session_id in summary):\n")
  cat("   Pre rows before session_id filter:", n_before, "| after:", nrow(data), "\n")
  cat("   Pre session_ids in summary?", any(match_pre), "| match count:", sum(match_pre), "\n")
  if (!any(match_pre) && n_before > 0) {
    cat("   MISMATCH: Pre session_ids not in summary. Sample Pre session_id:", data_sid[1], "\n")
    cat("   Sample summary session_id:", head(ss_sid, 2), "\n")
  }
} else {
  cat("4. filtered_pre: no session_id filter applied (ss empty or no session_id col)\n")
}
if (nrow(data) > 0 && sel_org != "All" && "org_name" %in% colnames(data)) {
  data_org <- trimws(as.character(data$org_name))
  match_org <- !is.na(data_org) & data_org == trimws(sel_org)
  if (sum(match_org) > 0) data <- data[match_org, , drop = FALSE]
}
if (nrow(data) > 0 && sel_grp != "All" && "group" %in% colnames(data)) {
  data_grp <- trimws(as.character(data$group))
  match_grp <- !is.na(data_grp) & data_grp == trimws(sel_grp)
  if (sum(match_grp) > 0) data <- data[match_grp, , drop = FALSE]
}
cat("   Final simulated filtered_pre rows:", nrow(data), "\n\n")

# Organization underway/completed (Overview tab)
if (nrow(ss) > 0 && "post_responses" %in% colnames(ss)) {
  org_clean <- ifelse(is.na(ss$org_name) | ss$org_name == "", "Unknown Organization", ss$org_name)
  grp <- dplyr::coalesce(as.character(ss$group), "")
  series_key <- paste(org_clean, grp, sep = "||")
  has_post <- ss %>% dplyr::mutate(org_clean = org_clean, grp = grp, key = series_key) %>%
    dplyr::group_by(key) %>% dplyr::mutate(series_has_post = any(post_responses > 0, na.rm = TRUE)) %>% dplyr::ungroup()
  ss_underway <- ss[!has_post$series_has_post, , drop = FALSE]
  ss_completed <- ss[has_post$series_has_post, , drop = FALSE]
  journey_underway <- tryCatch(calculate_organization_journeys(ss_underway), error = function(e) data.frame())
  journey_completed <- tryCatch(calculate_organization_journeys(ss_completed), error = function(e) data.frame())
  cat("5. Overview tab (Series by Org):\n")
  cat("   Underway (no Post) summary rows:", nrow(ss_underway), "| journey table rows:", nrow(journey_underway), "\n")
  cat("   Completed (has Post) summary rows:", nrow(ss_completed), "| journey table rows:", nrow(journey_completed), "\n")
}

cat("\nDone.\n")
