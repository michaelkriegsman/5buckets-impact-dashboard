#!/usr/bin/env Rscript
# Headless twin of sidebar "Export for AI (ZIP)" (full load, raw+map on).

args <- commandArgs(trailingOnly = TRUE)
out_zip <- if (length(args) >= 1) args[[1]] else "tmp_exports/5buckets_ai_export.zip"
out_dir <- if (length(args) >= 2) args[[2]] else "tmp_exports/sample_export"

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  setwd(normalizePath(file.path(dirname(file_arg), "..")))
}

message("cwd: ", getwd())
source("load_sources.R")

message("Loading Master / PM sheets…")
pre <- load_master_pre()
post <- load_master_post()
annual <- load_master_annual()
pm_combined <- combine_program_managers_for_typing(
  load_program_manager(),
  load_mercy_program_manager()
)
repaired <- repair_masters_with_pm(pre, post, pm_combined, alert = TRUE)
pre <- exclude_test_rows(repaired$pre)
post <- exclude_test_rows(repaired$post)
pm <- repaired$pm_for_typing
message(
  "rows pre=", nrow(pre), " post=", nrow(post), " annual=", nrow(annual),
  " pm=", nrow(pm), " repaired=", repaired$repaired_n,
  " test_excluded_from_raw_counts≈", repaired$test_n,
  " irreparable=", nrow(repaired$irreparable)
)

pre_typed <- identify_survey_type(pre, pm)
post_typed <- identify_survey_type(post, pm)

big_pre <- dplyr::filter(pre_typed, .data$is_big_pre == TRUE)
little_pre <- dplyr::filter(pre_typed, .data$is_big_pre == FALSE)
big_post <- dplyr::filter(post_typed, .data$is_big_post == TRUE)
little_post <- dplyr::filter(post_typed, .data$is_big_post == FALSE)

message(
  "Mercy big_post=", sum(grepl("MERCY", big_post$session_id), na.rm = TRUE),
  " ARISE Schoool left=", sum(grepl("Schoool|SCHOOOL", c(pre$org_name, post$org_name, pre$session_id, post$session_id))),
  " 1969 left=", sum(grepl("^1969", c(pre$session_id, post$session_id)))
)

snap <- list(
  exported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  org = character(0),
  group = character(0),
  language = "All",
  date_enabled = FALSE,
  export_sections = unname(EXPORT_SECTION_CHOICES),
  export_include_figures = FALSE,
  export_include_raw_and_map = TRUE
)

ss <- tryCatch(
  create_session_summary_from_master(pre_typed, post_typed, pm),
  error = function(e) data.frame()
)

bundle <- list(
  snap = snap,
  slug = "full",
  sections = unname(EXPORT_SECTION_CHOICES),
  include_figures = FALSE,
  include_figure_recipes = TRUE,
  pre_all = pre_typed,
  post_all = post_typed,
  annual = annual,
  big_pre = big_pre,
  little_pre = little_pre,
  little_post = little_post,
  big_post = big_post,
  session_summary = ss,
  indices = list(
    wellness = tryCatch(calculate_wellness_index(big_pre), error = function(e) numeric(0)),
    behavioral = tryCatch(calculate_behavioral_index(big_pre), error = function(e) numeric(0)),
    post_impact = tryCatch(calculate_post_impact_index(big_post), error = function(e) numeric(0)),
    planned = tryCatch(calculate_planned_actions_index(big_post), error = function(e) numeric(0)),
    quality = tryCatch(calculate_quality_index(big_post), error = function(e) numeric(0))
  ),
  past_counts = tryCatch(get_past_behaviors_counts(big_pre), error = function(e) data.frame()),
  planned_counts = tryCatch(get_planned_actions_counts(big_post), error = function(e) data.frame()),
  nps = list(nps = NA_real_),
  pairing = NULL,
  org_journeys = data.frame()
)

dir.create(dirname(out_zip), showWarnings = FALSE, recursive = TRUE)
message("Writing ZIP → ", out_zip)
.write_ai_export_zip(normalizePath(out_zip, mustWork = FALSE), bundle, include_raw_and_map = TRUE)

# Extract for inspection
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)
utils::unzip(out_zip, exdir = out_dir)
message("Extracted to ", out_dir, ":")
print(list.files(out_dir))
