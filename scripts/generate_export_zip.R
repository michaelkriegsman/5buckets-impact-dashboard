#!/usr/bin/env Rscript
# Headless twin of the sidebar "Filtered data (CSV ZIP)" button (all-filters / full load).
# Writes ZIP + extracted CSVs under tmp_exports/ for Data Map tuning.

args <- commandArgs(trailingOnly = TRUE)
out_zip <- if (length(args) >= 1) args[[1]] else "tmp_exports/5buckets_data_full.zip"
out_dir <- if (length(args) >= 2) args[[2]] else "tmp_exports/sample_export"

suppressPackageStartupMessages({
  # Match app load path
})

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
pm <- combine_program_managers_for_typing(
  load_program_manager(),
  load_mercy_program_manager()
)
message("rows pre=", nrow(pre), " post=", nrow(post), " annual=", nrow(annual), " pm=", nrow(pm))

pre_typed <- identify_survey_type(pre, pm)
post_typed <- identify_survey_type(post, pm)

big_pre <- dplyr::filter(pre_typed, .data$is_big_pre == TRUE)
little_pre <- dplyr::filter(pre_typed, .data$is_big_pre == FALSE)
big_post <- dplyr::filter(post_typed, .data$is_big_post == TRUE)
little_post <- dplyr::filter(post_typed, .data$is_big_post == FALSE)

# Minimal snap mimicking empty sidebar filters
snap <- list(
  exported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  org = character(0),
  group = character(0),
  language = "All",
  date_enabled = FALSE,
  export_sections = unname(EXPORT_SECTION_CHOICES),
  export_include_figures = FALSE
)

bundle <- list(
  snap = snap,
  slug = "full",
  sections = unname(EXPORT_SECTION_CHOICES),
  include_figures = FALSE,
  pre_all = pre_typed,
  post_all = post_typed,
  annual = annual,
  big_pre = big_pre,
  little_pre = little_pre,
  little_post = little_post,
  big_post = big_post,
  session_summary = data.frame(),
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
.write_filtered_data_zip(normalizePath(out_zip, mustWork = FALSE), bundle)

if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)
utils::unzip(out_zip, exdir = out_dir)
message("Extracted to ", out_dir)
csvs <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
for (f in csvs) {
  d <- tryCatch(utils::read.csv(f, nrows = 0, check.names = FALSE), error = function(e) NULL)
  if (!is.null(d)) message("  ", basename(f), ": ", ncol(d), " cols")
}
message("Done.")
