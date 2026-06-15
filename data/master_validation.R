# master_validation.R — Master sheet read health, cohort (new handler vs ported), column coverage

if (!exists("MASTER_TIMESTAMP_TZ")) {
  MASTER_TIMESTAMP_TZ <- Sys.getenv("MASTER_TIMESTAMP_TZ", unset = "America/Los_Angeles")
}

#' Column names from question_mapping_feb2026.R for cohort vs legacy comparison (only cols present in sheet are kept).
master_dq_watch_columns <- function() {
  p <- "data/question_mapping.R"
  if (!file.exists(p)) return(MASTER_EXPECTED_PRE_META)
  e <- new.env(parent = emptyenv())
  tryCatch(
    {
      sys.source(p, envir = e)
      unique(c(
        e$PRE_METADATA_COLS,
        e$PRE_OPENING_COLS,
        e$PRE_FINANCIAL_WELLNESS_COLS,
        e$PRE_PAST_BEHAVIORS_COL,
        e$PRE_DEMOGRAPHICS_COLS,
        e$PRE_SUPPORT_COL,
        e$PRE_ADDITIONAL_COMMENTS_COL,
        e$POST_TODAY_SESSION_COLS,
        e$POST_COMPARED_TO_COLS,
        e$POST_PLANNED_ACTIONS_COL,
        e$POST_IMPACT_STORY_COL,
        e$POST_NPS_COL,
        e$POST_KEEP_IN_TOUCH_COL,
        e$POST_DEMOGRAPHICS_COLS,
        e$POST_ADDITIONAL_COMMENTS_COL
      ))
    },
    error = function(...) MASTER_EXPECTED_PRE_META
  )
}

#' Per-cohort non-empty % for selected columns (spot legacy rows missing req-parsed fields, etc.).
column_fill_by_cohort <- function(d, tab_label, columns) {
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(
      tab = tab_label, column = character(0), cohort = character(0),
      n = integer(0), non_empty_pct = numeric(0), stringsAsFactors = FALSE
    ))
  }
  cohort <- guess_master_row_cohort(d)
  columns <- unique(intersect(columns, names(d)))
  if (!length(columns)) {
    return(data.frame(
      tab = tab_label, column = character(0), cohort = character(0),
      n = integer(0), non_empty_pct = numeric(0), stringsAsFactors = FALSE
    ))
  }
  uc <- unique(cohort)
  rows <- list()
  for (cn in columns) {
    ne <- non_empty_cell(d[[cn]])
    for (coh in uc) {
      i <- cohort == coh
      n <- sum(i)
      po <- if (n > 0) sum(ne[i], na.rm = TRUE) else 0L
      pct <- if (n > 0) round(100 * po / n, 1) else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        tab = tab_label,
        column = cn,
        cohort = coh,
        n = as.integer(n),
        non_empty_pct = pct,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

master_find_ts_col <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NA_character_)
  nms <- names(d)
  if ("timestamp_parsed" %in% nms && any(!is.na(d$timestamp_parsed))) return("timestamp_parsed")
  for (nm in c("timestamp", "Timestamp")) {
    if (nm %in% nms) return(nm)
  }
  hit <- grep("^timestamp$", nms, ignore.case = TRUE, value = TRUE)
  if (length(hit)) return(hit[1])
  NA_character_
}

non_empty_cell <- function(x) {
  if (is.null(x)) return(logical(0))
  if (inherits(x, "list")) {
    return(vapply(x, function(z) {
      if (is.null(z) || length(z) == 0) return(FALSE)
      if (is.list(z) && length(z) > 0) z <- z[[1]]
      if (length(z) > 1) return(TRUE)
      !is.na(z) && nzchar(trimws(as.character(z)))
    }, logical(1)))
  }
  !is.na(x) & nzchar(trimws(as.character(x)))
}

#' Guess row cohort: Feb 2026 handlers usually have `req` (zip | demog); ported rows often leave it blank.
guess_master_row_cohort <- function(d) {
  n <- nrow(d)
  if (!n) return(character(0))
  out <- rep("unknown", n)
  if ("req" %in% names(d)) {
    r <- trimws(as.character(d$req))
    out[!is.na(r) & nzchar(r)] <- "feb2026_handler"
    out[is.na(r) | !nzchar(r)] <- "legacy_or_ported_missing_req"
  } else {
    out[] <- "no_req_column"
  }
  if ("Source" %in% names(d)) {
    s <- tolower(trimws(as.character(d$Source)))
    hit <- grepl("port|import|migrat|legacy|copy", s)
    out[hit] <- "source_suggests_ported"
  }
  out
}

column_read_stats <- function(d, tab_label) {
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(tab = tab_label, column = character(0), n_rows = integer(0),
                      non_empty_pct = numeric(0), is_list_col = logical(0), stringsAsFactors = FALSE))
  }
  n <- nrow(d)
  cols <- names(d)
  data.frame(
    tab = tab_label,
    column = cols,
    n_rows = n,
    non_empty_pct = vapply(cols, function(cn) {
      round(100 * sum(non_empty_cell(d[[cn]]), na.rm = TRUE) / n, 1)
    }, numeric(1)),
    is_list_col = vapply(cols, function(cn) is.list(d[[cn]]), logical(1)),
    stringsAsFactors = FALSE
  )
}

#' Rows with non-empty raw timestamp but NA after parse; top distinct strings + pattern fingerprint.
timestamp_parse_failure_samples <- function(d, tab_label, max_n = 25L) {
  empty <- data.frame(
    tab = character(0), raw_sample = character(0), n_rows = integer(0), pattern = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(d) || nrow(d) == 0) return(empty)
  tc <- master_find_ts_col(d)
  if (is.na(tc)) return(empty)
  raw_vec <- if ("timestamp" %in% names(d)) {
    d$timestamp
  } else if ("Timestamp" %in% names(d)) {
    d$Timestamp
  } else {
    d[[tc]]
  }
  tp <- parse_master_timestamp_inclusive(raw_vec)
  raw_chr <- trimws(as.character(raw_vec))
  nonempty <- !is.na(raw_chr) & nzchar(raw_chr) & !toupper(raw_chr) %in% c("", "NA")
  is_fail <- nonempty & is.na(tp)
  if (!any(is_fail)) return(empty)
  bad <- raw_chr[is_fail]
  u <- unique(bad)
  n <- vapply(u, function(s) sum(bad == s, na.rm = TRUE), integer(1))
  o <- order(-n)
  u <- u[o]
  n <- n[o]
  take <- seq_len(min(length(u), max_n))
  data.frame(
    tab = tab_label,
    raw_sample = u[take],
    n_rows = as.integer(n[take]),
    pattern = vapply(u[take], timestamp_format_fingerprint, character(1)),
    stringsAsFactors = FALSE
  )
}

timestamp_parse_stats <- function(d, tab_label) {
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(tab = tab_label, cohort = character(0), n = integer(0), parsed_ok = integer(0),
                      parse_rate_pct = numeric(0), stringsAsFactors = FALSE))
  }
  cohort <- guess_master_row_cohort(d)
  tc <- master_find_ts_col(d)
  if (!is.na(tc) && tc == "timestamp_parsed") {
    tp <- d$timestamp_parsed
  } else if (!is.na(tc)) {
    tp <- parse_master_timestamp_inclusive(d[[tc]])
  } else {
    tp <- rep(as.POSIXct(NA_real_, tz = MASTER_TIMESTAMP_TZ), nrow(d))
  }
  ok <- !is.na(tp)
  ucoh <- unique(cohort)
  rows <- lapply(ucoh, function(coh) {
    i <- cohort == coh
    n <- sum(i)
    po <- sum(ok[i], na.rm = TRUE)
    data.frame(
      tab = tab_label,
      cohort = coh,
      n = as.integer(n),
      parsed_ok = as.integer(po),
      parse_rate_pct = if (n > 0) round(100 * po / n, 1) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

expected_cols_check <- function(d, expected, tab_label) {
  missing <- setdiff(expected, names(d))
  data.frame(
    tab = tab_label,
    n_missing = length(missing),
    missing_expected = if (length(missing)) paste(missing, collapse = "; ") else "(none)",
    stringsAsFactors = FALSE
  )
}

# Keep aligned with data/question_mapping_feb2026.R PRE_METADATA_COLS
MASTER_EXPECTED_PRE_META <- c(
  "timestamp", "session_id", "org_name", "group", "facilitators", "modules_taught",
  "session_date", "session_start_time", "qa_session_found", "qa_metadata_changed",
  "respondent_id", "is_anonymous", "req", "first_name", "last_name",
  "respondent_name", "respondent_email"
)

#' Full report for Data quality tab
master_data_quality_report <- function(pre, post) {
  watch <- master_dq_watch_columns()
  ts_pre <- if (nrow(pre) > 0 && "timestamp_parsed" %in% names(pre)) {
    sum(!is.na(pre$timestamp_parsed))
  } else {
    NA_integer_
  }
  ts_post <- if (nrow(post) > 0 && "timestamp_parsed" %in% names(post)) {
    sum(!is.na(post$timestamp_parsed))
  } else {
    NA_integer_
  }

  summary_html <- paste0(
    "<p><strong>Rows loaded:</strong> Pre ", nrow(pre), " | Post ", nrow(post), "</p>",
    "<p><strong>Timestamp parsed (<code>timestamp_parsed</code>):</strong> Pre ", ts_pre, " / ", nrow(pre),
    " | Post ", ts_post, " / ", nrow(post), "</p>",
    "<p><strong>Cohort legend:</strong> <code>feb2026_handler</code> = row has <code>req</code> set (zip|demog). ",
    "<code>legacy_or_ported_missing_req</code> = blank <code>req</code> (typical for some migrated rows). ",
    "Compare <strong>parse_rate_pct</strong> (timestamps) and <strong>non_empty_pct</strong> (variables) by cohort to spot migrated rows that are not reading cleanly.</p>"
  )

  list(
    summary_html = summary_html,
    cohort_pre = timestamp_parse_stats(pre, "Pre Submissions"),
    cohort_post = timestamp_parse_stats(post, "Post Submissions"),
    ts_fail_pre = timestamp_parse_failure_samples(pre, "Pre Submissions"),
    ts_fail_post = timestamp_parse_failure_samples(post, "Post Submissions"),
    cohort_fill_pre = column_fill_by_cohort(pre, "Pre Submissions", watch),
    cohort_fill_post = column_fill_by_cohort(post, "Post Submissions", watch),
    cols_pre = column_read_stats(pre, "Pre Submissions"),
    cols_post = column_read_stats(post, "Post Submissions"),
    expected_pre = expected_cols_check(pre, MASTER_EXPECTED_PRE_META, "Pre"),
    expected_post = expected_cols_check(post, MASTER_EXPECTED_PRE_META, "Post")
  )
}
