# language_filter.R — Master `language` column filter (es / zh / blank = English)

LANGUAGE_FILTER_LABELS <- c(
  "All" = "All languages",
  "en" = "English",
  "es" = "Spanish",
  "zh" = "Chinese"
)

language_filter_choices <- function(pre_df, post_df) {
  codes <- c("en", "es", "zh")
  present <- character(0)
  for (d in list(pre_df, post_df)) {
    if (!is.null(d) && nrow(d) > 0 && "language" %in% colnames(d)) {
      lv <- trimws(tolower(as.character(d$language)))
      lv <- lv[!is.na(lv) & nzchar(lv)]
      present <- unique(c(present, lv))
    }
  }
  # English always offered (blank/NA language on Pre rows = English monolingual forms)
  codes <- unique(c("en", intersect(codes, present)))
  setNames(codes, unname(LANGUAGE_FILTER_LABELS[codes]))
}

#' Filter rows by sidebar language selection.
#' Blank/NA `language` = English (standard Pre form and most Post rows).
apply_language_filter_rows <- function(data, selected) {
  if (is.null(data) || nrow(data) == 0) return(data)
  if (is.null(selected) || selected == "All" || !"language" %in% colnames(data)) {
    return(data)
  }
  lang <- trimws(tolower(as.character(data$language)))
  sel <- trimws(tolower(as.character(selected)))
  if (sel == "en") {
    keep <- is.na(lang) | lang == "" | lang == "en"
  } else {
    keep <- !is.na(lang) & lang == sel
  }
  data[keep, , drop = FALSE]
}

#' Filter Master rows by sidebar date range (submission timestamp).
apply_master_date_filter_rows <- function(data, use_date, date_range) {
  if (!isTRUE(use_date) || is.null(data) || nrow(data) == 0) return(data)
  if (is.null(date_range) || length(date_range) < 2L) return(data)
  start_date <- as.Date(date_range[1])
  end_date <- as.Date(date_range[2])
  if (any(is.na(c(start_date, end_date)))) return(data)
  ts_col <- if (exists("overview_ts_col", mode = "function")) overview_ts_col(data) else NA_character_
  if (is.na(ts_col) || !nzchar(ts_col)) return(data)
  ts <- if (exists("overview_parse_ts", mode = "function")) {
    overview_parse_ts(data[[ts_col]])
  } else {
    suppressWarnings(as.POSIXct(data[[ts_col]], tz = "America/Los_Angeles"))
  }
  d <- as.Date(ts, tz = if (exists("MASTER_TIMESTAMP_TZ")) MASTER_TIMESTAMP_TZ else "America/Los_Angeles")
  keep <- !is.na(d) & d >= start_date & d <= end_date
  if (!any(keep)) return(data[0, , drop = FALSE])
  data[keep, , drop = FALSE]
}
