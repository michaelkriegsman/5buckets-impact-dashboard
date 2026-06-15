# master_timestamp_parse.R — inclusive Master timestamp parsing for mixed migrated / new-sheet rows
# Used by global.R normalize_master_cols and overview_timeline.R.
#
# Feb 2026 Master: Apps Script repair + Pre/Post handlers store **plain text**
# `yyyy-MM-dd HH:mm:ss` in **America/Los_Angeles** (Pacific; see Survey Generator REPAIR_MASTER_TIMESTAMP_FORMAT_README.md).
# Override with env MASTER_TIMESTAMP_TZ if needed.

MASTER_TIMESTAMP_TZ <- Sys.getenv("MASTER_TIMESTAMP_TZ", unset = "America/Los_Angeles")
# Back-compat alias for any code still referencing LA_TZ
LA_TZ <- MASTER_TIMESTAMP_TZ

#' Strip milliseconds / microseconds for parsers that choke on fractional seconds.
strip_fractional_seconds_ <- function(x) {
  x <- sub("\\.\\d{1,6}(?=\\s*[+-Z]|$)", "", x, perl = TRUE)
  x
}

#' Normalize common variants from Sheets / exports (NBSP, T→space, odd dashes).
normalize_timestamp_string_ <- function(xc) {
  xc <- trimws(as.character(xc))
  if (!nzchar(xc)) return(xc)
  xc <- gsub("\u00a0", " ", xc, fixed = TRUE)
  xc <- gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2})T", "\\1 ", xc)
  xc <- gsub("[\u2013\u2014]", "-", xc)
  xc <- gsub("\\s+", " ", xc)
  xc
}

#' Core string → POSIXct (LA). Tries many real-world formats seen in Google Sheets + migrations.
parse_string_to_posix_la_ <- function(xc) {
  xc <- normalize_timestamp_string_(xc)
  if (length(xc) == 0L || is.na(xc) || !nzchar(xc) || toupper(xc) == "NA") {
    return(as.POSIXct(NA_real_, tz = MASTER_TIMESTAMP_TZ))
  }

  # --- Live handler format: M/d/yyyy HH:mm:ss (Pacific wall time, e.g. 5/19/2026 14:33:32) ---
  if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{2}:[0-9]{2}$", xc)) {
    pt_mdy <- tryCatch(
      lubridate::parse_date_time(xc, orders = "mdy HMS", tz = MASTER_TIMESTAMP_TZ, quiet = TRUE, truncated = 3),
      error = function(e) NA
    )
    if (inherits(pt_mdy, "POSIXct") && length(pt_mdy) >= 1L && !is.na(pt_mdy[1])) {
      return(suppressWarnings(lubridate::force_tz(pt_mdy[1], tzone = MASTER_TIMESTAMP_TZ)))
    }
  }

  # --- Canonical Master text: yyyy-MM-dd HH:mm:ss (Pacific wall time, no offset) ---
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{1,2}:[0-9]{2}:[0-9]{2}$", xc)) {
    pt0 <- tryCatch(lubridate::ymd_hms(xc, tz = MASTER_TIMESTAMP_TZ), error = function(e) NA)
    if (inherits(pt0, "POSIXct") && length(pt0) >= 1L && !is.na(pt0[1])) {
      return(suppressWarnings(lubridate::force_tz(pt0[1], tzone = MASTER_TIMESTAMP_TZ)))
    }
  }

  # --- Plain Excel serial as character (e.g. "44927.641") ---
  if (grepl("^[0-9]+\\.?[0-9]*$", xc)) {
    v <- suppressWarnings(as.numeric(xc))
    if (!is.na(v) && v > 20000 && v < 60000) {
      origin <- as.Date("1899-12-30")
      return(as.POSIXct(origin + v, tz = MASTER_TIMESTAMP_TZ))
    }
    if (!is.na(v) && v > 1e9 && v < 1e11) {
      return(lubridate::with_tz(as.POSIXct(v, origin = "1970-01-01", tz = "UTC"), tzone = LA_TZ))
    }
    if (!is.na(v) && v > 1e11 && v < 1e14) {
      return(lubridate::with_tz(as.POSIXct(v / 1000, origin = "1970-01-01", tz = "UTC"), tzone = LA_TZ))
    }
  }

  # --- ISO / RFC with Z (UTC) ---
  if (grepl("Z\\s*$", xc, ignore.case = TRUE)) {
    xc_z <- sub("\\s*Z\\s*$", "", xc, ignore.case = TRUE)
    xc_z <- strip_fractional_seconds_(xc_z)
    pt <- tryCatch(
      lubridate::parse_date_time(
        xc_z,
        orders = c("Ymd HMS", "Ymd HM", "ymd HMS", "ymd HM", "Ymd", "ymd"),
        tz = "UTC",
        quiet = TRUE,
        truncated = 3
      ),
      error = function(e) NA
    )
    if (inherits(pt, "POSIXct") && !is.na(pt[1])) {
      return(lubridate::with_tz(pt, tzone = LA_TZ))
    }
  }

  # --- ISO-like with numeric offset (e.g. ...-07:00) — let lubridate infer, then LA ---
  if (grepl("[+-][0-9]{2}:?[0-9]{2}\\s*$", xc)) {
    pt2 <- tryCatch(lubridate::parse_date_time(xc, orders = c("Ymd HMS", "ymd HMS", "Ymd HM", "ymd HM"), tz = LA_TZ, quiet = TRUE, truncated = 3),
      error = function(e) NA
    )
    if (inherits(pt2, "POSIXct") && length(pt2) >= 1L && !is.na(pt2[1])) {
      return(suppressWarnings(lubridate::force_tz(pt2[1], tzone = LA_TZ)))
    }
    pt3 <- tryCatch(lubridate::ymd_hms(xc, tz = LA_TZ), error = function(e) NA)
    if (inherits(pt3, "POSIXct") && length(pt3) >= 1L && !is.na(pt3[1])) {
      return(suppressWarnings(lubridate::force_tz(pt3[1], tzone = LA_TZ)))
    }
  }

  xc2 <- strip_fractional_seconds_(xc)

  # --- lubridate: US/EU/ISO; I = 12h clock + p = AM/PM; H = 24h ---
  lub_orders <- c(
    "mdy IMS p", "mdy IM p", "mdy HMS p", "mdy HM p",
    "mdy IMS", "mdy IM", "mdy HMS", "mdy HM", "mdy",
    "dmy IMS p", "dmy IM p", "dmy HMS", "dmy HM", "dmy",
    "ymd IMS p", "ymd IM p", "ymd HMS", "ymd HM", "ymd",
    "Ymd IMS p", "Ymd IM p", "Ymd HMS", "Ymd HM", "Ymd"
  )
  parsed <- tryCatch(
    lubridate::parse_date_time(
      xc2,
      orders = lub_orders,
      tz = LA_TZ,
      quiet = TRUE,
      truncated = 3
    ),
    error = function(e) NA
  )
  if (inherits(parsed, "POSIXct") && length(parsed) >= 1L && !is.na(parsed[1])) {
    return(suppressWarnings(lubridate::force_tz(parsed[1], tzone = LA_TZ)))
  }

  # --- strptime: 12h AM/PM (very common in Form → Sheet exports) ---
  fmts <- c(
    "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y/%m/%d",
    "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%m/%d/%y %I:%M:%S %p", "%m/%d/%y %I:%M %p", "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M", "%m/%d/%y",
    "%m-%d-%Y %I:%M:%S %p", "%m-%d-%Y %I:%M %p", "%m-%d-%Y %H:%M:%S", "%m-%d-%Y",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
    "%B %d, %Y %I:%M:%S %p", "%B %d, %Y %I:%M %p", "%B %d, %Y",
    "%b %d, %Y %I:%M:%S %p", "%b %d, %Y %I:%M %p", "%b %d, %Y",
    "%B %d %Y %I:%M:%S %p", "%b %d %Y %I:%M %p",
    "%b %d %Y %I:%M:%S %p", "%b %d %Y %I:%M %p"
  )
  for (fmt in fmts) {
    p2 <- tryCatch(strptime(xc2, format = fmt), error = function(e) NULL)
    if (!inherits(p2, "POSIXlt")) next
    if (length(p2) < 1L) next
    if (is.na(p2[1])) next
    return(as.POSIXct(p2[1], tz = LA_TZ))
  }

  # --- Date-only → midnight LA ---
  d_only <- suppressWarnings(tryCatch(as.Date(xc2), error = function(e) NA))
  if (inherits(d_only, "Date") && length(d_only) >= 1L && !is.na(d_only[1])) {
    return(as.POSIXct(d_only[1], tz = LA_TZ))
  }
  pd <- tryCatch(
    lubridate::parse_date_time(xc2, orders = c("mdy", "ymd", "dmy", "Ymd"), tz = LA_TZ, quiet = TRUE, truncated = 1),
    error = function(e) NA
  )
  if (inherits(pd, "POSIXct") && length(pd) >= 1L && !is.na(pd[1])) {
    return(suppressWarnings(lubridate::force_tz(pd[1], tzone = LA_TZ)))
  }

  # --- Last resort: base R heuristic (some locale-specific exports) ---
  pt0 <- suppressWarnings(tryCatch(as.POSIXct(xc2, tz = LA_TZ), error = function(e) NA))
  if (inherits(pt0, "POSIXct") && length(pt0) >= 1L && !is.na(pt0[1])) {
    return(suppressWarnings(lubridate::force_tz(pt0[1], tzone = LA_TZ)))
  }

  as.POSIXct(NA_real_, tz = LA_TZ)
}

parse_one_master_timestamp <- function(z) {
  if (is.null(z) || (length(z) == 0)) {
    return(as.POSIXct(NA_real_, tz = LA_TZ))
  }
  if (inherits(z, "list") && length(z) > 0) z <- z[[1]]
  if (is.null(z) || (length(z) == 1 && is.na(z))) {
    return(as.POSIXct(NA_real_, tz = LA_TZ))
  }
  if (inherits(z, "POSIXct")) {
    return(suppressWarnings(lubridate::force_tz(z, tzone = LA_TZ)))
  }
  if (inherits(z, "POSIXlt")) {
    return(as.POSIXct(z, tz = LA_TZ))
  }
  if (inherits(z, "Date")) {
    return(as.POSIXct(as.character(z), tz = LA_TZ))
  }
  if (is.numeric(z)) {
    if (length(z) != 1) z <- z[[1]]
    if (is.na(z)) {
      return(as.POSIXct(NA_real_, tz = LA_TZ))
    }
    if (z > 20000 && z < 60000) {
      origin <- as.Date("1899-12-30")
      return(as.POSIXct(origin + z, tz = LA_TZ))
    }
    if (z > 1e9 && z < 1e11) {
      return(lubridate::with_tz(as.POSIXct(z, origin = "1970-01-01", tz = "UTC"), tzone = LA_TZ))
    }
    if (z > 1e11 && z < 1e14) {
      return(lubridate::with_tz(as.POSIXct(z / 1000, origin = "1970-01-01", tz = "UTC"), tzone = LA_TZ))
    }
    return(parse_string_to_posix_la_(as.character(z)))
  }
  parse_string_to_posix_la_(z)
}

#' Vectorized inclusive Master timestamp parse (POSIXct in MASTER_TIMESTAMP_TZ, default America/Los_Angeles).
parse_master_timestamp_inclusive <- function(x) {
  if (is.null(x)) {
    return(as.POSIXct(numeric(0), tz = LA_TZ))
  }
  n <- length(x)
  if (inherits(x, "POSIXct")) {
    return(suppressWarnings(lubridate::force_tz(x, tzone = LA_TZ)))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(as.character(x), tz = LA_TZ))
  }
  if (inherits(x, "list")) {
    x <- vapply(seq_len(n), function(i) {
      z <- x[[i]]
      if (is.null(z) || length(z) == 0) return(NA_character_)
      if (inherits(z, "POSIXct")) return(format(z, tz = LA_TZ, usetz = FALSE))
      if (inherits(z, "Date")) return(format(z))
      if (is.list(z) && length(z) > 0) z <- z[[1]]
      trimws(as.character(z))
    }, character(1))
  }
  if (inherits(x, "numeric") || inherits(x, "integer")) {
    out <- rep(as.POSIXct(NA_real_, tz = LA_TZ), n)
    for (i in seq_len(n)) {
      out[i] <- parse_one_master_timestamp(x[i])
    }
    return(out)
  }
  out <- rep(as.POSIXct(NA_real_, tz = LA_TZ), n)
  xc <- trimws(as.character(x))
  for (i in seq_len(n)) {
    out[i] <- parse_one_master_timestamp(xc[i])
  }
  out
}

#' Flatten timestamp column from googlesheets4 (preserve POSIXct meaning via LA string).
flatten_master_timestamp_col <- function(v) {
  if (!is.list(v)) {
    if (inherits(v, "POSIXct")) {
      return(format(v, tz = LA_TZ, usetz = FALSE))
    }
    if (inherits(v, "Date")) return(format(v))
    return(trimws(as.character(v)))
  }
  vapply(v, function(z) {
    if (is.null(z) || length(z) == 0) return(NA_character_)
    if (inherits(z, "POSIXct")) return(format(z, tz = LA_TZ, usetz = FALSE))
    if (inherits(z, "Date")) return(format(z))
    if (is.list(z) && length(z) > 0) z <- z[[1]]
    trimws(as.character(z))
  }, character(1))
}

#' Fingerprint a timestamp string for grouping (digits → 0, letters → A).
timestamp_format_fingerprint <- function(s) {
  s <- trimws(as.character(s))
  if (is.na(s) || !nzchar(s)) return("")
  x <- gsub("[0-9]", "0", s)
  x <- gsub("[A-Za-z]", "A", x)
  gsub("\\s+", " ", x)
}
