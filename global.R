# global.R - 5 Buckets Impact Dashboard
# Global variables, data loading functions, and shared utilities

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(googlesheets4)
library(lubridate)
library(htmltools)

source("data/master_timestamp_parse.R")

# Optional: US zip → lat/lon for Overview maps (install.packages("zipcode"))
ZIPCODE_DATA <- NULL
if (requireNamespace("zipcode", quietly = TRUE)) {
  ZIPCODE_DATA <- tryCatch({
    e <- new.env(parent = emptyenv(), hash = TRUE)
    utils::data(zipcode, package = "zipcode", envir = e)
    z <- e$zipcode
    z$zip5 <- sprintf("%05d", suppressWarnings(as.integer(z$zip)))
    data.frame(zip = z$zip5, lat = z$latitude, lon = z$longitude, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# Text analysis packages (optional - install if needed)
if (requireNamespace("wordcloud2", quietly = TRUE)) {
  library(wordcloud2)
} else {
  warning("wordcloud2 package not installed. Wordclouds will not be available.")
}

if (requireNamespace("tm", quietly = TRUE)) {
  library(tm)
} else {
  warning("tm package not installed. Text analysis will be limited.")
}

if (requireNamespace("wordcloud", quietly = TRUE)) {
  suppressPackageStartupMessages(library(wordcloud))
}

if (requireNamespace("SentimentAnalysis", quietly = TRUE)) {
  library(SentimentAnalysis)
} else {
  warning("SentimentAnalysis package not installed. Sentiment analysis will not be available.")
}

# ============================================================================
# Google Sheets Authentication
# ============================================================================

# Service Account (for production/shinyapps.io) or OAuth (for local development)
service_account_key <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY", unset = "")

if (service_account_key != "") {
  # Service account authentication (production/shinyapps.io)
  # The key can be either:
  # 1. A file path to a JSON key file
  # 2. The JSON content itself (as a string)
  
  if (file.exists(service_account_key)) {
    # It's a file path
    gs4_auth(path = service_account_key)
  } else if (grepl("^\\{", service_account_key)) {
    # It's JSON content (starts with {)
    # Write to temporary file
    key_file <- tempfile(fileext = ".json")
    writeLines(service_account_key, key_file)
    gs4_auth(path = key_file)
  } else {
    # Try as file path anyway (might be relative)
    tryCatch({
      gs4_auth(path = service_account_key)
    }, error = function(e) {
      warning("Service account authentication failed. Falling back to OAuth.")
      gs4_auth()  # Fallback to OAuth
    })
  }
} else {
  # OAuth authentication (for local development)
  # If token doesn't exist, will open browser for authentication
  # Token is cached and reused automatically
  gs4_auth()
}

# ============================================================================
# Sheet IDs - From Pre/Post Survey Flow Apps Scripts
# ============================================================================

# Master Pre Responses Sheet
# Feb 2026: Master Workbook - Feb 2026
MASTER_PRE_SHEET_ID <- Sys.getenv("MASTER_PRE_SHEET_ID", unset = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw")
MASTER_PRE_SHEET_NAME <- "Pre Submissions"

# Master Post Responses Sheet
# Feb 2026: Master Workbook - Feb 2026
MASTER_POST_SHEET_ID <- Sys.getenv("MASTER_POST_SHEET_ID", unset = "1nxVENReSAURQlXLdJ2eMcaLZ4NWYA4xNo7eTwp6EHWw")
MASTER_POST_SHEET_NAME <- "Post Submissions"

# Program Manager Sheet (Workshops)
# Feb 2026: Program Manager - Feb 2026
PROGRAM_MANAGER_SHEET_ID <- Sys.getenv("PROGRAM_MANAGER_SHEET_ID", unset = "1aefJFVQtfYx2UCd5u9q1pl0raJXVzXaW8pKlzZRMXuQ")
PROGRAM_MANAGER_SHEET_NAME <- "Workshops"

# Mercy custom-flow Program Manager (separate Workshops sheet; session_ids do not live on company PM)
MERCY_PROGRAM_MANAGER_SHEET_ID <- Sys.getenv(
  "MERCY_PROGRAM_MANAGER_SHEET_ID",
  unset = "12XH-SbHwHGYHVPSfx7DrShuUC18btE2XnTKMiilU8iI"
)
MERCY_PROGRAM_MANAGER_SHEET_NAME <- Sys.getenv(
  "MERCY_PROGRAM_MANAGER_SHEET_NAME",
  unset = "Workshops"
)

# Google Sheets URLs (for hyperlinks)
MASTER_PRE_URL <- paste0("https://docs.google.com/spreadsheets/d/", MASTER_PRE_SHEET_ID, "/edit")
MASTER_POST_URL <- paste0("https://docs.google.com/spreadsheets/d/", MASTER_POST_SHEET_ID, "/edit")
PROGRAM_MANAGER_URL <- paste0("https://docs.google.com/spreadsheets/d/", PROGRAM_MANAGER_SHEET_ID, "/edit")

# ============================================================================
# Data Loading Functions
# ============================================================================
# Org/group dropdowns read from session summary (from Master Pre/Post).
# Normalize column names so both "Organization"/"org_name" and "Group"/"group" work.

# Parse org and group from session_id. Handles: "YYYY-MM-DD_ORG_GROUP_..." or URL containing that pattern.
parse_session_id_org_group <- function(s) {
  if (is.na(s) || nchar(trimws(as.character(s))) == 0) return(list(org = NA_character_, group = NA_character_))
  s <- trimws(as.character(s))
  # If string contains a date-like segment (YYYY-MM-DD), use from that point so URLs still work
  date_match <- regexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", s)
  if (date_match > 0) {
    segment <- substr(s, date_match, nchar(s))
    parts <- strsplit(segment, "_", fixed = TRUE)[[1]]
  } else {
    parts <- strsplit(s, "_", fixed = TRUE)[[1]]
  }
  if (length(parts) < 2) return(list(org = NA_character_, group = NA_character_))
  org <- parts[2]
  grp <- NA_character_
  if (length(parts) >= 3 && !grepl("^[0-9]{4}$", parts[3])) grp <- parts[3]
  list(org = org, group = grp)
}

normalize_master_cols <- function(d) {
  if (nrow(d) == 0) return(d)
  nms <- colnames(d)
  nms_lower <- tolower(trimws(nms))
  # Map common alternate headers to expected names (case-insensitive)
  if (!"org_name" %in% nms) {
    org_candidates <- c("organization", "org name", "org_name", "organization name", "org")
    idx <- which(nms_lower %in% org_candidates)[1]
    if (!is.na(idx)) names(d)[idx] <- "org_name"
  }
  if (!"group" %in% nms) {
    idx <- which(nms_lower == "group")[1]
    if (!is.na(idx)) names(d)[idx] <- "group"
  }
  if (!"session_id" %in% nms) {
    idx <- which(nms_lower %in% c("session link", "session id", "session_id"))[1]
    if (!is.na(idx)) names(d)[idx] <- "session_id"
  }
  # Unify Timestamp / timestamp so flatten and time-based charts see one column name
  if (!"timestamp" %in% names(d)) {
    ts_idx <- which(nms_lower == "timestamp")[1]
    if (!is.na(ts_idx)) names(d)[ts_idx] <- "timestamp"
  }
  # Alternate headers seen on migrated / legacy exports
  if (!"timestamp" %in% names(d)) {
    for (alt in c("submission time", "date submitted", "submitted at", "created", "datetime", "time")) {
      idx <- which(nms_lower == alt)[1]
      if (!is.na(idx)) {
        names(d)[idx] <- "timestamp"
        break
      }
    }
  }
  # Flatten list-cols for key metadata so joins and dropdowns work (googlesheets4 can return lists)
  flatten_col <- function(v) {
    if (!is.list(v)) return(as.character(v))
    vapply(v, function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      if (is.list(x) && length(x) > 0) x <- x[[1]]
      trimws(as.character(x))
    }, character(1))
  }
  to_flatten <- c("session_id", "session_date", "session_start_time", "org_name", "group", "req", "Source")
  for (nm in intersect(to_flatten, colnames(d))) {
    d[[nm]] <- flatten_col(d[[nm]])
  }
  if ("timestamp" %in% colnames(d)) {
    d$timestamp <- flatten_master_timestamp_col(d$timestamp)
    d$timestamp_parsed <- parse_master_timestamp_inclusive(d$timestamp)
  }
  # Fallback: derive org_name and group from session_id if missing/empty (handles URLs with embedded date_org_group)
  if ("session_id" %in% colnames(d)) {
    sid <- as.character(d$session_id)
    need_org <- !"org_name" %in% colnames(d) || all(is.na(d$org_name) | trimws(as.character(d$org_name)) == "", na.rm = TRUE)
    need_grp <- !"group" %in% colnames(d) || all(is.na(d$group) | trimws(as.character(d$group)) == "", na.rm = TRUE)
    if (need_org || need_grp) {
      parsed <- lapply(sid, function(s) parse_session_id_org_group(s))
      if (need_org) d$org_name <- vapply(parsed, function(p) p$org, character(1))
      if (need_grp) d$group <- vapply(parsed, function(p) p$group, character(1))
    }
  }
  d
}

source("data/master_validation.R")

load_master_pre <- function() {
  if (MASTER_PRE_SHEET_ID == "") {
    warning("MASTER_PRE_SHEET_ID not set. Using empty data frame.")
    return(data.frame())
  }
  tryCatch({
    d <- read_sheet(MASTER_PRE_SHEET_ID, sheet = MASTER_PRE_SHEET_NAME)
    normalize_master_cols(d)
  }, error = function(e) {
    warning("Error loading Master Pre data: ", e$message)
    return(data.frame())
  })
}

load_master_post <- function() {
  if (MASTER_POST_SHEET_ID == "") {
    warning("MASTER_POST_SHEET_ID not set. Using empty data frame.")
    return(data.frame())
  }
  tryCatch({
    d <- read_sheet(MASTER_POST_SHEET_ID, sheet = MASTER_POST_SHEET_NAME)
    normalize_master_cols(d)
  }, error = function(e) {
    warning("Error loading Master Post data: ", e$message)
    return(data.frame())
  })
}

load_program_manager_sheet <- function(sheet_id, sheet_name, label = "Program Manager") {
  if (is.null(sheet_id) || !nzchar(sheet_id)) return(data.frame())
  tryCatch({
    read_sheet(sheet_id, sheet = sheet_name)
  }, error = function(e) {
    warning("Error loading ", label, ": ", e$message)
    data.frame()
  })
}

load_program_manager <- function() {
  if (PROGRAM_MANAGER_SHEET_ID == "") {
    warning("PROGRAM_MANAGER_SHEET_ID not set. Using empty data frame.")
    return(data.frame())
  }
  load_program_manager_sheet(PROGRAM_MANAGER_SHEET_ID, PROGRAM_MANAGER_SHEET_NAME, "Program Manager")
}

load_mercy_program_manager <- function() {
  load_program_manager_sheet(
    MERCY_PROGRAM_MANAGER_SHEET_ID,
    MERCY_PROGRAM_MANAGER_SHEET_NAME,
    "Mercy Program Manager"
  )
}

#' Normalize PM rows so company + Mercy Workshops can be stacked for session typing.
normalize_program_manager_for_typing <- function(pm, source_label = NA_character_) {
  if (is.null(pm) || nrow(pm) == 0) {
    return(data.frame(
      session_id = character(0),
      session_number = numeric(0),
      sessions_in_series = numeric(0),
      org_name = character(0),
      group = character(0),
      pm_source = character(0),
      stringsAsFactors = FALSE
    ))
  }
  nms <- names(pm)
  nms_l <- tolower(trimws(nms))
  pick <- function(cands) {
    idx <- which(nms_l %in% tolower(cands))[1]
    if (is.na(idx)) return(NULL)
    nms[idx]
  }
  sid_col <- pick(c("session_id", "session id", "session link"))
  num_col <- pick(c("session_number", "session number"))
  ser_col <- pick(c("sessions_in_series", "sessions in series"))
  org_col <- pick(c("org_name", "organization", "org"))
  grp_col <- pick(c("group"))
  out <- data.frame(
    session_id = if (!is.null(sid_col)) trimws(as.character(pm[[sid_col]])) else rep(NA_character_, nrow(pm)),
    session_number = if (!is.null(num_col)) suppressWarnings(as.numeric(unlist(pm[[num_col]]))) else rep(NA_real_, nrow(pm)),
    sessions_in_series = if (!is.null(ser_col)) suppressWarnings(as.numeric(unlist(pm[[ser_col]]))) else rep(NA_real_, nrow(pm)),
    org_name = if (!is.null(org_col)) trimws(as.character(unlist(pm[[org_col]]))) else rep(NA_character_, nrow(pm)),
    group = if (!is.null(grp_col)) trimws(as.character(unlist(pm[[grp_col]]))) else rep(NA_character_, nrow(pm)),
    pm_source = rep(as.character(source_label), nrow(pm)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$session_id) & nzchar(out$session_id), , drop = FALSE]
  # Prefer rows that carry series metadata when the same session_id appears twice
  if (nrow(out) == 0) return(out)
  out$._rank <- ifelse(!is.na(out$session_number) & !is.na(out$sessions_in_series), 1L, 2L)
  out <- out[order(out$session_id, out$._rank), , drop = FALSE]
  out <- out[!duplicated(out$session_id), , drop = FALSE]
  out$._rank <- NULL
  out
}

#' Company PM + Mercy PM, deduped by session_id (for Big/Little Pre/Post typing).
combine_program_managers_for_typing <- function(pm_company, pm_mercy = NULL) {
  a <- normalize_program_manager_for_typing(pm_company, "company")
  b <- normalize_program_manager_for_typing(pm_mercy, "mercy")
  if (nrow(a) == 0 && nrow(b) == 0) return(a)
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)
  # Company first; Mercy fills session_ids company does not have
  only_mercy <- b[!b$session_id %in% a$session_id, , drop = FALSE]
  rbind(a, only_mercy)
}

# ============================================================================
# 5 Buckets Brand Colors
# ============================================================================

# Primary Colors
COLOR_PRIMARY_PURPLE <- "#5c2f92"
COLOR_PRIMARY_GRAY <- "#797d82"
COLOR_PRIMARY_GREEN <- "#82c341"

# Secondary Colors (use 30% or less)
COLOR_SECONDARY_ORANGE <- "#f58220"
COLOR_SECONDARY_YELLOW <- "#fdb71a"
COLOR_SECONDARY_BLUE <- "#0076be"
COLOR_SECONDARY_GRAY <- "#5f6369"

# Color palette for visualizations
BRAND_COLORS <- c(
  COLOR_PRIMARY_PURPLE,
  COLOR_PRIMARY_GREEN,
  COLOR_SECONDARY_ORANGE,
  COLOR_SECONDARY_BLUE,
  COLOR_SECONDARY_YELLOW,
  COLOR_PRIMARY_GRAY
)

# ============================================================================
# Helper Functions
# ============================================================================

# Format dates consistently
format_date_5b <- function(date) {
  if (is.null(date) || is.na(date)) return("")
  format(as.Date(date), "%B %d, %Y")
}

# Format time ranges
format_time_range <- function(start_time, end_time) {
  if (is.null(start_time) || is.na(start_time)) return("")
  start_str <- format(strptime(start_time, "%H:%M"), "%I:%M %p")
  if (!is.null(end_time) && !is.na(end_time) && end_time != "") {
    end_str <- format(strptime(end_time, "%H:%M"), "%I:%M %p")
    return(paste(start_str, "-", end_str))
  }
  return(start_str)
}

