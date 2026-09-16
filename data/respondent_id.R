# respondent_id.R — regenerate privacy-preserving IDs from email (same as Apps Script handlers)
#
# Live Pre/Post handlers:
#   Utilities.newBlob(SALT + '|' + email).getBytes() -> SHA-256 -> hex
# Equivalent in R with digest::digest(..., algo = "sha256", serialize = FALSE)
# when email is treated as a UTF-8 string (ASCII emails match Apps Script).
#
# Salt must NOT be committed. Set env var RESPONDENT_ID_SALT (local .Renviron / Connect).

get_respondent_id_salt <- function() {
  trimws(Sys.getenv("RESPONDENT_ID_SALT", unset = ""))
}

#' Hash email(s) to respondent_id using the Apps Script salt+pipe scheme.
#' @return character vector same length as emails; NA when email/salt missing
hash_respondent_id_from_email <- function(emails, salt = NULL) {
  if (is.null(salt)) salt <- get_respondent_id_salt()
  n <- length(emails)
  out <- rep(NA_character_, n)
  if (!nzchar(salt) || n == 0L) return(out)
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("digest package required to hash respondent_id from email")
    return(out)
  }
  em <- tolower(trimws(as.character(emails)))
  for (i in seq_len(n)) {
    if (is.na(em[i]) || !nzchar(em[i])) next
    out[i] <- digest::digest(paste0(salt, "|", em[i]), algo = "sha256", serialize = FALSE)
  }
  out
}

#' Resolve an email column from Annual (or Master) column names.
find_email_column <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(NULL)
  nms <- colnames(df)
  # Prefer explicit patterns used on Annual / Master
  exact <- c("respondent_email", "Email Address", "Email", "Personal Email Address")
  for (e in exact) {
    hit <- which(tolower(trimws(nms)) == tolower(e))
    if (length(hit)) return(nms[hit[1]])
  }
  if (exists("annual_find_col", mode = "function") && exists("ANNUAL_COL_PATTERNS")) {
    col <- annual_find_col(df, ANNUAL_COL_PATTERNS$email)
    if (!is.null(col)) return(col)
  }
  hit <- grep("^email", nms, ignore.case = TRUE, value = TRUE)
  if (length(hit)) return(hit[1])
  NULL
}

#' Attach / fill respondent_id on a data frame from its email column (in memory only).
#' Existing non-empty IDs are left alone unless overwrite = TRUE.
attach_respondent_ids_from_email <- function(df, overwrite = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  email_col <- find_email_column(df)
  if (is.null(email_col)) return(df)
  salt <- get_respondent_id_salt()
  if (!nzchar(salt)) {
    if (!"respondent_id" %in% names(df)) df$respondent_id <- NA_character_
    attr(df, "respondent_id_salt_missing") <- TRUE
    return(df)
  }
  hashed <- hash_respondent_id_from_email(df[[email_col]], salt = salt)
  if (!"respondent_id" %in% names(df) || isTRUE(overwrite)) {
    df$respondent_id <- hashed
  } else {
    cur <- as.character(df$respondent_id)
    fill <- is.na(cur) | !nzchar(trimws(cur)) | grepl("^ANON", trimws(cur), ignore.case = TRUE)
    df$respondent_id[fill] <- hashed[fill]
  }
  attr(df, "respondent_id_salt_missing") <- FALSE
  df
}
