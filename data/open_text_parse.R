# open_text_parse.R — bilingual open-text cells (Mercy / ES / ZH handlers)

ensure_text_vector <- function(text_vec) {
  if (is.null(text_vec)) return(character(0))
  if (is.list(text_vec)) {
    text_vec <- vapply(text_vec, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_
      else if (is.list(x)) as.character(unlist(x)[1])
      else as.character(x[1])
    }, character(1))
  }
  as.character(text_vec)
}

#' Exact delimiter written by Apps Script handlers (spaces included).
OPEN_TEXT_DELIM <- " (%-^-%) "

#' Split one cell into original + English sides.
split_open_text_cell <- function(x) {
  s <- trimws(as.character(x))
  if (is.na(s) || !nzchar(s)) {
    return(list(
      original = NA_character_,
      english = NA_character_,
      has_delim = FALSE,
      detected_lang_note = NA_character_
    ))
  }
  if (!grepl(OPEN_TEXT_DELIM, s, fixed = TRUE)) {
    return(list(
      original = s,
      english = s,
      has_delim = FALSE,
      detected_lang_note = NA_character_
    ))
  }
  parts <- strsplit(s, OPEN_TEXT_DELIM, fixed = TRUE)[[1L]]
  orig <- trimws(parts[1L])
  en_raw <- if (length(parts) >= 2L) trimws(paste(parts[-1L], collapse = OPEN_TEXT_DELIM)) else ""
  note <- NA_character_
  en <- en_raw
  m <- regexpr("^\\[([^\\]]+)\\s+language detected\\]\\s*", en_raw, perl = TRUE)
  if (m > 0) {
    note <- sub("^\\[|\\]\\s*$", "", regmatches(en_raw, m))
    en <- trimws(sub("^\\[[^\\]]+\\s+language detected\\]\\s*", "", en_raw, perl = TRUE))
  }
  if (!nzchar(en)) en <- orig
  list(
    original = orig,
    english = en,
    has_delim = TRUE,
    detected_lang_note = note
  )
}

strip_language_note <- function(english_side) {
  s <- trimws(as.character(english_side))
  if (is.na(s) || !nzchar(s)) return(s)
  sub("^\\[[^\\]]+\\s+language detected\\]\\s*", "", s, perl = TRUE)
}

#' Vector: text for wordclouds / sentiment by mode.
#' @param lang_mode "all" = original side; "english" = English side (delimiter-aware).
open_text_for_mode <- function(text_vec, lang_mode = c("all", "english")) {
  lang_mode <- match.arg(lang_mode)
  text_vec <- ensure_text_vector(text_vec)
  if (!length(text_vec)) return(character(0))
  out <- vapply(text_vec, function(x) {
    sp <- split_open_text_cell(x)
    if (lang_mode == "english") sp$english else sp$original
  }, character(1), USE.NAMES = FALSE)
  out[!is.na(out)]
}

#' Summary counts for UI footnotes.
open_text_bilingual_summary <- function(text_vec) {
  text_vec <- ensure_text_vector(text_vec)
  n <- sum(!is.na(text_vec) & nzchar(trimws(text_vec)))
  if (n == 0) {
    return(list(total = 0L, with_delim = 0L, monolingual = 0L))
  }
  flags <- vapply(text_vec, function(x) split_open_text_cell(x)$has_delim, logical(1))
  flags <- flags[!is.na(text_vec) & nzchar(trimws(text_vec))]
  list(
    total = length(flags),
    with_delim = sum(flags, na.rm = TRUE),
    monolingual = sum(!flags, na.rm = TRUE)
  )
}

#' Sample table for QA (original | english).
open_text_bilingual_samples <- function(text_vec, n = 8L) {
  text_vec <- ensure_text_vector(text_vec)
  rows <- lapply(text_vec, function(x) {
    sp <- split_open_text_cell(x)
    if (!sp$has_delim) return(NULL)
    data.frame(
      original = sp$original,
      english = sp$english,
      note = sp$detected_lang_note %||% "",
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(original = character(0), english = character(0), note = character(0)))
  }
  d <- do.call(rbind, rows)
  head(d, n)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x[1]))) y else x
