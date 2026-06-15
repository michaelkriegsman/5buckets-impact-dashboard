# behavior_item_canonical_feb2026.R
# Map Past Behaviors (Pre, often past tense) and Planned Actions (Post, present/future)
# to a single display label per behavior for item-level and aggregate analyses.

# Canonical labels (present / planned form — matches Big Post checkbox wording; used everywhere).
BEHAVIOR_ITEM_CANONICAL_LABELS <- c(
  "Set a financial goal",
  "Track my spending for at least one week",
  "Create or update a personal budget",
  "Contribute to a savings or investment account",
  "Check my credit score or credit report",
  "Share a financial tip or resource with someone in my life",
  "Talk to someone I trust about money",
  "Reflect on my thoughts and feelings about money"
)

# Normalized key (lowercase, single spaces, ASCII-ish apostrophe) -> canonical label
.behavior_alias_map <- local({
  m <- c(
    # Goal — usually identical
    "set a financial goal" = "Set a financial goal",
    # Spending
    "track my spending for at least one week" = "Track my spending for at least one week",
    "tracked my spending for at least one week" = "Track my spending for at least one week",
    # Budget
    "create or update a personal budget" = "Create or update a personal budget",
    "created or updated a personal budget" = "Create or update a personal budget",
    # Savings
    "contribute to a savings or investment account" = "Contribute to a savings or investment account",
    "contributed to a savings or investment account" = "Contribute to a savings or investment account",
    # Credit
    "check my credit score or credit report" = "Check my credit score or credit report",
    "checked my credit score or credit report" = "Check my credit score or credit report",
    # Share tip
    "share a financial tip or resource with someone in my life" = "Share a financial tip or resource with someone in my life",
    "shared a financial tip or resource with someone in my life" = "Share a financial tip or resource with someone in my life",
    # Talk
    "talk to someone i trust about money" = "Talk to someone I trust about money",
    "talked to someone i trust about money" = "Talk to someone I trust about money",
    # Reflect
    "reflect on my thoughts and feelings about money" = "Reflect on my thoughts and feelings about money",
    "reflected on my thoughts and feelings about money" = "Reflect on my thoughts and feelings about money"
  )
  m
})

#' @param x Character vector of single tokens from comma-separated Pre/Post cells
#' @return Character vector of canonical labels (NA if unmappable)
normalize_behavior_token <- function(x) {
  if (!length(x)) return(character(0))
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\u2019|\u2018", "'", x)
  x <- gsub("\u201c|\u201d", '"', x)
  x <- gsub("[[:space:]]+", " ", x)
  tolower(x)
}

#' Apply light past->present heuristics for behavior phrases (after normalize).
.behavior_past_to_present_heuristic <- function(key) {
  if (!nzchar(key)) return(key)
  # Verb-only substitutions (ordered)
  key <- sub("^tracked ", "track ", key)
  key <- sub("^created or updated ", "create or update ", key)
  key <- sub("^contributed ", "contribute ", key)
  key <- sub("^checked ", "check ", key)
  key <- sub("^shared ", "share ", key)
  key <- sub("^talked ", "talk ", key)
  key <- sub("^reflected ", "reflect ", key)
  key
}

#' Map one raw checkbox label to canonical display text.
canonical_behavior_label <- function(x) {
  if (length(x) == 0L) return(character(0))
  if (length(x) > 1L) {
    return(vapply(x, canonical_behavior_label, character(1), USE.NAMES = FALSE))
  }
  raw <- trimws(as.character(x[1]))
  if (is.na(raw) || !nzchar(raw)) return(NA_character_)
  key <- normalize_behavior_token(raw)
  if (key %in% names(.behavior_alias_map)) return(unname(.behavior_alias_map[[key]]))
  key2 <- .behavior_past_to_present_heuristic(key)
  if (key2 %in% names(.behavior_alias_map)) return(unname(.behavior_alias_map[[key2]]))
  # Fallback: keep raw trimmed text if survey adds new options not yet in the map
  trimws(raw)
}
