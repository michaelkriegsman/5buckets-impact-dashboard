# big_pre_analysis_feb2026.R - Analysis for Big Pre (Master Workbook - Feb 2026)
# Uses question_mapping_feb2026.R for column names

source("data/question_mapping.R", local = TRUE)
source("data/behavior_item_canonical.R")

# ============================================================================
# Helpers (same as big_pre_analysis.R)
# ============================================================================
# 4-point agreement scale (no Neutral). Any stray "Neutral" is dropped (treated as missing).
order_likert_responses <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
  factor(x, levels = levels, ordered = TRUE)
}

get_ordered_likert_table <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
  empty_tbl <- structure(rep(0L, length(levels)), names = levels, class = "table")
  if (is.null(x) || length(x) == 0) return(empty_tbl)
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(empty_tbl)
  f <- factor(x, levels = levels, ordered = TRUE)
  if (all(is.na(f))) return(empty_tbl)
  table(f)
}

order_income_levels <- function(income_vec) {
  income_order <- c(
    "If one person under $36k; two people under $41k; family of four under $52k",
    "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
    "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
    "If one person over $97k; two people over $111k; family of four over $139k"
  )
  factor(income_vec, levels = income_order, ordered = TRUE)
}

order_age_groups <- function(age_vec) {
  # Age brackets: former "Under 18" split into "Under 16" and "16-18"; legacy "Under 18" still recognized.
  age_order <- c("Under 16", "16-18", "Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  factor(age_vec, levels = age_order, ordered = TRUE)
}

# 4-point agreement scale (no Neutral). Symmetric coding centered on 0 so the index mean
# naturally leans negative/positive. Any stray "Neutral" maps to NA (treated as missing).
AGREE_LEVELS_4PT <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
AGREE_SCORES_4PT <- c(-3, -1, 1, 3)
AGREE_SCALE_MIN  <- -3
AGREE_SCALE_MAX  <- 3
AGREE_SCALE_NULL <- 0  # neutral / no-change reference for a symmetric scale

likert_to_numeric <- function(x) {
  result <- rep(NA_real_, length(x))
  result[x == "Strongly Disagree"] <- -3
  result[x == "Disagree"] <- -1
  result[x == "Agree"] <- 1
  result[x == "Strongly Agree"] <- 3
  as.numeric(result)
}

# Big Pre grid tokens (not labels): strip if they appear alone in legacy CSV
filter_pre_behavior_csv_labels <- function(parts) {
  parts <- trimws(parts[parts != ""])
  if (!length(parts)) return(parts)
  drop <- c("Yes", "No", "Maybe", "Yes!!", "Maybe...", "Nope", "Maybe.")
  parts[!parts %in% drop]
}

# ============================================================================
# Index Calculations - Feb 2026 (8 Financial Wellness items)
# ============================================================================

calculate_wellness_index_feb2026 <- function(pre_data) {
  if (nrow(pre_data) == 0) return(numeric(0))
  cols <- PRE_FINANCIAL_WELLNESS_COLS
  present <- intersect(cols, colnames(pre_data))
  if (length(present) == 0) return(rep(NA, nrow(pre_data)))
  mat <- sapply(present, function(c) likert_to_numeric(pre_data[[c]]))
  idx <- rowMeans(mat, na.rm = TRUE)
  idx[rowSums(!is.na(mat)) == 0] <- NA
  idx
}

calculate_behavioral_index_feb2026 <- function(pre_data) {
  if (nrow(pre_data) == 0) return(numeric(0))
  if (!PRE_PAST_BEHAVIORS_COL %in% colnames(pre_data)) return(rep(NA, nrow(pre_data)))
  vec <- pre_data[[PRE_PAST_BEHAVIORS_COL]]
  counts <- vapply(vec, function(x) {
    if (is.na(x) || as.character(x) == "") return(0L)
    parts <- strsplit(trimws(as.character(x)), ",\\s*")[[1]]
    parts <- filter_pre_behavior_csv_labels(parts)
    parts <- trimws(parts[parts != ""])
    if (!length(parts)) return(0L)
    labs <- canonical_behavior_label(parts)
    labs <- labs[!is.na(labs) & nzchar(labs)]
    length(unique(labs))
  }, integer(1))
  counts
}

# Parsed behavior counts for bar chart (Feb 2026: single comma-separated column)
get_past_behaviors_counts_feb2026 <- function(pre_data) {
  if (nrow(pre_data) == 0 || !PRE_PAST_BEHAVIORS_COL %in% colnames(pre_data)) {
    return(data.frame(behavior = character(0), count = integer(0)))
  }
  vec <- pre_data[[PRE_PAST_BEHAVIORS_COL]]
  all_parts <- character(0)
  for (x in vec) {
    if (is.na(x) || as.character(x) == "") next
    parts <- strsplit(trimws(as.character(x)), ",\\s*")[[1]]
    parts <- filter_pre_behavior_csv_labels(parts)
    parts <- trimws(parts[parts != ""])
    if (!length(parts)) next
    labs <- canonical_behavior_label(parts)
    labs <- labs[!is.na(labs) & nzchar(labs)]
    all_parts <- c(all_parts, labs)
  }
  if (length(all_parts) == 0) return(data.frame(behavior = character(0), count = integer(0)))
  tbl <- sort(table(all_parts), decreasing = TRUE)
  data.frame(behavior = names(tbl), count = as.integer(tbl), stringsAsFactors = FALSE)
}

# ============================================================================
# Variable Mapping Table - Feb 2026
# ============================================================================

get_variable_mapping_table_feb2026 <- function() {
  fc <- c(
    "Timestamp", "Session Link", "Organization", "Group", "Facilitators",
    "Content", "Date", "Time", "QA Flag", "QA Flag", "Identifier",
    "Privacy", "Req", "First Name", "Last Name", "Name", "Contact",
    "Intention", "Curiosities", "Hoped Feelings",
    "Know Amount", "Know Afford", "Know Where", "Optimism", "Money Relationship",
    "Stress Management", "Planning Confidence", "Professional Comfort",
    "Past Behaviors",
    "Zip", "Age", "Race/Ethnicity", "Gender", "Income", "Education",
    "First-Gen College", "First-Gen U.S.", "Veteran", "Disability", "Neurodivergent",
    "Learning Support",
    "Additional Comments"
  )
  mapping <- data.frame(
    Question_Text = c(
      PRE_METADATA_COLS,
      PRE_OPENING_COLS,
      PRE_FINANCIAL_WELLNESS_COLS,
      PRE_PAST_BEHAVIORS_COL,
      PRE_DEMOGRAPHICS_COLS,
      PRE_SUPPORT_COL,
      PRE_ADDITIONAL_COMMENTS_COL
    ),
    Gross_Category = c(
      rep("Metadata", length(PRE_METADATA_COLS)),
      rep("Workshop Intentions", length(PRE_OPENING_COLS)),
      rep("Financial Wellness", length(PRE_FINANCIAL_WELLNESS_COLS)),
      "Past Behaviors",
      rep("Demographics", length(PRE_DEMOGRAPHICS_COLS)),
      "Learning Support",
      "Additional Context"
    ),
    Fine_Category = fc,
    Response_Type = c(
      rep("Metadata", length(PRE_METADATA_COLS)),
      rep("Open Text", length(PRE_OPENING_COLS)),
      rep("Likert (5-point)", length(PRE_FINANCIAL_WELLNESS_COLS)),
      "Comma-separated multi-select",
      rep("Categorical", length(PRE_DEMOGRAPHICS_COLS)),
      "Open Text",
      "Open Text"
    ),
    Survey_Type = c(
      rep("All Surveys", length(PRE_METADATA_COLS)),
      rep("Big Pre + Little Pre", length(PRE_OPENING_COLS)),
      rep("Big Pre Only", length(PRE_FINANCIAL_WELLNESS_COLS) + 1L),
      rep("Big Pre Only", length(PRE_DEMOGRAPHICS_COLS) + 2L)
    ),
    Description = c(
      rep("Session/form metadata from submission", length(PRE_METADATA_COLS)),
      rep("Open-ended. Word cloud + lexicon-based sentiment.", length(PRE_OPENING_COLS)),
      rep("4-point agreement Likert (no neutral), scored −3 to +3. Contributes to Financial Wellness Index (average of 8 items).", length(PRE_FINANCIAL_WELLNESS_COLS)),
      "Comma-separated list. Behavioral Index = count of items checked (0–8).",
      rep("Self-reported demographic categories.", length(PRE_DEMOGRAPHICS_COLS)),
      "Open-ended. Optional feedback.",
      "Open-ended. Optional comments."
    ),
    stringsAsFactors = FALSE
  )
  mapping
}
