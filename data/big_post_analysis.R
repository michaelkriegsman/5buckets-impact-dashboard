# big_post_analysis_feb2026.R - Analysis for Big Post (Master Workbook - Feb 2026)
# Uses question_mapping_feb2026.R for column names

source("data/question_mapping.R", local = TRUE)
if (!exists("canonical_behavior_label", mode = "function")) {
  source("data/behavior_item_canonical.R")
}

# ============================================================================
# Helpers
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
  # googlesheets4 can return list-columns; flatten to atomic character first
  if (is.list(x) && !is.data.frame(x)) {
    x <- vapply(seq_along(x), function(i) {
      xi <- x[[i]]
      if (is.null(xi) || length(xi) == 0) return(NA_character_)
      if (is.list(xi)) xi <- unlist(xi, recursive = TRUE, use.names = FALSE)
      trimws(paste(as.character(xi), collapse = ", "))
    }, character(1))
  } else {
    x <- trimws(as.character(x))
  }
  x <- x[!is.na(x) & nzchar(x)]
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

# 4-point agreement scale (no Neutral); symmetric coding centered on 0. See big_pre_analysis.R
# for the shared AGREE_* constants. Any stray "Neutral" maps to NA (treated as missing).
likert_to_numeric <- function(x) {
  result <- rep(NA_real_, length(x))
  result[x == "Strongly Disagree"] <- -3
  result[x == "Disagree"] <- -1
  result[x == "Agree"] <- 1
  result[x == "Strongly Agree"] <- 3
  as.numeric(result)
}

# Big Post grid tokens (not labels): strip if they appear alone in legacy CSV
filter_post_planned_csv_labels <- function(parts) {
  parts <- trimws(parts[parts != ""])
  if (!length(parts)) return(parts)
  drop <- c("Yes!!", "Maybe...", "Nope", "Maybe", "Maybe.", "Yes", "No")
  parts[!parts %in% drop]
}

# ============================================================================
# Index Calculations - Feb 2026
# ============================================================================

calculate_post_impact_index_feb2026 <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  cols <- POST_COMPARED_TO_COLS
  present <- intersect(cols, colnames(post_data))
  if (length(present) == 0) return(rep(NA, nrow(post_data)))
  mat <- sapply(present, function(c) likert_to_numeric(post_data[[c]]))
  idx <- rowMeans(mat, na.rm = TRUE)
  idx[rowSums(!is.na(mat)) == 0] <- NA
  idx
}

calculate_planned_actions_index_feb2026 <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  if (!POST_PLANNED_ACTIONS_COL %in% colnames(post_data)) return(rep(NA, nrow(post_data)))
  vec <- post_data[[POST_PLANNED_ACTIONS_COL]]
  counts <- vapply(vec, function(x) {
    if (is.na(x) || as.character(x) == "") return(0L)
    parts <- strsplit(trimws(as.character(x)), ",\\s*")[[1]]
    parts <- filter_post_planned_csv_labels(parts)
    parts <- trimws(parts[parts != ""])
    if (!length(parts)) return(0L)
    labs <- canonical_behavior_label(parts)
    labs <- labs[!is.na(labs) & nzchar(labs)]
    length(unique(labs))
  }, integer(1))
  counts
}

calculate_quality_index_feb2026 <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  sat_col <- "How satisfied were you with today's 5 Buckets session?"
  if (!sat_col %in% colnames(post_data)) return(rep(NA, nrow(post_data)))
  as.numeric(post_data[[sat_col]])
}

# Parsed planned actions counts for bar chart (Feb 2026: single comma-separated column)
get_planned_actions_counts_feb2026 <- function(post_data) {
  if (nrow(post_data) == 0 || !POST_PLANNED_ACTIONS_COL %in% colnames(post_data)) {
    return(data.frame(action = character(0), count = integer(0)))
  }
  vec <- post_data[[POST_PLANNED_ACTIONS_COL]]
  all_parts <- character(0)
  for (x in vec) {
    if (is.na(x) || as.character(x) == "") next
    parts <- strsplit(trimws(as.character(x)), ",\\s*")[[1]]
    parts <- filter_post_planned_csv_labels(parts)
    parts <- trimws(parts[parts != ""])
    if (!length(parts)) next
    labs <- canonical_behavior_label(parts)
    labs <- labs[!is.na(labs) & nzchar(labs)]
    all_parts <- c(all_parts, labs)
  }
  if (length(all_parts) == 0) return(data.frame(action = character(0), count = integer(0)))
  tbl <- sort(table(all_parts), decreasing = TRUE)
  data.frame(action = names(tbl), count = as.integer(tbl), stringsAsFactors = FALSE)
}

# ============================================================================
# Variable Mapping Table - Feb 2026
# ============================================================================

get_big_post_variable_mapping_feb2026 <- function(post_data = NULL) {
  mapping <- data.frame(
    Question_Text = c(
      POST_METADATA_COLS,
      POST_TODAY_SESSION_COLS,
      POST_COMPARED_TO_COLS,
      POST_PLANNED_ACTIONS_COL,
      POST_IMPACT_STORY_COL,
      POST_NPS_COL,
      POST_KEEP_IN_TOUCH_COL,
      POST_DEMOGRAPHICS_COLS,
      POST_ADDITIONAL_COMMENTS_COL
    ),
    Gross_Category = c(
      rep("Metadata", length(POST_METADATA_COLS)),
      rep("Today's Session", length(POST_TODAY_SESSION_COLS)),
      rep("Compared to Before", length(POST_COMPARED_TO_COLS)),
      "Planned Actions",
      "Impact Story",
      "NPS",
      "Future Engagement",
      rep("Demographics", length(POST_DEMOGRAPHICS_COLS)),
      "Additional Context"
    ),
    Fine_Category = c(
      "Timestamp", "Session Link", "Organization", "Group", "Facilitators",
      "Content", "Date", "Time", "QA Flag", "QA Flag", "Identifier",
      "Privacy", "Req", "First Name", "Last Name", "Name", "Contact",
      "Idea/Insight", "Application", "Helpfulness", "Satisfaction",
      "Understanding", "Awareness: Amount", "Awareness: Afford", "Awareness: Where",
      "Optimism", "Money Relationship", "Stress", "Confidence", "Professional Comfort",
      "Planned Actions (comma-separated)",
      "Impact Story",
      "NPS",
      "Keep in Touch",
      "Zip", "Age", "Race/Ethnicity", "Gender", "Income", "Education",
      "First-Gen College", "First-Gen U.S.", "Veteran", "Disability", "Neurodivergent",
      "Additional Comments"
    ),
    Response_Type = c(
      rep("Metadata", length(POST_METADATA_COLS)),
      rep("Open Text", 3),
      "Numeric (1-5)",
      rep("Likert (5-point)", length(POST_COMPARED_TO_COLS)),
      "Comma-separated multi-select",
      "Open Text",
      "Numeric (1-10)",
      "Multi-select",
      rep("Categorical", length(POST_DEMOGRAPHICS_COLS)),
      "Open Text"
    ),
    Survey_Type = c(
      rep("All Surveys", length(POST_METADATA_COLS)),
      rep("Little Post + Big Post", 4),
      rep("Big Post Only", length(POST_COMPARED_TO_COLS) + 1L),
      rep("Big Post Only", 3),
      rep("Big Post Only", length(POST_DEMOGRAPHICS_COLS) + 1L)
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(post_data) && nrow(post_data) > 0) {
    total_responses <- numeric(nrow(mapping))
    central_tendency <- character(nrow(mapping))
    for (i in seq_len(nrow(mapping))) {
      q <- mapping$Question_Text[i]
      if (q %in% colnames(post_data)) {
        col_data <- post_data[[q]]
        non_na <- sum(!is.na(col_data))
        total_responses[i] <- non_na
        if (non_na > 0) {
          if (grepl("Numeric", mapping$Response_Type[i])) {
            nv <- as.numeric(col_data)
            nv <- nv[!is.na(nv)]
            if (length(nv) > 0) central_tendency[i] <- sprintf("Mean: %.2f", mean(nv))
          } else if (mapping$Response_Type[i] == "Likert (5-point)") {
            tv <- table(col_data[!is.na(col_data)])
            if (length(tv) > 0) central_tendency[i] <- paste0("Mode: ", names(which.max(tv)))
          } else if (mapping$Response_Type[i] == "Open Text") {
            txt <- as.character(col_data[!is.na(col_data)])
            if (length(txt) > 0) central_tendency[i] <- sprintf("Avg length: %.0f chars", mean(nchar(txt)))
          } else {
            tv <- table(col_data[!is.na(col_data)])
            if (length(tv) > 0) central_tendency[i] <- paste0("Mode: ", names(which.max(tv)))
          }
        } else {
          central_tendency[i] <- "No responses"
        }
      } else {
        total_responses[i] <- 0
        central_tendency[i] <- "Column not found"
      }
    }
    mapping$Total_Responses <- total_responses
    mapping$Central_Tendency <- central_tendency
  } else {
    mapping$Total_Responses <- NA
    mapping$Central_Tendency <- NA
  }
  mapping
}
