# annual_survey.R - column helpers + light analysis for Annual Survey tab (2026 form)

# Stable substring matchers for live Master "Annual Survey" headers (form may tweak wording).
ANNUAL_COL_PATTERNS <- list(
  first_attend = "When did you first attend",
  last_attend = "When did you last attend",
  how_attend = "How did you attend",
  host_org = "What organization hosted",
  topics = "Which workshop topic",
  compared_prefix = "Compared to before your 5 Buckets",
  behaviors = "As a result of my workshop",
  income_change = "Income: Roughly",
  savings_change = "Savings: Roughly",
  debt_change = "Debt: Roughly",
  investments_change = "Investments: Roughly",
  credit_change = "Credit Score: Roughly",
  takeaway = "one takeaway you still remember",
  proud_goal = "money decision you're proud of|financial goal",
  impact_story = "How has participating in this program helped",
  recommend = "How likely are you to recommend",
  keep_touch = "We'd love to keep in touch",
  learn_more = "personal finance topic would you most like",
  anything_else = "Anything else you'd like to share",
  email = "^Email Address$",
  zip = "^Zip Code$",
  age = "^Age Group$",
  race = "Race/Ethnicity",
  gender = "^Gender Identity$",
  income_hh = "^Household Income$",
  education = "highest level of education",
  first_gen_college = "First-Generation Status \\(College\\)|First.?Generation Status \\(College\\)",
  first_gen_us = "FirstGeneration Status \\(U\\.S\\.\\)|First-Generation Status \\(U\\.S\\.\\)",
  veteran = "^Veteran Status$",
  disability = "^Disability Status$",
  neurodivergent = "neurodivergent"
)

# Canonical response order for money-change items (form scale: largest change -> none -> N/A).
# Top of chart = first element.
ANNUAL_MONEY_LEVELS_INCREASE <- c(
  "Increased more than $5,000",
  "Increased $1,000 - $5,000",
  "Increased $500 - $1,000",
  "Increased less than $500",
  "No change",
  "Does not apply to me"
)
# Savings / investments can move either direction; form order: gains -> none -> losses -> N/A
ANNUAL_MONEY_LEVELS_BIPOLAR <- c(
  "Increased more than $5,000",
  "Increased $1,000 - $5,000",
  "Increased $500 - $1,000",
  "Increased less than $500",
  "No change",
  "Decreased less than $500",
  "Decreased $500 - $1,000",
  "Decreased $1,000 - $5,000",
  "Decreased more than $5,000",
  "Decreased",
  "Does not apply to me"
)
ANNUAL_MONEY_LEVELS_DECREASE <- c(
  "Decreased to $0",
  "Decreased more than $5,000",
  "Decreased $1,000 - $5,000",
  "Decreased $500 - $1,000",
  "Decreased less than $500",
  "Decreased",
  "No change",
  "Does not apply to me"
)
ANNUAL_MONEY_LEVELS_CREDIT <- c(
  "Increased more than 100 points",
  "Increased 50 - 100 points",
  "Increased 1-50 points",
  "No change",
  "I haven't checked my credit score",
  "I don't have a credit score",
  "Does not apply to me"
)
ANNUAL_ATTEND_TIMING_LEVELS <- c(
  "Within the last month",
  "1-3 months ago",
  "3-6 months ago",
  "7-12 months ago",
  "1-2 years ago",
  "More than 2 years ago",
  "I don't remember"
)

annual_find_col <- function(df, pattern, exact = FALSE) {
  if (is.null(df) || ncol(df) == 0) return(NULL)
  nms <- colnames(df)
  if (isTRUE(exact)) {
    hit <- nms[tolower(nms) == tolower(pattern)]
    if (length(hit)) return(hit[1])
    return(NULL)
  }
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else NULL
}

annual_money_change_cols <- function(df) {
  list(
    Income = annual_find_col(df, ANNUAL_COL_PATTERNS$income_change),
    Savings = annual_find_col(df, ANNUAL_COL_PATTERNS$savings_change),
    Debt = annual_find_col(df, ANNUAL_COL_PATTERNS$debt_change),
    Investments = annual_find_col(df, ANNUAL_COL_PATTERNS$investments_change),
    `Credit score` = annual_find_col(df, ANNUAL_COL_PATTERNS$credit_change)
  )
}

#' Flatten unicode dashes / odd spacing so form variants can match canonical levels.
annual_clean_label_text <- function(x) {
  s <- trimws(as.character(x))
  s <- gsub("\u2013|\u2014|\u2212", "-", s)
  s <- gsub("\\s+", " ", s)
  s <- gsub("\\$\\s*", "$", s)
  s <- gsub("\\s*-\\s*", " - ", s)
  # "$1000" -> "$1,000" when thousands without comma
  s <- gsub("\\$([0-9]{4})\\b", "$\\1", s)
  s <- gsub("\\$([0-9])([0-9]{3})\\b", "$\\1,\\2", s)
  s
}

#' Map messy Master form labels onto canonical money-change levels.
annual_normalize_money_label <- function(x, kind = c("increase", "decrease", "credit")) {
  kind <- match.arg(kind)
  s0 <- annual_clean_label_text(x)
  out <- rep(NA_character_, length(s0))
  for (i in seq_along(s0)) {
    s <- s0[i]
    if (is.na(s) || !nzchar(s)) next
    sl <- tolower(s)
    if (grepl("does not apply", sl)) {
      out[i] <- "Does not apply to me"
      next
    }
    if (grepl("^no change|^no changed$", sl)) {
      out[i] <- "No change"
      next
    }
    if (kind == "credit") {
      if (grepl("don'?t have|do not have", sl)) {
        out[i] <- "I don't have a credit score"
        next
      }
      if (grepl("haven'?t checked|have not checked", sl)) {
        out[i] <- "I haven't checked my credit score"
        next
      }
      if (grepl("more than\\s*100|100\\+", sl)) {
        out[i] <- "Increased more than 100 points"
        next
      }
      if (grepl("50\\s*-\\s*100|50-100", sl)) {
        out[i] <- "Increased 50 - 100 points"
        next
      }
      if (grepl("1\\s*-\\s*50|1-50", sl)) {
        out[i] <- "Increased 1-50 points"
        next
      }
      out[i] <- s
      next
    }
    # Dollar-band items (income / savings / investments / debt)
    if (grepl("decreased\\s+to\\s+\\$0|decreased to \\$0", sl)) {
      out[i] <- "Decreased to $0"
      next
    }
    if (grepl("more than\\s*\\$\\s*5", sl)) {
      out[i] <- if (grepl("decreased", sl)) "Decreased more than $5,000" else "Increased more than $5,000"
      next
    }
    if (grepl("1,?000\\s*-\\s*\\$?5,?000|1000\\s*-\\s*\\$?5", sl)) {
      out[i] <- if (grepl("decreased", sl)) "Decreased $1,000 - $5,000" else "Increased $1,000 - $5,000"
      next
    }
    if (grepl("500\\s*-\\s*\\$?1,?000|500-\\$?1", sl)) {
      out[i] <- if (grepl("decreased", sl)) "Decreased $500 - $1,000" else "Increased $500 - $1,000"
      next
    }
    # Form alternate: "Increased less than $1,000" -> $500-$1,000 band
    if (grepl("less than\\s*\\$\\s*1,?000", sl) && !grepl("500", sl)) {
      out[i] <- if (grepl("decreased", sl)) "Decreased $500 - $1,000" else "Increased $500 - $1,000"
      next
    }
    if (grepl("less than\\s*\\$\\s*500", sl)) {
      out[i] <- if (grepl("decreased", sl)) "Decreased less than $500" else "Increased less than $500"
      next
    }
    if (grepl("^decreased$", sl)) {
      out[i] <- "Decreased"
      next
    }
    out[i] <- s
  }
  out
}

annual_money_levels_for_kind <- function(kind = c("increase", "decrease", "credit", "bipolar")) {
  kind <- match.arg(kind)
  switch(
    kind,
    increase = ANNUAL_MONEY_LEVELS_INCREASE,
    decrease = ANNUAL_MONEY_LEVELS_DECREASE,
    credit = ANNUAL_MONEY_LEVELS_CREDIT,
    bipolar = ANNUAL_MONEY_LEVELS_BIPOLAR
  )
}

#' Count table for a categorical / multi-select column (comma-split when needed).
#' @param ordered_levels optional character vector; first = top of chart. Unknown labels append at end.
#' @param normalize_fun optional function(character) -> character applied before counting.
annual_count_table <- function(vec, split_multi = FALSE, ordered_levels = NULL, normalize_fun = NULL) {
  empty <- data.frame(Response = character(0), n = integer(0), pct = numeric(0), stringsAsFactors = FALSE)
  if (is.null(vec) || length(vec) == 0) return(empty)
  raw <- trimws(as.character(vec))
  raw <- raw[!is.na(raw) & nzchar(raw)]
  if (!length(raw)) return(empty)
  if (is.function(normalize_fun)) {
    raw <- normalize_fun(raw)
    raw <- raw[!is.na(raw) & nzchar(as.character(raw))]
    if (!length(raw)) return(empty)
  }
  den <- length(raw)
  if (isTRUE(split_multi)) {
    parts <- unlist(lapply(raw, function(x) {
      trimws(strsplit(x, ",\\s*")[[1]])
    }), use.names = FALSE)
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(empty)
    tab <- table(parts)
  } else {
    tab <- table(raw)
  }
  df <- data.frame(
    Response = names(tab),
    n = as.integer(tab),
    pct = round(100 * as.integer(tab) / den, 1),
    stringsAsFactors = FALSE
  )
  if (!is.null(ordered_levels) && length(ordered_levels)) {
    known <- ordered_levels[ordered_levels %in% df$Response]
    unknown <- setdiff(df$Response, ordered_levels)
    if (length(unknown)) {
      unknown <- unknown[order(-df$n[match(unknown, df$Response)], unknown)]
    }
    ord <- c(known, unknown)
    df <- df[match(ord, df$Response), , drop = FALSE]
    rownames(df) <- NULL
  } else {
    df <- df[order(-df$n, df$Response), , drop = FALSE]
    rownames(df) <- NULL
  }
  df
}

annual_money_count_table <- function(vec, kind = c("increase", "decrease", "credit", "bipolar")) {
  kind <- match.arg(kind)
  annual_count_table(
    vec,
    ordered_levels = annual_money_levels_for_kind(kind),
    normalize_fun = function(x) {
      if (kind == "bipolar") {
        dlab <- annual_normalize_money_label(x, kind = "decrease")
        ilab <- annual_normalize_money_label(x, kind = "increase")
        ifelse(grepl("^Decreased", dlab), dlab, ilab)
      } else {
        annual_normalize_money_label(x, kind = kind)
      }
    }
  )
}

annual_bar_plotly <- function(count_df, title, color = "#5c2f92", show_n = FALSE, show_pct = FALSE) {
  if (is.null(count_df) || nrow(count_df) == 0) {
    return(plotly::plotly_empty() %>% plotly::layout(title = title, font = list(family = "Arial, Helvetica, sans-serif", size = 12)))
  }
  df <- count_df
  # First row of df at top of horizontal bars
  df$Response <- factor(df$Response, levels = rev(as.character(df$Response)))
  txt <- rep("", nrow(df))
  for (i in seq_len(nrow(df))) {
    parts <- character(0)
    if (isTRUE(show_n)) parts <- c(parts, as.character(df$n[i]))
    if (isTRUE(show_pct)) parts <- c(parts, sprintf("%.0f%%", df$pct[i]))
    txt[i] <- paste(parts, collapse = "\n")
  }
  has_txt <- (isTRUE(show_n) || isTRUE(show_pct)) && any(nzchar(txt))
  plotly::plot_ly(
    df, x = ~n, y = ~Response, type = "bar", orientation = "h",
    marker = list(color = color),
    text = if (has_txt) txt else NULL,
    textposition = if (has_txt) "outside" else NULL,
    cliponaxis = FALSE
  ) %>%
    plotly::layout(
      title = list(text = title, font = list(family = "Arial, Helvetica, sans-serif", size = 13)),
      font = list(family = "Arial, Helvetica, sans-serif", size = 12),
      xaxis = list(title = "Count", rangemode = "nonnegative"),
      yaxis = list(title = ""),
      margin = list(l = 180, r = 40, t = 48, b = 40),
      showlegend = FALSE
    )
}

#' Recommend / NPS-style 0-10 (or 1-10) counts.
annual_recommend_scores <- function(vec) {
  if (is.null(vec) || length(vec) == 0) return(numeric(0))
  if (is.list(vec) && !is.data.frame(vec)) {
    vec <- vapply(seq_along(vec), function(i) {
      xi <- vec[[i]]
      if (is.null(xi) || length(xi) == 0) return(NA_character_)
      if (is.list(xi)) xi <- unlist(xi, recursive = TRUE, use.names = FALSE)
      trimws(paste(as.character(xi), collapse = " "))
    }, character(1))
  }
  x <- suppressWarnings(as.numeric(as.character(vec)))
  x[is.finite(x)]
}

#' Compared-to-before columns on the Annual Survey (Likert grid).
annual_compared_cols <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(character(0))
  grep(ANNUAL_COL_PATTERNS$compared_prefix, colnames(df), ignore.case = TRUE, value = TRUE)
}

annual_compared_short_label <- function(colname) {
  vapply(as.character(colname), function(nm) {
    m <- regmatches(nm, regexpr("\\[[^]]+\\]", nm, perl = TRUE))
    if (length(m) && nzchar(m[1])) {
      lab <- gsub("^\\[|\\]$", "", m[1])
      lab <- gsub("\\.$", "", trimws(lab))
      if (nchar(lab) > 72) paste0(substr(lab, 1, 69), "...") else lab
    } else {
      if (nchar(nm) > 72) paste0(substr(nm, 1, 69), "...") else nm
    }
  }, character(1), USE.NAMES = FALSE)
}

#' Share Agree + Strongly Agree per Compared-to-before item.
annual_compared_agree_table <- function(df) {
  cols <- annual_compared_cols(df)
  empty <- data.frame(
    Item = character(0), n = integer(0), agree_n = integer(0), agree_pct = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!length(cols) || is.null(df) || nrow(df) == 0) return(empty)
  rows <- lapply(cols, function(col) {
    raw <- trimws(as.character(df[[col]]))
    raw <- raw[!is.na(raw) & nzchar(raw)]
    n <- length(raw)
    if (n < 1) return(NULL)
    agree_n <- sum(grepl("^(Strongly )?Agree$", raw, ignore.case = TRUE))
    data.frame(
      Item = annual_compared_short_label(col),
      n = n,
      agree_n = agree_n,
      agree_pct = round(100 * agree_n / n, 1),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out[order(-out$agree_pct, out$Item), , drop = FALSE]
}

#' Long table of likert counts for annual Compared-to-before items (for stacked bars).
annual_compared_likert_long <- function(df) {
  cols <- annual_compared_cols(df)
  levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
  empty <- data.frame(
    Item = character(0), Response = character(0), n = integer(0),
    stringsAsFactors = FALSE
  )
  if (!length(cols) || is.null(df) || nrow(df) == 0) return(empty)
  rows <- list()
  for (col in cols) {
    lab <- annual_compared_short_label(col)
    raw <- trimws(as.character(df[[col]]))
    raw <- raw[!is.na(raw) & nzchar(raw)]
    if (!length(raw)) next
    f <- factor(raw, levels = levels)
    tab <- table(f)
    for (lev in levels) {
      rows[[length(rows) + 1L]] <- data.frame(
        Item = lab, Response = lev, n = as.integer(tab[[lev]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Mean of Annual Compared-to-before items (-3..+3), same scoring as Post Impact Index.
calculate_annual_compared_index <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(numeric(0))
  cols <- annual_compared_cols(df)
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  score_fun <- if (exists("likert_to_numeric", mode = "function")) {
    likert_to_numeric
  } else {
    function(x) {
      x <- trimws(as.character(x))
      out <- rep(NA_real_, length(x))
      out[tolower(x) == "strongly disagree"] <- -3
      out[tolower(x) == "disagree"] <- -1
      out[tolower(x) == "agree"] <- 1
      out[tolower(x) == "strongly agree"] <- 3
      out
    }
  }
  mat <- sapply(cols, function(c) score_fun(df[[c]]))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  idx <- rowMeans(mat, na.rm = TRUE)
  idx[rowSums(!is.na(mat)) == 0] <- NA_real_
  idx
}

#' Count of selected Annual behavior options (comma-separated multi-select).
calculate_annual_behavior_index <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(numeric(0))
  col <- annual_find_col(df, ANNUAL_COL_PATTERNS$behaviors)
  if (is.null(col)) return(rep(NA_real_, nrow(df)))
  vapply(as.character(df[[col]]), function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(NA_real_)
    parts <- trimws(strsplit(x, ",\\s*")[[1]])
    parts <- parts[nzchar(parts)]
    as.numeric(length(parts))
  }, numeric(1))
}
