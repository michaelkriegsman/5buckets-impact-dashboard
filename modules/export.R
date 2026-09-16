# export.R — Phase 3 sidebar exports
#
# Four downloads:
#   1. PDF report (human)           — current filters + export scope
#   2. Report (.md for AI)          — hierarchical numeric + figure recipes
#   3. Data Map (.md for AI)        — schema / relationships (filter-independent)
#   4. Filtered data (CSV ZIP)      — respondent-level CSVs (PII stripped)
#
# PII (names, emails) stripped from data exports. Hashed respondent_id kept.

# -----------------------------------------------------------------------------
# Section registry
# -----------------------------------------------------------------------------

EXPORT_SECTION_CHOICES <- c(
  "Overview" = "overview",
  "Reach" = "reach",
  "Financial Wellness" = "wellness",
  "Behavioral Readiness" = "behavioral",
  "Satisfaction" = "satisfaction",
  "Learning & Impact Stories" = "learning",
  "Annual Survey" = "annual"
)

EXPORT_SECTION_REGISTRY <- list(
  overview = list(
    id = "overview",
    title = "Overview",
    tab = "overview",
    blocks = c("response_descriptives", "respondent_pairing", "series_length")
  ),
  reach = list(
    id = "reach",
    title = "Reach",
    tab = "reach2",
    blocks = c("demographics")
  ),
  wellness = list(
    id = "wellness",
    title = "Financial Wellness",
    tab = "wellness",
    blocks = c(
      "pre_index", "pre_items", "post_index", "post_items",
      "between_index", "within_index", "between_items", "within_items"
    )
  ),
  behavioral = list(
    id = "behavioral",
    title = "Behavioral Readiness",
    tab = "behavioral",
    blocks = c(
      "pre_index", "past_items", "post_index", "planned_items",
      "between_index", "within_index", "between_items"
    )
  ),
  satisfaction = list(
    id = "satisfaction",
    title = "Satisfaction",
    tab = "satisfaction",
    blocks = c("session_sat", "nps", "by_module")
  ),
  learning = list(
    id = "learning",
    title = "Learning & Impact Stories",
    tab = "learning_stories",
    blocks = c("topic_understanding", "keep_in_touch", "word_freq")
  ),
  annual = list(
    id = "annual",
    title = "Annual Survey",
    tab = "annual_survey",
    blocks = c("counts", "compared", "recommend", "behaviors")
  )
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

export_sidebar_ui <- function() {
  div(
    class = "sidebar-card",
    tags$span(class = "sidebar-card-label", "Export"),
    tags$p(
      style = "font-size: 11px; color: #6a6f75; margin: 0 0 10px;",
      "Results, PDF, and data ZIP follow sidebar filters + section picks below. ",
      "Data Map is the full schema (not filter-dependent)."
    ),
    tags$h5("Sections to include", style = "margin: 0 0 6px; color: #5c2f92; font-size: 12px;"),
    checkboxGroupInput(
      "export_sections",
      label = NULL,
      choices = EXPORT_SECTION_CHOICES,
      selected = unname(EXPORT_SECTION_CHOICES)
    ),
    helpText(
      style = "font-size: 11px; color: #6a6f75; margin-top: 0;",
      "Results (md for AI) always includes numeric tables plus figure recipes/context so a secondary AI can recreate charts. ",
      "Data ZIP is respondent-level CSVs only (no images)."
    ),
    downloadButton(
      "export_human_pdf",
      "Report (PDF for humans)",
      icon = icon("file-pdf"),
      class = "btn-default",
      style = "width: 100%; margin-bottom: 6px; white-space: normal;"
    ),
    downloadButton(
      "export_analysis_md",
      "Results (md for AI)",
      icon = icon("file-alt"),
      class = "btn-default",
      style = "width: 100%; margin-bottom: 6px; white-space: normal;"
    ),
    downloadButton(
      "export_data_map_md",
      "Data Map (md for AI)",
      icon = icon("book"),
      class = "btn-default",
      style = "width: 100%; margin-bottom: 6px; white-space: normal;"
    ),
    downloadButton(
      "export_filtered_data_zip",
      "Filtered data (CSV ZIP)",
      icon = icon("database"),
      class = "btn-default",
      style = "width: 100%; margin-bottom: 0; white-space: normal;"
    )
  )
}

# -----------------------------------------------------------------------------
# Shared helpers
# -----------------------------------------------------------------------------

.export_safe_n <- function(df) {
  if (is.null(df) || !is.data.frame(df)) 0L else as.integer(nrow(df))
}

.export_md_escape <- function(x) {
  x <- .export_flatten_chr(x)
  x[is.na(x)] <- ""
  gsub("\\|", "\\\\|", x)
}

.export_flatten_chr <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x) && !is.data.frame(x)) {
    return(vapply(seq_along(x), function(i) {
      xi <- x[[i]]
      if (is.null(xi) || length(xi) == 0L || (length(xi) == 1L && is.na(xi))) return(NA_character_)
      if (is.list(xi)) xi <- unlist(xi, recursive = TRUE, use.names = FALSE)
      paste(as.character(xi), collapse = ", ")
    }, character(1)))
  }
  as.character(x)
}

.export_flatten_num <- function(x) {
  ch <- .export_flatten_chr(x)
  suppressWarnings(as.numeric(ch))
}

.export_md_table <- function(df, max_rows = 80L) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) {
    return("_No rows._\n")
  }
  df <- utils::head(df, max_rows)
  # Flatten list-columns from googlesheets4 before markdown / apply()
  for (j in seq_len(ncol(df))) {
    df[[j]] <- .export_md_escape(df[[j]])
  }
  # Ensure plain data.frame (no tibble list quirks)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(as.character(unlist(df[i, , drop = TRUE])), collapse = " | "), " |")
  }, character(1))
  paste(c(hdr, sep, rows, ""), collapse = "\n")
}

.export_fmt_list <- function(x, empty = "All (no restriction)") {
  x <- tryCatch({
    x <- as.character(x)
    x <- trimws(x[!is.na(x) & nzchar(x)])
    x
  }, error = function(e) character(0))
  if (!length(x)) return(empty)
  paste(x, collapse = "; ")
}

.export_selected_sections <- function(input) {
  sel <- tryCatch(input$export_sections, error = function(e) NULL)
  if (is.null(sel) || !length(sel)) return(unname(EXPORT_SECTION_CHOICES))
  intersect(as.character(sel), unname(EXPORT_SECTION_CHOICES))
}

.export_include_figures <- function(input) {
  # Deprecated: PNGs are not packed into the data ZIP.
  FALSE
}

.export_include_figure_recipes <- function(input) {
  # Always include figure recipes + context in Results MD.
  TRUE
}

.export_filter_snapshot_list <- function(input) {
  if (exists("collect_filter_snapshot", mode = "function")) {
    snap <- collect_filter_snapshot(input)
  } else {
    snap <- list(
      use_date_filter = isTRUE(input$use_date_filter),
      date_start = as.character(tryCatch(input$date_range[1], error = function(e) NA)),
      date_end = as.character(tryCatch(input$date_range[2], error = function(e) NA)),
      org = tryCatch(input$selected_org, error = function(e) NULL),
      group = tryCatch(input$selected_group, error = function(e) NULL),
      filter_gender = input$filter_gender,
      filter_veteran = input$filter_veteran,
      filter_income = input$filter_income,
      filter_education = input$filter_education,
      filter_language = input$filter_language %||% "All",
      selected_modules = input$selected_modules
    )
  }
  snap$opt_same_y <- isTRUE(input$opt_same_y)
  snap$opt_show_n <- isTRUE(input$opt_show_n)
  snap$opt_show_pct <- isTRUE(input$opt_show_pct)
  snap$export_sections <- .export_selected_sections(input)
  snap$export_include_figure_recipes <- TRUE
  snap$export_include_figures <- FALSE
  snap$include_annual_in_reach <- isTRUE(tryCatch(input$include_annual_in_reach, error = function(e) FALSE))
  snap$exported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  snap
}

.export_filter_snapshot_md <- function(snap) {
  date_lab <- if (isTRUE(snap$use_date_filter)) {
    paste(snap$date_start %||% "?", "->", snap$date_end %||% "?")
  } else {
    "Off (all dates)"
  }
  sec_lab <- .export_fmt_list(snap$export_sections, empty = "(none)")
  paste0(
    "## Filter snapshot\n\n",
    "| Setting | Value |\n| --- | --- |\n",
    "| Exported at | ", snap$exported_at %||% "", " |\n",
    "| Date filter | ", date_lab, " |\n",
    "| Organizations | ", .export_fmt_list(snap$org), " |\n",
    "| Groups | ", .export_fmt_list(snap$group), " |\n",
    "| Language | ", snap$filter_language %||% "All", " |\n",
    "| Gender | ", .export_fmt_list(snap$filter_gender), " |\n",
    "| Veteran | ", .export_fmt_list(snap$filter_veteran), " |\n",
    "| Household income | ", .export_fmt_list(snap$filter_income), " |\n",
    "| Education | ", .export_fmt_list(snap$filter_education), " |\n",
    "| Modules taught | ", .export_fmt_list(snap$selected_modules), " |\n",
    "| Export sections | ", sec_lab, " |\n",
    "| Display: paired Y | ", snap$opt_same_y %||% FALSE, " |\n",
    "| Display: show N | ", snap$opt_show_n %||% FALSE, " |\n",
    "| Display: show % | ", snap$opt_show_pct %||% TRUE, " |\n\n"
  )
}

.export_slug_from_filters <- function(snap) {
  orgs <- tryCatch({
    x <- as.character(snap$org %||% character(0))
    x <- trimws(x[nzchar(x)])
    x
  }, error = function(e) character(0))
  base <- if (!length(orgs)) {
    "all-orgs"
  } else if (length(orgs) == 1L) {
    gsub("[^A-Za-z0-9]+", "-", tolower(orgs[[1]]))
  } else {
    paste0(length(orgs), "-orgs")
  }
  paste0(base, "_", format(Sys.Date(), "%Y-%m-%d"))
}

.export_drop_pii_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0L) return(df)
  drop_exact <- c(
    "first_name", "last_name", "respondent_name", "respondent_email",
    "Email Address", "Email", "Personal Email Address", "req"
  )
  drop_pat <- "(?i)(^|_)(email|first.?name|last.?name|respondent.?name|phone|address)($|_)"
  keep <- !names(df) %in% drop_exact & !grepl(drop_pat, names(df), perl = TRUE)
  df[, keep, drop = FALSE]
}

.export_write_csv <- function(df, path) {
  if (!is.null(df) && is.data.frame(df) && ncol(df)) {
    # googlesheets4 often leaves multi-select / open-text as list-columns
    df <- .export_df_flatten_cols(df, cols = NULL)
  }
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

.export_mean_na <- function(x) {
  x <- .export_flatten_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  round(mean(x), 3)
}

.export_index_summary_row <- function(label, values) {
  v <- .export_flatten_num(values)
  v <- v[is.finite(v)]
  data.frame(
    Index = label,
    N = length(v),
    Mean = if (length(v)) round(mean(v), 3) else NA_real_,
    SD = if (length(v) > 1L) round(stats::sd(v), 3) else NA_real_,
    Min = if (length(v)) round(min(v), 3) else NA_real_,
    Max = if (length(v)) round(max(v), 3) else NA_real_,
    stringsAsFactors = FALSE
  )
}

.export_nps_bucket <- function(x) {
  x <- .export_flatten_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(list(n = 0L, promoters = 0L, passives = 0L, detractors = 0L, nps = NA_real_, mean = NA_real_))
  }
  promoters <- sum(x >= 9, na.rm = TRUE)
  passives <- sum(x >= 7 & x <= 8, na.rm = TRUE)
  detractors <- sum(x <= 6, na.rm = TRUE)
  n <- length(x)
  list(
    n = n,
    promoters = promoters,
    passives = passives,
    detractors = detractors,
    nps = round(100 * (promoters - detractors) / n, 1),
    mean = round(mean(x), 2)
  )
}

.export_find_col <- function(df, exact = NULL, pattern = NULL) {
  if (is.null(df) || !ncol(df)) return(NULL)
  nms <- colnames(df)
  if (!is.null(exact) && exact %in% nms) return(exact)
  if (!is.null(pattern)) {
    hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[[1]])
  }
  NULL
}

.export_count_table <- function(vec, levels = NULL, label = "Level") {
  x <- trimws(.export_flatten_chr(vec))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) {
    return(data.frame(setNames(list(character(0), integer(0), numeric(0)), c(label, "N", "Pct")),
                      stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (!is.null(levels) && length(levels)) {
    f <- factor(x, levels = unique(c(levels, setdiff(sort(unique(x)), levels))))
    tbl <- table(f, useNA = "no")
  } else {
    tbl <- sort(table(x), decreasing = TRUE)
  }
  n <- as.integer(tbl)
  data.frame(
    setNames(list(names(tbl), n, round(100 * n / sum(n), 1)), c(label, "N", "Pct")),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.export_likert_table <- function(vec, item_label = "Item") {
  vec <- .export_flatten_chr(vec)
  if (exists("get_ordered_likert_table", mode = "function")) {
    tbl <- tryCatch(get_ordered_likert_table(vec), error = function(e) NULL)
    if (!is.null(tbl) && length(tbl)) {
      n <- as.integer(tbl)
      return(data.frame(
        Response = names(tbl),
        N = n,
        Pct = if (sum(n)) round(100 * n / sum(n), 1) else 0,
        stringsAsFactors = FALSE
      ))
    }
  }
  .export_count_table(vec, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"),
                      label = "Response")
}

.export_recipe_md <- function(recipe, include_recipes = TRUE) {
  if (!isTRUE(include_recipes)) return("")
  if (is.null(recipe) || !length(recipe)) return("")
  lines <- vapply(names(recipe), function(k) {
    paste0("- **", k, ":** ", as.character(recipe[[k]] %||% ""))
  }, character(1))
  paste0("**Figure recipe** (enough to recreate the chart without a PNG)\n\n", paste(lines, collapse = "\n"), "\n\n")
}

.export_block <- function(id, path, title, tables = list(), stats = NULL, recipe = NULL,
                          narrative = NULL, figure_spec = NULL,
                          variables = NULL, csv_sources = NULL, why = NULL) {
  list(
    id = id,
    path = path,
    title = title,
    tables = tables,
    stats = stats,
    recipe = recipe,
    narrative = narrative,
    figure_spec = figure_spec,
    variables = variables,
    csv_sources = csv_sources,
    why = why
  )
}

.export_block_to_md <- function(block, heading_level = 3L, include_recipes = TRUE) {
  hashes <- paste(rep("#", heading_level), collapse = "")
  parts <- c(
    paste0(hashes, " ", block$title),
    "",
    if (!is.null(block$path) && nzchar(block$path)) paste0("*Dashboard path:* ", block$path),
    if (!is.null(block$path) && nzchar(block$path)) "",
    if (!is.null(block$why) && nzchar(block$why)) paste0("**Why this matters:** ", block$why),
    if (!is.null(block$why) && nzchar(block$why)) "",
    if (!is.null(block$narrative) && nzchar(block$narrative)) block$narrative,
    if (!is.null(block$narrative) && nzchar(block$narrative)) "",
    if (!is.null(block$variables) && length(block$variables)) {
      paste0("**Variables used:** ", paste(paste0("`", block$variables, "`"), collapse = "; "))
    },
    if (!is.null(block$variables) && length(block$variables)) "",
    if (!is.null(block$csv_sources) && length(block$csv_sources)) {
      paste0("**CSV universe:** ", paste(paste0("`", block$csv_sources, "`"), collapse = ", "))
    },
    if (!is.null(block$csv_sources) && length(block$csv_sources)) "",
    .export_recipe_md(block$recipe, include_recipes = include_recipes)
  )
  if (isTRUE(include_recipes) && !is.null(block$figure_spec) && !is.null(block$figure_spec$file)) {
    parts <- c(
      parts,
      paste0("*Chart id (for recreate / naming):* `", block$figure_spec$file, "`"),
      ""
    )
  }
  if (!is.null(block$stats) && is.data.frame(block$stats) && nrow(block$stats)) {
    parts <- c(parts, "**Summary stats**", "", .export_md_table(block$stats), "")
  }
  if (length(block$tables)) {
    for (nm in names(block$tables)) {
      parts <- c(parts, paste0("**", nm, "**"), "", .export_md_table(block$tables[[nm]]), "")
    }
  }
  paste(parts, collapse = "\n")
}

.export_parse_keep_in_touch <- function(cell) {
  if (exists(".parse_keep_in_touch_cell", mode = "function")) {
    return(tryCatch(.parse_keep_in_touch_cell(cell), error = function(e) character(0)))
  }
  if (!exists("KEEP_IN_TOUCH_CANONICAL_LABELS")) return(character(0))
  s <- trimws(.export_flatten_chr(cell)[1])
  if (is.na(s) || !nzchar(s)) return(character(0))
  labs <- KEEP_IN_TOUCH_CANONICAL_LABELS
  hit <- labs[vapply(labs, function(lab) grepl(lab, s, fixed = TRUE), logical(1))]
  unique(hit)
}

#' Lightweight word frequency for exports (no tm dependency / no full-corpus crash risk).
.export_simple_word_freq <- function(text_vec, top_n = 40L) {
  text_vec <- .export_flatten_chr(text_vec)
  text_vec <- text_vec[!is.na(text_vec) & nzchar(trimws(text_vec))]
  if (!length(text_vec)) {
    return(data.frame(word = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }
  blob <- tolower(paste(text_vec, collapse = " "))
  blob <- gsub("[^a-z0-9'\\s]+", " ", blob)
  toks <- unlist(strsplit(blob, "\\s+"), use.names = FALSE)
  toks <- toks[nzchar(toks) & nchar(toks) >= 3L]
  stop <- c(
    "the", "and", "for", "that", "with", "this", "from", "have", "was", "are",
    "but", "not", "you", "your", "about", "more", "been", "they", "their",
    "will", "would", "could", "should", "just", "like", "really", "very",
    "buckets", "workshop", "session", "program", "today", "money"
  )
  toks <- toks[!toks %in% stop]
  if (!length(toks)) {
    return(data.frame(word = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }
  tbl <- sort(table(toks), decreasing = TRUE)
  utils::head(
    data.frame(word = names(tbl), freq = as.integer(tbl), stringsAsFactors = FALSE),
    top_n
  )
}

.export_n_base_sessions <- function(df) {
  if (is.null(df) || !nrow(df) || !"session_id" %in% names(df)) return(0L)
  s <- trimws(as.character(df$session_id))
  s <- s[!is.na(s) & nzchar(s)]
  if (!length(s)) return(0L)
  if (exists("base_session_id", mode = "function")) {
    length(unique(base_session_id(s)))
  } else {
    length(unique(s))
  }
}

# -----------------------------------------------------------------------------
# Bundle collection
# -----------------------------------------------------------------------------

.export_df_flatten_cols <- function(df, cols = NULL) {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(df)
  targets <- if (is.null(cols)) names(df) else intersect(cols, names(df))
  for (nm in targets) {
    if (is.list(df[[nm]]) && !is.data.frame(df[[nm]])) {
      df[[nm]] <- .export_flatten_chr(df[[nm]])
    }
  }
  df
}

.export_collect_bundle <- function(input,
                                   filtered_pre, filtered_post, filtered_annual,
                                   filtered_big_pre, filtered_little_pre,
                                   filtered_little_post, filtered_big_post_only,
                                   session_summary_data = NULL) {
  snap <- .export_filter_snapshot_list(input)
  pre_all <- tryCatch(filtered_pre(), error = function(e) data.frame())
  post_all <- tryCatch(filtered_post(), error = function(e) data.frame())
  annual <- tryCatch({
    if (is.null(filtered_annual)) data.frame() else filtered_annual()
  }, error = function(e) data.frame())
  big_pre <- tryCatch(filtered_big_pre(), error = function(e) data.frame())
  little_pre <- tryCatch(filtered_little_pre(), error = function(e) data.frame())
  little_post <- tryCatch(filtered_little_post(), error = function(e) data.frame())
  big_post <- tryCatch(filtered_big_post_only(), error = function(e) data.frame())
  ss <- tryCatch({
    if (is.null(session_summary_data)) data.frame() else session_summary_data()
  }, error = function(e) data.frame())

  # Flatten googlesheets4 list-columns used by indices / NPS / open text
  flatten_need <- unique(c(
    if (exists("PRE_FINANCIAL_WELLNESS_COLS")) PRE_FINANCIAL_WELLNESS_COLS else character(0),
    if (exists("POST_COMPARED_TO_COLS")) POST_COMPARED_TO_COLS else character(0),
    if (exists("PRE_PAST_BEHAVIORS_COL")) PRE_PAST_BEHAVIORS_COL else character(0),
    if (exists("POST_PLANNED_ACTIONS_COL")) POST_PLANNED_ACTIONS_COL else character(0),
    if (exists("POST_NPS_COL")) POST_NPS_COL else character(0),
    if (exists("POST_KEEP_IN_TOUCH_COL")) POST_KEEP_IN_TOUCH_COL else character(0),
    if (exists("POST_IMPACT_STORY_COL")) POST_IMPACT_STORY_COL else character(0),
    if (exists("PRE_OPENING_COLS")) PRE_OPENING_COLS else character(0),
    "How satisfied were you with today's 5 Buckets session?",
    "modules_taught", "Age Group", "Gender Identity", "Household Income",
    "Race/Ethnicity (Select all that apply)"
  ))
  big_pre <- .export_df_flatten_cols(big_pre, flatten_need)
  big_post <- .export_df_flatten_cols(big_post, flatten_need)
  annual <- .export_df_flatten_cols(annual)

  wellness <- tryCatch(calculate_wellness_index(big_pre), error = function(e) numeric(0))
  behav_pre <- tryCatch(calculate_behavioral_index(big_pre), error = function(e) numeric(0))
  impact <- tryCatch(calculate_post_impact_index(big_post), error = function(e) numeric(0))
  planned <- tryCatch(calculate_planned_actions_index(big_post), error = function(e) numeric(0))
  quality <- tryCatch(calculate_quality_index(big_post), error = function(e) numeric(0))
  wellness <- .export_flatten_num(wellness)
  behav_pre <- .export_flatten_num(behav_pre)
  impact <- .export_flatten_num(impact)
  planned <- .export_flatten_num(planned)
  quality <- .export_flatten_num(quality)

  past_counts <- tryCatch(get_past_behaviors_counts(big_pre), error = function(e) data.frame())
  planned_counts <- tryCatch(get_planned_actions_counts(big_post), error = function(e) data.frame())

  nps_col <- if (exists("POST_NPS_COL")) POST_NPS_COL else "How likely are you to recommend 5 Buckets to a friend or colleague?"
  nps <- if (nps_col %in% names(big_post)) {
    tryCatch(.export_nps_bucket(big_post[[nps_col]]), error = function(e) .export_nps_bucket(numeric(0)))
  } else {
    .export_nps_bucket(numeric(0))
  }

  pairing <- tryCatch({
    compute_respondent_pairing_stats(big_pre, little_pre, little_post, big_post, annual)
  }, error = function(e) NULL)

  org_journeys <- tryCatch({
    if (exists("calculate_organization_journeys", mode = "function") && nrow(ss)) {
      calculate_organization_journeys(ss)
    } else {
      data.frame()
    }
  }, error = function(e) data.frame())

  list(
    snap = snap,
    slug = .export_slug_from_filters(snap),
    sections = snap$export_sections %||% unname(EXPORT_SECTION_CHOICES),
    include_figures = FALSE,
    include_figure_recipes = TRUE,
    pre_all = pre_all,
    post_all = post_all,
    annual = annual,
    big_pre = big_pre,
    little_pre = little_pre,
    little_post = little_post,
    big_post = big_post,
    session_summary = ss,
    org_journeys = org_journeys,
    indices = list(
      wellness = wellness,
      behavioral_pre = behav_pre,
      post_impact = impact,
      planned_actions = planned,
      quality = quality
    ),
    past_counts = past_counts,
    planned_counts = planned_counts,
    nps = nps,
    pairing = pairing
  )
}

# -----------------------------------------------------------------------------
# Section collectors
# -----------------------------------------------------------------------------

.export_collect_overview <- function(bundle) {
  blocks <- list()
  bp_s <- .export_n_base_sessions(bundle$big_pre)
  lp_s <- .export_n_base_sessions(bundle$little_pre)
  lpo_s <- .export_n_base_sessions(bundle$little_post)
  bpo_s <- .export_n_base_sessions(bundle$big_post)
  sess_total <- length(unique(c(
    if (nrow(bundle$big_pre) && "session_id" %in% names(bundle$big_pre)) {
      if (exists("base_session_id", mode = "function")) base_session_id(bundle$big_pre$session_id) else bundle$big_pre$session_id
    } else character(0),
    if (nrow(bundle$little_pre) && "session_id" %in% names(bundle$little_pre)) {
      if (exists("base_session_id", mode = "function")) base_session_id(bundle$little_pre$session_id) else bundle$little_pre$session_id
    } else character(0),
    if (nrow(bundle$little_post) && "session_id" %in% names(bundle$little_post)) {
      if (exists("base_session_id", mode = "function")) base_session_id(bundle$little_post$session_id) else bundle$little_post$session_id
    } else character(0),
    if (nrow(bundle$big_post) && "session_id" %in% names(bundle$big_post)) {
      if (exists("base_session_id", mode = "function")) base_session_id(bundle$big_post$session_id) else bundle$big_post$session_id
    } else character(0)
  )))
  desc <- data.frame(
    Metric = c("Sessions", "Responses"),
    `Big Pre` = c(bp_s, .export_safe_n(bundle$big_pre)),
    `Little Pre` = c(lp_s, .export_safe_n(bundle$little_pre)),
    `Little Post` = c(lpo_s, .export_safe_n(bundle$little_post)),
    `Big Post` = c(bpo_s, .export_safe_n(bundle$big_post)),
    `Session Total` = c(
      sess_total,
      .export_safe_n(bundle$big_pre) + .export_safe_n(bundle$little_pre) +
        .export_safe_n(bundle$little_post) + .export_safe_n(bundle$big_post)
    ),
    `Annual Survey` = c(NA_integer_, .export_safe_n(bundle$annual)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "overview_response_descriptives",
    path = "Overview > Summary Stats > Response Descriptives",
    title = "Response Descriptives by survey type",
    tables = list("By survey type" = desc),
    recipe = list(
      chart_type = "table",
      universe = "filtered Big/Little Pre/Post + Annual",
      aggregation = "response counts; sessions = distinct base_session_id"
    ),
    figure_spec = list(kind = "table", table = desc, file = "overview_response_descriptives.png")
  )

  if (!is.null(bundle$pairing)) {
    p <- bundle$pairing
    pair_df <- data.frame(
      Metric = c(
        "Distinct valid IDs", "Big Pre IDs", "Big Post IDs",
        "Big Pre ∩ Big Post IDs", "Continuers (+ Annual)",
        "% pairs of Big Pre IDs", "% continuers of pairs"
      ),
      Value = c(
        p$distinct_all %||% NA, p$big_pre %||% NA, p$big_post %||% NA,
        p$big_pre_post_pairs %||% NA, p$continuers %||% NA,
        p$pct_pairs_of_big_pre %||% NA, p$pct_continuers_of_pairs %||% NA
      ),
      stringsAsFactors = FALSE
    )
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "overview_respondent_pairing",
      path = "Overview > Summary Stats > Respondent IDs",
      title = "Respondent pairing",
      tables = list("Pairing" = pair_df),
      recipe = list(
        chart_type = "summary_table",
        universe = "valid non-ANON respondent_id",
        aggregation = "set intersection Big Pre ∩ Big Post ∩ Annual"
      )
    )
  }

  if (.export_safe_n(bundle$org_journeys) > 0) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "overview_series_length",
      path = "Overview > Series Length by Org",
      title = "Series length by organization",
      tables = list("Organization journeys" = utils::head(bundle$org_journeys, 100L)),
      recipe = list(
        chart_type = "table",
        universe = "session_summary series",
        aggregation = "Len1-Len6 buckets from PM sessions_in_series"
      ),
      figure_spec = list(kind = "table", table = utils::head(bundle$org_journeys, 40L),
                         file = "overview_series_length.png")
    )
  }
  blocks
}

.export_demo_wave_table <- function(df, col_exact = NULL, col_pattern = NULL, label = "Level",
                                    normalize = NULL) {
  col <- .export_find_col(df, exact = col_exact, pattern = col_pattern)
  if (is.null(col) || !nrow(df)) {
    return(data.frame(Level = character(0), N = integer(0), Pct = numeric(0), stringsAsFactors = FALSE))
  }
  vec <- df[[col]]
  if (is.function(normalize)) vec <- normalize(vec)
  .export_count_table(vec, label = label)
}

.export_collect_reach <- function(bundle) {
  blocks <- list()
  include_ann <- isTRUE(bundle$snap$include_annual_in_reach)
  edu_norm <- if (exists(".normalize_education_for_dashboard", mode = "function")) {
    .normalize_education_for_dashboard
  } else {
    NULL
  }
  demos <- list(
    list(name = "Age", exact = "Age Group", pattern = "^Age Group$", norm = NULL),
    list(name = "Gender", exact = "Gender Identity", pattern = "Gender Identity", norm = NULL),
    list(name = "Household Income", exact = "Household Income", pattern = "Household Income", norm = NULL),
    list(name = "Education", exact = NULL, pattern = "highest level of education", norm = edu_norm),
    list(name = "Race/Ethnicity", exact = NULL, pattern = "Race/Ethnicity", norm = NULL),
    list(name = "First-Gen College", exact = "First-Generation Status (College)", pattern = "First-Generation Status \\(College\\)", norm = NULL),
    list(name = "First-Gen U.S.", exact = "First-Generation Status (U.S.)", pattern = "First-Generation Status \\(U", norm = NULL),
    list(name = "Veteran Status", exact = "Veteran Status", pattern = "^Veteran Status$", norm = NULL),
    list(name = "Disability Status", exact = "Disability Status", pattern = "^Disability Status$", norm = NULL),
    list(name = "Neurodivergent", exact = "Do you identify as neurodivergent?", pattern = "neurodivergent", norm = NULL)
  )
  for (d in demos) {
    pre_t <- .export_demo_wave_table(bundle$big_pre, d$exact, d$pattern, d$name, d$norm)
    post_t <- .export_demo_wave_table(bundle$big_post, d$exact, d$pattern, d$name, d$norm)
    tables <- list("Big Pre" = pre_t, "Big Post" = post_t)
    if (include_ann) {
      tables[["Annual"]] <- .export_demo_wave_table(bundle$annual, d$exact, d$pattern, d$name, d$norm)
    }
    # Side-by-side merge on level
    levels_all <- unique(c(pre_t[[1]], post_t[[1]], if (include_ann) tables$Annual[[1]] else character(0)))
    levels_all <- levels_all[!is.na(levels_all) & nzchar(as.character(levels_all))]
    if (!length(levels_all)) next
    if (length(levels_all)) {
      merged <- data.frame(Level = levels_all, stringsAsFactors = FALSE)
      merged$Pre_N <- pre_t$N[match(levels_all, pre_t[[1]])]
      merged$Pre_Pct <- pre_t$Pct[match(levels_all, pre_t[[1]])]
      merged$Post_N <- post_t$N[match(levels_all, post_t[[1]])]
      merged$Post_Pct <- post_t$Pct[match(levels_all, post_t[[1]])]
      if (include_ann) {
        merged$Annual_N <- tables$Annual$N[match(levels_all, tables$Annual[[1]])]
        merged$Annual_Pct <- tables$Annual$Pct[match(levels_all, tables$Annual[[1]])]
      }
      merged[is.na(merged)] <- 0
      tables <- list("Pre vs Post (and Annual)" = merged)
    }
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = paste0("reach_", gsub("[^A-Za-z0-9]+", "_", tolower(d$name))),
      path = paste0("Reach > Demographic Distributions > ", d$name),
      title = d$name,
      why = "Describe who is being served for partner / equity reporting.",
      variables = c(d$exact %||% d$pattern),
      csv_sources = c("big_pre.csv", "big_post.csv", if (include_ann) "annual.csv" else NULL),
      tables = tables,
      recipe = list(
        chart_type = "grouped_bar",
        x = d$name,
        y = "count or percent of wave",
        universe = "Big Pre / Big Post (+ Annual if export noted)",
        aggregation = "category counts; Pct = share within wave",
        recreate = "grouped bars Pre vs Post (vs Annual); use N or % per display flags"
      ),
      figure_spec = list(
        kind = "bar",
        table = if (length(tables)) tables[[1]] else data.frame(),
        category_col = 1L,
        value_col = if (length(tables) && "Pre_N" %in% names(tables[[1]])) "Pre_N" else "N",
        file = paste0("reach_", gsub("[^A-Za-z0-9]+", "_", tolower(d$name)), ".png")
      )
    )
  }
  blocks
}

.export_collect_wellness <- function(bundle) {
  blocks <- list()
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "wellness_pre_index",
    path = "Financial Wellness > Pre > Summary Score",
    title = "Financial Wellness Index (Big Pre)",
    why = "Baseline money-mindset at workshop start (absolute agreement).",
    variables = if (exists("PRE_FINANCIAL_WELLNESS_COLS")) PRE_FINANCIAL_WELLNESS_COLS else character(0),
    csv_sources = "big_pre.csv",
    stats = .export_index_summary_row("Financial Wellness Index", bundle$indices$wellness),
    recipe = list(
      chart_type = "histogram",
      x = "index (−3 to +3)",
      y = "count of respondents",
      universe = "Big Pre",
      score_map = "Strongly Disagree=-3, Disagree=-1, Agree=+1, Strongly Agree=+3; index = mean of 8 items",
      recreate = "hist of index values; optional density curve; mark mean"
    ),
    figure_spec = list(
      kind = "hist",
      values = bundle$indices$wellness,
      file = "wellness_pre_index.png",
      title = "Financial Wellness Index"
    )
  )
  if (exists("PRE_FINANCIAL_WELLNESS_COLS") && nrow(bundle$big_pre)) {
    item_rows <- lapply(PRE_FINANCIAL_WELLNESS_COLS, function(col) {
      if (!col %in% names(bundle$big_pre)) return(NULL)
      short <- sub(".*\\[(.*)\\].*", "\\1", col)
      tbl <- .export_likert_table(bundle$big_pre[[col]])
      if (!nrow(tbl)) return(NULL)
      cbind(Item = short, tbl, stringsAsFactors = FALSE)
    })
    item_rows <- item_rows[!vapply(item_rows, is.null, logical(1))]
    if (length(item_rows)) {
      items_df <- do.call(rbind, item_rows)
      blocks[[length(blocks) + 1L]] <- .export_block(
        id = "wellness_pre_items",
        path = "Financial Wellness > Pre > Each Financial Wellness Metric",
        title = "Pre item distributions",
        why = "Which baseline beliefs are strong vs weak before the workshop.",
        variables = PRE_FINANCIAL_WELLNESS_COLS,
        csv_sources = "big_pre.csv",
        tables = list("Likert counts" = items_df),
        recipe = list(
          chart_type = "stacked_or_facet_bar",
          universe = "Big Pre",
          score_map = "4-point agreement",
          recreate = "one bar panel per item; x = response; y = N or %"
        )
      )
    }
  }
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "wellness_post_index",
    path = "Financial Wellness > Post > Summary Score",
    title = "Post Impact Index (Big Post)",
    why = "Perceived improvement after 5 Buckets (not the same question wording as Pre).",
    variables = if (exists("POST_COMPARED_TO_COLS")) POST_COMPARED_TO_COLS else character(0),
    csv_sources = "big_post.csv",
    stats = .export_index_summary_row("Post Impact Index", bundle$indices$post_impact),
    recipe = list(
      chart_type = "histogram",
      x = "index (−3 to +3)",
      y = "count",
      universe = "Big Post only",
      score_map = "same 4-point coding; index = mean of 9 Compared-to-before items",
      recreate = "hist of Post Impact Index; mark mean"
    ),
    figure_spec = list(
      kind = "hist",
      values = bundle$indices$post_impact,
      file = "wellness_post_index.png",
      title = "Post Impact Index"
    )
  )
  if (exists("POST_COMPARED_TO_COLS") && nrow(bundle$big_post)) {
    item_rows <- lapply(POST_COMPARED_TO_COLS, function(col) {
      if (!col %in% names(bundle$big_post)) return(NULL)
      short <- sub(".*\\[(.*)\\].*", "\\1", col)
      tbl <- .export_likert_table(bundle$big_post[[col]])
      if (!nrow(tbl)) return(NULL)
      cbind(Item = short, tbl, stringsAsFactors = FALSE)
    })
    item_rows <- item_rows[!vapply(item_rows, is.null, logical(1))]
    if (length(item_rows)) {
      items_df <- do.call(rbind, item_rows)
      blocks[[length(blocks) + 1L]] <- .export_block(
        id = "wellness_post_items",
        path = "Financial Wellness > Post > Each Compared-to-before Metric",
        title = "Post item distributions",
        why = "Where learners report the strongest perceived gains.",
        variables = POST_COMPARED_TO_COLS,
        csv_sources = "big_post.csv",
        tables = list("Likert counts" = items_df),
        recipe = list(
          chart_type = "stacked_or_facet_bar",
          universe = "Big Post only",
          recreate = "facet/stack by Compared-to item"
        )
      )
    }
  }

  # --- Pre vs Post (priority) ---
  between_idx <- .export_indep_ttest_row(
    "Index: Financial Wellness (Pre) vs Post Impact (Post)",
    bundle$indices$wellness,
    bundle$indices$post_impact
  )
  paired_idx <- .export_pair_index_df(
    bundle$big_pre, bundle$big_post,
    bundle$indices$wellness, bundle$indices$post_impact
  )
  within_idx <- .export_paired_ttest_row("Index: paired Pre → Post", paired_idx)
  change_hist <- if (nrow(paired_idx)) {
    .export_change_hist_table(paired_idx$post - paired_idx$pre, breaks = seq(-6, 6, by = 1))
  } else {
    data.frame()
  }

  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "wellness_between_index",
    path = "Financial Wellness > Pre vs Post > Between-subjects",
    title = "Pre vs Post — between-subjects (index)",
    why = "Cross-sectional contrast of Pre baseline vs Post perceived-change index (all respondents in each wave).",
    narrative = paste0(
      "Between-subjects compares the **mean of all Pre wellness indices** to the **mean of all Post impact indices** ",
      "(independent-samples t-test). Waves need not be the same people. ",
      "**Caveat:** constructs differ (absolute vs compared-to-before) — use for descriptive contrast, not pure causal effect."
    ),
    variables = c("Financial Wellness Index", "Post Impact Index"),
    csv_sources = c("big_pre.csv", "big_post.csv"),
    tables = list("Between-subjects test" = between_idx),
    recipe = list(
      chart_type = "overlaid_histogram",
      x = "index (−3..+3)",
      series = "Pre index + Post index",
      universe = "Big Pre vs Big Post (unpaired)",
      recreate = "overlay two histograms (and optional densities); annotate means; report t-test from table"
    ),
    figure_spec = list(
      kind = "hist",
      values = c(bundle$indices$wellness, bundle$indices$post_impact),
      file = "wellness_between_index.png",
      title = "Between-subjects Pre vs Post indices"
    )
  )

  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "wellness_within_index",
    path = "Financial Wellness > Pre vs Post > Within-subjects",
    title = "Pre vs Post — within-subjects (paired index)",
    why = "Person-level change for learners with both Big Pre and Big Post IDs.",
    narrative = paste0(
      "Within-subjects keeps **only** respondents with valid non-ANON `respondent_id` in both Big Pre and Big Post. ",
      "Change = Post Impact Index − Pre Wellness Index for each person (paired t-test). ",
      "Still note the Pre/Post construct difference when narrating."
    ),
    variables = c("respondent_id", "Financial Wellness Index", "Post Impact Index"),
    csv_sources = c("big_pre.csv", "big_post.csv"),
    tables = list(
      "Paired t-test" = within_idx,
      "Change-score distribution (Post−Pre bins)" = change_hist
    ),
    recipe = list(
      chart_type = "slope_graph + change_histogram",
      universe = "paired respondent_id in Big Pre ∩ Big Post",
      y_slope = "index at Pre and Post",
      y_change = "Post − Pre",
      recreate = "left: Pre→Post lines per ID (+ mean slope); right: hist of change scores with mean/CI"
    ),
    figure_spec = list(
      kind = "hist",
      values = if (nrow(paired_idx)) paired_idx$post - paired_idx$pre else numeric(0),
      file = "wellness_within_change.png",
      title = "Paired change (Post − Pre)"
    )
  )

  between_items <- .export_wellness_item_between_table(bundle$big_pre, bundle$big_post)
  if (nrow(between_items)) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "wellness_between_items",
      path = "Financial Wellness > Pre vs Post > Between-subjects > By item",
      title = "Pre vs Post — between-subjects by item",
      why = "Which wellness concepts show the largest Post−Pre mean gaps (unpaired).",
      variables = c(vapply(.export_wellness_item_pairs(), `[[`, "", "pre"),
                    vapply(.export_wellness_item_pairs(), `[[`, "", "post")),
      csv_sources = c("big_pre.csv", "big_post.csv"),
      tables = list("Item independent t-tests (sorted by Diff)" = between_items),
      recipe = list(
        chart_type = "horizontal_bar",
        x = "mean Diff (Post − Pre)",
        y = "item label",
        recreate = "hbar of Diff with 95% CI whiskers; sort descending"
      ),
      figure_spec = list(
        kind = "bar",
        table = between_items,
        category_col = "Item",
        value_col = "Diff",
        file = "wellness_between_items.png"
      )
    )
  }

  within_items <- .export_wellness_item_within_table(bundle$big_pre, bundle$big_post)
  if (nrow(within_items)) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "wellness_within_items",
      path = "Financial Wellness > Pre vs Post > Within-subjects > By item",
      title = "Pre vs Post — within-subjects by item",
      why = "Paired person-level change on each aligned concept key.",
      csv_sources = c("big_pre.csv", "big_post.csv"),
      tables = list("Item paired t-tests (sorted by Mean_change)" = within_items),
      recipe = list(
        chart_type = "horizontal_bar",
        x = "mean change (Post − Pre) among pairs",
        y = "item",
        recreate = "hbar of Mean_change with CI; sort descending"
      ),
      figure_spec = list(
        kind = "bar",
        table = within_items,
        category_col = "Item",
        value_col = "Mean_change",
        file = "wellness_within_items.png"
      )
    )
  }

  blocks
}

.export_collect_behavioral <- function(bundle) {
  blocks <- list()
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_pre_index",
    path = "Behavioral Readiness > Pre > Summary Score",
    title = "Behavioral Index (past behaviors count)",
    why = "How many positive money behaviors people already did before the workshop.",
    variables = if (exists("PRE_PAST_BEHAVIORS_COL")) PRE_PAST_BEHAVIORS_COL else "past behaviors",
    csv_sources = "big_pre.csv",
    stats = .export_index_summary_row("Behavioral Index (0-8)", bundle$indices$behavioral_pre),
    recipe = list(
      chart_type = "histogram",
      x = "count of canonical past behaviors (0–8)",
      y = "respondents",
      universe = "Big Pre",
      recreate = "hist of per-person distinct canonical label counts"
    ),
    figure_spec = list(kind = "hist", values = bundle$indices$behavioral_pre,
                       file = "behavioral_pre_index.png", title = "Behavioral Index")
  )
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_past_items",
    path = "Behavioral Readiness > Pre > Each Past Behavior",
    title = "Past behaviors — item counts",
    why = "Which behaviors were already common at Pre.",
    csv_sources = "big_pre.csv",
    tables = list("Item counts" = bundle$past_counts),
    recipe = list(
      chart_type = "horizontal_bar",
      x = "canonical behavior label",
      y = "respondent endorsements",
      universe = "Big Pre",
      recreate = "hbar of endorsement counts (or % of Big Pre N)"
    ),
    figure_spec = list(
      kind = "bar",
      table = bundle$past_counts,
      category_col = 1L,
      value_col = 2L,
      file = "behavioral_past_items.png"
    )
  )
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_post_index",
    path = "Behavioral Readiness > Post > Summary Score",
    title = "Planned Actions Index",
    why = "Intent/activation after the workshop (planned or already done).",
    variables = if (exists("POST_PLANNED_ACTIONS_COL")) POST_PLANNED_ACTIONS_COL else "planned actions",
    csv_sources = "big_post.csv",
    stats = .export_index_summary_row("Planned Actions Index (0-8)", bundle$indices$planned_actions),
    recipe = list(
      chart_type = "histogram",
      x = "count of canonical planned actions (0–8)",
      universe = "Big Post only",
      recreate = "hist of planned-action counts"
    ),
    figure_spec = list(kind = "hist", values = bundle$indices$planned_actions,
                       file = "behavioral_post_index.png", title = "Planned Actions Index")
  )
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_planned_items",
    path = "Behavioral Readiness > Post > Each Planned Action",
    title = "Planned actions — item counts",
    why = "Which next steps learners commit to.",
    csv_sources = "big_post.csv",
    tables = list("Item counts" = bundle$planned_counts),
    recipe = list(
      chart_type = "horizontal_bar",
      universe = "Big Post only",
      recreate = "hbar of planned-action endorsements"
    ),
    figure_spec = list(
      kind = "bar",
      table = bundle$planned_counts,
      category_col = 1L,
      value_col = 2L,
      file = "behavioral_planned_items.png"
    )
  )

  between_idx <- .export_indep_ttest_row(
    "Index: Behavioral (Pre) vs Planned Actions (Post)",
    bundle$indices$behavioral_pre,
    bundle$indices$planned_actions
  )
  paired_idx <- .export_pair_index_df(
    bundle$big_pre, bundle$big_post,
    bundle$indices$behavioral_pre, bundle$indices$planned_actions
  )
  within_idx <- .export_paired_ttest_row("Index: paired past → planned", paired_idx)
  change_hist <- if (nrow(paired_idx)) {
    .export_change_hist_table(paired_idx$post - paired_idx$pre, breaks = seq(-8, 8, by = 1))
  } else {
    data.frame()
  }

  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_between_index",
    path = "Behavioral Readiness > Pre vs Post > Between-subjects",
    title = "Pre vs Post — between-subjects (behavior indices)",
    why = "Compare average past-behavior counts vs planned-action counts across waves.",
    narrative = "Independent contrast of Pre behavioral index vs Post planned-actions index (not necessarily same people).",
    csv_sources = c("big_pre.csv", "big_post.csv"),
    tables = list("Between-subjects test" = between_idx),
    recipe = list(
      chart_type = "overlaid_histogram",
      x = "index 0–8",
      series = "Pre behavioral + Post planned",
      recreate = "overlay histograms; annotate means; use table for t-test"
    ),
    figure_spec = list(
      kind = "hist",
      values = c(bundle$indices$behavioral_pre, bundle$indices$planned_actions),
      file = "behavioral_between_index.png",
      title = "Between-subjects behavior indices"
    )
  )

  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "behavioral_within_index",
    path = "Behavioral Readiness > Pre vs Post > Within-subjects",
    title = "Pre vs Post — within-subjects (paired indices)",
    why = "Same-person movement from past behaviors to planned actions.",
    csv_sources = c("big_pre.csv", "big_post.csv"),
    variables = c("respondent_id", "Behavioral Index", "Planned Actions Index"),
    tables = list(
      "Paired t-test" = within_idx,
      "Change-score distribution (Post−Pre bins)" = change_hist
    ),
    recipe = list(
      chart_type = "slope_graph + change_histogram",
      universe = "paired respondent_id",
      recreate = "Pre→Post slopes + hist of (planned − past) counts"
    ),
    figure_spec = list(
      kind = "hist",
      values = if (nrow(paired_idx)) paired_idx$post - paired_idx$pre else numeric(0),
      file = "behavioral_within_change.png",
      title = "Paired behavior change"
    )
  )

  item_btw <- data.frame()
  n_pre <- sum(is.finite(.export_flatten_num(bundle$indices$behavioral_pre)))
  n_post <- sum(is.finite(.export_flatten_num(bundle$indices$planned_actions)))
  if (n_pre > 0 && n_post > 0) {
    item_btw <- .export_behavior_item_between_table(
      bundle$past_counts, bundle$planned_counts, n_pre, n_post
    )
  }
  if (nrow(item_btw)) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "behavioral_between_items",
      path = "Behavioral Readiness > Pre vs Post > Between-subjects by item",
      title = "Pre vs Post — behavior item share differences",
      why = "Which actions rise most from past endorsement to planned endorsement (percentage points).",
      narrative = paste0(
        "Diff_pp = Post % − Pre % (percentage points). Denominators: Pre N = ", n_pre,
        ", Post N = ", n_post, " respondents with a computable index."
      ),
      csv_sources = c("big_pre.csv", "big_post.csv"),
      tables = list("Item share contrast (sorted by Diff_pp)" = item_btw),
      recipe = list(
        chart_type = "horizontal_bar",
        x = "Diff_pp (Post % − Pre %)",
        y = "behavior label",
        recreate = "hbar of Diff_pp sorted descending; note pp = percentage points"
      ),
      figure_spec = list(
        kind = "bar",
        table = item_btw,
        category_col = "Behavior",
        value_col = "Diff_pp",
        file = "behavioral_between_items.png"
      )
    )
  }

  blocks
}

.export_collect_satisfaction <- function(bundle) {
  blocks <- list()
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "satisfaction_session",
    path = "Satisfaction > Post > This workshop session",
    title = "Session satisfaction",
    stats = .export_index_summary_row("Session satisfaction", bundle$indices$quality),
    tables = list(
      "Distribution" = .export_count_table(bundle$indices$quality, label = "Score")
    ),
    recipe = list(
      chart_type = "histogram_or_bar",
      universe = "Post quality column (Big Post preferred)",
      y = "numeric session satisfaction"
    ),
    figure_spec = list(kind = "hist", values = bundle$indices$quality,
                       file = "satisfaction_session.png", title = "Session satisfaction")
  )
  nps <- bundle$nps
  nps_df <- data.frame(
    Metric = c("N", "Mean", "Promoters (9-10)", "Passives (7-8)", "Detractors (<=6)", "NPS"),
    Value = c(nps$n, nps$mean, nps$promoters, nps$passives, nps$detractors, nps$nps),
    stringsAsFactors = FALSE
  )
  nps_col <- if (exists("POST_NPS_COL")) POST_NPS_COL else NULL
  nps_dist <- if (!is.null(nps_col) && nps_col %in% names(bundle$big_post)) {
    .export_count_table(bundle$big_post[[nps_col]], label = "Score")
  } else {
    data.frame()
  }
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "satisfaction_nps",
    path = "Satisfaction > Post > Likelihood to recommend",
    title = "NPS / recommend",
    tables = list("NPS summary" = nps_df, "Score distribution" = nps_dist),
    recipe = list(
      chart_type = "bar",
      universe = "Big Post",
      aggregation = "NPS = %promoters - %detractors"
    ),
    figure_spec = list(
      kind = "bar",
      table = nps_dist,
      category_col = 1L,
      value_col = 2L,
      file = "satisfaction_nps.png"
    )
  )
  # By module (precise): mean sat by exact modules_taught string
  if (nrow(bundle$big_post) && "modules_taught" %in% names(bundle$big_post)) {
    sat <- .export_flatten_num(bundle$indices$quality)
    if (length(sat) == nrow(bundle$big_post)) {
      mod <- trimws(as.character(bundle$big_post$modules_taught))
      ok <- is.finite(sat) & !is.na(mod) & nzchar(mod)
      if (any(ok)) {
        sat_ok <- sat[ok]
        mod_ok <- mod[ok]
        levels_m <- sort(unique(mod_ok))
        mod_df <- data.frame(
          modules_taught = levels_m,
          N = vapply(levels_m, function(m) sum(mod_ok == m), integer(1)),
          Mean = vapply(levels_m, function(m) round(mean(sat_ok[mod_ok == m]), 2), numeric(1)),
          SD = vapply(levels_m, function(m) {
            z <- sat_ok[mod_ok == m]
            if (length(z) > 1L) round(stats::sd(z), 2) else NA_real_
          }, numeric(1)),
          stringsAsFactors = FALSE
        )
        mod_df <- mod_df[order(-mod_df$N), , drop = FALSE]
        blocks[[length(blocks) + 1L]] <- .export_block(
          id = "satisfaction_by_module",
          path = "Satisfaction > Across sessions > By module type > Precise",
          title = "Session satisfaction by exact module combination",
          tables = list("By modules_taught" = utils::head(mod_df, 40L)),
          recipe = list(
            chart_type = "bar_with_ci",
            x = "exact modules_taught string",
            y = "mean session satisfaction",
            universe = "Big Post rows with modules_taught"
          )
        )
      }
    }
  }
  blocks
}

.export_collect_learning <- function(bundle) {
  blocks <- list()
  # Topic understanding = first Compared-to item (understanding)
  und_col <- if (exists("POST_COMPARED_TO_COLS") && length(POST_COMPARED_TO_COLS)) {
    POST_COMPARED_TO_COLS[[1]]
  } else {
    .export_find_col(bundle$big_post, pattern = "better understanding of the topics")
  }
  if (!is.null(und_col) && und_col %in% names(bundle$big_post)) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "learning_topic_understanding",
      path = "Learning & Impact Stories > Topic understanding",
      title = "Topic understanding (Compared to before)",
      tables = list("Distribution" = .export_likert_table(bundle$big_post[[und_col]])),
      stats = .export_index_summary_row(
        "Topic understanding (scored)",
        if (exists("likert_to_numeric", mode = "function")) {
          tryCatch(likert_to_numeric(.export_flatten_chr(bundle$big_post[[und_col]])), error = function(e) numeric(0))
        } else {
          numeric(0)
        }
      ),
      recipe = list(
        chart_type = "bar",
        universe = "Big Post",
        score_map = "4-point agreement -> -3..+3"
      ),
      figure_spec = list(
        kind = "bar",
        table = .export_likert_table(bundle$big_post[[und_col]]),
        category_col = 1L,
        value_col = 2L,
        file = "learning_topic_understanding.png"
      )
    )
  }

  kt_col <- if (exists("POST_KEEP_IN_TOUCH_COL")) {
    .export_find_col(bundle$big_post, exact = POST_KEEP_IN_TOUCH_COL, pattern = "keep in touch")
  } else {
    .export_find_col(bundle$big_post, pattern = "keep in touch")
  }
  if (!is.null(kt_col) && nrow(bundle$big_post)) {
    all_labs <- unlist(lapply(bundle$big_post[[kt_col]], .export_parse_keep_in_touch), use.names = FALSE)
    kt_tbl <- if (length(all_labs)) {
      t0 <- sort(table(all_labs), decreasing = TRUE)
      data.frame(Interest = names(t0), N = as.integer(t0),
                 Pct_of_Big_Post = round(100 * as.integer(t0) / nrow(bundle$big_post), 1),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(Interest = character(0), N = integer(0), Pct_of_Big_Post = numeric(0))
    }
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "learning_keep_in_touch",
      path = "Learning & Impact Stories > Learning interests",
      title = "Keep-in-touch / learning interests",
      tables = list("Interest counts" = kt_tbl),
      recipe = list(
        chart_type = "horizontal_bar",
        universe = "Big Post",
        aggregation = "multi-select; rematch fragments to canonical labels"
      ),
      figure_spec = list(
        kind = "bar",
        table = kt_tbl,
        category_col = 1L,
        value_col = 2L,
        file = "learning_keep_in_touch.png"
      )
    )
  }

  # Word frequencies — light tokenizer (avoid full-corpus tm on download path)
  text_specs <- list()
  if (exists("POST_IMPACT_STORY_COL")) {
    text_specs[[length(text_specs) + 1L]] <- list(
      label = "Impact story (Post)",
      col = POST_IMPACT_STORY_COL,
      df = bundle$big_post
    )
  }
  if (exists("PRE_OPENING_COLS") && length(PRE_OPENING_COLS) >= 1L) {
    text_specs[[length(text_specs) + 1L]] <- list(
      label = "Intention (Pre)",
      col = PRE_OPENING_COLS[[1]],
      df = bundle$big_pre
    )
  }
  wf_tables <- list()
  for (sp in text_specs) {
    if (is.null(sp$col) || !sp$col %in% names(sp$df) || !nrow(sp$df)) next
    vec <- sp$df[[sp$col]]
    eng <- if (exists("open_text_for_mode", mode = "function")) {
      tryCatch(open_text_for_mode(vec, "english"), error = function(e) .export_flatten_chr(vec))
    } else {
      .export_flatten_chr(vec)
    }
    # Cap rows for export speed / stability
    if (length(eng) > 400L) eng <- eng[seq_len(400L)]
    wf <- tryCatch(.export_simple_word_freq(eng, top_n = 40L), error = function(e) data.frame())
    if (is.data.frame(wf) && nrow(wf)) {
      wf_tables[[sp$label]] <- wf
    }
  }
  if (length(wf_tables)) {
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "learning_word_freq",
      path = "Learning & Impact Stories > Wordcloud stories",
      title = "Open-text word frequencies (English side)",
      why = "Themes in impact stories / intentions without dumping raw PII text.",
      tables = wf_tables,
      narrative = "Full story dumps omitted for privacy/size. Frequencies use English side of bilingual cells.",
      variables = names(wf_tables),
      csv_sources = c("big_post.csv", "big_pre.csv"),
      recipe = list(
        chart_type = "wordcloud_or_bar",
        universe = "filtered Pre/Post open text",
        aggregation = "token frequency after stopword filtering",
        recreate = "hbar of top tokens or wordcloud sized by freq"
      )
    )
  }
  blocks
}

.export_collect_annual <- function(bundle) {
  blocks <- list()
  d <- bundle$annual
  blocks[[length(blocks) + 1L]] <- .export_block(
    id = "annual_counts",
    path = "Annual Survey",
    title = "Annual Survey response count",
    stats = data.frame(Metric = "Annual rows (filtered)", N = .export_safe_n(d), stringsAsFactors = FALSE),
    recipe = list(chart_type = "kpi", universe = "filtered Annual")
  )
  if (!nrow(d)) return(blocks)

  if (exists("annual_compared_agree_table", mode = "function")) {
    agr <- tryCatch(annual_compared_agree_table(d), error = function(e) data.frame())
    if (nrow(agr)) {
      blocks[[length(blocks) + 1L]] <- .export_block(
        id = "annual_compared",
        path = "Annual Survey > Compared to before",
        title = "Compared-to-before agree rates",
        tables = list("Agree %" = agr),
        recipe = list(
          chart_type = "horizontal_bar",
          y = "Agree + Strongly Agree share",
          universe = "Annual"
        ),
        figure_spec = list(
          kind = "bar",
          table = agr,
          category_col = 1L,
          value_col = "agree_pct",
          file = "annual_compared.png"
        )
      )
    }
  }
  if (exists("calculate_annual_compared_index", mode = "function")) {
    idx <- tryCatch(calculate_annual_compared_index(d), error = function(e) numeric(0))
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "annual_compared_index",
      path = "Annual Survey > Compared index",
      title = "Annual compared-to index",
      stats = .export_index_summary_row("Annual compared index", idx),
      recipe = list(chart_type = "histogram", universe = "Annual")
    )
  }
  if (exists("annual_find_col", mode = "function") && exists("ANNUAL_COL_PATTERNS")) {
    rec_col <- annual_find_col(d, ANNUAL_COL_PATTERNS$recommend)
    if (!is.null(rec_col)) {
      scores <- if (exists("annual_recommend_scores", mode = "function")) {
        annual_recommend_scores(d[[rec_col]])
      } else {
        suppressWarnings(as.numeric(d[[rec_col]]))
      }
      blocks[[length(blocks) + 1L]] <- .export_block(
        id = "annual_recommend",
        path = "Annual Survey > Recommend",
        title = "Annual recommend / NPS-style",
        stats = .export_index_summary_row("Annual recommend", scores),
        tables = list(
          "NPS buckets" = {
            b <- .export_nps_bucket(scores)
            data.frame(
              Metric = c("N", "Mean", "Promoters", "Passives", "Detractors", "NPS"),
              Value = c(b$n, b$mean, b$promoters, b$passives, b$detractors, b$nps),
              stringsAsFactors = FALSE
            )
          }
        ),
        recipe = list(chart_type = "bar", universe = "Annual", aggregation = "0-10 recommend")
      )
    }
  }
  if (exists("calculate_annual_behavior_index", mode = "function")) {
    bidx <- tryCatch(calculate_annual_behavior_index(d), error = function(e) numeric(0))
    blocks[[length(blocks) + 1L]] <- .export_block(
      id = "annual_behaviors",
      path = "Annual Survey > Behaviors",
      title = "Annual behavior index",
      stats = .export_index_summary_row("Annual behavior index", bidx),
      recipe = list(chart_type = "histogram", universe = "Annual")
    )
  }
  blocks
}

.export_collect_sections <- function(bundle) {
  out <- list()
  for (sid in bundle$sections %||% character(0)) {
    blocks <- tryCatch({
      switch(
        sid,
        overview = .export_collect_overview(bundle),
        reach = .export_collect_reach(bundle),
        wellness = .export_collect_wellness(bundle),
        behavioral = .export_collect_behavioral(bundle),
        satisfaction = .export_collect_satisfaction(bundle),
        learning = .export_collect_learning(bundle),
        annual = .export_collect_annual(bundle),
        list()
      )
    }, error = function(e) {
      list(.export_block(
        id = paste0(sid, "_error"),
        path = sid,
        title = paste0("Section error: ", sid),
        narrative = paste0("Could not build this section: ", conditionMessage(e))
      ))
    })
    title <- EXPORT_SECTION_REGISTRY[[sid]]$title %||% sid
    out[[sid]] <- list(id = sid, title = title, blocks = blocks)
  }
  out
}

# -----------------------------------------------------------------------------
# Report Markdown (filter-aware, hierarchical)
# -----------------------------------------------------------------------------

build_report_markdown <- function(bundle) {
  sections <- .export_collect_sections(bundle)
  include_recipes <- if (is.null(bundle$include_figure_recipes)) TRUE else isTRUE(bundle$include_figure_recipes)
  section_ids <- names(sections)

  parts <- c(
    "# 5 Buckets Impact Dashboard — Results (for AI)",
    "",
    "> Filter-scoped numeric export for a secondary AI. **Primary insight lanes:** Financial Wellness Pre↔Post,",
    "> Behavioral Pre↔Post, Reach, then Satisfaction / Stories.",
    "> Schema & scoring: pair with **Data Map (md for AI)**. Do not invent Likert maps or Big/Little rules.",
    "",
    .export_filter_snapshot_md(bundle$snap),
    "## Export scope",
    "",
    paste0("- Sections: ", .export_fmt_list(bundle$sections, empty = "(none)")),
    "- Figure recipes + context: always included (numeric-first; recreate charts from tables)",
    "",
    .export_results_meta_map_md(section_ids)
  )

  if (!length(sections)) {
    parts <- c(parts, "_No sections selected._", "")
  }

  # Priority section order for reading
  order_pref <- c("wellness", "behavioral", "reach", "satisfaction", "learning", "overview", "annual")
  ordered_ids <- c(intersect(order_pref, section_ids), setdiff(section_ids, order_pref))

  for (sid in ordered_ids) {
    sec <- sections[[sid]]
    parts <- c(parts, paste0("## ", sec$title), "")
    intro <- .export_section_intro_md(sid)
    if (nzchar(intro)) parts <- c(parts, intro, "")
    if (!length(sec$blocks)) {
      parts <- c(parts, "_No blocks for this section under current filters._", "")
      next
    }
    for (block in sec$blocks) {
      parts <- c(parts, .export_block_to_md(block, heading_level = 3L, include_recipes = include_recipes), "")
    }
  }

  parts <- c(
    parts,
    "## Guidance for the secondary AI",
    "",
    "1. Lead with **Financial Wellness Pre↔Post** and **Behavioral Pre↔Post** when drafting impact narrative.",
    "2. Always restate the filter snapshot and N; flag any analysis with N < 10.",
    "3. Post Compared-to / Post Impact Index = *perceived improvement*, not a raw Pre twin — say so explicitly.",
    "4. Prefer tables + figure recipes over inventing new charts; PNGs are optional.",
    "5. For row-level joins or custom cuts, use the CSV ZIP + Data Map recipes.",
    "6. Open-text here is frequency-only; pull full stories from CSV if quoting individuals (still no PII).",
    ""
  )
  paste(parts, collapse = "\n")
}

# Backward-compatible alias
build_analysis_markdown <- function(bundle) build_report_markdown(bundle)

# -----------------------------------------------------------------------------
# Data Map Markdown (filter-independent)
# -----------------------------------------------------------------------------

.export_backtick_list <- function(x) {
  if (is.null(x) || !length(x)) return("_Not loaded._")
  paste0("`", paste(x, collapse = "`, `"), "`")
}

.export_bullet_backticks <- function(x) {
  if (is.null(x) || !length(x)) return("_Not loaded._")
  paste0("- `", x, "`", collapse = "\n")
}

build_data_mapping_markdown <- function() {
  likert_note <- paste0(
    "4-point agreement (no Neutral): Strongly Disagree = -3, Disagree = -1, ",
    "Agree = +1, Strongly Agree = +3. Stray Neutral -> NA. Scale midpoint / null = 0."
  )
  pre_fw <- if (exists("PRE_FINANCIAL_WELLNESS_COLS")) .export_bullet_backticks(PRE_FINANCIAL_WELLNESS_COLS) else "_Not loaded._"
  post_cmp <- if (exists("POST_COMPARED_TO_COLS")) .export_bullet_backticks(POST_COMPARED_TO_COLS) else "_Not loaded._"
  behaviors <- if (exists("BEHAVIOR_ITEM_CANONICAL_LABELS")) {
    paste0("- ", BEHAVIOR_ITEM_CANONICAL_LABELS, collapse = "\n")
  } else {
    "_Not loaded._"
  }
  past_col <- if (exists("PRE_PAST_BEHAVIORS_COL")) PRE_PAST_BEHAVIORS_COL else "(past behaviors column)"
  planned_col <- if (exists("POST_PLANNED_ACTIONS_COL")) POST_PLANNED_ACTIONS_COL else "(planned actions column)"
  nps_col <- if (exists("POST_NPS_COL")) POST_NPS_COL else "(NPS column)"
  open_delim <- if (exists("OPEN_TEXT_DELIM")) OPEN_TEXT_DELIM else " (%-^-%) "
  keep_touch <- if (exists("KEEP_IN_TOUCH_CANONICAL_LABELS")) {
    paste0("- ", KEEP_IN_TOUCH_CANONICAL_LABELS, collapse = "\n")
  } else {
    "_Not loaded._"
  }
  pre_meta <- if (exists("PRE_METADATA_COLS")) .export_backtick_list(PRE_METADATA_COLS) else "_Not loaded._"
  pre_open <- if (exists("PRE_OPENING_COLS")) .export_backtick_list(PRE_OPENING_COLS) else "_Not loaded._"
  pre_demo <- if (exists("PRE_DEMOGRAPHICS_COLS")) .export_backtick_list(PRE_DEMOGRAPHICS_COLS) else "_Not loaded._"
  post_today <- if (exists("POST_TODAY_SESSION_COLS")) .export_backtick_list(POST_TODAY_SESSION_COLS) else "_Not loaded._"
  sat_max <- if (exists("SESSION_SATISFACTION_MAX")) as.integer(SESSION_SATISFACTION_MAX) else 6L

  comparable_md <- "_Not loaded._"
  if (exists("PRE_POST_COMPARABLE") && is.list(PRE_POST_COMPARABLE)) {
    rows <- lapply(names(PRE_POST_COMPARABLE), function(k) {
      pair <- PRE_POST_COMPARABLE[[k]]
      pre <- if (is.null(pair$pre) || length(pair$pre) == 0 || is.na(pair$pre)) "_(none)_" else paste0("`", pair$pre, "`")
      post <- if (is.null(pair$post) || length(pair$post) == 0 || is.na(pair$post)) "_(none)_" else paste0("`", pair$post, "`")
      paste0("| ", k, " | ", pre, " | ", post, " |")
    })
    comparable_md <- paste(c("| Concept key | Pre column | Post column |", "| --- | --- | --- |", rows), collapse = "\n")
  }

  # Short labels used in Results MD / charts
  pre_short <- c(
    "Know amount", "Know afford", "Know where", "Optimism", "Relationship",
    "Stress", "Confidence", "Comfort professionals"
  )
  post_short <- c(
    "Understanding", "Awareness: amount", "Awareness: afford", "Awareness: where",
    "Optimism", "Relationship", "Stress", "Confidence", "Comfort professionals"
  )
  alias_rows <- character(0)
  if (exists("PRE_FINANCIAL_WELLNESS_COLS") && length(PRE_FINANCIAL_WELLNESS_COLS) == length(pre_short)) {
    alias_rows <- c(alias_rows, paste0("| Pre | ", pre_short, " | `", PRE_FINANCIAL_WELLNESS_COLS, "` |"))
  }
  if (exists("POST_COMPARED_TO_COLS") && length(POST_COMPARED_TO_COLS) == length(post_short)) {
    alias_rows <- c(alias_rows, paste0("| Post | ", post_short, " | `", POST_COMPARED_TO_COLS, "` |"))
  }
  alias_md <- if (length(alias_rows)) {
    paste(c("| Wave | Short label | Full Master column |", "| --- | --- | --- |", alias_rows), collapse = "\n")
  } else {
    "_Aliases unavailable._"
  }

  income_bands_md <- .export_income_bands_md()
  column_matrix_md <- .export_column_analysis_matrix_md()
  join_recipes_md <- .export_join_recipes_md()
  example_shapes_md <- .export_example_shapes_md()
  education_order_md <- paste0("- ", .export_education_order(), collapse = "\n")

  annual_patterns_md <- if (exists("ANNUAL_COL_PATTERNS") && is.list(ANNUAL_COL_PATTERNS)) {
    paste(
      c(
        "| Concept | Header match pattern (substring / regex) |",
        "| --- | --- |",
        paste0("| ", names(ANNUAL_COL_PATTERNS), " | `", unlist(ANNUAL_COL_PATTERNS, use.names = FALSE), "` |")
      ),
      collapse = "\n"
    )
  } else {
    "_Annual patterns not loaded._"
  }

  paste(
    c(
      "# 5 Buckets — Data Map (for AI)",
      "",
      "> **Audience:** a secondary AI that has never seen this survey design.",
      "> **Job of this file:** give you enough institutional knowledge to analyze the CSV ZIP / Results MD",
      "> without guessing column meaning, scoring, or Pre↔Post relationships.",
      "> This Data Map is **filter-independent**. Pair it with a filtered Results MD + CSV ZIP for a slice.",
      "",
      paste0("Exported: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      "",
      "---",
      "",
      "## 1. What this dataset is (read first)",
      "",
      "**5 Buckets** runs financial-literacy workshops for partner organizations (schools, nonprofits, etc.).",
      "A partner series may be a single session or multiple sessions (modules such as Mindset, Manage, Borrow, Grow, Protect).",
      "",
      "Learners answer surveys around those workshops:",
      "",
      "| Instrument | When | What it captures |",
      "| --- | --- | --- |",
      "| **Pre** | Start of a workshop/session | Intentions, (on Big Pre) financial wellness Likert + past behaviors + demographics |",
      "| **Post** | End of a workshop/session | Reflections, satisfaction; (on Big Post) perceived change Likert + planned actions + NPS + story |",
      "| **Annual** | ~1 year later follow-up | Longer-term compared-to / behaviors / money-change / recommend; joinable via hashed respondent_id |",
      "| **Program Manager (PM)** | Ops schedule | `session_number`, `sessions_in_series`, modules, dates — used to label Big vs Little |",
      "",
      "**Impact story the org usually wants:** reach (who came), session quality (satisfaction/NPS),",
      "shifts in money mindset (wellness / compared-to), behavior readiness (past → planned), and qualitative stories.",
      "",
      "---",
      "",
      "## 2. How to work (secondary-AI playbook)",
      "",
      "1. Skim **§3 Hard rules**, **§5 ZIP guide**, **§5b matrix**, and **§5d recipes** before touching numbers.",
      "2. Prefer the companion **Results (md for AI)** for already-aggregated, filter-scoped tables.",
      "3. Open CSV ZIP only when you need custom cuts or respondent-level joins.",
      "4. Recompute indices only with the formulas in **§8** — never invent alternate Likert maps.",
      "5. Draft partner narrative with: clear N, filter scope, and plain-English claims that match the measurement",
      "   (e.g. Post \"compared to before\" is *perceived improvement*, not a Pre→Post delta of the same item).",
      "",
      "---",
      "",
      "## 3. Hard rules (do not violate)",
      "",
      "1. **Do not treat Pre wellness items and Post \"Compared to before…\" items as the same question.**",
      "   Pre = absolute agreement *now*. Post = perceived change *since before 5 Buckets*.",
      "   You may align them by concept key (`PRE_POST_COMPARABLE`) for paired analyses, but do **not**",
      "   subtract means as if they were identical scales of the same construct without stating that caveat.",
      "2. **Big vs Little is defined by Program Manager session position, not by which columns are non-empty.**",
      "   Missing PM join ⇒ Little (not Big). Wellness / past behaviors / compared-to / planned actions are **Big-only**.",
      "3. **Use the typed CSV pools for indices:** `big_pre.csv` / `big_post.csv`. Do not compute Financial Wellness",
      "   on `little_pre.csv` or Planned Actions on `little_post.csv`.",
      "4. **Likert coding is fixed:** Strongly Disagree=-3, Disagree=-1, Agree=+1, Strongly Agree=+3; Neutral→NA; mean of available items.",
      "5. **Multi-select cells are comma-separated labels**, not JSON. Past/planned behavior cells may contain grid tokens",
      "   (`Yes!!`, `Maybe...`, `Nope`) that must be stripped when alone; rematch fragments to **canonical** behavior labels.",
      "6. **Keep-in-touch** options contain en-dashes; rematch to canonical full labels (never count comma fragments as options).",
      paste0("7. **Open-text bilingual cells** may contain delimiter `", open_delim, "` → left = original language, right = English."),
      "   Prefer the English side for English-language reporting.",
      "8. **PII is stripped from the ZIP** (names, emails). Join people with `respondent_id` only. IDs starting with `ANON` are unpaired.",
      "9. **Call out small N** (e.g. N<10) and never overclaim causality from observational workshop surveys.",
      paste0("10. **Session satisfaction is 1–", sat_max, "**; **NPS recommend is 0–10** (promoters 9–10, passives 7–8, detractors ≤6)."),
      "",
      "---",
      "",
      "## 4. Mental model: rows, sessions, series, people",
      "",
      "```",
      "Organization",
      "  └─ Group (cohort within org)",
      "       └─ Series (planned length = PM sessions_in_series)",
      "            └─ Session (session_id; language variants share base_session_id)",
      "                 └─ Response row (one Pre or Post submission)",
      "                      └─ Person (respondent_id hash; may appear in Pre, Post, Annual)",
      "```",
      "",
      "| Concept | Key fields | Notes |",
      "| --- | --- | --- |",
      "| Organization | `org_name` | Partner site |",
      "| Group | `group` | Nested under org; empty/(no group) possible |",
      "| Session | `session_id` | Join key to Program Manager |",
      "| Base session | `base_session_id` (derived) | Collapses EN/ES/ZH variants of one workshop |",
      "| Series length | `sessions_in_series`, `session_number` | From PM after join; drives Big/Little |",
      "| Modules | `modules_taught` | Often pipe-joined atoms, e.g. `Grow\\|Protect` |",
      "| Language | `language` | Blank/NA on Pre ≈ English; Post may be `en`/`es`/`zh` |",
      "| Person | `respondent_id` | SHA-256(`SALT\\|email`); salt not in export |",
      "",
      "**Big Pre** = first session in series (or single-session series).",
      "**Big Post** = last session in series (or single-session series).",
      "**Little** = mid-series (or unmatched to PM).",
      "",
      "---",
      "",
      "## 5. ZIP file guide (what to open for what)",
      "",
      "| File | Grain | Use for |",
      "| --- | --- | --- |",
      "| `filter_snapshot.csv` | 1 export | Record of sidebar filters / export sections |",
      "| `session_summary.csv` | session | Ops view: org/group/date, pre/post counts per session |",
      "| `pre_all.csv` | Pre row | All filtered Pre (Big+Little); demographics/open text may appear |",
      "| `post_all.csv` | Post row | All filtered Post (Big+Little) |",
      "| `big_pre.csv` | Pre row | **Financial wellness + past behaviors + Big Pre demographics** |",
      "| `little_pre.csv` | Pre row | Mid-series Pre (usually no wellness block) |",
      "| `big_post.csv` | Post row | **Compared-to, planned actions, NPS, impact story** (strict Big Post) |",
      "| `little_post.csv` | Post row | Mid-series Post (sat / learning text may still exist) |",
      "| `annual.csv` | Annual row | Follow-up survey; join on `respondent_id` |",
      "| `report_for_ai.md` | document | Copy of Results MD for this export |",
      "| `column_inventory.csv` | column | Exact header list per CSV in this ZIP |",
      "",
      "Typed pools (`big_*` / `little_*`) usually include `is_big_pre` / `is_big_post` boolean columns from PM typing.",
      "If absent, re-apply the Big/Little rules using PM fields — do not infer from question non-missingness alone.",
      "",
      "Both Pre and Post typed files carry **both** `is_big_pre` and `is_big_post` flags (typing is session-based).",
      "Filter with the flag that matches the wave you are analyzing.",
      "",
      "---",
      "",
      "## 5b. Per-file column / analysis matrix (read before opening CSVs)",
      "",
      column_matrix_md,
      "",
      "---",
      "",
      "## 5c. Example cell shapes",
      "",
      example_shapes_md,
      "",
      "---",
      "",
      "## 5d. Join & analysis recipes (do these, not freestyle)",
      "",
      join_recipes_md,
      "",
      "---",
      "",
      "## 6. Source sheets (upstream Google Sheets)",
      "",
      "| Logical table | Typical tab | Role |",
      "| --- | --- | --- |",
      "| Master Pre | `Pre Submissions` | Workshop Pre responses |",
      "| Master Post | `Post Submissions` | Workshop Post responses |",
      "| Annual Survey | `Annual Survey` | Follow-up |",
      "| Program Manager (company) | Workshops | Session schedule + typing fields |",
      "| Program Manager (Mercy) | Mercy Workshops | Mercy session_ids not on company PM |",
      "",
      "Company + Mercy PM are stacked/deduped by `session_id` for typing. Mercy dual-write may add `unit_number` off company Master.",
      "",
      "---",
      "",
      "## 7. Survey typing rules (Big vs Little)",
      "",
      "Join Master → PM on `session_id`, then:",
      "",
      "| Flag | Rule |",
      "| --- | --- |",
      "| **Big Pre** | `sessions_in_series == 1` OR (`session_number == 1` with both PM fields present) |",
      "| **Big Post** | `sessions_in_series == 1` OR (`session_number == sessions_in_series`) |",
      "| **Little** | Everything else, including **no PM match** |",
      "",
      "Empty PM sheet ⇒ legacy fallback marks all rows Big (rare in production).",
      "",
      "---",
      "",
      "## 8. Scoring & indices (authoritative)",
      "",
      "### Likert coding",
      "",
      likert_note,
      "",
      "### Financial Wellness Index (Big Pre only)",
      "",
      "Row mean of the 8 Pre items below (skip missing; all-missing → NA). Higher = more positive money mindset *at Pre*.",
      "",
      pre_fw,
      "",
      "### Post Impact Index (Big Post only)",
      "",
      "Row mean of the 9 \"Compared to before…\" items. Higher = greater *perceived improvement*.",
      "",
      post_cmp,
      "",
      "### Short label ↔ full column aliases",
      "",
      alias_md,
      "",
      "### Behavioral indices",
      "",
      paste0("- **Past behaviors (Big Pre):** column `", past_col, "`."),
      "  Split on commas → drop lone grid tokens → map to canonical labels → **count = distinct canonical labels** (0–8).",
      paste0("- **Planned actions (Big Post):** column `", planned_col, "`."),
      "  Same pipeline / same canonical label set.",
      "",
      "Canonical behavior labels:",
      "",
      behaviors,
      "",
      "### Satisfaction & NPS",
      "",
      paste0("- **Session satisfaction:** `How satisfied were you with today's 5 Buckets session?` — numeric **1–", sat_max, "** for that session."),
      paste0("- **Recommend / NPS:** `", nps_col, "` — typically **0–10**."),
      "  NPS = %promoters(9–10) − %detractors(≤6). Passives = 7–8.",
      "",
      "---",
      "",
      "## 9. Pre ↔ Post concept alignment (paired change)",
      "",
      "Use these keys when matching Pre absolute items to Post comparative items for the **same person** (`respondent_id`).",
      "Post has an extra \"understanding\" item with no Pre twin.",
      "",
      comparable_md,
      "",
      "**Paired analysis tip:** restrict to IDs in Big Pre ∩ Big Post; report N pairs; prefer within-person change / slopes over naive cross-sectional mean gaps.",
      "",
      "---",
      "",
      "## 10. Multi-select & open-text parsing",
      "",
      "### Behaviors / planned actions / keep-in-touch",
      "",
      "- Cells are human-readable CSV of option labels (commas inside labels are ambiguous — rematch to known canonical strings).",
      "- Strip solitary tokens: `Yes!!`, `Maybe...`, `Nope`, `Yes`, `No`, `Maybe` (legacy grid artifacts).",
      "- Keep-in-touch canonical options:",
      "",
      keep_touch,
      "",
      "### Open text",
      "",
      paste0("- Delimiter: `", open_delim, "`"),
      "- Pattern: `original language side` + delimiter + optional `[lang language detected]` + English side.",
      "- For English reports / word freq: use English side when delimiter present; else the whole cell.",
      "",
      "Pre opening prompts:",
      "",
      pre_open,
      "",
      "Post session reflections + satisfaction column set:",
      "",
      post_today,
      "",
      "Also: Post impact story column `How has participating in this program helped or impacted you? Your story inspires others!`",
      "",
      "---",
      "",
      "## 11. Demographics vocabulary (Reach)",
      "",
      "Demographics live primarily on **Big Pre / Big Post** (and Annual). Little surveys typically lack the full block (zip may appear).",
      "",
      "Master Pre/Post demographic columns:",
      "",
      pre_demo,
      "",
      "| Variable | Ordered levels / notes |",
      "| --- | --- |",
      "| Age Group | Under 16; 16-18; (legacy Under 18); 18-24; 25-34; 35-44; 45-54; 55-64; 65+ |",
      "| Education | See exact ordered list below |",
      "| Household Income | See **exact full strings** below — never invent bins |",
      "| Race/Ethnicity | Multi-select; may be comma-joined; normalize labels before counting |",
      "| Gender Identity | Female; Male; Non-binary / Gender non-conforming; Prefer to self-describe; Other (+ write-ins often bucketed) |",
      "",
      "### Education levels (low → high, exact)",
      "",
      education_order_md,
      "",
      "### Household Income bands (exact full strings)",
      "",
      income_bands_md,
      "",
      "---",
      "",
      "## 12. Annual Survey",
      "",
      "Annual headers drift; resolve columns with these patterns (first match wins):",
      "",
      "**Critical:** Annual Compared-to headers ≠ Post Master Compared-to headers (spacing/wording).",
      "Use patterns / `annual_find_col()`; do not join grids by identical column name.",
      "",
      annual_patterns_md,
      "",
      "Typical Annual analyses:",
      "- Compared-to-before Likert grid (same agreement coding) → agree% or annual compared index",
      "- Behavior multi-select → annual behavior index",
      "- Recommend 0–10 → NPS-style",
      "- Money-change items (income/savings/debt/investments/credit) use ordered categorical labels (not dollars)",
      "- Join to workshop people via `respondent_id` (hashed from email at load time)",
      "",
      "---",
      "",
      "## 13. Metadata columns (Pre/Post)",
      "",
      pre_meta,
      "",
      "Note: ZIP export drops name/email/`req` PII columns even if listed above.",
      "",
      "---",
      "",
      "## 14. Dashboard → Results section index",
      "",
      "| Dashboard tab | Results section id | Primary evidence |",
      "| --- | --- | --- |",
      "| Overview | `overview` | Typed response counts; pairing; org series lengths |",
      "| Reach | `reach` | Demo distributions Pre/Post/(Annual) |",
      "| Financial Wellness | `wellness` | Pre index + 8 items; Post impact index + 9 items |",
      "| Behavioral Readiness | `behavioral` | Past/planned counts + indices |",
      "| Satisfaction | `satisfaction` | Session sat; NPS; means by `modules_taught` |",
      "| Learning & Impact Stories | `learning` | Topic understanding; keep-in-touch; word freq |",
      "| Annual Survey | `annual` | Compared agree%; recommend; behavior index |",
      "",
      "Figure recipes in Results MD use fields: `chart_type`, `x`/`y`, `universe`, `aggregation`, `score_map`.",
      "",
      "---",
      "",
      "## 15. Sidebar filter semantics (for interpreting a filtered export)",
      "",
      "| Control | Empty / All means |",
      "| --- | --- |",
      "| Organizations (multi) | No org restriction |",
      "| Groups (multi) | Nested under selected orgs; empty = all groups in those orgs |",
      "| Date range | Off unless checkbox enabled |",
      "| Language | All; blank language ≈ English |",
      "| Demographics | Empty checkbox group = no restriction |",
      "| Modules taught | Keep session if it teaches **any** selected module; ignored when org/group scope is all |",
      "| Export sections | Which Results/PDF tabs were emitted |",
      "",
      "Always restate the filter snapshot from Results MD / `filter_snapshot.csv` in any partner-facing report.",
      "",
      "---",
      "",
      "## 16. Example: good vs bad secondary-AI moves",
      "",
      "| Bad (guessing) | Good (Data Map–aligned) |",
      "| --- | --- |",
      "| \"Post mean 1.2 vs Pre mean 0.4 ⇒ +0.8 point program effect on the same scale\" | Report Pre wellness mean and Post impact mean separately; for paired IDs, use concept keys and state Post is perceived change |",
      "| Compute wellness on all `pre_all.csv` rows | Restrict to `big_pre.csv` (or `is_big_pre==TRUE`) |",
      "| Split keep-in-touch on commas and treat fragments as options | Rematch to canonical keep-in-touch labels |",
      "| Treat blank `language` as missing and drop | Treat blank/NA Pre language as English |",
      "| Join Annual on email | Join on `respondent_id` (email stripped) |",
      "",
      "---",
      "",
      "## 17. Code anchors (for humans maintaining the dash)",
      "",
      "| Concern | File |",
      "| --- | --- |",
      "| Column names / PRE_POST_COMPARABLE | `data/question_mapping.R` |",
      "| Big/Little typing | `data/process_data.R` |",
      "| Wellness / behavior indices | `data/big_pre_analysis.R`, `data/big_post_analysis.R` |",
      "| Behavior canonicalization | `data/behavior_item_canonical.R` |",
      "| Language + date filter | `data/language_filter.R` |",
      "| Open-text | `data/open_text_parse.R`, `data/text_analysis.R` |",
      "| respondent_id | `data/respondent_id.R` |",
      "| Pairing | `server/respondent_pairing.R` |",
      "| Annual | `data/annual_survey.R` |",
      "| Export / this Data Map | `modules/export.R` |",
      ""
    ),
    collapse = "\n"
  )
}


# -----------------------------------------------------------------------------
# Optional figure PNGs
# -----------------------------------------------------------------------------

.export_render_figure_png <- function(spec, path) {
  if (is.null(spec) || is.null(spec$kind)) return(FALSE)
  ok <- FALSE
  tryCatch({
    grDevices::png(path, width = 900, height = 560, res = 120)
    if (identical(spec$kind, "hist")) {
      v <- suppressWarnings(as.numeric(spec$values))
      v <- v[is.finite(v)]
      if (!length(v)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data")
      } else {
        graphics::hist(v, main = spec$title %||% "Distribution", xlab = "Value",
                       col = "#cbb6e4", border = "white")
      }
    } else if (identical(spec$kind, "bar")) {
      df <- spec$table
      if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data")
      } else {
        cat_col <- if (is.numeric(spec$category_col)) names(df)[spec$category_col] else spec$category_col
        val_col <- if (is.numeric(spec$value_col)) names(df)[spec$value_col] else spec$value_col
        if (is.null(cat_col) || is.null(val_col) || !cat_col %in% names(df) || !val_col %in% names(df)) {
          cat_col <- names(df)[1]
          val_col <- names(df)[min(2L, ncol(df))]
        }
        labs <- as.character(df[[cat_col]])
        vals <- suppressWarnings(as.numeric(df[[val_col]]))
        labs <- ifelse(nchar(labs) > 40, paste0(substr(labs, 1, 37), "..."), labs)
        op <- graphics::par(mar = c(5, 10, 2, 2))
        on.exit(graphics::par(op), add = TRUE)
        graphics::barplot(
          rev(vals),
          names.arg = rev(labs),
          horiz = TRUE,
          las = 1,
          col = "#5c2f92",
          border = NA,
          main = spec$title %||% ""
        )
      }
    } else if (identical(spec$kind, "table")) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste0("See CSV/MD tables\n(", spec$file %||% "table", ")"), cex = 1)
    } else {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Unsupported figure kind")
    }
    grDevices::dev.off()
    ok <- file.exists(path) && isTRUE(file.info(path)$size > 0)
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    ok <<- FALSE
  })
  isTRUE(ok)
}

.export_write_figures <- function(bundle, fig_dir) {
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  sections <- .export_collect_sections(bundle)
  for (sec in sections) {
    for (block in sec$blocks) {
      spec <- block$figure_spec
      if (is.null(spec) || is.null(spec$file)) next
      dest <- file.path(fig_dir, spec$file)
      if (.export_render_figure_png(spec, dest)) written <- c(written, spec$file)
    }
  }
  written
}

# -----------------------------------------------------------------------------
# Human PDF
# -----------------------------------------------------------------------------

.build_human_report_html <- function(bundle) {
  snap <- bundle$snap
  date_lab <- if (isTRUE(snap$use_date_filter)) {
    paste(snap$date_start, "-", snap$date_end)
  } else {
    "All dates"
  }
  idx <- rbind(
    .export_index_summary_row("Financial wellness (Big Pre)", bundle$indices$wellness),
    .export_index_summary_row("Post impact index (Big Post)", bundle$indices$post_impact),
    .export_index_summary_row("Session satisfaction", bundle$indices$quality)
  )
  html_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }
  row_html <- function(df) {
    if (!nrow(df)) return("<tr><td colspan='6'><em>No data</em></td></tr>")
    paste(vapply(seq_len(nrow(df)), function(i) {
      paste0("<tr>", paste0("<td>", html_escape(df[i, ]), "</td>", collapse = ""), "</tr>")
    }, character(1)), collapse = "\n")
  }
  paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'/>",
    "<title>5 Buckets Impact Report</title>",
    "<style>",
    "body{font-family:Georgia,'Times New Roman',serif;color:#2b2438;margin:40px;line-height:1.45;}",
    "h1{color:#5c2f92;font-size:28px;margin-bottom:4px;}",
    "h2{color:#5c2f92;font-size:18px;margin-top:28px;border-bottom:1px solid #e4dceb;padding-bottom:4px;}",
    ".meta{color:#6a6f75;font-size:13px;margin-bottom:24px;}",
    "table{border-collapse:collapse;width:100%;font-size:13px;margin:12px 0 20px;}",
    "th,td{border:1px solid #ddd;padding:8px 10px;text-align:left;}",
    "th{background:#f4eef9;color:#5c2f92;}",
    ".kpi{display:flex;flex-wrap:wrap;gap:12px;margin:16px 0;}",
    ".kpi div{background:#f8f5fb;border:1px solid #e4dceb;border-radius:8px;padding:12px 16px;min-width:120px;}",
    ".kpi strong{display:block;font-size:22px;color:#5c2f92;}",
    ".kpi span{font-size:12px;color:#6a6f75;}",
    "</style></head><body>",
    "<h1>5 Buckets Impact Report</h1>",
    "<div class='meta'>Generated ", html_escape(snap$exported_at),
    " · Orgs: ", html_escape(.export_fmt_list(snap$org)),
    " · Sections: ", html_escape(.export_fmt_list(snap$export_sections)),
    " · Dates: ", html_escape(date_lab), "</div>",
    "<div class='kpi'>",
    "<div><strong>", .export_safe_n(bundle$big_pre), "</strong><span>Big Pre</span></div>",
    "<div><strong>", .export_safe_n(bundle$big_post), "</strong><span>Big Post</span></div>",
    "<div><strong>", .export_safe_n(bundle$session_summary), "</strong><span>Sessions</span></div>",
    "<div><strong>", html_escape(bundle$nps$nps), "</strong><span>NPS</span></div>",
    "</div>",
    "<h2>Index summaries</h2>",
    "<table><thead><tr>", paste0("<th>", html_escape(names(idx)), "</th>", collapse = ""),
    "</tr></thead><tbody>", row_html(idx), "</tbody></table>",
    "<p class='meta'>Full hierarchical detail is in Report (.md for AI). Scoring rules in Data Map.</p>",
    "</body></html>"
  )
}

.write_text_pdf <- function(path, title, lines) {
  grDevices::pdf(path, width = 8.5, height = 11, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- graphics::par(mar = c(0.8, 0.8, 0.8, 0.8), family = "sans")
  on.exit(graphics::par(op), add = TRUE)
  max_lines <- 52L
  chunks <- split(lines, ceiling(seq_along(lines) / max_lines))
  if (!length(chunks)) chunks <- list(character(0))
  for (i in seq_along(chunks)) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    y <- 0.98
    graphics::text(0, y, title, adj = c(0, 1), cex = 1.1, font = 2, col = "#5c2f92")
    y <- y - 0.04
    graphics::text(0, y, paste("Page", i, "of", length(chunks)), adj = c(0, 1), cex = 0.7, col = "#6a6f75")
    y <- y - 0.05
    for (ln in chunks[[i]]) {
      graphics::text(0, y, ln, adj = c(0, 1), cex = 0.72, family = "mono")
      y <- y - 0.017
      if (y < 0.03) break
    }
  }
  invisible(path)
}

.write_human_pdf <- function(path, bundle) {
  html <- .build_human_report_html(bundle)
  html_tmp <- tempfile(fileext = ".html")
  writeLines(html, html_tmp, useBytes = TRUE)
  if (requireNamespace("pagedown", quietly = TRUE)) {
    ok <- tryCatch({
      pagedown::chrome_print(html_tmp, output = path, verbose = FALSE)
      file.exists(path) && file.info(path)$size > 0
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(invisible(path))
  }
  snap <- bundle$snap
  lines <- c(
    paste("Generated:", snap$exported_at %||% ""),
    paste("Organizations:", .export_fmt_list(snap$org)),
    paste("Sections:", .export_fmt_list(snap$export_sections)),
    paste("Big Pre N:", .export_safe_n(bundle$big_pre)),
    paste("Big Post N:", .export_safe_n(bundle$big_post)),
    paste("NPS:", bundle$nps$nps),
    paste("Wellness mean:", .export_mean_na(bundle$indices$wellness)),
    paste("Post impact mean:", .export_mean_na(bundle$indices$post_impact))
  )
  .write_text_pdf(path, "5 Buckets Impact Report", lines)
}

# -----------------------------------------------------------------------------
# Filtered data ZIP (+ optional figures)
# -----------------------------------------------------------------------------

.write_filtered_data_zip <- function(path, bundle) {
  td <- tempfile("export_data_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Absolute zip path BEFORE setwd(td) — relative paths would resolve inside td.
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  zip_abs <- file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))

  tables <- list(
    filter_snapshot = {
      snap <- bundle$snap
      data.frame(
        key = names(snap),
        value = vapply(snap, function(v) {
          if (is.null(v)) return("")
          paste(as.character(v), collapse = "; ")
        }, character(1)),
        stringsAsFactors = FALSE
      )
    },
    session_summary = bundle$session_summary,
    pre_all = .export_drop_pii_cols(bundle$pre_all),
    post_all = .export_drop_pii_cols(bundle$post_all),
    big_pre = .export_drop_pii_cols(bundle$big_pre),
    little_pre = .export_drop_pii_cols(bundle$little_pre),
    little_post = .export_drop_pii_cols(bundle$little_post),
    big_post = .export_drop_pii_cols(bundle$big_post),
    annual = .export_drop_pii_cols(bundle$annual)
  )

  zip_entries <- character(0)
  for (nm in names(tables)) {
    df <- tables[[nm]]
    if (is.null(df)) df <- data.frame()
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    fn <- file.path(td, paste0(nm, ".csv"))
    .export_write_csv(df, fn)
    zip_entries <- c(zip_entries, paste0(nm, ".csv"))
  }

  # Column inventory so secondary AI need not guess which headers exist
  inv_rows <- list()
  for (nm in names(tables)) {
    df <- tables[[nm]]
    if (is.null(df) || !is.data.frame(df)) next
    if (!ncol(df)) {
      inv_rows[[length(inv_rows) + 1L]] <- data.frame(
        file = paste0(nm, ".csv"), column = NA_character_, col_index = NA_integer_,
        stringsAsFactors = FALSE
      )
    } else {
      inv_rows[[length(inv_rows) + 1L]] <- data.frame(
        file = paste0(nm, ".csv"),
        column = names(df),
        col_index = seq_along(names(df)),
        stringsAsFactors = FALSE
      )
    }
  }
  inv <- if (length(inv_rows)) do.call(rbind, inv_rows) else data.frame()
  inv_fn <- file.path(td, "column_inventory.csv")
  .export_write_csv(inv, inv_fn)
  zip_entries <- c(zip_entries, "column_inventory.csv")

  # Also drop a copy of the Results MD for convenience
  report_md <- tryCatch(build_report_markdown(bundle), error = function(e) "")
  if (nzchar(report_md)) {
    writeLines(report_md, file.path(td, "report_for_ai.md"), useBytes = TRUE)
    zip_entries <- c(zip_entries, "report_for_ai.md")
  }

  readme <- paste0(
    "5 Buckets filtered data export\n",
    "Generated: ", bundle$snap$exported_at %||% "", "\n\n",
    "PII columns (names, emails) were stripped. respondent_id (hash) is retained.\n",
    "Use with Data Map (md for AI) for column meaning and scoring rules.\n",
    "This ZIP is CSVs only (no figure PNGs). Results MD carries figure recipes.\n"
  )
  writeLines(readme, file.path(td, "README.txt"))
  zip_entries <- c(zip_entries, "README.txt")

  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile = zip_abs, files = zip_entries, flags = "-q")
  invisible(zip_abs)
}

# -----------------------------------------------------------------------------
# Server registration
# -----------------------------------------------------------------------------

register_export_server <- function(input, output, session,
                                   filtered_pre, filtered_post, filtered_annual = NULL,
                                   filtered_big_pre, filtered_little_pre,
                                   filtered_little_post, filtered_big_post_only,
                                   session_summary_data = NULL) {
  .bundle <- function() {
    .export_collect_bundle(
      input,
      filtered_pre, filtered_post, filtered_annual,
      filtered_big_pre, filtered_little_pre,
      filtered_little_post, filtered_big_post_only,
      session_summary_data
    )
  }

  .write_md_safe <- function(file, text) {
    # Avoid useBytes=TRUE (can break UTF-8 downloads); always produce a file.
    text <- as.character(text %||% "")
    if (!length(text) || !nzchar(paste(text, collapse = ""))) {
      text <- "# Export failed\n\nNo content was generated.\n"
    }
    con <- file(file, open = "wt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(text, con, useBytes = FALSE)
    invisible(file)
  }

  output$export_human_pdf <- downloadHandler(
    filename = function() {
      b <- tryCatch(.bundle(), error = function(e) list(slug = format(Sys.Date())))
      paste0("5buckets_report_", b$slug %||% Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        .write_human_pdf(file, .bundle())
      }, error = function(e) {
        .write_text_pdf(file, "5 Buckets Impact Report",
                        c("PDF export failed:", conditionMessage(e)))
      })
    }
  )

  output$export_analysis_md <- downloadHandler(
    filename = function() {
      b <- tryCatch(.bundle(), error = function(e) list(slug = format(Sys.Date())))
      paste0("5buckets_results_", b$slug %||% Sys.Date(), ".md")
    },
    content = function(file) {
      md <- tryCatch({
        build_report_markdown(.bundle())
      }, error = function(e) {
        paste0(
          "# 5 Buckets Results (for AI) — export error\n\n",
          "Could not build the full Results markdown.\n\n",
          "```\n", conditionMessage(e), "\n```\n"
        )
      })
      .write_md_safe(file, md)
    },
    contentType = "text/markdown; charset=UTF-8"
  )

  output$export_data_map_md <- downloadHandler(
    filename = function() {
      paste0("5buckets_data_map_", format(Sys.Date(), "%Y-%m-%d"), ".md")
    },
    content = function(file) {
      md <- tryCatch({
        build_data_mapping_markdown()
      }, error = function(e) {
        paste0("# Data Map export error\n\n```\n", conditionMessage(e), "\n```\n")
      })
      .write_md_safe(file, md)
    },
    contentType = "text/markdown; charset=UTF-8"
  )

  output$export_filtered_data_zip <- downloadHandler(
    filename = function() {
      b <- tryCatch(.bundle(), error = function(e) list(slug = format(Sys.Date())))
      paste0("5buckets_data_", b$slug %||% Sys.Date(), ".zip")
    },
    content = function(file) {
      tryCatch({
        .write_filtered_data_zip(file, .bundle())
      }, error = function(e) {
        td <- tempfile("export_fail_")
        dir.create(td)
        writeLines(paste("ZIP export failed:", conditionMessage(e)), file.path(td, "ERROR.txt"))
        old <- setwd(td)
        on.exit(setwd(old), add = TRUE)
        utils::zip(zipfile = file, files = "ERROR.txt", flags = "-q")
      })
    },
    contentType = "application/zip"
  )
}
