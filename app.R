# app.R - 5 Buckets Impact Dashboard (v3)
# Master Workbook Feb 2026, delimiter-aware open text, favorites, Impact + Survey tabs

source("load_sources.R")
source("server/server_register.R")

# ============================================================================
# Optional deployed-only login gate (shinymanager)
# ----------------------------------------------------------------------------
# Auth turns ON only when DASHBOARD_AUTH_PASSWORD is set in the environment
# (e.g. as a secret variable on Posit Connect Cloud). Locally the variable is
# unset, so the dashboard runs password-free for development.
#
# Env vars (set on the host, never committed):
#   DASHBOARD_AUTH_PASSWORD  required to enable auth; comma-separate for >1 user
#   DASHBOARD_AUTH_USERS     optional usernames (default "5buckets"); comma-separated
# ============================================================================
.auth_enabled <- function() {
  nzchar(Sys.getenv("DASHBOARD_AUTH_PASSWORD", "")) &&
    requireNamespace("shinymanager", quietly = TRUE)
}

.auth_credentials <- function() {
  users <- trimws(strsplit(Sys.getenv("DASHBOARD_AUTH_USERS", "5buckets"), ",")[[1]])
  pwds  <- trimws(strsplit(Sys.getenv("DASHBOARD_AUTH_PASSWORD", ""), ",")[[1]])
  n <- max(length(users), length(pwds))
  data.frame(
    user     = rep(users, length.out = n),
    password = rep(pwds,  length.out = n),
    stringsAsFactors = FALSE
  )
}

# Plot font and rotating color palette (5 Buckets brand: primary #5c2f92, supporting colors per brand guidelines)
PLOT_FONT <- list(family = "Arial, Helvetica, sans-serif", size = 12)
REACH_PALETTE <- c("#5c2f92", "#82c341", "#f58220", "#0076be", "#9c27b0", "#00acc1", "#7cb342", "#e65100")
# Pre vs Post index plots (Financial Wellness, Behavioral): cool blue = Pre, warm gold = Post (colorblind-friendly contrast)
# Plain names: Pre = "strong blue" / azure (#0076be); Post = "golden amber" / dark yellow-gold (#c9a227)
PRE_INDEX_COLOR <- "#0076be"
PRE_INDEX_LINE_COLOR <- "#004a78"  # darker blue so the smoothed curve reads over translucent bars
PRE_INDEX_FILL_RGBA <- "rgba(0, 118, 190, 0.15)"
POST_INDEX_COLOR <- "#c9a227"
POST_INDEX_LINE_COLOR <- "#8a6d12"  # darker gold so the smoothed curve reads over translucent bars
POST_INDEX_FILL_RGBA <- "rgba(201, 162, 39, 0.15)"
# Annual index: brand leafy green (distinct from Pre blue / Post gold, still on brand palette)
ANNUAL_INDEX_COLOR <- "#82c341"
ANNUAL_INDEX_LINE_COLOR <- "#4e7a22"
ANNUAL_INDEX_FILL_RGBA <- "rgba(130, 195, 65, 0.18)"
# Within-subjects mean connector: dark goldenrod (distinct from Post line gold)
MEAN_SLOPE_COLOR <- "#a67c00"

# Shared scale blurb for Pre / Post / Annual financial wellness summary scores
LIKERT_INDEX_SCALE_BLURB <- paste0(
  "Each item uses a 4-point agreement scale with no neutral option, converted to numbers: ",
  "Strongly Disagree = -3, Disagree = -1, Agree = +1, Strongly Agree = +3. ",
  "The index is the mean of those item scores, so it also runs from -3 to +3. ",
  "0 is neutral; higher means more positive financial wellness (Pre) or greater perceived improvement (Post / Annual)."
)

# --- Impact Story / Learning: Feb 2026 column resolution + multicolor wordclouds ---
.resolve_col <- function(df, exact, grep_pattern = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!is.null(exact) && nzchar(exact) && exact %in% colnames(df)) return(exact)
  if (!is.null(grep_pattern) && nzchar(grep_pattern)) {
    g <- grep(grep_pattern, colnames(df), ignore.case = TRUE, value = TRUE)
    if (length(g)) return(g[1])
  }
  NULL
}

.wc_reach_palette <- function(word_freq, size = 0.45) {
  if (!requireNamespace("wordcloud2", quietly = TRUE)) {
    return(shiny::HTML("<p>Install package <code>wordcloud2</code>.</p>"))
  }
  if (is.null(word_freq) || nrow(word_freq) == 0) {
    return(shiny::HTML("<p>No valid responses after filtering.</p>"))
  }
  word_freq <- head(word_freq, 50)
  cols <- REACH_PALETTE[(seq_len(nrow(word_freq)) - 1L) %% length(REACH_PALETTE) + 1L]
  htmltools::div(
    style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
    wordcloud2::wordcloud2(word_freq, color = cols, backgroundColor = "white", size = size)
  )
}

.impact_open_text_block <- function(df, title, subtitle, col_exact, grep_fallback, wc_size = 0.45) {
  col <- .resolve_col(df, col_exact, grep_fallback)
  if (is.null(col)) {
    return(shiny::tagList(
      shiny::h5(title),
      shiny::p(class = "text-muted", "Question column not found in current data.")
    ))
  }
  vec <- df[[col]]
  bi <- open_text_bilingual_summary(vec)
  wf_all <- create_word_freq(vec, lang_mode = "all")
  wf_en <- create_word_freq(vec, lang_mode = "english")
  samples <- open_text_bilingual_samples(vec, 5L)
  shiny::tagList(
    shiny::h5(title),
    if (!is.null(subtitle) && nzchar(subtitle)) shiny::p(style = "font-size:12px;color:#5f6369;", subtitle),
    if (bi$with_delim > 0) {
      shiny::p(style = "font-size:11px;color:#5f6369;",
        bi$with_delim, " bilingual response(s); delimiter ", tags$code(OPEN_TEXT_DELIM))
    },
    shiny::fluidRow(
      shiny::column(4,
        shiny::h6("Word cloud — all languages"),
        .wc_reach_palette(wf_all, wc_size)
      ),
      shiny::column(4,
        shiny::h6("Word cloud — English"),
        .wc_reach_palette(wf_en, wc_size)
      ),
      shiny::column(4,
        shiny::wellPanel(style = "background:#fff;",
          shiny::HTML(sentiment_panel_html(vec, include_lexicon_blurb = TRUE, lang_mode = "english"))
        )
      )
    ),
    if (nrow(samples) > 0) {
      shiny::tags$details(
        shiny::tags$summary("Sample bilingual responses"),
        DT::datatable(samples, options = list(dom = "t", pageLength = 5), rownames = FALSE)
      )
    }
  )
}

# --- Keep in touch: comma-safe parsing (fragments like "job", "club" dropped) + bar colors ---
.keep_touch_normalize <- function(s) {
  s <- trimws(as.character(s))
  gsub("\u2013|\u2014", "-", s)
}
.is_keep_touch_junk_fragment <- function(p) {
  t <- tolower(trimws(as.character(p)))
  if (!nzchar(t)) return(TRUE)
  if (t %in% c("job", "club", "or", "and", "church", "community group", "n/a", "na")) return(TRUE)
  if (grepl("^or\\s+community", t)) return(TRUE)
  if (nchar(t) <= 2L && !(t %in% c("ok", "ty"))) return(TRUE)
  FALSE
}
# Display: text before first en-dash/hyphen (strip boilerplate after " – ")
.keep_touch_label_before_dash <- function(text) {
  if (length(text) == 0) return(text)
  vapply(text, function(t) {
    t <- trimws(as.character(t))
    if (!nzchar(t)) return("")
    tn <- gsub("\u2013|\u2014", "-", t)
    parts <- strsplit(tn, "\\s*-\\s*", perl = TRUE)[[1L]]
    trimws(parts[1L])
  }, character(1))
}

.parse_keep_in_touch_cell <- function(cell) {
  s <- trimws(as.character(cell))
  if (is.na(s) || !nzchar(s)) return(character(0))
  parts <- trimws(strsplit(s, ",\\s*")[[1L]])
  matched <- character(0)
  labs <- KEEP_IN_TOUCH_CANONICAL_LABELS
  labs_ord <- labs[order(-nchar(labs), labs)]
  for (p in parts) {
    if (.is_keep_touch_junk_fragment(p)) next
    pn <- .keep_touch_normalize(p)
    hit <- NA_character_
    for (lab in labs_ord) {
      ln <- .keep_touch_normalize(lab)
      if (startsWith(pn, ln)) {
        hit <- lab
        break
      }
      if (nchar(ln) >= 24L) {
        pref <- substr(ln, 1L, 24L)
        if (startsWith(pn, pref)) {
          hit <- lab
          break
        }
      }
    }
    if (!is.na(hit)) matched <- c(matched, hit)
  }
  unique(matched)
}

# Learning-interest bars reuse the Reach tab's brighter REACH_PALETTE (the user found
# the older dedicated palette too dark vs the Reach charts).
.learning_interest_colors_for_options <- function(option_names) {
  n_pal <- length(REACH_PALETTE)
  out <- character(length(option_names))
  for (i in seq_along(option_names)) {
    ix <- match(option_names[i], KEEP_IN_TOUCH_CANONICAL_LABELS, nomatch = 0L)
    # Canonical options keep a stable color by their canonical index; any
    # non-canonical / unmatched label still gets a brand color (rotating by row
    # position) rather than a gray fallback.
    slot <- if (ix > 0L) ix else i
    out[i] <- REACH_PALETTE[((slot - 1L) %% n_pal) + 1L]
  }
  out
}

# UI block for one open-text question: bilingual note + dual base-R wordclouds
# (`wordcloud2` widgets inside renderUI only bind the first instance, so use base R via renderPlot)
.dual_wc_ui_block <- function(base_id, wc_height = "420px") {
  shiny::tagList(
    shiny::htmlOutput(paste0(base_id, "_bi_note")),
    shiny::fluidRow(
      shiny::column(6,
        shiny::h6("All languages", style = "font-size: 11px; color: #5f6369; text-align: center; margin: 0 0 4px 0;"),
        shiny::plotOutput(paste0(base_id, "_wc_all"), height = wc_height)
      ),
      shiny::column(6,
        shiny::h6("English", style = "font-size: 11px; color: #5f6369; text-align: center; margin: 0 0 4px 0;"),
        shiny::plotOutput(paste0(base_id, "_wc_en"), height = wc_height)
      )
    )
  )
}

# Attach the three outputs needed for a dual-wordcloud open-text question.
# Read the global "Words per cloud" slider from the active session (no need to thread
# `input` through every call site). Falls back to 60 before the slider has rendered.
.wc_max_words <- function() {
  dom <- shiny::getDefaultReactiveDomain()
  v <- if (!is.null(dom)) dom$input[["wc_max_words"]] else NULL
  if (is.null(v) || !is.finite(v)) 60L else as.integer(v)
}

.attach_dual_wc_outputs <- function(output, base_id, data_reactive, filter_key, col_exact, grep_fallback) {
  resolve_vec <- function() {
    df <- data_reactive()
    if (nrow(df) == 0) return(NULL)
    col <- .resolve_col(df, col_exact, grep_fallback)
    if (is.null(col)) return(NULL)
    df[[col]]
  }
  output[[paste0(base_id, "_wc_all")]] <- shiny::renderPlot({
    vec <- resolve_vec()
    if (is.null(vec)) { plot.new(); text(0.5, 0.5, "No data for current filters", cex = 1.05); return(invisible(NULL)) }
    .render_impact_wordcloud_plot(vec, lang_mode = "all", max_words = .wc_max_words())
  }, res = 110) %>% bindCache(filter_key(), paste0(base_id, "_wc_all"), .wc_max_words())

  output[[paste0(base_id, "_wc_en")]] <- shiny::renderPlot({
    vec <- resolve_vec()
    if (is.null(vec)) { plot.new(); text(0.5, 0.5, "No data for current filters", cex = 1.05); return(invisible(NULL)) }
    .render_impact_wordcloud_plot(vec, lang_mode = "english", max_words = .wc_max_words())
  }, res = 110) %>% bindCache(filter_key(), paste0(base_id, "_wc_en"), .wc_max_words())

  output[[paste0(base_id, "_bi_note")]] <- shiny::renderUI({
    vec <- resolve_vec()
    if (is.null(vec)) return(NULL)
    bi <- open_text_bilingual_summary(vec)
    if (bi$with_delim == 0) return(NULL)
    shiny::p(style = "font-size: 11px; color: #5f6369; margin: 4px 0;",
      bi$with_delim, " bilingual response(s); sentiment uses the English side.")
  }) %>% bindCache(filter_key(), paste0(base_id, "_bi_note"))
}

.render_impact_wordcloud_plot <- function(text_vec, lang_mode = "all", max_words = 60L) {
  if (!requireNamespace("wordcloud", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Install package: wordcloud", cex = 1.05)
    return(invisible(NULL))
  }
  max_words <- suppressWarnings(as.integer(max_words))
  if (is.na(max_words) || max_words < 5L) max_words <- 60L
  # "All languages" cloud uses language-balanced frequencies so non-English words
  # surface more; "English" cloud uses the straight English-side frequencies.
  wf <- if (identical(lang_mode, "all")) {
    balanced_word_freq_all(text_vec, top_k = max_words)
  } else {
    create_word_freq(text_vec, lang_mode = lang_mode)
  }
  if (is.null(wf) || nrow(wf) == 0L) {
    plot.new()
    text(0.5, 0.5, "No valid responses after filtering", cex = 1.05)
    return(invisible(NULL))
  }
  wf <- head(wf, max_words)
  # Randomly color each word from the Reach brand palette. `ordered.colors = TRUE`
  # makes the colors vector map 1:1 to words (instead of wordcloud's default
  # least-to-most-frequent gradient, which collapsed everything to ~2 hues).
  # Seed off the word set so colors are stable across re-renders (no flicker).
  seed <- sum(utf8ToInt(paste(wf$word, collapse = "")))
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  cols <- sample(REACH_PALETTE, nrow(wf), replace = TRUE)
  if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
  op <- par(mar = rep(0, 4), bg = "white")
  on.exit(par(op), add = TRUE)
  suppressWarnings(
    wordcloud::wordcloud(
      wf$word, wf$freq,
      scale = c(4.4, 0.5),
      min.freq = 1,
      max.words = max_words,
      colors = cols,
      ordered.colors = TRUE,
      random.order = FALSE,
      rot.per = 0.1
    )
  )
  invisible(NULL)
}

.impact_individual_responses_df <- function(vec) {
  vec <- ensure_text_vector(vec)
  ok <- !is.na(vec) & nzchar(trimws(as.character(vec)))
  vec <- vec[ok]
  if (length(vec) == 0) {
    return(data.frame(`#` = integer(0), Response = character(0), check.names = FALSE, stringsAsFactors = FALSE))
  }
  data.frame(`#` = seq_along(vec), Response = trimws(as.character(vec)), check.names = FALSE, stringsAsFactors = FALSE)
}

# Rounded-card, inline-filterable DT for the wordcloud "show original text" expanders.
# Rows are already language-filtered upstream (filtered_big_pre/post apply the sidebar
# language selection); `filter = "top"` adds a per-column search box on top of that.
.styled_responses_datatable <- function(vec) {
  df <- .impact_individual_responses_df(vec)
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    class = "vm-coverage compact stripe responses-table",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = "ftip",
      columnDefs = list(list(width = "54px", targets = 0, className = "dt-center"))
    )
  )
}

.ordered_demo_levels_present <- function(v, template) {
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  u <- unique(trimws(v))
  o1 <- template[template %in% u]
  o2 <- sort(setdiff(u, o1))
  c(o1, o2)
}

.learning_keep_touch_heatmap_plotly <- function(d, demo_col_exact, title, demo_grep_fallback = NULL) {
  if (!is.data.frame(d) || nrow(d) == 0) {
    return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
  }
  kt_col <- .resolve_col(d, POST_KEEP_IN_TOUCH_COL, "keep in touch")
  if (is.null(kt_col)) {
    return(plotly_empty() %>% layout(title = "Keep in touch column not found", font = PLOT_FONT))
  }
  demo_col <- .resolve_col(d, demo_col_exact, demo_grep_fallback)
  if (is.null(demo_col)) {
    return(plotly_empty() %>% layout(title = "Demographic column not found", font = PLOT_FONT))
  }
  rows_lab <- KEEP_IN_TOUCH_CANONICAL_LABELS
  n_row <- length(rows_lab)
  dvec <- d[[demo_col]]
  if (grepl("education", demo_col, ignore.case = TRUE)) {
    dvec <- .normalize_education_for_dashboard(dvec)
  }
  if (identical(demo_col, "Age Group")) dvec <- AGE_LEVELS_NORMALIZE(dvec)
  if (grepl("Income", demo_col, fixed = TRUE)) {
    col_levels <- .ordered_demo_levels_present(dvec, INCOME_ORDER)
  } else if (grepl("education", demo_col, ignore.case = TRUE)) {
    col_levels <- .ordered_demo_levels_present(dvec, EDUCATION_ORDER)
  } else if (identical(demo_col, "Age Group")) {
    col_levels <- .ordered_demo_levels_present(dvec, AGE_LEVELS_CANONICAL)
  } else {
    col_levels <- sort(unique(as.character(dvec[!is.na(dvec) & nzchar(trimws(as.character(dvec)))])))
  }
  n_col <- length(col_levels)
  if (n_col == 0) return(plotly_empty() %>% layout(title = "No demographic values", font = PLOT_FONT))
  mat_cnt <- matrix(0, nrow = n_row, ncol = n_col, dimnames = list(rows_lab, col_levels))
  denom <- setNames(rep(0, n_col), col_levels)
  for (i in seq_len(nrow(d))) {
    g <- dvec[i]
    if (is.na(g) || !nzchar(trimws(as.character(g)))) next
    g <- trimws(as.character(g))
    if (identical(demo_col, "Age Group")) g <- AGE_LEVELS_NORMALIZE(g)
    if (!g %in% col_levels) next
    sel <- .parse_keep_in_touch_cell(d[[kt_col]][i])
    if (length(sel) == 0) next
    denom[g] <- denom[g] + 1
    for (s in sel) {
      if (s %in% rownames(mat_cnt)) mat_cnt[s, g] <- mat_cnt[s, g] + 1
    }
  }
  den <- denom[col_levels]
  den_safe <- ifelse(den > 0, den, NA_real_)
  pct <- sweep(mat_cnt[, col_levels, drop = FALSE], 2, den_safe, "/") * 100
  pct[is.na(pct)] <- 0
  row_tot <- rowSums(mat_cnt)
  row_ord <- order(-row_tot, rows_lab)
  rows_lab <- rows_lab[row_ord]
  mat_cnt <- mat_cnt[row_ord, , drop = FALSE]
  pct <- pct[row_ord, , drop = FALSE]
  n_row <- length(rows_lab)
  row_short <- .keep_touch_label_before_dash(rows_lab)
  xlab_display <- col_levels
  if (grepl("Income", demo_col, fixed = TRUE)) {
    xlab_display <- ifelse(col_levels %in% names(INCOME_SHORT_LABELS), unname(INCOME_SHORT_LABELS[col_levels]), col_levels)
  }
  ht <- matrix("", nrow = n_row, ncol = n_col)
  for (ii in seq_len(n_row)) {
    for (jj in seq_len(n_col)) {
      ht[ii, jj] <- paste0(
        "<b>", row_short[ii], "</b><br>",
        "Subgroup: ", col_levels[jj], "<br>",
        sprintf("%.0f%% of this subgroup chose it", pct[ii, jj]), "<br>",
        mat_cnt[ii, jj], " of ", den[jj], " people in this subgroup"
      )
    }
  }
  plot_ly(
    x = xlab_display,
    y = row_short,
    z = pct,
    type = "heatmap",
    colorscale = list(c(0, "#f8f9fa"), c(0.5, "#e0d4f0"), c(1, "#5c2f92")),
    hoverinfo = "text",
    text = ht,
    hovertext = ht,
    colorbar = list(title = "% of subgroup")
  ) %>%
    layout(
      title = title,
      font = PLOT_FONT,
      xaxis = list(title = "", tickangle = -40),
      # Match bar chart: highest total count at top (Plotly categoryarray = bottom→top)
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(row_short)),
      margin = list(l = 160, b = 120, t = 56)
    )
}

# Big Post: session satisfaction question (numeric scale; typically 1–6 in Feb 2026 forms)
POST_SESSION_SATISFACTION_COL <- "How satisfied were you with today's 5 Buckets session?"
SESSION_SATISFACTION_MAX <- 6L

# Paired change plots: reference at x = 0 (no change), 95% CI band, mean line with black outline
.paired_change_ci_shapes <- function(x, ymax, fillcolor = "rgba(92, 47, 146, 0.14)", mean_color = "#5c2f92") {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) {
    return(list(shapes = list()))
  }
  shapes <- list()
  # No-change reference (x = 0): dashed, behind other overlays
  shapes[[length(shapes) + 1L]] <- list(
    type = "line", x0 = 0, x1 = 0, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
    line = list(color = "rgba(0,0,0,0.45)", width = 2, dash = "dash"), layer = "below"
  )
  if (n < 2 || !is.finite(m)) {
    return(list(shapes = shapes))
  }
  se <- stats::sd(x) / sqrt(n)
  if (!is.finite(se) || se <= 0) {
    shapes[[length(shapes) + 1L]] <- list(
      type = "line", x0 = m, x1 = m, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
      line = list(color = "rgba(0,0,0,0.9)", width = 4), layer = "below"
    )
    shapes[[length(shapes) + 1L]] <- list(
      type = "line", x0 = m, x1 = m, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
      line = list(color = mean_color, width = 2)
    )
    return(list(shapes = shapes))
  }
  tcrit <- stats::qt(0.975, df = n - 1)
  ci_lo <- m - tcrit * se
  ci_hi <- m + tcrit * se
  shapes[[length(shapes) + 1L]] <- list(
    type = "rect", x0 = ci_lo, x1 = ci_hi, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
    fillcolor = fillcolor, line = list(width = 0), layer = "below"
  )
  shapes[[length(shapes) + 1L]] <- list(
    type = "line", x0 = m, x1 = m, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
    line = list(color = "rgba(0,0,0,0.9)", width = 4), layer = "below"
  )
  shapes[[length(shapes) + 1L]] <- list(
    type = "line", x0 = m, x1 = m, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
    line = list(color = mean_color, width = 2)
  )
  list(shapes = shapes)
}

# Diverging fill for change-score histograms (brand palette): positive change -> blue
# (deeper = larger gain, brand #0076be), negative change -> orange (deeper = larger loss,
# brand #f58220), the zero bin stays near-white with a black outline so the lean of the
# distribution reads at a glance. Returns per-bar fill + outline specs.
.change_diverging_colors <- function(mids, zero_tol = NULL) {
  n <- length(mids)
  if (n == 0) return(list(fill = character(0), line_color = character(0), line_width = numeric(0)))
  if (is.null(zero_tol)) {
    bw <- if (n >= 2) min(diff(sort(unique(mids)))) else 0
    zero_tol <- if (is.finite(bw) && bw > 0) bw / 2 else 1e-9
  }
  zero <- abs(mids) <= zero_tol
  pos  <- mids >  zero_tol
  neg  <- mids < -zero_tol
  max_pos <- if (any(pos)) max(mids[pos]) else 1
  max_neg <- if (any(neg)) max(abs(mids[neg])) else 1
  pos_ramp <- grDevices::colorRamp(c("#cfe4f3", "#0076be"))  # light -> brand blue (positive)
  neg_ramp <- grDevices::colorRamp(c("#fde3cc", "#f58220"))  # light -> brand orange (negative)
  to_hex <- function(rgbm) grDevices::rgb(rgbm[, 1], rgbm[, 2], rgbm[, 3], maxColorValue = 255)
  fill <- character(n)
  if (any(pos)) fill[pos] <- to_hex(pos_ramp(pmin(1, mids[pos] / max_pos)))
  if (any(neg)) fill[neg] <- to_hex(neg_ramp(pmin(1, abs(mids[neg]) / max_neg)))
  fill[zero] <- "#f7f7f7"
  line_color <- ifelse(zero, "#000000", "rgba(0,0,0,0.30)")
  line_width <- ifelse(zero, 1.4, 0.5)
  list(fill = fill, line_color = line_color, line_width = line_width)
}

# Single-index mean: vertical line at mean, black outline + brand color (full plot height)
.mean_vline_outline_shapes <- function(x_mean, ymax, color) {
  if (!is.finite(x_mean) || !is.finite(ymax) || ymax <= 0) return(list())
  list(
    list(type = "line", x0 = x_mean, x1 = x_mean, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
         line = list(color = "rgba(0,0,0,0.88)", width = 4), layer = "below"),
    list(type = "line", x0 = x_mean, x1 = x_mean, xref = "x", yref = "y", y0 = 0, y1 = ymax * 1.02,
         line = list(color = color, width = 2))
  )
}

# Expand comma-separated metadata (refined: each comma-separated segment is one distinct label as stored)
# Coerce googlesheets4 list-columns to atomic character / numeric (Satisfaction crash fix).
.coerce_atomic_chr <- function(v) {
  if (is.null(v)) return(character(0))
  if (is.list(v)) {
    return(vapply(seq_along(v), function(i) {
      x <- v[[i]]
      if (is.null(x) || length(x) == 0) return(NA_character_)
      if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
      trimws(paste(as.character(x), collapse = ", "))
    }, character(1)))
  }
  trimws(as.character(v))
}
.coerce_numeric_vec <- function(v) {
  suppressWarnings(as.numeric(.coerce_atomic_chr(v)))
}

.satisfaction_expand_rows <- function(df, sat_col, split_col) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(key = character(0), satisfaction = numeric(0)))
  if (!sat_col %in% colnames(df) || !split_col %in% colnames(df)) {
    return(data.frame(key = character(0), satisfaction = numeric(0)))
  }
  sat <- .coerce_numeric_vec(df[[sat_col]])
  sp <- .coerce_atomic_chr(df[[split_col]])
  out <- list()
  for (i in seq_len(nrow(df))) {
    if (!is.finite(sat[i])) next
    raw <- sp[i]
    if (is.na(raw) || !nzchar(trimws(as.character(raw)))) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    parts <- trimws(strsplit(as.character(raw), ",")[[1L]])
    parts <- parts[nzchar(parts)]
    if (!length(parts)) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    for (p in parts) {
      out[[length(out) + 1L]] <- data.frame(key = trimws(p), satisfaction = sat[i], stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame(key = character(0), satisfaction = numeric(0)))
  do.call(rbind, out)
}

# Atomic module labels from one modules_taught cell (comma segments, then | within segment) — matches PM / Big Post metadata
.pm_modules_atoms_from_cell <- function(raw) {
  raw <- trimws(as.character(raw))
  if (length(raw) != 1L || is.na(raw) || !nzchar(raw)) return(character(0))
  segments <- trimws(strsplit(raw, ",")[[1L]])
  segments <- segments[nzchar(segments)]
  if (!length(segments)) return(character(0))
  atoms <- character(0)
  for (seg in segments) {
    parts <- trimws(strsplit(seg, "\\|")[[1L]])
    parts <- parts[nzchar(parts)]
    if (length(parts)) atoms <- c(atoms, unique(parts))
  }
  unique(atoms[nzchar(atoms)])
}

# Actual 5 Buckets modules (Program Manager Workshops / session metadata); sidebar order + fallback when sheet has no parseable modules
MODULE_FILTER_ORDER <- c("Mindset", "Manage", "Borrow", "Grow", "Protect")

.order_module_labels_for_sidebar <- function(atoms) {
  if (is.null(atoms) || !length(atoms)) return(character(0))
  u <- unique(atoms[nzchar(atoms)])
  u <- u[!tolower(u) %in% c("unknown", "n/a", "na")]
  c(intersect(MODULE_FILTER_ORDER, u), sort(setdiff(u, MODULE_FILTER_ORDER)))
}

.unique_module_labels_from_df_col <- function(vec) {
  if (is.null(vec) || !length(vec)) return(character(0))
  atoms <- character(0)
  for (x in vec) atoms <- c(atoms, .pm_modules_atoms_from_cell(x))
  .order_module_labels_for_sidebar(atoms)
}

.program_manager_module_choices <- function(pm, post_df = NULL) {
  mods <- character(0)
  if (!is.null(pm) && nrow(pm) > 0) {
    cn <- colnames(pm)
    col <- if ("modules_taught" %in% cn) {
      "modules_taught"
    } else {
      hit <- grep("module", cn, ignore.case = TRUE, value = TRUE)
      if (length(hit)) hit[[1]] else NA_character_
    }
    if (!is.na(col) && nzchar(col)) mods <- .unique_module_labels_from_df_col(pm[[col]])
  }
  if (!length(mods) && !is.null(post_df) && nrow(post_df) > 0 && "modules_taught" %in% colnames(post_df)) {
    mods <- .unique_module_labels_from_df_col(post_df[["modules_taught"]])
  }
  if (!length(mods)) {
    mods <- MODULE_FILTER_ORDER
  }
  mods
}

.session_row_matches_selected_modules <- function(modules_taught_vec, selected_modules) {
  if (is.null(selected_modules) || !length(selected_modules)) return(rep(TRUE, length(modules_taught_vec)))
  vapply(seq_along(modules_taught_vec), function(i) {
    atoms <- .pm_modules_atoms_from_cell(modules_taught_vec[i])
    length(intersect(atoms, selected_modules)) > 0L
  }, logical(1))
}

# Collapsed modules: split each segment on "|" so "Grow|Protect" contributes the same satisfaction to both Grow and Protect
.satisfaction_expand_modules_collapsed <- function(df, sat_col, split_col) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(key = character(0), satisfaction = numeric(0)))
  if (!sat_col %in% colnames(df) || !split_col %in% colnames(df)) {
    return(data.frame(key = character(0), satisfaction = numeric(0)))
  }
  sat <- .coerce_numeric_vec(df[[sat_col]])
  sp <- .coerce_atomic_chr(df[[split_col]])
  out <- list()
  for (i in seq_len(nrow(df))) {
    if (!is.finite(sat[i])) next
    raw <- sp[i]
    if (is.na(raw) || !nzchar(trimws(as.character(raw)))) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    segments <- trimws(strsplit(as.character(raw), ",")[[1L]])
    segments <- segments[nzchar(segments)]
    if (!length(segments)) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    atoms <- character(0)
    for (seg in segments) {
      parts <- trimws(strsplit(seg, "\\|")[[1L]])
      parts <- parts[nzchar(parts)]
      if (length(parts)) atoms <- c(atoms, unique(parts))
    }
    atoms <- unique(atoms)
    if (!length(atoms)) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    for (a in atoms) {
      out[[length(out) + 1L]] <- data.frame(key = a, satisfaction = sat[i], stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame(key = character(0), satisfaction = numeric(0)))
  do.call(rbind, out)
}

# Collapsed facilitators: split each comma segment further on | & / so co-listed names each get the score
.satisfaction_expand_facilitators_collapsed <- function(df, sat_col, split_col) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(key = character(0), satisfaction = numeric(0)))
  if (!sat_col %in% colnames(df) || !split_col %in% colnames(df)) {
    return(data.frame(key = character(0), satisfaction = numeric(0)))
  }
  sat <- .coerce_numeric_vec(df[[sat_col]])
  sp <- .coerce_atomic_chr(df[[split_col]])
  out <- list()
  split_atoms <- function(s) {
    s <- trimws(as.character(s))
    if (!nzchar(s)) return(character(0))
    segs <- trimws(strsplit(s, ",")[[1L]])
    segs <- segs[nzchar(segs)]
    atoms <- character(0)
    for (seg in segs) {
      sub <- trimws(strsplit(seg, "[|&/]")[[1L]])
      sub <- sub[nzchar(sub)]
      atoms <- c(atoms, sub)
    }
    unique(atoms)
  }
  for (i in seq_len(nrow(df))) {
    if (!is.finite(sat[i])) next
    atoms <- split_atoms(sp[i])
    if (!length(atoms)) {
      out[[length(out) + 1L]] <- data.frame(key = "Unknown", satisfaction = sat[i], stringsAsFactors = FALSE)
      next
    }
    for (a in atoms) {
      out[[length(out) + 1L]] <- data.frame(key = a, satisfaction = sat[i], stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame(key = character(0), satisfaction = numeric(0)))
  do.call(rbind, out)
}

# Precise: the entire cell, exactly as recorded, is ONE category — co-listed facilitators or
# modules stay together as a single combination (only whitespace is normalized). This is what
# makes co-facilitated sessions show up as their own bar instead of being split apart.
.satisfaction_expand_wholecell <- function(df, sat_col, split_col) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(key = character(0), satisfaction = numeric(0)))
  if (!sat_col %in% colnames(df) || !split_col %in% colnames(df)) {
    return(data.frame(key = character(0), satisfaction = numeric(0)))
  }
  sat <- .coerce_numeric_vec(df[[sat_col]])
  sp <- .coerce_atomic_chr(df[[split_col]])
  out <- list()
  for (i in seq_len(nrow(df))) {
    if (!is.finite(sat[i])) next
    raw <- trimws(as.character(sp[i]))
    key <- if (is.na(raw) || !nzchar(raw)) "Unknown" else gsub("\\s+", " ", raw)
    out[[length(out) + 1L]] <- data.frame(key = key, satisfaction = sat[i], stringsAsFactors = FALSE)
  }
  if (!length(out)) return(data.frame(key = character(0), satisfaction = numeric(0)))
  do.call(rbind, out)
}

.satisfaction_group_stats <- function(long_df) {
  if (is.null(long_df) || nrow(long_df) == 0) {
    return(data.frame(key = character(0), n = integer(0), mean = numeric(0), lo = numeric(0), hi = numeric(0)))
  }
  ukeys <- unique(long_df$key)
  rows <- lapply(ukeys, function(k) {
    x <- long_df$satisfaction[long_df$key == k]
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 1) return(NULL)
    m <- mean(x)
    if (n >= 2) {
      se <- stats::sd(x) / sqrt(n)
      tc <- stats::qt(0.975, n - 1)
      lo <- m - tc * se
      hi <- m + tc * se
    } else {
      lo <- hi <- m
    }
    data.frame(key = k, n = n, mean = m, lo = lo, hi = hi, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(key = character(0), n = integer(0), mean = numeric(0), lo = numeric(0), hi = numeric(0)))
  }
  do.call(rbind, rows)
}

# Facilitator reliability: down-weight thin samples via empirical-Bayes shrinkage.
# Pools facilitators with n < min_n into "other (low n)" so nothing is silently dropped.
# Returns a data.frame (key, n, mean, sd, se, lo, hi, shrunk, grand_mean, excludes_grand)
# ordered by shrunken mean (desc). grand_mean also stored as an attribute.
.satisfaction_facilitator_reliability <- function(long_df, min_n = 5) {
  if (is.null(long_df) || nrow(long_df) == 0) return(NULL)
  long_df <- long_df[is.finite(long_df$satisfaction) &
                     long_df$satisfaction >= 1 &
                     long_df$satisfaction <= SESSION_SATISFACTION_MAX, , drop = FALSE]
  long_df <- long_df[long_df$key != "Unknown", , drop = FALSE]
  if (nrow(long_df) == 0) return(NULL)
  tab <- table(long_df$key)
  low <- names(tab)[tab < min_n]
  grp <- long_df$key
  grp[grp %in% low] <- "other (low n)"
  long_df$grp <- grp
  grand_mean <- mean(long_df$satisfaction)
  keys <- unique(long_df$grp)
  rows <- lapply(keys, function(k) {
    x <- long_df$satisfaction[long_df$grp == k]
    n <- length(x); m <- mean(x)
    s <- if (n >= 2) stats::sd(x) else NA_real_
    se <- if (n >= 2) s / sqrt(n) else NA_real_
    if (n >= 2) {
      tc <- stats::qt(0.975, n - 1)
      lo <- m - tc * se; hi <- m + tc * se
    } else { lo <- hi <- m }
    data.frame(key = k, n = n, mean = m, sd = s, se = se, lo = lo, hi = hi, stringsAsFactors = FALSE)
  })
  st <- do.call(rbind, rows)
  sigma2 <- mean(st$sd^2, na.rm = TRUE)
  if (!is.finite(sigma2)) sigma2 <- stats::var(long_df$satisfaction)
  if (!is.finite(sigma2) || sigma2 <= 0) sigma2 <- 1
  tau2 <- if (nrow(st) >= 2) max(0, stats::var(st$mean) - mean(sigma2 / st$n, na.rm = TRUE)) else 0
  w <- tau2 / (tau2 + sigma2 / st$n)
  w[!is.finite(w)] <- 0
  st$shrunk <- grand_mean + w * (st$mean - grand_mean)
  st$grand_mean <- grand_mean
  st$excludes_grand <- is.finite(st$lo) & is.finite(st$hi) & (st$lo > grand_mean | st$hi < grand_mean)
  st <- st[order(st$shrunk, decreasing = TRUE), , drop = FALSE]
  attr(st, "grand_mean") <- grand_mean
  attr(st, "sigma2") <- sigma2
  st
}

.hist_bar_labels <- function(counts, total_n, show_n, show_pct) {
  if (!show_n && !show_pct) return(NULL)
  if (length(counts) == 0) return(NULL)
  vapply(seq_along(counts), function(i) {
    if (!is.finite(counts[i]) || counts[i] <= 0) return("")
    parts <- character(0)
    if (show_n) parts <- c(parts, as.character(as.integer(counts[i])))
    if (show_pct && total_n > 0) parts <- c(parts, sprintf("%.0f%%", counts[i] / total_n * 100))
    paste(parts, collapse = "\n")
  }, character(1))
}

.satisfaction_plot_mean_bar <- function(long_df, plot_title) {
  tryCatch({
    long_df <- long_df[is.finite(long_df$satisfaction) & long_df$satisfaction >= 1 & long_df$satisfaction <= SESSION_SATISFACTION_MAX, , drop = FALSE]
    if (nrow(long_df) == 0) return(plotly_empty() %>% layout(title = "No linked satisfaction rows", font = PLOT_FONT))
    st <- .satisfaction_group_stats(long_df)
    if (nrow(st) == 0) return(plotly_empty() %>% layout(title = "No groups", font = PLOT_FONT))
    st <- st[order(st$mean), , drop = FALSE]
    st$key <- factor(st$key, levels = st$key)
    ngrp <- nrow(st)
    cols <- REACH_PALETTE[seq_len(ngrp) %% length(REACH_PALETTE) + 1]
    plot_ly(
      st,
      x = ~mean, y = ~key, type = "bar", orientation = "h",
      marker = list(color = cols),
      error_x = list(
        type = "data", symmetric = FALSE,
        array = pmax(0, st$hi - st$mean),
        arrayminus = pmax(0, st$mean - st$lo),
        color = "#555555", thickness = 1.2
      ),
      hoverinfo = "text",
      hovertext = ~paste0("n=", n, ", mean=", round(mean, 2), "  95% CI [", round(lo, 2), ", ", round(hi, 2), "]")
    ) %>%
      layout(
        title = plot_title,
        font = PLOT_FONT,
        xaxis = list(title = "Mean satisfaction", range = c(1, SESSION_SATISFACTION_MAX)),
        yaxis = list(title = ""),
        margin = list(l = 220, t = 55, b = 50)
      )
  }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
}

.satisfaction_plot_dist_facets <- function(long_df, main_title, share_y, show_n, show_pct) {
  tryCatch({
    long_df <- long_df[is.finite(long_df$satisfaction) & long_df$satisfaction >= 1 & long_df$satisfaction <= SESSION_SATISFACTION_MAX, , drop = FALSE]
    if (nrow(long_df) == 0) return(plotly_empty())
    st <- .satisfaction_group_stats(long_df)
    ord <- st$key[order(st$mean, decreasing = TRUE)]
    plots <- lapply(seq_along(ord), function(i) {
      k <- ord[i]
      xv <- long_df$satisfaction[long_df$key == k]
      xv <- xv[is.finite(xv)]
      col <- REACH_PALETTE[(i - 1) %% length(REACH_PALETTE) + 1]
      brks <- seq(0.5, SESSION_SATISFACTION_MAX + 0.5, 1)
      h <- hist(xv, breaks = brks, plot = FALSE)
      mids <- seq_len(SESSION_SATISFACTION_MAX)
      total_n <- length(xv)
      txt <- .hist_bar_labels(h$counts, total_n, show_n, show_pct)
      has_txt <- !is.null(txt) && any(nzchar(txt))
      ttl <- if (isTRUE(show_n)) paste0(k, " (n=", total_n, ")") else k
      plot_ly(
        x = mids, y = h$counts, type = "bar", name = ttl, showlegend = FALSE,
        marker = list(color = col, line = list(color = "white", width = 0.4)),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE,
        hovertemplate = paste0("<b>", ttl, "</b><br>Score: %{x}<br>Count: %{y}<extra></extra>")
      ) %>%
        layout(
          title = list(text = ttl, font = list(size = 11)),
          xaxis = list(range = c(0.5, SESSION_SATISFACTION_MAX + 0.5), dtick = 1, title = "Satisfaction (1-6)"),
          yaxis = list(title = "Count"),
          showlegend = FALSE
        )
    })
    nr <- max(1L, as.integer(ceiling(length(plots) / 3)))
    plotly::subplot(plots, nrows = nr, shareX = TRUE, shareY = isTRUE(share_y), margin = 0.08, titleY = TRUE) %>%
      layout(
        title = list(text = main_title, font = PLOT_FONT$family),
        font = PLOT_FONT,
        margin = list(t = 60, b = 50),
        showlegend = FALSE
      )
  }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
}

# Overview zip maps: parse plotly geo relayout for zoom-linked aggregation.
# Relayout events can arrive as a list (e.g. autosize on layout reflow) rather than a
# data frame, so guard against a NULL/empty nrow before any logical comparison.
parse_plotly_geo_relayout <- function(ev) {
  if (is.null(ev)) return(NULL)
  nr <- nrow(ev)
  if (is.null(nr) || length(nr) == 0L || is.na(nr) || nr < 1L) return(NULL)
  ev1 <- ev[1, , drop = FALSE]
  cn <- names(ev1)
  pick_range <- function(axis) {
    exact <- paste0("geo.", axis, "axis.range")
    if (exact %in% cn) {
      v <- ev1[[1, exact]]
      if (is.list(v) && length(v)) v <- v[[1]]
      if (length(v) >= 2 && is.numeric(v)) return(range(v))
    }
    c0 <- paste0("geo.", axis, "axis.range[0]")
    c1 <- paste0("geo.", axis, "axis.range[1]")
    if (c0 %in% cn && c1 %in% cn) {
      a <- suppressWarnings(as.numeric(ev1[[1, c0]]))
      b <- suppressWarnings(as.numeric(ev1[[1, c1]]))
      if (is.finite(a) && is.finite(b)) return(range(c(a, b)))
    }
    NULL
  }
  lonr <- pick_range("lon")
  latr <- pick_range("lat")
  if (is.null(lonr) || is.null(latr)) return(NULL)
  list(lon = lonr, lat = latr)
}

grid_deg_from_view <- function(view) {
  if (is.null(view)) return(2.8)
  lon_span <- abs(diff(view$lon))
  lat_span <- abs(diff(view$lat))
  sp <- max(lon_span, lat_span, na.rm = TRUE)
  if (!is.finite(sp) || sp <= 0) return(2.8)
  if (sp > 42) return(6)
  if (sp > 22) return(3.5)
  if (sp > 12) return(2)
  if (sp > 6) return(1)
  if (sp > 3) return(0.5)
  if (sp > 1.2) return(0.22)
  if (sp > 0.45) return(0.1)
  0
}

aggregate_zip_markers_for_map <- function(m, grid_deg) {
  if (is.null(m) || nrow(m) == 0) return(m)
  if (grid_deg <= 0) {
    m$hover <- paste0("Zip: ", m$zip, "<br>Learners: ", m$n)
    m$siz <- pmax(16, sqrt(m$n) * 8)
    return(m)
  }
  m$lat_g <- round(m$lat / grid_deg) * grid_deg
  m$lon_g <- round(m$lon / grid_deg) * grid_deg
  out <- m %>%
    dplyr::group_by(lat_g, lon_g) %>%
    dplyr::summarise(
      n = sum(n),
      lat = mean(lat),
      lon = mean(lon),
      n_zips = dplyr::n(),
      .groups = "drop"
    )
  gd <- format(grid_deg, digits = 2, trim = TRUE)
  out$hover <- paste0(
    "Learners: ", out$n,
    "<br>(", out$n_zips, " zip codes, ~", gd, "\u00b0 grid)",
    "<br>Approx. ", round(out$lat, 2), ", ", round(out$lon, 2)
  )
  out$siz <- pmax(22, 16 + sqrt(out$n) * 9)
  out
}

view_changed_zip_map <- function(old, new, tol = 0.02) {
  if (is.null(new)) return(FALSE)
  if (is.null(old)) return(TRUE)
  max(abs(old$lon - new$lon), abs(old$lat - new$lat), na.rm = TRUE) > tol
}

# Canonical factor levels for comparable Pre/Post charts (youngest first). These always show
# (even at zero count) so Pre/Post axes line up. The survey age brackets changed: the former
# "Under 18" was split into "Under 16" and "16-18", so "Under 18" is NOT in this always-show list.
AGE_LEVELS_CANONICAL <- c("Under 16", "16-18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
# Legacy "Under 18" is still recognized if it appears in old data, and placed in the youth region.
# It is omitted above so an empty "Under 18" bar does not render when there are no such responses.
AGE_LEVELS_YOUTH <- c("Under 16", "16-18", "Under 18")
# Normalize age labels. (Previously merged "16-18" into "Under 18"; that merge is removed now that
# "Under 16" and "16-18" are distinct categories.)
AGE_LEVELS_NORMALIZE <- function(v) {
  if (is.null(v)) return(v)
  trimws(as.character(v))
}
# Canonical Race/Ethnicity order (matches the survey form). Any response not in this preset
# list (e.g. legacy "Asian", free-text "Vietnamese") is folded into "Other".
RACE_LEVELS_CANONICAL <- c(
  "Black or African American",
  "Hispanic or Latina/o/x",
  "American Indian or Alaska Native",
  "East Asian (e.g. Chinese, Japanese, Korean)",
  "South Asian (e.g. Indian, Pakistani, etc.)",
  "Southeast Asian (e.g. Filipino, Vietnamese, etc.)",
  "Central Asian (e.g., Nepali, Kazakh, etc.)",
  "Middle Eastern or North African (MENA)",
  "Native Hawaiian or Other Pacific Islander",
  "Indigenous (outside of the U.S.)",
  "White",
  "Prefer not to answer",
  "Other"
)
# Preset responses (everything except the catch-all "Other")
RACE_PRESET_RESPONSES <- setdiff(RACE_LEVELS_CANONICAL, "Other")
# The Annual form and the Big Pre/Post forms punctuate the same categories differently
# ("e.g.," vs "e.g.", "Alaska native" vs "Alaska Native"), so preset matching compares on a
# case- and punctuation-insensitive key rather than the raw string. Without this, Annual
# answers land in "Other".
.race_match_key <- function(x) {
  k <- tolower(trimws(as.character(x)))
  k <- gsub("[.,;]", "", k)
  gsub("[[:space:]]+", " ", k)
}
RACE_KEY_TO_CANONICAL <- setNames(RACE_PRESET_RESPONSES, .race_match_key(RACE_PRESET_RESPONSES))
# Label variants across form generations that must resolve to one category. Big Pre still uses
# the shorter Central Asian wording while Big Post added "Afghan"; both are real answers.
RACE_KEY_ALIASES <- c("Central Asian (e.g., Nepali, Kazakh, etc.)")
names(RACE_KEY_ALIASES) <- .race_match_key("Central Asian (e.g., Afghan, Nepali, Kazakh, etc.)")
RACE_KEY_TO_CANONICAL <- c(RACE_KEY_TO_CANONICAL, RACE_KEY_ALIASES)
# Render the Variable Mapping (Yes/No coverage) table with color-coded cells so
# "Yes" stands out (green) and "No" recedes (muted gray).
.style_variable_mapping_table <- function(mapping) {
  yn_cols <- setdiff(colnames(mapping), c("Variable", "Notes", "Note"))
  # Only style columns that look like Yes/No coverage (avoid coloring long note text)
  yn_cols <- yn_cols[vapply(yn_cols, function(nm) {
    vals <- unique(as.character(mapping[[nm]]))
    all(vals %in% c("Yes", "No", NA, ""))
  }, logical(1))]
  display_names <- gsub("_", " ", colnames(mapping))  # "Big_Pre" -> "Big Pre" in headers
  DT::datatable(
    mapping,
    class = "row-border vm-coverage",
    width = "auto",
    colnames = display_names,
    options = list(pageLength = 15, dom = "t", ordering = FALSE, autoWidth = FALSE),
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      columns = yn_cols,
      backgroundColor = DT::styleEqual(c("Yes", "No"), c("#1e7d44", "#f3f4f6")),
      color = DT::styleEqual(c("Yes", "No"), c("#ffffff", "#9aa0a6")),
      fontWeight = DT::styleEqual(c("Yes", "No"), c("bold", "normal")),
      textAlign = "center"
    )
}
INCOME_ORDER <- c(
  "If one person under $36k; two people under $41k; family of four under $52k",
  "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
  "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
  "If one person over $97k; two people over $111k; family of four over $139k"
)
# Short labels for income x-axis (full text remains in hover)
INCOME_SHORT_LABELS <- setNames(
  c("<$52k", "$52k-$87k", "$87k-$139k", ">$139k"),
  INCOME_ORDER
)

# Fixed Gender Identity options as shown on current forms (plus bare "Other").
GENDER_FORM_OPTIONS <- c(
  "Female",
  "Male",
  "Non-binary / Gender non-conforming",
  "Prefer to self-describe",
  "Other"
)

#' Gender labels for Reach charts.
#' - Known form options: case/spacing consensus only (male -> Male).
#' - Write-ins (e.g. "Gay"): default bucket as "Other"; if expand_others=TRUE keep each raw string.
.gender_for_reach_chart <- function(x, expand_others = FALSE) {
  if (is.null(x)) return(x)
  v <- trimws(as.character(x))
  v[v %in% c("", NA_character_)] <- NA_character_
  out <- rep(NA_character_, length(v))
  v_low <- tolower(v)
  out[!is.na(v_low) & v_low == "female"] <- "Female"
  out[!is.na(v_low) & v_low == "male"] <- "Male"
  out[!is.na(v_low) & grepl("non-?binary|gender non-?conform", v_low)] <-
    "Non-binary / Gender non-conforming"
  out[!is.na(v_low) & grepl("prefer to self-describe", v_low)] <- "Prefer to self-describe"
  out[!is.na(v_low) & v_low == "other"] <- "Other"
  still <- is.na(out) & !is.na(v)
  if (isTRUE(expand_others)) {
    out[still] <- v[still]
  } else {
    out[still] <- "Other"
  }
  out
}

# Back-compat alias (same as expand_others = FALSE)
.normalize_gender_for_dashboard <- function(x) .gender_for_reach_chart(x, expand_others = FALSE)
# Canonical education order (low → high). Used for filters, Reach, Learning heatmaps, Pre/Post dist plots.
# Raw sheet labels are mapped here via .normalize_education_for_dashboard() so "Middle school" and
# legacy text align to one ordering everywhere.
EDUCATION_ORDER <- c(
  "Middle school",
  "Some high school",
  "High school diploma or equivalent (GED)",
  "Trade school or technical certificate",
  "Some college, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree (JD, MD, etc.)",
  "Doctorate (PhD, EdD, etc.)"
)

# Map free-text / legacy education responses to EDUCATION_ORDER labels
.normalize_education_for_dashboard <- function(x) {
  if (is.null(x)) return(x)
  v <- trimws(as.character(x))
  v[v %in% c("", NA_character_)] <- NA_character_
  v <- gsub("\u2019", "'", v, fixed = TRUE)
  v_low <- tolower(v)
  out <- rep(NA_character_, length(v))
  # Middle / elementary (before high school)
  out[!is.na(v_low) & grepl("middle school|^middle\\b|elementary", v_low)] <- "Middle school"
  out[is.na(out) & !is.na(v_low) & grepl("some middle", v_low)] <- "Middle school"
  out[is.na(out) & !is.na(v_low) & grepl("some high school", v_low)] <- "Some high school"
  out[is.na(out) & !is.na(v_low) & grepl("high school diploma|\\bged\\b|equivalent", v_low)] <- "High school diploma or equivalent (GED)"
  out[is.na(out) & !is.na(v_low) & grepl("trade school|technical certificate", v_low)] <- "Trade school or technical certificate"
  out[is.na(out) & !is.na(v_low) & grepl("^some college, no degree", v_low)] <- "Some college, no degree"
  out[is.na(out) & !is.na(v_low) & v_low == "some college"] <- "Some college, no degree"
  out[is.na(out) & !is.na(v_low) & grepl("^associate", v_low)] <- "Associate's degree"
  out[is.na(out) & !is.na(v_low) & grepl("^bachelor", v_low)] <- "Bachelor's degree"
  out[is.na(out) & !is.na(v_low) & grepl("^master", v_low)] <- "Master's degree"
  out[is.na(out) & !is.na(v_low) & grepl("professional degree|\\bjd\\b|\\bmd\\b", v_low)] <- "Professional degree (JD, MD, etc.)"
  out[is.na(out) & !is.na(v_low) & grepl("doctorate|doctoral|phd|edd", v_low)] <- "Doctorate (PhD, EdD, etc.)"
  still <- is.na(out) & !is.na(v)
  out[still] <- v[still]
  out
}

# order_education_levels — Big Post education dist (overrides any legacy stub from sourced files)
order_education_levels <- function(edu_vec) {
  v <- .normalize_education_for_dashboard(edu_vec)
  extra <- sort(setdiff(unique(v[!is.na(v)]), EDUCATION_ORDER))
  factor(v, levels = c(EDUCATION_ORDER, extra), ordered = TRUE)
}

# ============================================================================
# Reusable "Display options" card so chart toggles (Same Y / Show N / Show %) look identical
# across every tab. Pass the relevant checkboxInput()s; each is wrapped in its own chip.
# `...` carries the checkboxInput()/numericInput() chips; `inline = TRUE` lays the chips out as
# equal-width columns on a single row (e.g. Bars | Curves | Bins) so they fill the card width.
display_options_card <- function(..., inline = FALSE) {
  chips <- Filter(Negate(is.null), list(...))
  cls <- if (inline) "display-options-card display-options-card--inline" else "display-options-card"
  div(class = cls,
    tags$span(class = "display-options-label", "Display options"),
    div(class = "display-options-chips",
      lapply(chips, function(ch) div(class = "display-option-chip", ch))
    )
  )
}

# Sidebar section presented as a labeled card, matching the display-options aesthetic.
sidebar_card <- function(title, ...) {
  div(class = "sidebar-card",
    if (!is.null(title)) tags$span(class = "sidebar-card-label", title),
    ...
  )
}

# ---- Deterministic (no-AI) summary boxes -------------------------------------
# Reproducible plain-English stats card shared across tabs. No LLM — just sprintf
# templates over standard t-tests / effect sizes so every box reads identically.
.fmt_p <- function(p) {
  if (is.null(p) || !is.finite(p)) return("\u2014")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

.signif_verdict <- function(p, effect, kind = c("level", "change"), alpha = 0.05) {
  kind <- match.arg(kind)
  if (is.null(p) || !is.finite(p)) return(list(text = "Not enough data for a significance test.", color = "#5f6369"))
  sig <- p < alpha
  if (kind == "change") {
    if (!sig) return(list(text = "No statistically significant change.", color = "#5f6369"))
    if (effect > 0) return(list(text = "Statistically significant improvement.", color = "#3c7a3c"))
    return(list(text = "Statistically significant decline.", color = "#b54b3a"))
  }
  if (!sig) return(list(text = "Not significantly different from the midpoint.", color = "#5f6369"))
  if (effect > 0) return(list(text = "Significantly above the midpoint.", color = "#3c7a3c"))
  list(text = "Significantly below the midpoint.", color = "#b54b3a")
}

# Normalize single-choice sidebar filters (language, etc.). With shinymanager, inputs can be NA before login;
# using them in `if (x != "All")` without this guard triggers "missing value where TRUE/FALSE needed".
.sidebar_filter_choice <- function(x, default = "All") {
  x <- tryCatch(x, error = function(e) NULL)
  if (is.null(x) || length(x) == 0L) return(default)
  if (length(x) == 1L && is.na(x)) return(default)
  x <- trimws(as.character(x))
  if (length(x) == 1L && !nzchar(x)) return(default)
  x[[1]]
}

# Multi-select org/group: empty / NULL / "All" → character(0) meaning no restriction.
.sidebar_filter_values <- function(x) {
  x <- tryCatch(x, error = function(e) NULL)
  if (is.null(x) || length(x) == 0L) return(character(0))
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x) & !identical(x, "All") & x != "All"]
  unique(x)
}

.sidebar_filter_active <- function(x) {
  length(.sidebar_filter_values(x)) > 0L
}

# Composite org+group keys in the Group multi-select (nested under org optgroups).
.ORG_GROUP_SEP <- "|||"
.org_group_key <- function(org, group) {
  paste0(trimws(as.character(org)), .ORG_GROUP_SEP, trimws(as.character(group)))
}
.org_group_parse_keys <- function(keys) {
  keys <- .sidebar_filter_values(keys)
  if (!length(keys)) {
    return(data.frame(org = character(0), group = character(0), key = character(0),
                      stringsAsFactors = FALSE))
  }
  parts <- strsplit(keys, .ORG_GROUP_SEP, fixed = TRUE)
  orgs <- vapply(parts, function(p) if (length(p) >= 1) p[[1]] else NA_character_, character(1))
  grps <- vapply(parts, function(p) {
    if (length(p) >= 2) paste(p[-1], collapse = .ORG_GROUP_SEP) else NA_character_
  }, character(1))
  ok <- !is.na(orgs) & !is.na(grps) & nzchar(orgs) & nzchar(grps)
  data.frame(org = orgs[ok], group = grps[ok], key = keys[ok], stringsAsFactors = FALSE)
}

# Row keep-mask: orgs are an include set; group keys are org-scoped.
# - no orgs selected → keep all
# - orgs selected, no group keys → keep all rows in those orgs
# - orgs + group keys → whole org if that org has no picked groups; else only picked groups
.rows_match_org_group_scope <- function(org_vec, group_vec, selected_orgs, selected_group_keys) {
  n <- length(org_vec)
  if (!n) return(logical(0))
  selected_orgs <- .sidebar_filter_values(selected_orgs)
  if (!length(selected_orgs)) return(rep(TRUE, n))
  org_chr <- trimws(as.character(org_vec))
  grp_chr <- trimws(as.character(group_vec))
  keep <- !is.na(org_chr) & org_chr %in% selected_orgs
  parsed <- .org_group_parse_keys(selected_group_keys)
  if (!nrow(parsed)) return(keep)
  restricted <- unique(parsed$org)
  key_set <- parsed$key
  row_keys <- .org_group_key(org_chr, grp_chr)
  needs_group <- keep & org_chr %in% restricted
  keep[needs_group] <- row_keys[needs_group] %in% key_set
  keep
}

.apply_org_group_scope <- function(data, selected_orgs, selected_group_keys,
                                   org_col = "org_name", group_col = "group") {
  if (is.null(data) || nrow(data) == 0) return(data)
  if (!org_col %in% colnames(data)) return(data)
  gvec <- if (group_col %in% colnames(data)) data[[group_col]] else rep(NA_character_, nrow(data))
  keep <- .rows_match_org_group_scope(data[[org_col]], gvec, selected_orgs, selected_group_keys)
  data[keep, , drop = FALSE]
}

# Build selectize optgroup choices: list(Org = c(GroupLabel = "Org|||Group", ...), ...)
.build_group_optgroup_choices <- function(pre_data, post_data, selected_orgs) {
  selected_orgs <- .sidebar_filter_values(selected_orgs)
  if (!length(selected_orgs)) return(list())
  pairs <- list()
  add_pairs <- function(df) {
    if (is.null(df) || nrow(df) == 0) return()
    if (!all(c("org_name", "group") %in% colnames(df))) return()
    o <- trimws(as.character(df$org_name))
    g <- trimws(as.character(df$group))
    ok <- !is.na(o) & nzchar(o) & !is.na(g) & nzchar(g) & o %in% selected_orgs
    if (!any(ok)) return()
    pairs[[length(pairs) + 1L]] <<- data.frame(org = o[ok], group = g[ok], stringsAsFactors = FALSE)
  }
  add_pairs(pre_data)
  add_pairs(post_data)
  if (!length(pairs)) return(list())
  all_pairs <- unique(do.call(rbind, pairs))
  all_pairs <- all_pairs[order(all_pairs$org, all_pairs$group), , drop = FALSE]
  by_org <- split(all_pairs, all_pairs$org)
  lapply(by_org, function(df) {
    setNames(.org_group_key(df$org, df$group), df$group)
  })
}

# Format multi-select for scope text / favorites display.
.sidebar_filter_label <- function(vals, singular = "organization", plural = "organizations") {
  vals <- .sidebar_filter_values(vals)
  if (!length(vals)) return(NULL)
  if (length(vals) == 1L) return(paste0(singular, " \u201C", vals[[1]], "\u201D"))
  if (length(vals) <= 3L) {
    return(paste0(plural, " ", paste0("\u201C", vals, "\u201D", collapse = ", ")))
  }
  paste0(length(vals), " ", plural)
}

.sidebar_group_scope_label <- function(group_keys, selected_orgs) {
  parsed <- .org_group_parse_keys(group_keys)
  selected_orgs <- .sidebar_filter_values(selected_orgs)
  if (!nrow(parsed)) return(NULL)
  bits <- vapply(seq_len(nrow(parsed)), function(i) {
    paste0("\u201C", parsed$group[[i]], "\u201D in ", parsed$org[[i]])
  }, character(1))
  if (length(bits) <= 3L) paste(bits, collapse = "; ") else paste0(length(bits), " org-groups")
}

# TRUE when a selectInput has a real user choice (not NULL / NA / blank).
.has_select_value <- function(x) {
  x <- tryCatch(x, error = function(e) NULL)
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

# Sidebar-aware scope sentence: states whether a box reflects the full population
# or a filtered subset. `input` is the server's reactive input object.
scope_sentence <- function(input) {
  orgs <- .sidebar_filter_values(tryCatch(input$selected_org, error = function(e) NULL))
  grps <- .sidebar_filter_values(tryCatch(input$selected_group, error = function(e) NULL))
  lang <- .sidebar_filter_choice(tryCatch(input$filter_language, error = function(e) NULL), default = "All")
  use_date <- tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE)
  dr   <- tryCatch(input$date_range, error = function(e) NULL)
  parts <- character(0)
  org_lab <- .sidebar_filter_label(orgs, "organization", "organizations")
  grp_lab <- .sidebar_group_scope_label(grps, orgs)
  if (!is.null(org_lab)) parts <- c(parts, org_lab)
  if (!is.null(grp_lab)) parts <- c(parts, grp_lab)
  if (!is.null(lang) && length(lang) == 1 && nzchar(lang) && lang != "All") parts <- c(parts, paste0(lang, " responses"))
  if (use_date && !is.null(dr) && length(dr) >= 2) parts <- c(parts, paste0(format(as.Date(dr[1])), " to ", format(as.Date(dr[2]))))
  if (!length(parts)) return("These results reflect all 5 Buckets workshops (no filters applied).")
  paste0("Filtered to: ", paste(parts, collapse = "; "), ".")
}

# mode = "level"  : one-sample t-test of `scores` vs `null_value` (agree scale, satisfaction mean, ...)
# mode = "change" : paired t-test of `post` vs `pre` (pre/post improvement)
deterministic_summary_box <- function(scores = NULL, pre = NULL, post = NULL,
                                      mode = c("level", "change"),
                                      null_value = 0,
                                      scale_label = "(\u22123 to +3)",
                                      headline_label = NULL,
                                      positive_threshold = NULL,
                                      scope_text = NULL,
                                      accent = "#5c2f92") {
  mode <- match.arg(mode)
  box <- function(...) div(class = "summary-box", style = paste0("border-left-color:", accent, ";"), ...)
  empty_msg <- function(msg) box(tags$p(class = "summary-box-detail", msg))
  if (mode == "level") {
    sc <- scores[is.finite(scores)]
    n <- length(sc)
    if (n < 1) return(empty_msg("No responses for the current filters."))
    m <- mean(sc); sdv <- if (n > 1) stats::sd(sc) else NA_real_
    thr <- if (is.null(positive_threshold)) null_value else positive_threshold
    pct_pos <- round(100 * mean(sc > thr), 1)
    tt <- if (n >= 2) tryCatch(stats::t.test(sc, mu = null_value), error = function(e) NULL) else NULL
    p_val <- if (!is.null(tt)) tt$p.value else NA_real_
    d <- if (!is.null(sdv) && is.finite(sdv) && sdv > 0) (m - null_value) / sdv else NA_real_
    verdict <- .signif_verdict(p_val, m - null_value, "level")
    head_lbl <- if (is.null(headline_label)) "Positive responses" else headline_label
    box(
      tags$p(class = "summary-box-headline",
        tags$strong(paste0(head_lbl, ": ")), sprintf("%s%% of %s respondents.", pct_pos, n)),
      tags$p(class = "summary-box-detail",
        sprintf("Mean %s = %.2f", scale_label, m),
        if (is.finite(sdv)) sprintf(" (SD = %.2f). ", sdv) else ". ",
        sprintf("One-sample t-test vs %s: p = %s. ", format(null_value), .fmt_p(p_val)),
        if (is.finite(d)) sprintf("Cohen's d = %.2f. ", d) else "",
        tags$span(style = paste0("color:", verdict$color, "; font-weight:600;"), verdict$text)),
      if (!is.null(scope_text)) tags$p(class = "summary-box-scope", scope_text)
    )
  } else {
    ok <- is.finite(pre) & is.finite(post)
    pr <- pre[ok]; po <- post[ok]
    n <- length(pr)
    if (n < 2) return(empty_msg("Paired comparison needs at least 2 matched respondents."))
    diff <- po - pr
    md <- mean(diff); sdd <- stats::sd(diff)
    tt <- tryCatch(stats::t.test(po, pr, paired = TRUE), error = function(e) NULL)
    p_val <- if (!is.null(tt)) tt$p.value else NA_real_
    d <- if (is.finite(sdd) && sdd > 0) md / sdd else NA_real_
    verdict <- .signif_verdict(p_val, md, "change")
    head_lbl <- if (is.null(headline_label)) "Mean change (Post \u2212 Pre)" else headline_label
    pct_up <- round(100 * mean(diff > 0), 1)
    box(
      tags$p(class = "summary-box-headline",
        tags$strong(paste0(head_lbl, ": ")),
        sprintf("%+.2f points across %s matched respondents (%s%% improved).", md, n, pct_up)),
      tags$p(class = "summary-box-detail",
        sprintf("SD of change = %.2f. ", sdd),
        sprintf("Paired t-test: p = %s. ", .fmt_p(p_val)),
        if (is.finite(d)) sprintf("Cohen's d = %.2f. ", d) else "",
        tags$span(style = paste0("color:", verdict$color, "; font-weight:600;"), verdict$text)),
      if (!is.null(scope_text)) tags$p(class = "summary-box-scope", scope_text)
    )
  }
}

# UI
# ============================================================================

ui <- fluidPage(
  
  # Boot loading covers menus/content only (header is rendered first and stays above the overlay).
  tags$head(
    tags$script(HTML("
      document.documentElement.classList.add('dash-boot-loading');
    ")),
    tags$script(src = "favorites.js"),
    tags$script(src = "favorites_shiny.js"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$style(HTML("
      body {
        background-color: #ffffff;
        color: #333333;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        font-size: 14px;
        -webkit-font-smoothing: antialiased;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        letter-spacing: -0.01em;
      }
      .navbar {
        background-color: #5c2f92 !important;
        border-color: #5c2f92 !important;
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: bold;
      }
      h1, h2, h3, h4 {
        color: #5c2f92;
        font-weight: bold;
      }
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
      }
      /* Overview Response Descriptives: emphasize Session Total + Total columns */
      #impact_overview_survey_type_stats_table table th:nth-child(6),
      #impact_overview_survey_type_stats_table table td:nth-child(6),
      #impact_overview_survey_type_stats_table table th:nth-child(8),
      #impact_overview_survey_type_stats_table table td:nth-child(8) {
        background-color: #efe8f7 !important;
        font-weight: 600;
      }
      #impact_overview_survey_type_stats_table table thead th:nth-child(6),
      #impact_overview_survey_type_stats_table table thead th:nth-child(8) {
        background-color: #4a2673 !important;
        color: #ffffff !important;
      }
      /* Tabs styled as sleek pill buttons (works for both .nav-tabs and pill tabsets) */
      .nav-tabs, .nav-pills {
        border-bottom: none;
        gap: 8px;
        padding: 4px 0 12px 0;
        display: flex;
        flex-wrap: wrap;
      }
      .nav-tabs > li, .nav-pills > li { margin: 0; float: none; }
      .nav-tabs > li > a, .nav-pills > li > a {
        color: #5c2f92;
        font-weight: 600;
        font-size: 13px;
        border: 1px solid #e3dcef;
        border-radius: 999px;
        background: #ffffff;
        padding: 7px 16px;
        margin: 0;
        transition: background 0.15s, color 0.15s, border-color 0.15s, box-shadow 0.15s;
      }
      .nav-tabs > li > a:hover, .nav-pills > li > a:hover {
        background: #f3eefb;
        border-color: #c9b8e6;
        color: #4a2673;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus,
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover,
      .nav-pills > li.active > a:focus {
        background: #5c2f92;
        border-color: #5c2f92;
        color: #ffffff;
        box-shadow: 0 2px 8px rgba(92, 47, 146, 0.28);
      }
      .btn-primary {
        background-color: #5c2f92;
        border-color: #5c2f92;
      }
      .btn-primary:hover {
        background-color: #4a2673;
        border-color: #4a2673;
      }
      .fav-star-btn { color: #c9a227; padding: 0 4px; margin-left: 6px; font-size: 16px; vertical-align: middle; }
      .fav-star-btn:hover { color: #5c2f92; }
      .fav-header-row { display: flex; align-items: center; flex-wrap: wrap; gap: 4px; }
      table {
        border-collapse: collapse;
      }
      table thead tr {
        background-color: #5c2f92;
        color: #ffffff;
      }
      table tbody tr:nth-child(even) {
        background-color: #f8f9fa;
      }
      /* Full-height app shell: title locked at top; sidebar + main scroll independently */
      html, body { height: 100%; margin: 0; }
      body > .container-fluid {
        height: 100vh;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      body > .container-fluid > .app-header { flex: 0 0 auto; }
      body > .container-fluid > #dashboard_loading_overlay { flex: 0 0 auto; }
      /* Title banner: logo + Impact Dashboard title (always above loading overlay) */
      .app-header {
        position: relative;
        z-index: 13000;
        display: flex;
        align-items: center;
        gap: 26px;
        padding: 10px 24px;
        margin: 4px 0 8px 0;
        background: #ffffff;
        border: 1px solid #e3dcef;
        border-radius: 12px;
        box-shadow: 0 1px 6px rgba(92, 47, 146, 0.06);
      }
      .app-header-logo { height: 84px; width: auto; flex-shrink: 0; background: #ffffff; border-radius: 6px; padding: 4px 8px; }
      .app-header-divider { width: 1px; align-self: stretch; background: #e0d6f0; margin: 6px 0; }
      .app-header-text { display: flex; flex-direction: column; justify-content: center; }
      .app-header-title {
        color: #5c2f92;
        font-weight: 800;
        font-size: 34px;
        line-height: 1.1;
        letter-spacing: -0.02em;
      }
      .app-header-subtitle {
        color: #8a7fa0;
        font-weight: 500;
        font-size: 15px;
        margin-top: 4px;
        letter-spacing: 0.01em;
      }
      .filter-label-row {
        display: flex;
        align-items: center;
        gap: 8px;
        margin: 10px 0 6px 0;
      }
      .selectize-dropdown .optgroup-header {
        font-weight: 700;
        color: #5c2f92;
        font-size: 12px;
        padding: 8px 10px 4px 10px;
        background: #f7f4fb;
        border-bottom: 1px solid #e3dcef;
      }
      .selectize-dropdown .optgroup .option {
        padding-left: 18px;
      }
      /* The sidebarLayout row fills the remaining height below the title */
      body > .container-fluid > .row {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        align-items: stretch;
        margin-left: 0;
        margin-right: 0;
      }
      /* Each top-level column (sidebar + main) gets its own scrollbar */
      body > .container-fluid > .row > [class*='col-sm-'] {
        height: 100%;
        min-height: 0;
        overflow-y: auto;
        overflow-x: hidden;
        -webkit-overflow-scrolling: touch;
      }
      #dashboard_sidebar { padding-right: 8px !important; }
      .learning-wc-row {
        margin-bottom: 8px;
        width: 100%;
      }
      .learning-wc-row .col-sm-6 {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .learning-wc-row .shiny-plot-output {
        width: 100% !important;
        max-width: 100%;
      }
      .learning-sentiment-panel { margin-top: 12px; max-width: 520px; margin-left: auto; margin-right: auto; }
      /* Collapsible sections styled as cards with a minimalist chevron */
      details {
        border: 1px solid #e3dcef;
        border-radius: 10px;
        background: #ffffff;
        margin: 14px 0;
        box-shadow: 0 1px 4px rgba(92, 47, 146, 0.05);
        overflow: hidden;
      }
      details:first-child { margin-top: 0; }
      details > summary {
        cursor: pointer;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 12px 16px;
        background: #faf8fd;
        border-bottom: 1px solid transparent;
        transition: background 0.15s ease;
        user-select: none;
      }
      details > summary:hover { background: #f1ebfa; }
      details[open] > summary { border-bottom-color: #ece5f6; }
      details summary::-webkit-details-marker { display: none; }
      /* Minimalist chevron drawn from two borders; rotates right -> down when open */
      details > summary::before {
        content: '';
        width: 7px;
        height: 7px;
        border-right: 2px solid #8a6fb8;
        border-bottom: 2px solid #8a6fb8;
        transform: rotate(-45deg);
        transition: transform 0.2s ease, border-color 0.15s ease;
        flex-shrink: 0;
        margin-top: -2px;
      }
      details > summary:hover::before { border-color: #5c2f92; }
      details[open] > summary::before { transform: rotate(45deg); }
      details summary h4, details summary h5, details summary h6 {
        margin: 0;
        display: inline-block;
      }
      /* Inset the section content from the card edges */
      details > *:not(summary) { margin-left: 16px; margin-right: 16px; }
      details > *:not(summary):first-of-type { margin-top: 14px; }
      details > *:not(summary):last-child { margin-bottom: 14px; }
      /* Nested sections: lighter, flatter than the parent card */
      details details {
        box-shadow: none;
        border-color: #ece5f6;
        margin: 12px 0;
      }
      details details > summary { background: #fcfbfe; padding: 9px 13px; }
      details details > summary:hover { background: #f5f1fb; }
      /* During boot: keep title/logo visible; hide only sidebar + main until data is ready */
      html.dash-boot-loading .sidebarPanel,
      html.dash-boot-loading .mainPanel {
        visibility: hidden !important;
      }
      html.dash-boot-loading #boot_loading_overlay,
      html.dash-boot-loading #dashboard_loading_overlay > .dashboard-loading-overlay {
        visibility: visible !important;
      }
      .dashboard-loading-overlay {
        position: fixed;
        /* Sit below the title banner (logo ~84px + padding/margins) */
        top: 118px;
        left: 0;
        right: 0;
        bottom: 0;
        background: rgba(240, 242, 245, 0.98);
        z-index: 12000; /* above sidebar, tabs, tables, plots; below .app-header */
        display: flex;
        align-items: center;
        justify-content: center;
        transition: opacity 0.18s ease-out;
      }
      .dashboard-loading-card {
        background: #ffffff;
        border: 1px solid #d9dce1;
        border-radius: 8px;
        padding: 20px 24px;
        min-width: 320px;
        text-align: center;
        box-shadow: 0 2px 14px rgba(0, 0, 0, 0.08);
      }
      .dashboard-loading-spinner {
        width: 30px;
        height: 30px;
        border: 3px solid #d7d0e5;
        border-top-color: #5c2f92;
        border-radius: 50%;
        margin: 0 auto 12px auto;
        animation: spin-loader 0.9s linear infinite;
      }
      @keyframes spin-loader {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }
      /* Wordcloud story tiers: match the rounded-card aesthetic used elsewhere */
      /* Tier 1 (Pre / Post) = outer card like .sat-subsection */
      details.wc-tier1 {
        background: #ffffff;
        border: 1px solid #e7ddf3;
        border-left: 4px solid #5c2f92;
        border-radius: 10px;
        padding: 10px 16px;
        margin: 12px 0;
        box-shadow: 0 1px 3px rgba(92, 47, 146, 0.06);
        overflow: visible;
      }
      details.wc-tier1 > summary { outline: none; cursor: pointer; font-weight: 600; }
      details.wc-tier1 > summary::marker { color: #5c2f92; }
      /* Tier 3 (individual questions) = nested lighter card */
      details.wc-tier3 {
        background: #faf8fd;
        border: 1px solid #e7ddf3;
        border-left: 3px solid #b9a7d4;
        border-radius: 8px;
        padding: 8px 14px;
        margin: 10px 0 10px 1.1rem;
        overflow: visible;
      }
      details.wc-tier3 > summary { outline: none; cursor: pointer; }
      details.wc-tier3 > summary::marker { color: #5c2f92; }
      /* Display-options card for chart toggles (N / % / shared axes) */
      .display-options-card {
        background: #f5f1fb;
        border: 1px solid #d9cdee;
        border-left: 4px solid #5c2f92;
        border-radius: 8px;
        padding: 10px 14px;
        margin: 4px 0 14px 0;
      }
      .display-options-label {
        display: block;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        color: #5c2f92;
        margin-bottom: 8px;
      }
      .display-options-chips {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        align-items: stretch;
      }
      .display-option-chip {
        background: #ffffff;
        border: 1px solid #d9cdee;
        border-radius: 6px;
        padding: 6px 12px;
        transition: background 0.15s, border-color 0.15s, box-shadow 0.15s;
      }
      .display-option-chip:hover {
        border-color: #5c2f92;
        box-shadow: 0 1px 4px rgba(92, 47, 146, 0.12);
      }
      /* Tighten the default Shiny checkbox spacing inside chips */
      .display-option-chip .checkbox,
      .display-option-chip .form-group { margin: 0; }
      .display-option-chip .checkbox label { font-size: 13px; color: #3d3350; margin: 0; }

      /* Inline variant: equal-width chips on a single row that fill the card */
      .display-options-card--inline .display-options-chips { flex-wrap: nowrap; }
      .display-options-card--inline .display-option-chip {
        flex: 1 1 0;
        min-width: 0;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      .display-options-card--inline .display-option-chip .shiny-input-container { width: 100%; min-width: 0; }
      .display-options-card--inline .display-option-chip .control-label {
        font-size: 11px;
        font-weight: 600;
        color: #5c2f92;
        margin: 0 0 3px 0;
      }
      .display-options-card--inline .display-option-chip input[type='number'] {
        height: 30px;
        padding: 2px 8px;
        font-size: 13px;
      }

      /* Variable Mapping coverage tables: compact, rounded card with soft separators */
      .dataTables_wrapper:has(table.vm-coverage) {
        width: auto;
        display: inline-block;
        max-width: 100%;
        border: 1px solid #e3dcef;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 1px 4px rgba(92, 47, 146, 0.05);
      }
      table.dataTable.vm-coverage {
        width: auto !important;
        margin: 0 !important;
        border: none !important;
        border-collapse: collapse;
        font-size: 11px;
        line-height: 1.25;
      }
      table.dataTable.vm-coverage thead th {
        background: #5c2f92;
        color: #ffffff;
        font-weight: 600;
        border: none !important;
        padding: 5px 8px;
        white-space: nowrap;
        font-size: 11px;
      }
      table.dataTable.vm-coverage thead th:not(:first-child) { text-align: center; }
      table.dataTable.vm-coverage tbody td {
        border-top: 1px solid #efeaf6 !important;
        border-bottom: none !important;
        padding: 4px 8px;
        vertical-align: middle;
      }
      table.dataTable.vm-coverage tbody tr:first-child td { border-top: none !important; }
      table.dataTable.vm-coverage tbody td:first-child { font-weight: 500; color: #3d3350; background: #ffffff; }
      /* Remove DT's default zebra/hover so the Yes/No color coding stays clean */
      table.dataTable.vm-coverage.row-border tbody tr { background-color: transparent; }
      .dataTables_wrapper:has(table.vm-coverage) .dataTables_scrollBody { border: none; }

      /* Compact Overview pair-source / similar DT tables */
      table.dataTable.pair-source-compact {
        font-size: 11px !important;
        line-height: 1.25;
      }
      table.dataTable.pair-source-compact thead th,
      table.dataTable.pair-source-compact tbody td {
        padding: 4px 8px !important;
      }
      .dataTables_wrapper:has(table.pair-source-compact) .dataTables_filter input {
        font-size: 11px;
        height: 26px;
        padding: 2px 6px;
      }
      .dataTables_wrapper:has(table.pair-source-compact) .dataTables_info,
      .dataTables_wrapper:has(table.pair-source-compact) .dataTables_paginate {
        font-size: 11px;
      }

      /* Bound certain plots to about half the main panel width (prevents over-wide charts) */
      .plot-half { width: 52%; min-width: 440px; max-width: 720px; margin-bottom: 10px; }
      .plot-half .plotly, .plot-half .js-plotly-plot { width: 100% !important; }
      /* Slightly wider bound for horizontal bar charts with long category labels */
      .plot-wide { width: 70%; min-width: 520px; max-width: 820px; margin-bottom: 10px; }
      .plot-wide .plotly, .plot-wide .js-plotly-plot { width: 100% !important; }

      /* Collapsible analysis subsections (Satisfaction by module / by facilitator) */
      details.sat-subsection {
        background: #ffffff;
        border: 1px solid #e7ddf3;
        border-left: 4px solid #5c2f92;
        border-radius: 10px;
        padding: 10px 16px;
        margin: 12px 0;
        box-shadow: 0 1px 3px rgba(92, 47, 146, 0.06);
      }
      details.sat-subsection > summary { outline: none; cursor: pointer; }
      details.sat-subsection > summary::marker { color: #5c2f92; }

      /* Shared deterministic summary box (Satisfaction / Wellness / Behavioral) */
      .summary-box {
        background: #faf8fd;
        border: 1px solid #e7ddf3;
        border-left: 4px solid #5c2f92;
        border-radius: 8px;
        padding: 12px 16px;
        margin: 12px 0;
      }
      .summary-box-headline { margin: 0 0 6px 0; font-size: 13px; color: #2d2640; }
      .summary-box-detail { margin: 0; font-size: 12px; color: #4a4458; line-height: 1.5; }
      .summary-box-scope { margin: 8px 0 0 0; font-size: 11px; color: #8a7fa6; font-style: italic; }

      /* Wordcloud words-per-cloud control */
      .wc-controls {
        background: #faf8fd;
        border: 1px solid #e7ddf3;
        border-radius: 8px;
        padding: 8px 16px 4px 16px;
        margin: 8px 0 14px 0;
      }
      .wc-controls .control-label { color: #5c2f92; font-weight: 600; font-size: 12px; }

      /* Wordcloud show-original-text tables: rounded card + readable wrapped text */
      table.dataTable.responses-table tbody td { white-space: normal; line-height: 1.45; font-size: 12px; }
      table.dataTable.responses-table tbody td:last-child { text-align: left; color: #3d3350; }
      .dataTables_wrapper:has(table.responses-table) .dataTables_filter input,
      .dataTables_wrapper:has(table.responses-table) thead input { border-radius: 6px; border: 1px solid #d9cdee; }

      /* Sidebar sections styled as cards to match the display-options aesthetic */
      #dashboard_sidebar.sidebar { background: transparent; border: none; box-shadow: none; padding: 4px 2px 12px 2px; }
      .sidebar-card {
        background: #f5f1fb;
        border: 1px solid #d9cdee;
        border-left: 4px solid #5c2f92;
        border-radius: 8px;
        padding: 12px 14px;
        margin: 0 0 14px 0;
      }
      .sidebar-card-label {
        display: block;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        color: #5c2f92;
        margin-bottom: 10px;
      }
      .sidebar-card .form-group { margin-bottom: 12px; }
      .sidebar-card .form-group:last-child { margin-bottom: 0; }
      .sidebar-card h5 { color: #5c2f92; font-size: 12px; font-weight: 700; letter-spacing: 0.02em;
        text-transform: uppercase; margin: 10px 0 6px 0; }
      .sidebar-card .help-block { margin-top: 4px; }
      .sidebar-card .radio label, .sidebar-card .checkbox label { color: #3d3350; }
      /* Display-options chips stack full-width inside the narrow sidebar */
      #dashboard_sidebar .display-options-chips { flex-direction: column; gap: 8px; }
      #dashboard_sidebar .display-option-chip { width: 100%; }
    "))
  ),
  
  # NOTE: Custom client-side JS temporarily disabled while debugging Shiny output binding.
  tags$head(
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('details').forEach(function(el) {
          el.removeAttribute('open');
        });
      });
      document.addEventListener('shiny:connected', function() {
        document.querySelectorAll('details').forEach(function(el) {
          el.removeAttribute('open');
        });
      });
      if (window.Shiny) {
        Shiny.addCustomMessageHandler('dashBootDone', function() {
          document.documentElement.classList.remove('dash-boot-loading');
        });
      } else {
        document.addEventListener('shiny:connected', function() {
          Shiny.addCustomMessageHandler('dashBootDone', function() {
            document.documentElement.classList.remove('dash-boot-loading');
          });
        }, { once: true });
      }
    "))
  ),
  
  tags$head(tags$title("5 Buckets Impact Dashboard")),
  div(class = "app-header",
    tags$img(src = "5buckets_logo.png", class = "app-header-logo", alt = "5 Buckets Foundation"),
    div(class = "app-header-divider"),
    div(class = "app-header-text",
      div(class = "app-header-title", "Impact Dashboard"),
      div(class = "app-header-subtitle", "Interactive Analyses of Survey Data")
    )
  ),
  # Covers sidebar + main only (CSS top offset + header z-index keep logo/title visible).
  div(
    id = "boot_loading_overlay",
    class = "dashboard-loading-overlay",
    div(
      class = "dashboard-loading-card",
      div(class = "dashboard-loading-spinner"),
      h4("Loading data dashboard...", style = "color: #5c2f92; margin: 0 0 8px 0;"),
      p("Pulling data from the Master Workbook. Please wait.",
        style = "color: #5f6369; margin: 0; font-size: 13px;")
    )
  ),
  uiOutput("dashboard_loading_overlay"),
  
  sidebarLayout(
    
    # Sidebar - Filters
    sidebarPanel(
      id = "dashboard_sidebar",
      width = 3,
      class = "sidebar",
      # Display options (global; applies to every tab's charts)
      display_options_card(
        checkboxInput("opt_same_y", "Paired Y axes", value = TRUE),
        checkboxInput("opt_show_n", "Show N on bars", value = FALSE),
        checkboxInput("opt_show_pct", "Show % on bars", value = TRUE)
      ),

      # 3) Filters
      sidebar_card("Filters",
        checkboxInput("use_date_filter", "Filter by date range", value = FALSE),
        conditionalPanel(
          condition = "input.use_date_filter == true",
          dateRangeInput(
            "date_range",
            label = "Date range",
            start = Sys.Date() - 1095,
            end = Sys.Date()
          )
        ),
        h5("Organization"),
        selectizeInput(
          "selected_org",
          label = NULL,
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "All organizations",
            plugins = list("remove_button")
          )
        ),
        helpText(style = "font-size: 11px; color: #6a6f75;", "Leave empty for all. Select one or more orgs."),
        div(class = "filter-label-row", h5("Group", style = "margin: 0;")),
        selectizeInput(
          "selected_group",
          label = NULL,
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select organizations first",
            plugins = list("remove_button")
          )
        ),
        helpText(style = "font-size: 11px; color: #6a6f75;", "Grouped under each selected organization."),
        h5("Survey language"),
        selectInput(
          "filter_language",
          label = NULL,
          choices = c("All languages" = "All"),
          selected = "All"
        ),
        helpText(style = "font-size: 11px; color: #6a6f75;", "Blank language on Pre = English. Post rows may be tagged es/zh.")
      ),

      # 4) Demographics
      sidebar_card("Filter by demographics",
        checkboxGroupInput("filter_gender", label = "Gender", choices = c(), selected = NULL),
        checkboxGroupInput("filter_veteran", label = "Veteran Status", choices = c("Yes" = "Yes", "No" = "No"), selected = NULL),
        checkboxGroupInput("filter_income", label = "Household Income", choices = c(), selected = NULL),
        checkboxGroupInput("filter_education", label = "Education Level", choices = c(), selected = NULL)
      ),

      # 5) Modules (choices filled from Program Manager Workshops sheet when data loads)
      sidebar_card("Modules taught",
        checkboxGroupInput("selected_modules", label = NULL, choices = character(0), selected = character(0)),
        helpText(style = "font-size: 11px; color: #6a6f75;", "Options come from Program Manager (Workshops). Select one or more; a session is kept if it teaches any selected module.")
      ),

      # Export (PDF / AI Markdown / data mapping / filtered CSV ZIP)
      export_sidebar_ui(),
      br(),
      actionButton("refresh_data", "Refresh Data",
                   class = "btn-primary",
                   style = "width: 100%;")
    ),
    
    # Main Panel
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "impact_tabs",
        type = "pills",
          tabPanel("Overview", value = "overview",
            h3("Impact Overview"),
            p("High-level reach, satisfaction, and participation highlights."),
            br(),
            tags$details(
              open = TRUE,
              tags$summary(h4("Summary Stats", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = TRUE,
                style = "margin-left: 18px;",
                tags$summary(h5("Response Descriptives", style = "color: #5c2f92; cursor: pointer;")),
                h6("By survey type", style = "color: #5c2f92; margin-top: 8px; margin-bottom: 6px;"),
                div(style = "width: fit-content; max-width: 980px;", tableOutput("impact_overview_survey_type_stats_table")),
                p(style = "font-size: 12px; color: #5f6369; margin-top: 8px; max-width: 980px;",
                  tags$em("Note."), " Big = first/last session in a series (or single-session workshops). ",
                  "Little = mid-series sessions. ",
                  tags$strong("Session Total"), " responses = Big Pre + Little Pre + Little Post + Big Post. ",
                  tags$strong("Total"), " responses = Session Total + Annual Survey ",
                  "(Sessions and averages are blank in Total because Annual is not a workshop session). ",
                  "Session counts are distinct workshops (", tags$code("base_session_id"),
                  ") and do not sum across Big/Little columns; Annual has no ", tags$code("session_id"), ".")
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Respondent IDs", style = "color: #5c2f92; cursor: pointer;")),
                htmlOutput("respondent_pairing_banner_overview"),
                uiOutput("respondent_pair_sources_blurb"),
                tags$details(
                  open = TRUE,
                  style = "margin-left: 12px;",
                  tags$summary(h6("Pair sources by organization", style = "color: #5c2f92; cursor: pointer;")),
                  DT::dataTableOutput("respondent_pair_sources_by_org")
                ),
                tags$details(
                  open = TRUE,
                  style = "margin-left: 12px;",
                  tags$summary(h6("Pair sources by organization + group", style = "color: #5c2f92; cursor: pointer;")),
                  DT::dataTableOutput("respondent_pair_sources_by_group")
                ),
                tags$details(
                  open = FALSE,
                  style = "margin-left: 12px;",
                  tags$summary(h6("Sessions that contain paired IDs", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 11px; color: #5f6369;",
                    "Each row is a Big Pre or Big Post session that includes at least one of the paired IDs. ",
                    "Compare Responses / Distinct IDs to Paired IDs in session."),
                  DT::dataTableOutput("respondent_pair_sources_by_session")
                )
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Survey submissions over time", style = "color: #5c2f92; cursor: pointer;")),
                p(
                  style = "font-size: 12px; color: #5f6369; margin-bottom: 8px;",
                  "Pre and Post counts (left axis) use ",
                  tags$strong("current sidebar filters"),
                  " and include ", tags$strong("all"), " Pre / Post responses (Big + Little), not Big-only. Workshops (right axis) = distinct ",
                  tags$code("session_id"),
                  " in that time bin. Choose day, week, or month bins."
                ),
              div(style = "padding-left: 12px;",
              fluidRow(
                column(4, selectInput(
                  "overview_master_time_unit",
                  "Time axis",
                  choices = c("Days" = "days", "Weeks (Mon start)" = "weeks", "Months" = "months"),
                  selected = "weeks"
                )),
                column(4, checkboxInput("overview_submissions_show_responses", "Responses (all Pre + all Post)", value = TRUE)),
                column(4, checkboxInput("overview_submissions_show_workshops", "Workshops", value = TRUE))
              )),
                uiOutput("overview_master_weekly_filter_note"),
                plotlyOutput("overview_master_weekly_line", height = "420px")
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Workshop series timeline", style = "color: #5c2f92; cursor: pointer;")),
                p(
                  style = "font-size: 12px; color: #5f6369; margin-bottom: 8px;",
                  "Tiles show when at least one workshop is scheduled in that time bin. Default: one row per group within an organization. Check the box below to collapse to one row per organization. Data: ",
                  tags$a("Program Manager - Workshops tab", href = PROGRAM_MANAGER_URL, target = "_blank"),
                  ". Date window follows the sidebar ", tags$strong("Filter by date range"),
                  " when enabled; otherwise the full PM schedule is shown. Org/group sidebar filters are not applied here."
                ),
              div(style = "padding-left: 12px;",
              fluidRow(
                column(6, selectInput(
                  "overview_pm_time_unit",
                  "Time axis",
                  choices = c("Days" = "days", "Weeks (Mon start)" = "weeks", "Months" = "months"),
                  selected = "weeks"
                )),
                column(6, selectInput(
                  "overview_pm_sort",
                  "Order rows by",
                  choices = c(
                    "First workshop ascending" = "first_asc",
                    "First workshop descending" = "first_desc",
                    "Last workshop ascending" = "last_asc",
                    "Last workshop descending" = "last_desc",
                    "Alphabetical (org) then chronological" = "alpha_chrono"
                  ),
                  selected = "first_asc"
                ))
              ),
              checkboxInput(
                "overview_pm_rows_by_org",
                "Group rows by organization (collapse groups within org)",
                value = FALSE
              )),
                p(style = "font-size: 11px; color: #5f6369; padding-left: 12px;", "Zoom the Gantt horizontally; pan in the chart to scroll through time."),
                uiOutput("overview_program_manager_gantt_ui")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Series Length by Org", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                "Number of series per organization by planned length from Program Manager (Len1 = single workshop, Len2 = two workshops, etc.). Master sessions are joined to PM by session_id; multiple series under the same org + group are split using PM session_number / sessions_in_series. If PM has no row for a session, that session is counted as its own series."),
              uiOutput("impact_sessions_in_progress_section"),
              h5("Series by Org - Completed", style = "color: #5c2f92; margin-top: 16px; margin-bottom: 6px;"),
              p(style = "color: #5f6369; font-size: 11px; margin-bottom: 8px;", "Workshop series that have Big Post data. Click a row to inspect the sessions behind it."),
              DT::dataTableOutput("impact_org_table_completed"),
              uiOutput("impact_org_detail"),
              DT::dataTableOutput("impact_org_detail_table")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Responses by Series", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                "Response counts per workshop series. Each row = one series (org + group). Columns = Pre/Post per session. Paired Big Pre-Post = same users in first pre and last post."),
              uiOutput("overview_journey_response_tables")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Learners by zip code (map)", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                "Point size = learner count (larger when zoomed out). Nearby zips are combined into grid cells at low zoom; zoom in for zip-level detail. Requires zipcode package for geocoding."),
              fluidRow(
                column(6, plotlyOutput("impact_overview_zip_map_big_pre", height = "420px")),
                column(6, plotlyOutput("impact_overview_zip_map_big_post", height = "420px"))
              )
            ),
            br(),
            p(style = "font-size: 11px; color: #5f6369;",
              "Use filters in the sidebar to refine all Impact views.")
          ),
          tabPanel("Data quality", value = "data_quality",
            h3("Master sheet read health"),
            p(
              style = "font-size: 12px; color: #5f6369; margin-bottom: 12px;",
              "Compare Feb 2026 handler rows (with ", tags$code("req"), " set) to legacy or ported rows. ",
              "Timestamp parse rates and per-column fill rates surface migrations that are not reading cleanly."
            ),
            htmlOutput("dq_summary"),
            br(),
            htmlOutput("respondent_pairing_banner_dq"),
            h4("Timestamp parse rate by cohort", style = "color: #5c2f92;"),
            fluidRow(
              column(6, h5("Pre"), DT::dataTableOutput("dq_cohort_pre")),
              column(6, h5("Post"), DT::dataTableOutput("dq_cohort_post"))
            ),
            h4("Still unparseable? (distinct raw values)", style = "color: #5c2f92;"),
            p(
              style = "font-size: 11px; color: #5f6369;",
              "After expanded parsing (12h AM/PM, UTC Z, Excel serial strings, ISO offsets, month names). ",
              tags$code("pattern"), " is a fingerprint (digits→0, letters→A) to group similar shapes."
            ),
            fluidRow(
              column(6, DT::dataTableOutput("dq_ts_fail_pre")),
              column(6, DT::dataTableOutput("dq_ts_fail_post"))
            ),
            h4("Variable fill rate by cohort (mapped questions)", style = "color: #5c2f92;"),
            p(
              style = "font-size: 11px; color: #5f6369;",
              "Columns from ", tags$code("question_mapping.R"), " that exist on the sheet. ",
              "Large gaps between cohorts often indicate legacy column names or formats."
            ),
            fluidRow(
              column(6, DT::dataTableOutput("dq_fill_pre")),
              column(6, DT::dataTableOutput("dq_fill_post"))
            ),
            h4("Expected metadata columns", style = "color: #5c2f92;"),
            fluidRow(
              column(6, DT::dataTableOutput("dq_expected_pre")),
              column(6, DT::dataTableOutput("dq_expected_post"))
            ),
            h4("Raw column coverage (all columns)", style = "color: #5c2f92;"),
            fluidRow(
              column(6, DT::dataTableOutput("dq_cols_pre")),
              column(6, DT::dataTableOutput("dq_cols_post"))
            )
          ),
          tabPanel("Reach", value = "reach2",
            h3("Reach & Demographics"),
            p(style = "color: #5f6369; font-size: 12px; margin-bottom: 8px;",
              "Big Pre (left) vs Big Post (middle). Optionally include Annual Survey (right). Same sidebar filters apply where columns exist. ",
              "Click a demographic bar to list respondents for that category and wave."),
            checkboxInput(
              "include_annual_in_reach",
              "Include Annual Survey in demographic charts",
              value = FALSE
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Variable Mapping", style = "color: #5c2f92; cursor: pointer;")),
              DT::dataTableOutput("reach2_variable_mapping"),
              p(style = "font-size: 11px; color: #5f6369; margin-top: 8px;",
                "Big Pre, Big Post, and Annual include the full demographic block. Little Pre/Post include Zip Code only.")
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Demographic Distributions", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 15px;",
                "X-axes share levels for direct comparison. Use the checkbox above to add an Annual column."),
              p(style = "color: #5f6369; font-size: 11px; margin: 4px 0 12px 0;",
                "Percent = share of respondents in that chart after filters (each bar\u2019s count / sum of counts on the plot)."),
              h5("Age", style = "color: #5c2f92; margin-top: 12px;"),
              uiOutput("reach2_age_row"),
              h5("Gender", style = "color: #5c2f92; margin-top: 12px;"),
              checkboxInput(
                "reach2_gender_display_others",
                "Display other responses separately (default: bucket write-ins as Other)",
                value = FALSE
              ),
              uiOutput("reach2_gender_row"),
              h5("Household Income", style = "color: #5c2f92; margin-top: 12px;"),
              uiOutput("reach2_income_row"),
              h5("Education", style = "color: #5c2f92; margin-top: 12px;"),
              uiOutput("reach2_education_row"),
              h5("Race/Ethnicity", style = "color: #5c2f92; margin-top: 12px;"),
              uiOutput("reach2_race_row")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Cross-tabs (Demographic × Demographic)", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                "Two-way demographic heatmaps from Big Pre. Hover for counts."),
              fluidRow(
                column(6, plotlyOutput("reach2_ct_age_income", height = "340px")),
                column(6, plotlyOutput("reach2_ct_education_income", height = "340px"))
              ),
              fluidRow(
                column(6, plotlyOutput("reach2_ct_age_race", height = "340px")),
                column(6, plotlyOutput("reach2_ct_gender_income", height = "340px"))
              ),
              fluidRow(
                column(6, plotlyOutput("reach2_ct_gender_race", height = "340px")),
                column(6, plotlyOutput("reach2_ct_gender_age", height = "340px"))
              )
            ),
            div(style = "height: 48px;")
          ),
          tabPanel("Financial Wellness", value = "wellness",
            h3("Financial Wellness"),
            div(class = "alert alert-light", style = "border: 1px solid #dee2e6; padding: 12px 16px; margin-bottom: 20px; font-size: 12px;",
              tags$strong("Data sources:"),
              tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                tags$li(tags$strong("Big Pre (8 Likert):"), " Awareness of money/affordability/where it goes; optimism; relationship with money; stress management; planning confidence; comfort with professionals"),
                tags$li(tags$strong("Big Post (9 Likert):"), " Same concepts as 'Compared to before...' (understanding, awareness, optimism, relationship, stress, confidence, comfort)")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Variable Mapping", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin-bottom: 10px;",
                "Items use a 4-point agreement scale (no neutral), scored −3 (Strongly Disagree), −1 (Disagree), +1 (Agree), +3 (Strongly Agree). ",
                "The Financial Wellness Index (Pre) is the mean of the 8 Big Pre items; the Post Impact Index is the mean of the 9 Big Post \"Compared to before...\" items. ",
                "0 = neutral; higher = more positive financial wellness (Pre) or greater perceived improvement (Post)."),
              DT::dataTableOutput("impact_wellness_variable_mapping"),
              p(style = "font-size: 11px; color: #5f6369; margin-top: 8px;",
                "Big Pre: 8 items. Little Pre: 0. Little Post: 0. Big Post: 9 items.")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Pre", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Summary Score \u2014 Financial Wellness Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Financial Wellness Index = mean of the 8 Big Pre items. ", LIKERT_INDEX_SCALE_BLURB,
                  " The histogram bins the continuous index (adjust Bins) and the smoothed curve shows the underlying shape."),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("wellness_index_pre_bars", "Bars", value = TRUE),
                    checkboxInput("wellness_index_pre_curves", "Curves", value = TRUE),
                    numericInput("wellness_index_pre_bins", "Bins", value = 24, min = 4, max = 60, step = 1),
                    inline = TRUE
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_wellness_hist", height = "300px")),
                  uiOutput("impact_wellness_hist_summary")
                )
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Each Financial Wellness Metric", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Individual item distributions (8 items, 4-point agreement scale, no neutral)."),
                fluidRow(
                  column(4, plotlyOutput("impact_pre_know_amount_hist", height = "260px")),
                  column(4, plotlyOutput("impact_pre_know_afford_hist", height = "260px")),
                  column(4, plotlyOutput("impact_pre_know_where_hist", height = "260px"))
                ),
                fluidRow(
                  column(4, plotlyOutput("impact_pre_optimism_hist", height = "260px")),
                  column(4, plotlyOutput("impact_pre_relationship_hist", height = "260px")),
                  column(4, plotlyOutput("impact_pre_stress_hist", height = "260px"))
                ),
                fluidRow(
                  column(6, plotlyOutput("impact_pre_confidence_hist", height = "260px")),
                  column(6, plotlyOutput("impact_pre_comfort_hist", height = "260px"))
                )
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Post", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Summary Score \u2014 Post Impact Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Post Impact Index = mean of the 9 Big Post Compared-to-before items (Understanding; Awareness of amount/afford/where; Optimism; Relationship; Stress; Confidence; Comfort with professionals). ",
                  LIKERT_INDEX_SCALE_BLURB,
                  " The histogram bins the continuous index (adjust Bins) and the smoothed curve shows the underlying shape."),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("wellness_index_post_bars", "Bars", value = TRUE),
                    checkboxInput("wellness_index_post_curves", "Curves", value = TRUE),
                    numericInput("wellness_index_post_bins", "Bins", value = 24, min = 4, max = 60, step = 1),
                    inline = TRUE
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_post_hist", height = "300px")),
                  uiOutput("impact_post_hist_summary")
                )
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Each Financial Wellness Metric", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Individual item distributions (9 'Compared to before...' items, 4-point agreement scale, no neutral)."),
                fluidRow(
                  column(4, plotlyOutput("impact_post_understanding_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_awareness_amount_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_awareness_afford_hist", height = "260px"))
                ),
                fluidRow(
                  column(4, plotlyOutput("impact_post_awareness_where_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_optimism_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_relationship_hist", height = "260px"))
                ),
                fluidRow(
                  column(4, plotlyOutput("impact_post_stress_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_confidence_hist", height = "260px")),
                  column(4, plotlyOutput("impact_post_comfort_hist", height = "260px"))
                )
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Pre vs Post", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Between-subjects", style = "color: #5c2f92; cursor: pointer; display: inline;")),
                div(class = "fav-header-row", style = "margin-top: 6px;",
                  fav_star_button("wellness_between")
                ),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("wellness_show_bars", "Bars", value = TRUE),
                    checkboxInput("wellness_show_curves", "Curves", value = TRUE),
                    numericInput("wellness_between_bins", "Bins", value = 24, min = 4, max = 60, step = 1),
                    inline = TRUE
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_wellness_between_dist", height = "300px"))
                ),
                p(style = "font-size: 12px; color: #5f6369; margin: 12px 0; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                  "Between-subjects takes the average of ", tags$strong("all"), " Pre responses and compares it to the average of ",
                  tags$strong("all"), " Post responses \u2014 regardless of which individuals answered each survey. An independent-samples t-test treats the two as separate groups."),
                htmlOutput("impact_wellness_between_summary"),
                br(),
                h6("By item", style = "color: #5c2f92; margin-top: 18px;"),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                  "Independent t-test for each of the 8 items (Pre group vs Post group), sorted by largest positive effect to largest negative."),
                plotlyOutput("impact_wellness_item_between_plot", height = "380px"),
                DT::dataTableOutput("impact_wellness_item_between_table")
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Within-subjects", style = "color: #5c2f92; cursor: pointer; display: inline;")),
                div(class = "fav-header-row", style = "margin-top: 6px;",
                  fav_star_button("wellness_within")
                ),
                fluidRow(
                  column(6,
                    h6("Within-subject change (Pre \u2192 Post)", style = "color: #5c2f92; margin-top: 0;"),
                    display_options_card(
                      checkboxInput("wellness_within_show_individual", "Individual slopes", value = TRUE),
                      checkboxInput("wellness_within_show_avg", "Average slope", value = TRUE),
                      inline = TRUE
                    ),
                    div(style = "min-height: 300px;",
                      plotlyOutput("impact_wellness_within_slopes", height = "300px"))
                  ),
                  column(6,
                    h6("Change Score Distribution", style = "color: #5c2f92; margin-top: 0;"),
                    display_options_card(
                      checkboxInput("wellness_change_show_bars", "Bars", value = TRUE),
                      checkboxInput("wellness_change_show_curves", "Curves", value = TRUE),
                      inline = TRUE
                    ),
                    div(style = "min-height: 300px;",
                      plotlyOutput("impact_wellness_paired_change", height = "300px"))
                  )
                ),
                p(style = "font-size: 12px; color: #5f6369; margin: 12px 0; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                  "Within-subjects includes ", tags$strong("only"), " learners for whom we have both a Big Pre and a Big Post score, so we can measure each person\u2019s individual change. The summary then averages those individual change scores (paired t-test). The change-score distribution shows individual change (Post \u2212 Pre) for those same paired respondents."),
                htmlOutput("impact_wellness_within_summary"),
                br(),
                h6("By item", style = "color: #5c2f92; margin-top: 18px;"),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                  "Paired t-test for each of the 8 financial wellness items (Pre \u2192 Post), sorted by largest positive effect to largest negative."),
                plotlyOutput("impact_wellness_item_within_plot", height = "380px"),
                DT::dataTableOutput("impact_wellness_item_within_table")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Annual Survey", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin: 8px 0 12px 0; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                "Follow-up ", tags$em("Compared to before your 5 Buckets experience..."),
                " financial wellness items from the Annual Survey (same structure as Post). Pre and Post above stay workshop-only."),
              tags$details(
                open = TRUE,
                style = "margin-left: 12px;",
                tags$summary(h5("Summary Score \u2014 Annual Financial Wellness Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                  "Annual Financial Wellness Index = mean of Annual Compared-to-before items (same structure as Post). ",
                  LIKERT_INDEX_SCALE_BLURB,
                  " The histogram bins the continuous index (adjust Bins) and the smoothed curve shows the underlying shape."),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("wellness_index_annual_bars", "Bars", value = TRUE),
                    checkboxInput("wellness_index_annual_curves", "Curves", value = TRUE),
                    numericInput("wellness_index_annual_bins", "Bins", value = 24, min = 4, max = 60, step = 1),
                    inline = TRUE
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_tab_annual_wellness_index_hist", height = "300px")),
                  uiOutput("impact_tab_annual_wellness_index_summary")
                )
              ),
              tags$details(
                open = TRUE,
                style = "margin-left: 12px;",
                tags$summary(h5("Each Financial Wellness Metric", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 8px;",
                  "Per-item Likert bars (sidebar Show N / Show %). Stacked overview below."),
                uiOutput("impact_tab_annual_wellness_item_hists"),
                br(),
                h6("Stacked overview", style = "color: #5c2f92; margin-top: 8px;"),
                plotlyOutput("impact_tab_annual_wellness_likert_plot", height = "480px")
              )
            )
          ),
          tabPanel("Behavioral Readiness", value = "behavioral",
            h3("Behavioral Readiness"),
            div(class = "alert alert-light", style = "border: 1px solid #dee2e6; padding: 12px 16px; margin-bottom: 20px; font-size: 12px;",
              tags$strong("Data sources:"),
              tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                tags$li(tags$strong("Big Pre:"), " Past behaviors (comma-separated multi-select)"),
                tags$li(tags$strong("Big Post:"), " Planned actions (comma-separated multi-select)")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Variable Mapping", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin-bottom: 10px;",
                "Behavioral Index (Pre) = count of past behaviors checked. Planned Actions Index (Post) = count of planned actions checked."),
              DT::dataTableOutput("impact_behavioral_variable_mapping"),
              p(style = "font-size: 11px; color: #5f6369; margin-top: 8px;",
                "Big Pre: 1 question (Past behaviors). Big Post: 1 question (Planned actions). Same option set.")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Pre", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Summary Score \u2014 Behavioral Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Behavioral Index = number of \"Yes\" behaviors per respondent on the 8-item list (scale 0\u20138); higher = more pre-existing positive behaviors."),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("behavioral_index_pre_bars", "Bars", value = TRUE),
                    checkboxInput("behavioral_index_pre_curves", "Curves", value = TRUE)
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_behavioral_pre_hist", height = "300px")),
                  uiOutput("impact_behavioral_pre_hist_summary")
                )
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Each Past Behavior", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Past behaviors before the workshop (share endorsing each behavior)."),
                div(class = "plot-wide", plotlyOutput("big_pre_behaviors_heatmap", height = "350px"))
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Post", style = "color: #5c2f92; cursor: pointer;")),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Summary Score \u2014 Planned Actions Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Planned Actions Index = count of planned actions checked per respondent (scale 0\u20138); higher = more actions planned as a result of the workshop."),
                div(class = "plot-half",
                  display_options_card(
                    checkboxInput("behavioral_index_post_bars", "Bars", value = TRUE),
                    checkboxInput("behavioral_index_post_curves", "Curves", value = TRUE)
                  ),
                  div(style = "min-height: 300px;",
                    plotlyOutput("impact_behavioral_post_hist", height = "300px")),
                  uiOutput("impact_behavioral_post_hist_summary")
                )
              ),
              tags$details(
                open = FALSE,
                style = "margin-left: 18px;",
                tags$summary(h5("Each Planned Action", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                  "Planned actions as a result of the workshop (share selecting each action)."),
                div(class = "plot-wide", plotlyOutput("big_post_planned_actions_heatmap", height = "350px"))
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Pre vs Post", style = "color: #5c2f92; cursor: pointer;")),
              div(
                style = "margin-top: 12px; padding-bottom: 56px; margin-bottom: 8px;",
                p(style = "font-size: 12px; color: #5f6369; margin-bottom: 12px; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                  "Between-subjects compares index averages (Pre vs Post). Each respondent’s index is a ",
                  tags$strong("within-person count"), " of endorsed behaviors (0–8); that count is then averaged across respondents for Pre and separately for Post. ",
                  "For within-person change, see the paired analysis."),
                fluidRow(
                  column(6,
                    h5("Between-subjects", style = "color: #5c2f92;"),
                    htmlOutput("impact_behavioral_between_summary"),
                    checkboxInput("behavioral_show_bars", "Bars", value = TRUE),
                    checkboxInput("behavioral_show_curves", "Curves", value = TRUE),
                    plotlyOutput("impact_behavioral_between_dist", height = "300px")
                  ),
                  column(6,
                    h5("Within-subjects", style = "color: #5c2f92;"),
                    p(style = "font-size: 11px; color: #5f6369;", "Each line: one respondent Pre → Post (toggle below)."),
                    htmlOutput("impact_behavioral_within_summary"),
                    fluidRow(
                      column(6, checkboxInput("behavioral_within_show_individual", "Individual slopes", value = TRUE)),
                      column(6, checkboxInput("behavioral_within_show_avg", "Average slope", value = TRUE))
                    ),
                    plotlyOutput("impact_behavioral_within_slopes", height = "300px")
                  )
                ),
                br(),
                div(class = "fav-header-row",
                  h5("Change Score Distribution", style = "color: #5c2f92; margin-top: 16px; margin-bottom: 0;"),
                  fav_star_button("behavioral_paired_change")
                ),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 8px;",
                  "Distribution of individual change (Post − Pre index) for paired respondents. Optional smoothed curve."),
                fluidRow(
                  column(6,
                    fluidRow(
                      column(6, checkboxInput("behavioral_change_show_bars", "Bars", value = TRUE)),
                      column(6, checkboxInput("behavioral_change_show_curves", "Curves", value = TRUE))
                    ),
                    plotlyOutput("impact_behavioral_paired_change", height = "300px")
                  )
                )
              ),
              br(),
              h5("Between-subjects by item", style = "color: #5c2f92; margin-top: 24px;"),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                "Difference in proportion (Post − Pre) for each behavior, sorted by largest positive to largest negative. ",
                tags$strong("pp"), " = percentage points (e.g. +5 pp means Post share is five points higher than Pre on a 0–100% scale)."),
              plotlyOutput("impact_behavioral_item_between_plot", height = "380px"),
              DT::dataTableOutput("impact_behavioral_item_between_table"),
              br(),
              h5("Within-subjects by item", style = "color: #5c2f92; margin-top: 24px;"),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                "Mean change (added − removed) for each behavior among paired respondents, sorted by effect."),
              plotlyOutput("impact_behavioral_item_within_plot", height = "380px"),
              DT::dataTableOutput("impact_behavioral_item_within_table")
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Annual Survey", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin: 8px 0 12px 0; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                "Annual follow-up: ", tags$em("As a result of my workshop(s)..."),
                " actions taken or planned. Pre and Post sections above are unchanged."),
              tags$details(
                open = TRUE,
                style = "margin-left: 12px;",
                tags$summary(h5("Summary Score — Annual Behavior Index", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                  "Count of selected actions per respondent on the Annual multi-select (higher = more actions taken/planned)."),
                plotlyOutput("impact_tab_annual_behaviors_index_hist", height = "300px"),
                htmlOutput("impact_tab_annual_behaviors_index_summary")
              ),
              tags$details(
                open = TRUE,
                style = "margin-left: 12px;",
                tags$summary(h5("Each action taken / planned", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "color: #5f6369; font-size: 12px; margin-bottom: 8px;",
                  "Share selecting each option. Sidebar Show N / Show % apply."),
                plotlyOutput("impact_tab_annual_behaviors_plot", height = "420px")
              )
            )
          ),
          tabPanel("Satisfaction", value = "satisfaction",
            h3("Satisfaction"),
            div(class = "alert alert-light", style = "border: 1px solid #dee2e6; padding: 12px 16px; margin-bottom: 20px; font-size: 12px;",
              tags$strong("Scales (Feb 2026 Big Post):"),
              tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                tags$li(tags$strong("This workshop session:"), " one rating per submission on how satisfied respondents were with ",
                  tags$em("that session"), " (numeric ", SESSION_SATISFACTION_MAX, "-point scale; charts use full range 1–", SESSION_SATISFACTION_MAX, ")."),
                tags$li(tags$strong("Overall 5 Buckets experience:"), " separate question — likelihood to recommend the organization (1–10).")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Variable Mapping", style = "color: #5c2f92; cursor: pointer;")),
              DT::dataTableOutput("impact_satisfaction_variable_mapping")
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Post", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 12px;",
                "Left: satisfaction with ", tags$strong("today's session"), ". Right: ", tags$strong("likelihood to recommend 5 Buckets"), " (overall experience)."),
              fluidRow(
                column(6,
                  h5("This workshop session (satisfaction)", style = "color: #5c2f92;"),
                  plotlyOutput("big_post_quality_index_hist", height = "320px"),
                  uiOutput("satisfaction_session_summary")),
                column(6,
                  h5("Overall 5 Buckets — Likelihood to recommend (1–10)", style = "color: #5c2f92;"),
                  plotlyOutput("big_post_recommendation_hist", height = "320px"),
                  uiOutput("satisfaction_recommend_summary"))
              )
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Across sessions", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 14px;",
                "Session satisfaction broken out by ", tags$code("modules_taught"), " and ", tags$code("facilitators"),
                " (Big Post session metadata). Each breakdown offers a ", tags$strong("Precise"), " and an ",
                tags$strong("Aggregated"), " view — defined per subsection below."),
              tags$details(
                open = TRUE, class = "sat-subsection",
                tags$summary(h5("By module type", style = "color: #5c2f92; margin: 4px 0; cursor: pointer; display: inline;")),
                tags$details(
                  open = TRUE,
                  tags$summary(h6("Precise — exact module combination as recorded", style = "color: #5c2f92; margin: 8px 0 4px 0; cursor: pointer; display: inline;")),
                  p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                    "One bar per exact module combination as recorded — a session taught as ",
                    tags$code("Grow|Protect"), " is its own category, distinct from ", tags$code("Grow"),
                    " alone. Mean with 95% CI, then a histogram per combination."),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_module_mean_refined", height = "400px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_module_dist_refined", height = "500px"))
                ),
                tags$details(
                  open = FALSE,
                  tags$summary(h6("Aggregated — atomic modules (duplicate counting)", style = "color: #5c2f92; margin: 12px 0 4px 0; cursor: pointer; display: inline;")),
                  p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                    "Each atomic module counted separately; a ", tags$code("Grow|Protect"),
                    " session contributes its rating to ", tags$strong("both"), " Grow and Protect (duplicate counting)."),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_module_mean_collapsed", height = "400px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_module_dist_collapsed", height = "500px"))
                )
              ),
              tags$details(
                open = TRUE, class = "sat-subsection",
                tags$summary(h5("By facilitator", style = "color: #5c2f92; margin: 16px 0 4px 0; cursor: pointer; display: inline;")),
                tags$details(
                  open = TRUE,
                  tags$summary(h6("Precise — exact facilitator lineup as recorded", style = "color: #5c2f92; margin: 8px 0 4px 0; cursor: pointer; display: inline;")),
                  p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                    "One bar per exact facilitator lineup as recorded — co-facilitated sessions (e.g. ",
                    tags$code("Alice & Bob"), ") stay together as their own category. Mean with 95% CI, then a histogram per lineup."),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_facilitator_mean_refined", height = "400px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_facilitator_dist_refined", height = "500px"))
                ),
                tags$details(
                  open = FALSE,
                  tags$summary(h6("Aggregated — individual facilitators (duplicate counting)", style = "color: #5c2f92; margin: 12px 0 4px 0; cursor: pointer; display: inline;")),
                  p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                    "Each facilitator counted individually (names split on comma, ", tags$code("|"), ", ",
                    tags$code("&"), ", or ", tags$code("/"), "); a co-taught session contributes its rating to every named facilitator (duplicate counting)."),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_facilitator_mean_collapsed", height = "400px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_by_facilitator_dist_collapsed", height = "500px"))
                ),
                tags$details(
                  open = FALSE,
                  tags$summary(h6("Facilitator reliability — how much to trust each comparison", style = "color: #5c2f92; margin: 16px 0 4px 0; cursor: pointer; display: inline;")),
                  p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                    "Small samples make raw means noisy. These views down-weight thin facilitators and show which differences are statistically meaningful. Uses the ",
                    tags$strong("aggregated"), " (individual-name) split; facilitators below the minimum n are pooled into ", tags$em("\u201Cother (low n)\u201D"), "."),
                  div(style = "margin: 6px 0 12px 0;",
                    sliderInput("satisfaction_facilitator_min_n", "Minimum responses per facilitator",
                      min = 1, max = 30, value = 5, step = 1, width = "320px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_facilitator_caterpillar", height = "440px")),
                  div(class = "plot-half", plotlyOutput("satisfaction_facilitator_funnel", height = "440px")),
                  div(style = "clear: both; margin-top: 12px;",
                    h6("Reliability-adjusted ranking", style = "color: #5c2f92; margin: 8px 0 4px 0;"),
                    p(style = "font-size: 11px; color: #5f6369; margin: 4px 0 8px 0;",
                      "Shrunken (empirical-Bayes) mean pulls each facilitator toward the overall average in proportion to how little data they have, so rankings are not dominated by lucky small samples."),
                    DT::dataTableOutput("satisfaction_facilitator_reliability_table"))
                )
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Annual Survey", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin: 8px 0 12px 0; background: #f8f4fc; padding: 10px; border-radius: 6px;",
                "Annual recommend (0–10) mirrors Big Post likelihood-to-recommend. ",
                tags$strong("Session satisfaction is Post-only"), " and is not asked on the Annual form."),
              h5("Likelihood to recommend 5 Buckets (Annual)", style = "color: #5c2f92;"),
              plotlyOutput("impact_tab_annual_recommend_hist", height = "320px"),
              uiOutput("impact_tab_annual_recommend_summary")
            )
          ),
          tabPanel("Learning & Impact Stories", value = "learning_stories",
            h3("Learning & Impact Stories"),
            div(class = "alert alert-light", style = "border: 1px solid #dee2e6; padding: 12px 16px; margin-bottom: 20px; font-size: 12px;",
              tags$strong("Data sources:"),
              tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                tags$li(tags$strong("Big Post only:"), " Multi-select “keep in touch” (what kinds of learning respondents want next)."),
                tags$li(tags$strong("Big Post:"), " “Better understanding” Likert (compared to before your 5 Buckets experience)."),
                tags$li(tags$strong("Big Pre:"), " Today’s intention, curiosities, hoped feelings, additional comments."),
                tags$li(tags$strong("Big Post:"), " Daily reflection (insight, application, helpful), additional comments, and overall program impact story."),
                tags$li(tags$strong("Annual Survey:"), " Takeaways, proud goals, impact stories, keep-in-touch / learn-more topics."),
                tags$li(tags$strong("Word clouds"), " use Pre/Post/Annual open text; low-information replies (e.g. “nope”, “n/a”) are filtered; short substantive answers are kept.")
              )
            ),
            tags$details(
              open = FALSE,
              tags$summary(h4("Variable Mapping", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin-bottom: 10px;",
                "Which of the five survey types feed each Learning & Impact Stories block."),
              DT::dataTableOutput("impact_learning_variable_mapping")
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Topic understanding", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "color: #5f6369; font-size: 12px; margin-bottom: 10px;",
                "Compared to before your 5 Buckets experience: I have a better understanding of the topics covered (4-point agreement scale, scored −3 to +3). ",
                "Session satisfaction (1–6) and NPS appear on the Satisfaction tab."),
              div(class = "plot-half",
                display_options_card(
                  checkboxInput("topic_understanding_bars", "Bars", value = TRUE),
                  checkboxInput("topic_understanding_curves", "Curve", value = TRUE),
                  inline = TRUE
                ),
                div(style = "min-height: 340px;",
                  plotlyOutput("big_post_impact_understanding_hist", height = "340px"))
              ),
              uiOutput("topic_understanding_summary_ui"),
              checkboxInput("topic_understanding_explore_sd", "Explore Strongly Disagree (session context)", value = FALSE),
              conditionalPanel(
                condition = "input.topic_understanding_explore_sd == true",
                DT::dataTableOutput("topic_understanding_sd_table")
              )
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Learning interests", style = "color: #5c2f92; cursor: pointer;")),
              p(style = "font-size: 12px; color: #5f6369; margin-bottom: 8px;",
                "Counts how often each option was selected. Comma-separated cells are re-matched to full option labels so fragments (e.g. from “… church, job, club …”) are not counted separately."),
              checkboxInput(
                "learning_interests_pct_denominator_all_post",
                "% denominator: all Big Post rows (not only those who answered keep-in-touch)",
                value = FALSE
              ),
              div(class = "plot-wide", plotlyOutput("learning_keep_in_touch_bar", height = "440px")),
              h5("By demographic", style = "color: #5c2f92; margin-top: 20px;"),
              p(style = "font-size: 11px; color: #6a6f75; margin-bottom: 6px;",
                tags$strong("Education ordering:"), " The dashboard maps raw survey wording to a single ordered list (Middle school → … → Doctorate). ",
                "Reach charts and Learning heatmaps now use the same mapping, so axes match filters and Pre/Post education charts."),
              p(style = "font-size: 12px; color: #5f6369; margin-bottom: 4px;",
                "Each ", tags$strong("column"), " is a demographic subgroup (e.g. an age band); each ",
                tags$strong("row"), " is a learning interest. The cell ", tags$strong("color/value is the percent"),
                " of that subgroup who selected that interest."),
              p(style = "font-size: 11px; color: #6a6f75; margin-bottom: 8px;",
                "Hover any cell for the detail. ", tags$strong("\u201CX of Y people in this subgroup\u201D"),
                " means X respondents in that subgroup chose this interest, out of Y total respondents in that subgroup who answered the keep-in-touch question. ",
                "Multi-select is allowed, so a column\u2019s percentages can sum to more than 100%."),
              fluidRow(
                column(6, plotlyOutput("learning_keep_in_touch_by_gender", height = "420px")),
                column(6, plotlyOutput("learning_keep_in_touch_by_age", height = "420px"))
              ),
              fluidRow(
                column(6, plotlyOutput("learning_keep_in_touch_by_income", height = "420px")),
                column(6, plotlyOutput("learning_keep_in_touch_by_education", height = "420px"))
              )
            ),
            tags$details(
              open = TRUE,
              tags$summary(h4("Wordcloud stories", style = "color: #5c2f92; cursor: pointer;")),
              div(class = "wc-controls",
                sliderInput("wc_max_words", "Words per cloud",
                  min = 15, max = 120, value = 60, step = 5, width = "320px"),
                p(style = "font-size: 11px; color: #5f6369; margin: 2px 0 0 0;",
                  "Each cloud keeps the most frequent words after stopword filtering. Slide right to include more (rarer) words; slide left to trim down to the strongest themes. Applies to every cloud below.")
              ),
              htmlOutput("impact_sentiment_lexicon_blurb"),
              tags$details(
                class = "wc-tier1",
                open = TRUE,
                tags$summary(h5("Pre — before today’s workshop", style = "color: #5c2f92; cursor: pointer;")),
                tags$details(
                  class = "wc-tier3",
                  open = TRUE,
                  tags$summary(h5("1. Today’s intention", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "What is one intention you have for today’s workshop?"),
                  checkboxInput("impact_pre_intention_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_pre_intention", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_pre_intention_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_pre_intention_show_responses == true",
                    DT::dataTableOutput("impact_pre_intention_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("2. Curious?", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "If you are arriving with any questions or curiosities, please share!"),
                  checkboxInput("impact_pre_curiosity_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_pre_curiosity", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_pre_curiosity_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_pre_curiosity_show_responses == true",
                    DT::dataTableOutput("impact_pre_curiosity_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("3. Hope to feel", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "How do you hope to feel at the end of today’s workshop?"),
                  checkboxInput("impact_pre_hope_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_pre_hope", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_pre_hope_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_pre_hope_show_responses == true",
                    DT::dataTableOutput("impact_pre_hope_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("4. Additional comments", style = "color: #5c2f92; cursor: pointer;")),
                  checkboxInput("impact_pre_extra_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_pre_extra", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_pre_extra_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_pre_extra_show_responses == true",
                    DT::dataTableOutput("impact_pre_extra_responses_table")
                  )
                )
              ),
              tags$details(
                class = "wc-tier1",
                open = TRUE,
                tags$summary(h5("Post — reflections", style = "color: #5c2f92; cursor: pointer;")),
                p(style = "font-size: 12px; color: #5f6369; margin-bottom: 12px;",
                  tags$strong("Items 1–3"), " describe ",
                  tags$strong("today’s workshop session"), ". ",
                  tags$strong("Impact story (5)"), " reflects the ",
                  tags$strong("overall program series"), ", not only this session. ",
                  "Learning interests are in the section above."),
                tags$details(
                  class = "wc-tier3",
                  open = TRUE,
                  tags$summary(h5("1. Insight from today", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "What is one idea, tool, or insight from today that stood out to you most?"),
                  checkboxInput("impact_post_insight_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_post_insight", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_post_insight_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_post_insight_show_responses == true",
                    DT::dataTableOutput("impact_post_insight_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("2. Applied learning", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "What is one way you could apply something you learned today in your own life?"),
                  checkboxInput("impact_post_apply_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_post_apply", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_post_apply_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_post_apply_show_responses == true",
                    DT::dataTableOutput("impact_post_apply_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("3. Helpful?", style = "color: #5c2f92; cursor: pointer;")),
                  p(style = "font-size: 12px; color: #5f6369;", "What made today’s session helpful for you?"),
                  checkboxInput("impact_post_helpful_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_post_helpful", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_post_helpful_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_post_helpful_show_responses == true",
                    DT::dataTableOutput("impact_post_helpful_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(h5("4. Additional comments", style = "color: #5c2f92; cursor: pointer;")),
                  checkboxInput("impact_post_extra_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_post_extra", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_post_extra_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_post_extra_show_responses == true",
                    DT::dataTableOutput("impact_post_extra_responses_table")
                  )
                ),
                tags$details(
                  class = "wc-tier3",
                  open = FALSE,
                  tags$summary(
                    div(class = "fav-header-row", style = "display: inline-flex;",
                      h5("5. Impact story (overall program)", style = "color: #5c2f92; cursor: pointer; margin: 0;"),
                      fav_star_button("learning_impact_story")
                    )
                  ),
                  p(style = "font-size: 12px; color: #5f6369;", "How has participating in this program helped or impacted you? Your story inspires others!"),
                  checkboxInput("impact_post_story_show_responses", "Show individual responses", value = FALSE),
                  div(class = "learning-wc-row", .dual_wc_ui_block("impact_post_story", wc_height = "420px")),
                  div(class = "learning-sentiment-panel",
                    wellPanel(style = "background:#fff;", htmlOutput("impact_post_story_sent"))
                  ),
                  conditionalPanel(
                    condition = "input.impact_post_story_show_responses == true",
                    DT::dataTableOutput("impact_post_story_responses_table")
                  )
                )
              ),
              div(style = "min-height: 240px; padding-bottom: 80px; width: 100%;")
            )
          ),
          tabPanel(
            "Annual Survey",
            value = "annual_survey",
            annual_survey_tab_ui()
          ),
          tabPanel(
            "User Journey",
            value = "user_journey",
            user_journey_tab_ui()
          ),
          tabPanel("Favorites", value = "favorites_impact",
            favorites_tab_ui()
          )
        )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Deployed-only login gate: validate credentials when auth is enabled.
  if (.auth_enabled()) {
    shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(.auth_credentials())
    )
  }

  # Trap errors to avoid grey screen; log and show notification
  options(shiny.error = function() {
    err <- geterrmessage()
    try(shiny::showNotification(paste("Error:", err), type = "error", duration = 15), silent = TRUE)
    message("Shiny error: ", err)
  })

  # ========================================================================
  # Data Loading (Reactive)
  # ========================================================================
  # Load Master Pre/Post and Program Manager once per refresh click (explicit cache via reactiveVal)
  data_ready <- reactiveVal(FALSE)
  # FALSE until sheets are loaded; overlay dismisses on the next flushed paint after that.
  app_ui_ready <- reactiveVal(FALSE)
  master_pre_val <- reactiveVal(data.frame())
  master_post_val <- reactiveVal(data.frame())
  master_annual_val <- reactiveVal(data.frame())
  program_manager_val <- reactiveVal(data.frame())
  mercy_program_manager_val <- reactiveVal(data.frame())
  # Last choices pushed to sidebar widgets — skip no-op update*Input calls (prevents request storms).
  last_org_choices <- reactiveVal(NULL)
  last_group_choices <- reactiveVal(NULL)
  last_group_selected <- reactiveVal(NULL)
  last_lang_choices <- reactiveVal(NULL)
  last_module_choices <- reactiveVal(NULL)
  last_module_selected <- reactiveVal(NULL)
  
  dismiss_dashboard_loading <- function() {
    try(shiny::removeUI(selector = "#boot_loading_overlay", immediate = TRUE), silent = TRUE)
    try(session$sendCustomMessage("dashBootDone", list()), silent = TRUE)
  }

  # Holds repaired PM used for typing (session_id typos remapped).
  program_manager_typing_val <- reactiveVal(NULL)
  qa_repair_summary_val <- reactiveVal(NULL)

  load_all_data <- function() {
    # Keep overlay up until every sheet finishes (Pre/Post/Annual/PM/Mercy).
    app_ui_ready(FALSE)
    data_ready(FALSE)
    last_org_choices(NULL)
    last_group_choices(NULL)
    last_group_selected(NULL)
    last_lang_choices(NULL)
    last_module_choices(NULL)
    last_module_selected(NULL)

    pre <- tryCatch(load_master_pre(), error = function(e) {
      warning("load_master_pre: ", conditionMessage(e))
      data.frame()
    })
    post <- tryCatch(load_master_post(), error = function(e) {
      warning("load_master_post: ", conditionMessage(e))
      data.frame()
    })
    pm <- tryCatch(load_program_manager(), error = function(e) {
      warning("load_program_manager: ", conditionMessage(e))
      data.frame()
    })
    annual <- tryCatch(load_master_annual(), error = function(e) {
      warning("load_master_annual: ", conditionMessage(e))
      data.frame()
    })
    pm_mercy <- tryCatch(load_mercy_program_manager(), error = function(e) {
      warning("load_mercy_program_manager: ", conditionMessage(e))
      data.frame()
    })

    # PM registry repair + is_test flag (in-memory; email on irreparable)
    pm_combined <- tryCatch(
      combine_program_managers_for_typing(pm, pm_mercy),
      error = function(e) data.frame()
    )
    repaired <- tryCatch(
      repair_masters_with_pm(pre, post, pm_combined, alert = TRUE),
      error = function(e) {
        warning("repair_masters_with_pm: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(repaired)) {
      pre <- repaired$pre %||% pre
      post <- repaired$post %||% post
      if (!is.null(repaired$pm_for_typing) && nrow(repaired$pm_for_typing)) {
        program_manager_typing_val(repaired$pm_for_typing)
      } else {
        program_manager_typing_val(pm_combined)
      }
      qa_repair_summary_val(list(
        repaired_n = repaired$repaired_n,
        test_n = repaired$test_n,
        irreparable_n = nrow(repaired$irreparable %||% data.frame()),
        alert = repaired$alert
      ))
      if (!is.null(repaired$alert) && isTRUE(repaired$alert$sent == FALSE) &&
          !is.null(repaired$alert$path) && nrow(repaired$irreparable %||% data.frame()) > 0) {
        try(shiny::showNotification(
          paste0(
            "QA: ", nrow(repaired$irreparable),
            " irreparable session_id(s). Report: ", basename(repaired$alert$path)
          ),
          type = "warning",
          duration = 12
        ), silent = TRUE)
      }
    } else {
      pre <- flag_is_test_rows(pre)
      post <- flag_is_test_rows(post)
      program_manager_typing_val(pm_combined)
    }

    master_pre_val(pre)
    master_post_val(post)
    program_manager_val(pm)
    master_annual_val(annual)
    mercy_program_manager_val(pm_mercy)
    data_ready(TRUE)
  }

  # After sheets land, dismiss overlay on the next UI flush (filter observers run in between).
  observeEvent(data_ready(), {
    if (!isTRUE(data_ready())) {
      app_ui_ready(FALSE)
      return()
    }
    session$onFlushed(function() {
      if (!isTRUE(isolate(data_ready()))) return()
      app_ui_ready(TRUE)
      dismiss_dashboard_loading()
    }, once = TRUE)
  }, ignoreInit = TRUE)

  # Start sheet load as soon as the session exists (first paint can show header + overlay).
  later::later(function() {
    shiny::withReactiveDomain(session, {
      tryCatch(load_all_data(), error = function(e) {
        message("load_all_data failed: ", conditionMessage(e))
        data_ready(TRUE)
        app_ui_ready(TRUE)
        dismiss_dashboard_loading()
      })
    })
  }, delay = 0)

  # Overview zip maps: viewport from plotly relayout → coarser grid at low zoom
  zip_map_view_pre <- reactiveVal(NULL)
  zip_map_view_post <- reactiveVal(NULL)

  observeEvent(input$refresh_data, {
    zip_map_view_pre(NULL)
    zip_map_view_post(NULL)
    app_ui_ready(FALSE)
    load_all_data()
  }, ignoreInit = TRUE)

  # Debounce N/% toggles on Behaviors tab so Plotly outputs do not flash-reload on every click
  behavioral_bar_flags_d <- shiny::debounce(reactive({
    list(
      n = tryCatch(isTRUE(input$opt_show_n), error = function(e) FALSE),
      pct = tryCatch(isTRUE(input$opt_show_pct), error = function(e) FALSE)
    )
  }), millis = 400)

  satisfaction_bar_flags_d <- shiny::debounce(reactive({
    list(
      n = tryCatch(isTRUE(input$opt_show_n), error = function(e) FALSE),
      pct = tryCatch(isTRUE(input$opt_show_pct), error = function(e) FALSE)
    )
  }), millis = 400)

  satisfaction_facilitator_plot_flags_d <- shiny::debounce(reactive({
    list(
      same_y = tryCatch(isTRUE(input$opt_same_y), error = function(e) TRUE),
      n = tryCatch(isTRUE(input$opt_show_n), error = function(e) FALSE),
      pct = tryCatch(isTRUE(input$opt_show_pct), error = function(e) FALSE)
    )
  }), millis = 400)
  
  # Simple accessors used throughout the app
  master_pre <- reactive(master_pre_val())
  master_post <- reactive(master_post_val())
  master_annual <- reactive(master_annual_val())
  program_manager <- reactive(program_manager_val())
  mercy_program_manager <- reactive(mercy_program_manager_val())
  # Full company + Mercy PM for Big/Little Pre/Post typing (not org-filtered).
  # Prefer in-memory repaired/normalized ids from load_all_data when available.
  program_manager_for_typing <- reactive({
    repaired <- tryCatch(program_manager_typing_val(), error = function(e) NULL)
    if (is.data.frame(repaired) && nrow(repaired) > 0) return(repaired)
    combine_program_managers_for_typing(program_manager(), mercy_program_manager())
  })
  observeEvent(plotly::event_data("plotly_relayout", source = "zipmap_pre"), {
    req(data_ready())
    ev <- plotly::event_data("plotly_relayout", source = "zipmap_pre")
    nv <- parse_plotly_geo_relayout(ev)
    if (is.null(nv)) return()
    if (view_changed_zip_map(zip_map_view_pre(), nv)) zip_map_view_pre(nv)
  }, ignoreNULL = TRUE)
  observeEvent(plotly::event_data("plotly_relayout", source = "zipmap_post"), {
    req(data_ready())
    ev <- plotly::event_data("plotly_relayout", source = "zipmap_post")
    nv <- parse_plotly_geo_relayout(ev)
    if (is.null(nv)) return()
    if (view_changed_zip_map(zip_map_view_post(), nv)) zip_map_view_post(nv)
  }, ignoreNULL = TRUE)

  dq_report <- reactive({
    req(data_ready())
    master_data_quality_report(master_pre(), master_post())
  })
  
  # Loading overlay until sheets are loaded and filter sidebar has settled
  output$dashboard_loading_overlay <- renderUI({
    if (isTRUE(app_ui_ready())) return(NULL)
    div(
      class = "dashboard-loading-overlay",
      div(
        class = "dashboard-loading-card",
        div(class = "dashboard-loading-spinner"),
        h4("Loading data dashboard...", style = "color: #5c2f92; margin: 0 0 8px 0;"),
        p("Pulling data from the Master Workbook. Please wait.",
          style = "color: #5f6369; margin: 0; font-size: 13px;")
      )
    )
  })
  
  # ========================================================================
  # Update Filter Choices Based on Data
  # ========================================================================
  # Org and Group dropdowns: filled only from Master workbook columns (org_name, group).
  # global.R normalizes column names (Organization/org name -> org_name, Group -> group) when loading.

  data_load_notified <- reactiveVal(FALSE)
  observe({
    pre_data <- master_pre()
    post_data <- master_post()
    orgs <- character(0)
    org_col <- NULL
    if (nrow(pre_data) > 0) {
      if ("org_name" %in% colnames(pre_data)) org_col <- "org_name"
      else if ("Organization" %in% colnames(pre_data)) org_col <- "Organization"
      else { oc <- grep("^org|^organization", colnames(pre_data), ignore.case = TRUE, value = TRUE); if (length(oc) > 0) org_col <- oc[1] }
      if (!is.null(org_col)) {
        orgs <- unique(as.character(pre_data[[org_col]]))
        orgs <- orgs[!is.na(orgs) & trimws(orgs) != ""]
      }
    }
    if (length(orgs) == 0 && nrow(post_data) > 0 && "org_name" %in% colnames(post_data)) {
      orgs <- unique(as.character(post_data$org_name))
      orgs <- orgs[!is.na(orgs) & trimws(orgs) != ""]
    }
    orgs <- sort(unique(trimws(orgs)))
    choices_org <- setNames(orgs, orgs)
    if (!identical(choices_org, isolate(last_org_choices()))) {
      last_org_choices(choices_org)
      shiny::freezeReactiveValue(input, "selected_org")
      cur_orgs <- .sidebar_filter_values(tryCatch(input$selected_org, error = function(e) NULL))
      sel_orgs <- intersect(cur_orgs, orgs)
      updateSelectizeInput(session, "selected_org", choices = choices_org, selected = sel_orgs, server = TRUE)
    }
    if (length(orgs) > 0 && !data_load_notified()) {
      data_load_notified(TRUE)
      try(shiny::showNotification(paste("Data loaded:", nrow(pre_data), "pre,", nrow(post_data), "post,", length(orgs), "organizations"), type = "message", duration = 4), silent = TRUE)
    }
  })
  observe({
    # Nested group choices under org optgroups. Require org selection first.
    pre_data <- master_pre()
    post_data <- master_post()
    sel_orgs <- .sidebar_filter_values(tryCatch(input$selected_org, error = function(e) NULL))
    choices_grp <- .build_group_optgroup_choices(pre_data, post_data, sel_orgs)
    valid_keys <- if (!length(choices_grp)) {
      character(0)
    } else {
      unique(unlist(lapply(choices_grp, unname), use.names = FALSE))
    }
    cur_grps <- isolate(.sidebar_filter_values(tryCatch(input$selected_group, error = function(e) NULL)))
    sel_grps <- intersect(cur_grps, valid_keys)
    placeholder <- if (!length(sel_orgs)) {
      "Select organizations first"
    } else {
      "All groups in selected orgs"
    }
    choices_changed <- !identical(choices_grp, isolate(last_group_choices()))
    sel_changed <- !identical(sort(sel_grps), sort(isolate(last_group_selected()) %||% character(0)))
    if (!choices_changed && !sel_changed) return()
    if (choices_changed || !identical(sort(sel_grps), sort(cur_grps))) {
      last_group_choices(choices_grp)
      last_group_selected(sel_grps)
      shiny::freezeReactiveValue(input, "selected_group")
      updateSelectizeInput(
        session, "selected_group",
        choices = choices_grp,
        selected = sel_grps,
        server = FALSE,
        options = list(placeholder = placeholder, plugins = list("remove_button"))
      )
    } else {
      last_group_choices(choices_grp)
      last_group_selected(sel_grps)
    }
  })
  
  observe({
    pre_data <- master_pre()
    post_data <- master_post()
    choices_lang <- c("All languages" = "All", language_filter_choices(pre_data, post_data))
    if (!identical(choices_lang, isolate(last_lang_choices()))) {
      last_lang_choices(choices_lang)
      shiny::freezeReactiveValue(input, "filter_language")
      cur_lang <- .sidebar_filter_choice(tryCatch(input$filter_language, error = function(e) NULL), default = "All")
      sel_lang <- if (!is.null(cur_lang) && cur_lang %in% unname(choices_lang)) cur_lang else "All"
      updateSelectInput(session, "filter_language", choices = choices_lang, selected = sel_lang)
    }
  })
  
  # Update gender dropdown
  observe({
    pre_data <- master_pre()
    if (nrow(pre_data) > 0) {
      # Find gender column (may vary)
      gender_col <- grep("gender|Gender", colnames(pre_data), ignore.case = TRUE, value = TRUE)
      if (length(gender_col) > 0) {
        genders <- unique(pre_data[[gender_col[1]]])
        genders <- genders[!is.na(genders) & genders != ""]
        updateCheckboxGroupInput(session, "filter_gender", choices = setNames(genders, genders))
      }
      
      # Update income filter (ordered)
      income_col <- grep("Household Income", colnames(pre_data), ignore.case = TRUE, value = TRUE)
      if (length(income_col) > 0) {
        incomes <- unique(pre_data[[income_col[1]]])
        incomes <- incomes[!is.na(incomes) & incomes != ""]
        # Order income levels
        income_order <- c(
          "If one person under $36k; two people under $41k; family of four under $52k",
          "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
          "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
          "If one person over $97k; two people over $111k; family of four over $139k"
        )
        ordered_incomes <- c(intersect(income_order, incomes), setdiff(incomes, income_order))
        updateCheckboxGroupInput(session, "filter_income", choices = setNames(ordered_incomes, ordered_incomes))
      }
      
      # Update education filter (ordered)
      edu_col <- grep("highest level of education", colnames(pre_data), ignore.case = TRUE, value = TRUE)
      if (length(edu_col) > 0) {
        edus <- unique(pre_data[[edu_col[1]]])
        edus <- edus[!is.na(edus) & edus != ""]
        canon <- unique(.normalize_education_for_dashboard(edus))
        ordered_edus <- c(intersect(EDUCATION_ORDER, canon), sort(setdiff(canon, EDUCATION_ORDER)))
        updateCheckboxGroupInput(session, "filter_education", choices = setNames(ordered_edus, ordered_edus))
      }
    }
  })
  
  # ========================================================================
  # Filtered Data (Reactive)
  # ========================================================================
  
  # Safe access to org/group (they live in uiOutput and may be NULL before first render)
  # Multi-select org/group: character(0) = no restriction (all).
  selected_orgs <- reactive({
    .sidebar_filter_values(tryCatch(input$selected_org, error = function(e) NULL))
  })
  selected_groups <- reactive({
    .sidebar_filter_values(tryCatch(input$selected_group, error = function(e) NULL))
  })
  # Back-compat aliases used in a few snapshot strings
  selected_org <- reactive({
    orgs <- selected_orgs()
    if (!length(orgs)) "All" else if (length(orgs) == 1L) orgs[[1]] else paste(orgs, collapse = " | ")
  })
  selected_group <- reactive({
    grps <- selected_groups()
    if (!length(grps)) "All" else if (length(grps) == 1L) grps[[1]] else paste(grps, collapse = " | ")
  })
  
  # Filtered program manager data
  filtered_program_manager <- reactive({
    data <- program_manager()
    if (nrow(data) == 0) return(data)
    data <- .apply_org_group_scope(data, selected_orgs(), selected_groups())
    
    # Filter by date range (only when checkbox on)
    if (tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE) && "date" %in% colnames(data)) {
      dr <- tryCatch(input$date_range, error = function(e) NULL)
      if (!is.null(dr) && length(dr) >= 2) {
        data$date <- as.Date(data$date)
        data <- data[data$date >= dr[1] & data$date <= dr[2], ]
      }
    }
    
    return(data)
  })

  # Sidebar: module checkboxes from Program Manager (scoped to org/group/date when PM rows exist; else full PM).
  # When no org/group filter, always select the full module list so a sticky subset cannot undercount.
  # Skip updateCheckboxGroupInput when choices/selection are unchanged — re-pushing retriggers every output.
  observeEvent(
    list(selected_orgs(), selected_groups(), nrow(program_manager()), nrow(master_post())),
    {
      pm_full <- tryCatch(program_manager(), error = function(e) data.frame())
      pm_scoped <- tryCatch(filtered_program_manager(), error = function(e) data.frame())
      pm <- if (nrow(pm_scoped) > 0) pm_scoped else pm_full
      post_data <- tryCatch(master_post(), error = function(e) data.frame())
      mods <- .program_manager_module_choices(pm, post_data)
      scope_is_all <- !length(selected_orgs()) && !length(selected_groups())
      cur <- isolate(tryCatch(input$selected_modules, error = function(e) NULL))
      new_sel <- if (scope_is_all || is.null(cur) || length(cur) == 0) {
        mods
      } else {
        inter <- intersect(cur, mods)
        if (length(inter) > 0) inter else mods
      }
      same_choices <- identical(mods, isolate(last_module_choices()))
      same_sel <- identical(sort(unique(as.character(new_sel))), sort(unique(as.character(isolate(last_module_selected()) %||% character(0)))))
      if (same_choices && same_sel) return()
      last_module_choices(mods)
      last_module_selected(new_sel)
      # Freeze so downstream reactives don't see a one-tick stale subset while the UI catches up.
      shiny::freezeReactiveValue(input, "selected_modules")
      updateCheckboxGroupInput(session, "selected_modules", choices = setNames(mods, mods), selected = new_sel)
    },
    ignoreNULL = FALSE
  )
  
  # Big Pre data (filtered and typed via company + Mercy PM)
  filtered_big_pre <- reactive({
    pre_data <- filtered_pre()
    pm_data <- program_manager_for_typing()
    if (nrow(pre_data) == 0) return(data.frame())
    
    pre_typed <- identify_survey_type(pre_data, pm_data)
    pre_typed %>% filter(is_big_pre == TRUE)
  })
  
  # Post data for Impact tabs: typed with company + Mercy PM.
  # Includes Big Post and Little Post rows — Little Post carries satisfaction, learning, open text, etc.
  # Big Post-only metrics (e.g. "Compared to before…") show empty states when those fields are absent.
  filtered_big_post <- reactive({
    post_data <- filtered_post()
    pm_data <- program_manager_for_typing()
    if (nrow(post_data) == 0) return(data.frame())
    identify_survey_type(post_data, pm_data)
  })
  
  # Strict Big Post only (last session in series) — for indices that require Compared-to-before items.
  filtered_big_post_only <- reactive({
    post_data <- filtered_big_post()
    if (nrow(post_data) == 0 || !"is_big_post" %in% names(post_data)) return(post_data)
    post_data[post_data$is_big_post == TRUE, , drop = FALSE]
  })

  filtered_little_pre <- reactive({
    pre_data <- filtered_pre()
    pm_data <- program_manager_for_typing()
    if (nrow(pre_data) == 0) return(data.frame())
    pre_typed <- identify_survey_type(pre_data, pm_data)
    pre_typed %>% dplyr::filter(is_big_pre == FALSE)
  })

  filtered_little_post <- reactive({
    post_data <- filtered_big_post()
    if (nrow(post_data) == 0 || !"is_big_post" %in% names(post_data)) return(data.frame())
    post_data[post_data$is_big_post == FALSE, , drop = FALSE]
  })

  # Annual Survey: not session-typed. Optional org match on host-organization text; date on timestamp.
  filtered_annual <- reactive({
    data <- master_annual()
    if (is.null(data) || nrow(data) == 0) return(data.frame())
    orgs <- tryCatch(selected_orgs(), error = function(e) character(0))
    if (length(orgs) > 0) {
      host_col <- annual_find_col(data, ANNUAL_COL_PATTERNS$host_org)
      org_col <- if ("org_name" %in% names(data)) "org_name" else NULL
      keep <- rep(FALSE, nrow(data))
      if (!is.null(host_col)) {
        host_txt <- as.character(data[[host_col]])
        for (org in orgs) {
          keep <- keep | grepl(org, host_txt, ignore.case = TRUE, fixed = TRUE)
        }
      }
      if (!is.null(org_col)) {
        keep <- keep | (trimws(as.character(data[[org_col]])) %in% orgs)
      }
      if (any(keep)) data <- data[keep, , drop = FALSE]
    }
    # Demographics (when columns exist)
    if (!is.null(input$filter_gender) && length(input$filter_gender) > 0) {
      gcol <- annual_find_col(data, "Gender Identity")
      if (!is.null(gcol)) data <- data[data[[gcol]] %in% input$filter_gender, , drop = FALSE]
    }
    if (!is.null(input$filter_veteran) && length(input$filter_veteran) > 0) {
      vcol <- annual_find_col(data, "Veteran Status")
      if (!is.null(vcol)) data <- data[data[[vcol]] %in% input$filter_veteran, , drop = FALSE]
    }
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      icol <- annual_find_col(data, "Household Income")
      if (!is.null(icol)) data <- data[data[[icol]] %in% input$filter_income, , drop = FALSE]
    }
    if (!is.null(input$filter_education) && length(input$filter_education) > 0) {
      ecol <- annual_find_col(data, "highest level of education")
      if (!is.null(ecol)) {
        nv <- .normalize_education_for_dashboard(data[[ecol]])
        data <- data[nv %in% input$filter_education, , drop = FALSE]
      }
    }
    data <- apply_master_date_filter_rows(
      data,
      tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE),
      tryCatch(input$date_range, error = function(e) NULL)
    )
    data
  })

  filter_cache_key <- reactive({
    req(data_ready())
    digest::digest(list(
      org = tryCatch(paste(sort(selected_orgs()), collapse = "|"), error = function(e) ""),
      grp = tryCatch(paste(sort(selected_groups()), collapse = "|"), error = function(e) ""),
      lang = tryCatch(input$filter_language, error = function(e) "All"),
      gender = tryCatch(paste(sort(input$filter_gender %||% character(0)), collapse = "|"), error = function(e) ""),
      veteran = tryCatch(paste(sort(input$filter_veteran %||% character(0)), collapse = "|"), error = function(e) ""),
      income = tryCatch(paste(sort(input$filter_income %||% character(0)), collapse = "|"), error = function(e) ""),
      education = tryCatch(paste(sort(input$filter_education %||% character(0)), collapse = "|"), error = function(e) ""),
      modules = tryCatch(paste(sort(input$selected_modules %||% character(0)), collapse = "|"), error = function(e) ""),
      pre_n = nrow(filtered_big_pre()),
      post_n = nrow(filtered_big_post())
    ))
  })
  
  # Filtered pre-survey data (aligned with session_summary: date, org, group)
  # Use normalized session_id for matching; if no pre rows would match, skip session_id filter so Pre data still loads
  filtered_pre <- reactive({
    data <- master_pre()
    if (nrow(data) == 0) return(data)
    # Default-exclude test / junk rows flagged at load
    if (exists("exclude_test_rows", mode = "function")) {
      data <- exclude_test_rows(data, default_exclude = TRUE)
    }
    if (nrow(data) == 0) return(data)
    ss <- session_summary_data()
    if (nrow(ss) > 0 && "session_id" %in% colnames(ss) && "session_id" %in% colnames(data)) {
      data_sid <- trimws(as.character(data$session_id))
      ss_sid <- trimws(as.character(ss$session_id))
      match_pre <- data_sid %in% ss_sid & !is.na(data_sid) & data_sid != ""
      if (any(match_pre)) data <- data[match_pre, , drop = FALSE]
      # If no match would keep any pre rows but we have pre data and summary has rows, don't drop all pre (keep all; org/group filter below still applies)
    }
    if (nrow(data) == 0) return(data)
    # Org + nested group scope (whole org unless subgroups picked for that org)
    data <- .apply_org_group_scope(data, selected_orgs(), selected_groups())
    
    # Filter by gender (checkbox - empty/null means all)
    if (!is.null(input$filter_gender) && length(input$filter_gender) > 0) {
      gender_col <- grep("gender|Gender", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(gender_col) > 0) {
        data <- data[data[[gender_col[1]]] %in% input$filter_gender, ]
      }
    }
    
    # Filter by veteran status (checkbox - empty/null means all)
    if (!is.null(input$filter_veteran) && length(input$filter_veteran) > 0) {
      veteran_col <- grep("veteran|Veteran", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(veteran_col) > 0) {
        data <- data[data[[veteran_col[1]]] %in% input$filter_veteran, ]
      }
    }
    
    # Filter by income (checkbox - empty/null means all)
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      income_col <- grep("Household Income", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(income_col) > 0) {
        data <- data[data[[income_col[1]]] %in% input$filter_income, ]
      }
    }
    
    # Filter by education (checkbox — compare canonical labels via .normalize_education_for_dashboard)
    if (!is.null(input$filter_education) && length(input$filter_education) > 0) {
      edu_col <- grep("highest level of education", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(edu_col) > 0) {
        nv <- .normalize_education_for_dashboard(data[[edu_col[1]]])
        data <- data[nv %in% input$filter_education, ]
      }
    }
    
    data <- apply_language_filter_rows(data, input$filter_language %||% "All")
    data <- apply_master_date_filter_rows(
      data,
      tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE),
      tryCatch(input$date_range, error = function(e) NULL)
    )
    return(data)
  })
  
  # Filtered post-survey data (aligned with session_summary; same demographic filters as pre)
  # Normalized session_id matching; if no post rows would match, skip session_id filter so Post data still loads
  filtered_post <- reactive({
    data <- master_post()
    if (nrow(data) == 0) return(data)
    if (exists("exclude_test_rows", mode = "function")) {
      data <- exclude_test_rows(data, default_exclude = TRUE)
    }
    if (nrow(data) == 0) return(data)
    ss <- session_summary_data()
    if (nrow(ss) > 0 && "session_id" %in% colnames(ss) && "session_id" %in% colnames(data)) {
      data_sid <- trimws(as.character(data$session_id))
      ss_sid <- trimws(as.character(ss$session_id))
      match_post <- data_sid %in% ss_sid & !is.na(data_sid) & data_sid != ""
      if (any(match_post)) data <- data[match_post, , drop = FALSE]
    }
    if (nrow(data) == 0) return(data)
    data <- .apply_org_group_scope(data, selected_orgs(), selected_groups())
    if (!is.null(input$filter_gender) && length(input$filter_gender) > 0) {
      gender_col <- grep("gender|Gender", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(gender_col) > 0) data <- data[data[[gender_col[1]]] %in% input$filter_gender, ]
    }
    if (!is.null(input$filter_veteran) && length(input$filter_veteran) > 0) {
      veteran_col <- grep("veteran|Veteran", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(veteran_col) > 0) data <- data[data[[veteran_col[1]]] %in% input$filter_veteran, ]
    }
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      income_col <- grep("Household Income", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(income_col) > 0) data <- data[data[[income_col[1]]] %in% input$filter_income, ]
    }
    if (!is.null(input$filter_education) && length(input$filter_education) > 0) {
      edu_col <- grep("highest level of education", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(edu_col) > 0) {
        nv <- .normalize_education_for_dashboard(data[[edu_col[1]]])
        data <- data[nv %in% input$filter_education, ]
      }
    }
    data <- apply_language_filter_rows(data, input$filter_language %||% "All")
    data <- apply_master_date_filter_rows(
      data,
      tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE),
      tryCatch(input$date_range, error = function(e) NULL)
    )
    data
  })
  
  # Helper function to get grouping column name and values
  get_group_by_column <- function(data) {
    if (is.null(input$group_by_demo) || input$group_by_demo == "None") return(NULL)
    
    col_name <- NULL
    if (input$group_by_demo == "Gender") {
      col_name <- grep("gender|Gender", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    } else if (input$group_by_demo == "Veteran") {
      col_name <- grep("veteran|Veteran", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    } else if (input$group_by_demo == "Income") {
      col_name <- grep("Household Income", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    } else if (input$group_by_demo == "Education") {
      col_name <- grep("highest level of education", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    } else if (input$group_by_demo == "FirstGenCollege") {
      col_name <- grep("First-Generation Status \\(College\\)", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    } else if (input$group_by_demo == "FirstGenUS") {
      col_name <- grep("First-Generation Status \\(U.S.\\)", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    }
    
    if (is.null(col_name) || length(col_name) == 0) return(NULL)
    return(col_name)
  }
  
  # Helper function to binarize Likert responses
  binarize_likert <- function(response, reverse = FALSE) {
    # Low: Strongly Disagree or Disagree
    # High: Agree or Strongly Agree
    # Neutral: excluded (returns NA)
    low_responses <- c("Strongly Disagree", "Disagree")
    high_responses <- c("Agree", "Strongly Agree")
    
    if (reverse) {
      # For stress: reverse the logic
      result <- ifelse(response %in% low_responses, "High", 
                      ifelse(response %in% high_responses, "Low", NA))
    } else {
      result <- ifelse(response %in% low_responses, "Low", 
                      ifelse(response %in% high_responses, "High", NA))
    }
    return(result)
  }
  
  # Helper function to split data by group_by
  split_data_by_group <- function(data) {
    if (is.null(input$group_by_demo) || input$group_by_demo == "None" || input$group_by_demo == "---" || input$group_by_demo == "---2") {
      return(list("All" = data))
    }
    
    # Check for binarized financial wellness groupings
    if (input$group_by_demo == "Wellness_Stress") {
      stress_col <- grep("stressed.*financ|able to manage stress", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(stress_col) > 0) {
        binarized <- binarize_likert(data[[stress_col[1]]], reverse = TRUE)
        data[["_temp_group_"]] <- binarized
        data <- data[!is.na(binarized), ]
        if (nrow(data) > 0) {
          result <- list()
          for (g in c("Low", "High")) {
            result[[paste0("Stress: ", g)]] <- data[data[["_temp_group_"]] == g, ]
          }
          data[["_temp_group_"]] <- NULL
          return(result)
        }
      }
    }
    
    if (input$group_by_demo == "Wellness_Optimism") {
      optimism_col <- grep("optimistic.*financial future", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(optimism_col) > 0) {
        binarized <- binarize_likert(data[[optimism_col[1]]], reverse = FALSE)
        data[["_temp_group_"]] <- binarized
        data <- data[!is.na(binarized), ]
        if (nrow(data) > 0) {
          result <- list()
          for (g in c("Low", "High")) {
            result[[paste0("Optimism: ", g)]] <- data[data[["_temp_group_"]] == g, ]
          }
          data[["_temp_group_"]] <- NULL
          return(result)
        }
      }
    }
    
    if (input$group_by_demo == "Wellness_Relationship") {
      rel_col <- grep("healthy relationship.*money", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(rel_col) > 0) {
        binarized <- binarize_likert(data[[rel_col[1]]], reverse = FALSE)
        data[["_temp_group_"]] <- binarized
        data <- data[!is.na(binarized), ]
        if (nrow(data) > 0) {
          result <- list()
          for (g in c("Low", "High")) {
            result[[paste0("Relationship: ", g)]] <- data[data[["_temp_group_"]] == g, ]
          }
          data[["_temp_group_"]] <- NULL
          return(result)
        }
      }
    }
    
    if (input$group_by_demo == "Wellness_Confidence") {
      conf_col <- grep("confident.*plan", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(conf_col) > 0) {
        binarized <- binarize_likert(data[[conf_col[1]]], reverse = FALSE)
        data[["_temp_group_"]] <- binarized
        data <- data[!is.na(binarized), ]
        if (nrow(data) > 0) {
          result <- list()
          for (g in c("Low", "High")) {
            result[[paste0("Confidence: ", g)]] <- data[data[["_temp_group_"]] == g, ]
          }
          data[["_temp_group_"]] <- NULL
          return(result)
        }
      }
    }
    
    if (input$group_by_demo == "Wellness_Comfort") {
      comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(data), ignore.case = TRUE, value = TRUE)
      if (length(comfort_col) > 0) {
        binarized <- binarize_likert(data[[comfort_col[1]]], reverse = FALSE)
        data[["_temp_group_"]] <- binarized
        data <- data[!is.na(binarized), ]
        if (nrow(data) > 0) {
          result <- list()
          for (g in c("Low", "High")) {
            result[[paste0("Comfort: ", g)]] <- data[data[["_temp_group_"]] == g, ]
          }
          data[["_temp_group_"]] <- NULL
          return(result)
        }
      }
    }
    
    # Check for regular demographic grouping
    col_name <- get_group_by_column(data)
    if (is.null(col_name)) return(list("All" = data))
    
    groups <- unique(data[[col_name]])
    groups <- groups[!is.na(groups) & groups != ""]
    
    result <- list()
    for (g in groups) {
      result[[as.character(g)]] <- data[data[[col_name]] == g & !is.na(data[[col_name]]), ]
    }
    
    return(result)
  }
  
  # ========================================================================
  # Session Summary Tab
  # ========================================================================
  
  # Raw session summary from Master Pre/Post (no filters)
  session_summary_raw <- reactive({
    tryCatch({
      pre_data <- master_pre()
      post_data <- master_post()
      # Company + Mercy PM so Mercy workshops type/series-label correctly in Overview
      pm_data <- tryCatch({
        combine_program_managers_for_typing(
          program_manager(),
          mercy_program_manager()
        )
      }, error = function(e) tryCatch(program_manager(), error = function(e2) data.frame()))
      create_session_summary_from_master(pre_data, post_data, pm_data)
    }, error = function(e) {
      warning("session_summary_raw error: ", conditionMessage(e))
      data.frame()
    })
  })
  
  # Centralized session filters (date / org / group). org/group are character vectors; empty = all.
  current_session_filters <- reactive({
    use_date <- tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE)
    dr <- tryCatch(input$date_range, error = function(e) NULL)
    list(
      use_date = use_date,
      date_range = dr,
      org = .sidebar_filter_values(tryCatch(input$selected_org, error = function(e) NULL)),
      group = .sidebar_filter_values(tryCatch(input$selected_group, error = function(e) NULL))
    )
  })
  
  # Filtered session summary used throughout the app
  session_summary_data <- reactive({
    tryCatch({
      summary <- session_summary_raw()
      if (nrow(summary) == 0) return(summary)

      filters <- current_session_filters()

      # Date filter
      if (isTRUE(filters$use_date) &&
          !is.null(filters$date_range) &&
          length(filters$date_range) == 2 &&
          all(!is.na(filters$date_range))) {
        start_date <- as.Date(filters$date_range[1])
        end_date <- as.Date(filters$date_range[2])
        if ("date" %in% colnames(summary)) {
          summary <- dplyr::filter(
            summary,
            !is.na(.data[["date"]]),
            .data[["date"]] >= start_date,
            .data[["date"]] <= end_date
          )
        }
      }

      # Organization + nested group scope
      if ((length(filters$org) > 0 || length(filters$group) > 0) &&
          "org_name" %in% colnames(summary)) {
        gvec <- if ("group" %in% colnames(summary)) summary$group else rep(NA_character_, nrow(summary))
        keep <- .rows_match_org_group_scope(summary$org_name, gvec, filters$org, filters$group)
        summary <- summary[keep, , drop = FALSE]
      }

      # Modules taught (sidebar). When no org/group filter, ignore selected_modules
      # so a stale scoped checkbox subset cannot flash a low total before the UI resets.
      scope_all <- !length(filters$org) && !length(filters$group)
      if (!scope_all) {
        pm_full_m <- tryCatch(program_manager(), error = function(e) data.frame())
        pm_scoped_m <- tryCatch(filtered_program_manager(), error = function(e) data.frame())
        pm_m <- if (nrow(pm_scoped_m) > 0) pm_scoped_m else pm_full_m
        post_m <- tryCatch(master_post(), error = function(e) data.frame())
        full_mods <- .program_manager_module_choices(pm_m, post_m)
        sel_mod <- tryCatch(input$selected_modules, error = function(e) character(0))
        if (length(sel_mod) > 0 && length(full_mods) > 0 &&
            !identical(sort(unique(sel_mod)), sort(unique(full_mods))) &&
            "modules_taught" %in% colnames(summary)) {
          keep <- .session_row_matches_selected_modules(summary$modules_taught, sel_mod)
          summary <- summary[keep, , drop = FALSE]
        }
      }

      summary
    }, error = function(e) {
      warning("session_summary_data error: ", conditionMessage(e))
      data.frame()
    })
  })

  session_summary_display_data <- reactive({
    summary_data <- session_summary_data()
    if (nrow(summary_data) == 0) return(summary_data)

    filtered_rows <- tryCatch(input$session_summary_table_rows_all, error = function(e) NULL)
    if (is.null(filtered_rows) || length(filtered_rows) == 0) return(summary_data)

    valid_rows <- filtered_rows[filtered_rows >= 1 & filtered_rows <= nrow(summary_data)]
    if (length(valid_rows) == 0) return(summary_data[integer(0), , drop = FALSE])

    summary_data[valid_rows, , drop = FALSE]
  })

  organization_summary_data <- reactive({
    tryCatch({
      calculate_organization_journeys(session_summary_data())
    }, error = function(e) {
      warning("organization_summary_data error: ", conditionMessage(e))
      data.frame()
    })
  })
  organization_underway_data <- reactive({
    tryCatch({
      ss <- session_summary_data()
      if (nrow(ss) == 0) return(data.frame())
      if (!"post_responses" %in% colnames(ss)) return(data.frame())
      org_clean <- ifelse(is.na(ss$org_name) | ss$org_name == "", "Unknown Organization", ss$org_name)
      grp <- coalesce(as.character(ss$group), "")
      sk <- if ("series_key" %in% names(ss)) as.character(ss$series_key) else NA_character_
      series_key <- dplyr::coalesce(sk, paste(org_clean, grp, sep = "||"))
      has_post <- ss %>% mutate(org_clean = org_clean, grp = grp, key = series_key) %>%
        group_by(key) %>% mutate(series_has_post = isTRUE(any(post_responses > 0, na.rm = TRUE))) %>% ungroup()
      ss_underway <- ss[which(!has_post$series_has_post), , drop = FALSE]
      if (nrow(ss_underway) == 0) return(data.frame())
      calculate_organization_journeys(ss_underway)
    }, error = function(e) { warning("organization_underway_data error: ", conditionMessage(e)); data.frame() })
  })
  # Session-level rows behind the "completed" table (series with any Big Post). Shared by the
  # journey summary and the per-org drill-down so both reflect the exact same sessions.
  organization_completed_sessions <- reactive({
    tryCatch({
      ss <- session_summary_data()
      if (nrow(ss) == 0) return(data.frame())
      if (!"post_responses" %in% colnames(ss)) return(data.frame())
      org_clean <- ifelse(is.na(ss$org_name) | ss$org_name == "", "Unknown Organization", ss$org_name)
      grp <- coalesce(as.character(ss$group), "")
      sk <- if ("series_key" %in% names(ss)) as.character(ss$series_key) else NA_character_
      series_key <- dplyr::coalesce(sk, paste(org_clean, grp, sep = "||"))
      has_post <- ss %>% mutate(org_clean = org_clean, grp = grp, key = series_key) %>%
        group_by(key) %>% mutate(series_has_post = isTRUE(any(post_responses > 0, na.rm = TRUE))) %>% ungroup()
      ss[which(has_post$series_has_post), , drop = FALSE]
    }, error = function(e) { warning("organization_completed_sessions error: ", conditionMessage(e)); data.frame() })
  })
  organization_completed_data <- reactive({
    tryCatch({
      ss_completed <- organization_completed_sessions()
      if (nrow(ss_completed) == 0) return(data.frame())
      calculate_organization_journeys(ss_completed)
    }, error = function(e) { warning("organization_completed_data error: ", conditionMessage(e)); data.frame() })
  })
  
  register_dashboard_modules(
    input, output, session,
    filtered_pre, filtered_post, filtered_annual,
    filtered_big_pre, filtered_little_pre, filtered_little_post, filtered_big_post_only,
    session_summary_data
  )

  .render_org_table <- function(org_summary, selection = "none") {
    if (is.null(org_summary) || nrow(org_summary) == 0) return(DT::datatable(data.frame(Message = "No data."), rownames = FALSE))
    len_cols <- paste0("Len", 1:6)
    has_len <- len_cols[len_cols %in% colnames(org_summary)]
    dt <- DT::datatable(
      org_summary,
      width = "100%",
      selection = selection,
      options = list(
        pageLength = 10,
        scrollX = FALSE,
        autoWidth = FALSE,
        order = list(list(1, "desc")),
        columnDefs = list(
          list(targets = 0, className = "dt-left", width = "26%"),
          list(targets = "_all", className = "dt-center")
        )
      ),
      rownames = FALSE
    )
    if (length(has_len) > 0) {
      dt <- dt %>% DT::formatStyle(
        columns = has_len,
        backgroundColor = DT::styleInterval(
          c(0, 1, 2, 3, 4, 5, 6),
          c("#ffffff", "#f3e5f5", "#e1bee7", "#ce93d8", "#ba68c8", "#ab47bc", "#7b1fa2", "#4a148c")
        )
      )
    }
    dt
  }
  # "Session in progress" only when there is at least one series with Pre but no Post
  output$impact_sessions_in_progress_section <- renderUI({
    uw <- tryCatch(organization_underway_data(), error = function(e) data.frame())
    if (is.null(uw) || nrow(uw) == 0) return(NULL)
    tagList(
      h5("Series in progress", style = "color: #5c2f92; margin-top: 8px; margin-bottom: 6px;"),
      p(style = "color: #5f6369; font-size: 11px; margin-bottom: 8px;",
        "Series (org + group) with at least one session that has Pre responses but no Post yet. Completed series (with any Post) appear in the table below."),
      DT::dataTableOutput("impact_org_table_underway")
    )
  })
  output$impact_org_table_underway <- DT::renderDataTable({
    req(app_ui_ready())
    uw <- tryCatch(organization_underway_data(), error = function(e) data.frame())
    if (is.null(uw) || nrow(uw) == 0) {
      return(DT::datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
    }
    tryCatch(.render_org_table(uw), error = function(e) DT::datatable(data.frame(Error = paste("Error:", conditionMessage(e))), rownames = FALSE))
  })
  outputOptions(output, "impact_org_table_underway", suspendWhenHidden = TRUE)
  output$impact_org_table_completed <- DT::renderDataTable({
    req(app_ui_ready())
    tryCatch(.render_org_table(organization_completed_data(), selection = "single"), error = function(e) DT::datatable(data.frame(Error = paste("Error:", conditionMessage(e))), rownames = FALSE))
  })
  outputOptions(output, "impact_org_table_completed", suspendWhenHidden = TRUE)

  # Per-session detail behind each completed org row (drill-down to confirm series counts).
  .org_session_detail_df <- function(sessions, org_name) {
    if (is.null(sessions) || nrow(sessions) == 0) return(data.frame())
    org_clean <- ifelse(is.na(sessions$org_name) | sessions$org_name == "", "Unknown Organization", sessions$org_name)
    d <- sessions[org_clean == org_name, , drop = FALSE]
    if (nrow(d) == 0) return(data.frame())
    grp <- dplyr::coalesce(as.character(d$group), "")
    bid <- if ("base_session_id" %in% names(d)) as.character(d$base_session_id) else as.character(d$session_id)
    sk <- if ("series_key" %in% names(d)) as.character(d$series_key) else rep(NA_character_, nrow(d))
    org_lab <- ifelse(is.na(d$org_name) | d$org_name == "", "Unknown Organization", as.character(d$org_name))
    journey_key <- dplyr::coalesce(sk, paste(org_lab, grp, bid, sep = "||"))
    snum <- if ("session_number" %in% names(d)) d$session_number else rep(NA, nrow(d))
    sser <- if ("sessions_in_series" %in% names(d)) d$sessions_in_series else rep(NA, nrow(d))
    out <- data.frame(
      `Series Key` = journey_key,
      Date = as.character(d$date),
      Group = grp,
      `Session ID` = as.character(d$session_id),
      `Base Session` = bid,
      `Session #` = ifelse(is.na(snum), "-", as.character(snum)),
      `Of` = ifelse(is.na(sser), "-", as.character(sser)),
      Pre = if ("pre_responses" %in% names(d)) d$pre_responses else NA,
      Post = if ("post_responses" %in% names(d)) d$post_responses else NA,
      Languages = toupper(as.character(if ("languages_offered" %in% names(d)) d$languages_offered else "en")),
      Facilitators = if ("facilitators" %in% names(d)) as.character(d$facilitators) else "",
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out[order(out$`Series Key`, out$Date, na.last = TRUE), , drop = FALSE]
  }

  .selected_completed_org <- reactive({
    sel <- input$impact_org_table_completed_rows_selected
    if (is.null(sel) || length(sel) == 0L) return(NULL)
    idx <- as.integer(sel[1])
    if (length(idx) == 0L || is.na(idx)) return(NULL)
    org_df <- organization_completed_data()
    if (is.null(org_df) || nrow(org_df) == 0L || idx < 1L || idx > nrow(org_df)) return(NULL)
    as.character(org_df$Organization[idx])
  })

  # Caption only (dynamic). The table itself is a STABLE output element in the UI (not nested
  # in renderUI) so DataTables is never destroyed/recreated mid-Ajax on row click.
  output$impact_org_detail <- renderUI({
    tryCatch({
      org_name <- .selected_completed_org()
      if (is.null(org_name)) {
        return(p(style = "color: #9a93a8; font-size: 12px; font-style: italic; margin-top: 10px;",
                 "Click an organization row above to see the individual sessions that make up its series."))
      }
      detail <- tryCatch(.org_session_detail_df(organization_completed_sessions(), org_name), error = function(e) data.frame())
      n_series <- if (nrow(detail) > 0L) length(unique(detail$`Series Key`)) else 0L
      div(style = "margin-top: 14px; border-top: 1px solid #e3dcef; padding-top: 12px;",
        h5(paste0("Sessions for: ", org_name), style = "color: #5c2f92; margin-bottom: 4px;"),
        p(style = "color: #5f6369; font-size: 11px; margin-bottom: 8px;",
          sprintf("%d session row(s) across %d series. ", nrow(detail), n_series),
          "Each row is one session (language variants already collapsed by Base Session). Rows sharing a ",
          tags$strong("Series Key"), " belong to the same workshop series; a key shaped ",
          tags$code("Org||Group||BaseSession"), " means Program Manager had no row for that session, so it counts as its own series.")
      )
    }, error = function(e) {
      p(style = "color: #c62828; font-size: 12px; margin-top: 10px;",
        paste("Error loading session detail:", conditionMessage(e)))
    })
  })

  output$impact_org_detail_table <- DT::renderDataTable({
    empty_dt <- DT::datatable(data.frame(), rownames = FALSE, options = list(dom = "t"))
    tryCatch({
      org_name <- .selected_completed_org()
      if (is.null(org_name)) return(empty_dt)
      detail <- tryCatch(.org_session_detail_df(organization_completed_sessions(), org_name), error = function(e) data.frame())
      if (is.null(detail) || nrow(detail) == 0L) {
        return(DT::datatable(data.frame(Message = "No sessions found."), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(
        detail,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 25, scrollX = TRUE, dom = "t", order = list(list(0, "asc")))
      )
    }, error = function(e) empty_dt)
  })
  outputOptions(output, "impact_org_detail_table", suspendWhenHidden = TRUE)
  # Series response matrices by planned length (Overview > Responses by Series). Uses PM
  # sessions_in_series when present; otherwise falls back to number of session rows (not n() alone when PM has length on one row).
  journey_response_matrices <- reactive({
    req(app_ui_ready())
    ss <- session_summary_data()
    pre <- tryCatch(filtered_pre(), error = function(e) data.frame())
    post <- tryCatch(filtered_post(), error = function(e) data.frame())
    if (nrow(ss) == 0 || !"org_name" %in% colnames(ss)) return(list())
    if (!"pre_responses" %in% colnames(ss) || !"post_responses" %in% colnames(ss)) return(list())
    ss <- ss %>% mutate(
      # series_label already includes org (short form); do not prefix org_name again
      org_grp = {
        if ("series_label" %in% names(ss)) {
          skv <- if ("series_key" %in% names(ss)) as.character(ss$series_key) else rep(NA_character_, nrow(ss))
          dplyr::coalesce(as.character(ss$series_label), skv)
        } else if ("series_key" %in% names(ss)) {
          dplyr::coalesce(as.character(ss$series_key), paste(coalesce(as.character(ss$org_name), ""), coalesce(as.character(ss$group), ""), sep = " | "))
        } else {
          paste(coalesce(as.character(ss$org_name), ""), coalesce(as.character(ss$group), ""), sep = " | ")
        }
      }
    )
    jdf <- ss %>%
      arrange(org_grp, session_number) %>%
      group_by(org_grp) %>%
      mutate(
        sessions_planned = {
          sv <- sessions_in_series[!is.na(sessions_in_series) & sessions_in_series > 0]
          if (length(sv) > 0) max(sv) else NA_real_
        },
        sessions_in_series = pmin(pmax(1, dplyr::coalesce(sessions_planned, as.numeric(dplyr::n()))), 6)
      ) %>%
      ungroup() %>%
      dplyr::select(-sessions_planned)
    # Paired Big Pre-Post: same users in first session pre and last session post
    if (nrow(pre) > 0 && nrow(post) > 0 &&
        "respondent_id" %in% colnames(pre) && "respondent_id" %in% colnames(post) &&
        "session_id" %in% colnames(pre) && "session_id" %in% colnames(post)) {
      valid_id <- function(x) !is.na(x) & nzchar(trimws(as.character(x))) & !grepl("^ANON#", as.character(x))
      pre_ids <- pre %>% filter(valid_id(.data[["respondent_id"]])) %>% select(session_id, respondent_id) %>% distinct()
      post_ids <- post %>% filter(valid_id(.data[["respondent_id"]])) %>% select(session_id, respondent_id) %>% distinct()
      first_last <- jdf %>% group_by(org_grp) %>%
        summarise(
          first_session = session_id[session_number == 1][1],
          last_session = session_id[session_number == max(session_number)][1],
          .groups = "drop"
        )
      paired_vec <- vapply(seq_len(nrow(first_last)), function(i) {
        sid_first <- first_last$first_session[i]
        sid_last <- first_last$last_session[i]
        ppre <- pre_ids$respondent_id[pre_ids$session_id == sid_first]
        ppost <- post_ids$respondent_id[post_ids$session_id == sid_last]
        length(intersect(ppre, ppost))
      }, integer(1))
      paired_by_journey <- data.frame(org_grp = first_last$org_grp, paired = paired_vec, stringsAsFactors = FALSE)
    } else {
      paired_by_journey <- jdf %>% group_by(org_grp) %>% summarise(paired = NA_integer_, .groups = "drop")
    }
    # Get first session date per series (for Date column)
    series_dates <- jdf %>% group_by(org_grp) %>% slice(1) %>% ungroup()
    if ("date" %in% colnames(jdf)) {
      series_dates <- series_dates %>% select(org_grp, series_date = date)
    } else {
      series_dates <- series_dates %>% select(org_grp) %>% mutate(series_date = NA)
    }
    out <- list()
    for (len in sort(unique(pmin(jdf$sessions_in_series, 6)))) {
      sub <- jdf %>% filter(sessions_in_series == len)
      if (nrow(sub) == 0) next
      # Reshape: each session → 2 columns (Pre, Post)
      sub_long <- sub %>%
        select(org_grp, session_number, pre_responses, post_responses) %>%
        tidyr::pivot_longer(cols = c(pre_responses, post_responses), names_to = "survey", values_to = "n") %>%
        mutate(survey = sub("_responses", "", survey), survey = sub("pre", "Pre", sub("post", "Post", survey))) %>%
        mutate(col = paste0("W", session_number, "_", survey))
      sub_wide <- sub_long %>%
        select(org_grp, col, n) %>%
        tidyr::pivot_wider(names_from = col, values_from = n)
      sub_wide <- sub_wide %>% left_join(paired_by_journey, by = "org_grp") %>%
        left_join(series_dates, by = "org_grp")
      # Order columns: Series, Date, W1_Pre, W1_Post, ..., Paired Big Pre-Post
      want_cols <- c(paste0("W", rep(1:len, each = 2), "_", rep(c("Pre", "Post"), len)), "paired")
      want_cols <- want_cols[want_cols %in% colnames(sub_wide)]
      sub_wide <- sub_wide %>% select(org_grp, series_date, all_of(want_cols))
      names(sub_wide)[1] <- "Series"
      names(sub_wide)[2] <- "Date"
      if ("paired" %in% names(sub_wide)) names(sub_wide)[names(sub_wide) == "paired"] <- "Paired Big Pre-Post"
      sub_wide$Date <- tryCatch(format(as.Date(sub_wide$Date), "%Y-%m-%d"), error = function(e) as.character(sub_wide$Date))
      out[[as.character(len)]] <- sub_wide
    }
    out
  })
  # Helper to render series response heatmap DT (brand colors, readable text, condensed width)
  .render_journey_response_heatmap <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE))
    id_cols <- c("Series", "Date")
    num_cols <- setdiff(colnames(df), id_cols)
    if (length(num_cols) == 0) return(DT::datatable(df, rownames = FALSE))
    vals <- unlist(df[num_cols])
    vals <- vals[!is.na(vals) & is.finite(vals)]
    mx <- max(1, if (length(vals) > 0) max(vals, na.rm = TRUE) else 1)
    nsteps <- min(6, max(3, ceiling(mx)))
    cuts <- unique(floor(seq(0, mx, length.out = nsteps + 1)))
    nv <- length(cuts) + 1
    # Brand palette: light purple -> purple, cap darkness for black text readability
    pal <- c("#ffffff", "#f3e5f5", "#e1bee7", "#ce93d8", "#ba68c8", "#ab47bc")
    colors <- pal[seq_len(min(nv, length(pal)))]
    if (nv > length(colors)) colors <- c(colors, rep(pal[length(pal)], nv - length(colors)))
    col_defs <- list(
      list(className = "dt-center", targets = seq_along(colnames(df)) - 1L)
    )
    dt <- DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE,
      columnDefs = col_defs), rownames = FALSE) %>%
      DT::formatStyle(columns = colnames(df), `text-align` = "center") %>%
      DT::formatStyle(columns = id_cols, `white-space` = "nowrap")
    for (col in num_cols) {
      dt <- dt %>% DT::formatStyle(col, backgroundColor = DT::styleInterval(cuts, colors), color = "black")
    }
    dt
  }
  output$overview_journey_response_tables <- renderUI({
    req(app_ui_ready())
    mats <- journey_response_matrices()
    if (length(mats) == 0) return(HTML("<p style='color: #5f6369;'>No series data available.</p>"))
    lens <- sort(as.numeric(names(mats)))
    labels <- c("1" = "1 workshop = 2 surveys", "2" = "2 workshops = 4 surveys", "3" = "3 workshops = 6 surveys",
      "4" = "4 workshops = 8 surveys", "5" = "5 workshops = 10 surveys", "6" = "6 workshops = 12 surveys")
    out <- list()
    for (len in lens) {
      out <- c(out, list(
        tags$h4(paste0("Series length ", len, " (", labels[as.character(len)], ")"), style = "color: #5c2f92; font-weight: bold; font-size: 18px; margin-top: 24px; margin-bottom: 12px;"),
        div(style = "max-width: 900px;", DT::dataTableOutput(paste0("overview_journey_len", len)))
      ))
    }
    do.call(tagList, out)
  })
  outputOptions(output, "overview_journey_response_tables", suspendWhenHidden = TRUE)
  lapply(1:6, function(l) {
    output_name <- paste0("overview_journey_len", l)
    output[[output_name]] <- DT::renderDataTable({
      req(app_ui_ready())
      mats <- journey_response_matrices()
      if (!as.character(l) %in% names(mats)) {
        return(DT::datatable(data.frame(Message = paste0("No series of length ", l, ".")), rownames = FALSE))
      }
      .render_journey_response_heatmap(mats[[as.character(l)]])
    })
    outputOptions(output, output_name, suspendWhenHidden = TRUE)
  })
  output$impact_session_table <- DT::renderDataTable({
    tryCatch({
    summary_data <- session_summary_data()
    if (nrow(summary_data) == 0) return(data.frame(Message = "No session data available."))
    display_cols <- list()
    if ("date" %in% colnames(summary_data)) {
      display_cols$Date <- tryCatch(format(as.Date(summary_data$date), "%B %d, %Y"), error = function(e) as.character(summary_data$date))
    }
    if ("org_name" %in% colnames(summary_data)) display_cols$Organization <- summary_data$org_name
    if ("group" %in% colnames(summary_data)) display_cols$Group <- summary_data$group
    if ("time_range" %in% colnames(summary_data)) display_cols$`Time Range` <- summary_data$time_range
    if ("facilitators" %in% colnames(summary_data)) display_cols$Facilitators <- summary_data$facilitators
    if ("pre_responses" %in% colnames(summary_data)) display_cols$`Pre` <- summary_data$pre_responses
    if ("post_responses" %in% colnames(summary_data)) display_cols$`Post` <- summary_data$post_responses
    if (length(display_cols) == 0) return(data.frame(Message = "No displayable columns."))
    df <- as.data.frame(display_cols)
    DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE, filter = "top")
    }, error = function(e) DT::datatable(data.frame(Error = paste("Error:", conditionMessage(e))), rownames = FALSE))
  })
  # Read a user-supplied histogram bin count, clamped to a sane range with a sensible default.
  # The index range is [-3, 3] (width 6); the default 24 bins matches a 0.25-wide bar.
  .read_index_bins <- function(id, default = 24L) {
    v <- tryCatch(input[[id]], error = function(e) NULL)
    if (is.null(v) || length(v) != 1L || is.na(v) || !is.finite(v)) return(default)
    as.integer(min(max(round(v), 4L), 60L))
  }
  output$impact_wellness_hist <- renderPlotly({
    tryCatch({
      big_pre_data <- filtered_big_pre()
      if (nrow(big_pre_data) == 0) return(plotly_empty())
      wellness <- calculate_wellness_index(big_pre_data)
      wellness <- wellness[!is.na(wellness)]
      if (length(wellness) == 0) return(plotly_empty())
      show_bars <- tryCatch(isTRUE(input$wellness_index_pre_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$wellness_index_pre_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      bf <- .wellness_bar_label_flags()
      want_lab <- bf$n || bf$pct
      total_n <- length(wellness)
      # Summary score is continuous over [-3, 3]; bin it like any histogram (bin count adjustable).
      bins <- .read_index_bins("wellness_index_pre_bins")
      br <- seq(-3, 3, length.out = bins + 1L)
      bw <- 6 / bins
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        h <- hist(wellness, breaks = br, plot = FALSE)
        txt <- if (want_lab) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
        has_txt <- !is.null(txt) && any(nzchar(txt))
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count",
          marker = list(color = PRE_INDEX_COLOR, opacity = 0.45, line = list(color = PRE_INDEX_COLOR, width = 1.3)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && total_n >= 2) {
        d <- density(wellness, from = -3, to = 3, n = 256)
        ycurve <- d$y * total_n * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = PRE_INDEX_COLOR, width = 2),
          fill = "tozeroy",
          fillcolor = PRE_INDEX_FILL_RGBA
        )
      }
      mean_shapes <- list()
      if (total_n >= 1 && is.finite(mean(wellness)) && (show_bars || show_curves)) {
        mean_shapes <- .mean_vline_outline_shapes(mean(wellness), ymax, PRE_INDEX_COLOR)
      }
      p %>%
        layout(
          title = list(text = "Financial Wellness Index (Pre)", font = PLOT_FONT),
          font = PLOT_FONT,
          barmode = "overlay",
          bargap = 0,
          xaxis = list(title = list(text = "Index (−3 to +3)", standoff = 14), range = c(-3, 3)),
          yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
          margin = list(t = 72, b = 100, l = 60, r = 24),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38, yanchor = "top"),
          shapes = mean_shapes,
          autosize = TRUE
        ) %>%
        config(displayModeBar = TRUE, responsive = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_hist <- renderPlotly({
    tryCatch({
      big_post_data <- filtered_big_post_only()
      if (nrow(big_post_data) == 0) {
        all_post <- filtered_big_post()
        empty_msg <- if (nrow(all_post) > 0) "No Big Post in filter" else "No data"
        return(plotly_empty() %>% layout(
          title = list(
            text = paste0("Post Impact Index<br><span style='font-size:11px;color:#5f6369;'>", empty_msg, "</span>"),
            font = PLOT_FONT
          ),
          margin = list(t = 56),
          font = PLOT_FONT
        ))
      }
      impact_idx <- calculate_post_impact_index(big_post_data)
      impact_idx <- impact_idx[!is.na(impact_idx)]
      if (length(impact_idx) == 0) {
        return(plotly_empty() %>% layout(
          title = list(
            text = "Post Impact Index<br><span style='font-size:11px;color:#5f6369;'>No Compared-to-before answers</span>",
            font = PLOT_FONT
          ),
          margin = list(t = 56),
          font = PLOT_FONT
        ))
      }
      show_bars <- tryCatch(isTRUE(input$wellness_index_post_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$wellness_index_post_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      bf <- .wellness_bar_label_flags()
      want_lab <- bf$n || bf$pct
      total_n <- length(impact_idx)
      # Summary score is continuous over [-3, 3]; bin it like any histogram (bin count adjustable).
      bins <- .read_index_bins("wellness_index_post_bins")
      br <- seq(-3, 3, length.out = bins + 1L)
      bw <- 6 / bins
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        h <- hist(impact_idx, breaks = br, plot = FALSE)
        txt <- if (want_lab) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
        has_txt <- !is.null(txt) && any(nzchar(txt))
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count",
          marker = list(color = POST_INDEX_COLOR, opacity = 0.45, line = list(color = POST_INDEX_COLOR, width = 1.3)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && total_n >= 2) {
        d <- density(impact_idx, from = -3, to = 3, n = 256)
        ycurve <- d$y * total_n * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = POST_INDEX_COLOR, width = 2),
          fill = "tozeroy",
          fillcolor = POST_INDEX_FILL_RGBA
        )
      }
      mean_shapes_post <- list()
      if (total_n >= 1 && is.finite(mean(impact_idx)) && (show_bars || show_curves)) {
        mean_shapes_post <- .mean_vline_outline_shapes(mean(impact_idx), ymax, POST_INDEX_COLOR)
      }
      p %>%
        layout(
          title = list(text = "Post-Workshop Impact Index", font = PLOT_FONT),
          font = PLOT_FONT,
          bargap = 0,
          barmode = "overlay",
          xaxis = list(title = list(text = "Index (−3 to +3)", standoff = 14), range = c(-3, 3)),
          yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
          margin = list(t = 72, b = 100, l = 60, r = 24),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38, yanchor = "top"),
          shapes = mean_shapes_post
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$impact_wellness_hist_summary <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(NULL)
    wellness <- calculate_wellness_index(big_pre_data)
    wellness <- wellness[is.finite(wellness)]
    if (!length(wellness)) return(NULL)
    deterministic_summary_box(
      scores = wellness, mode = "level",
      null_value = 0, positive_threshold = 0,
      scale_label = "(-3 to +3)",
      headline_label = "Above neutral (score above 0)",
      scope_text = scope_sentence(input),
      accent = PRE_INDEX_COLOR
    )
  })

  output$impact_post_hist_summary <- renderUI({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(NULL)
    impact_idx <- calculate_post_impact_index(big_post_data)
    impact_idx <- impact_idx[is.finite(impact_idx)]
    if (!length(impact_idx)) return(NULL)
    deterministic_summary_box(
      scores = impact_idx, mode = "level",
      null_value = 0, positive_threshold = 0,
      scale_label = "(-3 to +3)",
      headline_label = "Above neutral (score above 0)",
      scope_text = scope_sentence(input),
      accent = POST_INDEX_COLOR
    )
  })

  output$impact_wellness_variable_mapping <- DT::renderDataTable({
    tryCatch({
      # Questions on rows, surveys on columns (matches the Reach mapping template).
      mapping <- data.frame(
        Variable = c(
          "Better understanding of topics",
          "Awareness: how much money I have",
          "Awareness: what I can afford",
          "Awareness: where my money goes",
          "Financial future optimism",
          "Healthy relationship with money",
          "Managing financial stress",
          "Planning confidence",
          "Comfort with financial professionals"
        ),
        Big_Pre = c("No", rep("Yes", 8)),
        Little_Pre = rep("No", 9),
        Little_Post = rep("No", 9),
        Big_Post = rep("Yes", 9),
        Annual = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  # Universal y-axis headroom above the tallest bar, sized to the on-bar labels:
  # two-line labels (N AND %) need the most room, a single label needs some, none needs almost none.
  .label_headroom_factor <- function(show_n, show_pct) {
    if (isTRUE(show_n) && isTRUE(show_pct)) 1.32
    else if (isTRUE(show_n) || isTRUE(show_pct)) 1.16
    else 1.08
  }
  # Pre: 3 awareness histograms (know amount, afford, where)
  .hist_bar_text <- function(counts, total_n, show_n, show_pct) {
    if (!show_n && !show_pct) return(NULL)
    if (length(counts) == 0) return(NULL)
    vapply(seq_along(counts), function(i) {
      if (!is.finite(counts[i]) || counts[i] <= 0) return("")
      parts <- character(0)
      if (show_n) parts <- c(parts, as.character(as.integer(counts[i])))
      if (show_pct && total_n > 0) parts <- c(parts, sprintf("%.0f%%", counts[i] / total_n * 100))
      paste(parts, collapse = "\n")
    }, character(1))
  }
  .wellness_bar_label_flags <- function() {
    list(
      n = tryCatch(isTRUE(input$opt_show_n), error = function(e) FALSE),
      pct = tryCatch(isTRUE(input$opt_show_pct), error = function(e) FALSE)
    )
  }
  .behavioral_bar_label_flags <- function() {
    behavioral_bar_flags_d()
  }
  .satisfaction_bar_label_flags <- function() {
    satisfaction_bar_flags_d()
  }
  # Past/planned multi-select horizontal bars: N = count selecting behavior; % = share of respondents with any answer in that column
  .behavior_horiz_bar_labels <- function(counts, n_den, show_n, show_pct) {
    if (!show_n && !show_pct) return(NULL)
    if (length(counts) == 0) return(NULL)
    vapply(seq_along(counts), function(i) {
      if (!is.finite(counts[i]) || counts[i] < 0) return("")
      parts <- character(0)
      if (show_n) parts <- c(parts, as.character(as.integer(counts[i])))
      if (show_pct && n_den > 0) parts <- c(parts, sprintf("%.0f%%", counts[i] / n_den * 100))
      paste(parts, collapse = "\n")
    }, character(1))
  }
  # Compact Plotly title: short metric name; empty reason on a second line when needed.
  .plot_title <- function(title, empty_msg = NULL) {
    if (is.null(empty_msg) || !nzchar(empty_msg)) {
      return(list(text = title, font = PLOT_FONT))
    }
    list(
      text = paste0(title, "<br><span style='font-size:11px;color:#5f6369;'>", empty_msg, "</span>"),
      font = PLOT_FONT
    )
  }
  .render_likert_hist <- function(data, col, title, color = "#5c2f92", y_max_override = NULL, show_bar_n = FALSE, show_bar_pct = FALSE, big_post_only = FALSE) {
    if (nrow(data) == 0 || is.null(col) || !col %in% colnames(data)) {
      empty_msg <- if (isTRUE(big_post_only)) "No Big Post data" else "No data"
      return(plotly_empty() %>% layout(title = .plot_title(title, empty_msg), margin = list(t = 56), font = PLOT_FONT))
    }
    raw <- data[[col]]
    raw <- raw[!is.na(raw) & as.character(raw) != ""]
    if (length(raw) == 0) {
      empty_msg <- "No responses"
      if (isTRUE(big_post_only)) empty_msg <- "No Big Post answers"
      return(plotly_empty() %>% layout(title = .plot_title(title, empty_msg), margin = list(t = 56), font = PLOT_FONT))
    }
    tbl <- get_ordered_likert_table(data[[col]])
    counts <- as.numeric(tbl)
    if (length(counts) == 0 || sum(counts, na.rm = TRUE) == 0) {
      return(plotly_empty() %>% layout(title = .plot_title(title, "No responses"), margin = list(t = 56), font = PLOT_FONT))
    }
    base_max <- if (!is.null(y_max_override) && is.finite(y_max_override) && y_max_override > 0) y_max_override else max(counts, na.rm = TRUE)
    y_max <- base_max * .label_headroom_factor(show_bar_n, show_bar_pct)
    total_n <- sum(counts, na.rm = TRUE)
    txt <- .hist_bar_text(counts, total_n, show_bar_n, show_bar_pct)
    has_txt <- !is.null(txt) && any(nzchar(txt))
    plot_ly(
      x = names(tbl), y = counts, type = "bar", marker = list(color = color),
      text = if (has_txt) txt else NULL,
      textposition = if (has_txt) "outside" else NULL,
      cliponaxis = FALSE
    ) %>%
      layout(
        title = .plot_title(title),
        font = PLOT_FONT,
        margin = list(l = 55, r = 20, t = 50, b = 65),
        xaxis = list(title = "Response", categoryorder = "array",
          categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
        yaxis = list(title = "Count", range = c(0, y_max), rangemode = "nonnegative")
      )
  }
  output$impact_pre_know_amount_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- find_col(data, PRE_FINANCIAL_WELLNESS_COLS[1], exact = TRUE)
      if (is.null(col)) col <- grep("know how much money I have", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Know How Much Money I Have", "#5c2f92", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_know_afford_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- find_col(data, PRE_FINANCIAL_WELLNESS_COLS[2], exact = TRUE)
      if (is.null(col)) col <- grep("know what I can afford", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Know What I Can Afford", "#82c341", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_know_where_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- find_col(data, PRE_FINANCIAL_WELLNESS_COLS[3], exact = TRUE)
      if (is.null(col)) col <- grep("know where my money goes", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Know Where My Money Goes", "#f58220", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  # Pre: 5 remaining wellness histograms (dedicated for Impact tab)
  output$impact_pre_optimism_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- grep("optimistic.*financial future", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Financial Future Optimism", "#5c2f92", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_relationship_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- grep("healthy relationship.*money", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Healthy Relationship with Money", "#82c341", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_stress_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- grep("stressed.*financ|able to manage stress", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Financial Stress Management", "#f58220", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_confidence_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- grep("confident.*plan", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Planning Confidence", "#5c2f92", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_pre_comfort_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_pre()
      col <- grep("comfortable.*speaking.*financial professional", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Professional Comfort", "#82c341", NULL, bf$n, bf$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  # Reactive: shared y-max across all Post Likert hists (for Same Y axes checkbox)
  post_wellness_ymax <- reactive({
    if (!tryCatch(isTRUE(input$opt_same_y), error = function(e) FALSE)) return(NULL)
    data <- filtered_big_post_only()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    col1 <- grep("better understanding of the topics covered", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col2 <- find_col(data, POST_COMPARED_TO_COLS[2], exact = TRUE); if (is.null(col2)) col2 <- grep("aware.*how much money|how much money I have", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col3 <- find_col(data, POST_COMPARED_TO_COLS[3], exact = TRUE); if (is.null(col3)) col3 <- grep("aware.*what I can afford", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col4 <- find_col(data, POST_COMPARED_TO_COLS[4], exact = TRUE); if (is.null(col4)) col4 <- grep("aware.*where my money", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col5 <- grep("more optimistic about my financial future", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col6 <- grep("healthier relationship with money", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col7 <- grep("stress.*financ|able to manage stress|better able to manage stress", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col8 <- grep("more confident.*plan|confident planning ahead", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    col9 <- grep("more comfortable speaking with financial professionals", colnames(data), ignore.case = TRUE, value = TRUE)[1]
    post_col_patterns <- list(col1, col2, col3, col4, col5, col6, col7, col8, col9)
    max_ct <- 0
    for (col in post_col_patterns) {
      if (is.null(col) || !col %in% colnames(data)) next
      raw <- data[[col]][!is.na(data[[col]]) & as.character(data[[col]]) != ""]
      if (length(raw) == 0) next
      tbl <- tryCatch(get_ordered_likert_table(data[[col]]), error = function(e) NULL)
      if (!is.null(tbl)) {
        counts <- as.numeric(tbl)
        if (length(counts) > 0) max_ct <- max(max_ct, counts, na.rm = TRUE)
      }
    }
    if (max_ct <= 0) return(NULL)
    max_ct  # raw shared max; .render_likert_hist adds label headroom on top
  })
  # Post: 3 awareness histograms (match column - try multiple patterns)
  output$impact_post_awareness_amount_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- find_col(data, POST_COMPARED_TO_COLS[2], exact = TRUE)
      if (is.null(col)) col <- grep("aware.*how much money|how much money I have", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(col)) col <- grep("Compared to before", colnames(data), ignore.case = TRUE, value = TRUE)[2]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Awareness: Amount", REACH_PALETTE[1], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_post_awareness_afford_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- find_col(data, POST_COMPARED_TO_COLS[3], exact = TRUE)
      if (is.null(col)) col <- grep("aware.*what I can afford|what I can afford", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(col)) col <- grep("Compared to before", colnames(data), ignore.case = TRUE, value = TRUE)[3]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Awareness: Afford", REACH_PALETTE[2], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_post_awareness_where_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- find_col(data, POST_COMPARED_TO_COLS[4], exact = TRUE)
      if (is.null(col)) col <- grep("aware.*where my money|where my money goes", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(col)) col <- grep("Compared to before", colnames(data), ignore.case = TRUE, value = TRUE)[4]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Awareness: Where", REACH_PALETTE[3], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  # Post: understanding + 5 remaining wellness histograms (dedicated for Impact tab)
  output$impact_post_understanding_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("better understanding of the topics covered", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Understanding", REACH_PALETTE[1], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_optimism_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("more optimistic about my financial future", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Optimism", REACH_PALETTE[2], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_relationship_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("healthier relationship with money", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Relationship", REACH_PALETTE[3], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_stress_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("stress.*financ|able to manage stress|better able to manage stress", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Stress management", REACH_PALETTE[4], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_confidence_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("more confident.*plan|confident planning ahead", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Planning confidence", REACH_PALETTE[5], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_post_comfort_hist <- renderPlotly({
    tryCatch({
      data <- filtered_big_post_only()
      col <- grep("more comfortable speaking with financial professionals", colnames(data), ignore.case = TRUE, value = TRUE)[1]
      bf <- .wellness_bar_label_flags()
      .render_likert_hist(data, col, "Comfort w/ professionals", REACH_PALETTE[6], y_max_override = post_wellness_ymax(), show_bar_n = bf$n, show_bar_pct = bf$pct, big_post_only = TRUE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  format_p_value <- function(p) {
    if (is.na(p) || is.null(p)) return("—")
    if (p < 0.0001) return("< .0001")
    if (p < 0.001) return("< .001")
    return(as.character(round(p, 4)))
  }
  # Between-subjects: independent t-test + distribution figure
  output$impact_wellness_between_summary <- renderUI({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post_only()
      pre_idx <- calculate_wellness_index(big_pre)
      post_idx <- calculate_post_impact_index(big_post)
      pre_idx <- pre_idx[!is.na(pre_idx)]
      post_idx <- post_idx[!is.na(post_idx)]
      n_pre <- length(pre_idx)
      n_post <- length(post_idx)
      avg_pre <- if (n_pre > 0) round(mean(pre_idx), 3) else NA
      avg_post <- if (n_post > 0) round(mean(post_idx), 3) else NA
      sd_pre <- if (n_pre > 1) round(sd(pre_idx), 3) else NA
      sd_post <- if (n_post > 1) round(sd(post_idx), 3) else NA
      tt <- if (n_pre >= 2 && n_post >= 2) {
        tryCatch(t.test(pre_idx, post_idx, alternative = "two.sided", var.equal = FALSE), error = function(e) NULL)
      } else NULL
      t_stat <- if (!is.null(tt)) round(tt$statistic, 3) else NA
      p_val <- if (!is.null(tt)) format_p_value(tt$p.value) else "—"
      ci_lo <- if (!is.null(tt)) round(tt$conf.int[1], 3) else NA
      ci_hi <- if (!is.null(tt)) round(tt$conf.int[2], 3) else NA
      ttest_str <- if (!is.null(tt)) {
        paste0("<p class='summary-box-detail'><strong>Independent-samples t-test:</strong> t = ", t_stat, ", p = ", p_val,
               ", 95% CI for difference [", ci_lo, ", ", ci_hi, "]</p>")
      } else ""
      verdict <- if (!is.null(tt)) .signif_verdict(tt$p.value, (avg_post - avg_pre), "change") else list(text = "Not enough data for a significance test.", color = "#5f6369")
      verdict_str <- paste0("<p class='summary-box-detail' style='color:", verdict$color, "; font-weight:600;'>", verdict$text, "</p>")
      html <- paste0(
        "<div class='summary-box' style='border-left-color: ", PRE_INDEX_COLOR, ";'>",
        "<p class='summary-box-headline'><strong>Pre:</strong> M = ", if (is.na(avg_pre)) "—" else avg_pre, ", SD = ", if (is.na(sd_pre)) "—" else sd_pre, ", n = ", n_pre,
        " &nbsp;|&nbsp; <strong>Post:</strong> M = ", if (is.na(avg_post)) "—" else avg_post, ", SD = ", if (is.na(sd_post)) "—" else sd_post, ", n = ", n_post, "</p>",
        ttest_str,
        verdict_str,
        "<p class='summary-box-scope'>", scope_sentence(input), "</p>",
        "</div>"
      )
      HTML(html)
    }, error = function(e) HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
  })
  output$impact_wellness_between_dist <- renderPlotly({
    tryCatch({
      show_bars <- tryCatch(isTRUE(input$wellness_show_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$wellness_show_curves), error = function(e) TRUE)
      bf <- .wellness_bar_label_flags()
      want_lab <- bf$n || bf$pct
      bins <- .read_index_bins("wellness_between_bins")
      br <- seq(-3, 3, length.out = bins + 1L)
      bw <- 6 / bins
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post_only()
      pre_idx <- calculate_wellness_index(big_pre)
      post_idx <- calculate_post_impact_index(big_post)
      pre_idx <- pre_idx[!is.na(pre_idx)]
      post_idx <- post_idx[!is.na(post_idx)]
      if (length(pre_idx) == 0 && length(post_idx) == 0) return(plotly_empty() %>% layout(title = "No data"))
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        # Always overlay Pre/Post as translucent bars with a solid outline so both are visible
        # stacked on top of each other (side-by-side grouping was visually confusing).
        if (length(pre_idx) > 0) {
          hpre <- hist(pre_idx, breaks = br, plot = FALSE)
          txt_pre <- if (want_lab) .hist_bar_text(hpre$counts, length(pre_idx), bf$n, bf$pct) else NULL
          has_t <- !is.null(txt_pre) && any(nzchar(txt_pre))
          p <- p %>% add_trace(x = hpre$mids, y = hpre$counts, type = "bar", name = "Pre",
            marker = list(color = PRE_INDEX_COLOR, opacity = 0.45, line = list(color = PRE_INDEX_COLOR, width = 1.3)),
            text = if (has_t) txt_pre else NULL,
            textposition = if (has_t) "outside" else NULL,
            cliponaxis = FALSE)
        }
        if (length(post_idx) > 0) {
          hpost <- hist(post_idx, breaks = br, plot = FALSE)
          txt_post <- if (want_lab) .hist_bar_text(hpost$counts, length(post_idx), bf$n, bf$pct) else NULL
          has_t <- !is.null(txt_post) && any(nzchar(txt_post))
          p <- p %>% add_trace(x = hpost$mids, y = hpost$counts, type = "bar", name = "Post",
            marker = list(color = POST_INDEX_COLOR, opacity = 0.45, line = list(color = POST_INDEX_COLOR, width = 1.3)),
            text = if (has_t) txt_post else NULL,
            textposition = if (has_t) "outside" else NULL,
            cliponaxis = FALSE)
        }
      }
      if (show_curves) {
        if (length(pre_idx) >= 2) {
          dpre <- density(pre_idx, from = -3, to = 3, n = 100)
          scale_pre <- length(pre_idx) * bw
          p <- p %>% add_trace(x = dpre$x, y = dpre$y * scale_pre, type = "scatter", mode = "lines", name = "Pre (curve)", fill = "tozeroy",
            line = list(color = PRE_INDEX_COLOR, width = 2), fillcolor = PRE_INDEX_FILL_RGBA)
          ymax <- max(ymax, max(dpre$y * scale_pre, na.rm = TRUE))
        }
        if (length(post_idx) >= 2) {
          dpost <- density(post_idx, from = -3, to = 3, n = 100)
          scale_post <- length(post_idx) * bw
          p <- p %>% add_trace(x = dpost$x, y = dpost$y * scale_post, type = "scatter", mode = "lines", name = "Post (curve)", fill = "tozeroy",
            line = list(color = POST_INDEX_COLOR, width = 2), fillcolor = POST_INDEX_FILL_RGBA)
          ymax <- max(ymax, max(dpost$y * scale_post, na.rm = TRUE))
        }
      }
      if (show_bars) {
        hpre <- if (length(pre_idx) > 0) max(hist(pre_idx, breaks = br, plot = FALSE)$counts, 0) else 0
        hpost <- if (length(post_idx) > 0) max(hist(post_idx, breaks = br, plot = FALSE)$counts, 0) else 0
        ymax <- max(ymax, hpre, hpost)
      }
      if (ymax <= 0) ymax <- 1
      mean_shapes_w <- list()
      if (length(pre_idx) >= 1 && is.finite(mean(pre_idx)) && (show_curves || show_bars)) {
        mean_shapes_w <- c(mean_shapes_w, .mean_vline_outline_shapes(mean(pre_idx), ymax, PRE_INDEX_COLOR))
      }
      if (length(post_idx) >= 1 && is.finite(mean(post_idx)) && (show_curves || show_bars)) {
        mean_shapes_w <- c(mean_shapes_w, .mean_vline_outline_shapes(mean(post_idx), ymax, POST_INDEX_COLOR))
      }
      p %>% layout(barmode = "overlay", bargap = 0, font = PLOT_FONT,
        title = list(text = "Between-subjects: Pre vs Post Scores", x = 0.5, xref = "paper"),
        margin = list(t = 72, b = 110, l = 60, r = 24),
        xaxis = list(title = list(text = "Index (−3 to +3)", standoff = 16), range = c(-3, 3)),
        yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.42, yanchor = "top"),
        shapes = mean_shapes_w)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  # Within-subjects: paired t-test + slope graph (Pre→Post lines)
  output$impact_wellness_within_summary <- renderUI({
    tryCatch({
      paired <- .get_wellness_paired()
      if (is.null(paired) || nrow(paired) < 2) {
        return(deterministic_summary_box(pre = numeric(0), post = numeric(0), mode = "change",
                                         headline_label = "Mean wellness change (Post \u2212 Pre)",
                                         accent = POST_INDEX_COLOR, scope_text = scope_sentence(input)))
      }
      deterministic_summary_box(
        pre = paired$pre, post = paired$post, mode = "change",
        scale_label = "(\u22123 to +3)",
        headline_label = "Mean wellness change (Post \u2212 Pre)",
        accent = POST_INDEX_COLOR,
        scope_text = scope_sentence(input)
      )
    }, error = function(e) HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
  })
  output$impact_wellness_within_slopes <- renderPlotly({
    tryCatch({
      paired <- .get_wellness_paired()
      if (is.null(paired) || nrow(paired) == 0) {
        return(plotly_empty() %>% layout(title = "No paired respondents. respondent_id matching may be limited.", font = PLOT_FONT))
      }
      show_ind <- tryCatch(isTRUE(input$wellness_within_show_individual), error = function(e) TRUE)
      show_avg <- tryCatch(isTRUE(input$wellness_within_show_avg), error = function(e) TRUE)
      if (!show_ind && !show_avg) {
        return(plotly_empty() %>% layout(title = "Turn on Individual slopes and/or Average slope", font = PLOT_FONT))
      }
      n <- nrow(paired)
      p <- plot_ly()
      if (show_ind) {
        for (i in seq_len(n)) {
          p <- p %>% add_trace(
            x = c(0, 1), y = c(paired$pre[i], paired$post[i]),
            type = "scatter", mode = "lines+markers",
            line = list(color = "rgba(90, 90, 90, 0.4)", width = 2),
            marker = list(size = 8, color = c(PRE_INDEX_COLOR, POST_INDEX_COLOR), line = list(color = "white", width = 0.5)),
            showlegend = FALSE,
            hoverinfo = "text",
            hovertext = sprintf("Pre %.2f → Post %.2f", paired$pre[i], paired$post[i])
          )
        }
      }
      if (show_avg) {
        mpre <- mean(paired$pre)
        mpost <- mean(paired$post)
        p <- p %>% add_trace(
          x = c(0, 1), y = c(mpre, mpost),
          type = "scatter", mode = "lines+markers",
          line = list(color = MEAN_SLOPE_COLOR, width = 4),
          marker = list(size = 12, color = c(PRE_INDEX_COLOR, POST_INDEX_COLOR), line = list(color = "white", width = 0.5)),
          name = "Mean (Pre → Post)",
          showlegend = TRUE,
          hoverinfo = "text",
          hovertext = sprintf("Mean Pre %.3f → Post %.3f", mpre, mpost)
        )
      }
      p %>% layout(
        font = PLOT_FONT,
        title = paste0("Within-subject change: Pre → Post (n = ", n, ")"),
        margin = list(t = 70, b = 55, l = 60, r = 60),
        xaxis = list(title = "", tickvals = c(0, 1), ticktext = c("Pre", "Post"), range = c(-0.05, 1.05)),
        yaxis = list(title = "Score (−3 to +3)", range = c(-3, 3), dtick = 1),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")
      )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  # Pair on respondent_id only (any session in current filters). Does not require same session_id.
  .get_wellness_paired <- function() {
    big_pre <- filtered_big_pre()
    big_post <- filtered_big_post_only()
    if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(NULL)
    if (!"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(NULL)
    pre_idx <- calculate_wellness_index(big_pre)
    post_idx <- calculate_post_impact_index(big_post)
    pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = pre_idx, stringsAsFactors = FALSE)
    post_df <- data.frame(respondent_id = big_post$respondent_id, post = post_idx, stringsAsFactors = FALSE)
    valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON", ids, ignore.case = TRUE)
    pre_df <- pre_df[valid_id(pre_df$respondent_id), , drop = FALSE]
    post_df <- post_df[valid_id(post_df$respondent_id), , drop = FALSE]
    paired <- merge(pre_df, post_df, by = "respondent_id")
    paired <- paired[!is.na(paired$pre) & !is.na(paired$post), , drop = FALSE]
    if (nrow(paired) == 0) return(NULL)
    paired
  }
  output$impact_wellness_paired_change <- renderPlotly({
    tryCatch({
      paired <- .get_wellness_paired()
      if (is.null(paired)) return(plotly_empty() %>% layout(title = "respondent_id not available for pairing", font = PLOT_FONT))
      change_vals <- paired$post - paired$pre
      change_vals <- change_vals[is.finite(change_vals)]
      if (length(change_vals) == 0) {
        return(plotly_empty() %>% layout(title = "No paired respondents with valid pre/post scores.", font = PLOT_FONT))
      }
      show_bars <- tryCatch(isTRUE(input$wellness_change_show_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$wellness_change_show_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      n_breaks <- max(6L, min(24L, ceiling(2 * sqrt(length(change_vals)))))
      h <- hist(change_vals, breaks = n_breaks, plot = FALSE)
      bw <- if (length(h$breaks) >= 2) diff(h$breaks[1:2]) else 0.1
      p <- plot_ly()
      ymax <- 1
      bf <- .wellness_bar_label_flags()
      total_n <- length(change_vals)
      txt <- if (show_bars) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      div_cols <- .change_diverging_colors(h$mids)
      if (show_bars) {
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count", showlegend = FALSE,
          width = bw,
          marker = list(color = div_cols$fill,
                        line = list(color = div_cols$line_color, width = div_cols$line_width)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && length(change_vals) >= 2) {
        rng <- range(change_vals, na.rm = TRUE)
        pad <- if (diff(rng) > 1e-6) 0.04 * diff(rng) else 0.25
        d <- density(change_vals, n = 256, from = rng[1] - pad, to = rng[2] + pad)
        ycurve <- d$y * length(change_vals) * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = "#5b7c99", width = 2.5)
        )
      }
      ci_vis <- .paired_change_ci_shapes(change_vals, ymax)
      # Force 0 to the center: the larger absolute change sets both ends symmetrically.
      rx <- range(change_vals, na.rm = TRUE)
      max_abs <- max(abs(rx), na.rm = TRUE)
      padx <- if (max_abs > 1e-6) 0.05 * max_abs else 0.35
      lim <- max_abs + padx
      xrng <- c(-lim, lim)
      p %>% layout(
        title = list(
          text = paste0(
            "Change score distribution (n = ", length(change_vals), ")<br>",
            "<sup style='font-size:11px;color:#5f6369'>Dashed vertical = no change (0); shaded band = 95% CI of mean; double line = mean</sup>"
          ),
          font = PLOT_FONT
        ),
        barmode = "overlay",
        bargap = 0,
        xaxis = list(title = list(text = "Post index − Pre index", standoff = 12), range = xrng, zeroline = FALSE),
        yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
        font = PLOT_FONT,
        margin = list(t = 88, b = 70, l = 55, r = 25),
        showlegend = isTRUE(show_curves),
        legend = list(x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.72)", bordercolor = "rgba(0,0,0,0.12)", borderwidth = 1),
        shapes = ci_vis$shapes
      )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  # Between-subjects by item: independent t-test for each of 8 items, sorted by effect
  .wellness_item_pairs <- list(
    list(key = "awareness_amount", label = "Awareness: How much money I have", pre = PRE_FINANCIAL_WELLNESS_COLS[1], post = POST_COMPARED_TO_COLS[2]),
    list(key = "awareness_afford", label = "Awareness: What I can afford", pre = PRE_FINANCIAL_WELLNESS_COLS[2], post = POST_COMPARED_TO_COLS[3]),
    list(key = "awareness_where", label = "Awareness: Where my money goes", pre = PRE_FINANCIAL_WELLNESS_COLS[3], post = POST_COMPARED_TO_COLS[4]),
    list(key = "optimism", label = "Financial future optimism", pre = PRE_FINANCIAL_WELLNESS_COLS[4], post = POST_COMPARED_TO_COLS[5]),
    list(key = "relationship", label = "Healthy relationship with money", pre = PRE_FINANCIAL_WELLNESS_COLS[5], post = POST_COMPARED_TO_COLS[6]),
    list(key = "stress", label = "Managing financial stress", pre = PRE_FINANCIAL_WELLNESS_COLS[6], post = POST_COMPARED_TO_COLS[7]),
    list(key = "confidence", label = "Planning confidence", pre = PRE_FINANCIAL_WELLNESS_COLS[7], post = POST_COMPARED_TO_COLS[8]),
    list(key = "comfort_professionals", label = "Comfort with financial professionals", pre = PRE_FINANCIAL_WELLNESS_COLS[8], post = POST_COMPARED_TO_COLS[9])
  )
  output$impact_wellness_item_between_plot <- renderPlotly({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
      results <- list()
      for (pair in .wellness_item_pairs) {
        pre_col <- find_col(big_pre, pair$pre, exact = TRUE)
        if (is.null(pre_col)) pre_col <- grep(gsub("\\[.*\\]", ".*", pair$pre), colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
        post_col <- find_col(big_post, pair$post, exact = TRUE)
        if (is.null(post_col)) post_col <- grep(gsub("\\[.*\\]", ".*", pair$post), colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
        if (is.null(pre_col) || is.null(post_col) || !pre_col %in% colnames(big_pre) || !post_col %in% colnames(big_post)) next
        pre_vals <- likert_to_numeric(big_pre[[pre_col]])
        post_vals <- likert_to_numeric(big_post[[post_col]])
        pre_vals <- pre_vals[!is.na(pre_vals)]
        post_vals <- post_vals[!is.na(post_vals)]
        if (length(pre_vals) < 2 || length(post_vals) < 2) next
        tt <- tryCatch(t.test(pre_vals, post_vals, alternative = "two.sided", var.equal = FALSE), error = function(e) NULL)
        if (is.null(tt)) next
        mean_diff <- mean(post_vals) - mean(pre_vals)
        ci <- tt$conf.int
        results[[length(results) + 1L]] <- data.frame(
          item = pair$label,
          mean_diff = mean_diff,
          ci_lo = -ci[2],
          ci_hi = -ci[1],
          n_pre = length(pre_vals),
          n_post = length(post_vals),
          stringsAsFactors = FALSE
        )
      }
      if (length(results) == 0) return(plotly_empty() %>% layout(title = "No item data available", font = PLOT_FONT))
      df <- do.call(rbind, results)
      df <- df[order(-df$mean_diff), ]
      df$item <- factor(df$item, levels = rev(df$item))
      colors <- REACH_PALETTE[(seq_len(nrow(df)) - 1L) %% length(REACH_PALETTE) + 1L]
      bf <- .wellness_bar_label_flags()
      bar_txt <- rep("", nrow(df))
      for (i in seq_len(nrow(df))) {
        parts <- character(0)
        if (bf$n) parts <- c(parts, paste0("n ", df$n_pre[i], "/", df$n_post[i]))
        if (bf$pct) parts <- c(parts, sprintf("%+.0f%%", df$mean_diff[i] / 4 * 100))
        bar_txt[i] <- paste(parts, collapse = "\n")
      }
      has_bar_txt <- (bf$n || bf$pct) && any(nzchar(bar_txt))
      plot_ly(df, x = ~mean_diff, y = ~item, type = "bar", orientation = "h",
        marker = list(color = colors),
        error_x = list(type = "data", array = df$ci_hi - df$mean_diff, arrayminus = df$mean_diff - df$ci_lo, thickness = 1),
        text = if (has_bar_txt) bar_txt else NULL,
        textposition = if (has_bar_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        layout(
          font = PLOT_FONT,
          title = "Between-subjects: Mean difference (Post − Pre) per item",
          xaxis = list(title = "Mean difference (Post − Pre)", zeroline = TRUE, zerolinewidth = 1),
          yaxis = list(title = ""),
          margin = list(l = 200),
          showlegend = FALSE
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_wellness_item_between_table <- DT::renderDataTable({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE))
      results <- list()
      for (pair in .wellness_item_pairs) {
        pre_col <- find_col(big_pre, pair$pre, exact = TRUE)
        if (is.null(pre_col)) pre_col <- grep(gsub("\\[.*\\]", ".*", pair$pre), colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
        post_col <- find_col(big_post, pair$post, exact = TRUE)
        if (is.null(post_col)) post_col <- grep(gsub("\\[.*\\]", ".*", pair$post), colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
        if (is.null(pre_col) || is.null(post_col) || !pre_col %in% colnames(big_pre) || !post_col %in% colnames(big_post)) next
        pre_vals <- likert_to_numeric(big_pre[[pre_col]])
        post_vals <- likert_to_numeric(big_post[[post_col]])
        pre_vals <- pre_vals[!is.na(pre_vals)]
        post_vals <- post_vals[!is.na(post_vals)]
        if (length(pre_vals) < 2 || length(post_vals) < 2) next
        tt <- tryCatch(t.test(pre_vals, post_vals, alternative = "two.sided", var.equal = FALSE), error = function(e) NULL)
        if (is.null(tt)) next
        mean_diff <- mean(post_vals) - mean(pre_vals)
        ci_diff <- c(-tt$conf.int[2], -tt$conf.int[1])
        results[[length(results) + 1L]] <- data.frame(
          Item = pair$label,
          `Pre M` = round(mean(pre_vals), 2),
          `Pre SD` = round(sd(pre_vals), 2),
          `Pre n` = length(pre_vals),
          `Post M` = round(mean(post_vals), 2),
          `Post SD` = round(sd(post_vals), 2),
          `Post n` = length(post_vals),
          `Difference` = round(mean_diff, 3),
          t = round(-tt$statistic, 2),
          p = format_p_value(tt$p.value),
          `95% CI` = paste0("[", round(ci_diff[1], 2), ", ", round(ci_diff[2], 2), "]"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      if (length(results) == 0) return(DT::datatable(data.frame(Message = "No item data"), rownames = FALSE))
      df <- do.call(rbind, results)
      df <- df[order(-df$Difference), ]
      DT::datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$impact_wellness_item_within_plot <- renderPlotly({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
      if (!"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(plotly_empty() %>% layout(title = "respondent_id required for pairing", font = PLOT_FONT))
      valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON#", ids)
      results <- list()
      for (pair in .wellness_item_pairs) {
        pre_col <- find_col(big_pre, pair$pre, exact = TRUE)
        if (is.null(pre_col)) pre_col <- grep(gsub("\\[.*\\]", ".*", pair$pre), colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
        post_col <- find_col(big_post, pair$post, exact = TRUE)
        if (is.null(post_col)) post_col <- grep(gsub("\\[.*\\]", ".*", pair$post), colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
        if (is.null(pre_col) || is.null(post_col) || !pre_col %in% colnames(big_pre) || !post_col %in% colnames(big_post)) next
        pre_num <- likert_to_numeric(big_pre[[pre_col]])
        post_num <- likert_to_numeric(big_post[[post_col]])
        pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = pre_num, stringsAsFactors = FALSE)
        post_df <- data.frame(respondent_id = big_post$respondent_id, post = post_num, stringsAsFactors = FALSE)
        pre_df <- pre_df[valid_id(pre_df$respondent_id) & !is.na(pre_df$pre), , drop = FALSE]
        post_df <- post_df[valid_id(post_df$respondent_id) & !is.na(post_df$post), , drop = FALSE]
        merged <- merge(pre_df, post_df, by = "respondent_id")
        merged <- merged[!is.na(merged$pre) & !is.na(merged$post), , drop = FALSE]
        if (nrow(merged) < 2) next
        diff <- merged$post - merged$pre
        tt <- tryCatch(t.test(diff, mu = 0, alternative = "two.sided"), error = function(e) NULL)
        if (is.null(tt)) next
        results[[length(results) + 1L]] <- data.frame(
          item = pair$label,
          mean_change = mean(diff),
          sd_change = sd(diff),
          n = nrow(merged),
          t_stat = tt$statistic,
          p_value = tt$p.value,
          ci_lo = tt$conf.int[1],
          ci_hi = tt$conf.int[2],
          stringsAsFactors = FALSE
        )
      }
      if (length(results) == 0) return(plotly_empty() %>% layout(title = "No paired item data available", font = PLOT_FONT))
      df <- do.call(rbind, results)
      df <- df[order(-df$mean_change), ]
      df$item <- factor(df$item, levels = rev(df$item))
      colors <- REACH_PALETTE[(seq_len(nrow(df)) - 1L) %% length(REACH_PALETTE) + 1L]
      bf <- .wellness_bar_label_flags()
      bar_txt <- rep("", nrow(df))
      for (i in seq_len(nrow(df))) {
        parts <- character(0)
        if (bf$n) parts <- c(parts, paste0("n=", df$n[i]))
        if (bf$pct) parts <- c(parts, sprintf("%+.0f%%", df$mean_change[i] / 4 * 100))
        bar_txt[i] <- paste(parts, collapse = "\n")
      }
      has_bar_txt <- (bf$n || bf$pct) && any(nzchar(bar_txt))
      plot_ly(df, x = ~mean_change, y = ~item, type = "bar", orientation = "h",
        marker = list(color = colors),
        error_x = list(type = "data", array = df$mean_change - df$ci_lo, arrayminus = df$ci_hi - df$mean_change, thickness = 1),
        text = if (has_bar_txt) bar_txt else NULL,
        textposition = if (has_bar_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        layout(
          font = PLOT_FONT,
          title = "Within-subjects: Mean change (Post − Pre) per item",
          xaxis = list(title = "Mean change (Post − Pre)", zeroline = TRUE, zerolinewidth = 1),
          yaxis = list(title = ""),
          margin = list(l = 200),
          showlegend = FALSE
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_wellness_item_within_table <- DT::renderDataTable({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE))
      if (!"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(DT::datatable(data.frame(Message = "respondent_id required"), rownames = FALSE))
      valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON#", ids)
      results <- list()
      for (pair in .wellness_item_pairs) {
        pre_col <- find_col(big_pre, pair$pre, exact = TRUE)
        if (is.null(pre_col)) pre_col <- grep(gsub("\\[.*\\]", ".*", pair$pre), colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
        post_col <- find_col(big_post, pair$post, exact = TRUE)
        if (is.null(post_col)) post_col <- grep(gsub("\\[.*\\]", ".*", pair$post), colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
        if (is.null(pre_col) || is.null(post_col) || !pre_col %in% colnames(big_pre) || !post_col %in% colnames(big_post)) next
        pre_num <- likert_to_numeric(big_pre[[pre_col]])
        post_num <- likert_to_numeric(big_post[[post_col]])
        pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = pre_num, stringsAsFactors = FALSE)
        post_df <- data.frame(respondent_id = big_post$respondent_id, post = post_num, stringsAsFactors = FALSE)
        pre_df <- pre_df[valid_id(pre_df$respondent_id) & !is.na(pre_df$pre), , drop = FALSE]
        post_df <- post_df[valid_id(post_df$respondent_id) & !is.na(post_df$post), , drop = FALSE]
        merged <- merge(pre_df, post_df, by = "respondent_id")
        merged <- merged[!is.na(merged$pre) & !is.na(merged$post), , drop = FALSE]
        if (nrow(merged) < 2) next
        diff <- merged$post - merged$pre
        tt <- tryCatch(t.test(diff, mu = 0, alternative = "two.sided"), error = function(e) NULL)
        if (is.null(tt)) next
        results[[length(results) + 1L]] <- data.frame(
          Item = pair$label,
          n = nrow(merged),
          `Mean change` = round(mean(diff), 3),
          SD = round(sd(diff), 3),
          t = round(tt$statistic, 2),
          p = format_p_value(tt$p.value),
          `95% CI` = paste0("[", round(tt$conf.int[1], 2), ", ", round(tt$conf.int[2], 2), "]"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      if (length(results) == 0) return(DT::datatable(data.frame(Message = "No paired item data"), rownames = FALSE))
      df <- do.call(rbind, results)
      df <- df[order(-df$`Mean change`), ]
      DT::datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })

  # Overview: Master submission timeline (dual Y, day/week/month) + Program Manager series timeline
  output$overview_master_weekly_filter_note <- renderUI({
    pre_n <- tryCatch(nrow(filtered_pre()), error = function(e) NA_integer_)
    post_n <- tryCatch(nrow(filtered_post()), error = function(e) NA_integer_)
    if (!is.finite(pre_n) || !is.finite(post_n)) return(NULL)
    p(
      style = "font-size: 12px; color: #5f6369; margin: 4px 0 8px 0;",
      paste0("Filtered rows: Pre ", pre_n, " | Post ", post_n)
    )
  })

  output$overview_master_weekly_line <- renderPlotly({
    tryCatch({
      unit <- if (is.null(input$overview_master_time_unit)) "weeks" else input$overview_master_time_unit
      show_resp <- tryCatch(isTRUE(input$overview_submissions_show_responses), error = function(e) TRUE)
      show_work <- tryCatch(isTRUE(input$overview_submissions_show_workshops), error = function(e) TRUE)
      res <- build_master_submission_timeline(filtered_pre(), filtered_post(), unit)
      df <- res$df
      if (nrow(df) == 0) {
        return(plotly_empty() %>% layout(title = "No Master Pre/Post data loaded.", font = PLOT_FONT))
      }
      if (max(df$pre_n, na.rm = TRUE) == 0 && max(df$post_n, na.rm = TRUE) == 0 && max(df$workshops_n, na.rm = TRUE) == 0) {
        return(plotly_empty() %>% layout(
          title = "No rows with a parseable timestamp (check Master column timestamp).",
          font = PLOT_FONT
        ))
      }
      if (!show_resp && !show_work) {
        return(plotly_empty() %>% layout(title = "Turn on Responses and/or Workshops", font = PLOT_FONT))
      }
      y_left <- switch(unit,
        days = "Responses per day",
        weeks = "Responses per week",
        months = "Responses per month",
        "Responses"
      )
      y_right <- switch(unit,
        days = "Distinct workshops / day",
        weeks = "Distinct workshops / week",
        months = "Distinct workshops / month",
        "Distinct workshops"
      )
      x_title <- paste0("Time (", res$period_label, ")")
      # Pad date range so lines are not flush against plot edges
      d_min <- as.Date(min(df$period, na.rm = TRUE))
      d_max <- as.Date(max(df$period, na.rm = TRUE))
      span_days <- as.numeric(d_max - d_min)
      pad <- if (!is.finite(span_days) || span_days <= 0) {
        7L
      } else {
        as.integer(max(7L, ceiling(span_days * 0.04)))
      }
      xa <- list(
        title = list(text = x_title, standoff = 18),
        type = "date",
        range = c(as.character(d_min - pad), as.character(d_max + pad)),
        automargin = TRUE
      )
      if (length(res$x_breaks) > 0L) {
        xa$tickmode <- "array"
        xa$tickvals <- res$x_breaks
        xa$ticktext <- res$x_labels
      }
      vline_dates <- overview_master_submission_vline_dates(df$period, unit)
      vshapes <- overview_plotly_vline_shapes(vline_dates)
      p <- plot_ly()
      if (show_resp) {
        p <- p %>% add_trace(
          data = df, x = ~period, y = ~pre_n, name = "Pre responses", type = "scatter", mode = "lines+markers",
          line = list(color = "#5c2f92", width = 2), marker = list(color = "#5c2f92", size = 7),
          yaxis = "y", hovertemplate = "<b>Pre</b>: %{y}<extra></extra>"
        ) %>% add_trace(
          data = df, x = ~period, y = ~post_n, name = "Post responses", type = "scatter", mode = "lines+markers",
          line = list(color = "#82c341", width = 2), marker = list(color = "#82c341", size = 7),
          yaxis = "y", hovertemplate = "<b>Post</b>: %{y}<extra></extra>"
        )
      }
      if (show_work) {
        y_w <- if (isTRUE(show_resp)) "y2" else "y"
        p <- p %>% add_trace(
          data = df, x = ~period, y = ~workshops_n, name = "Workshops (distinct session_id)",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#f58220", width = 2, dash = "dot"),
          marker = list(color = "#f58220", size = 6),
          yaxis = y_w, hovertemplate = "<b>Workshops</b>: %{y}<extra></extra>"
        )
      }
      # Legend below x-axis with enough bottom margin so dates stay readable
      leg <- list(
        orientation = "h",
        y = -0.28,
        x = 0.5,
        xanchor = "center",
        yanchor = "top",
        font = list(size = 12)
      )
      marg <- list(l = 85, r = 70, b = 130, t = 24, pad = 4)
      if (isTRUE(show_resp) && isTRUE(show_work)) {
        plotly::layout(p,
          font = PLOT_FONT,
          hovermode = "x unified",
          xaxis = xa,
          legend = leg,
          shapes = vshapes,
          margin = marg,
          yaxis = list(title = y_left, side = "left", rangemode = "tozero", automargin = TRUE),
          yaxis2 = list(
            title = y_right,
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            rangemode = "tozero",
            automargin = TRUE
          )
        )
      } else if (isTRUE(show_resp)) {
        plotly::layout(p,
          font = PLOT_FONT,
          hovermode = "x unified",
          xaxis = xa,
          legend = leg,
          shapes = vshapes,
          margin = marg,
          yaxis = list(title = y_left, side = "left", rangemode = "tozero", automargin = TRUE)
        )
      } else {
        plotly::layout(p,
          font = PLOT_FONT,
          hovermode = "x unified",
          xaxis = xa,
          legend = leg,
          shapes = vshapes,
          margin = marg,
          yaxis = list(title = y_right, side = "left", rangemode = "tozero", automargin = TRUE)
        )
      }
    }, error = function(e) {
      plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT)
    })
  })

  dq_dt <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    }
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, dom = "ftip"))
  }

  output$dq_summary <- renderUI({
    r <- dq_report()
    req(r)
    HTML(r$summary_html)
  })

  output$dq_cohort_pre <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cohort_pre)
  })

  output$dq_cohort_post <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cohort_post)
  })

  output$dq_ts_fail_pre <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$ts_fail_pre)
  })

  output$dq_ts_fail_post <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$ts_fail_post)
  })

  output$dq_fill_pre <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cohort_fill_pre)
  })

  output$dq_fill_post <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cohort_fill_post)
  })

  output$dq_expected_pre <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$expected_pre)
  })

  output$dq_expected_post <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$expected_post)
  })

  output$dq_cols_pre <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cols_pre)
  })

  output$dq_cols_post <- DT::renderDataTable({
    req(dq_report())
    dq_dt(dq_report()$cols_post)
  })

  # Shared builder for PM Gantt so UI height matches plot content
  overview_pm_gantt_built <- reactive({
    unit <- if (is.null(input$overview_pm_time_unit)) "weeks" else input$overview_pm_time_unit
    sort_by <- if (is.null(input$overview_pm_sort)) "first_asc" else input$overview_pm_sort
    row_grain <- if (isTRUE(input$overview_pm_rows_by_org)) "org" else "group"
    res <- build_program_manager_timeline(program_manager(), unit, sort_by, row_grain = row_grain)
    if (!is.null(res$msg)) return(list(error = res$msg, h_px = 320L, n_series = 0L, res = NULL))
    # Date window: sidebar Filter by date range when enabled; otherwise full PM schedule
    use_date <- tryCatch(isTRUE(input$use_date_filter), error = function(e) FALSE)
    if (isTRUE(use_date)) {
      dr <- tryCatch(input$date_range, error = function(e) NULL)
      if (!is.null(dr) && length(dr) >= 2) {
        rs <- tryCatch(as.Date(dr[1]), error = function(e) NA)
        re <- tryCatch(as.Date(dr[2]), error = function(e) NA)
        if (!is.na(rs) && !is.na(re)) {
          res <- overview_clip_pm_timeline_window(res, rs, re)
        }
      }
    }
    pdat <- res$df
    if (is.null(pdat) || nrow(pdat) == 0) {
      return(list(error = "No Program Manager rows to plot.", h_px = 320L, n_series = 0L, res = NULL))
    }
    n_series <- if (is.factor(pdat$series)) nlevels(pdat$series) else length(unique(as.character(pdat$series)))
    # Tighter rows; keep room for x-axis labels
    h_px <- as.integer(max(360L, 24L * max(1L, n_series) + 140L))
    list(error = NULL, h_px = h_px, n_series = n_series, res = res)
  })

  output$overview_program_manager_gantt_ui <- renderUI({
    built <- tryCatch(overview_pm_gantt_built(), error = function(e) NULL)
    h <- if (!is.null(built) && is.finite(built$h_px)) built$h_px else 420L
    plotlyOutput("overview_program_manager_gantt", height = paste0(h, "px"))
  })

  output$overview_program_manager_gantt <- renderPlotly({
    tryCatch({
      built <- overview_pm_gantt_built()
      if (!is.null(built$error)) {
        return(plotly_empty() %>% layout(title = built$error, font = PLOT_FONT))
      }
      res <- built$res
      pdat <- res$df
      h_px <- built$h_px
      n_series <- built$n_series

      max_n <- max(pdat$n, na.rm = TRUE)
      if (!is.finite(max_n) || max_n < 0) max_n <- 0L
      max_n <- as.integer(max_n)
      fill_breaks <- if (max_n <= 20L) {
        seq.int(0L, max_n)
      } else {
        unique(as.integer(round(seq(0, max_n, length.out = min(12L, max_n + 1L)))))
      }

      p <- ggplot(pdat, aes(x = period, y = series, fill = n))
      if (!is.null(res$weekend_rect) && nrow(res$weekend_rect) > 0) {
        p <- p + ggplot2::geom_rect(
          data = res$weekend_rect,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          fill = "#e4e4e4",
          alpha = 0.35,
          inherit.aes = FALSE
        )
      }
      vln_days <- if (!is.null(res$sat_sun_boundary_vlines) && nrow(res$sat_sun_boundary_vlines) > 0) {
        res$sat_sun_boundary_vlines
      } else if (!is.null(res$weekend_vlines) && nrow(res$weekend_vlines) > 0) {
        res$weekend_vlines
      } else NULL
      if (!is.null(vln_days) && nrow(vln_days) > 0) {
        p <- p + ggplot2::geom_vline(
          data = vln_days,
          ggplot2::aes(xintercept = x),
          color = "grey42",
          linewidth = 0.45,
          inherit.aes = FALSE
        )
      }
      if (!is.null(res$month_boundary_vlines) && nrow(res$month_boundary_vlines) > 0) {
        p <- p + ggplot2::geom_vline(
          data = res$month_boundary_vlines,
          ggplot2::aes(xintercept = x),
          color = "grey35",
          linewidth = 0.55,
          inherit.aes = FALSE
        )
      }
      p <- p +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_x_date(
          limits = as.Date(res$x_limits),
          breaks = res$x_breaks,
          labels = res$x_labels,
          expand = ggplot2::expansion(mult = c(0.02, 0.02), add = c(0.5, 0.5))
        ) +
        ggplot2::scale_fill_gradient(
          low = "#efe8f5",
          high = "#5c2f92",
          limits = c(0, max(1L, max_n)),
          breaks = fill_breaks,
          labels = function(x) format(as.integer(round(x)), scientific = FALSE, trim = TRUE),
          name = "Workshops\nin bin",
          na.value = "#efe8f5"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.margin = ggplot2::margin(4, 8, 4, 4)
        ) +
        ggplot2::labs(x = paste0("Time (", res$period_label, ")"), y = NULL)

      ggplotly(p, height = h_px, tooltip = c("x", "y", "fill")) %>%
        layout(
          font = PLOT_FONT,
          margin = list(l = 200, r = 90, b = 100, t = 20, pad = 2),
          xaxis = list(
            title = list(text = paste0("Time (", res$period_label, ")"), standoff = 12),
            automargin = TRUE,
            showticklabels = TRUE,
            tickangle = -45,
            rangeslider = list(visible = FALSE)
          ),
          yaxis = list(automargin = TRUE, side = "left"),
          legend = list(orientation = "v", y = 1, yanchor = "top")
        )
    }, error = function(e) {
      plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT)
    })
  })

  # Debounced bundle of summary inputs to keep stats tables from re-rendering
  # every time the sidebar updates choices during initial data load.
  summary_stats_bundle <- reactive({
    req(data_ready())
    .n_base_sessions <- function(d) {
      if (is.null(d) || nrow(d) == 0 || !"session_id" %in% names(d)) return(0L)
      s <- trimws(as.character(d$session_id))
      s <- s[!is.na(s) & nzchar(s)]
      if (!length(s)) return(0L)
      length(unique(base_session_id(s)))
    }
    list(
      summary_data = tryCatch(session_summary_data(), error = function(e) data.frame()),
      pre_n = tryCatch(nrow(filtered_pre()), error = function(e) NA_integer_),
      post_n = tryCatch(nrow(filtered_post()), error = function(e) NA_integer_),
      big_pre_n = tryCatch(nrow(filtered_big_pre()), error = function(e) NA_integer_),
      little_pre_n = tryCatch(nrow(filtered_little_pre()), error = function(e) NA_integer_),
      little_post_n = tryCatch(nrow(filtered_little_post()), error = function(e) NA_integer_),
      big_post_n = tryCatch(nrow(filtered_big_post_only()), error = function(e) NA_integer_),
      annual_n = tryCatch(nrow(filtered_annual()), error = function(e) NA_integer_),
      big_pre_sessions = tryCatch(.n_base_sessions(filtered_big_pre()), error = function(e) 0L),
      little_pre_sessions = tryCatch(.n_base_sessions(filtered_little_pre()), error = function(e) 0L),
      little_post_sessions = tryCatch(.n_base_sessions(filtered_little_post()), error = function(e) 0L),
      big_post_sessions = tryCatch(.n_base_sessions(filtered_big_post_only()), error = function(e) 0L),
      lang_sel = tryCatch(input$filter_language, error = function(e) "All")
    )
  }) %>% debounce(350)

  # Summary Statistics table removed from Overview — survey-type table is the sole Response Descriptives view.

  output$impact_overview_survey_type_stats_table <- renderTable({
    bundle <- summary_stats_bundle()
    fmt_int <- function(x) formatC(round(as.numeric(x)), format = "d", big.mark = ",")
    fmt_avg <- function(resp, sess) {
      if (!is.finite(sess) || sess <= 0) return("-")
      formatC(round(as.numeric(resp) / as.numeric(sess), 1), format = "f", digits = 1)
    }
    bp <- if (is.finite(bundle$big_pre_n)) bundle$big_pre_n else 0L
    lp <- if (is.finite(bundle$little_pre_n)) bundle$little_pre_n else 0L
    lpo <- if (is.finite(bundle$little_post_n)) bundle$little_post_n else 0L
    bpo <- if (is.finite(bundle$big_post_n)) bundle$big_post_n else 0L
    ann <- if (is.finite(bundle$annual_n)) bundle$annual_n else 0L
    bp_s <- if (is.finite(bundle$big_pre_sessions)) bundle$big_pre_sessions else 0L
    lp_s <- if (is.finite(bundle$little_pre_sessions)) bundle$little_pre_sessions else 0L
    lpo_s <- if (is.finite(bundle$little_post_sessions)) bundle$little_post_sessions else 0L
    bpo_s <- if (is.finite(bundle$big_post_sessions)) bundle$big_post_sessions else 0L
    # Distinct workshops across typed Pre/Post (base_session_id); used for Sessions row.
    all_bids <- character(0)
    for (getter in list(filtered_big_pre, filtered_little_pre, filtered_little_post, filtered_big_post_only)) {
      d <- tryCatch(getter(), error = function(e) data.frame())
      if (nrow(d) > 0 && "session_id" %in% names(d)) {
        s <- trimws(as.character(d$session_id))
        s <- s[!is.na(s) & nzchar(s)]
        if (length(s)) all_bids <- c(all_bids, base_session_id(s))
      }
    }
    sess_total_sessions <- length(unique(all_bids))
    sess_total_resp <- bp + lp + lpo + bpo
    grand_resp <- sess_total_resp + ann
    out <- data.frame(
      Metric = c("Sessions", "Responses", "Average responses per session"),
      `Big Pre` = c(fmt_int(bp_s), fmt_int(bp), fmt_avg(bp, bp_s)),
      `Little Pre` = c(fmt_int(lp_s), fmt_int(lp), fmt_avg(lp, lp_s)),
      `Little Post` = c(fmt_int(lpo_s), fmt_int(lpo), fmt_avg(lpo, lpo_s)),
      `Big Post` = c(fmt_int(bpo_s), fmt_int(bpo), fmt_avg(bpo, bpo_s)),
      `Session Total` = c(
        fmt_int(sess_total_sessions),
        fmt_int(sess_total_resp),
        fmt_avg(sess_total_resp, sess_total_sessions)
      ),
      `Annual Survey` = c("-", fmt_int(ann), "-"),
      Total = c(
        "-",
        fmt_int(grand_resp),
        "-"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(out)[1] <- "Metric"
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  # (no fixed align= — knitr errors when align length ≠ ncol; defaults are fine)

  # Summary statistics output (Overview tab — At a Glance HTML block).
  output$session_summary_stats <- renderUI({
    tryCatch({
      bundle <- summary_stats_bundle()
      summary_data <- bundle$summary_data
      if (is.null(summary_data) || nrow(summary_data) == 0) {
        return(HTML("<p>No session data for the current filters. Check that Pre and Post sheets have a <strong>Session Link</strong> column and refresh.</p>"))
      }
      stats <- calculate_session_stats(summary_data)
      pre_n <- if (is.finite(bundle$pre_n)) bundle$pre_n else 0L
      post_n <- if (is.finite(bundle$post_n)) bundle$post_n else 0L
      big_pre_n <- if (is.finite(bundle$big_pre_n)) bundle$big_pre_n else 0L
      data_line <- paste0("Data: Pre ", pre_n, " | Post ", post_n, " | Sessions ", nrow(summary_data), " | Big Pre ", big_pre_n)
      html <- paste0(
        "<div style='width: fit-content; max-width: 420px;'>",
        "<p style='font-size: 11px; color: #5f6369; margin-bottom: 8px;'>", data_line, "</p>",
        "<table style='width: 100%; border-collapse: collapse; margin: 20px 0;'>",
        "<tr style='background-color: #5c2f92; color: #ffffff;'>",
        "<th style='padding: 10px; text-align: left; border: 1px solid #4a2673;'>Metric</th>",
        "<th style='padding: 10px; text-align: right; border: 1px solid #4a2673;'>Value</th>",
        "</tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Total Sessions</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", stats$total_sessions, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Total Pre-Survey Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", pre_n, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Total Post-Survey Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", post_n, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Average Pre Responses per Session</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", stats$avg_pre_per_session, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Average Post Responses per Session</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", stats$avg_post_per_session, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Sessions with Pre Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", stats$sessions_with_pre, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Sessions with Post Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", stats$sessions_with_post, "</td></tr>",
        "</table></div>"
      )
      
      return(HTML(html))
    }, error = function(e) {
      return(HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
    })
  })

  output$organization_summary_table <- DT::renderDataTable({
    org_summary <- organization_summary_data()
    if (nrow(org_summary) == 0) {
      return(data.frame(Message = "No organization series available for the selected filters."))
    }

    DT::datatable(
      org_summary,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(1, 'desc'))
      ),
      rownames = FALSE
    )
  })

  output$filtered_summary_stats_table <- renderTable({
    disp <- session_summary_display_data()
    if (is.null(disp) || nrow(disp) == 0) {
      return(data.frame(Message = "No session data to display. Adjust filters or load data."))
    }
    stats <- calculate_session_stats(disp)
    data.frame(
      Metric = c(
        "Sessions in current table",
        "Pre-survey responses (filtered)",
        "Post-survey responses (filtered)",
        "Average pre responses per session",
        "Average post responses per session",
        "Sessions with pre responses",
        "Sessions with post responses"
      ),
      Value = c(
        stats$total_sessions,
        stats$total_pre_responses,
        stats$total_post_responses,
        stats$avg_pre_per_session,
        stats$avg_post_per_session,
        stats$sessions_with_pre,
        stats$sessions_with_post
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Session summary table
  output$session_summary_table <- DT::renderDataTable({
    tryCatch({
      summary_data <- session_summary_data()
      if (nrow(summary_data) == 0) {
        return(data.frame(Message = "No session data available for the selected filters."))
      }
      
      # Check which columns exist and build display data
      display_cols <- list()
      
      if ("date" %in% colnames(summary_data)) {
        display_cols$Date <- summary_data$date
        # Format date
        display_cols$Date <- tryCatch(
          format(as.Date(display_cols$Date), "%B %d, %Y"),
          error = function(e) as.character(display_cols$Date)
        )
      }
      
      if ("org_name" %in% colnames(summary_data)) {
        display_cols$Organization <- summary_data$org_name
      }
      
      if ("group" %in% colnames(summary_data)) {
        display_cols$Group <- summary_data$group
      }
      
      if ("time_range" %in% colnames(summary_data)) {
        display_cols$`Time Range` <- summary_data$time_range
      }
      
      if ("facilitators" %in% colnames(summary_data)) {
        display_cols$Facilitators <- summary_data$facilitators
      }
      
      if ("modules_taught" %in% colnames(summary_data)) {
        display_cols$Modules <- summary_data$modules_taught
      }
      
      if ("session_label" %in% colnames(summary_data)) {
        display_cols$`Session Info` <- summary_data$session_label
      }
      
      if ("pre_responses" %in% colnames(summary_data)) {
        display_cols$`Pre Responses` <- summary_data$pre_responses
      }
      
      if ("post_responses" %in% colnames(summary_data)) {
        display_cols$`Post Responses` <- summary_data$post_responses
      }
      
      if ("total_responses" %in% colnames(summary_data)) {
        display_cols$`Total Responses` <- summary_data$total_responses
      }
      
      if (length(display_cols) == 0) {
        return(data.frame(Message = "No displayable columns found in session data."))
      }
      
      display_data <- as.data.frame(display_cols, stringsAsFactors = FALSE)
      
      # Sort by date if available
      if ("Date" %in% colnames(display_data)) {
        display_data <- display_data[order(display_data$Date, decreasing = TRUE), ]
      }
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = if ("Date" %in% colnames(display_data)) { list(list(0, 'desc')) } else { NULL }
        ),
        rownames = FALSE,
        filter = 'top'
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(display_data),
          backgroundColor = '#ffffff',
          color = '#333333'
        )
    }, error = function(e) {
      return(data.frame(Error = paste("Error creating table:", as.character(e$message))))
    })
  })
  
  # ========================================================================
  # Big Pre Tab
  # ========================================================================
  
  output$big_pre_summary_stats <- renderUI({
    tryCatch({
      big_pre_data <- filtered_big_pre()
      
      has_valid_id <- function(ids) {
        !is.na(ids) & ids != "" & !grepl("^ANON#", ids)
      }
      
      total <- nrow(big_pre_data)
      with_id <- sum(has_valid_id(big_pre_data$respondent_id), na.rm = TRUE)
      without_id <- total - with_id
      
      html <- paste0(
        "<table style='width:100%; border-collapse: collapse; margin: 20px 0;'>",
        "<tr style='background-color: #5c2f92; color: #ffffff;'>",
        "<th style='padding: 10px; text-align: left; border: 1px solid #4a2673;'>Metric</th>",
        "<th style='padding: 10px; text-align: right; border: 1px solid #4a2673;'>Count</th>",
        "</tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Total Big Pre Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", total, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6; padding-left: 20px;'>With user_id</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", with_id, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6; padding-left: 20px;'>Without user_id</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", without_id, "</td></tr>",
        "</table>"
      )
      
      return(HTML(html))
    }, error = function(e) {
      return(HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
    })
  })
  
  # Variable Mapping Table
  output$big_pre_variable_mapping_table <- renderUI({
    tryCatch({
      mapping <- get_variable_mapping_table()
      
      html <- paste0(
        "<p style='margin: 10px 0; color: #5f6369;'>",
        "The pre-survey contains <strong>17 metadata variables</strong> (timestamp through respondent_email), ",
        "<strong>3 common questions</strong> shared by Big Pre and Little Pre (intentions, curiosities, hoped feelings), ",
        "and <strong>Big Pre only</strong>: ",
        "<strong>8 Financial Wellness</strong> (Likert scale), ",
        "<strong>1 Past Behaviors</strong> (comma-separated multi-select), ",
        "<strong>11 Demographics</strong> (including zip, neurodivergent), ",
        "and <strong>support/feedback</strong> questions.",
        "</p>",
        "<table style='width:100%; border-collapse: collapse; margin: 20px 0; font-size: 12px;'>",
        "<tr style='background-color: #5c2f92; color: #ffffff;'>",
        "<th style='padding: 8px; text-align: left; border: 1px solid #4a2673;'>Question Text</th>",
        "<th style='padding: 8px; text-align: left; border: 1px solid #4a2673;'>Category</th>",
        "<th style='padding: 8px; text-align: left; border: 1px solid #4a2673;'>Response Type</th>",
        "<th style='padding: 8px; text-align: left; border: 1px solid #4a2673;'>Source / Computation</th>",
        "</tr>"
      )
      
      for (i in 1:nrow(mapping)) {
        q_text <- mapping$Question_Text[i]
        if (nchar(q_text) > 60) q_text <- paste0(substr(q_text, 1, 57), "...")
        desc <- if ("Description" %in% colnames(mapping)) mapping$Description[i] else ""
        html <- paste0(html,
          "<tr>",
          "<td style='padding: 6px; border: 1px solid #dee2e6;'>", q_text, "</td>",
          "<td style='padding: 6px; border: 1px solid #dee2e6;'>", mapping$Gross_Category[i], "</td>",
          "<td style='padding: 6px; border: 1px solid #dee2e6;'>", mapping$Response_Type[i], "</td>",
          "<td style='padding: 6px; border: 1px solid #dee2e6;'>", desc, "</td>",
          "</tr>"
        )
      }
      
      html <- paste0(html, "</table>")
      
      return(HTML(html))
    }, error = function(e) {
      return(HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
    })
  })
  
  # Financial Wellness Visualizations
  output$big_pre_optimism_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("optimistic.*financial future", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_pre_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Financial Future Optimism", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_relationship_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("healthy relationship.*money", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_pre_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Healthy Relationship with Money", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_stress_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("stressed.*financ|able to manage stress", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_pre_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#f58220")) %>%
      layout(title = "Financial Stress*", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_wellness_index_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    # Check if group_by is active
    if (input$group_by_demo != "None" && input$group_by_demo != "---" && input$group_by_demo != "---2") {
      split_data <- split_data_by_group(big_pre_data)
      if (length(split_data) == 0) return(plotly_empty())
      
      # Create subplots for each group
      plots <- list()
      for (group_name in names(split_data)) {
        group_data <- split_data[[group_name]]
        if (nrow(group_data) == 0) next
        
        wellness <- calculate_wellness_index(group_data)
        wellness <- wellness[!is.na(wellness)]
        if (length(wellness) == 0) next
        
        # Create plot with clear title - titles will be preserved in subplot
        p <- plot_ly(x = wellness, type = "histogram", nbinsx = 20,
                     marker = list(color = "#5c2f92"), showlegend = FALSE,
                     name = group_name) %>%
          layout(
            title = list(
              text = paste0("<b>", group_name, "</b>"),
              font = list(size = 14, color = "#5c2f92"),
              x = 0.5
            ),
            xaxis = list(title = "Wellness Index (−3 to +3)", range = c(-3, 3)),
            yaxis = list(title = "Count"),
            margin = list(t = 70, b = 50, l = 60, r = 20)  # Add margins for titles
          )
        plots[[group_name]] <- p
      }
      
      if (length(plots) == 0) return(plotly_empty())
      
      # Create subplot with individual titles preserved
      n_plots <- length(plots)
      n_cols <- min(2, n_plots)
      n_rows <- ceiling(n_plots / n_cols)
      
      # Get group by label for main title
      group_label <- if (input$group_by_demo %in% c("Wellness_Optimism", "Wellness_Relationship", 
                                                     "Wellness_Stress", "Wellness_Confidence", "Wellness_Comfort")) {
        gsub("Wellness_", "", input$group_by_demo)
      } else {
        input$group_by_demo
      }
      
      # Create subplot - individual plot titles should be visible
      p <- subplot(plots, nrows = n_rows, shareX = TRUE, shareY = TRUE) %>%
        layout(
          title = list(
            text = paste0("<b>Financial Wellness Index Distribution</b><br><sub>Grouped by: ", group_label, "</sub>"),
            x = 0.5,
            font = list(size = 16),
            y = 0.98
          ),
          margin = list(t = 100)  # Extra top margin for main title
        )
      
      return(p)
    } else {
      # No grouping - single plot
      wellness <- calculate_wellness_index(big_pre_data)
      wellness <- wellness[!is.na(wellness)]
      
      if (length(wellness) == 0) return(plotly_empty())
      
      p <- plot_ly(x = wellness, type = "histogram", nbinsx = 20,
                   marker = list(color = "#5c2f92")) %>%
        layout(title = "Financial Wellness Index Distribution", 
               xaxis = list(title = "Wellness Index (−3 to +3)", range = c(-3, 3)),
               yaxis = list(title = "Count"))
      return(p)
    }
  })
  
  output$big_pre_wellness_radar <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    # Calculate averages for all 5 dimensions
    optimism_col <- grep("optimistic.*financial future", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    relationship_col <- grep("healthy relationship.*money", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    stress_col <- grep("stressed.*financ|able to manage stress", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    confidence_col <- grep("confident.*plan", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    
    if (length(optimism_col) == 0 || length(relationship_col) == 0 || length(stress_col) == 0 ||
        length(confidence_col) == 0 || length(comfort_col) == 0) {
      return(plotly_empty())
    }
    
    optimism_vals <- likert_to_numeric(big_pre_data[[optimism_col[1]]])
    relationship_vals <- likert_to_numeric(big_pre_data[[relationship_col[1]]])
    stress_vals <- likert_to_numeric(big_pre_data[[stress_col[1]]])
    confidence_vals <- likert_to_numeric(big_pre_data[[confidence_col[1]]])
    comfort_vals <- likert_to_numeric(big_pre_data[[comfort_col[1]]])
    # Feb 2026: "able to manage stress" is positive framing; old "stressed" was negative
    stress_reversed <- if (grepl("able to manage", stress_col[1], ignore.case = TRUE)) stress_vals else (-stress_vals)
    
    avg_optimism <- mean(optimism_vals, na.rm = TRUE)
    avg_relationship <- mean(relationship_vals, na.rm = TRUE)
    avg_stress_rev <- mean(stress_reversed, na.rm = TRUE)
    avg_confidence <- mean(confidence_vals, na.rm = TRUE)
    avg_comfort <- mean(comfort_vals, na.rm = TRUE)
    
    p <- plot_ly(
      type = 'scatterpolar',
      r = c(avg_optimism, avg_relationship, avg_stress_rev, avg_confidence, avg_comfort, avg_optimism),
      theta = c('Optimism', 'Money Relationship', 'Low Stress*', 'Planning Confidence', 'Professional Comfort', 'Optimism'),
      fill = 'toself',
      marker = list(color = "#5c2f92")
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(-3, 3)
          )
        ),
        title = "Average Financial Wellness Profile (8 Questions)"
      )
    
    return(p)
  })
  
  # Self-Efficacy Visualizations
  output$big_pre_confidence_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("confident.*plan", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_pre_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Planning Confidence", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_comfort_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("comfortable.*speaking.*financial professional", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_pre_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Professional Comfort", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  # Behavioral Visualizations (Feb 2026: single comma-separated column; old: 8 separate columns)
  output$big_pre_behaviors_heatmap <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    behavior_cols <- grep("Before today's workshop, I have", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(behavior_cols) == 0) return(plotly_empty())
    
    # Feb 2026: single comma-separated column -> horizontal bar chart
    if (length(behavior_cols) == 1L && exists("get_past_behaviors_counts")) {
      counts_df <- get_past_behaviors_counts(big_pre_data)
      if (nrow(counts_df) == 0) return(plotly_empty())
      vec <- big_pre_data[[behavior_cols[1]]]
      n_den <- sum(!is.na(vec) & nzchar(trimws(as.character(vec))))
      bf <- .behavioral_bar_label_flags()
      htxt <- .behavior_horiz_bar_labels(counts_df$count, n_den, bf$n, bf$pct)
      has_txt <- !is.null(htxt) && any(nzchar(htxt))
      # Use multiple brand colors and horizontal orientation for readability
      colors <- REACH_PALETTE[seq_len(nrow(counts_df)) %% length(REACH_PALETTE) + 1]
      hovertext <- paste0(
        counts_df$behavior, ": ", counts_df$count,
        if (n_den > 0) paste0(" (", sprintf("%.0f%%", counts_df$count / n_den * 100), " of ", n_den, " with an answer)") else ""
      )
      p <- plot_ly(
        counts_df,
        x = ~count,
        y = ~reorder(behavior, count),
        type = "bar",
        orientation = "h",
        marker = list(color = colors),
        text = if (has_txt) htxt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE,
        hoverinfo = "text",
        hovertext = hovertext
      ) %>%
        layout(
          title = "Past Behaviors (counts)",
          xaxis = list(title = "Count"),
          yaxis = list(title = ""),
          margin = list(l = 200, r = 40)
        )
      return(p)
    }
    
    # Create matrix: behaviors × responses (No/Maybe/Yes) - old format
    behavior_names <- gsub("Before today's workshop, I have\\.\\.\\. \\[(.*)\\]", "\\1", behavior_cols)
    behavior_names <- gsub("^\\s+|\\s+$", "", behavior_names)  # Trim whitespace
    
    response_types <- c("No", "Maybe", "Yes")  # Ordered left to right
    heatmap_data <- matrix(0, nrow = length(behavior_cols), ncol = length(response_types))
    rownames(heatmap_data) <- behavior_names
    colnames(heatmap_data) <- response_types
    
    for (i in 1:length(behavior_cols)) {
      counts <- table(big_pre_data[[behavior_cols[i]]], useNA = "no")
      for (j in 1:length(response_types)) {
        if (response_types[j] %in% names(counts)) {
          heatmap_data[i, j] <- counts[response_types[j]]
        }
      }
    }
    
    # Order rows by Yes count (descending), then Maybe count (descending)
    yes_counts <- heatmap_data[, "Yes"]
    maybe_counts <- heatmap_data[, "Maybe"]
    row_order <- order(-yes_counts, -maybe_counts)
    heatmap_data <- heatmap_data[row_order, , drop = FALSE]
    
    # Create corrplot-style visualization with circles in a grid
    # Use heatmap but with custom styling for corrplot look
    # For now, use regular heatmap with proper ordering - corrplot circles are complex in plotly
    # We'll use a heatmap with better color scale to show intensity
    p <- plot_ly(
      x = colnames(heatmap_data),
      y = rownames(heatmap_data),
      z = heatmap_data,
      type = "heatmap",
      colorscale = list(
        c(0, "#f8f9fa"),
        c(0.25, "#e0e0e0"),
        c(0.5, "#b0b0b0"),
        c(0.75, "#7a7a7a"),
        c(1, "#5c2f92")
      ),
      text = matrix(paste0("Count: ", as.vector(heatmap_data)), 
                    nrow = nrow(heatmap_data), ncol = ncol(heatmap_data)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Financial Behaviors Heatmap",
        xaxis = list(
          title = "Response",
          categoryorder = "array",
          categoryarray = response_types
        ),
        yaxis = list(
          title = "Behavior"
        )
      )
    
    return(p)
  })
  output$impact_behavioral_variable_mapping <- DT::renderDataTable({
    tryCatch({
      # Questions on rows, surveys on columns (matches the Reach mapping template).
      mapping <- data.frame(
        Variable = c(
          "Past financial behaviors (multi-select) \u2192 Behavioral Index",
          "Planned / completed actions after workshop (multi-select) \u2192 Planned Actions Index",
          "As a result of my workshop(s)... (Annual multi-select) \u2192 Annual Behavior Index"
        ),
        Big_Pre = c("Yes", "No", "No"),
        Little_Pre = c("No", "No", "No"),
        Little_Post = c("No", "No", "No"),
        Big_Post = c("No", "Yes", "No"),
        Annual = c("No", "No", "Yes"),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$impact_satisfaction_variable_mapping <- DT::renderDataTable({
    tryCatch({
      # Questions on rows, surveys on columns (matches the Reach mapping template).
      mapping <- data.frame(
        Variable = c(
          "Session satisfaction",
          "Likelihood to recommend 5 Buckets (NPS)"
        ),
        Big_Pre = c("No", "No"),
        Little_Pre = c("No", "No"),
        Little_Post = c("Yes", "No"),
        Big_Post = c("Yes", "Yes"),
        Annual = c("No", "Yes"),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$impact_learning_variable_mapping <- DT::renderDataTable({
    tryCatch({
      mapping <- data.frame(
        Variable = c(
          "Topic understanding (Compared to before... better understanding)",
          "Learning interests / keep in touch (multi-select)",
          "Learn-more topics (open / multi)",
          "Wordcloud / open text — Pre opening & comments",
          "Wordcloud / open text — Post reflection & impact story",
          "Wordcloud / open text — Annual takeaway / goal / impact story"
        ),
        Big_Pre = c("No", "No", "No", "Yes", "No", "No"),
        Little_Pre = c("No", "No", "No", "No", "No", "No"),
        Little_Post = c("No", "No", "No", "No", "No", "No"),
        Big_Post = c("Yes", "Yes", "Yes", "No", "Yes", "No"),
        Annual = c("No", "Yes", "Yes", "No", "No", "Yes"),
        Notes = c(
          "Big Post Likert; Annual has related Compared-to-before items under Wellness",
          "Big Post keep-in-touch; Annual keep-in-touch when present",
          "Big Post + Annual learn-more wording",
          "PRE_OPENING_COLS / PRE_ADDITIONAL_COMMENTS_COL",
          "POST_TODAY_SESSION_COLS / POST_IMPACT_STORY_COL",
          "Annual takeaway / proud goal / impact story (tables on Annual hub)"
        ),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$impact_behavioral_pre_hist <- renderPlotly({
    tryCatch({
      big_pre_data <- filtered_big_pre()
      if (nrow(big_pre_data) == 0) return(plotly_empty())
      behavioral_index <- calculate_behavioral_index(big_pre_data)
      behavioral_index <- behavioral_index[!is.na(behavioral_index)]
      if (length(behavioral_index) == 0) return(plotly_empty())
      show_bars <- tryCatch(isTRUE(input$behavioral_index_pre_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$behavioral_index_pre_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      bf <- .behavioral_bar_label_flags()
      h <- hist(behavioral_index, breaks = seq(-0.5, 8.5, 1), plot = FALSE)
      bw <- 1
      total_n <- length(behavioral_index)
      txt <- if (show_bars) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count",
          marker = list(color = PRE_INDEX_COLOR, opacity = 0.45, line = list(color = PRE_INDEX_COLOR, width = 1.3)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && length(behavioral_index) >= 2) {
        d <- density(behavioral_index, from = 0, to = 8, n = 256)
        ycurve <- d$y * length(behavioral_index) * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = PRE_INDEX_COLOR, width = 2),
          fill = "tozeroy",
          fillcolor = PRE_INDEX_FILL_RGBA
        )
      }
      mean_shapes_bh <- list()
      if (length(behavioral_index) >= 1 && is.finite(mean(behavioral_index)) && (show_bars || show_curves)) {
        mean_shapes_bh <- .mean_vline_outline_shapes(mean(behavioral_index), ymax, PRE_INDEX_COLOR)
      }
      p %>%
        layout(
          title = list(text = "Behavioral Index (Pre)", font = PLOT_FONT),
          font = PLOT_FONT,
          barmode = "overlay",
          bargap = 0,
          xaxis = list(title = list(text = "Index (0–8, count of behaviors)", standoff = 14), dtick = 1, range = c(-0.5, 8.5)),
          yaxis = list(title = "Respondents", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
          margin = list(t = 72, b = 100, l = 60, r = 24),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38, yanchor = "top"),
          shapes = mean_shapes_bh
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_behavioral_post_hist <- renderPlotly({
    tryCatch({
      big_post_data <- filtered_big_post()
      if (nrow(big_post_data) == 0) return(plotly_empty())
      actions_index <- calculate_planned_actions_index(big_post_data)
      actions_index <- actions_index[!is.na(actions_index)]
      if (length(actions_index) == 0) return(plotly_empty())
      if (is.matrix(actions_index) || is.array(actions_index)) actions_index <- as.vector(actions_index)
      show_bars <- tryCatch(isTRUE(input$behavioral_index_post_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$behavioral_index_post_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      bf <- .behavioral_bar_label_flags()
      h <- hist(actions_index, breaks = seq(-0.5, 8.5, 1), plot = FALSE)
      bw <- 1
      total_n <- length(actions_index)
      txt <- if (show_bars) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count",
          marker = list(color = POST_INDEX_COLOR, opacity = 0.45, line = list(color = POST_INDEX_COLOR, width = 1.3)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && length(actions_index) >= 2) {
        d <- density(actions_index, from = 0, to = 8, n = 256)
        ycurve <- d$y * length(actions_index) * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = POST_INDEX_COLOR, width = 2),
          fill = "tozeroy",
          fillcolor = POST_INDEX_FILL_RGBA
        )
      }
      mean_shapes_ph <- list()
      if (length(actions_index) >= 1 && is.finite(mean(actions_index)) && (show_bars || show_curves)) {
        mean_shapes_ph <- .mean_vline_outline_shapes(mean(actions_index), ymax, POST_INDEX_COLOR)
      }
      p %>%
        layout(
          title = list(text = "Planned Actions Index (Post)", font = PLOT_FONT),
          font = PLOT_FONT,
          barmode = "overlay",
          bargap = 0,
          xaxis = list(title = list(text = "Index (0–8, count of planned actions)", standoff = 14), dtick = 1, range = c(-0.5, 8.5)),
          yaxis = list(title = "Respondents", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
          margin = list(t = 72, b = 100, l = 60, r = 24),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38, yanchor = "top"),
          shapes = mean_shapes_ph
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$impact_behavioral_pre_hist_summary <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(NULL)
    bi <- calculate_behavioral_index(big_pre_data)
    bi <- bi[is.finite(bi)]
    if (!length(bi)) return(NULL)
    deterministic_summary_box(
      scores = bi, mode = "level",
      null_value = 4, positive_threshold = 3,
      scale_label = "(0-8 behaviors)",
      headline_label = "Reported 4 or more past behaviors",
      scope_text = scope_sentence(input),
      accent = PRE_INDEX_COLOR
    )
  })

  output$impact_behavioral_post_hist_summary <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(NULL)
    ai <- calculate_planned_actions_index(big_post_data)
    if (is.matrix(ai) || is.array(ai)) ai <- as.vector(ai)
    ai <- ai[is.finite(ai)]
    if (!length(ai)) return(NULL)
    deterministic_summary_box(
      scores = ai, mode = "level",
      null_value = 4, positive_threshold = 3,
      scale_label = "(0-8 planned actions)",
      headline_label = "Planning 4 or more actions",
      scope_text = scope_sentence(input),
      accent = POST_INDEX_COLOR
    )
  })

  output$big_pre_behavioral_index_hist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    behavioral_index <- calculate_behavioral_index(big_pre_data)
    behavioral_index <- behavioral_index[!is.na(behavioral_index)]
    
    if (length(behavioral_index) == 0) return(plotly_empty())
    
    p <- plot_ly(x = behavioral_index, type = "histogram", nbinsx = 9,
                 marker = list(color = PRE_INDEX_COLOR)) %>%
      layout(title = "Behavioral Index Distribution (0-8)", 
             xaxis = list(title = "Number of 'Yes' Responses"),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_action_behaviors <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    # Action-oriented behaviors: Q23-Q26 (goals, tracking, budgeting, saving)
    action_cols <- grep("Before today's workshop, I have", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(action_cols) < 4) return(plotly_empty())
    
    action_cols <- action_cols[1:4]  # First 4 are action-oriented
    behavior_names <- gsub("Before today's workshop, I have\\.\\.\\. \\[(.*)\\]", "\\1", action_cols)
    behavior_names <- gsub("^\\s+|\\s+$", "", behavior_names)
    
    yes_counts <- sapply(action_cols, function(col) {
      sum(big_pre_data[[col]] == "Yes", na.rm = TRUE)
    })
    
    p <- plot_ly(x = behavior_names, y = yes_counts, type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(
        title = "Action-Oriented Behaviors (Yes Counts)",
        xaxis = list(
          title = "Behavior", 
          tickangle = -45,
          automargin = TRUE,
          margin = list(b = 100)
        ),
        yaxis = list(
          title = "Count",
          automargin = TRUE
        ),
        margin = list(b = 120, l = 80, r = 20, t = 60)
      )
    return(p)
  })
  
  output$big_pre_awareness_social_behaviors <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    behavior_cols <- grep("Before today's workshop, I have", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(behavior_cols) < 8) return(plotly_empty())
    
    # Awareness: Q27, Q30 (credit, reflection)
    # Social: Q28, Q29 (sharing, talking)
    awareness_cols <- behavior_cols[c(5, 8)]  # Credit check, Reflection
    social_cols <- behavior_cols[c(6, 7)]  # Sharing, Talking
    
    awareness_names <- c("Credit Check", "Reflection")
    social_names <- c("Sharing Tips", "Talking About Money")
    
    awareness_yes <- sapply(awareness_cols, function(col) {
      sum(big_pre_data[[col]] == "Yes", na.rm = TRUE)
    })
    social_yes <- sapply(social_cols, function(col) {
      sum(big_pre_data[[col]] == "Yes", na.rm = TRUE)
    })
    
    p <- plot_ly() %>%
      add_trace(x = awareness_names, y = awareness_yes, type = "bar", name = "Awareness",
                marker = list(color = "#82c341")) %>%
      add_trace(x = social_names, y = social_yes, type = "bar", name = "Socialization",
                marker = list(color = "#f58220")) %>%
      layout(
        title = "Awareness & Social Behaviors (Yes Counts)",
        xaxis = list(
          title = "Behavior", 
          tickangle = -45,
          automargin = TRUE,
          margin = list(b = 100)
        ),
        yaxis = list(
          title = "Count",
          automargin = TRUE
        ),
        barmode = "group",
        margin = list(b = 120, l = 80, r = 20, t = 60)
      )
    return(p)
  })
  
  # Demographic Visualizations
  output$big_pre_age_dist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    if (!"Age Group" %in% colnames(big_pre_data)) return(plotly_empty())
    
    age_data <- big_pre_data$`Age Group`
    age_data <- age_data[!is.na(age_data)]
    
    # Define logical order
    age_order <- AGE_LEVELS_CANONICAL
    # Get unique values and order them
    unique_ages <- unique(age_data)
    # Try to match to order, put unmatched at end
    ordered_ages <- c(intersect(age_order, unique_ages), setdiff(unique_ages, age_order))
    
    age_data <- factor(age_data, levels = ordered_ages, ordered = TRUE)
    data <- table(age_data)
    
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Age Distribution", 
             xaxis = list(title = "Age Group", categoryorder = "array", categoryarray = names(data)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_gender_dist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("Gender Identity", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- table(big_pre_data[[col[1]]], useNA = "no")
    p <- plot_ly(labels = names(data), values = as.numeric(data), type = "pie",
                 marker = list(colors = c("#5c2f92", "#82c341", "#f58220"))) %>%
      layout(title = "Gender Distribution")
    return(p)
  })
  
  output$big_pre_income_dist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("Household Income", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    income_data <- big_pre_data[[col[1]]]
    income_data <- income_data[!is.na(income_data)]
    
    # Define logical order (low to high)
    income_order <- c(
      "If one person under $36k; two people under $41k; family of four under $52k",
      "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
      "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
      "If one person over $97k; two people over $111k; family of four over $139k"
    )
    
    # Get unique values and order them
    unique_incomes <- unique(income_data)
    ordered_incomes <- c(intersect(income_order, unique_incomes), setdiff(unique_incomes, income_order))
    
    income_data <- factor(income_data, levels = ordered_incomes, ordered = TRUE)
    data <- table(income_data)
    
    # Shorten long labels for display
    labels <- names(data)
    labels <- gsub("If one person \\$([0-9]+k)-\\$([0-9]+k);.*", "\\$\\1k-\\$\\2k", labels)
    labels <- gsub("If one person over \\$([0-9]+k);.*", "Over \\$\\1k", labels)
    labels <- gsub("If one person under \\$([0-9]+k);.*", "Under \\$\\1k", labels)
    
    p <- plot_ly(x = labels, y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Household Income Distribution", 
             xaxis = list(title = "Income Range", tickangle = -45, categoryorder = "array", categoryarray = labels),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_education_dist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("highest level of education", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    edu_data <- .normalize_education_for_dashboard(big_pre_data[[col[1]]])
    edu_data <- edu_data[!is.na(edu_data)]
    unique_edus <- unique(edu_data)
    ordered_edus <- c(intersect(EDUCATION_ORDER, unique_edus), sort(setdiff(unique_edus, EDUCATION_ORDER)))
    edu_data <- factor(edu_data, levels = ordered_edus, ordered = TRUE)
    data <- table(edu_data)
    
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Education Distribution", 
             xaxis = list(title = "Education Level", tickangle = -45, categoryorder = "array", categoryarray = names(data)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_pre_race_ethnicity_dist <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(plotly_empty())
    
    col <- grep("Race/Ethnicity", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    # Multi-select: paren-aware split, fold non-preset values into "Other", canonical order.
    all_races <- .reach2_race_vec(big_pre_data[[col[1]]])
    if (length(all_races) == 0) return(plotly_empty())
    
    data <- table(all_races)
    ord <- c(intersect(RACE_LEVELS_CANONICAL, names(data)), sort(setdiff(names(data), RACE_LEVELS_CANONICAL)))
    data <- data[ord]
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Race/Ethnicity Distribution (Multi-select)", 
             xaxis = list(title = "Race/Ethnicity", tickangle = -45,
                          categoryorder = "array", categoryarray = names(data)),
             yaxis = list(title = "Count"))
    return(p)
  })
  output$impact_reach_variable_mapping <- DT::renderDataTable({
    tryCatch({
      mapping <- data.frame(
        Variable = c("Zip Code", "Age Group", "Race/Ethnicity", "Gender Identity", "Household Income",
                     "Education", "First-Gen College", "First-Gen U.S.", "Veteran Status", "Disability Status", "Neurodivergent"),
        Big_Pre = rep("Yes", 11),
        Little_Pre = c("Yes", rep("No", 10)),
        Little_Post = c("Yes", rep("No", 10)),
        Big_Post = rep("Yes", 11),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  # Overview: zip code maps (Big Pre / Big Post); aggregation scales with zoom (relayout → reactive view)
  .overview_zip_map <- function(df, title, plot_source, view_state = NULL) {
    if (is.null(ZIPCODE_DATA) || nrow(ZIPCODE_DATA) == 0) {
      return(plotly_empty() %>% layout(title = "Install CRAN package zipcode for zip maps (install.packages(\"zipcode\"))", font = PLOT_FONT))
    }
    if (is.null(df) || nrow(df) == 0) return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
    zip_patterns <- c("Zip Code", "Zip", "ZIP", "Postal Code", "zipcode", "Postal")
    zip_col <- NULL
    for (pat in zip_patterns) {
      hit <- grep(pat, colnames(df), ignore.case = TRUE, value = TRUE)
      if (length(hit) > 0) { zip_col <- hit[1]; break }
    }
    if (is.null(zip_col) || length(zip_col) == 0) return(plotly_empty() %>% layout(title = "No Zip column", font = PLOT_FONT))
    raw <- gsub("[^0-9]", "", as.character(df[[zip_col]]))
    raw <- substr(raw, 1, 5)
    raw[nchar(raw) < 5] <- NA
    raw <- raw[!is.na(raw)]
    if (length(raw) == 0) return(plotly_empty() %>% layout(title = "No zip codes", font = PLOT_FONT))
    agg <- as.data.frame(table(raw), stringsAsFactors = FALSE)
    names(agg) <- c("zip", "n")
    agg$zip <- sprintf("%05d", suppressWarnings(as.integer(agg$zip)))
    m <- dplyr::inner_join(agg, ZIPCODE_DATA, by = "zip")
    if (nrow(m) == 0) return(plotly_empty() %>% layout(title = "No zips matched to US database", font = PLOT_FONT))
    gd <- grid_deg_from_view(view_state)
    m <- aggregate_zip_markers_for_map(m, gd)
    max_s <- max(m$siz, na.rm = TRUE)
    if (!is.finite(max_s) || max_s <= 0) max_s <- 20
    sizeref <- max_s / 10
    plot_ly(
      m,
      lat = ~lat, lon = ~lon, type = "scattergeo", mode = "markers", source = plot_source,
      marker = list(
        size = ~siz, sizemode = "area", sizemin = 14, sizeref = sizeref,
        color = "#5c2f92", opacity = 0.82, line = list(color = "white", width = 0.6)
      ),
      text = ~hover, hoverinfo = "text"
    ) %>%
      layout(
        title = title, font = PLOT_FONT, uirevision = plot_source,
        geo = list(
          scope = "usa", projection = list(type = "albers usa"),
          showland = TRUE, landcolor = "#f0f0f0",
          lakecolor = "#e6f3ff", bgcolor = "#ffffff"
        )
      )
  }
  output$impact_overview_zip_map_big_pre <- renderPlotly({
    req(data_ready())
    .overview_zip_map(filtered_big_pre(), "Big Pre — learners by zip code", "zipmap_pre", zip_map_view_pre())
  })
  output$impact_overview_zip_map_big_post <- renderPlotly({
    req(data_ready())
    .overview_zip_map(filtered_big_post(), "Big Post — learners by zip code", "zipmap_post", zip_map_view_post())
  })
  # Reach tab visuals temporarily disabled while stabilizing initial load/Overview.

  # ---- Reach 2: simple demographic charts from filtered_big_pre / filtered_big_post ----
  output$reach2_variable_mapping <- DT::renderDataTable({
    tryCatch({
      # Live Master Annual Survey (2026) includes the same demographic block as Big Pre/Post
      mapping <- data.frame(
        Variable = c("Zip Code", "Age Group", "Race/Ethnicity", "Gender Identity", "Household Income",
                     "Education", "First-Gen College", "First-Gen U.S.", "Veteran Status", "Disability Status", "Neurodivergent"),
        Big_Pre = rep("Yes", 11),
        Little_Pre = c("Yes", rep("No", 10)),
        Little_Post = c("Yes", rep("No", 10)),
        Big_Post = rep("Yes", 11),
        Annual = rep("Yes", 11),
        stringsAsFactors = FALSE
      )
      .style_variable_mapping_table(mapping)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  # Align with EDUCATION_ORDER / .normalize_education_for_dashboard (Middle school before high school)
  .reach2_norm_education <- function(x) {
    .normalize_education_for_dashboard(x)
  }
  .reach2_norm_race_label <- function(x) {
    if (is.null(x)) return(x)
    v <- trimws(as.character(x))
    v[v == ""] <- NA
    out <- rep(NA_character_, length(v))
    non_na <- !is.na(v)
    if (!any(non_na)) return(out)
    matched <- unname(RACE_KEY_TO_CANONICAL[.race_match_key(v[non_na])])
    # Any non-preset value (e.g. legacy "Asian", free-text "Vietnamese") is folded into "Other".
    matched[is.na(matched)] <- "Other"
    out[non_na] <- matched
    out
  }
  .reach2_col <- function(df, pattern) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    ccol <- grep(pattern, colnames(df), ignore.case = TRUE, value = TRUE)
    # Annual form uses "FirstGeneration Status (U.S.)" (no hyphen after First)
    if (length(ccol) == 0 && grepl("First.?Generation.*U\\.S", pattern, ignore.case = TRUE)) {
      ccol <- grep("FirstGeneration Status.*U\\.S|First-Generation Status.*U\\.S", colnames(df), ignore.case = TRUE, value = TRUE)
    }
    if (length(ccol) == 0 && grepl("neurodivergent", pattern, ignore.case = TRUE)) {
      ccol <- grep("neurodivergent", colnames(df), ignore.case = TRUE, value = TRUE)
    }
    if (length(ccol) == 0) return(NULL)
    df[[ccol[1]]]
  }
  # Fill counts for all full_levels (0 where missing) so Pre/Post share same x-axis
  .reach2_tab_full_levels <- function(raw_tab, full_levels) {
    if (length(full_levels) == 0) return(raw_tab)
    out <- setNames(rep(0, length(full_levels)), full_levels)
    for (lev in names(raw_tab)) if (lev %in% full_levels) out[lev] <- as.numeric(raw_tab[lev])
    out
  }
  .reach2_bar <- function(tab, title, order_levels = NULL, y_max = NULL, x_display = NULL,
                          show_n = TRUE, show_pct = FALSE, source = NULL) {
    if (is.null(tab) || length(tab) == 0) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
    if (!is.null(order_levels)) {
      ord <- intersect(order_levels, names(tab))
      extra <- setdiff(names(tab), order_levels)
      tab <- tab[c(ord, extra)]
    }
    if (length(tab) == 0) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
    x_lab <- names(tab)
    x_axis <- if (!is.null(x_display) && length(x_display) > 0)
      unname(x_display[x_lab]) else x_lab
    x_axis[is.na(x_axis)] <- x_lab[is.na(x_axis)]
    y <- as.numeric(tab)
    total <- sum(y, na.rm = TRUE)
    pct <- if (is.finite(total) && total > 0) round(100 * y / total, 1) else rep(0, length(y))
    # Plain-English one-liner under the chart title (most common category).
    eng_sub <- ""
    if (is.finite(total) && total > 0 && length(y) > 0) {
      i_max <- which.max(y)
      top_lab <- as.character(x_axis[[i_max]])
      click_hint <- if (!is.null(source)) " Click a bar for respondents." else ""
      eng_sub <- sprintf(
        "<br><span style='font-size:11px;color:#5f6369;font-weight:400'>Most common: %s (%s%% of %s).%s</span>",
        htmltools::htmlEscape(top_lab),
        format(pct[[i_max]], nsmall = 0),
        format(as.integer(total), big.mark = ","),
        click_hint
      )
    }
    show_bar_labs <- isTRUE(show_n) || isTRUE(show_pct)
    bar_text <- vapply(seq_along(y), function(i) {
      parts <- character(0)
      if (isTRUE(show_n)) parts <- c(parts, as.character(y[i]))
      if (isTRUE(show_pct)) parts <- c(parts, paste0(pct[i], "%"))
      paste(parts, collapse = "\n")
    }, character(1))
    if (!show_bar_labs) bar_text <- ""
    lab_line <- if (!is.null(x_display) && any(x_lab != x_axis, na.rm = TRUE)) x_lab else x_axis
    hover_base <- paste0(lab_line, ": ", y, " (", pct, "%)")
    hover_text <- if (!is.null(source)) paste0(hover_base, "<br>Click for respondents") else hover_base
    # Ensure category order is stable (use axis labels since those are what we're plotting)
    cat_order <- x_axis
    # customdata keeps the chart category key (needed when axis shows short income labels)
    p <- plot_ly(
      x = x_axis, y = y, type = "bar",
      customdata = x_lab,
      source = if (!is.null(source)) source else "reach2_bar",
      text = bar_text, hovertext = hover_text, hoverinfo = "text",
      textposition = if (show_bar_labs) "outside" else "none",
      marker = list(color = REACH_PALETTE[seq_along(x_axis) %% length(REACH_PALETTE) + 1])
    ) %>%
      layout(
        title = list(text = paste0(title, eng_sub), font = PLOT_FONT),
        font = PLOT_FONT,
        xaxis = list(tickangle = -45, categoryorder = "array", categoryarray = cat_order),
        margin = list(b = 100, t = if (nzchar(eng_sub)) 72 else 50)
      )
    # Headroom for bar labels: two-line labels (N AND %) need extra room above the tallest bar.
    base_max <- if (!is.null(y_max) && is.finite(y_max) && y_max > 0) y_max else max(y, na.rm = TRUE)
    both_labs <- isTRUE(show_n) && isTRUE(show_pct)
    headroom_factor <- if (both_labs) 1.40 else if (show_bar_labs) 1.22 else 1.05
    headroom <- base_max * headroom_factor
    if (is.finite(headroom) && headroom > 0)
      p <- p %>% layout(yaxis = list(range = c(0, headroom), autorange = FALSE))
    if (!is.null(source)) p <- p %>% plotly::event_register("plotly_click")
    p
  }
  # Split a multi-select cell on commas/semicolons that are NOT inside parentheses,
  # so labels like "East Asian (e.g. Chinese, Japanese, Korean)" stay intact.
  .reach2_split_top_level <- function(s) {
    out <- character(0)
    current <- ""
    depth <- 0L
    for (ch in strsplit(s, "")[[1]]) {
      if (ch == "(") {
        depth <- depth + 1L
        current <- paste0(current, ch)
      } else if (ch == ")") {
        depth <- max(0L, depth - 1L)
        current <- paste0(current, ch)
      } else if ((ch == "," || ch == ";") && depth == 0L) {
        out <- c(out, trimws(current))
        current <- ""
      } else {
        current <- paste0(current, ch)
      }
    }
    out <- c(out, trimws(current))
    out[nzchar(out)]
  }
  .reach2_race_vec <- function(vec) {
    if (is.null(vec) || all(is.na(vec))) return(character(0))
    vals <- as.character(vec)
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) return(character(0))
    out <- unlist(lapply(vals, .reach2_split_top_level), use.names = FALSE)
    out <- .reach2_norm_race_label(out)
    out <- out[!is.na(out) & out != ""]
    out
  }
  # Shared level sets so Pre and Post use same x-axis (all possible levels)
  # For age, the youngest brackets (Under 16, 16-18, legacy Under 18) are kept left-most.
  .reach2_ordered_levels <- function(canonical, pre_vals, post_vals, youth_first = FALSE) {
    all_vals <- unique(c(canonical, na.omit(pre_vals), na.omit(post_vals)))
    ord <- intersect(canonical, all_vals)
    extra <- sort(setdiff(all_vals, canonical))
    out <- c(ord, extra)
    if (youth_first) {
      present_youth <- intersect(AGE_LEVELS_YOUTH, out)
      out <- c(present_youth, setdiff(out, present_youth))
    }
    out
  }
  pre_r2 <- reactive(filtered_big_pre())
  post_r2 <- reactive(filtered_big_post())
  ann_r2 <- reactive(filtered_annual())
  .reach2_demo_row_ui <- function(pre_id, post_id, ann_id, height = "280px") {
    use_ann <- tryCatch(isTRUE(input$include_annual_in_reach), error = function(e) FALSE)
    w <- if (isTRUE(use_ann)) 4L else 6L
    cols <- list(
      column(w, plotlyOutput(pre_id, height = height)),
      column(w, plotlyOutput(post_id, height = height))
    )
    if (isTRUE(use_ann)) cols[[length(cols) + 1L]] <- column(w, plotlyOutput(ann_id, height = height))
    do.call(fluidRow, cols)
  }
  output$reach2_age_row <- renderUI(.reach2_demo_row_ui("reach2_age_pre", "reach2_age_post", "reach2_age_annual", "280px"))
  output$reach2_gender_row <- renderUI(.reach2_demo_row_ui("reach2_gender_pre", "reach2_gender_post", "reach2_gender_annual", "280px"))
  output$reach2_income_row <- renderUI(.reach2_demo_row_ui("reach2_income_pre", "reach2_income_post", "reach2_income_annual", "280px"))
  output$reach2_education_row <- renderUI(.reach2_demo_row_ui("reach2_education_pre", "reach2_education_post", "reach2_education_annual", "380px"))
  output$reach2_race_row <- renderUI(.reach2_demo_row_ui("reach2_race_pre", "reach2_race_post", "reach2_race_annual", "460px"))

  reach2_levels_age <- reactive({
    if (!isTRUE(data_ready())) return(character(0))
    pre_v <- AGE_LEVELS_NORMALIZE(.reach2_col(pre_r2(), "Age Group"))
    post_v <- AGE_LEVELS_NORMALIZE(.reach2_col(post_r2(), "Age Group"))
    ann_v <- if (isTRUE(input$include_annual_in_reach)) {
      AGE_LEVELS_NORMALIZE(.reach2_col(ann_r2(), "Age Group"))
    } else NULL
    .reach2_ordered_levels(AGE_LEVELS_CANONICAL, c(pre_v, ann_v), post_v, youth_first = TRUE)
  })
  .reach2_gender_expand <- reactive({
    tryCatch(isTRUE(input$reach2_gender_display_others), error = function(e) FALSE)
  })
  .reach2_gender_vec <- function(df) {
    .gender_for_reach_chart(.reach2_col(df, "Gender Identity"), expand_others = .reach2_gender_expand())
  }
  reach2_levels_gender <- reactive({
    if (!isTRUE(data_ready())) return(character(0))
    pre_v <- .reach2_gender_vec(pre_r2())
    post_v <- .reach2_gender_vec(post_r2())
    ann_v <- if (isTRUE(input$include_annual_in_reach)) .reach2_gender_vec(ann_r2()) else NULL
    present <- unique(c(na.omit(pre_v), na.omit(post_v), na.omit(ann_v)))
    form_first <- intersect(GENDER_FORM_OPTIONS, present)
    extras <- sort(setdiff(present, GENDER_FORM_OPTIONS))
    c(form_first, extras)
  })
  reach2_levels_income <- reactive({
    if (!isTRUE(data_ready())) return(character(0))
    pre_v <- .reach2_col(pre_r2(), "Household Income")
    post_v <- .reach2_col(post_r2(), "Household Income")
    ann_v <- if (isTRUE(input$include_annual_in_reach)) {
      .reach2_col(ann_r2(), "Household Income")
    } else NULL
    .reach2_ordered_levels(INCOME_ORDER, c(pre_v, ann_v), post_v)
  })
  reach2_levels_education <- reactive({
    if (!isTRUE(data_ready())) return(character(0))
    pre_v <- .reach2_norm_education(.reach2_col(pre_r2(), "highest level of education"))
    post_v <- .reach2_norm_education(.reach2_col(post_r2(), "highest level of education"))
    ann_v <- if (isTRUE(input$include_annual_in_reach)) {
      .reach2_norm_education(.reach2_col(ann_r2(), "highest level of education"))
    } else NULL
    .reach2_ordered_levels(EDUCATION_ORDER, c(pre_v, ann_v), post_v)
  })
  reach2_levels_race <- reactive({
    if (!isTRUE(data_ready())) return(character(0))
    pre_v <- .reach2_col(pre_r2(), "Race/Ethnicity")
    post_v <- .reach2_col(post_r2(), "Race/Ethnicity")
    ann_v <- if (isTRUE(input$include_annual_in_reach)) .reach2_col(ann_r2(), "Race/Ethnicity") else NULL
    pre_flat <- .reach2_race_vec(pre_v)
    post_flat <- .reach2_race_vec(post_v)
    ann_flat <- .reach2_race_vec(ann_v)
    present <- unique(c(pre_flat, post_flat, ann_flat))
    c(intersect(RACE_LEVELS_CANONICAL, present), sort(setdiff(present, RACE_LEVELS_CANONICAL)))
  })
  output$reach2_age_pre <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- AGE_LEVELS_NORMALIZE(.reach2_col(pre_r2(), "Age Group"))
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Age (Pre) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_age()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Age (Pre) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      post_raw <- table(na.omit(AGE_LEVELS_NORMALIZE(.reach2_col(post_r2(), "Age Group"))))
      post_tab <- .reach2_tab_full_levels(post_raw, lvls)
      max(c(tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Age (Pre)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_age_pre", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_age_post <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- AGE_LEVELS_NORMALIZE(.reach2_col(post_r2(), "Age Group"))
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Age (Post) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_age()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Age (Post) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_raw <- table(na.omit(AGE_LEVELS_NORMALIZE(.reach2_col(pre_r2(), "Age Group"))))
      pre_tab <- .reach2_tab_full_levels(pre_raw, lvls)
      max(c(tab, pre_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Age (Post)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_age_post", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_gender_pre <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_gender_vec(pre_r2())
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Gender (Pre) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_gender()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Gender (Pre) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      post_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_gender_vec(post_r2()))), lvls)
      max(c(tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Gender (Pre)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_gender_pre", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_gender_post <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_gender_vec(post_r2())
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Gender (Post) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_gender()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Gender (Post) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_gender_vec(pre_r2()))), lvls)
      max(c(tab, pre_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Gender (Post)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_gender_post", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_income_pre <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(pre_r2(), "Household Income")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Household Income (Pre) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_income()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Household Income (Pre) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      post_raw <- table(na.omit(.reach2_col(post_r2(), "Household Income")))
      post_tab <- .reach2_tab_full_levels(post_raw, lvls)
      max(c(tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Household Income (Pre)", lvls, y_max, x_display = INCOME_SHORT_LABELS,
                show_n = isTRUE(input$opt_show_n), show_pct = isTRUE(input$opt_show_pct), source = "reach2_income_pre")
  })
  output$reach2_income_post <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(post_r2(), "Household Income")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Household Income (Post) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_income()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Household Income (Post) (no levels)", font = PLOT_FONT))
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_raw <- table(na.omit(.reach2_col(pre_r2(), "Household Income")))
      pre_tab <- .reach2_tab_full_levels(pre_raw, lvls)
      max(c(tab, pre_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Household Income (Post)", lvls, y_max, x_display = INCOME_SHORT_LABELS,
                show_n = isTRUE(input$opt_show_n), show_pct = isTRUE(input$opt_show_pct), source = "reach2_income_post")
  })
  output$reach2_education_pre <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(pre_r2(), "highest level of education")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Education (Pre) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_education()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Education (Pre) (no levels)", font = PLOT_FONT))
    v <- .reach2_norm_education(v)
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      post_raw <- table(na.omit(.reach2_col(post_r2(), "highest level of education")))
      post_tab <- .reach2_tab_full_levels(post_raw, lvls)
      max(c(tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Education (Pre)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_education_pre", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_education_post <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(post_r2(), "highest level of education")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Education (Post) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_education()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Education (Post) (no levels)", font = PLOT_FONT))
    v <- .reach2_norm_education(v)
    raw <- table(na.omit(v))
    tab <- .reach2_tab_full_levels(raw, lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_raw <- table(na.omit(.reach2_col(pre_r2(), "highest level of education")))
      pre_tab <- .reach2_tab_full_levels(pre_raw, lvls)
      max(c(tab, pre_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Education (Post)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_education_post", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_race_pre <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(pre_r2(), "Race/Ethnicity")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Race/Ethnicity (Pre) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_race()
    flat <- .reach2_race_vec(v)
    if (length(lvls) == 0 && length(flat) == 0) return(plotly_empty() %>% layout(title = "Race/Ethnicity (Pre) (no data)", font = PLOT_FONT))
    raw <- table(flat)
    tab <- if (length(lvls) > 0) .reach2_tab_full_levels(raw, lvls) else raw
    y_max <- if (isTRUE(input$opt_same_y) && length(lvls) > 0) {
      post_flat <- .reach2_race_vec(.reach2_col(post_r2(), "Race/Ethnicity"))
      post_tab <- .reach2_tab_full_levels(table(post_flat), lvls)
      max(c(tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Race/Ethnicity (Pre)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_race_pre", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_race_post <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    v <- .reach2_col(post_r2(), "Race/Ethnicity")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Race/Ethnicity (Post) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_race()
    flat <- .reach2_race_vec(v)
    if (length(lvls) == 0 && length(flat) == 0) return(plotly_empty() %>% layout(title = "Race/Ethnicity (Post) (no data)", font = PLOT_FONT))
    raw <- table(flat)
    tab <- if (length(lvls) > 0) .reach2_tab_full_levels(raw, lvls) else raw
    y_max <- if (isTRUE(input$opt_same_y) && length(lvls) > 0) {
      pre_flat <- .reach2_race_vec(.reach2_col(pre_r2(), "Race/Ethnicity"))
      pre_tab <- .reach2_tab_full_levels(table(pre_flat), lvls)
      max(c(tab, pre_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Race/Ethnicity (Post)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_race_post", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_age_annual <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    if (!isTRUE(input$include_annual_in_reach)) return(plotly_empty() %>% layout(title = "Age (Annual)", font = PLOT_FONT))
    v <- AGE_LEVELS_NORMALIZE(.reach2_col(ann_r2(), "Age Group"))
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Age (Annual) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_age()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Age (Annual) (no levels)", font = PLOT_FONT))
    tab <- .reach2_tab_full_levels(table(na.omit(v)), lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_tab <- .reach2_tab_full_levels(table(na.omit(AGE_LEVELS_NORMALIZE(.reach2_col(pre_r2(), "Age Group")))), lvls)
      post_tab <- .reach2_tab_full_levels(table(na.omit(AGE_LEVELS_NORMALIZE(.reach2_col(post_r2(), "Age Group")))), lvls)
      max(c(tab, pre_tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Age (Annual)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_age_annual", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_gender_annual <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    if (!isTRUE(input$include_annual_in_reach)) return(plotly_empty() %>% layout(title = "Gender (Annual)", font = PLOT_FONT))
    v <- .reach2_gender_vec(ann_r2())
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Gender (Annual) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_gender()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Gender (Annual) (no levels)", font = PLOT_FONT))
    tab <- .reach2_tab_full_levels(table(na.omit(v)), lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_gender_vec(pre_r2()))), lvls)
      post_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_gender_vec(post_r2()))), lvls)
      max(c(tab, pre_tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Gender (Annual)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_gender_annual", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_income_annual <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    if (!isTRUE(input$include_annual_in_reach)) return(plotly_empty() %>% layout(title = "Household Income (Annual)", font = PLOT_FONT))
    v <- .reach2_col(ann_r2(), "Household Income")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Household Income (Annual) - column not found", font = PLOT_FONT))
    lvls <- reach2_levels_income()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Household Income (Annual) (no levels)", font = PLOT_FONT))
    tab <- .reach2_tab_full_levels(table(na.omit(v)), lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_col(pre_r2(), "Household Income"))), lvls)
      post_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_col(post_r2(), "Household Income"))), lvls)
      max(c(tab, pre_tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Household Income (Annual)", lvls, y_max, x_display = INCOME_SHORT_LABELS,
                show_n = isTRUE(input$opt_show_n), show_pct = isTRUE(input$opt_show_pct), source = "reach2_income_annual")
  })
  output$reach2_education_annual <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    if (!isTRUE(input$include_annual_in_reach)) return(plotly_empty() %>% layout(title = "Education (Annual)", font = PLOT_FONT))
    v <- .reach2_col(ann_r2(), "highest level of education")
    if (is.null(v)) return(plotly_empty() %>% layout(title = "Education (Annual) — column not found", font = PLOT_FONT))
    lvls <- reach2_levels_education()
    if (length(lvls) == 0) return(plotly_empty() %>% layout(title = "Education (Annual) (no levels)", font = PLOT_FONT))
    v <- .reach2_norm_education(v)
    tab <- .reach2_tab_full_levels(table(na.omit(v)), lvls)
    y_max <- if (isTRUE(input$opt_same_y)) {
      pre_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_norm_education(.reach2_col(pre_r2(), "highest level of education")))), lvls)
      post_tab <- .reach2_tab_full_levels(table(na.omit(.reach2_norm_education(.reach2_col(post_r2(), "highest level of education")))), lvls)
      max(c(tab, pre_tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Education (Annual)", lvls, y_max, show_n = isTRUE(input$opt_show_n), source = "reach2_education_annual", show_pct = isTRUE(input$opt_show_pct))
  })
  output$reach2_race_annual <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    if (!isTRUE(input$include_annual_in_reach)) return(plotly_empty() %>% layout(title = "Race (Annual)", font = PLOT_FONT))
    v <- .reach2_col(ann_r2(), "Race/Ethnicity")
    if (is.null(v)) {
      return(plotly_empty() %>% layout(
        title = "Race/Ethnicity (Annual) — not on Annual form",
        font = PLOT_FONT
      ))
    }
    lvls <- reach2_levels_race()
    flat <- .reach2_race_vec(v)
    if (length(lvls) == 0 && length(flat) == 0) {
      return(plotly_empty() %>% layout(title = "Race/Ethnicity (Annual) (no data)", font = PLOT_FONT))
    }
    raw <- table(flat)
    tab <- if (length(lvls) > 0) .reach2_tab_full_levels(raw, lvls) else raw
    y_max <- if (isTRUE(input$opt_same_y) && length(lvls) > 0) {
      pre_tab <- .reach2_tab_full_levels(table(.reach2_race_vec(.reach2_col(pre_r2(), "Race/Ethnicity"))), lvls)
      post_tab <- .reach2_tab_full_levels(table(.reach2_race_vec(.reach2_col(post_r2(), "Race/Ethnicity"))), lvls)
      max(c(tab, pre_tab, post_tab), na.rm = TRUE)
    } else NULL
    .reach2_bar(tab, "Race/Ethnicity (Annual)", lvls, y_max, show_n = isTRUE(input$opt_show_n), show_pct = isTRUE(input$opt_show_pct), source = "reach2_race_annual")
  })

  # ---- Reach bar click -> respondent modal (wave of clicked chart only) ----
  reach_drill_rows <- reactiveVal(NULL)
  reach_drill_title <- reactiveVal("")
  user_journey_active_id <- reactiveVal(NULL)

  .reach_drill_wave_df <- function(wave) {
    if (identical(wave, "pre")) return(pre_r2())
    if (identical(wave, "post")) return(post_r2())
    if (identical(wave, "annual")) return(ann_r2())
    data.frame()
  }

  .reach_drill_filter_df <- function(df, var, category) {
    if (is.null(df) || !nrow(df)) return(df)
    cat <- as.character(category)
    if (identical(var, "age")) {
      v <- AGE_LEVELS_NORMALIZE(.reach2_col(df, "Age Group"))
      keep <- !is.na(v) & as.character(v) == cat
      return(list(df = df[keep, , drop = FALSE], raw = as.character(v[keep])))
    }
    if (identical(var, "gender")) {
      v <- .reach2_gender_vec(df)
      keep <- !is.na(v) & as.character(v) == cat
      raw_col <- .reach2_col(df, "Gender Identity")
      return(list(df = df[keep, , drop = FALSE], raw = as.character(raw_col[keep])))
    }
    if (identical(var, "income")) {
      v <- as.character(.reach2_col(df, "Household Income"))
      keep <- !is.na(v) & v == cat
      return(list(df = df[keep, , drop = FALSE], raw = v[keep]))
    }
    if (identical(var, "education")) {
      raw_col <- .reach2_col(df, "highest level of education")
      v <- .reach2_norm_education(raw_col)
      keep <- !is.na(v) & as.character(v) == cat
      return(list(df = df[keep, , drop = FALSE], raw = as.character(raw_col[keep])))
    }
    if (identical(var, "race")) {
      raw_col <- .reach2_col(df, "Race/Ethnicity")
      keep <- vapply(as.character(raw_col), function(cell) {
        if (is.na(cell) || !nzchar(trimws(cell))) return(FALSE)
        cat %in% .reach2_race_vec(cell)
      }, logical(1))
      return(list(df = df[keep, , drop = FALSE], raw = as.character(raw_col[keep])))
    }
    list(df = df[0, , drop = FALSE], raw = character(0))
  }

  .reach_open_drill_modal <- function(src) {
    parts <- strsplit(src, "_", fixed = TRUE)[[1]]
    if (length(parts) < 3) return()
    wave <- parts[length(parts)]
    var <- paste(parts[2:(length(parts) - 1)], collapse = "_")
    ev <- tryCatch(plotly::event_data("plotly_click", source = src), error = function(e) NULL)
    if (is.null(ev) || !nrow(ev)) return()
    category <- {
      cd <- if ("customdata" %in% names(ev)) ev$customdata else NULL
      if (!is.null(cd) && length(unlist(cd))) as.character(unlist(cd)[[1]])
      else as.character(ev$x[[1]])
    }
    if (!nzchar(category) || identical(category, "NULL")) return()
    df <- .reach_drill_wave_df(wave)
    matched <- .reach_drill_filter_df(df, var, category)
    tbl <- build_reach_drill_table(
      matched$df,
      wave = tools::toTitleCase(wave),
      chart_value = category,
      raw_response = matched$raw
    )
    reach_drill_rows(tbl)
    reach_drill_title(paste0(
      tools::toTitleCase(var), " / ", tools::toTitleCase(wave), ": ", category
    ))
    shiny::showModal(shiny::modalDialog(
      title = paste0("Respondents — ", reach_drill_title()),
      size = "l",
      easyClose = TRUE,
      shiny::p(
        style = "font-size: 12px; color: #5f6369;",
        "People in this bar for the clicked wave only (current sidebar filters). ",
        "Select a row, then open User Journey for that person's full history."
      ),
      DT::DTOutput("reach_drill_table"),
      footer = shiny::tagList(
        shiny::actionButton("reach_drill_open_journey", "Open selected in User Journey", class = "btn-primary"),
        shiny::modalButton("Close")
      )
    ))
  }

  for (.reach_src in c(
    "reach2_age_pre", "reach2_age_post", "reach2_age_annual",
    "reach2_gender_pre", "reach2_gender_post", "reach2_gender_annual",
    "reach2_income_pre", "reach2_income_post", "reach2_income_annual",
    "reach2_education_pre", "reach2_education_post", "reach2_education_annual",
    "reach2_race_pre", "reach2_race_post", "reach2_race_annual"
  )) {
    local({
      src <- .reach_src
      observeEvent(
        plotly::event_data("plotly_click", source = src),
        {
          .reach_open_drill_modal(src)
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    })
  }

  output$reach_drill_table <- DT::renderDataTable({
    tbl <- reach_drill_rows()
    if (is.null(tbl) || !nrow(tbl)) {
      return(DT::datatable(
        data.frame(Message = "No rows"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    DT::datatable(
      tbl,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  observeEvent(input$reach_drill_open_journey, {
    tbl <- reach_drill_rows()
    sel <- input$reach_drill_table_rows_selected
    if (is.null(tbl) || !nrow(tbl) || !"respondent_id" %in% names(tbl)) {
      showNotification("No respondent table available.", type = "warning")
      return()
    }
    if (!length(sel)) {
      showNotification("Select a respondent row first.", type = "warning")
      return()
    }
    rid <- trimws(as.character(tbl$respondent_id[sel[[1]]]))
    if (!nzchar(rid) || is.na(rid)) {
      showNotification("Selected row has no respondent_id.", type = "warning")
      return()
    }
    removeModal()
    updateTabsetPanel(session, "impact_tabs", selected = "user_journey")
    updateTextInput(session, "user_journey_id", value = rid)
    user_journey_active_id(rid)
  })

  register_user_journey_outputs(
    input, output, session,
    master_pre, master_post, master_annual,
    program_manager_for_typing,
    user_journey_active_id
  )

  # Reach 2 cross-tab heatmaps (Big Pre only; brand color scale)
  .reach2_crosstab_heatmap <- function(df, pattern1, pattern2, title, order1 = NULL, order2 = NULL, expand2 = FALSE) {
    if (is.null(df) || nrow(df) == 0) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
    v1 <- .reach2_col(df, pattern1)
    v2_raw <- .reach2_col(df, pattern2)
    if (is.null(v1) || is.null(v2_raw)) return(plotly_empty() %>% layout(title = paste0(title, " — column not found"), font = PLOT_FONT))
    if (identical(pattern1, "Age Group")) v1 <- AGE_LEVELS_NORMALIZE(v1)
    if (grepl("highest level of education", pattern1, ignore.case = TRUE)) v1 <- .reach2_norm_education(v1)
    if (grepl("highest level of education", pattern2, ignore.case = TRUE)) v2_raw <- .reach2_norm_education(v2_raw)
    if (expand2) {
      races_list <- lapply(as.character(v2_raw), function(s) {
        if (is.na(s) || !nzchar(trimws(s))) return(character(0))
        .reach2_split_top_level(s)
      })
      races_list <- lapply(races_list, function(x) x[!is.na(x) & x != ""])
      lens <- lengths(races_list)
      if (sum(lens) == 0) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
      v1_exp <- rep(v1, lens)
      v2_exp <- unlist(races_list, use.names = FALSE)
      v2_exp <- .reach2_norm_race_label(v2_exp)
      keep <- !(is.na(v1_exp) | as.character(v1_exp) == "" | is.na(v2_exp) | v2_exp == "")
      v1_exp <- v1_exp[keep]
      v2_exp <- v2_exp[keep]
      if (length(v1_exp) == 0) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
      tab <- table(v1_exp, v2_exp)
    } else {
      v2 <- if (identical(pattern2, "Age Group")) AGE_LEVELS_NORMALIZE(v2_raw) else v2_raw
      keep <- !is.na(v1) & !is.na(v2) & as.character(v1) != "" & as.character(v2) != ""
      tab <- table(v1[keep], v2[keep])
    }
    if (length(dim(tab)) != 2 || any(dim(tab) == 0)) return(plotly_empty() %>% layout(title = paste0(title, " (no data)"), font = PLOT_FONT))
    if (!is.null(order1)) {
      r_keep <- intersect(order1, rownames(tab))
      r_extra <- setdiff(rownames(tab), order1)
      tab <- tab[c(r_keep, r_extra), , drop = FALSE]
    }
    if (!is.null(order2)) {
      c_keep <- intersect(order2, colnames(tab))
      c_extra <- setdiff(colnames(tab), order2)
      tab <- tab[, c(c_keep, c_extra), drop = FALSE]
    }
    z <- unname(tab)
    x_lab <- colnames(tab)
    if (identical(order2, INCOME_ORDER) && length(INCOME_SHORT_LABELS) > 0)
      x_lab <- ifelse(is.na(INCOME_SHORT_LABELS[x_lab]), x_lab, unname(INCOME_SHORT_LABELS[x_lab]))
    # Use HTML <br> for readable multi-line hover labels in Plotly
    text_mat <- matrix(paste0(outer(rownames(tab), colnames(tab), paste, sep = " × "), "<br>Count: ", as.vector(z)),
                        nrow = nrow(tab), ncol = ncol(tab))
    p <- plot_ly(x = x_lab, y = rownames(tab), z = z, type = "heatmap", text = text_mat, hoverinfo = "text",
                 colorscale = list(c(0, "#f8f9fa"), c(0.5, "#e0d4f0"), c(1, "#5c2f92"))) %>%
      layout(
        title = title,
        font = PLOT_FONT,
        hovermode = "closest",
        hoverdistance = 40,
        hoverlabel = list(align = "left", bgcolor = "white", bordercolor = "#5c2f92", font = list(color = "#333333", size = 12)),
        xaxis = list(tickangle = -45),
        margin = list(b = 80, l = 120)
      )
    p
  }
  big_pre_ct <- reactive({ if (!isTRUE(data_ready())) return(NULL); filtered_big_pre() })
  output$reach2_ct_age_income <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "Age Group", "Household Income", "Age × Income",
                            order1 = AGE_LEVELS_CANONICAL, order2 = INCOME_ORDER)
  })
  output$reach2_ct_education_income <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "highest level of education", "Household Income", "Education × Income",
                            order1 = EDUCATION_ORDER, order2 = INCOME_ORDER)
  })
  output$reach2_ct_age_race <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "Age Group", "Race/Ethnicity", "Age × Race",
                            order1 = AGE_LEVELS_CANONICAL, order2 = RACE_LEVELS_CANONICAL, expand2 = TRUE)
  })
  output$reach2_ct_gender_income <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "Gender Identity", "Household Income", "Gender × Income",
                            order2 = INCOME_ORDER)
  })
  output$reach2_ct_gender_race <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "Gender Identity", "Race/Ethnicity", "Gender × Race",
                            order2 = RACE_LEVELS_CANONICAL, expand2 = TRUE)
  })
  output$reach2_ct_gender_age <- renderPlotly({
    if (!isTRUE(data_ready())) return(plotly_empty() %>% layout(title = "Loading...", font = PLOT_FONT))
    .reach2_crosstab_heatmap(big_pre_ct(), "Gender Identity", "Age Group", "Gender × Age",
                            order2 = AGE_LEVELS_CANONICAL)
  })

  # Impact: Learning & Impact Stories — keep in touch (multi-select); Pre/Post open text
  output$learning_keep_in_touch_bar <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty() %>% layout(title = "No Big Post data", font = PLOT_FONT))
      col <- .resolve_col(d, POST_KEEP_IN_TOUCH_COL, "keep in touch")
      if (is.null(col)) return(plotly_empty() %>% layout(title = "Keep in touch column not found", font = PLOT_FONT))
      vec <- d[[col]]
      n_responders <- sum(!is.na(vec) & nzchar(trimws(as.character(vec))))
      n_big <- nrow(d)
      use_all_post <- isTRUE(input$learning_interests_pct_denominator_all_post)
      n_den_pct <- if (use_all_post) n_big else n_responders
      all_labs <- character(0)
      for (i in seq_along(vec)) {
        x <- vec[i]
        if (is.na(x) || !nzchar(trimws(as.character(x)))) next
        all_labs <- c(all_labs, .parse_keep_in_touch_cell(x))
      }
      if (length(all_labs) == 0) return(plotly_empty() %>% layout(title = "No selections", font = PLOT_FONT))
      tbl <- table(all_labs)
      canon_first <- KEEP_IN_TOUCH_CANONICAL_LABELS[KEEP_IN_TOUCH_CANONICAL_LABELS %in% names(tbl)]
      rest <- sort(setdiff(names(tbl), canon_first))
      ord <- c(canon_first, rest)
      ord <- ord[ord %in% names(tbl)]
      df <- data.frame(option = ord, count = as.integer(tbl[ord]), stringsAsFactors = FALSE)
      df <- df[df$count > 0, , drop = FALSE]
      df <- df[order(-df$count, df$option), , drop = FALSE]
      df$lab <- .keep_touch_label_before_dash(df$option)
      if (any(duplicated(df$lab))) df$lab <- make.unique(df$lab, sep = " · ")
      colors <- .learning_interest_colors_for_options(df$option)
      # Descending count → top row: Plotly h-bar shows last factor level at top
      df$lab <- factor(df$lab, levels = rev(unique(df$lab)))
      show_n_bar <- isTRUE(input$opt_show_n)
      show_pct_bar <- isTRUE(input$opt_show_pct)
      df$hover_extra <- if (n_den_pct > 0) {
        paste0(
          sprintf("%.0f", pmin(100, df$count / n_den_pct * 100)),
          if (use_all_post) " pct of all Big Post rows (current filters)" else " pct of respondents who answered keep-in-touch",
          " (denom n=", n_den_pct, ")"
        )
      } else {
        rep("", nrow(df))
      }
      bar_txt <- rep("", nrow(df))
      for (ir in seq_len(nrow(df))) {
        parts <- character(0)
        if (show_n_bar) parts <- c(parts, paste0("n=", df$count[ir]))
        if (show_pct_bar && n_den_pct > 0) parts <- c(parts, paste0(sprintf("%.1f", df$count[ir] / n_den_pct * 100), "%"))
        bar_txt[ir] <- paste(parts, collapse = " ")
      }
      has_bar_txt <- (show_n_bar || show_pct_bar) && any(nzchar(trimws(bar_txt)))
      plot_ly(
        df,
        x = ~count,
        y = ~lab,
        type = "bar",
        orientation = "h",
        marker = list(color = colors, line = list(color = "rgba(0,0,0,0.12)", width = 1)),
        customdata = ~hover_extra,
        hovertemplate = "%{y}<br>Count: %{x}<br>%{customdata}<extra></extra>",
        text = if (has_bar_txt) bar_txt else NULL,
        textposition = if (has_bar_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        layout(
          title = "Learning interests",
          font = PLOT_FONT,
          xaxis = list(title = "Count of selections"),
          yaxis = list(title = ""),
          margin = list(l = 220, t = 56, r = 24)
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$learning_keep_in_touch_by_gender <- renderPlotly({
    tryCatch({
      .learning_keep_touch_heatmap_plotly(filtered_big_post(), "Gender Identity", "Learning interests by gender", "^Gender Identity")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$learning_keep_in_touch_by_age <- renderPlotly({
    tryCatch({
      .learning_keep_touch_heatmap_plotly(filtered_big_post(), "Age Group", "Learning interests by age", "^Age Group")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$learning_keep_in_touch_by_income <- renderPlotly({
    tryCatch({
      .learning_keep_touch_heatmap_plotly(filtered_big_post(), "Household Income", "Learning interests by household income", "household income")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$learning_keep_in_touch_by_education <- renderPlotly({
    tryCatch({
      .learning_keep_touch_heatmap_plotly(
        filtered_big_post(),
        "What is the highest level of education you have completed? (Select one)",
        "Learning interests by education",
        "highest level of education|education.*completed"
      )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$impact_sentiment_lexicon_blurb <- renderUI({
    HTML(paste0(
      "<div style=\"margin-bottom:16px;border:1px solid #e0e0e0;border-radius:6px;padding:12px;background:#fafafa;\">",
      sentiment_units_blurb(),
      "</div>"
    ))
  })

  .attach_dual_wc_outputs(output, "impact_pre_intention", filtered_big_pre, filter_cache_key, PRE_OPENING_COLS[1], "intention.*workshop")
  output$impact_pre_intention_sent <- renderUI({
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(HTML("<p>No Big Pre data for current filters.</p>"))
    col <- .resolve_col(df, PRE_OPENING_COLS[1], "intention.*workshop")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_pre_intention_sent")

  .attach_dual_wc_outputs(output, "impact_pre_curiosity", filtered_big_pre, filter_cache_key, PRE_OPENING_COLS[2], "curiosities|questions.*curiosit")
  output$impact_pre_curiosity_sent <- renderUI({
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(HTML("<p>No Big Pre data for current filters.</p>"))
    col <- .resolve_col(df, PRE_OPENING_COLS[2], "curiosities|questions.*curiosit")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_pre_curiosity_sent")

  .attach_dual_wc_outputs(output, "impact_pre_hope", filtered_big_pre, filter_cache_key, PRE_OPENING_COLS[3], "hope.*feel")
  output$impact_pre_hope_sent <- renderUI({
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(HTML("<p>No Big Pre data for current filters.</p>"))
    col <- .resolve_col(df, PRE_OPENING_COLS[3], "hope.*feel")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_pre_hope_sent")

  .attach_dual_wc_outputs(output, "impact_pre_extra", filtered_big_pre, filter_cache_key, PRE_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
  output$impact_pre_extra_sent <- renderUI({
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(HTML("<p>No Big Pre data for current filters.</p>"))
    col <- .resolve_col(df, PRE_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_pre_extra_sent")

  .attach_dual_wc_outputs(output, "impact_post_insight", filtered_big_post, filter_cache_key, POST_TODAY_SESSION_COLS[1], "stood out|idea.*tool")
  output$impact_post_insight_sent <- renderUI({
    df <- filtered_big_post()
    if (nrow(df) == 0) return(HTML("<p>No Big Post data for current filters.</p>"))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[1], "stood out|idea.*tool")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_post_insight_sent")

  .attach_dual_wc_outputs(output, "impact_post_apply", filtered_big_post, filter_cache_key, POST_TODAY_SESSION_COLS[2], "apply.*learned")
  output$impact_post_apply_sent <- renderUI({
    df <- filtered_big_post()
    if (nrow(df) == 0) return(HTML("<p>No Big Post data for current filters.</p>"))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[2], "apply.*learned")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_post_apply_sent")

  .attach_dual_wc_outputs(output, "impact_post_helpful", filtered_big_post, filter_cache_key, POST_TODAY_SESSION_COLS[3], "helpful.*session")
  output$impact_post_helpful_sent <- renderUI({
    df <- filtered_big_post()
    if (nrow(df) == 0) return(HTML("<p>No Big Post data for current filters.</p>"))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[3], "helpful.*session")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_post_helpful_sent")

  .attach_dual_wc_outputs(output, "impact_post_extra", filtered_big_post, filter_cache_key, POST_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
  output$impact_post_extra_sent <- renderUI({
    df <- filtered_big_post()
    if (nrow(df) == 0) return(HTML("<p>No Big Post data for current filters.</p>"))
    col <- .resolve_col(df, POST_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_post_extra_sent")

  .attach_dual_wc_outputs(output, "impact_post_story", filtered_big_post, filter_cache_key, POST_IMPACT_STORY_COL, "participating.*helped")
  output$impact_post_story_sent <- renderUI({
    df <- filtered_big_post()
    if (nrow(df) == 0) return(HTML("<p>No Big Post data for current filters.</p>"))
    col <- .resolve_col(df, POST_IMPACT_STORY_COL, "participating.*helped")
    if (is.null(col)) return(HTML("<p>Question column not found.</p>"))
    HTML(sentiment_panel_html(df[[col]], include_lexicon_blurb = FALSE, lang_mode = "english"))
  }) %>% bindCache(filter_cache_key(), "impact_post_story_sent")

  output$impact_pre_intention_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_pre_intention_show_responses))
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, PRE_OPENING_COLS[1], "intention.*workshop")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_pre_curiosity_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_pre_curiosity_show_responses))
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, PRE_OPENING_COLS[2], "curiosities|questions.*curiosit")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_pre_hope_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_pre_hope_show_responses))
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, PRE_OPENING_COLS[3], "hope.*feel")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_pre_extra_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_pre_extra_show_responses))
    df <- filtered_big_pre()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, PRE_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_post_insight_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_post_insight_show_responses))
    df <- filtered_big_post()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[1], "stood out|idea.*tool")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_post_apply_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_post_apply_show_responses))
    df <- filtered_big_post()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[2], "apply.*learned")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_post_helpful_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_post_helpful_show_responses))
    df <- filtered_big_post()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, POST_TODAY_SESSION_COLS[3], "helpful.*session")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_post_extra_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_post_extra_show_responses))
    df <- filtered_big_post()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, POST_ADDITIONAL_COMMENTS_COL, "^additional_comments$")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })
  output$impact_post_story_responses_table <- DT::renderDataTable({
    shiny::req(isTRUE(input$impact_post_story_show_responses))
    df <- filtered_big_post()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    col <- .resolve_col(df, POST_IMPACT_STORY_COL, "participating.*helped")
    if (is.null(col)) return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
    .styled_responses_datatable(df[[col]])
  })

  # Workshop Intentions Wordcloud
  output$big_pre_intentions_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    intention_col <- grep("intention.*workshop", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(intention_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[intention_col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    # Limit to top 50 words
    word_freq <- head(word_freq, 50)
    
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#5c2f92", backgroundColor = "white", size = 0.5)
    )
  })
  
  # Workshop Intentions Sentiment
  output$big_pre_intentions_sentiment <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    intention_col <- grep("intention.*workshop", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(intention_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[intention_col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  # Curiosities Wordcloud
  output$big_pre_curiosities_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    curiosities_col <- grep("questions.*curiosities|curiosities", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(curiosities_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[curiosities_col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    word_freq <- head(word_freq, 50)
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#82c341", backgroundColor = "white", size = 0.5)
    )
  })
  
  # Curiosities Sentiment
  output$big_pre_curiosities_sentiment <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    curiosities_col <- grep("questions.*curiosities|curiosities", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(curiosities_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[curiosities_col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  # Hopes Wordcloud
  output$big_pre_hopes_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    hopes_col <- grep("hope.*feel.*end.*workshop", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(hopes_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[hopes_col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    word_freq <- head(word_freq, 50)
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#f58220", backgroundColor = "white", size = 0.5)
    )
  })
  
  # Hopes Sentiment
  output$big_pre_hopes_sentiment <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    hopes_col <- grep("hope.*feel.*end.*workshop", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(hopes_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[hopes_col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  # Comments Wordcloud
  output$big_pre_comments_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    comments_col <- grep("anything else.*share|additional.*comment", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(comments_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[comments_col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    word_freq <- head(word_freq, 50)
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#0076be", backgroundColor = "white", size = 0.5)
    )
  })
  
  # Comments Sentiment
  output$big_pre_comments_sentiment <- renderUI({
    big_pre_data <- filtered_big_pre()
    if (nrow(big_pre_data) == 0) return(HTML("<p>No data available.</p>"))
    
    comments_col <- grep("anything else.*share|additional.*comment", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(comments_col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_pre_data[[comments_col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  output$big_pre_preview <- renderText({
    big_pre_data <- filtered_big_pre()
    paste0("Big pre rows: ", nrow(big_pre_data), "\n",
           "Columns: ", paste(colnames(big_pre_data), collapse = ", "))
  })
  
  # ========================================================================
  # Big Post Tab
  # ========================================================================
  
  output$big_post_summary_stats <- renderUI({
    tryCatch({
      big_post_data <- filtered_big_post()
      
      has_valid_id <- function(ids) {
        !is.na(ids) & ids != "" & !grepl("^ANON#", ids)
      }
      
      total <- nrow(big_post_data)
      with_id <- sum(has_valid_id(big_post_data$respondent_id), na.rm = TRUE)
      without_id <- total - with_id
      
      html <- paste0(
        "<table style='width:100%; border-collapse: collapse; margin: 20px 0;'>",
        "<tr style='background-color: #5c2f92; color: #ffffff;'>",
        "<th style='padding: 10px; text-align: left; border: 1px solid #4a2673;'>Metric</th>",
        "<th style='padding: 10px; text-align: right; border: 1px solid #4a2673;'>Count</th>",
        "</tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6;'>Total Big Post Responses</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", total, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6; padding-left: 20px;'>With user_id</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", with_id, "</td></tr>",
        "<tr><td style='padding: 8px; border: 1px solid #dee2e6; padding-left: 20px;'>Without user_id</td>",
        "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>", without_id, "</td></tr>",
        "</table>"
      )
      
      return(HTML(html))
    }, error = function(e) {
      return(HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
    })
  })
  
  output$big_post_preview <- renderText({
    big_post_data <- filtered_big_post()
    paste0("Big post rows: ", nrow(big_post_data), "\n",
           "Columns: ", paste(colnames(big_post_data), collapse = ", "))
  })
  
  # Variable Mapping Tables (split by survey type)
  output$big_post_variable_mapping_metadata_dt <- DT::renderDataTable({
    tryCatch({
      big_post_data <- filtered_big_post()
      mapping <- get_big_post_variable_mapping(big_post_data)
      mapping_subset <- mapping %>% filter(Survey_Type == "All Surveys")
      
      DT::datatable(
        mapping_subset,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Question Text", "Gross Category", "Fine Category", "Response Type", "Survey Type", "Total Responses", "Central Tendency")
      )
    }, error = function(e) {
      return(DT::datatable(data.frame(Error = as.character(e$message))))
    })
  })
  
  output$big_post_variable_mapping_common_dt <- DT::renderDataTable({
    tryCatch({
      big_post_data <- filtered_big_post()
      mapping <- get_big_post_variable_mapping(big_post_data)
      mapping_subset <- mapping %>% filter(Survey_Type == "Little Post + Big Post")
      
      DT::datatable(
        mapping_subset,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Question Text", "Gross Category", "Fine Category", "Response Type", "Survey Type", "Total Responses", "Central Tendency")
      )
    }, error = function(e) {
      return(DT::datatable(data.frame(Error = as.character(e$message))))
    })
  })
  
  output$big_post_variable_mapping_bigpost_dt <- DT::renderDataTable({
    tryCatch({
      big_post_data <- filtered_big_post()
      mapping <- get_big_post_variable_mapping(big_post_data)
      mapping_subset <- mapping %>% filter(Survey_Type == "Big Post Only")
      
      DT::datatable(
        mapping_subset,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Question Text", "Gross Category", "Fine Category", "Response Type", "Survey Type", "Total Responses", "Central Tendency")
      )
    }, error = function(e) {
      return(DT::datatable(data.frame(Error = as.character(e$message))))
    })
  })
  
  # Workshop Quality & Experience Visualizations
  output$big_post_recommendation_hist <- renderPlotly({
    tryCatch({
      big_post_data <- filtered_big_post()
      if (nrow(big_post_data) == 0) return(plotly_empty())
      col <- grep("How likely are you to recommend", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
      if (length(col) == 0) return(plotly_empty())
      rec_vals <- .coerce_numeric_vec(big_post_data[[col[1]]])
      rec_vals <- rec_vals[is.finite(rec_vals) & rec_vals >= 1 & rec_vals <= 10]
      if (length(rec_vals) == 0) return(plotly_empty())
      bf <- .satisfaction_bar_label_flags()
      h <- hist(rec_vals, breaks = seq(0.5, 10.5, 1), plot = FALSE)
      total_n <- length(rec_vals)
      txt <- .hist_bar_text(h$counts, total_n, bf$n, bf$pct)
      has_txt <- !is.null(txt) && any(nzchar(txt))
      mids <- 1:10
      cols <- REACH_PALETTE[seq_len(10) %% length(REACH_PALETTE) + 1]
      plot_ly(
        x = mids, y = h$counts, type = "bar",
        marker = list(color = cols, line = list(color = "rgba(255,255,255,0.35)", width = 0.3)),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        layout(
          title = "Overall 5 Buckets — Likelihood to recommend (1–10)",
          font = PLOT_FONT,
          xaxis = list(title = "Rating (1–10)", range = c(0.5, 10.5), dtick = 1),
          yaxis = list(title = "Count", range = c(0, max(h$counts, 1) * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
          margin = list(t = 50, b = 50)
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  
  output$big_post_experience_satisfaction_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("How satisfied (are you with your|were you with today's).*5 Buckets", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    sat_vals <- .coerce_numeric_vec(big_post_data[[col[1]]])
    sat_vals <- sat_vals[is.finite(sat_vals)]
    if (length(sat_vals) == 0) return(plotly_empty())
    
    # Count by rating
    counts <- table(factor(sat_vals, levels = 1:5, ordered = TRUE))
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Experience Satisfaction", 
             xaxis = list(title = "Rating (1-5)", categoryorder = "array", categoryarray = as.character(1:5)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_facilitator_satisfaction_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("How satisfied are you with the quality of your facilitator", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    sat_vals <- .coerce_numeric_vec(big_post_data[[col[1]]])
    sat_vals <- sat_vals[is.finite(sat_vals)]
    if (length(sat_vals) == 0) return(plotly_empty())
    
    counts <- table(factor(sat_vals, levels = 1:5, ordered = TRUE))
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#f58220")) %>%
      layout(title = "Facilitator Satisfaction", 
             xaxis = list(title = "Rating (1-5)", categoryorder = "array", categoryarray = as.character(1:5)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  # Facilitator Ratings (Likert scales)
  output$big_post_facilitator_knowledgeable_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("My facilitator was.*Knowledgeable", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(.coerce_atomic_chr(big_post_data[[col[1]]]))
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Facilitator: Knowledgeable", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_facilitator_interactive_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("My facilitator was.*Interactive", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(.coerce_atomic_chr(big_post_data[[col[1]]]))
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Facilitator: Interactive", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_facilitator_relatable_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("My facilitator was.*Relatable", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(.coerce_atomic_chr(big_post_data[[col[1]]]))
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#f58220")) %>%
      layout(title = "Facilitator: Relatable", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_facilitator_engaging_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("My facilitator was.*Engaging", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(.coerce_atomic_chr(big_post_data[[col[1]]]))
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#0076be")) %>%
      layout(title = "Facilitator: Engaging", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_quality_index_hist <- renderPlotly({
    tryCatch({
      big_post_data <- filtered_big_post()
      if (nrow(big_post_data) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(big_post_data)) {
        return(plotly_empty() %>% layout(title = "Session satisfaction column not found", font = PLOT_FONT))
      }
      q <- .coerce_numeric_vec(big_post_data[[POST_SESSION_SATISFACTION_COL]])
      q <- q[is.finite(q) & q >= 1 & q <= SESSION_SATISFACTION_MAX]
      if (length(q) == 0) return(plotly_empty())
      bf <- .satisfaction_bar_label_flags()
      h <- hist(q, breaks = seq(0.5, SESSION_SATISFACTION_MAX + 0.5, 1), plot = FALSE)
      total_n <- length(q)
      txt <- .hist_bar_text(h$counts, total_n, bf$n, bf$pct)
      has_txt <- !is.null(txt) && any(nzchar(txt))
      mids <- seq_len(SESSION_SATISFACTION_MAX)
      cols <- REACH_PALETTE[seq_len(length(h$counts)) %% length(REACH_PALETTE) + 1]
      plot_ly(
        x = mids, y = h$counts, type = "bar",
        marker = list(color = cols, line = list(color = "rgba(255,255,255,0.35)", width = 0.3)),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        layout(
          title = paste0("This workshop session — satisfaction (1–", SESSION_SATISFACTION_MAX, ")"),
          font = PLOT_FONT,
          xaxis = list(title = paste0("Rating (1–", SESSION_SATISFACTION_MAX, ")"), range = c(0.5, SESSION_SATISFACTION_MAX + 0.5), dtick = 1),
          yaxis = list(title = "Count"),
          margin = list(t = 50, b = 50)
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_session_summary <- renderUI({
    d <- filtered_big_post()
    if (nrow(d) == 0 || !POST_SESSION_SATISFACTION_COL %in% colnames(d)) return(NULL)
    q <- .coerce_numeric_vec(d[[POST_SESSION_SATISFACTION_COL]])
    q <- q[is.finite(q) & q >= 1 & q <= SESSION_SATISFACTION_MAX]
    deterministic_summary_box(
      scores = q, mode = "level",
      null_value = (1 + SESSION_SATISFACTION_MAX) / 2,
      positive_threshold = SESSION_SATISFACTION_MAX - 2,
      scale_label = paste0("(1\u2013", SESSION_SATISFACTION_MAX, ")"),
      headline_label = paste0(
        "Highly satisfied (score ", SESSION_SATISFACTION_MAX - 1, " or ", SESSION_SATISFACTION_MAX, ")"
      ),
      scope_text = scope_sentence(input)
    )
  })

  output$satisfaction_recommend_summary <- renderUI({
    d <- filtered_big_post()
    if (nrow(d) == 0) return(NULL)
    col <- grep("How likely are you to recommend", colnames(d), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(NULL)
    r <- .coerce_numeric_vec(d[[col[1]]])
    r <- r[is.finite(r) & r >= 1 & r <= 10]
    deterministic_summary_box(
      scores = r, mode = "level",
      null_value = 5.5, positive_threshold = 8,
      scale_label = "(1\u201310)",
      headline_label = "Very likely to recommend (score 9 or 10)",
      scope_text = scope_sentence(input)
    )
  })

  output$satisfaction_by_module_mean_refined <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"modules_taught" %in% colnames(d)) {
        return(plotly_empty() %>% layout(title = "Need session satisfaction and modules_taught", font = PLOT_FONT))
      }
      long <- .satisfaction_expand_wholecell(d, POST_SESSION_SATISFACTION_COL, "modules_taught")
      .satisfaction_plot_mean_bar(long, "Mean session satisfaction by module — precise (95% CI)")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_module_mean_collapsed <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"modules_taught" %in% colnames(d)) {
        return(plotly_empty() %>% layout(title = "Need session satisfaction and modules_taught", font = PLOT_FONT))
      }
      long <- .satisfaction_expand_modules_collapsed(d, POST_SESSION_SATISFACTION_COL, "modules_taught")
      .satisfaction_plot_mean_bar(long, "Mean session satisfaction by module — aggregated (atomic modules; 95% CI)")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_module_dist_refined <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"modules_taught" %in% colnames(d)) return(plotly_empty())
      long <- .satisfaction_expand_wholecell(d, POST_SESSION_SATISFACTION_COL, "modules_taught")
      .satisfaction_plot_dist_facets(long, "Session satisfaction by module — precise", TRUE, FALSE, FALSE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_module_dist_collapsed <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"modules_taught" %in% colnames(d)) return(plotly_empty())
      long <- .satisfaction_expand_modules_collapsed(d, POST_SESSION_SATISFACTION_COL, "modules_taught")
      .satisfaction_plot_dist_facets(long, "Session satisfaction by module — aggregated", TRUE, FALSE, FALSE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_facilitator_mean_refined <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"facilitators" %in% colnames(d)) {
        return(plotly_empty() %>% layout(title = "Need session satisfaction and facilitators", font = PLOT_FONT))
      }
      long <- .satisfaction_expand_wholecell(d, POST_SESSION_SATISFACTION_COL, "facilitators")
      .satisfaction_plot_mean_bar(long, "Mean session satisfaction by facilitator — precise (95% CI)")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_facilitator_mean_collapsed <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"facilitators" %in% colnames(d)) {
        return(plotly_empty() %>% layout(title = "Need session satisfaction and facilitators", font = PLOT_FONT))
      }
      long <- .satisfaction_expand_facilitators_collapsed(d, POST_SESSION_SATISFACTION_COL, "facilitators")
      .satisfaction_plot_mean_bar(long, "Mean session satisfaction by facilitator — aggregated (95% CI)")
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_facilitator_dist_refined <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"facilitators" %in% colnames(d)) return(plotly_empty())
      long <- .satisfaction_expand_wholecell(d, POST_SESSION_SATISFACTION_COL, "facilitators")
      ff <- satisfaction_facilitator_plot_flags_d()
      .satisfaction_plot_dist_facets(long, "Session satisfaction by facilitator — precise", ff$same_y, ff$n, ff$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_by_facilitator_dist_collapsed <- renderPlotly({
    tryCatch({
      d <- filtered_big_post()
      if (nrow(d) == 0) return(plotly_empty())
      if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"facilitators" %in% colnames(d)) return(plotly_empty())
      long <- .satisfaction_expand_facilitators_collapsed(d, POST_SESSION_SATISFACTION_COL, "facilitators")
      ff <- satisfaction_facilitator_plot_flags_d()
      .satisfaction_plot_dist_facets(long, "Session satisfaction by facilitator — aggregated", ff$same_y, ff$n, ff$pct)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  # --- Facilitator reliability (min-n pooling + empirical-Bayes shrinkage) ---
  satisfaction_facilitator_reliability_d <- reactive({
    d <- filtered_big_post()
    if (nrow(d) == 0) return(NULL)
    if (!POST_SESSION_SATISFACTION_COL %in% colnames(d) || !"facilitators" %in% colnames(d)) return(NULL)
    long <- .satisfaction_expand_facilitators_collapsed(d, POST_SESSION_SATISFACTION_COL, "facilitators")
    min_n <- input$satisfaction_facilitator_min_n
    if (is.null(min_n) || !is.finite(min_n)) min_n <- 5
    .satisfaction_facilitator_reliability(long, min_n = as.integer(min_n))
  })

  output$satisfaction_facilitator_caterpillar <- renderPlotly({
    tryCatch({
      st <- satisfaction_facilitator_reliability_d()
      if (is.null(st) || nrow(st) == 0) return(plotly_empty() %>% layout(title = "No linked facilitator satisfaction rows", font = PLOT_FONT))
      gm <- attr(st, "grand_mean")
      st <- st[order(st$mean), , drop = FALSE]
      st$key <- factor(st$key, levels = st$key)
      cols <- ifelse(st$excludes_grand, "#5c2f92", "#b9a7d4")
      plot_ly(
        st, x = ~mean, y = ~key, type = "scatter", mode = "markers",
        marker = list(color = cols, size = 9),
        error_x = list(type = "data", symmetric = FALSE,
                       array = pmax(0, st$hi - st$mean), arrayminus = pmax(0, st$mean - st$lo),
                       color = "#999999", thickness = 1.2),
        hoverinfo = "text",
        hovertext = ~paste0(key, "<br>n=", n, ", mean=", round(mean, 2),
                            "<br>95% CI [", round(lo, 2), ", ", round(hi, 2), "]",
                            ifelse(excludes_grand, "<br><b>differs from overall</b>", ""))
      ) %>%
        layout(
          title = "Caterpillar — mean \u00B1 95% CI (purple = differs from overall)",
          font = PLOT_FONT,
          xaxis = list(title = "Mean satisfaction", range = c(1, SESSION_SATISFACTION_MAX)),
          yaxis = list(title = ""),
          shapes = list(list(type = "line", x0 = gm, x1 = gm, y0 = -0.5, y1 = nrow(st) - 0.5,
                             line = list(color = "#d9534f", width = 1.4, dash = "dash"))),
          margin = list(l = 220, t = 55, b = 50)
        )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_facilitator_funnel <- renderPlotly({
    tryCatch({
      st <- satisfaction_facilitator_reliability_d()
      if (is.null(st) || nrow(st) == 0) return(plotly_empty() %>% layout(title = "No linked facilitator satisfaction rows", font = PLOT_FONT))
      gm <- attr(st, "grand_mean"); sigma2 <- attr(st, "sigma2")
      sigma <- sqrt(sigma2)
      nseq <- seq(1, max(st$n, 2), length.out = 60)
      hi95 <- gm + 1.96 * sigma / sqrt(nseq)
      lo95 <- gm - 1.96 * sigma / sqrt(nseq)
      hi99 <- gm + 2.576 * sigma / sqrt(nseq)
      lo99 <- gm - 2.576 * sigma / sqrt(nseq)
      p <- plot_ly() %>%
        add_lines(x = nseq, y = hi99, line = list(color = "#e6dcf2", width = 1), name = "99%", hoverinfo = "skip", showlegend = FALSE) %>%
        add_lines(x = nseq, y = lo99, line = list(color = "#e6dcf2", width = 1), name = "99%", hoverinfo = "skip", showlegend = FALSE) %>%
        add_lines(x = nseq, y = hi95, line = list(color = "#b9a7d4", width = 1, dash = "dot"), name = "95%", hoverinfo = "skip", showlegend = FALSE) %>%
        add_lines(x = nseq, y = lo95, line = list(color = "#b9a7d4", width = 1, dash = "dot"), name = "95%", hoverinfo = "skip", showlegend = FALSE) %>%
        add_markers(x = st$n, y = st$mean, marker = list(color = "#5c2f92", size = 9),
                    hoverinfo = "text",
                    text = paste0(st$key, "<br>n=", st$n, ", mean=", round(st$mean, 2)), showlegend = FALSE) %>%
        layout(
          title = "Funnel — mean vs sample size (dashed = 95% / 99% limits)",
          font = PLOT_FONT,
          xaxis = list(title = "Responses (n)"),
          yaxis = list(title = "Mean satisfaction", range = c(1, SESSION_SATISFACTION_MAX)),
          shapes = list(list(type = "line", x0 = 1, x1 = max(st$n, 2), y0 = gm, y1 = gm,
                             line = list(color = "#d9534f", width = 1.4, dash = "dash"))),
          margin = list(l = 60, t = 55, b = 50)
        )
      p
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })

  output$satisfaction_facilitator_reliability_table <- DT::renderDataTable({
    st <- satisfaction_facilitator_reliability_d()
    if (is.null(st) || nrow(st) == 0) {
      return(DT::datatable(data.frame(Message = "No linked facilitator satisfaction rows"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    gm <- attr(st, "grand_mean")
    df <- data.frame(
      Facilitator = st$key,
      `Responses (n)` = st$n,
      `Raw mean` = round(st$mean, 2),
      `Shrunken mean` = round(st$shrunk, 2),
      `vs overall` = round(st$shrunk - gm, 2),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    DT::datatable(
      df, rownames = FALSE, class = "vm-coverage compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = TRUE, order = list(list(3, "desc")),
                     columnDefs = list(list(className = "dt-center", targets = 1:4)))
    ) %>%
      DT::formatStyle("Shrunken mean", fontWeight = "bold") %>%
      DT::formatStyle("vs overall",
                      color = DT::styleInterval(c(-0.001, 0.001), c("#b54b3a", "#5f6369", "#3c7a3c")))
  })

  # Topic understanding — single distribution plot (Bars and/or Curve), matching the
  # Financial Wellness / Behavioral plots. Scores live on the 4-point agreement scale
  # (Strongly Disagree -3, Disagree -1, Agree +1, Strongly Agree +3); the x-axis is
  # labeled at those 4 anchors only (no bare -2/0/2 ticks).
  topic_understanding_plot_rx <- reactive({
    ana <- topic_understanding_analysis_rx()
    if (is.null(ana) || length(ana$scores) < 1) {
      return(plotly_empty() %>% layout(title = "No topic understanding responses for current filters", font = PLOT_FONT))
    }
    show_bars <- tryCatch(isTRUE(input$topic_understanding_bars), error = function(e) TRUE)
    show_curves <- tryCatch(isTRUE(input$topic_understanding_curves), error = function(e) TRUE)
    if (!show_bars && !show_curves) {
      return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
    }
    sc <- ana$scores
    total_n <- length(sc)
    show_n <- isTRUE(input$opt_show_n)
    show_pct <- isTRUE(input$opt_show_pct)
    pos <- c(-3, -1, 1, 3)
    pos_lab <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
    pos_cols <- c("#b71c1c", "#e65100", "#5c2f92", "#2e7d32")
    cnt <- vapply(pos, function(v) sum(abs(sc - v) < 1e-6), integer(1))
    p <- plot_ly()
    ymax <- max(c(cnt, 1))
    if (show_bars) {
      txt <- rep("", length(pos))
      if (show_n || show_pct) {
        for (i in seq_along(pos)) {
          parts <- character(0)
          if (show_n) parts <- c(parts, paste0("n=", cnt[i]))
          if (show_pct && total_n > 0) parts <- c(parts, paste0(sprintf("%.1f", 100 * cnt[i] / total_n), "%"))
          txt[i] <- paste(parts, collapse = " ")
        }
      }
      has_txt <- any(nzchar(txt))
      p <- p %>% add_trace(
        x = pos, y = cnt, type = "bar", name = "Count", width = 1.1,
        marker = list(color = pos_cols, line = list(color = "rgba(0,0,0,0.12)", width = 1)),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE
      )
    }
    if (show_curves && total_n >= 2) {
      dd <- tryCatch(stats::density(sc, from = -3, to = 3, n = 256, bw = 0.6), error = function(e) NULL)
      if (!is.null(dd)) {
        ycurve <- dd$y * total_n * 2  # anchors are spaced 2 apart; scale density to counts
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = dd$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = "#5c2f92", width = 2), fill = "tozeroy", fillcolor = "rgba(92,47,146,0.12)"
        )
      }
    }
    mean_sc <- mean(sc)
    mean_shapes <- if (is.finite(mean_sc)) .mean_vline_outline_shapes(mean_sc, ymax, "#5c2f92") else list()
    headroom <- if (show_n || show_pct) 1.22 else 1.08
    p %>%
      layout(
        title = list(text = "Better understanding of topics covered", font = PLOT_FONT),
        font = PLOT_FONT,
        barmode = "overlay", bargap = 0.05,
        xaxis = list(title = "", range = c(-3.7, 3.7),
                     tickvals = pos, ticktext = paste0(pos_lab, "<br>(", pos, ")")),
        yaxis = list(title = "Count", range = c(0, ymax * headroom), rangemode = "nonnegative"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18, yanchor = "top"),
        margin = list(t = 56, b = 80),
        shapes = mean_shapes
      )
  })
  output$big_post_impact_understanding_hist <- renderPlotly({
    topic_understanding_plot_rx()
  })
  output$big_post_impact_understanding_hist_survey <- renderPlotly({
    topic_understanding_plot_rx()
  })

  topic_understanding_analysis_rx <- reactive({
    if (!isTRUE(data_ready())) return(NULL)
    d <- filtered_big_post()
    if (nrow(d) == 0) return(NULL)
    col <- grep("better understanding of the topics", colnames(d), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(NULL)
    raw <- as.character(d[[col[1]]])
    sc <- likert_to_numeric(raw)
    ok <- !is.na(sc)
    if (!any(ok)) return(NULL)
    list(
      df = d[ok, , drop = FALSE],
      col = col[1],
      raw = raw[ok],
      scores = as.numeric(sc[ok])
    )
  })

  output$topic_understanding_summary_ui <- renderUI({
    ana <- topic_understanding_analysis_rx()
    if (is.null(ana)) {
      return(div(class = "text-muted", style = "margin: 10px 0;", "No topic understanding responses for current filters."))
    }
    deterministic_summary_box(
      scores = ana$scores,
      mode = "level",
      null_value = AGREE_SCALE_NULL,
      positive_threshold = 0,
      scale_label = "(\u22123 to +3)",
      headline_label = "Agree or Strongly Agree",
      scope_text = scope_sentence(input)
    )
  })

  output$topic_understanding_sd_table <- DT::renderDataTable({
    req(isTRUE(input$topic_understanding_explore_sd))
    ana <- topic_understanding_analysis_rx()
    if (is.null(ana)) {
      return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    }
    sub <- ana$df[trimws(as.character(ana$df[[ana$col]])) == "Strongly Disagree", , drop = FALSE]
    if (nrow(sub) == 0) {
      return(DT::datatable(data.frame(Message = "No Strongly Disagree responses for current filters."), rownames = FALSE, options = list(dom = "t")))
    }
    want <- c("session_id", "session_date", "org_name", "group", "facilitators", "modules_taught", "timestamp")
    demo <- grep("Age Group|highest level of education|Gender Identity|respondent", colnames(sub), ignore.case = TRUE, value = TRUE)
    want <- unique(c(intersect(want, colnames(sub)), demo, ana$col))
    out <- sub[, want, drop = FALSE]
    DT::datatable(out, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })
  
  output$big_post_impact_optimism_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("more optimistic about my financial future", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_post_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "More Optimistic About Financial Future", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_relationship_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("healthier relationship with money", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_post_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#f58220")) %>%
      layout(title = "Healthier Relationship with Money", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_stress_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("feel less stress about my finances|better able to manage stress|stress.*financ", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_post_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#0076be")) %>%
      layout(title = "Less Stress About Finances", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_confidence_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("more confident I can plan ahead", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_post_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "More Confident in Planning", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_comfort_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("more comfortable speaking with financial professionals", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    data <- get_ordered_likert_table(big_post_data[[col[1]]])
    p <- plot_ly(x = names(data), y = as.numeric(data), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "More Comfortable with Financial Professionals", 
             xaxis = list(title = "Response", categoryorder = "array", 
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_index_hist <- renderPlotly({
    big_post_data <- filtered_big_post_only()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    impact_index <- calculate_post_impact_index(big_post_data)
    impact_index <- impact_index[!is.na(impact_index)]
    if (length(impact_index) == 0) return(plotly_empty())
    
    p <- plot_ly(x = impact_index, type = "histogram", nbinsx = 20,
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Post-Workshop Impact Index Distribution (9 Questions)", 
             xaxis = list(title = "Average Impact Score (−3 to +3)", range = c(-3, 3)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_impact_radar <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    # Find all 6 impact question columns
    impact_cols <- grep("Compared to before (today's session|your 5 Buckets experience)", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(impact_cols) < 1) return(plotly_empty())
    
    # Calculate averages for each dimension
    impact_means <- sapply(impact_cols, function(col) {
      vals <- likert_to_numeric(big_post_data[[col]])
      mean(vals, na.rm = TRUE)
    })
    
    # Create radar chart (spider chart)
    dim_names <- c("Understanding", "Optimism", "Relationship", "Stress Reduction", "Confidence", "Comfort")
    if (length(dim_names) != length(impact_means)) {
      dim_names <- gsub("Compared to before (today's session with 5 Buckets|your 5 Buckets experience)\\.\\.\\. \\[(.*)\\]", "\\2", names(impact_means))
    }
    
    p <- plot_ly(
      type = 'scatterpolar',
      r = c(impact_means, impact_means[1]),  # Close the loop
      theta = c(dim_names, dim_names[1]),
      fill = 'toself',
      name = 'Average Impact'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(-3, 3)
          )
        ),
        title = "Average Post-Workshop Impact Profile"
      )
    
    return(p)
  })
  
  # Planned Actions Visualizations (Feb 2026: single comma-separated column; old: 8 separate columns)
  output$big_post_planned_actions_heatmap <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    action_cols <- grep("As a result of (this workshop, I plan to take|my workshop\\(s\\) with 5 Buckets)", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(action_cols) == 0) return(plotly_empty())
    
    # Feb 2026: single comma-separated column -> horizontal bar chart (match Past Behaviors style)
    if (length(action_cols) == 1L && exists("get_planned_actions_counts")) {
      counts_df <- get_planned_actions_counts(big_post_data)
      if (nrow(counts_df) == 0) return(plotly_empty())
      vec <- big_post_data[[action_cols[1]]]
      n_den <- sum(!is.na(vec) & nzchar(trimws(as.character(vec))))
      bf <- .behavioral_bar_label_flags()
      htxt <- .behavior_horiz_bar_labels(counts_df$count, n_den, bf$n, bf$pct)
      has_txt <- !is.null(htxt) && any(nzchar(htxt))
      colors <- REACH_PALETTE[seq_len(nrow(counts_df)) %% length(REACH_PALETTE) + 1]
      hovertext <- paste0(
        counts_df$action, ": ", counts_df$count,
        if (n_den > 0) paste0(" (", sprintf("%.0f%%", counts_df$count / n_den * 100), " of ", n_den, " with an answer)") else ""
      )
      p <- plot_ly(
        counts_df,
        x = ~count,
        y = ~reorder(action, count),
        type = "bar",
        orientation = "h",
        marker = list(color = colors),
        text = if (has_txt) htxt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE,
        hoverinfo = "text",
        hovertext = hovertext
      ) %>%
        layout(
          title = "Planned Actions (counts)",
          xaxis = list(title = "Count"),
          yaxis = list(title = ""),
          margin = list(l = 200, r = 40)
        )
      return(p)
    }
    
    # Create matrix: actions × responses (old format)
    action_names <- gsub("As a result of this workshop, I plan to take the following actions in the next 30 days: \\[(.*)\\]", "\\1", action_cols)
    action_names <- gsub("^\\s+|\\s+$", "", action_names)
    
    response_types <- c("No", "Maybe", "Yes")  # Ordered left to right
    heatmap_data <- matrix(0, nrow = length(action_cols), ncol = length(response_types))
    rownames(heatmap_data) <- action_names
    colnames(heatmap_data) <- response_types
    
    for (i in 1:length(action_cols)) {
      responses <- big_post_data[[action_cols[i]]]
      counts <- table(responses, useNA = "no")
      
      # Map Yes variations to "Yes"
      yes_count <- sum(counts[names(counts) %in% c("Yes", "Yes!!", "Yes!")], na.rm = TRUE)
      no_count <- sum(counts[names(counts) == "No"], na.rm = TRUE)
      maybe_count <- sum(counts[names(counts) == "Maybe"], na.rm = TRUE)
      
      heatmap_data[i, "No"] <- no_count
      heatmap_data[i, "Maybe"] <- maybe_count
      heatmap_data[i, "Yes"] <- yes_count
    }
    
    # Order rows by Yes count (descending), then Maybe count (descending)
    yes_counts <- heatmap_data[, "Yes"]
    maybe_counts <- heatmap_data[, "Maybe"]
    row_order <- order(-yes_counts, -maybe_counts)
    heatmap_data <- heatmap_data[row_order, , drop = FALSE]
    
    # Create text matrix with cell numbers
    text_matrix <- matrix(paste0("Count: ", as.vector(heatmap_data)), 
                         nrow = nrow(heatmap_data), ncol = ncol(heatmap_data))
    
    # Create heatmap with Big Pre color scale
    p <- plot_ly(
      x = colnames(heatmap_data),
      y = rownames(heatmap_data),
      z = heatmap_data,
      type = "heatmap",
      colorscale = list(
        c(0, "#f8f9fa"),
        c(0.25, "#e0e0e0"),
        c(0.5, "#b0b0b0"),
        c(0.75, "#7a7a7a"),
        c(1, "#5c2f92")
      ),
      text = text_matrix,
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Planned Actions Heatmap",
        xaxis = list(
          title = "Response",
          categoryorder = "array",
          categoryarray = response_types
        ),
        yaxis = list(
          title = "Action",
          autorange = "reversed"  # Flip y-axis
        )
      )
    
    return(p)
  })
  
  output$big_post_planned_actions_index_hist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    actions_index <- calculate_planned_actions_index(big_post_data)
    actions_index <- actions_index[!is.na(actions_index)]
    if (length(actions_index) == 0) return(plotly_empty())
    
    # Ensure actions_index is a vector, not a matrix
    if (is.matrix(actions_index) || is.array(actions_index)) {
      actions_index <- as.vector(actions_index)
    }
    
    # Ensure numeric vector
    actions_index <- as.numeric(actions_index)
    actions_index <- actions_index[!is.na(actions_index)]
    if (length(actions_index) == 0) return(plotly_empty())
    
    # Create data frame for plotly
    plot_df <- data.frame(value = actions_index)
    
    # Create histogram
    p <- plot_ly(data = plot_df, x = ~value, type = "histogram", 
                 xbins = list(start = 0, end = 8, size = 1),
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Planned Actions Index Distribution (0-8)", 
             xaxis = list(title = "Number of Planned Actions", range = c(-0.5, 8.5)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_action_oriented_planned <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    action_cols <- grep("As a result of this workshop, I plan to take", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(action_cols) < 4) return(plotly_empty())
    
    # First 4 are action-oriented
    action_cols <- action_cols[1:4]
    action_names <- gsub("As a result of this workshop, I plan to take the following actions in the next 30 days: \\[(.*)\\]", "\\1", action_cols)
    action_names <- gsub("^\\s+|\\s+$", "", action_names)
    
    yes_counts <- sapply(action_cols, function(col) {
      responses <- big_post_data[[col]]
      sum(responses %in% c("Yes", "Yes!!", "Yes!"), na.rm = TRUE)
    })
    
    p <- plot_ly(x = action_names, y = yes_counts, type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Action-Oriented Planned Behaviors", 
             xaxis = list(title = "Behavior", tickangle = -45),
             yaxis = list(title = "Count of 'Yes' Responses"))
    return(p)
  })
  
  output$big_post_awareness_social_planned <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    action_cols <- grep("As a result of this workshop, I plan to take", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(action_cols) < 8) return(plotly_empty())
    
    # Last 4 are awareness/social (indices 5-8)
    action_cols <- action_cols[5:8]
    action_names <- gsub("As a result of this workshop, I plan to take the following actions in the next 30 days: \\[(.*)\\]", "\\1", action_cols)
    action_names <- gsub("^\\s+|\\s+$", "", action_names)
    
    yes_counts <- sapply(action_cols, function(col) {
      responses <- big_post_data[[col]]
      sum(responses %in% c("Yes", "Yes!!", "Yes!"), na.rm = TRUE)
    })
    
    p <- plot_ly(x = action_names, y = yes_counts, type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Awareness & Social Planned Behaviors", 
             xaxis = list(title = "Behavior", tickangle = -45),
             yaxis = list(title = "Count of 'Yes' Responses"))
    return(p)
  })
  
  # Demographics (reuse patterns from Big Pre, but need to handle post-specific columns)
  output$big_post_age_dist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("^Age Group$", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    age_ordered <- order_age_groups(big_post_data[[col[1]]])
    counts <- table(age_ordered, useNA = "no")
    counts <- counts[counts > 0]  # drop empty brackets (e.g. legacy "Under 18" with no responses)
    if (length(counts) == 0) return(plotly_empty())
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Age Distribution", 
             xaxis = list(title = "Age Group", categoryorder = "array", categoryarray = names(counts)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_gender_dist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("^Gender Identity", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    counts <- table(big_post_data[[col[1]]], useNA = "no")
    
    p <- plot_ly(labels = names(counts), values = as.numeric(counts), type = "pie",
                 marker = list(colors = c("#5c2f92", "#82c341", "#f58220", "#0076be"))) %>%
      layout(title = "Gender Distribution")
    return(p)
  })
  
  output$big_post_income_dist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("^Household Income", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    income_ordered <- order_income_levels(big_post_data[[col[1]]])
    counts <- table(income_ordered, useNA = "no")
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#82c341")) %>%
      layout(title = "Household Income Distribution", 
             xaxis = list(title = "Income Level", tickangle = -45),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_education_dist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("highest level of education", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    edu_ordered <- order_education_levels(big_post_data[[col[1]]])
    counts <- table(edu_ordered, useNA = "no")
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#f58220")) %>%
      layout(title = "Education Level Distribution", 
             xaxis = list(title = "Education Level", tickangle = -45),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  output$big_post_race_ethnicity_dist <- renderPlotly({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(plotly_empty())
    
    col <- grep("^Race.*Select all that apply", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(plotly_empty())
    
    # Multi-select: paren-aware split, fold non-preset values into "Other", canonical order.
    all_races <- .reach2_race_vec(big_post_data[[col[1]]])
    if (length(all_races) == 0) return(plotly_empty())
    
    counts <- table(all_races)
    ord <- c(intersect(RACE_LEVELS_CANONICAL, names(counts)), sort(setdiff(names(counts), RACE_LEVELS_CANONICAL)))
    counts <- counts[ord]
    
    p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                 marker = list(color = "#0076be")) %>%
      layout(title = "Race/Ethnicity Distribution", 
             xaxis = list(title = "Race/Ethnicity", tickangle = -45,
                          categoryorder = "array", categoryarray = names(counts)),
             yaxis = list(title = "Count"))
    return(p)
  })
  
  # Text Analysis for Open-Ended Questions
  # Learnings Wordcloud
  output$big_post_learnings_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_TODAY_SESSION_COLS[1], "most notable|stood out|idea.*tool")
    if (is.null(col)) return(HTML("<p>Question not found (insight from today).</p>"))
    responses <- big_post_data[[col]]
    word_freq <- create_word_freq(responses)
    .wc_reach_palette(word_freq, size = 0.5)
  })

  output$big_post_learnings_sentiment <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_TODAY_SESSION_COLS[1], "most notable|stood out|idea.*tool")
    if (is.null(col)) return(HTML("<p>Question not found.</p>"))
    HTML(sentiment_panel_html(big_post_data[[col]]))
  })

  # Changes Wordcloud (Feb 2026: "applied learning" open text)
  output$big_post_changes_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_TODAY_SESSION_COLS[2], "apply.*learned|change.*finances")
    if (is.null(col)) return(HTML("<p>Question not found (applied learning).</p>"))
    responses <- big_post_data[[col]]
    word_freq <- create_word_freq(responses)
    .wc_reach_palette(word_freq, size = 0.5)
  })

  output$big_post_changes_sentiment <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_TODAY_SESSION_COLS[2], "apply.*learned|change.*finances")
    if (is.null(col)) return(HTML("<p>Question not found.</p>"))
    HTML(sentiment_panel_html(big_post_data[[col]]))
  })
  
  # Impact Story Wordcloud (legacy output id; Survey-centric tab may still reference)
  output$big_post_impact_story_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_IMPACT_STORY_COL, "How has participating.*helped")
    if (is.null(col)) return(HTML("<p>Question not found (impact story).</p>"))
    responses <- big_post_data[[col]]
    word_freq <- create_word_freq(responses)
    .wc_reach_palette(word_freq, size = 0.45)
  })

  output$big_post_impact_story_sentiment <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    col <- .resolve_col(big_post_data, POST_IMPACT_STORY_COL, "How has participating.*helped")
    if (is.null(col)) return(HTML("<p>Question not found.</p>"))
    HTML(sentiment_panel_html(big_post_data[[col]]))
  })
  
  # Future Topics Wordcloud
  output$big_post_future_topics_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    
    col <- grep("personal finance topic.*learn more", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_post_data[[col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    word_freq <- head(word_freq, 50)
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#0076be", backgroundColor = "white", size = 0.5)
    )
  })
  
  output$big_post_future_topics_sentiment <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    
    col <- grep("personal finance topic.*learn more", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_post_data[[col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  # Comments Wordcloud
  output$big_post_comments_wordcloud <- renderUI({
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      return(HTML("<p>wordcloud2 package not installed.<br>Install with: install.packages('wordcloud2')</p>"))
    }
    
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    
    col <- grep("Anything else.*share.*team", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_post_data[[col[1]]]
    word_freq <- create_word_freq(responses)
    
    if (nrow(word_freq) == 0) return(HTML("<p>No valid responses after filtering.</p>"))
    
    word_freq <- head(word_freq, 50)
    htmltools::div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 280px; overflow: hidden;",
      wordcloud2::wordcloud2(word_freq, color = "#797d82", backgroundColor = "white", size = 0.5)
    )
  })
  
  output$big_post_comments_sentiment <- renderUI({
    big_post_data <- filtered_big_post()
    if (nrow(big_post_data) == 0) return(HTML("<p>No data available.</p>"))
    
    col <- grep("Anything else.*share.*team", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(col) == 0) return(HTML("<p>Question not found.</p>"))
    
    responses <- big_post_data[[col[1]]]
    sentiment <- calculate_sentiment(responses)
    
    na_count <- ifelse(is.null(sentiment$na_count), 0, sentiment$na_count)
    analyzed_count <- sentiment$total - na_count
    
    HTML(paste0(
      "<h5>Sentiment Analysis</h5>",
      "<p><strong>Overall Sentiment:</strong> ", sentiment$sentiment, "</p>",
      "<p><strong>Average Score:</strong> ", round(sentiment$score, 3), "</p>",
      if (analyzed_count > 0) {
        paste0(
          "<p><strong>Positive Responses:</strong> ", sentiment$positive, " (", round(100 * sentiment$positive / analyzed_count, 1), "%)</p>",
          "<p><strong>Neutral Responses:</strong> ", sentiment$neutral, " (", round(100 * sentiment$neutral / analyzed_count, 1), "%)</p>",
          "<p><strong>Negative Responses:</strong> ", sentiment$negative, " (", round(100 * sentiment$negative / analyzed_count, 1), "%)</p>"
        )
      } else {
        "<p><em>No responses available for sentiment analysis</em></p>"
      },
      "<p><strong>Analyzed Responses:</strong> ", analyzed_count, "</p>",
      if (na_count > 0) {
        paste0("<p><strong>Non-Responses Filtered:</strong> ", na_count, " (e.g., 'no', 'nope', very short responses)</p>")
      },
      "<p><strong>Total Responses:</strong> ", sentiment$total, "</p>"
    ))
  })
  
  # ========================================================================
  # Big Pre-Post Pairs Tab
  # ========================================================================
  
  # Financial Wellness Comparisons
  output$pairs_optimism_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Get Pre data
    pre_col <- grep("optimistic.*financial future", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_col) == 0) return(plotly_empty())
    pre_data <- get_ordered_likert_table(big_pre_data[[pre_col[1]]])
    
    # Get Post data
    post_col <- grep("more optimistic.*financial future", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(post_col) == 0) return(plotly_empty())
    post_data <- get_ordered_likert_table(big_post_data[[post_col[1]]])
    
    # Create side-by-side comparison
    # Use numeric positions (−3,−1,1,3) for bars so they align with the density curves
    likert_levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
    likert_numeric <- c(-3, -1, 1, 3)
    
    # Map pre_data and post_data to numeric positions
    pre_numeric_pos <- likert_numeric[match(names(pre_data), likert_levels)]
    post_numeric_pos <- likert_numeric[match(names(post_data), likert_levels)]
    
    p <- plot_ly() %>%
      add_trace(x = pre_numeric_pos, y = as.numeric(pre_data), type = "bar", 
                name = "Pre (Baseline)", marker = list(color = "#82c341"),
                text = names(pre_data), textposition = "outside") %>%
      add_trace(x = post_numeric_pos, y = as.numeric(post_data), type = "bar",
                name = "Post (Change)", marker = list(color = "#5c2f92"),
                text = names(post_data), textposition = "outside") %>%
      layout(title = "Financial Optimism: Pre vs Post",
             xaxis = list(title = "Response", 
                         tickmode = "array",
                         tickvals = c(-3, -1, 1, 3),
                         ticktext = likert_levels,
                         range = c(-3.6, 3.6)),
             yaxis = list(title = "Count"),
             barmode = "group")
    
    # Add bell curves if checkbox is checked
    if (input$pairs_show_bell_curves) {
      # Convert to numeric for density
      pre_numeric <- likert_to_numeric(big_pre_data[[pre_col[1]]])
      post_numeric <- likert_to_numeric(big_post_data[[post_col[1]]])
      pre_numeric <- pre_numeric[!is.na(pre_numeric)]
      post_numeric <- post_numeric[!is.na(post_numeric)]
      
      if (length(pre_numeric) > 0 && length(post_numeric) > 0) {
        # Create density curves with more points for smoothness
        pre_density <- density(pre_numeric, from = -3, to = 3, n = 200, adjust = 1.5)
        post_density <- density(post_numeric, from = -3, to = 3, n = 200, adjust = 1.5)
        
        # Scale density to match bar heights
        max_count <- max(c(as.numeric(pre_data), as.numeric(post_data)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.8
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.8
        
        # Use numeric x values for smooth curves
        p <- p %>%
          add_trace(x = pre_density$x, y = pre_density$y * pre_scale, 
                    type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#82c341", width = 3),
                    opacity = 0.6, showlegend = TRUE) %>%
          add_trace(x = post_density$x, y = post_density$y * post_scale, 
                    type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 3),
                    opacity = 0.6, showlegend = TRUE)
      }
    }
    
    return(p)
  })
  
  output$pairs_relationship_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_col <- grep("healthy relationship.*money", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    post_col <- grep("healthier relationship.*money", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_col) == 0 || length(post_col) == 0) return(plotly_empty())
    
    pre_data <- get_ordered_likert_table(big_pre_data[[pre_col[1]]])
    post_data <- get_ordered_likert_table(big_post_data[[post_col[1]]])
    
    p <- plot_ly() %>%
      add_trace(x = names(pre_data), y = as.numeric(pre_data), type = "bar",
                name = "Pre (Baseline)", marker = list(color = "#82c341")) %>%
      add_trace(x = names(post_data), y = as.numeric(post_data), type = "bar",
                name = "Post (Change)", marker = list(color = "#5c2f92")) %>%
      layout(title = "Money Relationship: Pre vs Post",
             xaxis = list(title = "Response", categoryorder = "array",
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"),
             barmode = "group")
    
    # Add bell curves if checkbox is checked
    if (input$pairs_show_bell_curves) {
      pre_numeric <- likert_to_numeric(big_pre_data[[pre_col[1]]])
      post_numeric <- likert_to_numeric(big_post_data[[post_col[1]]])
      pre_numeric <- pre_numeric[!is.na(pre_numeric)]
      post_numeric <- post_numeric[!is.na(post_numeric)]
      
      if (length(pre_numeric) > 0 && length(post_numeric) > 0) {
        pre_density <- density(pre_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        post_density <- density(post_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        max_count <- max(c(as.numeric(pre_data), as.numeric(post_data)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.8
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.8
        
        likert_levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
        map_to_category <- function(x_val) {
          scores <- c(-3, -1, 1, 3)  # SD, D, A, SA
          likert_levels[which.min(abs(x_val - scores))]
        }
        
        pre_cat_x <- sapply(pre_density$x, map_to_category)
        post_cat_x <- sapply(post_density$x, map_to_category)
        
        pre_curve_df <- data.frame(x = pre_cat_x, y = pre_density$y * pre_scale)
        post_curve_df <- data.frame(x = post_cat_x, y = post_density$y * post_scale)
        
        p <- p %>%
          add_trace(data = pre_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#82c341", width = 3), opacity = 0.6) %>%
          add_trace(data = post_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 3), opacity = 0.6)
      }
    }
    
    return(p)
  })
  
  output$pairs_stress_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_col <- grep("stressed.*financ|able to manage stress", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    post_col <- grep("feel less stress.*finances", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_col) == 0 || length(post_col) == 0) return(plotly_empty())
    
    pre_data <- get_ordered_likert_table(big_pre_data[[pre_col[1]]])
    post_data <- get_ordered_likert_table(big_post_data[[post_col[1]]])
    
    p <- plot_ly() %>%
      add_trace(x = names(pre_data), y = as.numeric(pre_data), type = "bar",
                name = "Pre Stress", marker = list(color = "#f58220")) %>%
      add_trace(x = names(post_data), y = as.numeric(post_data), type = "bar",
                name = "Post (Less Stress)", marker = list(color = "#5c2f92")) %>%
      layout(title = "Financial Stress: Pre vs Post (Less Stress = Better)",
             xaxis = list(title = "Response", categoryorder = "array",
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"),
             barmode = "group")
    
    if (input$pairs_show_bell_curves) {
      pre_numeric <- likert_to_numeric(big_pre_data[[pre_col[1]]])
      post_numeric <- likert_to_numeric(big_post_data[[post_col[1]]])
      pre_numeric <- pre_numeric[!is.na(pre_numeric)]
      post_numeric <- post_numeric[!is.na(post_numeric)]
      
      if (length(pre_numeric) > 0 && length(post_numeric) > 0) {
        pre_density <- density(pre_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        post_density <- density(post_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        max_count <- max(c(as.numeric(pre_data), as.numeric(post_data)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.3
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.3
        
        p <- p %>%
          add_trace(x = pre_density$x, y = pre_density$y * pre_scale, type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#f58220", width = 2, dash = "dash")) %>%
          add_trace(x = post_density$x, y = post_density$y * post_scale, type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 2, dash = "dash"))
      }
    }
    
    return(p)
  })
  
  output$pairs_confidence_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_col <- grep("confident.*plan", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    post_col <- grep("more confident.*plan", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_col) == 0 || length(post_col) == 0) return(plotly_empty())
    
    pre_data <- get_ordered_likert_table(big_pre_data[[pre_col[1]]])
    post_data <- get_ordered_likert_table(big_post_data[[post_col[1]]])
    
    p <- plot_ly() %>%
      add_trace(x = names(pre_data), y = as.numeric(pre_data), type = "bar",
                name = "Pre (Baseline)", marker = list(color = "#82c341")) %>%
      add_trace(x = names(post_data), y = as.numeric(post_data), type = "bar",
                name = "Post (Change)", marker = list(color = "#5c2f92")) %>%
      layout(title = "Planning Confidence: Pre vs Post",
             xaxis = list(title = "Response", categoryorder = "array",
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"),
             barmode = "group")
    
    if (input$pairs_show_bell_curves) {
      pre_numeric <- likert_to_numeric(big_pre_data[[pre_col[1]]])
      post_numeric <- likert_to_numeric(big_post_data[[post_col[1]]])
      pre_numeric <- pre_numeric[!is.na(pre_numeric)]
      post_numeric <- post_numeric[!is.na(post_numeric)]
      
      if (length(pre_numeric) > 0 && length(post_numeric) > 0) {
        pre_density <- density(pre_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        post_density <- density(post_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        max_count <- max(c(as.numeric(pre_data), as.numeric(post_data)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.8
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.8
        
        likert_levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
        map_to_category <- function(x_val) {
          scores <- c(-3, -1, 1, 3)  # SD, D, A, SA
          likert_levels[which.min(abs(x_val - scores))]
        }
        
        pre_cat_x <- sapply(pre_density$x, map_to_category)
        post_cat_x <- sapply(post_density$x, map_to_category)
        
        pre_curve_df <- data.frame(x = pre_cat_x, y = pre_density$y * pre_scale)
        post_curve_df <- data.frame(x = post_cat_x, y = post_density$y * post_scale)
        
        p <- p %>%
          add_trace(data = pre_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#82c341", width = 3), opacity = 0.6) %>%
          add_trace(data = post_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 3), opacity = 0.6)
      }
    }
    
    return(p)
  })
  
  output$pairs_comfort_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_col <- grep("comfortable.*speaking.*financial professional", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    post_col <- grep("more comfortable.*speaking.*financial professionals", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_col) == 0 || length(post_col) == 0) return(plotly_empty())
    
    pre_data <- get_ordered_likert_table(big_pre_data[[pre_col[1]]])
    post_data <- get_ordered_likert_table(big_post_data[[post_col[1]]])
    
    p <- plot_ly() %>%
      add_trace(x = names(pre_data), y = as.numeric(pre_data), type = "bar",
                name = "Pre (Baseline)", marker = list(color = "#82c341")) %>%
      add_trace(x = names(post_data), y = as.numeric(post_data), type = "bar",
                name = "Post (Change)", marker = list(color = "#5c2f92")) %>%
      layout(title = "Professional Comfort: Pre vs Post",
             xaxis = list(title = "Response", categoryorder = "array",
                         categoryarray = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")),
             yaxis = list(title = "Count"),
             barmode = "group")
    
    if (input$pairs_show_bell_curves) {
      pre_numeric <- likert_to_numeric(big_pre_data[[pre_col[1]]])
      post_numeric <- likert_to_numeric(big_post_data[[post_col[1]]])
      pre_numeric <- pre_numeric[!is.na(pre_numeric)]
      post_numeric <- post_numeric[!is.na(post_numeric)]
      
      if (length(pre_numeric) > 0 && length(post_numeric) > 0) {
        pre_density <- density(pre_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        post_density <- density(post_numeric, from = -3, to = 3, n = 100, adjust = 1.5)
        max_count <- max(c(as.numeric(pre_data), as.numeric(post_data)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.8
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.8
        
        likert_levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
        map_to_category <- function(x_val) {
          scores <- c(-3, -1, 1, 3)  # SD, D, A, SA
          likert_levels[which.min(abs(x_val - scores))]
        }
        
        pre_cat_x <- sapply(pre_density$x, map_to_category)
        post_cat_x <- sapply(post_density$x, map_to_category)
        
        pre_curve_df <- data.frame(x = pre_cat_x, y = pre_density$y * pre_scale)
        post_curve_df <- data.frame(x = post_cat_x, y = post_density$y * post_scale)
        
        p <- p %>%
          add_trace(data = pre_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#82c341", width = 3), opacity = 0.6) %>%
          add_trace(data = post_curve_df, x = ~x, y = ~y, type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 3), opacity = 0.6)
      }
    }
    
    return(p)
  })
  
  output$pairs_wellness_index_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post_only()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_index <- calculate_wellness_index(big_pre_data)
    post_index <- calculate_post_impact_index(big_post_data)
    
    # Ensure vectors, not matrices
    if (is.matrix(pre_index) || is.array(pre_index)) pre_index <- as.vector(pre_index)
    if (is.matrix(post_index) || is.array(post_index)) post_index <- as.vector(post_index)
    
    pre_index <- as.numeric(pre_index[!is.na(pre_index)])
    post_index <- as.numeric(post_index[!is.na(post_index)])
    
    if (length(pre_index) == 0 || length(post_index) == 0) return(plotly_empty())
    
    # Create data frame for plotly
    plot_df <- data.frame(
      value = c(pre_index, post_index),
      group = c(rep("Pre Wellness Index", length(pre_index)), 
                rep("Post Impact Index", length(post_index)))
    )
    
    p <- plot_ly(data = plot_df, x = ~value, type = "histogram", 
                 color = ~group, colors = c("#82c341", "#5c2f92"),
                 xbins = list(start = 1, end = 5, size = 0.2),
                 opacity = 0.7) %>%
      layout(title = "Wellness Index: Pre vs Post",
             xaxis = list(title = "Index Score (−3 to +3)", range = c(-3, 3)),
             yaxis = list(title = "Count"),
             barmode = "overlay")
    
    # Add bell curves if checkbox is checked
    if (input$pairs_show_bell_curves) {
      if (length(pre_index) > 0 && length(post_index) > 0) {
        # Create density curves
        pre_density <- density(pre_index, from = -3, to = 3, n = 200, adjust = 1.5)
        post_density <- density(post_index, from = -3, to = 3, n = 200, adjust = 1.5)
        
        # Scale to match histogram heights
        max_count <- max(c(length(pre_index), length(post_index)), na.rm = TRUE)
        pre_scale <- max_count / max(pre_density$y, na.rm = TRUE) * 0.8
        post_scale <- max_count / max(post_density$y, na.rm = TRUE) * 0.8
        
        p <- p %>%
          add_trace(x = pre_density$x, y = pre_density$y * pre_scale,
                    type = "scatter", mode = "lines",
                    name = "Pre Curve", line = list(color = "#82c341", width = 3),
                    opacity = 0.6, showlegend = TRUE) %>%
          add_trace(x = post_density$x, y = post_density$y * post_scale,
                    type = "scatter", mode = "lines",
                    name = "Post Curve", line = list(color = "#5c2f92", width = 3),
                    opacity = 0.6, showlegend = TRUE)
      }
    }
    
    return(p)
  })
  
  output$pairs_wellness_radar_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Calculate Pre averages
    pre_opt_col <- grep("optimistic.*financial future", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_rel_col <- grep("healthy relationship.*money", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_stress_col <- grep("stressed.*financ|able to manage stress", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_conf_col <- grep("confident.*plan", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    
    if (length(pre_opt_col) == 0 || length(pre_rel_col) == 0 || length(pre_stress_col) == 0 ||
        length(pre_conf_col) == 0 || length(pre_comfort_col) == 0) return(plotly_empty())
    
    pre_opt <- mean(likert_to_numeric(big_pre_data[[pre_opt_col[1]]]), na.rm = TRUE)
    pre_rel <- mean(likert_to_numeric(big_pre_data[[pre_rel_col[1]]]), na.rm = TRUE)
    pre_stress <- mean(likert_to_numeric(big_pre_data[[pre_stress_col[1]]]), na.rm = TRUE)
    pre_conf <- mean(likert_to_numeric(big_pre_data[[pre_conf_col[1]]]), na.rm = TRUE)
    pre_comfort <- mean(likert_to_numeric(big_pre_data[[pre_comfort_col[1]]]), na.rm = TRUE)
    
    # Reverse stress for Pre (high stress = low wellness); symmetric scale -> negate
    pre_stress_rev <- -pre_stress
    
    # Calculate Post averages
    post_opt_col <- grep("more optimistic.*financial future", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_rel_col <- grep("healthier relationship.*money", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_stress_col <- grep("feel less stress.*finances", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_conf_col <- grep("more confident.*plan", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_comfort_col <- grep("more comfortable.*speaking.*financial professionals", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    
    if (length(post_opt_col) == 0 || length(post_rel_col) == 0 || length(post_stress_col) == 0 ||
        length(post_conf_col) == 0 || length(post_comfort_col) == 0) return(plotly_empty())
    
    post_opt <- mean(likert_to_numeric(big_post_data[[post_opt_col[1]]]), na.rm = TRUE)
    post_rel <- mean(likert_to_numeric(big_post_data[[post_rel_col[1]]]), na.rm = TRUE)
    post_stress <- mean(likert_to_numeric(big_post_data[[post_stress_col[1]]]), na.rm = TRUE)
    post_conf <- mean(likert_to_numeric(big_post_data[[post_conf_col[1]]]), na.rm = TRUE)
    post_comfort <- mean(likert_to_numeric(big_post_data[[post_comfort_col[1]]]), na.rm = TRUE)
    
    # Post stress is already "less stress" so higher = better
    post_stress_rev <- post_stress
    
    dims <- c("Optimism", "Relationship", "Low Stress", "Confidence", "Comfort")
    pre_vals <- c(pre_opt, pre_rel, pre_stress_rev, pre_conf, pre_comfort)
    post_vals <- c(post_opt, post_rel, post_stress_rev, post_conf, post_comfort)
    
    p <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = c(pre_vals, pre_vals[1]),
        theta = c(dims, dims[1]),
        name = 'Pre (Baseline)',
        line = list(color = '#82c341')
      ) %>%
      add_trace(
        r = c(post_vals, post_vals[1]),
        theta = c(dims, dims[1]),
        name = 'Post (Change)',
        line = list(color = '#5c2f92')
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(-3, 3)
          )
        ),
        title = "Wellness Profile: Pre vs Post Comparison"
      )
    
    return(p)
  })
  
  # Behavioral Comparisons
  output$pairs_behaviors_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Get Pre behavior columns
    pre_behavior_cols <- grep("Before today's workshop, I have", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_behavior_cols) == 0) return(plotly_empty())
    
    # Get Post action columns
    post_action_cols <- grep("As a result of this workshop, I plan to take", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(post_action_cols) == 0) return(plotly_empty())
    
    # Extract behavior names (should match between pre and post)
    behavior_names <- gsub("Before today's workshop, I have\\.\\.\\. \\[(.*)\\]", "\\1", pre_behavior_cols)
    behavior_names <- gsub("^\\s+|\\s+$", "", behavior_names)
    
    # Calculate Pre percentages (Yes responses)
    pre_percentages <- sapply(pre_behavior_cols, function(col) {
      responses <- big_pre_data[[col]]
      yes_count <- sum(responses == "Yes", na.rm = TRUE)
      total <- sum(!is.na(responses))
      if (total == 0) return(0)
      return(100 * yes_count / total)
    })
    
    # Calculate Post percentages (Yes/Yes!! responses)
    post_percentages <- sapply(post_action_cols, function(col) {
      responses <- big_post_data[[col]]
      yes_count <- sum(responses %in% c("Yes", "Yes!!", "Yes!"), na.rm = TRUE)
      total <- sum(!is.na(responses))
      if (total == 0) return(0)
      return(100 * yes_count / total)
    })
    
    # Calculate change (Post - Pre)
    change <- post_percentages - pre_percentages
    
    # Create annotations for arrows - ONE arrow per behavior showing change
    annotations_list <- list()
    for (i in 1:length(behavior_names)) {
      if (abs(change[i]) > 0.1) {  # Only show arrows for meaningful changes
        # Arrow color: Blue (#5c2f92) for increases, Green (#82c341) for decreases
        arrow_color <- ifelse(change[i] > 0, "#5c2f92", "#82c341")
        arrow_symbol <- ifelse(change[i] > 0, "↑", "↓")
        
        # Position arrow above the higher bar
        y_pos <- max(pre_percentages[i], post_percentages[i]) + 8
        
        annotations_list[[length(annotations_list) + 1]] <- list(
          x = behavior_names[i],
          y = y_pos,
          text = paste0(arrow_symbol, " ", round(abs(change[i]), 1), "%"),
          showarrow = FALSE,  # Just text, no arrow line (cleaner)
          font = list(color = arrow_color, size = 14, bold = TRUE),
          xref = "x",
          yref = "y"
        )
      }
    }
    
    # Create bar chart with baseline and change indicators
    p <- plot_ly() %>%
      add_trace(x = behavior_names, y = pre_percentages, type = "bar",
                name = "Pre (Baseline %)", marker = list(color = "#82c341", opacity = 0.7)) %>%
      add_trace(x = behavior_names, y = post_percentages, type = "bar",
                name = "Post (% Planned)", marker = list(color = "#5c2f92", opacity = 0.7)) %>%
      layout(
        title = "Financial Behaviors: Pre Baseline vs Post Planned Actions",
        xaxis = list(title = "Behavior", tickangle = -45),
        yaxis = list(title = "Percentage (%)", range = c(0, max(c(pre_percentages, post_percentages), na.rm = TRUE) + 20)),
        barmode = "group",
        annotations = annotations_list
      )
    
    return(p)
  })
  
  output$pairs_behavioral_index_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_index <- calculate_behavioral_index(big_pre_data)
    post_index <- calculate_planned_actions_index(big_post_data)
    
    # Ensure vectors, not matrices
    if (is.matrix(pre_index) || is.array(pre_index)) pre_index <- as.vector(pre_index)
    if (is.matrix(post_index) || is.array(post_index)) post_index <- as.vector(post_index)
    
    pre_index <- pre_index[!is.na(pre_index)]
    post_index <- post_index[!is.na(post_index)]
    
    if (length(pre_index) == 0 || length(post_index) == 0) return(plotly_empty())
    
    # Ensure numeric vectors
    pre_index <- as.numeric(pre_index)
    post_index <- as.numeric(post_index)
    pre_index <- pre_index[!is.na(pre_index)]
    post_index <- post_index[!is.na(post_index)]
    
    if (length(pre_index) == 0 || length(post_index) == 0) return(plotly_empty())
    
    # Create data frame for plotly
    plot_df <- data.frame(
      value = c(pre_index, post_index),
      group = c(rep("Pre Behavioral Index", length(pre_index)), 
                rep("Post Planned Actions Index", length(post_index)))
    )
    
    # Create histograms
    p <- plot_ly(data = plot_df, x = ~value, type = "histogram", 
                 color = ~group, colors = c("#82c341", "#5c2f92"),
                 xbins = list(start = 0, end = 8, size = 1),
                 opacity = 0.7) %>%
      layout(title = "Behavioral Index: Pre (Past) vs Post (Planned)",
             xaxis = list(title = "Index Score (0-8)", range = c(-0.5, 8.5)),
             yaxis = list(title = "Count"),
             barmode = "overlay")
    return(p)
  })
  
  # Summary Indices Scatter Plots
  output$pairs_wellness_index_scatter <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post_only()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Group-level aggregation (since individual matching is limited)
    # Aggregate by organization and group
    pre_wellness_vals <- calculate_wellness_index(big_pre_data)
    pre_agg <- big_pre_data %>%
      mutate(wellness_index = pre_wellness_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        pre_wellness = mean(wellness_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(pre_wellness))
    
    post_impact_vals <- calculate_post_impact_index(big_post_data)
    post_agg <- big_post_data %>%
      mutate(impact_index = post_impact_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        post_impact = mean(impact_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(post_impact))
    
    # Join on org and group - use inner_join to ensure both exist
    combined <- pre_agg %>%
      inner_join(post_agg, by = c("org_name", "group")) %>%
      filter(!is.na(pre_wellness) & !is.na(post_impact))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    # Ensure numeric and create clean data frame
    combined <- combined %>%
      mutate(
        pre_wellness = as.numeric(pre_wellness),
        post_impact = as.numeric(post_impact)
      ) %>%
      filter(!is.na(pre_wellness) & !is.na(post_impact))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    p <- plot_ly(data = combined, x = ~pre_wellness, y = ~post_impact,
                 type = "scatter", mode = "markers",
                 marker = list(color = "#5c2f92", size = 10),
                 text = ~paste("Org:", org_name, "<br>Group:", ifelse(is.na(group), "None", group)),
                 hoverinfo = "text") %>%
      layout(title = "Wellness Index: Pre vs Post (Group Averages)",
             xaxis = list(title = "Pre Wellness Index (−3 to +3)", range = c(-3, 3)),
             yaxis = list(title = "Post Impact Index (−3 to +3)", range = c(-3, 3)))
    
    return(p)
  })
  
  output$pairs_wellness_index_change <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post_only()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Group-level change scores
    pre_wellness_vals <- calculate_wellness_index(big_pre_data)
    pre_agg <- big_pre_data %>%
      mutate(wellness_index = pre_wellness_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        pre_wellness = mean(wellness_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(pre_wellness))
    
    post_impact_vals <- calculate_post_impact_index(big_post_data)
    post_agg <- big_post_data %>%
      mutate(impact_index = post_impact_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        post_impact = mean(impact_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(post_impact))
    
    combined <- pre_agg %>%
      full_join(post_agg, by = c("org_name", "group")) %>%
      filter(!is.na(pre_wellness) & !is.na(post_impact)) %>%
      mutate(change = post_impact - pre_wellness)
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    change_vals <- as.numeric(combined$change)
    change_vals <- change_vals[!is.na(change_vals)]
    if (length(change_vals) == 0) return(plotly_empty())
    
    plot_df <- data.frame(change = change_vals)
    p <- plot_ly(data = plot_df, x = ~change, type = "histogram",
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Wellness Index Change Distribution (Post - Pre)",
             xaxis = list(title = "Change Score"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  output$pairs_behavioral_index_scatter <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_behavioral_vals <- calculate_behavioral_index(big_pre_data)
    pre_agg <- big_pre_data %>%
      mutate(behavioral_index = pre_behavioral_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        pre_behavioral = mean(behavioral_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(pre_behavioral))
    
    post_planned_vals <- calculate_planned_actions_index(big_post_data)
    post_agg <- big_post_data %>%
      mutate(planned_index = post_planned_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        post_planned = mean(planned_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(post_planned))
    
    combined <- pre_agg %>%
      inner_join(post_agg, by = c("org_name", "group")) %>%
      filter(!is.na(pre_behavioral) & !is.na(post_planned))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    # Ensure numeric and create clean data frame
    combined <- combined %>%
      mutate(
        pre_behavioral = as.numeric(pre_behavioral),
        post_planned = as.numeric(post_planned)
      ) %>%
      filter(!is.na(pre_behavioral) & !is.na(post_planned))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    p <- plot_ly(data = combined, x = ~pre_behavioral, y = ~post_planned,
                 type = "scatter", mode = "markers",
                 marker = list(color = "#5c2f92", size = 10),
                 text = ~paste("Org:", org_name, "<br>Group:", ifelse(is.na(group), "None", group)),
                 hoverinfo = "text") %>%
      layout(title = "Behavioral Index: Pre (Past) vs Post (Planned) (Group Averages)",
             xaxis = list(title = "Pre Behavioral Index (0-8)", range = c(0, 8)),
             yaxis = list(title = "Post Planned Actions Index (0-8)", range = c(0, 8)))
    
    return(p)
  })
  .get_behavioral_paired <- function() {
    big_pre <- filtered_big_pre()
    big_post <- filtered_big_post_only()
    if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(NULL)
    if (!"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(NULL)
    pre_idx <- calculate_behavioral_index(big_pre)
    post_idx <- calculate_planned_actions_index(big_post)
    if (is.matrix(post_idx) || is.array(post_idx)) post_idx <- as.vector(post_idx)
    pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = pre_idx, stringsAsFactors = FALSE)
    post_df <- data.frame(respondent_id = big_post$respondent_id, post = post_idx, stringsAsFactors = FALSE)
    valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON", ids, ignore.case = TRUE)
    pre_df <- pre_df[valid_id(pre_df$respondent_id) & !is.na(pre_df$pre), , drop = FALSE]
    post_df <- post_df[valid_id(post_df$respondent_id) & !is.na(post_df$post), , drop = FALSE]
    paired <- merge(pre_df, post_df, by = "respondent_id")
    paired <- paired[!is.na(paired$pre) & !is.na(paired$post), , drop = FALSE]
    if (nrow(paired) == 0) return(NULL)
    paired
  }
  output$impact_behavioral_between_summary <- renderUI({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      pre_idx <- calculate_behavioral_index(big_pre)
      post_idx <- as.vector(calculate_planned_actions_index(big_post))
      pre_idx <- pre_idx[!is.na(pre_idx)]
      post_idx <- post_idx[!is.na(post_idx)]
      n_pre <- length(pre_idx)
      n_post <- length(post_idx)
      avg_pre <- if (n_pre > 0) round(mean(pre_idx), 3) else NA
      avg_post <- if (n_post > 0) round(mean(post_idx), 3) else NA
      sd_pre <- if (n_pre > 1) round(sd(pre_idx), 3) else NA
      sd_post <- if (n_post > 1) round(sd(post_idx), 3) else NA
      tt <- if (n_pre >= 2 && n_post >= 2) tryCatch(t.test(pre_idx, post_idx, alternative = "two.sided", var.equal = FALSE), error = function(e) NULL) else NULL
      ci_lo <- if (!is.null(tt)) round(tt$conf.int[1], 3) else NA
      ci_hi <- if (!is.null(tt)) round(tt$conf.int[2], 3) else NA
      ttest_str <- if (!is.null(tt)) paste0("<p class='summary-box-detail'><strong>Independent-samples t-test:</strong> t = ", round(tt$statistic, 3), ", p = ", format_p_value(tt$p.value), ", 95% CI for difference [", ci_lo, ", ", ci_hi, "]</p>") else ""
      verdict <- if (!is.null(tt)) .signif_verdict(tt$p.value, (avg_post - avg_pre), "change") else list(text = "Not enough data for a significance test.", color = "#5f6369")
      verdict_str <- paste0("<p class='summary-box-detail' style='color:", verdict$color, "; font-weight:600;'>", verdict$text, "</p>")
      html <- paste0(
        "<div class='summary-box' style='border-left-color: ", PRE_INDEX_COLOR, ";'>",
        "<p class='summary-box-detail' style='margin-bottom:10px;'>Index = within-respondent sum of endorsed behaviors (0–8), treated as a count for averaging and tests.</p>",
        "<p class='summary-box-headline'><strong>Pre (Behavioral Index):</strong> M = ", if (is.na(avg_pre)) "—" else avg_pre, ", SD = ", if (is.na(sd_pre)) "—" else sd_pre, ", n = ", n_pre,
        " &nbsp;|&nbsp; <strong>Post (Planned Actions Index):</strong> M = ", if (is.na(avg_post)) "—" else avg_post, ", SD = ", if (is.na(sd_post)) "—" else sd_post, ", n = ", n_post, "</p>",
        ttest_str, verdict_str,
        "<p class='summary-box-scope'>", scope_sentence(input), "</p>",
        "</div>"
      )
      HTML(html)
    }, error = function(e) HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
  })
  output$impact_behavioral_between_dist <- renderPlotly({
    tryCatch({
      show_bars <- tryCatch(isTRUE(input$behavioral_show_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$behavioral_show_curves), error = function(e) TRUE)
      bf <- .behavioral_bar_label_flags()
      want_lab <- bf$n || bf$pct
      br <- seq(0, 9, 0.5)
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      pre_idx <- calculate_behavioral_index(big_pre)
      post_idx <- as.vector(calculate_planned_actions_index(big_post))
      pre_idx <- pre_idx[!is.na(pre_idx)]
      post_idx <- post_idx[!is.na(post_idx)]
      if (length(pre_idx) == 0 && length(post_idx) == 0) return(plotly_empty() %>% layout(title = "No data"))
      p <- plot_ly()
      ymax <- 1
      if (show_bars) {
        # Always overlay Pre/Post as translucent bars with a solid outline (no side-by-side grouping).
        if (length(pre_idx) > 0) {
          hpre <- hist(pre_idx, breaks = br, plot = FALSE)
          txt_pre <- if (want_lab) .hist_bar_text(hpre$counts, length(pre_idx), bf$n, bf$pct) else NULL
          has_t <- !is.null(txt_pre) && any(nzchar(txt_pre))
          p <- p %>% add_trace(x = hpre$mids, y = hpre$counts, type = "bar", name = "Pre",
            marker = list(color = PRE_INDEX_COLOR, opacity = 0.45, line = list(color = PRE_INDEX_COLOR, width = 1.3)),
            text = if (has_t) txt_pre else NULL,
            textposition = if (has_t) "outside" else NULL,
            cliponaxis = FALSE)
        }
        if (length(post_idx) > 0) {
          hpost <- hist(post_idx, breaks = br, plot = FALSE)
          txt_post <- if (want_lab) .hist_bar_text(hpost$counts, length(post_idx), bf$n, bf$pct) else NULL
          has_t <- !is.null(txt_post) && any(nzchar(txt_post))
          p <- p %>% add_trace(x = hpost$mids, y = hpost$counts, type = "bar", name = "Post",
            marker = list(color = POST_INDEX_COLOR, opacity = 0.45, line = list(color = POST_INDEX_COLOR, width = 1.3)),
            text = if (has_t) txt_post else NULL,
            textposition = if (has_t) "outside" else NULL,
            cliponaxis = FALSE)
        }
      }
      if (show_curves) {
        if (length(pre_idx) >= 2) {
          dpre <- density(pre_idx, from = 0, to = max(8, max(pre_idx, na.rm = TRUE) + 1), n = 100)
          scale_pre <- length(pre_idx) * 0.3
          p <- p %>% add_trace(x = dpre$x, y = dpre$y * scale_pre, type = "scatter", mode = "lines", name = "Pre (curve)", fill = "tozeroy",
            line = list(color = PRE_INDEX_COLOR, width = 2), fillcolor = PRE_INDEX_FILL_RGBA)
          ymax <- max(ymax, max(dpre$y * scale_pre, na.rm = TRUE))
        }
        if (length(post_idx) >= 2) {
          dpost <- density(post_idx, from = 0, to = max(8, max(post_idx, na.rm = TRUE) + 1), n = 100)
          scale_post <- length(post_idx) * 0.3
          p <- p %>% add_trace(x = dpost$x, y = dpost$y * scale_post, type = "scatter", mode = "lines", name = "Post (curve)", fill = "tozeroy",
            line = list(color = POST_INDEX_COLOR, width = 2), fillcolor = POST_INDEX_FILL_RGBA)
          ymax <- max(ymax, max(dpost$y * scale_post, na.rm = TRUE))
        }
      }
      if (show_bars) {
        hpre <- if (length(pre_idx) > 0) max(hist(pre_idx, breaks = seq(0, 9, 0.5), plot = FALSE)$counts, 0) else 0
        hpost <- if (length(post_idx) > 0) max(hist(post_idx, breaks = seq(0, 9, 0.5), plot = FALSE)$counts, 0) else 0
        ymax <- max(ymax, hpre, hpost)
      }
      if (ymax <= 0) ymax <- 1
      mean_shapes_b <- list()
      if (length(pre_idx) >= 1 && is.finite(mean(pre_idx)) && (show_curves || show_bars)) {
        mean_shapes_b <- c(mean_shapes_b, .mean_vline_outline_shapes(mean(pre_idx), ymax, PRE_INDEX_COLOR))
      }
      if (length(post_idx) >= 1 && is.finite(mean(post_idx)) && (show_curves || show_bars)) {
        mean_shapes_b <- c(mean_shapes_b, .mean_vline_outline_shapes(mean(post_idx), ymax, POST_INDEX_COLOR))
      }
      p %>% layout(barmode = "overlay", bargap = 0, font = PLOT_FONT,
        title = list(text = "Between-subjects: Pre vs Post Index", x = 0.5, xref = "paper"),
        margin = list(t = 72, b = 72, l = 60, r = 24),
        xaxis = list(title = "Index (count)", range = c(0, 9)), yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22, yanchor = "top"),
        shapes = mean_shapes_b)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e))))
  })
  output$impact_behavioral_within_summary <- renderUI({
    tryCatch({
      paired <- .get_behavioral_paired()
      if (is.null(paired) || nrow(paired) < 2) {
        return(deterministic_summary_box(pre = numeric(0), post = numeric(0), mode = "change",
                                         headline_label = "Mean behavioral change (Post \u2212 Pre)",
                                         accent = POST_INDEX_COLOR, scope_text = scope_sentence(input)))
      }
      deterministic_summary_box(
        pre = paired$pre, post = paired$post, mode = "change",
        scale_label = "(0\u20138 count)",
        headline_label = "Mean behavioral change (Post \u2212 Pre)",
        accent = POST_INDEX_COLOR,
        scope_text = scope_sentence(input)
      )
    }, error = function(e) HTML(paste0("<p style='color: red;'>Error: ", as.character(e$message), "</p>")))
  })
  output$impact_behavioral_within_slopes <- renderPlotly({
    tryCatch({
      paired <- .get_behavioral_paired()
      if (is.null(paired) || nrow(paired) == 0) {
        return(plotly_empty() %>% layout(title = "No paired respondents.", font = PLOT_FONT))
      }
      show_ind <- tryCatch(isTRUE(input$behavioral_within_show_individual), error = function(e) TRUE)
      show_avg <- tryCatch(isTRUE(input$behavioral_within_show_avg), error = function(e) TRUE)
      if (!show_ind && !show_avg) {
        return(plotly_empty() %>% layout(title = "Turn on Individual slopes and/or Average slope", font = PLOT_FONT))
      }
      n <- nrow(paired)
      p <- plot_ly()
      if (show_ind) {
        for (i in seq_len(n)) {
          p <- p %>% add_trace(
            x = c(0, 1), y = c(paired$pre[i], paired$post[i]),
            type = "scatter", mode = "lines+markers",
            line = list(color = "rgba(90, 90, 90, 0.4)", width = 2),
            marker = list(size = 8, color = c(PRE_INDEX_COLOR, POST_INDEX_COLOR), line = list(color = "white", width = 0.5)),
            showlegend = FALSE,
            hoverinfo = "text",
            hovertext = sprintf("Pre %.2f → Post %.2f", paired$pre[i], paired$post[i])
          )
        }
      }
      if (show_avg) {
        mpre <- mean(paired$pre)
        mpost <- mean(paired$post)
        p <- p %>% add_trace(
          x = c(0, 1), y = c(mpre, mpost),
          type = "scatter", mode = "lines+markers",
          line = list(color = MEAN_SLOPE_COLOR, width = 4),
          marker = list(size = 12, color = c(PRE_INDEX_COLOR, POST_INDEX_COLOR), line = list(color = "white", width = 0.5)),
          name = "Mean (Pre → Post)",
          showlegend = TRUE,
          hoverinfo = "text",
          hovertext = sprintf("Mean Pre %.3f → Post %.3f", mpre, mpost)
        )
      }
      p %>% layout(
        font = PLOT_FONT,
        title = paste0("Within-subject change: Pre → Post (n = ", n, ")"),
        margin = list(t = 70, b = 55, l = 60, r = 60),
        xaxis = list(title = "", tickvals = c(0, 1), ticktext = c("Pre", "Post"), range = c(-0.05, 1.05)),
        yaxis = list(title = "Index (0–8)", range = c(0, 8), dtick = 1),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")
      )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_behavioral_paired_change <- renderPlotly({
    tryCatch({
      paired <- .get_behavioral_paired()
      if (is.null(paired)) return(plotly_empty() %>% layout(title = "respondent_id not available", font = PLOT_FONT))
      change_vals <- paired$post - paired$pre
      change_vals <- change_vals[is.finite(change_vals)]
      if (length(change_vals) == 0) return(plotly_empty() %>% layout(title = "No paired respondents.", font = PLOT_FONT))
      show_bars <- tryCatch(isTRUE(input$behavioral_change_show_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$behavioral_change_show_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly_empty() %>% layout(title = "Enable Bars and/or Curves", font = PLOT_FONT))
      }
      n_breaks <- max(6L, min(24L, ceiling(2 * sqrt(length(change_vals)))))
      h <- hist(change_vals, breaks = n_breaks, plot = FALSE)
      bw <- if (length(h$breaks) >= 2) diff(h$breaks[1:2]) else 0.1
      p <- plot_ly()
      ymax <- 1
      bf <- .behavioral_bar_label_flags()
      total_n <- length(change_vals)
      txt <- if (show_bars) .hist_bar_text(h$counts, total_n, bf$n, bf$pct) else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      div_cols <- .change_diverging_colors(h$mids)
      if (show_bars) {
        p <- p %>% add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count", showlegend = FALSE,
          width = bw,
          marker = list(color = div_cols$fill,
                        line = list(color = div_cols$line_color, width = div_cols$line_width)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && length(change_vals) >= 2) {
        rng <- range(change_vals, na.rm = TRUE)
        pad <- if (diff(rng) > 1e-6) 0.04 * diff(rng) else 0.25
        d <- density(change_vals, n = 256, from = rng[1] - pad, to = rng[2] + pad)
        ycurve <- d$y * length(change_vals) * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% add_trace(
          x = d$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = "#5b7c99", width = 2.5)
        )
      }
      ci_vis <- .paired_change_ci_shapes(change_vals, ymax)
      # Force 0 to the center: the larger absolute change sets both ends symmetrically.
      rx <- range(change_vals, na.rm = TRUE)
      max_abs <- max(abs(rx), na.rm = TRUE)
      padx <- if (max_abs > 1e-6) 0.05 * max_abs else 0.35
      lim <- max_abs + padx
      xrng <- c(-lim, lim)
      p %>% layout(
        title = list(
          text = paste0(
            "Change score distribution (n = ", length(change_vals), ")<br>",
            "<sup style='font-size:11px;color:#5f6369'>Dashed vertical = no change (0); shaded band = 95% CI of mean; double line = mean</sup>"
          ),
          font = PLOT_FONT
        ),
        barmode = "overlay",
        bargap = 0,
        xaxis = list(title = list(text = "Post index − Pre index", standoff = 12), range = xrng, zeroline = FALSE),
        yaxis = list(title = "Count", range = c(0, ymax * .label_headroom_factor(bf$n, bf$pct)), rangemode = "nonnegative"),
        font = PLOT_FONT,
        margin = list(t = 88, b = 70, l = 55, r = 25),
        showlegend = isTRUE(show_curves),
        legend = list(x = 0.99, xanchor = "right", y = 0.99, yanchor = "top",
                      bgcolor = "rgba(255,255,255,0.72)", bordercolor = "rgba(0,0,0,0.12)", borderwidth = 1),
        shapes = ci_vis$shapes
      )
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  .behavior_has_label <- function(vec, label) {
    vapply(vec, function(x) {
      if (is.na(x) || as.character(x) == "") return(FALSE)
      parts <- trimws(strsplit(as.character(x), ",\\s*")[[1]])
      parts <- parts[nzchar(parts)]
      if (!length(parts)) return(FALSE)
      canon <- canonical_behavior_label(parts)
      any(canon == label, na.rm = TRUE)
    }, logical(1))
  }
  output$impact_behavioral_item_between_plot <- renderPlotly({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
      pre_col <- grep("Before today's workshop, I have", colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
      post_col <- grep("As a result of.*5 Buckets", colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(pre_col) || is.null(post_col)) return(plotly_empty() %>% layout(title = "Behavior columns not found", font = PLOT_FONT))
      pre_counts <- get_past_behaviors_counts(big_pre)
      post_counts <- get_planned_actions_counts(big_post)
      labels_pre <- if (nrow(pre_counts) > 0) pre_counts$behavior else character(0)
      labels_post <- if (nrow(post_counts) > 0) post_counts$action else character(0)
      all_labels <- unique(c(labels_pre, labels_post))
      if (length(all_labels) == 0) return(plotly_empty() %>% layout(title = "No behaviors in data", font = PLOT_FONT))
      results <- list()
      pre_vec <- big_pre[[pre_col]]
      post_vec <- big_post[[post_col]]
      n_pre <- sum(!is.na(pre_vec) & as.character(pre_vec) != "")
      n_post <- sum(!is.na(post_vec) & as.character(post_vec) != "")
      if (n_pre < 2 || n_post < 2) return(plotly_empty() %>% layout(title = "Insufficient data", font = PLOT_FONT))
      for (lab in all_labels) {
        pre_checked <- .behavior_has_label(pre_vec, lab)
        post_checked <- .behavior_has_label(post_vec, lab)
        p_pre <- mean(pre_checked, na.rm = TRUE)
        p_post <- mean(post_checked, na.rm = TRUE)
        diff <- p_post - p_pre
        tt <- tryCatch(prop.test(c(sum(post_checked), sum(pre_checked)), c(n_post, n_pre), correct = FALSE), error = function(e) NULL)
        if (is.null(tt)) next
        ci <- tt$conf.int
        results[[length(results) + 1L]] <- data.frame(
          item = lab, mean_diff = diff, ci_lo = ci[1], ci_hi = ci[2],
          pre_yes = sum(pre_checked), post_yes = sum(post_checked),
          stringsAsFactors = FALSE
        )
      }
      if (length(results) == 0) return(plotly_empty() %>% layout(title = "No item results", font = PLOT_FONT))
      df <- do.call(rbind, results)
      df <- df[order(-df$mean_diff), ]
      df$item <- factor(df$item, levels = rev(df$item))
      colors <- REACH_PALETTE[(seq_len(nrow(df)) - 1L) %% length(REACH_PALETTE) + 1L]
      bf <- .behavioral_bar_label_flags()
      bar_txt <- rep("", nrow(df))
      for (i in seq_len(nrow(df))) {
        parts <- character(0)
        if (bf$n) parts <- c(parts, paste0("n ", df$pre_yes[i], "/", df$post_yes[i]))
        if (bf$pct) parts <- c(parts, sprintf("%+.1f pp", df$mean_diff[i] * 100))
        bar_txt[i] <- paste(parts, collapse = "\n")
      }
      has_bar_txt <- (bf$n || bf$pct) && any(nzchar(bar_txt))
      plot_ly(df, x = ~mean_diff, y = ~item, type = "bar", orientation = "h", marker = list(color = colors),
        error_x = list(type = "data", array = df$ci_hi - df$mean_diff, arrayminus = df$mean_diff - df$ci_lo, thickness = 1),
        text = if (has_bar_txt) bar_txt else NULL,
        textposition = if (has_bar_txt) "outside" else NULL,
        cliponaxis = FALSE) %>%
        layout(font = PLOT_FONT, title = "Between-subjects: Proportion difference (Post − Pre) per behavior",
          xaxis = list(title = "Proportion difference", zeroline = TRUE), yaxis = list(title = ""),
          margin = list(l = 200), showlegend = FALSE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_behavioral_item_between_table <- DT::renderDataTable({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE))
      pre_col <- grep("Before today's workshop, I have", colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
      post_col <- grep("As a result of.*5 Buckets", colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(pre_col) || is.null(post_col)) return(DT::datatable(data.frame(Message = "Columns not found"), rownames = FALSE))
      pre_counts <- get_past_behaviors_counts(big_pre)
      post_counts <- get_planned_actions_counts(big_post)
      all_labels <- unique(c(if (nrow(pre_counts) > 0) pre_counts$behavior else character(0), if (nrow(post_counts) > 0) post_counts$action else character(0)))
      if (length(all_labels) == 0) return(DT::datatable(data.frame(Message = "No behaviors"), rownames = FALSE))
      pre_vec <- big_pre[[pre_col]]
      post_vec <- big_post[[post_col]]
      n_pre <- length(pre_vec[!is.na(pre_vec) & as.character(pre_vec) != ""])
      n_post <- length(post_vec[!is.na(post_vec) & as.character(post_vec) != ""])
      results <- list()
      for (lab in all_labels) {
        pre_checked <- .behavior_has_label(pre_vec, lab)
        post_checked <- .behavior_has_label(post_vec, lab)
        p_pre <- mean(pre_checked, na.rm = TRUE)
        p_post <- mean(post_checked, na.rm = TRUE)
        diff <- p_post - p_pre
        tt <- tryCatch(prop.test(c(sum(post_checked), sum(pre_checked)), c(n_post, n_pre), correct = FALSE), error = function(e) NULL)
        if (is.null(tt)) next
        results[[length(results) + 1L]] <- data.frame(
          Behavior = lab, `Pre %` = round(p_pre * 100, 1), `Pre n` = sum(pre_checked),
          `Post %` = round(p_post * 100, 1), `Post n` = sum(post_checked),
          Difference = round(diff, 3), p = format_p_value(tt$p.value),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
      if (length(results) == 0) return(DT::datatable(data.frame(Message = "No results"), rownames = FALSE))
      df <- do.call(rbind, results)
      df <- df[order(-df$Difference), ]
      DT::datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$impact_behavioral_item_within_plot <- renderPlotly({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0 || !"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(plotly_empty() %>% layout(title = "No data", font = PLOT_FONT))
      pre_col <- grep("Before today's workshop, I have", colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
      post_col <- grep("As a result of.*5 Buckets", colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(pre_col) || is.null(post_col)) return(plotly_empty() %>% layout(title = "Columns not found", font = PLOT_FONT))
      pre_counts <- get_past_behaviors_counts(big_pre)
      post_counts <- get_planned_actions_counts(big_post)
      all_labels <- unique(c(if (nrow(pre_counts) > 0) pre_counts$behavior else character(0), if (nrow(post_counts) > 0) post_counts$action else character(0)))
      if (length(all_labels) == 0) return(plotly_empty() %>% layout(title = "No behaviors", font = PLOT_FONT))
      valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON#", ids)
      results <- list()
      for (lab in all_labels) {
        pre_checked <- .behavior_has_label(big_pre[[pre_col]], lab)
        post_checked <- .behavior_has_label(big_post[[post_col]], lab)
        pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = as.integer(pre_checked), stringsAsFactors = FALSE)
        post_df <- data.frame(respondent_id = big_post$respondent_id, post = as.integer(post_checked), stringsAsFactors = FALSE)
        pre_df <- pre_df[valid_id(pre_df$respondent_id), , drop = FALSE]
        post_df <- post_df[valid_id(post_df$respondent_id), , drop = FALSE]
        merged <- merge(pre_df, post_df, by = "respondent_id")
        if (nrow(merged) < 2) next
        diff <- merged$post - merged$pre
        tt <- tryCatch(t.test(diff, mu = 0, alternative = "two.sided"), error = function(e) NULL)
        if (is.null(tt)) next
        results[[length(results) + 1L]] <- data.frame(
          item = lab, mean_change = mean(diff), ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
          n = nrow(merged),
          stringsAsFactors = FALSE
        )
      }
      if (length(results) == 0) return(plotly_empty() %>% layout(title = "No paired item results", font = PLOT_FONT))
      df <- do.call(rbind, results)
      df <- df[order(-df$mean_change), ]
      df$item <- factor(df$item, levels = rev(df$item))
      colors <- REACH_PALETTE[(seq_len(nrow(df)) - 1L) %% length(REACH_PALETTE) + 1L]
      bf <- .behavioral_bar_label_flags()
      bar_txt <- rep("", nrow(df))
      for (i in seq_len(nrow(df))) {
        parts <- character(0)
        if (bf$n) parts <- c(parts, paste0("n=", df$n[i]))
        if (bf$pct) parts <- c(parts, sprintf("%+.0f%%", df$mean_change[i] * 100))
        bar_txt[i] <- paste(parts, collapse = "\n")
      }
      has_bar_txt <- (bf$n || bf$pct) && any(nzchar(bar_txt))
      plot_ly(df, x = ~mean_change, y = ~item, type = "bar", orientation = "h", marker = list(color = colors),
        error_x = list(type = "data", array = df$ci_hi - df$mean_change, arrayminus = df$mean_change - df$ci_lo, thickness = 1),
        text = if (has_bar_txt) bar_txt else NULL,
        textposition = if (has_bar_txt) "outside" else NULL,
        cliponaxis = FALSE) %>%
        layout(font = PLOT_FONT, title = "Within-subjects: Mean change (added − removed) per behavior",
          xaxis = list(title = "Mean change", zeroline = TRUE), yaxis = list(title = ""),
          margin = list(l = 200), showlegend = FALSE)
    }, error = function(e) plotly_empty() %>% layout(title = paste("Error:", conditionMessage(e)), font = PLOT_FONT))
  })
  output$impact_behavioral_item_within_table <- DT::renderDataTable({
    tryCatch({
      big_pre <- filtered_big_pre()
      big_post <- filtered_big_post()
      if (nrow(big_pre) == 0 || nrow(big_post) == 0 || !"respondent_id" %in% colnames(big_pre) || !"respondent_id" %in% colnames(big_post)) return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE))
      pre_col <- grep("Before today's workshop, I have", colnames(big_pre), ignore.case = TRUE, value = TRUE)[1]
      post_col <- grep("As a result of.*5 Buckets", colnames(big_post), ignore.case = TRUE, value = TRUE)[1]
      if (is.null(pre_col) || is.null(post_col)) return(DT::datatable(data.frame(Message = "Columns not found"), rownames = FALSE))
      pre_counts <- get_past_behaviors_counts(big_pre)
      post_counts <- get_planned_actions_counts(big_post)
      all_labels <- unique(c(if (nrow(pre_counts) > 0) pre_counts$behavior else character(0), if (nrow(post_counts) > 0) post_counts$action else character(0)))
      if (length(all_labels) == 0) return(DT::datatable(data.frame(Message = "No behaviors"), rownames = FALSE))
      valid_id <- function(ids) !is.na(ids) & nzchar(trimws(ids)) & !grepl("^ANON#", ids)
      results <- list()
      for (lab in all_labels) {
        pre_checked <- .behavior_has_label(big_pre[[pre_col]], lab)
        post_checked <- .behavior_has_label(big_post[[post_col]], lab)
        pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = as.integer(pre_checked), stringsAsFactors = FALSE)
        post_df <- data.frame(respondent_id = big_post$respondent_id, post = as.integer(post_checked), stringsAsFactors = FALSE)
        pre_df <- pre_df[valid_id(pre_df$respondent_id), , drop = FALSE]
        post_df <- post_df[valid_id(post_df$respondent_id), , drop = FALSE]
        merged <- merge(pre_df, post_df, by = "respondent_id")
        if (nrow(merged) < 2) next
        diff <- merged$post - merged$pre
        tt <- tryCatch(t.test(diff, mu = 0, alternative = "two.sided"), error = function(e) NULL)
        if (is.null(tt)) next
        results[[length(results) + 1L]] <- data.frame(
          Behavior = lab, n = nrow(merged), `Mean change` = round(mean(diff), 3), SD = round(sd(diff), 3),
          t = round(tt$statistic, 2), p = format_p_value(tt$p.value),
          `95% CI` = paste0("[", round(tt$conf.int[1], 2), ", ", round(tt$conf.int[2], 2), "]"),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
      if (length(results) == 0) return(DT::datatable(data.frame(Message = "No results"), rownames = FALSE))
      df <- do.call(rbind, results)
      df <- df[order(-df$`Mean change`), ]
      DT::datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    }, error = function(e) DT::datatable(data.frame(Error = conditionMessage(e)), rownames = FALSE))
  })
  output$pairs_behavioral_index_change <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    pre_behavioral_vals <- calculate_behavioral_index(big_pre_data)
    pre_agg <- big_pre_data %>%
      mutate(behavioral_index = pre_behavioral_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        pre_behavioral = mean(behavioral_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(pre_behavioral))
    
    post_planned_vals <- calculate_planned_actions_index(big_post_data)
    post_agg <- big_post_data %>%
      mutate(planned_index = post_planned_vals) %>%
      group_by(org_name, group) %>%
      summarise(
        post_planned = mean(planned_index, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(post_planned))
    
    combined <- pre_agg %>%
      inner_join(post_agg, by = c("org_name", "group")) %>%
      filter(!is.na(pre_behavioral) & !is.na(post_planned)) %>%
      mutate(change = as.numeric(post_planned) - as.numeric(pre_behavioral))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    # Ensure change is a numeric vector
    change_vals <- as.numeric(combined$change)
    change_vals <- change_vals[!is.na(change_vals)]
    if (length(change_vals) == 0) return(plotly_empty())
    
    plot_df <- data.frame(change = change_vals)
    p <- plot_ly(data = plot_df, x = ~change, type = "histogram",
                 xbins = list(start = min(change_vals, na.rm = TRUE) - 0.5, 
                             end = max(change_vals, na.rm = TRUE) + 0.5, size = 0.5),
                 marker = list(color = "#5c2f92")) %>%
      layout(title = "Behavioral Index Change Distribution (Post - Pre)",
             xaxis = list(title = "Change Score"),
             yaxis = list(title = "Count"))
    
    return(p)
  })
  
  # Sentiment vs Satisfaction
  output$pairs_sentiment_satisfaction_scatter <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Get Pre intentions sentiment (group-level)
    pre_intentions_col <- grep("one intention.*workshop", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    if (length(pre_intentions_col) == 0) return(plotly_empty())
    
    # Calculate sentiment for each row, then aggregate
    pre_sentiment_scores <- sapply(1:nrow(big_pre_data), function(i) {
      resp <- big_pre_data[[pre_intentions_col[1]]][i]
      if (is.na(resp) || trimws(resp) == "") return(NA)
      sentiment_result <- calculate_sentiment(resp)
      return(sentiment_result$score)
    })
    
    pre_sentiment_agg <- big_pre_data %>%
      mutate(sentiment_score = pre_sentiment_scores) %>%
      group_by(org_name, group) %>%
      summarise(
        avg_sentiment = mean(sentiment_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_sentiment))
    
    # Get Post satisfaction (group-level)
    post_satisfaction_col <- grep("How satisfied are you with your.*5 Buckets experience", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    if (length(post_satisfaction_col) == 0) return(plotly_empty())
    
    post_satisfaction_agg <- big_post_data %>%
      mutate(satisfaction = as.numeric(.data[[post_satisfaction_col[1]]])) %>%
      group_by(org_name, group) %>%
      summarise(
        avg_satisfaction = mean(satisfaction, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_satisfaction))
    
    # Join - use full_join to see all data, then filter
    combined <- pre_sentiment_agg %>%
      full_join(post_satisfaction_agg, by = c("org_name", "group")) %>%
      filter(!is.na(avg_sentiment) & !is.na(avg_satisfaction))
    
    if (nrow(combined) == 0) {
      # Return a message plot instead of empty
      p <- plot_ly() %>%
        add_annotations(
          text = "No matching groups found between Pre (sentiment) and Post (satisfaction) data.<br>This comparison requires groups with both pre-workshop intentions and post-workshop satisfaction responses.",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(
          title = "Pre-Workshop Sentiment vs Post-Workshop Satisfaction",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      return(p)
    }
    
    # Ensure numeric and clean
    combined <- combined %>%
      mutate(
        avg_sentiment = as.numeric(avg_sentiment),
        avg_satisfaction = as.numeric(avg_satisfaction)
      ) %>%
      filter(!is.na(avg_sentiment) & !is.na(avg_satisfaction))
    
    if (nrow(combined) == 0) return(plotly_empty())
    
    p <- plot_ly(data = combined, x = ~avg_sentiment, y = ~avg_satisfaction,
                 type = "scatter", mode = "markers",
                 marker = list(color = "#5c2f92", size = 10),
                 text = ~paste("Org:", org_name, "<br>Group:", ifelse(is.na(group), "None", group)),
                 hoverinfo = "text") %>%
      layout(title = "Pre-Workshop Sentiment vs Post-Workshop Satisfaction (Group Averages)",
             xaxis = list(title = "Pre-Workshop Sentiment Score"),
             yaxis = list(title = "Post-Workshop Satisfaction (1-5)"))
    
    return(p)
  })
  
  # Wellness Summary Comparison (Financial Behaviors style)
  output$pairs_wellness_summary_comparison <- renderPlotly({
    big_pre_data <- filtered_big_pre()
    big_post_data <- filtered_big_post()
    
    if (nrow(big_pre_data) == 0 || nrow(big_post_data) == 0) return(plotly_empty())
    
    # Get all 5 wellness dimensions
    pre_opt_col <- grep("optimistic.*financial future", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_rel_col <- grep("healthy relationship.*money", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_stress_col <- grep("stressed.*financ|able to manage stress", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_conf_col <- grep("confident.*plan", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    pre_comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(big_pre_data), ignore.case = TRUE, value = TRUE)
    
    post_opt_col <- grep("more optimistic.*financial future", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_rel_col <- grep("healthier relationship.*money", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_stress_col <- grep("feel less stress.*finances", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_conf_col <- grep("more confident.*plan", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    post_comfort_col <- grep("more comfortable.*speaking.*financial professionals", colnames(big_post_data), ignore.case = TRUE, value = TRUE)
    
    # Convert to numeric and calculate means
    dimensions <- c("Optimism", "Relationship", "Stress*", "Confidence", "Comfort")
    pre_means <- numeric(5)
    post_means <- numeric(5)
    changes <- numeric(5)
    change_pct <- numeric(5)
    
    pre_cols <- list(pre_opt_col[1], pre_rel_col[1], pre_stress_col[1], pre_conf_col[1], pre_comfort_col[1])
    post_cols <- list(post_opt_col[1], post_rel_col[1], post_stress_col[1], post_conf_col[1], post_comfort_col[1])
    
    for (i in 1:5) {
      if (length(pre_cols[[i]]) > 0 && length(post_cols[[i]]) > 0) {
        pre_vals <- likert_to_numeric(big_pre_data[[pre_cols[[i]]]])
        post_vals <- likert_to_numeric(big_post_data[[post_cols[[i]]]])
        pre_vals <- pre_vals[!is.na(pre_vals)]
        post_vals <- post_vals[!is.na(post_vals)]
        
        # For stress (i == 3), reverse-score pre so higher = less stress (like post)
        # Symmetric scale (−3..3): reversing is just negation (−3 <-> 3, −1 <-> 1).
        # Post: "feel less stress" (Strongly Agree = +3, good)
        if (i == 3) {  # Stress dimension
          pre_vals_reversed <- -pre_vals  # Reverse so higher = less stress
          if (length(pre_vals_reversed) > 0) pre_means[i] <- mean(pre_vals_reversed)
          if (length(post_vals) > 0) post_means[i] <- mean(post_vals)
          # Change: increase in "less stress" = positive change
          changes[i] <- post_means[i] - pre_means[i]
        } else {
          if (length(pre_vals) > 0) pre_means[i] <- mean(pre_vals)
          if (length(post_vals) > 0) post_means[i] <- mean(post_vals)
          changes[i] <- post_means[i] - pre_means[i]
        }
      }
    }
    
    # Create annotations for arrows (change in score units, not percentage)
    annotations_list <- list()
    for (i in 1:5) {
      if (abs(changes[i]) > 0.01) {
        arrow_color <- ifelse(changes[i] > 0, "#5c2f92", "#82c341")
        arrow_symbol <- ifelse(changes[i] > 0, "↑", "↓")
        
        annotations_list[[length(annotations_list) + 1]] <- list(
          x = dimensions[i],
          y = max(pre_means[i], post_means[i]) + 0.15,
          text = paste0(arrow_symbol, " ", round(abs(changes[i]), 2)),
          showarrow = FALSE,
          font = list(color = arrow_color, size = 14, bold = TRUE),
          xref = "x",
          yref = "y"
        )
      }
    }
    
    # Create bar chart matching Financial Behaviors style
    p <- plot_ly() %>%
      add_trace(x = dimensions, y = pre_means, type = "bar",
                name = "Pre (Baseline)", marker = list(color = "#82c341", opacity = 0.7)) %>%
      add_trace(x = dimensions, y = post_means, type = "bar",
                name = "Post (Change)", marker = list(color = "#5c2f92", opacity = 0.7)) %>%
      layout(
        title = "Financial Wellness: Pre Baseline vs Post Change",
        xaxis = list(title = "Dimension", tickangle = -45),
        yaxis = list(title = "Average Score (−3 to +3)", range = c(-3, 3)),
        barmode = "group",
        annotations = annotations_list
      )
    
    return(p)
  })
  
  # ========================================================================
  # Within Session Tab
  # ========================================================================
  
  # Update within-session organization dropdown (from Master)
  observe({
    pre_data <- master_pre()
    if (nrow(pre_data) > 0 && "org_name" %in% colnames(pre_data)) {
      orgs <- unique(pre_data$org_name)
      orgs <- orgs[!is.na(orgs) & orgs != ""]
      choices <- c("Select organization..." = "", setNames(orgs, orgs))
      updateSelectInput(session, "within_session_org", choices = choices)
    }
  })

  # Update within-session group dropdown based on selected org (from Master)
  observe({
    pre_data <- master_pre()
    selected_org <- tryCatch(input$within_session_org, error = function(e) NULL)
    if (nrow(pre_data) > 0 && "group" %in% colnames(pre_data) && .has_select_value(selected_org)) {
      org_sessions <- pre_data %>% filter(org_name == selected_org)
      groups <- unique(org_sessions$group)
      groups <- groups[!is.na(groups) & groups != ""]
      
      # Check if org has any sessions without groups
      has_no_group <- any(is.na(org_sessions$group) | org_sessions$group == "")
      
      # Build choices
      choices <- c("Select group..." = "")
      if (length(groups) > 0) {
        choices <- c(choices, setNames(groups, groups))
      }
      if (has_no_group) {
        choices <- c(choices, "(No Group)" = "__NO_GROUP__")
      }
      
      updateSelectInput(session, "within_session_group", choices = choices)
    } else {
      updateSelectInput(session, "within_session_group", 
                       choices = c("Select group..." = "", "(No Group)" = "__NO_GROUP__"))
    }
  })
  
  # Get series data for selected org/group (from Master Pre/Post)
  within_session_journey <- reactive({
    if (!.has_select_value(tryCatch(input$within_session_org, error = function(e) NULL))) return(NULL)
    pre_data <- master_pre()
    post_data <- master_post()
    pm_data <- tryCatch(program_manager(), error = function(e) data.frame())
    ss <- create_session_summary_from_master(pre_data, post_data, pm_data)
    if (nrow(ss) == 0) return(NULL)
    ws_org <- input$within_session_org
    ws_grp <- tryCatch(input$within_session_group, error = function(e) NULL)
    if (!.has_select_value(ws_grp) || identical(ws_grp, "__NO_GROUP__")) {
      journey_sessions <- ss %>%
        filter(org_name == ws_org, is.na(group) | group == "") %>%
        arrange(date, start_time)
    } else {
      journey_sessions <- ss %>%
        filter(org_name == ws_org, group == ws_grp) %>%
        arrange(date, start_time)
    }
    
    if (nrow(journey_sessions) == 0) {
      return(NULL)
    }
    
    # Get all responses for these sessions
    session_ids <- journey_sessions$session_id
    
    journey_pre <- pre_data %>% filter(session_id %in% session_ids)
    journey_post <- post_data %>% filter(session_id %in% session_ids)
    
    list(
      sessions = journey_sessions,
      pre = journey_pre,
      post = journey_post,
      sessions_in_series = unique(journey_sessions$sessions_in_series)[1]
    )
  })
  
  # Series info display (within-session tab)
  output$within_session_journey_info <- renderUI({
    journey <- within_session_journey()
    if (is.null(journey)) {
      if (!.has_select_value(tryCatch(input$within_session_org, error = function(e) NULL))) {
        return(HTML("<p style='color: #797d82;'>Select an organization to view series information. If the organization has groups, also select a group.</p>"))
      } else {
        return(HTML("<p style='color: #797d82;'>No series found for the selected organization and group combination.</p>"))
      }
    }
    
    html <- paste0(
      "<p style='margin-top: 10px;'><strong>Series Found:</strong></p>",
      "<ul>",
      "<li>Sessions in series: <strong>", journey$sessions_in_series, "</strong></li>",
      "<li>Total sessions: ", nrow(journey$sessions), "</li>",
      "<li>Pre responses: ", nrow(journey$pre), "</li>",
      "<li>Post responses: ", nrow(journey$post), "</li>",
      "</ul>"
    )
    
    return(HTML(html))
  })
  
  # Series overview (placeholder)
  output$within_session_overview <- renderUI({
    journey <- within_session_journey()
    if (is.null(journey)) {
      return(HTML(""))
    }
    
    # Create summary table
    html <- "<p>Series overview details will appear here.</p>"
    return(HTML(html))
  })
  
  # Response flow plot
  output$within_session_flow_plot <- renderPlotly({
    journey <- within_session_journey()
    if (is.null(journey)) {
      return(plotly_empty())
    }
    
    # Aggregate responses by session
    session_responses <- journey$sessions %>%
      left_join(
        journey$pre %>% group_by(session_id) %>% summarise(pre_count = n(), .groups = 'drop'),
        by = "session_id"
      ) %>%
      left_join(
        journey$post %>% group_by(session_id) %>% summarise(post_count = n(), .groups = 'drop'),
        by = "session_id"
      ) %>%
      mutate(
        pre_count = ifelse(is.na(pre_count), 0, pre_count),
        post_count = ifelse(is.na(post_count), 0, post_count),
        session_label = paste0("Session ", session_number, " of ", sessions_in_series)
      )
    
    p <- plot_ly(session_responses, x = ~session_label) %>%
      add_trace(y = ~pre_count, name = "Pre Responses", type = 'scatter', mode = 'lines+markers', 
                line = list(color = '#5c2f92'), marker = list(color = '#5c2f92')) %>%
      add_trace(y = ~post_count, name = "Post Responses", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#82c341'), marker = list(color = '#82c341')) %>%
      layout(
        title = "Response Flow Over Time",
        xaxis = list(title = "Session"),
        yaxis = list(title = "Number of Responses"),
        hovermode = 'x unified'
      )
    
    return(p)
  })
  
  # Session details table
  output$within_session_details_table <- DT::renderDataTable({
    journey <- within_session_journey()
    if (is.null(journey)) {
      return(data.frame(Message = "Select an organization and group to view session details."))
    }
    
    display_data <- journey$sessions %>%
      select(date, session_number, sessions_in_series, facilitators, modules_taught) %>%
      mutate(date = format(as.Date(date), "%B %d, %Y"))
    
    DT::datatable(
      display_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ========================================================================
  # Outputs - Data Previews (Temporary for Testing - Other Tabs)
  # ========================================================================
  
  # Pre-Survey Tab
  output$pre_data_preview <- renderText({
    pre_data <- filtered_pre()
    paste0("Pre-survey rows: ", nrow(pre_data), "\n",
           "Columns: ", paste(colnames(pre_data), collapse = ", "))
  })
  
  # Post-Survey Tab
  output$post_data_preview <- renderText({
    post_data <- filtered_post()
    paste0("Post-survey rows: ", nrow(post_data), "\n",
           "Columns: ", paste(colnames(post_data), collapse = ", "))
  })
  
  # ========================================================================
  # Series Tab
  # ========================================================================
  
    # Series Summary Table (collapsed across all orgs, rows by series length)
  output$journeys_summary_table <- DT::renderDataTable({
    session_summary <- session_summary_data()
    if (nrow(session_summary) == 0) return(data.frame())
    
    # Calculate series statistics by length
    journey_stats <- calculate_organization_journeys(session_summary)
    if (nrow(journey_stats) == 0) return(data.frame())
    
    # Get post data for metrics
    post_data <- filtered_big_post()
    
    # Create summary by series length (rows)
    summary_rows <- list()
    
    for (len in 1:6) {
      len_col <- paste0("Len", len)
      journey_count <- if (len_col %in% colnames(journey_stats)) {
        sum(journey_stats[[len_col]], na.rm = TRUE)
      } else {
        0
      }
      
      avg_recommend <- NA
      avg_satisfaction <- NA
      min_responses <- NA
      max_responses <- NA
      
      if (journey_count > 0 && nrow(post_data) > 0) {
        # Filter post data for series of this planned length
        post_with_session <- post_data %>%
          left_join(session_summary %>% select(session_id, sessions_in_series),
                    by = "session_id") %>%
          filter(!is.na(sessions_in_series) & pmin(sessions_in_series, 6) == len)
        
        if (nrow(post_with_session) > 0) {
          # Get recommendation
          recommend_col <- grep("likely.*recommend", colnames(post_with_session), ignore.case = TRUE, value = TRUE)
          if (length(recommend_col) > 0) {
            recommend_vals <- .coerce_numeric_vec(post_with_session[[recommend_col[1]]])
            recommend_vals <- recommend_vals[!is.na(recommend_vals)]
            avg_recommend <- if (length(recommend_vals) > 0) round(mean(recommend_vals), 2) else NA
          }
          
          # Get satisfaction
          satisfaction_col <- grep("satisfied.*5 Buckets experience", colnames(post_with_session), ignore.case = TRUE, value = TRUE)
          if (length(satisfaction_col) > 0) {
            satisfaction_vals <- .coerce_numeric_vec(post_with_session[[satisfaction_col[1]]])
            satisfaction_vals <- satisfaction_vals[!is.na(satisfaction_vals)]
            avg_satisfaction <- if (length(satisfaction_vals) > 0) round(mean(satisfaction_vals), 2) else NA
          }
          
          # Count responses per session
          session_responses <- post_with_session %>%
            group_by(session_id) %>%
            summarise(response_count = n(), .groups = "drop")
          
          if (nrow(session_responses) > 0) {
            min_responses <- min(session_responses$response_count, na.rm = TRUE)
            max_responses <- max(session_responses$response_count, na.rm = TRUE)
          }
        }
      }
      
      summary_rows[[len]] <- data.frame(
        `Series Length` = len,
        `Count` = journey_count,
        `Min Responses` = min_responses,
        `Max Responses` = max_responses,
        `Avg Recommend (1-10)` = avg_recommend,
        `Avg Satisfaction (1-5)` = avg_satisfaction,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    # Combine all rows
    if (length(summary_rows) > 0) {
      tryCatch({
        summary_df <- do.call(rbind, summary_rows)
        if (nrow(summary_df) > 0) {
          return(DT::datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
        } else {
          return(DT::datatable(data.frame(Message = "No series data available"), rownames = FALSE))
        }
      }, error = function(e) {
        return(DT::datatable(data.frame(Error = paste("Error creating table:", e$message)), rownames = FALSE))
      })
    } else {
      return(DT::datatable(data.frame(Message = "No series data available"), rownames = FALSE))
    }
  })
  
  # Satisfaction over time by series length
  output$journeys_satisfaction_by_length <- renderPlotly({
    session_summary <- session_summary_data()
    post_data <- filtered_big_post()
    
    if (nrow(session_summary) == 0 || nrow(post_data) == 0) return(plotly_empty())
    
    # Join post data with session summary for planned series length
    post_with_session <- post_data %>%
      left_join(session_summary %>% select(session_id, org_name, group, sessions_in_series, session_number),
                by = "session_id") %>%
      filter(!is.na(sessions_in_series) & !is.na(session_number))
    
    if (nrow(post_with_session) == 0) return(plotly_empty())
    
    # Get satisfaction column
    satisfaction_col <- grep("satisfied.*5 Buckets experience", colnames(post_with_session), ignore.case = TRUE, value = TRUE)
    if (length(satisfaction_col) == 0) return(plotly_empty())
    
    # Average satisfaction by planned series length and session number
    satisfaction_by_length <- post_with_session %>%
      mutate(
        satisfaction = as.numeric(.data[[satisfaction_col[1]]]),
        journey_length = pmin(sessions_in_series, 6)
      ) %>%
      filter(!is.na(satisfaction)) %>%
      group_by(journey_length, session_number) %>%
      summarise(
        avg_satisfaction = mean(satisfaction, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(journey_length <= 6)
    
    if (nrow(satisfaction_by_length) == 0) return(plotly_empty())
    
    # One trace per series length bucket
    p <- plot_ly()
    
    for (len in unique(satisfaction_by_length$journey_length)) {
      len_data <- satisfaction_by_length %>% filter(journey_length == len)
      if (nrow(len_data) > 0) {
        # Ensure numeric and create clean vectors
        session_nums <- as.numeric(as.character(len_data$session_number))
        satisfaction_vals <- as.numeric(as.character(len_data$avg_satisfaction))
        
        # Remove any NAs
        valid_idx <- !is.na(session_nums) & !is.na(satisfaction_vals)
        session_nums <- session_nums[valid_idx]
        satisfaction_vals <- satisfaction_vals[valid_idx]
        
        if (length(session_nums) > 0 && length(satisfaction_vals) > 0) {
          p <- p %>%
            add_trace(
              x = session_nums,
              y = satisfaction_vals,
              type = "scatter",
              mode = "lines+markers",
              name = paste0("Series length ", len),
              line = list(width = 2),
              marker = list(size = 8)
            )
        }
      }
    }
    
    p <- p %>%
      layout(
        title = "Average Satisfaction Over Time by Series Length",
        xaxis = list(title = "Session Number"),
        yaxis = list(title = "Average Satisfaction (1-5)", range = c(1, 5)),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # Series detail view
  output$journey_detail_info <- renderUI({
    org <- input$journey_detail_org
    group <- input$journey_detail_group
    
    if (!.has_select_value(org)) {
      return(HTML("<p>Please select an organization.</p>"))
    }
    
    session_summary <- session_summary_data()
    journey_sessions <- session_summary %>%
      filter(org_name == org)
    
    if (.has_select_value(group) && !identical(group, "__NO_GROUP__")) {
      journey_sessions <- journey_sessions %>% filter(group == group)
    } else if (identical(group, "__NO_GROUP__")) {
      journey_sessions <- journey_sessions %>% filter(is.na(group) | group == "")
    }
    
    if (nrow(journey_sessions) == 0) {
      return(HTML("<p>No sessions found for this organization/group combination.</p>"))
    }
    
    num_sessions <- nrow(journey_sessions)
    sessions_in_series <- unique(journey_sessions$sessions_in_series)
    sessions_in_series <- sessions_in_series[!is.na(sessions_in_series)]
    
    HTML(paste0(
      "<p><strong>Organization:</strong> ", org, "<br>",
      ifelse(.has_select_value(group) && !identical(group, "__NO_GROUP__"), 
             paste0("<strong>Group:</strong> ", group, "<br>"), ""),
      "<strong>Number of Sessions:</strong> ", num_sessions, "<br>",
      ifelse(length(sessions_in_series) > 0,
             paste0("<strong>Sessions in Series:</strong> ", paste(sessions_in_series, collapse = ", "), "<br>"),
             ""),
      "</p>"
    ))
  })
  
  output$journey_detail_table <- DT::renderDataTable({
    org <- input$journey_detail_org
    group <- input$journey_detail_group
    
    if (!.has_select_value(org)) return(data.frame())
    
    session_summary <- session_summary_data()
    post_data <- filtered_big_post()
    
    journey_sessions <- session_summary %>%
      filter(org_name == org) %>%
      arrange(session_number)
    
    if (.has_select_value(group) && !identical(group, "__NO_GROUP__")) {
      journey_sessions <- journey_sessions %>% filter(group == group)
    } else if (identical(group, "__NO_GROUP__")) {
      journey_sessions <- journey_sessions %>% filter(is.na(group) | group == "")
    }
    
    if (nrow(journey_sessions) == 0) return(data.frame(Message = "No sessions found."))
    
    # Get post data for these sessions
    post_for_journey <- post_data %>%
      filter(session_id %in% journey_sessions$session_id) %>%
      group_by(session_id) %>%
      summarise(
        post_responses = n(),
        .groups = "drop"
      )
    
    # Create detail table - ensure all columns exist and are properly formatted
    detail_table <- journey_sessions %>%
      left_join(post_for_journey, by = "session_id") %>%
      mutate(
        post_responses = ifelse(is.na(post_responses), 0, post_responses),
        session_number = ifelse(is.na(session_number), "", as.character(session_number)),
        date = ifelse(is.na(date), "", as.character(date)),
        facilitators = ifelse(is.na(facilitators), "", as.character(facilitators)),
        modules_taught = ifelse(is.na(modules_taught), "", as.character(modules_taught))
      ) %>%
      select(
        Session = session_number,
        Date = date,
        Facilitators = facilitators,
        Modules = modules_taught,
        `Post Responses` = post_responses
      )
    
    if (nrow(detail_table) == 0) {
      return(DT::datatable(data.frame(Message = "No session details available for this series."), rownames = FALSE))
    }
    
    tryCatch({
      DT::datatable(detail_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error creating table:", e$message)), rownames = FALSE)
    })
  })
  
  # Update journey detail selectors
  observe({
    session_summary <- session_summary_data()
    if (nrow(session_summary) == 0) return()
    
    orgs <- unique(session_summary$org_name)
    orgs <- orgs[!is.na(orgs)]
    updateSelectInput(session, "journey_detail_org", choices = c("Select organization..." = "", orgs))
  })
  
  observe({
    org <- tryCatch(input$journey_detail_org, error = function(e) NULL)
    if (!.has_select_value(org)) {
      updateSelectInput(session, "journey_detail_group", choices = c("Select group..." = ""))
      return()
    }
    
    session_summary <- session_summary_data()
    groups <- session_summary %>%
      filter(org_name == org) %>%
      pull(group) %>%
      unique()
    groups <- groups[!is.na(groups) & groups != ""]
    
    choices <- c("Select group..." = "", "(No Group)" = "__NO_GROUP__")
    if (length(groups) > 0) {
      choices <- c(choices, setNames(groups, groups))
    }
    updateSelectInput(session, "journey_detail_group", choices = choices)
  })
}

# ============================================================================
# Run App
# ============================================================================

# Wrap the UI in a branded login screen only when auth is enabled (deployed).
app_ui <- if (.auth_enabled()) {
  shinymanager::secure_app(
    ui,
    enable_admin = FALSE,
    fab_position = "bottom-right",
    tags_top = tags$div(
      style = "text-align:center; margin-bottom:10px;",
      tags$img(src = "5buckets_logo.png", height = "56px"),
      tags$h4("Impact Dashboard", style = "color:#5c2f92; margin:8px 0 0;"),
      tags$p("Please sign in to continue.", style = "color:#797d82; font-size:13px;")
    ),
    background = "linear-gradient(135deg, #f7f4fb 0%, #ffffff 100%)"
  )
} else {
  ui
}

shinyApp(ui = app_ui, server = server)

