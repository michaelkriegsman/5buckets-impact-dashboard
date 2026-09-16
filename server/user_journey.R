# user_journey.R - Impact tab: one respondent over time + Reach drill helpers

.uj_copy_btn <- function(text, label = "Copy") {
  raw <- as.character(if (is.null(text) || (length(text) == 1 && is.na(text))) "" else text)
  esc_attr <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x
  }
  htmltools::HTML(sprintf(
    paste0(
      '<button type="button" class="btn btn-xs btn-default uj-copy-btn" ',
      'data-copy="%s" title="Copy full value" ',
      'onclick="(function(b){var t=b.getAttribute(\'data-copy\');',
      'if(navigator.clipboard&&navigator.clipboard.writeText){navigator.clipboard.writeText(t);}',
      'else{var i=document.createElement(\'textarea\');i.value=t;document.body.appendChild(i);',
      'i.select();document.execCommand(\'copy\');document.body.removeChild(i);}',
      'var o=b.textContent;b.textContent=\'Copied\';setTimeout(function(){b.textContent=o;},900);})(this);">%s</button>'
    ),
    esc_attr(raw),
    htmltools::htmlEscape(label)
  ))
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

.uj_short_id <- function(id, n = 10L) {
  id <- as.character(id %||% "")
  if (!nzchar(id)) return("-")
  if (nchar(id) <= n + 3L) return(id)
  paste0(substr(id, 1L, n), "...")
}

.uj_pick_col <- function(df, patterns) {
  if (is.null(df) || !ncol(df)) return(NULL)
  nms <- names(df)
  for (p in patterns) {
    exact <- which(tolower(trimws(nms)) == tolower(trimws(p)))
    if (length(exact)) return(nms[[exact[[1]]]])
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[[1]])
  }
  NULL
}

.uj_chr <- function(df, col) {
  if (is.null(col) || !col %in% names(df)) return(NA_character_)
  raw <- df[[col]]
  if (is.null(raw) || length(raw) < 1) return(NA_character_)
  v <- raw[[1]]
  if (is.null(v) || length(v) < 1) return(NA_character_)
  v <- as.character(v)[[1]]
  if (is.null(v) || length(v) < 1 || is.na(v)) return(NA_character_)
  v <- trimws(v)
  if (!nzchar(v)) return(NA_character_)
  v
}

.uj_demo_specs <- function() {
  list(
    list(label = "Zip", patterns = c("^Zip Code$", "Zip")),
    list(label = "Age", patterns = c("^Age Group$")),
    list(label = "Gender", patterns = c("^Gender Identity$")),
    list(label = "Race/Ethnicity", patterns = c("Race/Ethnicity")),
    list(label = "Income", patterns = c("^Household Income$")),
    list(label = "Education", patterns = c("highest level of education")),
    list(label = "First-gen college", patterns = c("First-Generation Status \\(College\\)", "FirstGeneration Status \\(College\\)")),
    list(label = "First-gen U.S.", patterns = c("First-Generation Status \\(U\\.S\\.\\)", "FirstGeneration Status \\(U\\.S\\.\\)")),
    list(label = "Veteran", patterns = c("^Veteran Status$")),
    list(label = "Disability", patterns = c("^Disability Status$")),
    list(label = "Neurodivergent", patterns = c("neurodivergent"))
  )
}

.uj_text_specs_pre <- function() {
  list(
    list(label = "Intention", patterns = c("one intention")),
    list(label = "Curiosities", patterns = c("questions or curiosities")),
    list(label = "Hope to feel", patterns = c("hope to feel")),
    list(label = "Support needed", patterns = c("help support your learning")),
    list(label = "Additional comments", patterns = c("^additional_comments$", "additional comments"))
  )
}

.uj_text_specs_post <- function() {
  list(
    list(label = "Stood out", patterns = c("stood out to you")),
    list(label = "Apply learning", patterns = c("apply something you learned")),
    list(label = "What made it helpful", patterns = c("made today's session helpful", "made todays session helpful")),
    list(label = "Impact story", patterns = c("helped or impacted you")),
    list(label = "Additional comments", patterns = c("^additional_comments$", "additional comments"))
  )
}

.uj_collect_named <- function(df, specs, keep_empty = FALSE) {
  out <- list()
  for (sp in specs) {
    col <- .uj_pick_col(df, sp$patterns)
    val <- .uj_chr(df, col)
    if (!is.na(val) && nzchar(val)) {
      out[[sp$label]] <- val
    } else if (isTRUE(keep_empty)) {
      out[[sp$label]] <- "-"
    }
  }
  out
}

.uj_fmt_num <- function(x, digits = 2L) {
  if (length(x) != 1L || is.na(x) || !is.finite(as.numeric(x))) return("-")
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

user_journey_tab_ui <- function() {
  shiny::tagList(
    shiny::tags$style(htmltools::HTML("
      .uj-status { background:#f8f9fa; border:1px solid #dee2e6; padding:10px 14px; border-radius:4px; margin-bottom:14px; }
      .uj-card { border:1px solid #e3e6ea; border-radius:6px; margin-bottom:10px; background:#fff; }
      .uj-card > summary { cursor:pointer; padding:10px 14px; color:#5c2f92; font-weight:600; list-style:none; }
      .uj-card > summary::-webkit-details-marker { display:none; }
      .uj-card > summary:before { content:'> '; color:#797d82; }
      .uj-card[open] > summary:before { content:'v '; }
      .uj-card-body { padding:0 14px 14px 14px; font-size:13px; }
      .uj-kv { display:grid; grid-template-columns: 160px 1fr; gap:4px 12px; margin:8px 0; }
      .uj-k { color:#5f6369; }
      .uj-section-title { margin:12px 0 4px; color:#5c2f92; font-size:13px; font-weight:600; }
      .uj-muted { color:#5f6369; font-size:12px; }
      .uj-mono { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size:12px; }
      .uj-copy-btn { margin-left:6px; vertical-align:middle; }
      .reach-drill-modal .dataTables_wrapper { font-size:12px; }
      .reach-drill-modal table.dataTable td { vertical-align: middle !important; white-space: nowrap; }
      .reach-drill-modal table.dataTable td.uj-wrap { white-space: normal !important; max-width: 220px; }
    ")),
    shiny::h3("User Journey"),
    shiny::p(
      class = "uj-muted", style = "max-width: 760px;",
      "Look up a ", shiny::tags$code("respondent_id"),
      " for that person's full Pre / Post / Annual history. Sidebar filters do not apply here. ",
      "From Reach, click a bar -> select a row -> Open in User Journey."
    ),
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::textInput(
          "user_journey_id",
          label = "Respondent ID",
          value = "",
          placeholder = "Paste full ID or a unique prefix"
        )
      ),
      shiny::column(
        4,
        shiny::tags$label("\u00a0", style = "display:block;"),
        shiny::actionButton(
          "user_journey_load",
          "Load journey",
          class = "btn-primary",
          style = "margin-top: 5px;"
        )
      )
    ),
    shiny::uiOutput("user_journey_status"),
    shiny::uiOutput("user_journey_anomaly"),
    shiny::h4("Scores over time", style = "color: #5c2f92;"),
    shiny::p(
      class = "uj-muted",
      "One row per submission. Wellness / impact indices apply to Big Pre / Big Post when those items exist. ",
      "Session satisfaction and recommend usually appear on Post. Em dash means not on that form or unanswered."
    ),
    DT::dataTableOutput("user_journey_score_timeline"),
    shiny::br(),
    shiny::h4("Submissions (chronological)", style = "color: #5c2f92;"),
    shiny::p(class = "uj-muted", "Expand each submission for demographics, scores, and open text."),
    shiny::uiOutput("user_journey_cards")
  )
}

#' Resolve a typed query to a single respondent_id (exact, else unique prefix).
user_journey_resolve_id <- function(query, candidate_ids) {
  q <- trimws(as.character(if (is.null(query)) "" else query))
  if (!nzchar(q)) {
    return(list(ok = FALSE, id = NA_character_, message = "Enter a respondent_id."))
  }
  ids <- unique(as.character(candidate_ids))
  ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
  if (!length(ids)) {
    return(list(ok = FALSE, id = NA_character_, message = "No respondent IDs available in loaded data."))
  }
  exact <- ids[ids == q]
  if (length(exact) >= 1L) {
    return(list(ok = TRUE, id = exact[[1]], message = NULL))
  }
  pref <- ids[startsWith(tolower(ids), tolower(q))]
  if (length(pref) == 1L) {
    return(list(ok = TRUE, id = pref[[1]], message = paste0("Matched prefix to ", pref[[1]], ".")))
  }
  if (length(pref) > 1L) {
    return(list(
      ok = FALSE,
      id = NA_character_,
      message = paste0(
        "Ambiguous prefix: ", length(pref), " IDs match. Paste a longer prefix or the full ID. ",
        "Examples: ", paste(substr(pref, 1, 12), collapse = ", "), "..."
      )
    ))
  }
  list(ok = FALSE, id = NA_character_, message = "No matching respondent_id found.")
}

.uj_one_row_card <- function(row_df, survey_label, sheet) {
  stopifnot(nrow(row_df) == 1L)
  ts_c <- if ("timestamp" %in% names(row_df)) "timestamp" else .uj_pick_col(row_df, c("^timestamp"))
  host_c <- if ("org_name" %in% names(row_df)) {
    "org_name"
  } else if (exists("annual_find_col", mode = "function") && exists("ANNUAL_COL_PATTERNS")) {
    annual_find_col(row_df, ANNUAL_COL_PATTERNS$host_org)
  } else {
    .uj_pick_col(row_df, c("organization hosted", "host"))
  }

  wellness <- NA_real_
  behavior <- NA_real_
  satisfaction <- NA_character_
  recommend <- NA_character_

  if (identical(sheet, "pre")) {
    wellness <- tryCatch(as.numeric(calculate_wellness_index(row_df)[[1]]), error = function(e) NA_real_)
    behavior <- tryCatch(as.numeric(calculate_behavioral_index(row_df)[[1]]), error = function(e) NA_real_)
    texts <- .uj_collect_named(row_df, .uj_text_specs_pre())
  } else if (identical(sheet, "post")) {
    wellness <- tryCatch(as.numeric(calculate_post_impact_index(row_df)[[1]]), error = function(e) NA_real_)
    behavior <- tryCatch(as.numeric(calculate_planned_actions_index(row_df)[[1]]), error = function(e) NA_real_)
    sat_c <- .uj_pick_col(row_df, c("How satisfied were you with today's 5 Buckets session"))
    satisfaction <- .uj_chr(row_df, sat_c)
    rec_c <- if (exists("POST_NPS_COL")) .uj_pick_col(row_df, c(POST_NPS_COL, "recommend 5 Buckets")) else .uj_pick_col(row_df, c("recommend 5 Buckets"))
    recommend <- .uj_chr(row_df, rec_c)
    texts <- .uj_collect_named(row_df, .uj_text_specs_post())
  } else {
    wellness <- tryCatch({
      if (exists("calculate_annual_compared_index", mode = "function")) {
        as.numeric(calculate_annual_compared_index(row_df)[[1]])
      } else NA_real_
    }, error = function(e) NA_real_)
    behavior <- tryCatch({
      if (exists("calculate_annual_behavior_index", mode = "function")) {
        as.numeric(calculate_annual_behavior_index(row_df)[[1]])
      } else NA_real_
    }, error = function(e) NA_real_)
    rec_c <- if (exists("annual_find_col", mode = "function")) {
      annual_find_col(row_df, ANNUAL_COL_PATTERNS$recommend)
    } else NULL
    recommend <- .uj_chr(row_df, rec_c)
    texts <- list()
  }

  demos <- .uj_collect_named(row_df, .uj_demo_specs(), keep_empty = TRUE)
  # Prefer short income for display when possible
  if (!is.null(demos[["Income"]]) && exists("INCOME_SHORT_LABELS")) {
    inc <- demos[["Income"]]
    if (inc %in% names(INCOME_SHORT_LABELS)) demos[["Income"]] <- unname(INCOME_SHORT_LABELS[[inc]])
  }

  list(
    survey = survey_label,
    sheet = sheet,
    timestamp = .uj_chr(row_df, ts_c),
    session_id = .uj_chr(row_df, if ("session_id" %in% names(row_df)) "session_id" else NULL),
    org = .uj_chr(row_df, host_c),
    group = .uj_chr(row_df, if ("group" %in% names(row_df)) "group" else NULL),
    demos = demos,
    wellness_or_impact = wellness,
    behavior_index = behavior,
    session_satisfaction = satisfaction,
    recommend = recommend,
    texts = texts
  )
}

#' Build chronological submission cards for one respondent_id (unfiltered masters).
build_user_journey_cards <- function(pre_df, post_df, annual_df, pm_df, respondent_id) {
  rid <- as.character(respondent_id)
  cards <- list()
  if (!nzchar(rid)) return(cards)

  add_typed <- function(df, sheet) {
    if (is.null(df) || !nrow(df) || !"respondent_id" %in% names(df)) return()
    keep <- !is.na(df$respondent_id) & as.character(df$respondent_id) == rid
    sub <- df[keep, , drop = FALSE]
    if (!nrow(sub)) return()
    typed <- tryCatch({
      if (exists("identify_survey_type", mode = "function") && !is.null(pm_df) && nrow(pm_df) > 0) {
        identify_survey_type(sub, pm_df)
      } else sub
    }, error = function(e) sub)
    for (i in seq_len(nrow(typed))) {
      row <- typed[i, , drop = FALSE]
      if (identical(sheet, "pre")) {
        lab <- if ("is_big_pre" %in% names(row) && !is.na(row$is_big_pre[[1]]) && isTRUE(as.logical(row$is_big_pre[[1]]))) {
          "Big Pre"
        } else {
          "Little Pre"
        }
      } else {
        lab <- if ("is_big_post" %in% names(row) && !is.na(row$is_big_post[[1]]) && isTRUE(as.logical(row$is_big_post[[1]]))) {
          "Big Post"
        } else {
          "Little Post"
        }
      }
      cards[[length(cards) + 1L]] <<- .uj_one_row_card(row, lab, sheet)
    }
  }

  add_typed(pre_df, "pre")
  add_typed(post_df, "post")

  if (!is.null(annual_df) && nrow(annual_df) && "respondent_id" %in% names(annual_df)) {
    keep <- !is.na(annual_df$respondent_id) & as.character(annual_df$respondent_id) == rid
    sub <- annual_df[keep, , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      cards[[length(cards) + 1L]] <- .uj_one_row_card(sub[i, , drop = FALSE], "Annual", "annual")
    }
  }

  if (!length(cards)) return(cards)
  ts <- vapply(cards, function(c) as.character(c$timestamp %||% ""), character(1))
  cards[order(ts, na.last = TRUE)]
}

#' Flag odd journeys (e.g. Post before Pre on same session_id).
user_journey_anomaly_notes <- function(cards) {
  notes <- character(0)
  if (length(cards) < 2) return(notes)
  by_sid <- split(cards, vapply(cards, function(c) as.character(c$session_id %||% ""), character(1)))
  for (sid in names(by_sid)) {
    if (!nzchar(sid) || identical(sid, "NA")) next
    bunch <- by_sid[[sid]]
    sheets <- vapply(bunch, function(c) c$sheet, character(1))
    surveys <- vapply(bunch, function(c) c$survey, character(1))
    if (any(sheets == "pre") && any(sheets == "post")) {
      pre_ts <- min(vapply(bunch[sheets == "pre"], function(c) c$timestamp %||% "", character(1)))
      post_ts <- min(vapply(bunch[sheets == "post"], function(c) c$timestamp %||% "", character(1)))
      if (nzchar(pre_ts) && nzchar(post_ts) && post_ts < pre_ts) {
        notes <- c(notes, paste0(
          "Same session_id has Post timestamp before Pre (", post_ts, " then ", pre_ts, "). ",
          "Survey typing follows Program Manager session number (e.g. session 1 of 3 => Big Pre + Little Post), ",
          "but the form fill order looks reversed - often a QR/form routing issue, not a dashboard bug."
        ))
      }
    }
    if (any(surveys == "Big Pre") && any(surveys == "Little Post") && length(unique(sheets)) > 1) {
      # already covered by above when timestamps inverted; add context when same calendar day
      days <- substr(vapply(bunch, function(c) c$timestamp %||% "", character(1)), 1, 10)
      if (length(unique(days[nzchar(days)])) == 1L) {
        notes <- c(notes, paste0(
          "Big Pre and Little Post share session_id on ", unique(days[nzchar(days)]),
          ". For a multi-session series, Little Post on session 1 is expected by typing rules; ",
          "both coming in on workshop day can be valid if Post and Pre were both administered that day - ",
          "but Post before Pre is unusual and worth checking operations."
        ))
      }
    }
  }
  unique(notes)
}

build_user_journey_score_df <- function(cards) {
  if (!length(cards)) {
    return(data.frame(
      survey = character(0), timestamp = character(0),
      wellness_or_impact = character(0), behavior = character(0),
      session_satisfaction = character(0), recommend = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    survey = vapply(cards, function(c) c$survey, character(1)),
    timestamp = vapply(cards, function(c) c$timestamp %||% "", character(1)),
    wellness_or_impact = vapply(cards, function(c) .uj_fmt_num(c$wellness_or_impact), character(1)),
    behavior = vapply(cards, function(c) {
      if (is.na(c$behavior_index) || !is.finite(c$behavior_index)) "-" else as.character(as.integer(c$behavior_index))
    }, character(1)),
    session_satisfaction = vapply(cards, function(c) c$session_satisfaction %||% "-", character(1)),
    recommend = vapply(cards, function(c) c$recommend %||% "-", character(1)),
    stringsAsFactors = FALSE
  )
}

.uj_render_kv <- function(named) {
  if (!length(named)) {
    return(shiny::p(class = "uj-muted", "None recorded on this submission."))
  }
  rows <- lapply(names(named), function(nm) {
    shiny::tagList(
      shiny::div(class = "uj-k", nm),
      shiny::div(named[[nm]])
    )
  })
  shiny::div(class = "uj-kv", rows)
}

.uj_render_card <- function(card, open = FALSE) {
  title <- paste0(
    card$survey, "  |  ", card$timestamp %||% "-",
    if (!is.na(card$org) && nzchar(card$org)) paste0("  |  ", card$org) else ""
  )
  sid_line <- if (!is.na(card$session_id) && nzchar(card$session_id)) {
    shiny::div(
      class = "uj-muted",
      "session_id: ",
      shiny::span(class = "uj-mono", .uj_short_id(card$session_id, 28L)),
      .uj_copy_btn(card$session_id, "Copy session")
    )
  } else NULL

  score_bits <- c(
    paste0("Wellness/impact: ", .uj_fmt_num(card$wellness_or_impact)),
    paste0(
      "Behavior: ",
      if (is.na(card$behavior_index) || !is.finite(card$behavior_index)) "-" else as.character(as.integer(card$behavior_index))
    ),
    paste0("Session satisfaction: ", card$session_satisfaction %||% "-"),
    paste0("Recommend: ", card$recommend %||% "-")
  )

  body <- shiny::div(
    class = "uj-card-body",
    sid_line,
    shiny::div(class = "uj-muted", paste(score_bits, collapse = "  |  ")),
    shiny::div(class = "uj-section-title", "Demographics"),
    .uj_render_kv(card$demos),
    shiny::div(class = "uj-section-title", "Open text"),
    .uj_render_kv(card$texts)
  )
  if (isTRUE(open)) {
    shiny::tags$details(class = "uj-card", open = "open", shiny::tags$summary(title), body)
  } else {
    shiny::tags$details(class = "uj-card", shiny::tags$summary(title), body)
  }
}

#' Compact respondent table for Reach bar drilldown modal (HTML-ready).
build_reach_drill_table <- function(df, wave, chart_value, raw_response = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(
      Message = "No respondents match this bar in the current filters.",
      stringsAsFactors = FALSE
    ))
  }
  n <- nrow(df)
  rid <- if ("respondent_id" %in% names(df)) as.character(df$respondent_id) else rep(NA_character_, n)
  org <- if ("org_name" %in% names(df)) {
    as.character(df$org_name)
  } else if (exists("annual_find_col", mode = "function") && exists("ANNUAL_COL_PATTERNS")) {
    hc <- annual_find_col(df, ANNUAL_COL_PATTERNS$host_org)
    if (!is.null(hc)) as.character(df[[hc]]) else rep(NA_character_, n)
  } else {
    rep(NA_character_, n)
  }
  grp <- if ("group" %in% names(df)) as.character(df$group) else rep(NA_character_, n)
  sid <- if ("session_id" %in% names(df)) as.character(df$session_id) else rep(NA_character_, n)
  ts <- if ("timestamp" %in% names(df)) as.character(df$timestamp) else rep(NA_character_, n)
  raw <- if (!is.null(raw_response) && length(raw_response) == n) {
    as.character(raw_response)
  } else {
    rep(as.character(chart_value), n)
  }

    id_html <- vapply(seq_len(n), function(i) {
      id <- rid[[i]]
      if (is.na(id) || !nzchar(id)) return("-")
      paste0(
        '<span class="uj-mono" title="', gsub('"', "&quot;", id, fixed = TRUE), '">',
        htmltools::htmlEscape(.uj_short_id(id, 12L)),
        "</span> ",
        as.character(.uj_copy_btn(id, "Copy"))
      )
    }, character(1))

  sid_short <- vapply(sid, function(s) {
    if (is.na(s) || !nzchar(s)) return("-")
    htmltools::htmlEscape(.uj_short_id(s, 22L))
  }, character(1))

  data.frame(
    respondent_id = rid,
    ID = id_html,
    Org = ifelse(is.na(org) | !nzchar(org), "-", org),
    Group = ifelse(is.na(grp) | !nzchar(grp), "-", grp),
    Time = ifelse(is.na(ts) | !nzchar(ts), "-", ts),
    Session = sid_short,
    Response = ifelse(is.na(raw) | !nzchar(raw), as.character(chart_value), raw),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

register_user_journey_outputs <- function(input, output, session,
                                          master_pre, master_post, master_annual,
                                          program_manager_for_typing,
                                          active_id) {
  journey_cards <- shiny::reactive({
    rid <- active_id()
    if (is.null(rid) || !nzchar(as.character(rid))) return(NULL)
    build_user_journey_cards(
      master_pre(),
      master_post(),
      master_annual(),
      program_manager_for_typing(),
      rid
    )
  })

  output$user_journey_status <- shiny::renderUI({
    rid <- active_id()
    if (is.null(rid) || !nzchar(as.character(rid))) {
      return(shiny::p(class = "uj-muted", "No user loaded yet."))
    }
    cards <- journey_cards()
    n <- if (is.null(cards)) 0L else length(cards)
    shiny::div(
      class = "uj-status",
      shiny::tags$strong("Active ID: "),
      shiny::span(class = "uj-mono", as.character(rid)),
      .uj_copy_btn(as.character(rid), "Copy ID"),
      shiny::tags$span(style = "margin-left:12px;color:#5f6369;", paste0(n, " submission(s)"))
    )
  })

  output$user_journey_anomaly <- shiny::renderUI({
    cards <- journey_cards()
    if (is.null(cards) || !length(cards)) return(NULL)
    notes <- user_journey_anomaly_notes(cards)
    if (!length(notes)) return(NULL)
    shiny::div(
      class = "alert alert-warning",
      style = "font-size: 12px;",
      shiny::tags$strong("Data check: "),
      shiny::tags$ul(lapply(notes, shiny::tags$li))
    )
  })

  output$user_journey_score_timeline <- DT::renderDataTable({
    cards <- journey_cards()
    if (is.null(cards)) {
      return(DT::datatable(
        data.frame(Message = "Enter a respondent_id and click Load journey."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    if (!length(cards)) {
      return(DT::datatable(
        data.frame(Message = "No submissions found for this ID."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    DT::datatable(
      build_user_journey_score_df(cards),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
    )
  })

  output$user_journey_cards <- shiny::renderUI({
    cards <- journey_cards()
    if (is.null(cards)) return(NULL)
    if (!length(cards)) return(shiny::p(class = "uj-muted", "No submissions."))
    shiny::tagList(lapply(seq_along(cards), function(i) {
      .uj_render_card(cards[[i]], open = identical(i, length(cards)))
    }))
  })

  shiny::observeEvent(input$user_journey_load, {
    pre <- master_pre()
    post <- master_post()
    ann <- master_annual()
    candidates <- c(
      if (!is.null(pre) && "respondent_id" %in% names(pre)) as.character(pre$respondent_id),
      if (!is.null(post) && "respondent_id" %in% names(post)) as.character(post$respondent_id),
      if (!is.null(ann) && "respondent_id" %in% names(ann)) as.character(ann$respondent_id)
    )
    res <- user_journey_resolve_id(input$user_journey_id, candidates)
    if (!isTRUE(res$ok)) {
      active_id(NULL)
      shiny::showNotification(res$message, type = "warning", duration = 8)
      return()
    }
    active_id(res$id)
    shiny::updateTextInput(session, "user_journey_id", value = res$id)
    if (!is.null(res$message)) {
      shiny::showNotification(res$message, type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)
}
