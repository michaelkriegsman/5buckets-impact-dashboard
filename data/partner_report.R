# partner_report.R — pre-computed Report file for secondary AI / partner layout
#
# Post-led by design: Big Post agree% is primary; Big Pre and paired shifts only
# when N allows. Little Post is UX/sat, not overall impact.

.pr_flatten_vec <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x) && !is.data.frame(x)) {
    return(vapply(seq_along(x), function(i) {
      xi <- x[[i]]
      if (is.null(xi) || length(xi) == 0L) return(NA_character_)
      if (is.list(xi)) xi <- unlist(xi, recursive = TRUE, use.names = FALSE)
      paste(as.character(xi), collapse = ", ")
    }, character(1)))
  }
  as.character(x)
}

.pr_likert_agree <- function(x) {
  # Index scale −3/−1/+1/+3 or labels Agree/Strongly Agree
  if (is.null(x)) return(list(agree_n = 0L, n = 0L, agree_pct = NA_real_))
  v <- .pr_flatten_vec(x)
  ch <- trimws(as.character(v))
  ch <- ch[!is.na(ch) & nzchar(ch) & !grepl("prefer not|^na$", ch, ignore.case = TRUE)]
  n <- length(ch)
  if (!n) return(list(agree_n = 0L, n = 0L, agree_pct = NA_real_))
  agree <- grepl("strongly agree|^agree$", ch, ignore.case = TRUE)
  num <- suppressWarnings(as.numeric(ch))
  if (sum(!is.na(num)) >= max(1L, floor(0.8 * n))) {
    agree <- !is.na(num) & num > 0
  }
  an <- as.integer(sum(agree, na.rm = TRUE))
  list(agree_n = an, n = as.integer(n), agree_pct = round(100 * an / n, 1))
}

.pr_top2_sat <- function(x) {
  ch <- .pr_flatten_vec(x)
  num <- suppressWarnings(as.numeric(ch))
  num <- num[is.finite(num)]
  n <- length(num)
  if (!n) return(list(top2_pct = NA_real_, mean = NA_real_, n = 0L))
  list(
    top2_pct = round(100 * mean(num >= 5), 1),
    mean = round(mean(num), 2),
    n = as.integer(n)
  )
}

.pr_find_col <- function(df, patterns) {
  if (is.null(df) || !ncol(df)) return(NULL)
  nms <- names(df)
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NULL
}

.pr_agree_table_for_cols <- function(df, cols, short_labels = NULL) {
  if (is.null(df) || !nrow(df) || !length(cols)) {
    return(data.frame(
      item = character(0), agree_n = integer(0), n = integer(0),
      agree_pct = numeric(0), stringsAsFactors = FALSE
    ))
  }
  cols <- intersect(cols, names(df))
  rows <- lapply(seq_along(cols), function(i) {
    nm <- cols[i]
    lab <- if (!is.null(short_labels) && length(short_labels) >= i) short_labels[[i]] else nm
    a <- .pr_likert_agree(df[[nm]])
    data.frame(
      item = as.character(lab),
      agree_n = a$agree_n,
      n = a$n,
      agree_pct = a$agree_pct,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.pr_overall_agree <- function(item_tbl) {
  if (is.null(item_tbl) || !nrow(item_tbl)) {
    return(list(agree_n = 0L, n = 0L, agree_pct = NA_real_))
  }
  an <- sum(item_tbl$agree_n, na.rm = TRUE)
  n <- sum(item_tbl$n, na.rm = TRUE)
  list(
    agree_n = as.integer(an),
    n = as.integer(n),
    agree_pct = if (n > 0) round(100 * an / n, 1) else NA_real_
  )
}

.pr_pick_quotes <- function(df, max_n = 8L) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(
      text = character(0), partner = character(0), module = character(0),
      session_date = character(0), stringsAsFactors = FALSE
    ))
  }
  story_cols <- grep("impacted you|stood out|apply something|helped or impacted",
                     names(df), ignore.case = TRUE, value = TRUE)
  if (!length(story_cols)) return(data.frame(
    text = character(0), partner = character(0), module = character(0),
    session_date = character(0), stringsAsFactors = FALSE
  ))
  col <- story_cols[1]
  txt <- .pr_flatten_vec(df[[col]])
  if (exists("split_bilingual_open_text", mode = "function")) {
    # Prefer English side when delimiter present
    txt <- vapply(txt, function(t) {
      if (is.na(t) || !nzchar(t)) return(NA_character_)
      sp <- tryCatch(split_bilingual_open_text(t), error = function(e) NULL)
      if (is.null(sp)) return(trimws(t))
      en <- sp$english %||% sp$en %||% NA_character_
      if (!is.na(en) && nzchar(en)) en else trimws(t)
    }, character(1))
  } else {
    txt <- gsub("\\(%-\\^-%\\).*$", "", txt)
    txt <- trimws(txt)
  }
  ok <- !is.na(txt) & nzchar(txt) & nchar(txt) >= 20L
  if (!any(ok)) {
    return(data.frame(
      text = character(0), partner = character(0), module = character(0),
      session_date = character(0), stringsAsFactors = FALSE
    ))
  }
  sub <- df[ok, , drop = FALSE]
  txt <- txt[ok]
  # Prefer Big Post when flag present
  if ("is_big_post" %in% names(sub)) {
    bp <- sub$is_big_post %in% TRUE
    if (any(bp)) {
      sub <- sub[bp, , drop = FALSE]
      txt <- txt[bp]
    }
  }
  take <- seq_len(min(max_n, length(txt)))
  data.frame(
    text = txt[take],
    partner = if ("org_name" %in% names(sub)) as.character(sub$org_name[take]) else NA_character_,
    module = if ("modules_taught" %in% names(sub)) as.character(sub$modules_taught[take]) else NA_character_,
    session_date = if ("session_date" %in% names(sub)) as.character(sub$session_date[take]) else NA_character_,
    publish_ok = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Build partner Report markdown from an export bundle.
build_partner_report_markdown <- function(bundle) {
  snap <- bundle$snap %||% list()
  thr <- SUPPRESSION_N_DEFAULT
  if (exists("SUPPRESSION_N_DEFAULT")) thr <- SUPPRESSION_N_DEFAULT

  org_lab <- .export_fmt_list(snap$org %||% character(0), empty = "All partners")
  scope_type <- if (!length(snap$org)) "all_partners" else if (length(snap$org) == 1L) "partner" else "partner"
  partner_group <- NA_character_
  if (length(snap$org) == 1L && exists("partner_group_for_org", mode = "function")) {
    partner_group <- partner_group_for_org(snap$org[[1]])
  }

  big_pre <- bundle$big_pre %||% data.frame()
  big_post <- bundle$big_post %||% data.frame()
  little_post <- bundle$little_post %||% data.frame()
  annual <- bundle$annual %||% data.frame()

  bp_n <- .export_safe_n(big_post)
  bpre_n <- .export_safe_n(big_pre)

  lines <- c(
    "# 5 Buckets Partner Report (for AI)",
    "",
    "## A. Header",
    "",
    paste0("- **scope_label:** ", org_lab),
    paste0("- **scope_type:** ", scope_type),
    paste0("- **partner_group:** ", if (is.na(partner_group) || !nzchar(partner_group)) "_unmapped_" else partner_group),
    paste0("- **period_type:** ", if (isTRUE(snap$date_enabled)) "custom" else "all_available"),
    paste0("- **filters_applied:** org=", org_lab,
           "; group=", .export_fmt_list(snap$group %||% character(0)),
           "; language=", snap$language %||% "All"),
    paste0("- **generated_at:** ", snap$exported_at %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- **suppression_threshold:** ", thr),
    paste0("- **design_note:** Outcomes are **Big Post–led**. Missing Little Pre is intentional for some partners. Pre↔Post shifts only when Big Pre pairs exist."),
    ""
  )

  # Delivery
  all_post <- tryCatch({
    dplyr::bind_rows(
      if (nrow(big_post)) big_post else NULL,
      if (nrow(little_post)) little_post else NULL
    )
  }, error = function(e) big_post)
  sess_ids <- character(0)
  if (nrow(all_post) && "session_id" %in% names(all_post)) {
    sess_ids <- unique(as.character(all_post$session_id))
  }
  if (nrow(big_pre) && "session_id" %in% names(big_pre)) {
    sess_ids <- unique(c(sess_ids, as.character(big_pre$session_id)))
  }
  sessions_distinct <- if (exists("base_session_id", mode = "function") && length(sess_ids)) {
    length(unique(base_session_id(sess_ids)))
  } else {
    length(sess_ids)
  }
  learners <- NA_integer_
  if (nrow(all_post) && "respondent_id" %in% names(all_post)) {
    ids <- as.character(all_post$respondent_id)
    ids <- ids[!is.na(ids) & nzchar(ids) & !grepl("^ANON", ids)]
    learners <- length(unique(ids))
  }
  mods <- if (nrow(all_post) && "modules_taught" %in% names(all_post)) {
    sort(unique(trimws(as.character(all_post$modules_taught))))
  } else character(0)
  mods <- mods[nzchar(mods) & !is.na(mods)]
  langs <- if (nrow(all_post) && "language" %in% names(all_post)) {
    lv <- trimws(tolower(as.character(all_post$language)))
    lv[is.na(lv) | !nzchar(lv)] <- "en"
    sort(unique(lv))
  } else character(0)
  facs <- if (nrow(all_post) && "facilitators" %in% names(all_post)) {
    sort(unique(trimws(as.character(all_post$facilitators))))
  } else character(0)
  facs <- facs[nzchar(facs)]

  lines <- c(
    lines,
    "## B. Delivery facts",
    "",
    paste0("- **sessions_distinct:** ", sessions_distinct, " (language variants collapsed)"),
    paste0("- **learners_reached:** ", learners, " (non-anonymous respondent_id)"),
    paste0("- **modules_delivered:** ", if (length(mods)) paste(mods, collapse = "; ") else "_none_"),
    paste0("- **languages_delivered:** ", if (length(langs)) paste(langs, collapse = "; ") else "_none_"),
    paste0("- **facilitators:** ", if (length(facs)) paste(facs, collapse = "; ") else "_none_"),
    paste0("- **Big Pre N:** ", bpre_n, " | **Big Post N:** ", bp_n,
           " | **Little Post N:** ", .export_safe_n(little_post)),
    ""
  )

  # Outcomes — Compared-to columns on Big Post
  post_cols <- grep("Compared to before", names(big_post), value = TRUE)
  pre_cols <- character(0)
  if (exists("PRE_WELLNESS_COLS")) {
    pre_cols <- intersect(PRE_WELLNESS_COLS, names(big_pre))
  } else {
    pre_cols <- grep("I (have|am|feel|would)", names(big_pre), value = TRUE)
  }

  lines <- c(lines, "## C. Outcomes (agree %)", "")
  if (bp_n < thr) {
    lines <- c(
      lines,
      paste0("**Big Post outcomes: unavailable** — Big Post N=", bp_n,
             " (below suppression ", thr, ")."),
      "Do not substitute all-partner figures.",
      ""
    )
  } else if (!length(post_cols)) {
    lines <- c(lines, "**Big Post outcomes: unavailable** — no Compared-to-before columns in scope.", "")
  } else {
    short <- sub(".*\\[(.*)\\].*", "\\1", post_cols)
    short[short == post_cols] <- post_cols
    post_tbl <- .pr_agree_table_for_cols(big_post, post_cols, short)
    ov <- .pr_overall_agree(post_tbl)
    lines <- c(
      lines,
      paste0("### Big Post overall agree%: **", ov$agree_pct, "%** (agree_n=", ov$agree_n, ", cell_n=", ov$n, ")"),
      "",
      .export_md_table(post_tbl),
      ""
    )
    if (bpre_n >= thr && length(pre_cols)) {
      pre_short <- sub(".*\\[(.*)\\].*", "\\1", pre_cols)
      pre_tbl <- .pr_agree_table_for_cols(big_pre, pre_cols, pre_short)
      pov <- .pr_overall_agree(pre_tbl)
      lines <- c(
        lines,
        paste0("### Big Pre overall agree%: **", pov$agree_pct, "%** (n_cells=", pov$n, ")"),
        "",
        paste0("- **shift_overall_pp:** ",
               if (is.finite(ov$agree_pct) && is.finite(pov$agree_pct))
                 round(ov$agree_pct - pov$agree_pct, 1) else "NA"),
        "- **construct_caveat:** Pre wellness wording differs from Post compared-to items; treat shift as directional.",
        ""
      )
    } else {
      lines <- c(
        lines,
        paste0("**Pre↔Post shift:** unavailable (Big Pre N=", bpre_n, "). Report is Post-led."),
        ""
      )
    }
  }

  # Behavior
  lines <- c(lines, "## D. Behavior", "")
  pc <- bundle$planned_counts %||% data.frame()
  if (is.data.frame(pc) && nrow(pc)) {
    lines <- c(lines, .export_md_table(pc), "")
  } else {
    lines <- c(lines, "_No planned-action counts in scope._", "")
  }

  # Reach composites
  lines <- c(lines, "## E. Reach (composites)", "")
  demo_src <- if (bp_n >= thr) big_post else all_post
  if (.export_safe_n(demo_src) < thr) {
    lines <- c(lines, paste0("**Reach suppressed** — N=", .export_safe_n(demo_src), "."), "")
  } else {
    inc_col <- .pr_find_col(demo_src, c("^Household Income$", "Household Income"))
    race_col <- .pr_find_col(demo_src, c("^Race", "Ethnicity"))
    age_col <- .pr_find_col(demo_src, c("^Age Group$", "Age Group"))
    if (!is.null(inc_col) && exists("canonicalize_income_band")) {
      bands <- canonicalize_income_band(demo_src[[inc_col]])
      comp <- income_composite_shares(bands)
      lines <- c(
        lines,
        paste0("- **lmi_pct:** ", comp$lmi_pct, "% (n=", comp$n, ")"),
        paste0("- **very_low_income_pct:** ", comp$very_low_income_pct, "%"),
        ""
      )
    }
    if (!is.null(race_col) && exists("poc_and_hispanic_shares")) {
      rc <- poc_and_hispanic_shares(demo_src[[race_col]])
      lines <- c(
        lines,
        paste0("- **poc_pct:** ", rc$poc_pct, "% (n=", rc$n, ")"),
        paste0("- **hispanic_latino_pct:** ", rc$hispanic_latino_pct, "%"),
        ""
      )
    }
    if (!is.null(age_col) && exists("youth_12_24_share")) {
      yc <- youth_12_24_share(demo_src[[age_col]])
      lines <- c(lines, paste0("- **youth_12_24_pct:** ", yc$youth_12_24_pct, "% (n=", yc$n, ")"), "")
    }
    lines <- c(lines, "- **disability:** withheld pending Pre/Post discrepancy review.", "")
  }

  # Quality
  lines <- c(lines, "## F. Quality", "")
  sat_col <- .pr_find_col(
    if (nrow(little_post)) little_post else big_post,
    c("How satisfied were you", "satisfied")
  )
  sat_df <- if (nrow(little_post)) little_post else big_post
  if (!is.null(sat_col)) {
    sat <- .pr_top2_sat(sat_df[[sat_col]])
    lines <- c(
      lines,
      paste0("- **satisfaction_top2_pct (5–6):** ", sat$top2_pct, "% (mean=", sat$mean, ", n=", sat$n, ")")
    )
  }
  nps <- bundle$nps %||% list(nps = NA)
  lines <- c(
    lines,
    paste0("- **nps:** ", nps$nps %||% NA),
    paste0("- **promoters_pct:** ", nps$promoter_pct %||% nps$promoters_pct %||% NA),
    paste0("- **detractors_pct:** ", nps$detractor_pct %||% nps$detractors_pct %||% NA),
    ""
  )

  # Quotes
  lines <- c(lines, "## G. Quotes", "")
  qdf <- .pr_pick_quotes(if (nrow(big_post)) big_post else little_post)
  if (!nrow(qdf)) {
    lines <- c(lines, "_No publishable quotes in scope (or all empty)._", "")
  } else {
    for (i in seq_len(nrow(qdf))) {
      lines <- c(
        lines,
        paste0(i, ". \"", gsub("\"", "'", qdf$text[i]), "\""),
        paste0("   — ", qdf$partner[i], " | ", qdf$module[i], " | ", qdf$session_date[i]),
        ""
      )
    }
  }

  # YoY stub by fiscal year if dates exist
  lines <- c(lines, "## H. Year over year (fiscal)", "")
  date_vec <- NULL
  if (nrow(all_post) && "session_date" %in% names(all_post) && exists("fiscal_year_label")) {
    date_vec <- tryCatch({
      if (exists(".plausible_session_date")) .plausible_session_date(all_post$session_date)
      else as.Date(all_post$session_date)
    }, error = function(e) as.Date(rep(NA, nrow(all_post))))
  }
  if (!is.null(date_vec) && any(!is.na(date_vec)) && exists("fiscal_year_label")) {
    fy <- fiscal_year_label(date_vec)
    tab <- as.data.frame(table(fy = fy), stringsAsFactors = FALSE)
    names(tab)[2] <- "post_responses"
    lines <- c(lines, .export_md_table(tab), "")
  } else {
    lines <- c(lines, "_FY rollup unavailable for this scope._", "")
  }

  # Annual
  lines <- c(lines, "## I. Annual survey", "")
  an <- .export_safe_n(annual)
  lines <- c(
    lines,
    paste0("- **annual_n:** ", an),
    paste0("- **ready_flag:** ", if (an >= thr) "true" else "false"),
    if (an < thr) "_Below suppression — included in ZIP raw when requested, not used for claims._" else "_See annual.csv / Analyst file for measures._",
    ""
  )

  paste(lines, collapse = "\n")
}
