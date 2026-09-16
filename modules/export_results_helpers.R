# Results MD helpers — Pre/Post change tables, meta-map, figure context

.export_fmt_p <- function(p) {
  if (is.null(p) || !is.finite(p)) return("—")
  if (exists("format_p_value", mode = "function")) {
    return(tryCatch(format_p_value(p), error = function(e) sprintf("%.4f", p)))
  }
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

.export_valid_rid <- function(ids) {
  ids <- trimws(as.character(ids))
  !is.na(ids) & nzchar(ids) & !grepl("^ANON", ids, ignore.case = TRUE)
}

.export_indep_ttest_row <- function(label, pre, post) {
  pre <- .export_flatten_num(pre)
  post <- .export_flatten_num(post)
  pre <- pre[is.finite(pre)]
  post <- post[is.finite(post)]
  n_pre <- length(pre)
  n_post <- length(post)
  avg_pre <- if (n_pre) round(mean(pre), 3) else NA_real_
  avg_post <- if (n_post) round(mean(post), 3) else NA_real_
  sd_pre <- if (n_pre > 1) round(stats::sd(pre), 3) else NA_real_
  sd_post <- if (n_post > 1) round(stats::sd(post), 3) else NA_real_
  diff <- if (n_pre && n_post) round(avg_post - avg_pre, 3) else NA_real_
  tt <- if (n_pre >= 2 && n_post >= 2) {
    tryCatch(stats::t.test(pre, post, alternative = "two.sided", var.equal = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  data.frame(
    Comparison = label,
    Design = "Between-subjects (independent)",
    Pre_N = n_pre,
    Pre_Mean = avg_pre,
    Pre_SD = sd_pre,
    Post_N = n_post,
    Post_Mean = avg_post,
    Post_SD = sd_post,
    Diff_Post_minus_Pre = diff,
    t = if (!is.null(tt)) round(unname(tt$statistic), 3) else NA_real_,
    p = if (!is.null(tt)) .export_fmt_p(tt$p.value) else "—",
    CI_95 = if (!is.null(tt)) {
      paste0("[", round(tt$conf.int[1], 3), ", ", round(tt$conf.int[2], 3), "]")
    } else {
      "—"
    },
    Note = "Pre absolute wellness vs Post *perceived change* are related but not identical constructs.",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.export_paired_ttest_row <- function(label, paired_df) {
  # paired_df columns: pre, post
  if (is.null(paired_df) || !nrow(paired_df)) {
    return(data.frame(
      Comparison = label,
      Design = "Within-subjects (paired)",
      N_pairs = 0L,
      Pre_Mean = NA_real_,
      Post_Mean = NA_real_,
      Mean_change = NA_real_,
      SD_change = NA_real_,
      t = NA_real_,
      p = "—",
      CI_95 = "—",
      Note = "No paired respondent_id matches with valid scores.",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  pre <- .export_flatten_num(paired_df$pre)
  post <- .export_flatten_num(paired_df$post)
  ok <- is.finite(pre) & is.finite(post)
  pre <- pre[ok]
  post <- post[ok]
  ch <- post - pre
  n <- length(ch)
  tt <- if (n >= 2) {
    tryCatch(stats::t.test(ch, mu = 0, alternative = "two.sided"), error = function(e) NULL)
  } else {
    NULL
  }
  data.frame(
    Comparison = label,
    Design = "Within-subjects (paired)",
    N_pairs = n,
    Pre_Mean = if (n) round(mean(pre), 3) else NA_real_,
    Post_Mean = if (n) round(mean(post), 3) else NA_real_,
    Mean_change = if (n) round(mean(ch), 3) else NA_real_,
    SD_change = if (n > 1) round(stats::sd(ch), 3) else NA_real_,
    t = if (!is.null(tt)) round(unname(tt$statistic), 3) else NA_real_,
    p = if (!is.null(tt)) .export_fmt_p(tt$p.value) else "—",
    CI_95 = if (!is.null(tt)) {
      paste0("[", round(tt$conf.int[1], 3), ", ", round(tt$conf.int[2], 3), "]")
    } else {
      "—"
    },
    Note = "Same person Pre→Post via respondent_id; still interpret Post as perceived-change scale.",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.export_pair_index_df <- function(big_pre, big_post, pre_idx, post_idx) {
  if (!nrow(big_pre) || !nrow(big_post)) return(data.frame())
  if (!"respondent_id" %in% names(big_pre) || !"respondent_id" %in% names(big_post)) return(data.frame())
  if (length(pre_idx) != nrow(big_pre) || length(post_idx) != nrow(big_post)) return(data.frame())
  pre_df <- data.frame(
    respondent_id = big_pre$respondent_id,
    pre = .export_flatten_num(pre_idx),
    stringsAsFactors = FALSE
  )
  post_df <- data.frame(
    respondent_id = big_post$respondent_id,
    post = .export_flatten_num(post_idx),
    stringsAsFactors = FALSE
  )
  pre_df <- pre_df[.export_valid_rid(pre_df$respondent_id) & is.finite(pre_df$pre), , drop = FALSE]
  post_df <- post_df[.export_valid_rid(post_df$respondent_id) & is.finite(post_df$post), , drop = FALSE]
  # one row per id (latest-ish: keep first)
  pre_df <- pre_df[!duplicated(pre_df$respondent_id), , drop = FALSE]
  post_df <- post_df[!duplicated(post_df$respondent_id), , drop = FALSE]
  merge(pre_df, post_df, by = "respondent_id")
}

.export_wellness_item_pairs <- function() {
  if (!exists("PRE_FINANCIAL_WELLNESS_COLS") || !exists("POST_COMPARED_TO_COLS")) return(list())
  pre <- PRE_FINANCIAL_WELLNESS_COLS
  post <- POST_COMPARED_TO_COLS
  if (length(pre) < 8 || length(post) < 9) return(list())
  list(
    list(key = "awareness_amount", label = "Awareness: How much money I have", pre = pre[1], post = post[2]),
    list(key = "awareness_afford", label = "Awareness: What I can afford", pre = pre[2], post = post[3]),
    list(key = "awareness_where", label = "Awareness: Where my money goes", pre = pre[3], post = post[4]),
    list(key = "optimism", label = "Financial future optimism", pre = pre[4], post = post[5]),
    list(key = "relationship", label = "Healthy relationship with money", pre = pre[5], post = post[6]),
    list(key = "stress", label = "Managing financial stress", pre = pre[6], post = post[7]),
    list(key = "confidence", label = "Planning confidence", pre = pre[7], post = post[8]),
    list(key = "comfort_professionals", label = "Comfort with financial professionals", pre = pre[8], post = post[9])
  )
}

.export_find_pair_col <- function(df, wanted) {
  if (is.null(wanted) || !nzchar(wanted) || is.null(df) || !ncol(df)) return(NULL)
  if (wanted %in% names(df)) return(wanted)
  .export_find_col(df, exact = wanted, pattern = gsub("\\[.*\\]", ".*", wanted))
}

.export_wellness_item_between_table <- function(big_pre, big_post) {
  pairs <- .export_wellness_item_pairs()
  if (!length(pairs) || !nrow(big_pre) || !nrow(big_post)) return(data.frame())
  if (!exists("likert_to_numeric", mode = "function")) return(data.frame())
  rows <- list()
  for (pair in pairs) {
    pre_col <- .export_find_pair_col(big_pre, pair$pre)
    post_col <- .export_find_pair_col(big_post, pair$post)
    if (is.null(pre_col) || is.null(post_col)) next
    pre_vals <- likert_to_numeric(.export_flatten_chr(big_pre[[pre_col]]))
    post_vals <- likert_to_numeric(.export_flatten_chr(big_post[[post_col]]))
    pre_vals <- pre_vals[is.finite(pre_vals)]
    post_vals <- post_vals[is.finite(post_vals)]
    if (length(pre_vals) < 2 || length(post_vals) < 2) next
    tt <- tryCatch(stats::t.test(pre_vals, post_vals, alternative = "two.sided", var.equal = FALSE),
                   error = function(e) NULL)
    if (is.null(tt)) next
    mean_diff <- mean(post_vals) - mean(pre_vals)
    rows[[length(rows) + 1L]] <- data.frame(
      Item = pair$label,
      Concept_key = pair$key,
      Pre_col = pre_col,
      Post_col = post_col,
      Pre_M = round(mean(pre_vals), 3),
      Pre_SD = round(stats::sd(pre_vals), 3),
      Pre_n = length(pre_vals),
      Post_M = round(mean(post_vals), 3),
      Post_SD = round(stats::sd(post_vals), 3),
      Post_n = length(post_vals),
      Diff = round(mean_diff, 3),
      t = round(unname(tt$statistic), 3),
      p = .export_fmt_p(tt$p.value),
      CI_95 = paste0("[", round(tt$conf.int[1], 3), ", ", round(tt$conf.int[2], 3), "]"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  df <- do.call(rbind, rows)
  df[order(-df$Diff), , drop = FALSE]
}

.export_wellness_item_within_table <- function(big_pre, big_post) {
  pairs <- .export_wellness_item_pairs()
  if (!length(pairs) || !nrow(big_pre) || !nrow(big_post)) return(data.frame())
  if (!exists("likert_to_numeric", mode = "function")) return(data.frame())
  if (!"respondent_id" %in% names(big_pre) || !"respondent_id" %in% names(big_post)) return(data.frame())
  rows <- list()
  for (pair in pairs) {
    pre_col <- .export_find_pair_col(big_pre, pair$pre)
    post_col <- .export_find_pair_col(big_post, pair$post)
    if (is.null(pre_col) || is.null(post_col)) next
    pre_num <- likert_to_numeric(.export_flatten_chr(big_pre[[pre_col]]))
    post_num <- likert_to_numeric(.export_flatten_chr(big_post[[post_col]]))
    pre_df <- data.frame(respondent_id = big_pre$respondent_id, pre = pre_num, stringsAsFactors = FALSE)
    post_df <- data.frame(respondent_id = big_post$respondent_id, post = post_num, stringsAsFactors = FALSE)
    pre_df <- pre_df[.export_valid_rid(pre_df$respondent_id) & is.finite(pre_df$pre), , drop = FALSE]
    post_df <- post_df[.export_valid_rid(post_df$respondent_id) & is.finite(post_df$post), , drop = FALSE]
    pre_df <- pre_df[!duplicated(pre_df$respondent_id), , drop = FALSE]
    post_df <- post_df[!duplicated(post_df$respondent_id), , drop = FALSE]
    merged <- merge(pre_df, post_df, by = "respondent_id")
    if (nrow(merged) < 2) next
    ch <- merged$post - merged$pre
    tt <- tryCatch(stats::t.test(ch, mu = 0, alternative = "two.sided"), error = function(e) NULL)
    if (is.null(tt)) next
    rows[[length(rows) + 1L]] <- data.frame(
      Item = pair$label,
      Concept_key = pair$key,
      N_pairs = nrow(merged),
      Mean_change = round(mean(ch), 3),
      SD_change = round(stats::sd(ch), 3),
      t = round(unname(tt$statistic), 3),
      p = .export_fmt_p(tt$p.value),
      CI_95 = paste0("[", round(tt$conf.int[1], 3), ", ", round(tt$conf.int[2], 3), "]"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  df <- do.call(rbind, rows)
  df[order(-df$Mean_change), , drop = FALSE]
}

.export_behavior_item_between_table <- function(past_counts, planned_counts, n_pre, n_post) {
  if (is.null(past_counts) || !is.data.frame(past_counts) || !nrow(past_counts)) return(data.frame())
  if (is.null(planned_counts) || !is.data.frame(planned_counts) || !nrow(planned_counts)) return(data.frame())
  lab_col <- names(past_counts)[1]
  n_col_p <- if ("N" %in% names(past_counts)) "N" else names(past_counts)[2]
  n_col_pl <- if ("N" %in% names(planned_counts)) "N" else names(planned_counts)[2]
  labs <- unique(c(as.character(past_counts[[lab_col]]), as.character(planned_counts[[1]])))
  labs <- labs[!is.na(labs) & nzchar(labs)]
  if (!length(labs) || !n_pre || !n_post) return(data.frame())
  pre_n <- past_counts[[n_col_p]][match(labs, past_counts[[lab_col]])]
  post_n <- planned_counts[[n_col_pl]][match(labs, planned_counts[[1]])]
  pre_n[is.na(pre_n)] <- 0
  post_n[is.na(post_n)] <- 0
  pre_pct <- 100 * as.numeric(pre_n) / n_pre
  post_pct <- 100 * as.numeric(post_n) / n_post
  df <- data.frame(
    Behavior = labs,
    Pre_N = as.integer(pre_n),
    Pre_Pct = round(pre_pct, 1),
    Post_N = as.integer(post_n),
    Post_Pct = round(post_pct, 1),
    Diff_pp = round(post_pct - pre_pct, 1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df[order(-df$Diff_pp), , drop = FALSE]
}

.export_change_hist_table <- function(values, breaks = NULL) {
  v <- .export_flatten_num(values)
  v <- v[is.finite(v)]
  if (!length(v)) return(data.frame())
  if (is.null(breaks)) {
    rng <- range(v)
    if (!all(is.finite(rng)) || diff(rng) == 0) rng <- c(rng[1] - 0.5, rng[2] + 0.5)
    breaks <- seq(rng[1], rng[2], length.out = 13L)
  }
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)
  data.frame(
    bin_mid = round(h$mids, 3),
    bin_lo = round(h$breaks[-length(h$breaks)], 3),
    bin_hi = round(h$breaks[-1], 3),
    count = as.integer(h$counts),
    stringsAsFactors = FALSE
  )
}

.export_results_meta_map_md <- function(sections_present) {
  all_rows <- list(
    overview = "| Overview | Workshop volume & pairing | Who came / how series are structured | `big_*`/`little_*` counts; `respondent_id` |",
    reach = "| Reach | Who is served | Demographics of learners | Age, gender, income, education, race (+ optional Annual) on Big Pre/Post |",
    wellness = "| **Financial Wellness** | **Mindset shift (priority)** | Pre absolute wellness + Post perceived change + Pre↔Post tests | 8 Pre Likert; 9 Post Compared-to; indices; item/index between & within |",
    behavioral = "| **Behavioral Readiness** | **Behavior shift (priority)** | Past behaviors → planned actions | Multi-select canonical labels; indices 0–8; Pre vs Post diffs |",
    satisfaction = "| Satisfaction | Session quality | Sat + NPS + by modules | Session sat 1–6; recommend 0–10; `modules_taught` |",
    learning = "| Learning & Impact Stories | Qualitative signal | Understanding, interest, open-text themes | Compared-to understanding; keep-in-touch; word freq |",
    annual = "| Annual Survey | Longer-term follow-up | Compared-to / recommend / behaviors ~1y later | Annual headers via patterns; join on `respondent_id` |"
  )
  present <- intersect(names(all_rows), sections_present)
  if (!length(present)) present <- names(all_rows)
  # Priority ordering
  order_pref <- c("wellness", "behavioral", "reach", "satisfaction", "learning", "overview", "annual")
  present <- c(intersect(order_pref, present), setdiff(present, order_pref))
  paste(
    c(
      "## Results document map (how to read this file)",
      "",
      "This Results MD mirrors the dashboard **Impact** tabs. Each `##` section below is one tab.",
      "Inside a section, each `###` block is one analysis/figure with: context, variables, CSV universe,",
      "numeric tables, and a **figure recipe** so you can recreate the chart without PNGs.",
      "",
      "**Read priority for partner insights:** Financial Wellness Pre↔Post → Behavioral Pre↔Post → Reach → Satisfaction/Stories → Overview/Annual.",
      "",
      "| Tab (section) | Why it matters | What \"good\" looks like | Core variables / outputs |",
      "| --- | --- | --- | --- |",
      unlist(all_rows[present], use.names = FALSE),
      "",
      "Pair with **Data Map (md for AI)** for scoring rules, Big/Little, and join recipes.",
      "Do **not** treat Post Compared-to means as the same scale as Pre wellness means without the caveats in each Pre↔Post block.",
      ""
    ),
    collapse = "\n"
  )
}

.export_section_intro_md <- function(section_id) {
  intros <- list(
    overview = paste0(
      "**Section purpose:** Establish N, survey typing (Big/Little), and how many people can be paired.\n\n",
      "**Variables / keys:** `session_id`, `base_session_id`, `is_big_pre`/`is_big_post`, `respondent_id`, org/group.\n\n",
      "**CSV universe:** `big_pre.csv`, `little_pre.csv`, `little_post.csv`, `big_post.csv`, `annual.csv`."
    ),
    reach = paste0(
      "**Section purpose:** Describe who is being served (priority for equity / partner reporting).\n\n",
      "**Variables:** Age Group; Gender Identity; Household Income (exact band strings); Education; Race/Ethnicity; ",
      "optional First-Gen / Veteran / Disability / Neurodivergent when present.\n\n",
      "**CSV universe:** Prefer `big_pre.csv` / `big_post.csv` (Little demos are mostly empty)."
    ),
    wellness = paste0(
      "**Section purpose (priority):** Money-mindset signal — Pre absolute agreement vs Post perceived improvement, ",
      "plus Pre↔Post between- and within-subjects tests.\n\n",
      "**Variables:** 8 Pre Financial Wellness Likert; 9 Post Compared-to Likert; Financial Wellness Index; Post Impact Index; `respondent_id` for pairs.\n\n",
      "**CSV universe:** `big_pre.csv`, `big_post.csv` only.\n\n",
      "**Interpretation trap:** Post items ask *compared to before 5 Buckets* — not the same wording as Pre. ",
      "Report both indices; for paired change, state the construct caveat."
    ),
    behavioral = paste0(
      "**Section purpose (priority):** Behavior readiness — what people already did (Pre) vs plan to do (Post).\n\n",
      "**Variables:** Past behaviors multi-select; Planned actions multi-select; indices = distinct canonical label counts (0–8).\n\n",
      "**CSV universe:** `big_pre.csv`, `big_post.csv`."
    ),
    satisfaction = paste0(
      "**Section purpose:** Session quality and advocacy.\n\n",
      "**Variables:** Session satisfaction (1–6); NPS recommend (0–10); `modules_taught`.\n\n",
      "**CSV universe:** Post rows — satisfaction often on Little+Big (`post_all` / both); NPS strongest on `big_post.csv`."
    ),
    learning = paste0(
      "**Section purpose:** Learning takeaways and stories (qualitative + light quant).\n\n",
      "**Variables:** Topic understanding Compared-to item; keep-in-touch multi-select; open-text columns (stood out / apply / helpful / impact story).\n\n",
      "**CSV universe:** Primarily `big_post.csv` (+ Little Post for session reflections)."
    ),
    annual = paste0(
      "**Section purpose:** ~1-year follow-up outcomes.\n\n",
      "**Variables:** Annual Compared-to grid (headers differ from Post); recommend; behavior multi-select; money-change categoricals.\n\n",
      "**CSV universe:** `annual.csv`; join people with workshop files on `respondent_id`."
    )
  )
  intros[[section_id]] %||% ""
}
