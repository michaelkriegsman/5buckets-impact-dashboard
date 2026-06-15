# process_data.R - Data processing and aggregation functions
# Feb 2026: loads question mapping for comparable variable identification
if (file.exists("data/question_mapping.R")) {
  source("data/question_mapping.R", local = TRUE)
}

# ============================================================================
# Session-Level Aggregation
# ============================================================================

# Aggregate pre-survey responses to session level
aggregate_pre_to_sessions <- function(pre_data, program_manager_data) {
  if (nrow(pre_data) == 0 || nrow(program_manager_data) == 0) {
    return(data.frame())
  }
  
  # Group by session_id and calculate metrics
  session_pre <- pre_data %>%
    group_by(session_id) %>%
    summarise(
      pre_responses = n(),
      .groups = 'drop'
    )
  
  return(session_pre)
}

# Aggregate post-survey responses to session level
aggregate_post_to_sessions <- function(post_data, program_manager_data) {
  if (nrow(post_data) == 0 || nrow(program_manager_data) == 0) {
    return(data.frame())
  }
  
  # Group by session_id and calculate metrics
  session_post <- post_data %>%
    group_by(session_id) %>%
    summarise(
      post_responses = n(),
      .groups = 'drop'
    )
  
  return(session_post)
}

# Program Manager → one row per session_id for joining to Master (session_number, sessions_in_series, date).
prep_program_manager_session_join <- function(program_manager) {
  if (is.null(program_manager) || nrow(program_manager) == 0) return(data.frame())
  nms <- names(program_manager)
  pick_col <- function(cands) {
    for (c in cands) {
      hit <- nms[tolower(nms) == tolower(c)]
      if (length(hit)) return(hit[1])
    }
    NA_character_
  }
  sid_nm <- pick_col(c("session_id", "Session ID", "session link"))
  if (is.na(sid_nm)) return(data.frame())
  pm <- program_manager
  if (sid_nm != "session_id") {
    pm <- pm %>% dplyr::rename(session_id = dplyr::all_of(sid_nm))
  }
  pm <- pm %>%
    dplyr::mutate(session_id = trimws(as.character(.data[["session_id"]]))) %>%
    dplyr::filter(!is.na(.data[["session_id"]]), .data[["session_id"]] != "")
  if (nrow(pm) == 0) return(data.frame())
  nms2 <- names(pm)
  sn_nm <- {
    hit <- nms2[tolower(nms2) %in% c("session_number", "session number", "workshop_number")]
    if (length(hit)) hit[1] else NA_character_
  }
  sis_nm <- {
    hit <- nms2[tolower(nms2) %in% c("sessions_in_series", "sessions in series", "series_length", "workshops_in_series")]
    if (length(hit)) hit[1] else NA_character_
  }
  date_nm <- {
    hit <- nms2[tolower(nms2) == "date"]
    if (length(hit)) hit[1] else NA_character_
  }
  sn <- if (!is.na(sn_nm)) suppressWarnings(as.integer(pm[[sn_nm]])) else rep(NA_integer_, nrow(pm))
  sis <- if (!is.na(sis_nm)) suppressWarnings(as.numeric(pm[[sis_nm]])) else rep(NA_real_, nrow(pm))
  pmd <- if (!is.na(date_nm)) {
    tryCatch(as.Date(pm[[date_nm]]), error = function(e) rep(as.Date(NA), nrow(pm)))
  } else {
    rep(as.Date(NA), nrow(pm))
  }
  out <- data.frame(
    session_id = pm$session_id,
    pm_session_number = sn,
    pm_sessions_in_series = sis,
    pm_workshop_date = pmd,
    stringsAsFactors = FALSE
  )
  out %>% dplyr::group_by(session_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
}

# Short display for Responses by Series: one org mention; optional group if distinct; ASCII only.
series_display_label_short <- function(org_i, group_i, sis_val, sid_ctr, sid_str, singleton = FALSE) {
  org <- trimws(as.character(org_i))
  gr <- trimws(as.character(group_i))
  if (singleton || is.na(sis_val)) {
    if (nzchar(gr) && gr != org) return(paste0(org, " - ", gr, " (", sid_str, ")"))
    return(paste0(org, " (", sid_str, ")"))
  }
  base <- if (nzchar(gr) && gr != org) paste0(org, " - ", gr) else org
  paste0(base, " | ", sis_val, "-part #", sid_ctr)
}

# Multilingual collapse key.
# The Program Manager builds session_id as YYYY-MM-DD_ORG[_GROUP]_HHMM[_S{n}-{N}]_SUFFIX,
# where SUFFIX is a random 4-char token generated per row. When a workshop is offered in
# multiple languages, each language form gets its own PM row -> its own session_id that differs
# ONLY in that trailing suffix (same date, org, group, time, session-number). Language itself is
# never encoded in session_id (it lives in the Master `language` column). Stripping the trailing
# suffix yields a stable base key that treats those language variants as ONE real session.
base_session_id <- function(sid) {
  s <- trimws(as.character(sid))
  has <- !is.na(s) & nzchar(s)
  # Anchor on the first date-like token so embedded URLs still collapse correctly.
  dm <- regexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", s)
  seg <- ifelse(dm > 0, substr(s, dm, nchar(s)), s)
  parts <- strsplit(seg, "_", fixed = TRUE)
  vapply(seq_along(s), function(i) {
    if (!has[i]) return(s[i])
    p <- parts[[i]]
    if (length(p) <= 1L) return(seg[i])
    paste(p[-length(p)], collapse = "_")  # drop random suffix
  }, character(1))
}

# Reject implausible parsed dates: epoch-0 timestamps land on 1969-12-31, and malformed
# strings parse to things like 0006-02-20. Anything outside a sane workshop window -> NA.
# Uses explicit formats so unparseable values return NA instead of erroring.
.plausible_session_date <- function(x) {
  if (inherits(x, "Date")) {
    d <- x
  } else {
    xc <- trimws(as.character(x))
    xc[is.na(xc) | xc == ""] <- NA
    d <- suppressWarnings(as.Date(xc, format = "%Y-%m-%d"))
    na2 <- is.na(d) & !is.na(xc)
    if (any(na2)) d[na2] <- suppressWarnings(as.Date(xc[na2], format = "%m/%d/%Y"))
  }
  yr <- as.integer(format(d, "%Y"))
  bad <- is.na(yr) | yr < 2015L | yr > (as.integer(format(Sys.Date(), "%Y")) + 1L)
  d[bad] <- NA
  d
}

# Pull the first yyyy-mm-dd (20xx) token embedded anywhere in the session id; the date is the
# most reliable signal even when session_date / timestamp are blank or corrupted.
.embedded_session_date <- function(sid) {
  s <- as.character(sid)
  m <- regexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", s)
  out <- rep(NA_character_, length(s))
  hit <- !is.na(m) & m > 0
  out[hit] <- substr(s[hit], m[hit], m[hit] + 9L)
  suppressWarnings(as.Date(out, format = "%Y-%m-%d"))
}

# Multiple workshop series can share the same org + group. Split into series_instance using PM
# session_number + sessions_in_series (sorted by date), same rules as port scripts.
assign_series_keys <- function(session_summary) {
  n <- nrow(session_summary)
  if (n == 0) return(session_summary)
  org_clean <- ifelse(is.na(session_summary$org_name) | session_summary$org_name == "", "Unknown Organization", trimws(as.character(session_summary$org_name)))
  group_clean <- trimws(dplyr::coalesce(as.character(session_summary$group), ""))
  sid_vec <- trimws(as.character(session_summary$session_id))
  # Real session unit = base session key (language variants of one workshop collapse together).
  bid_vec <- if ("base_session_id" %in% names(session_summary)) {
    trimws(as.character(session_summary$base_session_id))
  } else {
    base_session_id(sid_vec)
  }
  sis_vec <- suppressWarnings(as.numeric(session_summary$sessions_in_series))
  sn_vec <- suppressWarnings(as.integer(session_summary$session_number))
  dvec <- if ("date" %in% names(session_summary)) as.Date(session_summary$date) else rep(as.Date(NA), n)

  series_key <- rep(NA_character_, n)
  series_instance <- rep(NA_integer_, n)
  series_label <- rep(NA_character_, n)

  # Groups: org + group + planned length; rows without PM length get a unique key per base session
  sis_chr <- ifelse(is.na(sis_vec), paste0("UNK_", bid_vec), sprintf("%.0f", sis_vec))
  ug <- paste(org_clean, group_clean, sis_chr, sep = "||")

  for (u in unique(ug)) {
    idx <- which(ug == u)
    sis_val <- sis_vec[idx[1]]
    ubids <- unique(bid_vec[idx])  # distinct real sessions in this org+group+length
    if (is.na(sis_val)) {
      # No PM length: each base session is its own single-session series.
      for (b in ubids) {
        rb <- idx[bid_vec[idx] == b]
        i1 <- rb[1]
        series_key[rb] <- paste(org_clean[i1], group_clean[i1], "session", b, sep = "||")
        series_instance[rb] <- 1L
        series_label[rb] <- series_display_label_short(org_clean[i1], group_clean[i1], NA_real_, NA_integer_, b, TRUE)
      }
      next
    }
    # Order base sessions by date / session_number, then assign series instances over them.
    rep_idx <- vapply(ubids, function(b) idx[bid_vec[idx] == b][1], integer(1))
    ord_b <- ubids[order(dvec[rep_idx], sn_vec[rep_idx], ubids, na.last = TRUE)]
    sid_ctr <- 1L
    seen_sn <- integer(0)
    for (b in ord_b) {
      rb <- idx[bid_vec[idx] == b]
      i1 <- rb[1]
      sn <- sn_vec[i1]
      if (length(seen_sn) > 0 && !is.na(sn) && sn %in% seen_sn) {
        sid_ctr <- sid_ctr + 1L
        seen_sn <- if (!is.na(sn)) sn else integer(0)
      } else if (length(seen_sn) > 0 && !is.na(sn) && sn == 1L) {
        sid_ctr <- sid_ctr + 1L
        seen_sn <- if (!is.na(sn)) sn else integer(0)
      } else {
        if (!is.na(sn)) seen_sn <- c(seen_sn, sn)
      }
      series_instance[rb] <- sid_ctr
      series_key[rb] <- paste(org_clean[i1], group_clean[i1], sis_val, sid_ctr, sep = "||")
      series_label[rb] <- series_display_label_short(org_clean[i1], group_clean[i1], sis_val, sid_ctr, b, FALSE)
    }
  }
  session_summary$series_key <- series_key
  session_summary$series_instance <- series_instance
  session_summary$series_label <- series_label
  session_summary
}

# Create session summary from Master Pre/Post; optional Program Manager for true series boundaries
# Normalize session_id (trim, character) so Pre and Post match even with format differences
create_session_summary_from_master <- function(pre_data, post_data, program_manager = NULL) {
  meta_cols <- c("session_id", "org_name", "group", "facilitators", "modules_taught", "session_date", "session_start_time", "timestamp")
  norm_sid <- function(d) {
    if (!"session_id" %in% colnames(d)) return(d)
    d$session_id <- trimws(as.character(d$session_id))
    d
  }
  pre_meta <- if (nrow(pre_data) > 0 && "session_id" %in% colnames(pre_data)) {
    pre_data %>% norm_sid() %>% select(any_of(meta_cols)) %>% group_by(session_id) %>% slice(1) %>% ungroup()
  } else data.frame(session_id = character(0))
  post_meta <- if (nrow(post_data) > 0 && "session_id" %in% colnames(post_data)) {
    post_data %>% norm_sid() %>% select(any_of(meta_cols)) %>% group_by(session_id) %>% slice(1) %>% ungroup()
  } else data.frame(session_id = character(0))
  meta <- bind_rows(pre_meta, post_meta)
  if (nrow(meta) == 0) return(data.frame())
  meta <- meta %>%
    mutate(session_id = trimws(as.character(session_id))) %>%
    group_by(session_id) %>% slice(1) %>% ungroup()

  # Ensure org_name and group exist (Overview tab and dropdowns need them)
  # Parse from session_id if missing; uses parse_session_id_org_group from global.R (handles URLs with embedded date_org_group)
  sid <- as.character(meta$session_id)
  need_org <- !"org_name" %in% colnames(meta) || all(is.na(meta$org_name) | trimws(as.character(meta$org_name)) == "", na.rm = TRUE)
  need_grp <- !"group" %in% colnames(meta) || all(is.na(meta$group) | trimws(as.character(meta$group)) == "", na.rm = TRUE)
  if (need_org || need_grp) {
    parsed <- lapply(sid, function(s) parse_session_id_org_group(s))
    if (need_org) meta$org_name <- vapply(parsed, function(p) p$org, character(1))
    if (need_grp) meta$group <- vapply(parsed, function(p) p$group, character(1))
  }

  pre_agg <- if (nrow(pre_data) > 0 && "session_id" %in% colnames(pre_data)) {
    pre_data %>% mutate(session_id = trimws(as.character(session_id))) %>%
      filter(!is.na(session_id), session_id != "") %>%
      group_by(session_id) %>% summarise(pre_responses = n(), .groups = "drop")
  } else data.frame(session_id = character(0), pre_responses = integer(0))
  post_agg <- if (nrow(post_data) > 0 && "session_id" %in% colnames(post_data)) {
    post_data %>% mutate(session_id = trimws(as.character(session_id))) %>%
      filter(!is.na(session_id), session_id != "") %>%
      group_by(session_id) %>% summarise(post_responses = n(), .groups = "drop")
  } else data.frame(session_id = character(0), post_responses = integer(0))

  session_summary <- meta %>%
    left_join(pre_agg, by = "session_id") %>%
    left_join(post_agg, by = "session_id") %>%
    mutate(
      pre_responses = replace(pre_responses, is.na(pre_responses), 0),
      post_responses = replace(post_responses, is.na(post_responses), 0),
      total_responses = pre_responses + post_responses,
      date = dplyr::coalesce(
        if ("session_date" %in% colnames(.)) .plausible_session_date(session_date) else as.Date(rep(NA, dplyr::n())),
        if ("timestamp" %in% colnames(.)) .plausible_session_date(substr(timestamp, 1, 10)) else as.Date(rep(NA, dplyr::n())),
        .embedded_session_date(session_id)
      ),
      start_time = if ("session_start_time" %in% colnames(.)) as.character(session_start_time) else NA_character_,
      end_time = NA_character_,
      response_rate_pre = ifelse(pre_responses > 0, round(100 * pre_responses / max(pre_responses, 1), 1), 0),
      response_rate_post = ifelse(post_responses > 0, round(100 * post_responses / max(post_responses, 1), 1), 0),
      time_range = ifelse(!is.na(start_time) & start_time != "", as.character(start_time), "")
    )
  if (!"date" %in% colnames(session_summary)) session_summary$date <- NA

  pmj <- prep_program_manager_session_join(program_manager)
  if (nrow(pmj) > 0) {
    session_summary <- session_summary %>%
      dplyr::mutate(session_id = trimws(as.character(.data[["session_id"]]))) %>%
      dplyr::left_join(pmj, by = "session_id") %>%
      dplyr::mutate(
        date = dplyr::coalesce(.data[["date"]], .plausible_session_date(.data[["pm_workshop_date"]])),
        session_number = dplyr::coalesce(.data[["pm_session_number"]], NA_integer_),
        sessions_in_series = dplyr::coalesce(.data[["pm_sessions_in_series"]], NA_real_)
      )
    drop_pm <- intersect(c("pm_session_number", "pm_sessions_in_series", "pm_workshop_date"), names(session_summary))
    if (length(drop_pm)) session_summary <- session_summary[, !names(session_summary) %in% drop_pm, drop = FALSE]
  } else {
    session_summary$session_number <- NA_integer_
    session_summary$sessions_in_series <- NA_real_
  }

  # Multilingual collapse: language variants of one workshop share the same base session key
  # (session_id minus its random suffix). Keep the original session_id (raw-row joins, pairing,
  # reports rely on it) but add base_session_id so session / series counts treat the language
  # variants as one real session. Also record which languages were offered per base session.
  session_summary$base_session_id <- base_session_id(session_summary$session_id)
  collect_session_langs <- function(d) {
    if (is.null(d) || nrow(d) == 0 || !"session_id" %in% colnames(d)) {
      return(data.frame(session_id = character(0), language = character(0), stringsAsFactors = FALSE))
    }
    lv <- if ("language" %in% colnames(d)) trimws(tolower(as.character(d$language))) else rep("", nrow(d))
    lv[is.na(lv) | lv == ""] <- "en"
    data.frame(session_id = trimws(as.character(d$session_id)), language = lv, stringsAsFactors = FALSE)
  }
  lang_rows <- dplyr::bind_rows(collect_session_langs(pre_data), collect_session_langs(post_data))
  lang_rows <- lang_rows[!is.na(lang_rows$session_id) & lang_rows$session_id != "", , drop = FALSE]
  if (nrow(lang_rows) > 0) {
    lang_rows$base_session_id <- base_session_id(lang_rows$session_id)
    langs_by_base <- lang_rows %>%
      dplyr::group_by(base_session_id) %>%
      dplyr::summarise(
        languages_offered = paste(sort(unique(language)), collapse = "/"),
        n_languages = dplyr::n_distinct(language),
        .groups = "drop"
      )
    session_summary <- session_summary %>% dplyr::left_join(langs_by_base, by = "base_session_id")
  }
  if (!"languages_offered" %in% names(session_summary)) session_summary$languages_offered <- "en"
  if (!"n_languages" %in% names(session_summary)) session_summary$n_languages <- 1L
  session_summary$languages_offered[is.na(session_summary$languages_offered)] <- "en"
  session_summary$n_languages[is.na(session_summary$n_languages)] <- 1L

  session_summary <- assign_series_keys(session_summary)
  session_summary <- session_summary %>%
    dplyr::mutate(
      session_label = dplyr::if_else(
        !is.na(.data[["sessions_in_series"]]) & !is.na(.data[["session_number"]]) & .data[["sessions_in_series"]] > 1,
        paste0("Session ", .data[["session_number"]], " of ", .data[["sessions_in_series"]]),
        dplyr::if_else(!is.na(.data[["series_label"]]), .data[["series_label"]], "Single Session")
      )
    )
  session_summary
}

# Create comprehensive session summary (Program Manager - deprecated for Master-only mode)
create_session_summary <- function(program_manager_data, pre_data, post_data) {
  if (nrow(program_manager_data) == 0) {
    return(data.frame())
  }
  
  # Check which columns exist
  available_cols <- colnames(program_manager_data)
  
  # Start with program manager data (session metadata)
  # Select only columns that exist
  cols_to_select <- c()
  if ("session_id" %in% available_cols) cols_to_select <- c(cols_to_select, "session_id")
  if ("org_name" %in% available_cols) cols_to_select <- c(cols_to_select, "org_name")
  if ("group" %in% available_cols) cols_to_select <- c(cols_to_select, "group")
  if ("date" %in% available_cols) cols_to_select <- c(cols_to_select, "date")
  if ("start_time" %in% available_cols) cols_to_select <- c(cols_to_select, "start_time")
  if ("end_time" %in% available_cols) cols_to_select <- c(cols_to_select, "end_time")
  if ("facilitators" %in% available_cols) cols_to_select <- c(cols_to_select, "facilitators")
  if ("modules_taught" %in% available_cols) cols_to_select <- c(cols_to_select, "modules_taught")
  if ("session_number" %in% available_cols) cols_to_select <- c(cols_to_select, "session_number")
  if ("sessions_in_series" %in% available_cols) cols_to_select <- c(cols_to_select, "sessions_in_series")
  
  if (length(cols_to_select) == 0) {
    return(data.frame())
  }
  
  session_summary <- program_manager_data %>%
    select(all_of(cols_to_select))
  
  # Add missing columns with NA
  if (!"session_id" %in% colnames(session_summary)) session_summary$session_id <- NA
  if (!"org_name" %in% colnames(session_summary)) session_summary$org_name <- NA
  if (!"group" %in% colnames(session_summary)) session_summary$group <- NA
  if (!"date" %in% colnames(session_summary)) session_summary$date <- NA
  if (!"start_time" %in% colnames(session_summary)) session_summary$start_time <- NA
  if (!"end_time" %in% colnames(session_summary)) session_summary$end_time <- NA
  if (!"facilitators" %in% colnames(session_summary)) session_summary$facilitators <- NA
  if (!"modules_taught" %in% colnames(session_summary)) session_summary$modules_taught <- NA
  if (!"session_number" %in% colnames(session_summary)) session_summary$session_number <- NA
  if (!"sessions_in_series" %in% colnames(session_summary)) session_summary$sessions_in_series <- NA
  
  # Process date
  if ("date" %in% colnames(session_summary)) {
    session_summary <- session_summary %>%
      mutate(date = tryCatch(as.Date(date), error = function(e) NA))
  }
  
  # Create session label
  session_summary <- session_summary %>%
    mutate(
      session_label = ifelse(
        !is.na(sessions_in_series) & !is.na(session_number) & sessions_in_series > 1,
        paste0("Session ", session_number, " of ", sessions_in_series),
        "Single Session"
      )
    )
  
  # Aggregate pre-survey responses
  if (nrow(pre_data) > 0 && "session_id" %in% colnames(pre_data)) {
    pre_agg <- aggregate_pre_to_sessions(pre_data, program_manager_data)
    session_summary <- session_summary %>%
      left_join(pre_agg, by = "session_id") %>%
      mutate(pre_responses = ifelse(is.na(pre_responses), 0, pre_responses))
  } else {
    session_summary$pre_responses <- 0
  }
  
  # Aggregate post-survey responses
  if (nrow(post_data) > 0 && "session_id" %in% colnames(post_data)) {
    post_agg <- aggregate_post_to_sessions(post_data, program_manager_data)
    session_summary <- session_summary %>%
      left_join(post_agg, by = "session_id") %>%
      mutate(post_responses = ifelse(is.na(post_responses), 0, post_responses))
  } else {
    session_summary$post_responses <- 0
  }
  
  # Calculate response rates (if we have expected participants, otherwise use max)
  session_summary <- session_summary %>%
    mutate(
      total_responses = pre_responses + post_responses,
      response_rate_pre = ifelse(pre_responses > 0, 
                                 round(100 * pre_responses / max(pre_responses, 1), 1), 
                                 0),
      response_rate_post = ifelse(post_responses > 0,
                                 round(100 * post_responses / max(post_responses, 1), 1),
                                 0)
    )
  
  # Format time range
  session_summary <- session_summary %>%
    mutate(
      time_range = tryCatch({
        if (!is.na(start_time) && start_time != "" && !is.na(end_time) && end_time != "") {
          start_formatted <- tryCatch(
            format(strptime(start_time, "%H:%M"), "%I:%M %p"),
            error = function(e) as.character(start_time)
          )
          end_formatted <- tryCatch(
            format(strptime(end_time, "%H:%M"), "%I:%M %p"),
            error = function(e) as.character(end_time)
          )
          paste(start_formatted, "-", end_formatted)
        } else if (!is.na(start_time) && start_time != "") {
          tryCatch(
            format(strptime(start_time, "%H:%M"), "%I:%M %p"),
            error = function(e) as.character(start_time)
          )
        } else {
          ""
        }
      }, error = function(e) "")
    )
  
  return(session_summary)
}

# ============================================================================
# Summary Statistics
# ============================================================================

calculate_session_stats <- function(session_summary) {
  if (nrow(session_summary) == 0) {
    # Return all metrics so callers can safely index
    return(list(
      total_sessions = 0,
      total_pre_responses = 0,
      total_post_responses = 0,
      avg_pre_per_session = 0,
      avg_post_per_session = 0,
      sessions_with_pre = 0,
      sessions_with_post = 0
    ))
  }
  
  # Count real sessions, collapsing multilingual variants (same base_session_id) into one.
  bid <- if ("base_session_id" %in% names(session_summary)) {
    as.character(session_summary$base_session_id)
  } else {
    as.character(session_summary$session_id)
  }
  pre <- ifelse(is.na(session_summary$pre_responses), 0, session_summary$pre_responses)
  post <- ifelse(is.na(session_summary$post_responses), 0, session_summary$post_responses)
  pre_by_base <- tapply(pre, bid, sum)
  post_by_base <- tapply(post, bid, sum)
  total_sessions <- length(unique(bid))
  total_pre <- sum(pre, na.rm = TRUE)
  total_post <- sum(post, na.rm = TRUE)

  list(
    total_sessions = total_sessions,
    total_pre_responses = total_pre,
    total_post_responses = total_post,
    avg_pre_per_session = round(total_pre / max(total_sessions, 1), 1),
    avg_post_per_session = round(total_post / max(total_sessions, 1), 1),
    sessions_with_pre = sum(pre_by_base > 0, na.rm = TRUE),
    sessions_with_post = sum(post_by_base > 0, na.rm = TRUE)
  )
}

calculate_organization_journeys <- function(session_summary) {
  if (nrow(session_summary) == 0 || !"org_name" %in% colnames(session_summary)) {
    return(data.frame())
  }

  row_index <- seq_len(nrow(session_summary))
  session_id_vec <- if ("session_id" %in% colnames(session_summary)) {
    as.character(session_summary$session_id)
  } else {
    rep(NA_character_, nrow(session_summary))
  }

  # Real session unit = base session (multilingual variants collapse). Used both for the series
  # key fallback and for the session-row count so a language-only duplicate never inflates length.
  bid_vec <- if ("base_session_id" %in% colnames(session_summary)) {
    as.character(session_summary$base_session_id)
  } else {
    session_id_vec
  }

  # A journey = one planned workshop series. Prefer series_key from PM + assign_series_keys
  # (multiple series per org+group); else legacy org||group||base session.
  sk_col <- if ("series_key" %in% names(session_summary)) as.character(session_summary$series_key) else rep(NA_character_, nrow(session_summary))
  journey_data <- session_summary %>%
    mutate(
      row_index = row_index,
      .bid = bid_vec,
      org_name_clean = ifelse(is.na(org_name) | org_name == "", "Unknown Organization", org_name),
      group_clean = coalesce(as.character(group), ""),
      journey_key = dplyr::coalesce(sk_col, paste(org_name_clean, group_clean, .bid, sep = "||"))
    ) %>%
    group_by(org_name_clean, journey_key) %>%
    summarise(
      sessions_planned = {
        sv <- sessions_in_series[!is.na(sessions_in_series) & sessions_in_series > 0]
        if (length(sv) > 0) max(sv) else NA_real_
      },
      n_sessions = dplyr::n_distinct(.bid),
      .groups = "drop"
    ) %>%
    mutate(
      sessions_in_series = dplyr::coalesce(sessions_planned, as.numeric(n_sessions)),
      sessions_in_series = ifelse(is.na(sessions_in_series) | sessions_in_series <= 0, 1, sessions_in_series),
      journey_type = ifelse(sessions_in_series == 1, "1/1", paste0("1/", sessions_in_series))
    ) %>%
    select(-sessions_planned, -n_sessions)
  length_counts <- journey_data %>%
    mutate(journey_length = pmin(sessions_in_series, 6)) %>%
    group_by(org_name_clean, journey_length) %>%
    summarise(journey_count = n(), .groups = "drop")

  # Exclude NA journey_length to avoid pivot issues
  length_counts <- length_counts %>% filter(!is.na(journey_length))

  length_summary <- length_counts %>%
    tidyr::pivot_wider(
      id_cols = org_name_clean,
      names_from = journey_length,
      names_prefix = "Len",
      values_from = journey_count,
      values_fill = 0
    )

  target_cols <- paste0("Len", 1:6)

  # Languages offered per org (collapsed over base sessions) so multilingual reach is visible.
  langs_by_org <- if ("languages_offered" %in% colnames(session_summary)) {
    data.frame(
      org_name_clean = ifelse(is.na(session_summary$org_name) | session_summary$org_name == "",
                              "Unknown Organization", as.character(session_summary$org_name)),
      lang = as.character(session_summary$languages_offered),
      stringsAsFactors = FALSE
    ) %>%
      tidyr::separate_rows(lang, sep = "/") %>%
      mutate(lang = toupper(trimws(lang))) %>%
      filter(lang != "") %>%
      group_by(org_name_clean) %>%
      summarise(Languages = paste(sort(unique(lang)), collapse = ", "), .groups = "drop")
  } else {
    data.frame(org_name_clean = character(0), Languages = character(0), stringsAsFactors = FALSE)
  }

  org_summary <- journey_data %>%
    group_by(org_name_clean) %>%
    summarise(total_journeys = n(), .groups = "drop") %>%
    left_join(length_summary, by = "org_name_clean") %>%
    left_join(langs_by_org, by = "org_name_clean")

  if (!"Languages" %in% colnames(org_summary)) org_summary$Languages <- "EN"
  org_summary$Languages[is.na(org_summary$Languages) | org_summary$Languages == ""] <- "EN"

  # Ensure all Len columns exist and have same length as org_summary
  n <- nrow(org_summary)
  for (col in target_cols) {
    if (!col %in% colnames(org_summary)) {
      org_summary[[col]] <- rep(0L, n)
    } else {
      # Replace NA with 0 and ensure correct length
      org_summary[[col]] <- as.integer(replace(org_summary[[col]], is.na(org_summary[[col]]), 0))
    }
  }

  org_summary %>%
    arrange(desc(total_journeys)) %>%
    select(Organization = org_name_clean, `Series` = total_journeys, all_of(target_cols), Languages)
}

# ============================================================================
# Journey Statistics
# ============================================================================

# Identify big pre vs little pre based on session position (uses Program Manager when available)
identify_survey_type <- function(session_data, program_manager_data) {
  if (nrow(session_data) == 0) return(session_data)
  if (nrow(program_manager_data) == 0) {
    session_data$is_big_pre <- TRUE
    session_data$is_big_post <- TRUE
    return(session_data)
  }
  
  # Join with program manager to get session_number and sessions_in_series
  if (!"session_id" %in% colnames(session_data)) {
    session_data$is_big_pre <- TRUE
    session_data$is_big_post <- TRUE
    return(session_data)
  }
  
  pm_cols <- c("session_id", "session_number", "sessions_in_series")
  pm_subset <- program_manager_data %>%
    select(any_of(pm_cols))
  if (nrow(pm_subset) > 0 && "session_id" %in% colnames(pm_subset)) {
    pm_subset$session_id <- trimws(as.character(pm_subset$session_id))
  }
  session_data <- session_data %>%
    mutate(session_id = trimws(as.character(session_id))) %>%
    left_join(pm_subset, by = "session_id")
  
  # Big Pre / Big Post only when Program Manager supplies both session_number and sessions_in_series.
  # Rows with no PM match (NA after join) were previously treated as "big", which misclassified
  # Little Pre/Post rows and broke planned-behavior / past-behavior indices.
  session_data <- session_data %>%
    mutate(
      is_big_pre = case_when(
        !is.na(sessions_in_series) & sessions_in_series == 1 ~ TRUE,
        !is.na(session_number) & !is.na(sessions_in_series) & session_number == 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      is_big_post = case_when(
        !is.na(sessions_in_series) & sessions_in_series == 1 ~ TRUE,
        !is.na(session_number) & !is.na(sessions_in_series) &
          session_number == sessions_in_series ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  return(session_data)
}

# Calculate journey statistics
calculate_journey_stats <- function(pre_data, post_data, program_manager_data) {
  # Identify survey types
  pre_typed <- identify_survey_type(pre_data, program_manager_data)
  post_typed <- identify_survey_type(post_data, program_manager_data)
  
  # Helper function to check if respondent_id is valid
  has_valid_id <- function(ids) {
    !is.na(ids) & ids != "" & !grepl("^ANON#", ids)
  }
  
  # Big Pre statistics
  big_pre <- pre_typed %>% filter(is_big_pre == TRUE)
  big_pre_with_id <- sum(has_valid_id(big_pre$respondent_id), na.rm = TRUE)
  big_pre_without_id <- nrow(big_pre) - big_pre_with_id
  
  # Big Post statistics
  big_post <- post_typed %>% filter(is_big_post == TRUE)
  big_post_with_id <- sum(has_valid_id(big_post$respondent_id), na.rm = TRUE)
  big_post_without_id <- nrow(big_post) - big_post_with_id
  
  # Paired responses (matching big pre and big post by respondent_id)
  big_pre_ids <- big_pre %>%
    filter(has_valid_id(respondent_id)) %>%
    select(respondent_id, session_id) %>%
    distinct()
  
  big_post_ids <- big_post %>%
    filter(has_valid_id(respondent_id)) %>%
    select(respondent_id, session_id) %>%
    distinct()
  
  paired_count <- big_pre_ids %>%
    inner_join(big_post_ids, by = "respondent_id", suffix = c("_pre", "_post")) %>%
    nrow()
  
  # Find intact journeys
  # An intact journey has: big pre + all intermediate surveys + big post
  all_pre <- pre_typed %>% filter(has_valid_id(respondent_id))
  all_post <- post_typed %>% filter(has_valid_id(respondent_id))
  
  # Get all sessions for each respondent
  respondent_sessions <- bind_rows(
    all_pre %>% select(respondent_id, session_id, is_big_pre, is_big_post) %>% 
      mutate(survey_type = ifelse(is_big_pre, "big_pre", "little_pre")),
    all_post %>% select(respondent_id, session_id, is_big_pre, is_big_post) %>% 
      mutate(survey_type = ifelse(is_big_post, "big_post", "little_post"))
  ) %>%
    distinct() %>%
    group_by(respondent_id) %>%
    summarise(
      has_big_pre = any(survey_type == "big_pre"),
      has_big_post = any(survey_type == "big_post"),
      total_sessions = n_distinct(session_id),
      session_ids = list(unique(session_id)),
      .groups = "drop"
    )
  
  # Join with program manager to get expected session counts
  pm_sessions <- program_manager_data %>%
    select(session_id, sessions_in_series, session_number) %>%
    filter(!is.na(sessions_in_series))
  
  # For each respondent, check if they have all expected sessions
  intact_journeys <- 0
  if (nrow(respondent_sessions) > 0 && nrow(pm_sessions) > 0) {
    # Get session series info
    session_series <- pm_sessions %>%
      group_by(sessions_in_series) %>%
      summarise(expected_sessions = n(), .groups = "drop")
    
    # Check each respondent's journey
    for (i in 1:nrow(respondent_sessions)) {
      resp <- respondent_sessions[i, ]
      if (resp$has_big_pre && resp$has_big_post) {
        # Get the sessions this respondent attended
        resp_session_ids <- unlist(resp$session_ids)
        resp_pm_sessions <- pm_sessions %>% filter(session_id %in% resp_session_ids)
        
        if (nrow(resp_pm_sessions) > 0) {
          # Check if they attended all sessions in at least one series
          series_counts <- resp_pm_sessions %>%
            group_by(sessions_in_series) %>%
            summarise(attended = n(), .groups = "drop") %>%
            left_join(session_series, by = "sessions_in_series") %>%
            filter(attended == expected_sessions)
          
          if (nrow(series_counts) > 0) {
            intact_journeys <- intact_journeys + 1
          }
        }
      }
    }
  }
  
  return(list(
    big_pre_total = nrow(big_pre),
    big_pre_with_id = big_pre_with_id,
    big_pre_without_id = big_pre_without_id,
    big_post_total = nrow(big_post),
    big_post_with_id = big_post_with_id,
    big_post_without_id = big_post_without_id,
    paired_responses = paired_count,
    intact_journeys = intact_journeys
  ))
}

# Identify comparable variables between big pre and big post
identify_comparable_variables <- function(pre_data, post_data) {
  if (exists("PRE_POST_COMPARABLE", mode = "list")) {
    return(identify_comparable_variables_feb2026(pre_data, post_data))
  }
  pre_cols <- colnames(pre_data)
  post_cols <- colnames(post_data)
  
  comparable <- list()
  pre_opt <- pre_cols[grepl("optimistic.*financial future", pre_cols, ignore.case = TRUE)]
  post_opt <- post_cols[grepl("optimistic.*financial future", post_cols, ignore.case = TRUE)]
  if (length(pre_opt) > 0 && length(post_opt) > 0) {
    comparable[["Financial Future Optimism"]] <- list(pre = pre_opt[1], post = post_opt[1])
  }
  pre_healthy <- pre_cols[grepl("healthy relationship.*money", pre_cols, ignore.case = TRUE)]
  post_healthy <- post_cols[grepl("healthy relationship.*money", post_cols, ignore.case = TRUE)]
  if (length(pre_healthy) > 0 && length(post_healthy) > 0) {
    comparable[["Healthy Relationship with Money"]] <- list(pre = pre_healthy[1], post = post_healthy[1])
  }
  pre_stress <- pre_cols[grepl("stressed.*financ|able to manage stress", pre_cols, ignore.case = TRUE)]
  post_stress <- post_cols[grepl("stressed.*financ|better able to manage stress", post_cols, ignore.case = TRUE)]
  if (length(pre_stress) > 0 && length(post_stress) > 0) {
    comparable[["Financial Stress"]] <- list(pre = pre_stress[1], post = post_stress[1])
  }
  pre_conf <- pre_cols[grepl("confident.*plan", pre_cols, ignore.case = TRUE)]
  post_conf <- post_cols[grepl("confident.*plan", post_cols, ignore.case = TRUE)]
  if (length(pre_conf) > 0 && length(post_conf) > 0) {
    comparable[["Confidence in Planning"]] <- list(pre = pre_conf[1], post = post_conf[1])
  }
  pre_comfort <- pre_cols[grepl("comfortable.*speaking.*financial professional", pre_cols, ignore.case = TRUE)]
  post_comfort <- post_cols[grepl("comfortable.*speaking.*financial professional", post_cols, ignore.case = TRUE)]
  if (length(pre_comfort) > 0 && length(post_comfort) > 0) {
    comparable[["Comfort with Financial Professionals"]] <- list(pre = pre_comfort[1], post = post_comfort[1])
  }
  return(comparable)
}

# Feb 2026: Use PRE_POST_COMPARABLE from question_mapping_feb2026.R
identify_comparable_variables_feb2026 <- function(pre_data, post_data) {
  if (!exists("PRE_POST_COMPARABLE", mode = "list")) {
    source("data/question_mapping.R", local = TRUE)
  }
  pre_cols <- colnames(pre_data)
  post_cols <- colnames(post_data)
  comparable <- list()
  for (nm in names(PRE_POST_COMPARABLE)) {
    p <- PRE_POST_COMPARABLE[[nm]]
    pre_ok <- !is.na(p$pre) && p$pre %in% pre_cols
    post_ok <- p$post %in% post_cols
    if (pre_ok && post_ok) {
      comparable[[nm]] <- list(pre = p$pre, post = p$post)
    }
  }
  comparable
}

