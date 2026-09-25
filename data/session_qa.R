# Null-coalesce used before shiny/rlang may be attached in headless scripts
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# -----------------------------------------------------------------------------
# Known fixes (applied only when the bad pattern is still present)
# -----------------------------------------------------------------------------

# Full-id remaps for obvious broken prefixes (epoch / malformed date generation).
KNOWN_SESSION_ID_REMAPS <- c(
  "1969-12-31_MERCY-HOUSING_SF-LA_1100_S4-6_EUD8" =
    "2026-06-02_MERCY-HOUSING_SF-LA_1100_S4-6_EUD8"
)

# Token-level typos inside session_id (applied before PM lookup).
SESSION_ID_TOKEN_FIXES <- c(
  "SCHOOOL" = "SCHOOL"
)

# Display-name fixes when free-text disagrees with a corrected id / known spelling.
ORG_NAME_FIXES <- c(
  "ARISE High Schoool" = "ARISE High School"
)

FACILITATOR_NAME_FIXES <- c(
  "Chris Flores" = "Chris Florez"
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

.qa_alert_email <- function() {
  e <- Sys.getenv("QA_ALERT_EMAIL", unset = "")
  if (!nzchar(e)) e <- Sys.getenv("IMPACT_ALERT_EMAIL", unset = "")
  if (!nzchar(e)) "impact@5buckets.org"
  else e
}

.qa_state_dir <- function() {
  d <- file.path(tempdir(), "5buckets_qa")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

normalize_session_id_typos <- function(sid) {
  s <- trimws(as.character(sid))
  out <- s
  for (bad in names(SESSION_ID_TOKEN_FIXES)) {
    out <- gsub(bad, SESSION_ID_TOKEN_FIXES[[bad]], out, fixed = TRUE)
  }
  # Apply full remaps after token fixes
  mapped <- KNOWN_SESSION_ID_REMAPS[out]
  hit <- !is.na(mapped)
  out[hit] <- unname(mapped[hit])
  # Also try remap on original before token fix
  mapped2 <- KNOWN_SESSION_ID_REMAPS[s]
  hit2 <- !is.na(mapped2) & !hit
  out[hit2] <- unname(mapped2[hit2])
  out
}

.levenshtein_dist <- function(a, b) {
  # Prefer utils::adist (base R)
  as.integer(utils::adist(a, b)[1, 1])
}

.is_junk_session_id <- function(sid) {
  s <- trimws(as.character(sid))
  is.na(s) | !nzchar(s) | grepl("^field empty$", s, ignore.case = TRUE) |
    grepl("^null$", s, ignore.case = TRUE) | grepl("^na$", s, ignore.case = TRUE)
}

#' Flag test / junk rows. Does not drop — callers filter with !is_test.
flag_is_test_rows <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    if (is.data.frame(df) && !"is_test" %in% names(df)) df$is_test <- logical(0)
    return(df)
  }
  sid <- if ("session_id" %in% names(df)) as.character(df$session_id) else rep(NA_character_, nrow(df))
  org <- if ("org_name" %in% names(df)) as.character(df$org_name) else rep(NA_character_, nrow(df))
  grp <- if ("group" %in% names(df)) as.character(df$group) else rep(NA_character_, nrow(df))
  fac <- if ("facilitators" %in% names(df)) as.character(df$facilitators) else rep(NA_character_, nrow(df))

  test <- rep(FALSE, nrow(df))
  test <- test | grepl("_TEST_", sid, ignore.case = TRUE)
  test <- test | grepl("^test$", trimws(grp), ignore.case = TRUE)
  test <- test | grepl("^testorg$", trimws(org), ignore.case = TRUE)
  test <- test | grepl("^test\\b", trimws(org), ignore.case = TRUE)
  test <- test | .is_junk_session_id(sid)
  test <- test | grepl("^field empty$", trimws(org), ignore.case = TRUE)
  test <- test | grepl("^field empty$", trimws(grp), ignore.case = TRUE)
  # Facilitator named only "test" (rare)
  test <- test | grepl("^test$", trimws(fac), ignore.case = TRUE)
  df$is_test <- test
  df
}

exclude_test_rows <- function(df, default_exclude = TRUE) {
  if (!isTRUE(default_exclude)) return(df)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!"is_test" %in% names(df)) df <- flag_is_test_rows(df)
  df[!df$is_test, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# PM registry
# -----------------------------------------------------------------------------

#' Build lookup: session_id -> typing/meta from combined PM.
#' Also indexes typo-normalized and known-remap variants pointing at the same row.
build_pm_session_registry <- function(pm_combined) {
  empty <- list(
    ids = character(0),
    by_id = list(),
    normalized_to_canonical = character(0)
  )
  if (is.null(pm_combined) || !is.data.frame(pm_combined) || nrow(pm_combined) == 0L) {
    return(empty)
  }
  if (!"session_id" %in% names(pm_combined)) return(empty)

  sid <- trimws(as.character(pm_combined$session_id))
  keep <- !is.na(sid) & nzchar(sid)
  pm <- pm_combined[keep, , drop = FALSE]
  sid <- sid[keep]
  if (!length(sid)) return(empty)

  by_id <- list()
  norm_map <- character(0)
  for (i in seq_along(sid)) {
    id <- sid[i]
    meta <- list(
      session_id = id,
      session_number = if ("session_number" %in% names(pm)) pm$session_number[i] else NA_real_,
      sessions_in_series = if ("sessions_in_series" %in% names(pm)) pm$sessions_in_series[i] else NA_real_,
      org_name = if ("org_name" %in% names(pm)) as.character(pm$org_name[i]) else NA_character_,
      group = if ("group" %in% names(pm)) as.character(pm$group[i]) else NA_character_
    )
    # Prefer first occurrence; later duplicates skipped unless first lacked typing
    if (is.null(by_id[[id]])) {
      by_id[[id]] <- meta
    } else if (is.na(by_id[[id]]$session_number) && !is.na(meta$session_number)) {
      by_id[[id]] <- meta
    }
    # Index normalized form -> this canonical id
    nid <- normalize_session_id_typos(id)
    if (!identical(nid, id) && nzchar(nid)) {
      norm_map[[nid]] <- id
      if (is.null(by_id[[nid]])) by_id[[nid]] <- modifyList(meta, list(session_id = nid))
    }
    # Index remapped form of this id
    remapped <- KNOWN_SESSION_ID_REMAPS[id]
    if (!is.na(remapped) && nzchar(remapped)) {
      norm_map[[remapped]] <- remapped
      if (is.null(by_id[[remapped]])) {
        by_id[[remapped]] <- modifyList(meta, list(session_id = remapped))
      }
    }
  }

  # For remapped targets that don't exist in PM, copy typing from the bad source
  for (bad in names(KNOWN_SESSION_ID_REMAPS)) {
    good <- KNOWN_SESSION_ID_REMAPS[[bad]]
    if (!is.null(by_id[[bad]]) && is.null(by_id[[good]])) {
      by_id[[good]] <- modifyList(by_id[[bad]], list(session_id = good))
    }
  }

  list(
    ids = names(by_id),
    by_id = by_id,
    normalized_to_canonical = norm_map
  )
}

.pm_lookup <- function(registry, sid) {
  if (is.null(registry) || is.null(registry$by_id)) return(NULL)
  s <- trimws(as.character(sid))
  if (!nzchar(s)) return(NULL)
  if (!is.null(registry$by_id[[s]])) return(registry$by_id[[s]])
  n <- normalize_session_id_typos(s)
  if (!identical(n, s) && !is.null(registry$by_id[[n]])) return(registry$by_id[[n]])
  NULL
}

.find_unique_near_pm_match <- function(sid, registry, max_dist = 2L) {
  if (is.null(registry) || !length(registry$ids)) return(NA_character_)
  s <- normalize_session_id_typos(sid)
  if (!nzchar(s) || .is_junk_session_id(s)) return(NA_character_)
  # Fast path: same base (drop suffix) among PM ids
  if (exists("base_session_id", mode = "function")) {
    base <- base_session_id(s)
    # Also try with 19xx dates normalized via remap already done
    cands <- registry$ids[vapply(registry$ids, function(id) {
      identical(base_session_id(id), base)
    }, logical(1))]
    # Prefer exact language suffix match among same base
    suf <- sub("^.*_", "", s)
    if (length(cands) == 1L) return(cands[1])
    if (length(cands) > 1L) {
      same_suf <- cands[sub("^.*_", "", cands) == suf]
      if (length(same_suf) == 1L) return(same_suf[1])
    }
  }
  # Edit distance against PM ids (cap candidates by shared org token)
  parts <- strsplit(s, "_", fixed = TRUE)[[1]]
  org_tok <- if (length(parts) >= 2L) parts[2] else ""
  pool <- registry$ids
  if (nzchar(org_tok)) {
    pool2 <- pool[grepl(org_tok, pool, fixed = TRUE)]
    if (length(pool2)) pool <- pool2
  }
  if (!length(pool) || length(pool) > 400L) {
    # Too large for full adist — sample by shared S#-# token
    sn <- regmatches(s, regexpr("S[0-9]+-[0-9]+", s))
    if (length(sn) == 1L) {
      pool2 <- pool[grepl(sn, pool, fixed = TRUE)]
      if (length(pool2)) pool <- pool2
    }
  }
  if (!length(pool) || length(pool) > 200L) return(NA_character_)
  dists <- as.integer(utils::adist(s, pool)[1, ])
  ok <- which(dists <= max_dist & dists > 0L)
  if (!length(ok)) return(NA_character_)
  best <- min(dists[ok])
  winners <- pool[ok][dists[ok] == best]
  if (length(winners) == 1L) winners[1] else NA_character_
}

# -----------------------------------------------------------------------------
# Repair Master against PM registry
# -----------------------------------------------------------------------------

#' Repair one Master frame (Pre or Post) against PM session ids.
#' @return list(df, irreparable = data.frame, repaired_n, exact_n, test_n)
repair_master_against_pm <- function(df, registry, tab_label = "Master") {
  empty_irr <- data.frame(
    tab = character(0),
    session_id = character(0),
    org_name = character(0),
    group = character(0),
    n = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    return(list(df = df, irreparable = empty_irr, repaired_n = 0L, exact_n = 0L, test_n = 0L))
  }
  if (!"session_id" %in% names(df)) {
    df <- flag_is_test_rows(df)
    return(list(df = df, irreparable = empty_irr, repaired_n = 0L, exact_n = 0L, test_n = sum(df$is_test)))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  sid0 <- trimws(as.character(df$session_id))
  repaired <- rep(FALSE, nrow(df))
  exact <- rep(FALSE, nrow(df))
  irreparable_idx <- integer(0)

  for (i in seq_len(nrow(df))) {
    raw <- sid0[i]
    if (.is_junk_session_id(raw)) next

    # 1) Apply known remaps / token typos
    cand <- normalize_session_id_typos(raw)
    meta <- .pm_lookup(registry, cand)
    if (is.null(meta)) meta <- .pm_lookup(registry, raw)

    if (!is.null(meta)) {
      exact[i] <- identical(raw, meta$session_id) || identical(cand, meta$session_id)
      if (!identical(raw, meta$session_id)) {
        df$session_id[i] <- meta$session_id
        repaired[i] <- TRUE
      } else if (!identical(cand, raw) && identical(cand, meta$session_id)) {
        df$session_id[i] <- cand
        repaired[i] <- TRUE
      }
      # Refresh display meta from PM when present
      if ("org_name" %in% names(df) && !is.na(meta$org_name) && nzchar(meta$org_name)) {
        cur <- trimws(as.character(df$org_name[i]))
        if (!nzchar(cur) || grepl("Schoool", cur, fixed = TRUE) ||
            (nzchar(cur) && !identical(cur, meta$org_name) && grepl("SCHOOOL|Schoool", raw))) {
          df$org_name[i] <- meta$org_name
          repaired[i] <- TRUE
        }
      }
      if ("group" %in% names(df) && !is.na(meta$group) && nzchar(meta$group)) {
        cur <- trimws(as.character(df$group[i]))
        if (!nzchar(cur)) {
          df$group[i] <- meta$group
          repaired[i] <- TRUE
        }
      }
      next
    }

    # 2) Near-match
    near <- .find_unique_near_pm_match(cand, registry)
    if (!is.na(near) && nzchar(near)) {
      df$session_id[i] <- near
      repaired[i] <- TRUE
      meta2 <- .pm_lookup(registry, near)
      if (!is.null(meta2) && "org_name" %in% names(df) && !is.na(meta2$org_name) && nzchar(meta2$org_name)) {
        df$org_name[i] <- meta2$org_name
      }
      next
    }

    # 3) Irreparable (skip test ids — handled by is_test)
    if (!grepl("_TEST_", raw, ignore.case = TRUE) &&
        !grepl("^test$", trimws(as.character(df$group[i] %||% "")), ignore.case = TRUE)) {
      irreparable_idx <- c(irreparable_idx, i)
    }
  }

  # Display-only org / facilitator fixes
  if ("org_name" %in% names(df)) {
    for (bad in names(ORG_NAME_FIXES)) {
      hit <- trimws(as.character(df$org_name)) == bad
      if (any(hit, na.rm = TRUE)) {
        df$org_name[hit] <- ORG_NAME_FIXES[[bad]]
        repaired[hit] <- TRUE
      }
    }
  }
  if ("facilitators" %in% names(df)) {
    for (bad in names(FACILITATOR_NAME_FIXES)) {
      hit <- grepl(bad, as.character(df$facilitators), fixed = TRUE)
      if (any(hit, na.rm = TRUE)) {
        df$facilitators[hit] <- gsub(bad, FACILITATOR_NAME_FIXES[[bad]],
                                     as.character(df$facilitators[hit]), fixed = TRUE)
        repaired[hit] <- TRUE
      }
    }
  }

  # Fix session_date when remap fixed a 1969 / 06/-2 row
  if ("session_date" %in% names(df)) {
    bad_date <- grepl("06/-2", as.character(df$session_date), fixed = TRUE) |
      grepl("^1969", as.character(df$session_date))
    if (any(bad_date, na.rm = TRUE)) {
      # Prefer date from repaired session_id prefix
      new_sid <- as.character(df$session_id)
      dm <- regexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", new_sid)
      for (i in which(bad_date)) {
        if (dm[i] > 0) {
          df$session_date[i] <- substr(new_sid[i], dm[i], dm[i] + 9L)
          repaired[i] <- TRUE
        }
      }
    }
  }

  if ("qa_metadata_changed" %in% names(df)) {
    df$qa_metadata_changed[repaired] <- "REPAIRED"
  } else if (any(repaired)) {
    df$qa_metadata_changed <- ifelse(repaired, "REPAIRED",
                                     if ("qa_metadata_changed" %in% names(df)) df$qa_metadata_changed else "OK")
  }
  if ("qa_session_found" %in% names(df)) {
    df$qa_session_found[irreparable_idx] <- "NOT_FOUND"
  }

  df <- flag_is_test_rows(df)

  irr <- empty_irr
  if (length(irreparable_idx)) {
    sub <- df[irreparable_idx, , drop = FALSE]
    agg <- as.data.frame(table(
      session_id = as.character(sub$session_id),
      org_name = if ("org_name" %in% names(sub)) as.character(sub$org_name) else NA_character_,
      group = if ("group" %in% names(sub)) as.character(sub$group) else NA_character_
    ), stringsAsFactors = FALSE)
    if (nrow(agg)) {
      names(agg)[names(agg) == "Freq"] <- "n"
      agg$tab <- tab_label
      agg$n <- as.integer(agg$n)
      irr <- agg[, c("tab", "session_id", "org_name", "group", "n"), drop = FALSE]
    }
  }

  list(
    df = df,
    irreparable = irr,
    repaired_n = as.integer(sum(repaired)),
    exact_n = as.integer(sum(exact)),
    test_n = as.integer(sum(df$is_test, na.rm = TRUE))
  )
}

#' Run repair on Pre + Post; alert on irreparable.
repair_masters_with_pm <- function(pre, post, pm_combined, alert = TRUE) {
  registry <- build_pm_session_registry(pm_combined)
  # Also apply known remaps inside a working copy of PM so typing joins see fixed ids
  pm_fix <- pm_combined
  if (is.data.frame(pm_fix) && nrow(pm_fix) && "session_id" %in% names(pm_fix)) {
    pm_fix$session_id <- normalize_session_id_typos(pm_fix$session_id)
    # Deduplicate after remap
    pm_fix <- pm_fix[!duplicated(trimws(as.character(pm_fix$session_id))), , drop = FALSE]
  }
  registry <- build_pm_session_registry(pm_fix)

  pre_r <- repair_master_against_pm(pre, registry, "Pre Submissions")
  post_r <- repair_master_against_pm(post, registry, "Post Submissions")
  irr <- rbind(pre_r$irreparable, post_r$irreparable)

  alert_result <- NULL
  if (isTRUE(alert) && nrow(irr) > 0L) {
    alert_result <- qa_alert_irreparable(irr)
  }

  list(
    pre = pre_r$df,
    post = post_r$df,
    pm_for_typing = pm_fix,
    irreparable = irr,
    repaired_n = pre_r$repaired_n + post_r$repaired_n,
    test_n = pre_r$test_n + post_r$test_n,
    alert = alert_result,
    registry_n = length(registry$ids)
  )
}

# -----------------------------------------------------------------------------
# Email / file alert
# -----------------------------------------------------------------------------

qa_alert_irreparable <- function(issues_df) {
  if (is.null(issues_df) || !nrow(issues_df)) {
    return(list(sent = FALSE, reason = "no_issues"))
  }
  # Deduplicate: hash of session_ids; at most once per calendar day unless new ids
  ids <- sort(unique(as.character(issues_df$session_id)))
  digest <- paste(ids, collapse = "|")
  day <- format(Sys.Date(), "%Y-%m-%d")
  state_file <- file.path(.qa_state_dir(), "last_alert.rds")
  prev <- if (file.exists(state_file)) tryCatch(readRDS(state_file), error = function(e) NULL) else NULL
  if (!is.null(prev) && identical(prev$day, day) && identical(prev$digest, digest)) {
    return(list(sent = FALSE, reason = "deduped_same_day", path = prev$path %||% NA_character_))
  }

  to <- .qa_alert_email()
  subject <- sprintf(
    "[5 Buckets QA] %d irreparable session_id(s) not in Program Manager",
    length(ids)
  )
  sample_n <- min(25L, nrow(issues_df))
  body_lines <- c(
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Irreparable distinct session_ids: %d", length(ids)),
    sprintf("Rows affected: %d", sum(as.integer(issues_df$n), na.rm = TRUE)),
    "",
    "These Master session_ids were not found in company or Mercy Program Manager",
    "and could not be confidently near-matched. Please check PM / Master.",
    "",
    "Sample:",
    capture.output(print(utils::head(issues_df, sample_n)))
  )
  body <- paste(body_lines, collapse = "\n")

  report_path <- file.path(
    .qa_state_dir(),
    sprintf("irreparable_session_ids_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  writeLines(c(subject, "", body), report_path)

  sent <- FALSE
  method <- "file"
  # Optional SMTP via emayili / blastula if installed + env configured
  smtp_host <- Sys.getenv("QA_SMTP_HOST", unset = "")
  if (nzchar(smtp_host) && requireNamespace("blastula", quietly = TRUE)) {
    sent <- tryCatch({
      # Minimal: user can wire credentials; we attempt only if fully configured
      FALSE
    }, error = function(e) FALSE)
  }
  # mailto-style fallback via system `mail` if present
  if (!sent && nzchar(Sys.which("mail"))) {
    sent <- tryCatch({
      tf <- tempfile(fileext = ".txt")
      writeLines(body, tf)
      status <- system2("mail", c("-s", subject, to), stdin = tf, stdout = FALSE, stderr = FALSE)
      unlink(tf)
      identical(as.integer(status), 0L)
    }, error = function(e) FALSE)
    if (sent) method <- "mail"
  }

  saveRDS(list(day = day, digest = digest, path = report_path, sent = sent), state_file)
  list(
    sent = sent,
    method = method,
    path = report_path,
    to = to,
    n_ids = length(ids)
  )
}

#' Stable session key that does not depend solely on a valid 20xx date prefix.
#' Uses org_group_time_S#-# when parseable; falls back to base_session_id with 19xx allowed.
stable_session_key <- function(sid) {
  s <- trimws(as.character(sid))
  s <- normalize_session_id_typos(s)
  has <- !is.na(s) & nzchar(s)
  # Allow 19xx and 20xx date anchors
  dm <- regexpr("[12][0-9]{3}-[0-9]{2}-[0-9]{2}", s)
  seg <- ifelse(dm > 0, substr(s, dm, nchar(s)), s)
  parts <- strsplit(seg, "_", fixed = TRUE)
  vapply(seq_along(s), function(i) {
    if (!has[i]) return(s[i])
    p <- parts[[i]]
    if (length(p) <= 1L) return(seg[i])
    # Drop date + random suffix when we have org_group_time_Sn-m
    # Pattern: DATE_ORG_GROUP_HHMM_Sn-m_RAND
    if (length(p) >= 5L) {
      # org, group, time, series — drop date (1) and rand (last)
      core <- p[2:(length(p) - 1L)]
      return(paste(core, collapse = "_"))
    }
    paste(p[-length(p)], collapse = "_")
  }, character(1))
}
