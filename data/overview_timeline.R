# overview_timeline.R — Master weekly activity + Program Manager series timeline (Overview tab)

if (!exists("MASTER_TIMESTAMP_TZ")) {
  MASTER_TIMESTAMP_TZ <- Sys.getenv("MASTER_TIMESTAMP_TZ", unset = "America/Los_Angeles")
}

#' Flatten list-cols from googlesheets4 (same idea as global.R normalize_master_cols)
overview_flatten_cell <- function(x) {
  if (!is.list(x)) return(x)
  vapply(x, function(z) {
    if (is.null(z) || length(z) == 0) return(NA_character_)
    if (inherits(z, "POSIXct")) return(format(z, "%Y-%m-%d %H:%M:%OS"))
    if (inherits(z, "Date")) return(format(z, "%Y-%m-%d"))
    if (is.list(z) && length(z) > 0) z <- z[[1]]
    trimws(as.character(z))
  }, character(1))
}

#' Parse Master timestamp column — delegates to inclusive parser (global.R / master_timestamp_parse.R).
overview_parse_ts <- function(x) {
  parse_master_timestamp_inclusive(x)
}

overview_ts_col <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NA_character_)
  nms <- names(d)
  if ("timestamp_parsed" %in% nms && any(!is.na(d$timestamp_parsed))) return("timestamp_parsed")
  for (nm in c("timestamp", "Timestamp")) {
    if (nm %in% nms) return(nm)
  }
  hit <- grep("^timestamp$", nms, ignore.case = TRUE, value = TRUE)
  if (length(hit)) return(hit[1])
  NA_character_
}

#' Calendar date in Master TZ (Pacific), then bin to day / week (Mon) / month
overview_bin_period <- function(ts_posix, time_unit) {
  time_unit <- match.arg(time_unit, c("days", "weeks", "months"))
  d <- if (inherits(ts_posix, "POSIXct")) {
    as.Date(ts_posix, tz = MASTER_TIMESTAMP_TZ)
  } else {
    as.Date(ts_posix)
  }
  switch(
    time_unit,
    days = d,
    weeks = as.Date(lubridate::floor_date(d, "week", week_start = 1)),
    months = as.Date(lubridate::floor_date(d, "month")),
    d
  )
}

#' Master Pre/Post: counts per time bin + full period range (zeros where no rows). Master only; no sidebar filters.
build_master_submission_timeline <- function(pre, post, time_unit) {
  time_unit <- match.arg(time_unit, c("days", "weeks", "months"))
  empty <- data.frame(
    period = as.Date(character()),
    pre_n = integer(),
    post_n = integer(),
    workshops_n = integer(),
    stringsAsFactors = FALSE
  )
  if ((is.null(pre) || nrow(pre) == 0) && (is.null(post) || nrow(post) == 0)) {
    return(list(df = empty, time_unit = time_unit, x_breaks = as.Date(character()), x_labels = character()))
  }

  ts_col_pre <- if (!is.null(pre) && nrow(pre) > 0) overview_ts_col(pre) else NA_character_
  ts_col_post <- if (!is.null(post) && nrow(post) > 0) overview_ts_col(post) else NA_character_
  ts_pre <- if (!is.na(ts_col_pre)) overview_parse_ts(pre[[ts_col_pre]]) else NULL
  ts_post <- if (!is.na(ts_col_post)) overview_parse_ts(post[[ts_col_post]]) else NULL

  all_dates <- c(
    if (!is.null(ts_pre)) as.Date(ts_pre[!is.na(ts_pre)], tz = MASTER_TIMESTAMP_TZ),
    if (!is.null(ts_post)) as.Date(ts_post[!is.na(ts_post)], tz = MASTER_TIMESTAMP_TZ)
  )
  all_dates <- all_dates[!is.na(all_dates)]
  if (!length(all_dates)) {
    return(list(df = empty, time_unit = time_unit, x_breaks = as.Date(character()), x_labels = character()))
  }

  full_periods <- overview_full_period_seq(all_dates, time_unit)

  wpre <- if (!is.null(ts_pre) && any(!is.na(ts_pre))) {
    bp <- overview_bin_period(ts_pre, time_unit)
    ok <- !is.na(ts_pre) & !is.na(bp)
    if (any(ok)) {
      data.frame(period = bp[ok], stringsAsFactors = FALSE) %>%
        dplyr::count(period, name = "pre_n")
    } else {
      data.frame(period = as.Date(character()), pre_n = integer(), stringsAsFactors = FALSE)
    }
  } else {
    data.frame(period = as.Date(character()), pre_n = integer(), stringsAsFactors = FALSE)
  }

  wpost <- if (!is.null(ts_post) && any(!is.na(ts_post))) {
    bp <- overview_bin_period(ts_post, time_unit)
    ok <- !is.na(ts_post) & !is.na(bp)
    if (any(ok)) {
      data.frame(period = bp[ok], stringsAsFactors = FALSE) %>%
        dplyr::count(period, name = "post_n")
    } else {
      data.frame(period = as.Date(character()), post_n = integer(), stringsAsFactors = FALSE)
    }
  } else {
    data.frame(period = as.Date(character()), post_n = integer(), stringsAsFactors = FALSE)
  }

  sid_rows <- list()
  if (!is.null(pre) && nrow(pre) > 0 && !is.na(ts_col_pre) && "session_id" %in% names(pre)) {
    ts <- overview_parse_ts(pre[[ts_col_pre]])
    sid <- trimws(as.character(pre$session_id))
    ok <- !is.na(ts) & nzchar(sid)
    if (any(ok)) {
      bp <- overview_bin_period(ts, time_unit)
      ok2 <- ok & !is.na(bp)
      if (any(ok2)) {
        sid_rows[[length(sid_rows) + 1L]] <- data.frame(
          period = bp[ok2],
          session_id = sid[ok2],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!is.null(post) && nrow(post) > 0 && !is.na(ts_col_post) && "session_id" %in% names(post)) {
    ts <- overview_parse_ts(post[[ts_col_post]])
    sid <- trimws(as.character(post$session_id))
    ok <- !is.na(ts) & nzchar(sid)
    if (any(ok)) {
      bp <- overview_bin_period(ts, time_unit)
      ok2 <- ok & !is.na(bp)
      if (any(ok2)) {
        sid_rows[[length(sid_rows) + 1L]] <- data.frame(
          period = bp[ok2],
          session_id = sid[ok2],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  wshop <- if (length(sid_rows)) {
    dplyr::bind_rows(sid_rows) %>%
      dplyr::distinct(period, session_id) %>%
      dplyr::count(period, name = "workshops_n")
  } else {
    data.frame(period = as.Date(character()), workshops_n = integer(), stringsAsFactors = FALSE)
  }

  grid <- data.frame(period = full_periods, stringsAsFactors = FALSE)
  out <- grid %>%
    dplyr::left_join(wpre, by = "period") %>%
    dplyr::left_join(wpost, by = "period") %>%
    dplyr::left_join(wshop, by = "period")
  out$pre_n[is.na(out$pre_n)] <- 0L
  out$post_n[is.na(out$post_n)] <- 0L
  out$workshops_n[is.na(out$workshops_n)] <- 0L
  out$pre_n <- as.integer(out$pre_n)
  out$post_n <- as.integer(out$post_n)
  out$workshops_n <- as.integer(out$workshops_n)

  lab_full <- overview_format_axis_labels(full_periods, time_unit)
  thin <- overview_thin_axis(full_periods, lab_full, max_labels = 40L)

  list(
    df = out,
    time_unit = time_unit,
    x_breaks = thin$breaks,
    x_labels = thin$labels,
    period_label = switch(time_unit, days = "Day", weeks = "Week (Mon start)", months = "Month")
  )
}

build_master_weekly_activity <- function(pre, post) {
  build_master_submission_timeline(pre, post, "weeks")$df
}

overview_pm_find_col <- function(d, candidates) {
  if (is.null(d) || nrow(d) == 0) return(NA_character_)
  nms <- names(d)
  nl <- tolower(trimws(nms))
  for (cand in candidates) {
    hit <- which(nl == tolower(trimws(cand)))[1]
    if (!is.na(hit)) return(nms[hit])
  }
  NA_character_
}

#' Replace Unicode dashes with ASCII so ggplot/plotly don't show mojibake (e.g. <e2><80><94>)
overview_sanitize_pm_label <- function(x) {
  x <- as.character(x)
  x <- gsub("\u2014", "-", x, fixed = TRUE)
  x <- gsub("\u2013", "-", x, fixed = TRUE)
  trimws(x)
}

#' Full period sequence from min to max (inclusive) for axis + complete grid
overview_full_period_seq <- function(d_ok, time_unit) {
  switch(
    time_unit,
    days = seq.Date(min(d_ok), max(d_ok), by = "day"),
    weeks = {
      pmin <- lubridate::floor_date(min(d_ok), "week", week_start = 1)
      pmax <- lubridate::floor_date(max(d_ok), "week", week_start = 1)
      seq(pmin, pmax, by = "week")
    },
    months = {
      pmin <- lubridate::floor_date(min(d_ok), "month")
      pmax <- lubridate::floor_date(max(d_ok), "month")
      seq(pmin, pmax, by = "month")
    },
    seq.Date(min(d_ok), max(d_ok), by = "day")
  )
}

#' Human-readable x-axis labels per time unit (months: "Mar" same year; else "Mar 2025")
overview_format_axis_labels <- function(dates, time_unit) {
  if (length(dates) == 0) return(character(0))
  dates <- as.Date(dates)
  switch(
    time_unit,
    days = {
      yrs <- format(dates, "%Y")
      if (length(unique(yrs)) <= 1L) {
        sprintf("%s %d", format(dates, "%b"), as.integer(format(dates, "%d")))
      } else {
        sprintf("%s %d %s", format(dates, "%b"), as.integer(format(dates, "%d")), yrs)
      }
    },
    weeks = {
      sprintf("%s %d", format(dates, "%b"), as.integer(format(dates, "%d")))
    },
    months = {
      yrs <- format(dates, "%Y")
      if (length(unique(yrs)) <= 1L) {
        format(dates, "%b")
      } else {
        format(dates, "%b %Y")
      }
    },
    format(dates, "%Y-%m-%d")
  )
}

#' When many day ticks, thin labels but keep full data columns
overview_thin_axis <- function(breaks, labels, max_labels = 40L) {
  n <- length(breaks)
  if (n <= max_labels) {
    return(list(breaks = breaks, labels = labels))
  }
  step <- max(1L, ceiling(n / max_labels))
  idx <- seq(1L, n, by = step)
  list(breaks = breaks[idx], labels = labels[idx])
}

overview_pm_parse_date <- function(x) {
  if (inherits(x, "list")) x <- overview_flatten_cell(x)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x, tz = MASTER_TIMESTAMP_TZ))
  if (is.numeric(x)) {
    ok <- !is.na(x) & x > 20000 & x < 60000
    out <- rep(as.Date(NA), length(x))
    if (any(ok)) {
      origin <- as.Date("1899-12-30")
      out[ok] <- origin + as.integer(x[ok])
    }
    return(out)
  }
  xc <- trimws(as.character(x))
  xc[xc == "" | toupper(xc) == "NA"] <- NA_character_
  parsed <- lubridate::parse_date_time(
    xc,
    orders = c("mdy", "ymd", "dmy", "Ymd", "mdy HMS", "ymd HMS"),
    tz = MASTER_TIMESTAMP_TZ,
    quiet = TRUE,
    truncated = 3
  )
  as.Date(parsed)
}

#' Bin workshop dates for Gantt-style heatmap (complete grid, ASCII series labels)
build_program_manager_timeline <- function(pm, time_unit, sort_by) {
  if (is.null(pm) || nrow(pm) == 0) {
    return(list(df = NULL, msg = "No Program Manager rows loaded."))
  }
  org_c <- overview_pm_find_col(pm, c("org_name", "Organization", "organization"))
  grp_c <- overview_pm_find_col(pm, c("group", "Group"))
  date_c <- overview_pm_find_col(pm, c("date", "Date"))
  sid_c <- overview_pm_find_col(pm, c("session_id", "Session ID"))

  if (is.na(date_c)) {
    return(list(df = NULL, msg = "Program Manager sheet needs a date column (date)."))
  }

  org <- if (!is.na(org_c)) overview_sanitize_pm_label(pm[[org_c]]) else rep("", nrow(pm))
  grp <- if (!is.na(grp_c)) overview_sanitize_pm_label(pm[[grp_c]]) else rep("", nrow(pm))
  org <- trimws(as.character(org))
  grp <- trimws(as.character(grp))
  series <- ifelse(nzchar(grp), paste(org, "-", grp), org)
  series <- trimws(gsub("\\s*-\\s*", " - ", series))

  d <- overview_pm_parse_date(pm[[date_c]])
  ok <- !is.na(d)
  if (!any(ok)) {
    return(list(df = NULL, msg = "Could not parse workshop dates in Program Manager."))
  }

  d_ok <- d[ok]
  series_ok <- series[ok]
  sid <- if (!is.na(sid_c)) trimws(as.character(pm[[sid_c]])) else as.character(seq_len(nrow(pm)))
  sid_ok <- sid[ok]

  period <- switch(
    time_unit,
    days = d_ok,
    weeks = as.Date(lubridate::floor_date(d_ok, "week", week_start = 1)),
    months = as.Date(lubridate::floor_date(d_ok, "month")),
    d_ok
  )

  plot_df <- data.frame(series = series_ok, period = period, sid = sid_ok, stringsAsFactors = FALSE) %>%
    dplyr::mutate(series = ifelse(nzchar(series), series, "(no org)")) %>%
    dplyr::group_by(series, period) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  summ <- data.frame(series = series_ok, d = d_ok, stringsAsFactors = FALSE) %>%
    dplyr::mutate(series = ifelse(nzchar(as.character(series)), series, "(no org)")) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(first = min(d), last = max(d), .groups = "drop")

  # Org key = text before first " - " (series label is org " - " group)
  summ$org_key <- vapply(as.character(summ$series), function(s) {
    p <- strsplit(s, " - ", fixed = TRUE)[[1]]
    if (length(p) >= 2) trimws(p[1]) else trimws(s)
  }, character(1))

  ord <- switch(
    sort_by,
    first_asc = summ$series[order(summ$first, summ$series)],
    first_desc = summ$series[order(-as.numeric(summ$first), summ$series)],
    last_asc = summ$series[order(summ$last, summ$series)],
    last_desc = summ$series[order(-as.numeric(summ$last), summ$series)],
    alpha_chrono = summ$series[order(summ$org_key, summ$first, summ$series)],
    summ$series[order(summ$first, summ$series)]
  )

  full_periods <- overview_full_period_seq(d_ok, time_unit)
  grid <- expand.grid(series = ord, period = full_periods, stringsAsFactors = FALSE)
  plot_df <- merge(grid, plot_df, by = c("series", "period"), all.x = TRUE)
  plot_df$n[is.na(plot_df$n)] <- 0L
  plot_df$n <- as.integer(plot_df$n)

  plot_df$series <- factor(plot_df$series, levels = rev(ord))

  lab_full <- overview_format_axis_labels(full_periods, time_unit)
  thin <- overview_thin_axis(full_periods, lab_full, max_labels = 40L)

  weekend_vlines <- NULL
  weekend_rect <- NULL
  # Sat/Sun boundary lines: line at each Sunday (between Saturday and Sunday bins)
  sat_sun_boundary_vlines <- NULL
  month_boundary_vlines <- NULL
  if (identical(time_unit, "days") && length(full_periods) > 0L) {
    w <- lubridate::wday(full_periods, week_start = 1L)
    sat <- full_periods[w == 6L]
    sun <- full_periods[w == 7L]
    if (length(sat)) {
      weekend_rect <- data.frame(
        xmin = sat,
        xmax = sat + 2L,
        stringsAsFactors = FALSE
      )
    }
    if (length(sat) || length(sun)) {
      weekend_vlines <- data.frame(
        x = as.Date(c(sat, sun)),
        stringsAsFactors = FALSE
      )
    }
    sun_only <- full_periods[w == 7L]
    if (length(sun_only)) {
      sat_sun_boundary_vlines <- data.frame(x = sun_only, stringsAsFactors = FALSE)
    }
  }
  if (identical(time_unit, "weeks") && length(full_periods) > 1L) {
    fp <- sort(full_periods)
    ch <- which(format(fp[-1], "%Y-%m") != format(head(fp, -1), "%Y-%m"))
    if (length(ch)) {
      month_boundary_vlines <- data.frame(x = fp[ch + 1L], stringsAsFactors = FALSE)
    }
  }

  list(
    df = plot_df,
    msg = NULL,
    period_label = switch(time_unit, days = "Day", weeks = "Week (Mon start)", months = "Month"),
    time_unit = time_unit,
    x_breaks = thin$breaks,
    x_labels = thin$labels,
    x_limits = range(full_periods),
    weekend_rect = weekend_rect,
    weekend_vlines = weekend_vlines,
    sat_sun_boundary_vlines = sat_sun_boundary_vlines,
    month_boundary_vlines = month_boundary_vlines
  )
}

#' Clip Program Manager Gantt result to a visible date window (sidebar zoom).
overview_clip_pm_timeline_window <- function(res, range_start, range_end) {
  if (is.null(res) || is.null(res$df) || nrow(res$df) == 0) return(res)
  rs <- as.Date(range_start)
  re <- as.Date(range_end)
  if (any(is.na(c(rs, re)))) return(res)
  if (rs > re) {
    tmp <- rs
    rs <- re
    re <- tmp
  }
  pdat <- res$df
  pdat <- pdat[pdat$period >= rs & pdat$period <= re, , drop = FALSE]
  if (nrow(pdat) == 0) {
    res$df <- pdat
    res$x_limits <- c(rs, re)
    res$x_breaks <- rs
    res$x_labels <- overview_format_axis_labels(rs, res$time_unit %||% "weeks")
    return(res)
  }
  visible_periods <- sort(unique(pdat$period))
  lab_full <- overview_format_axis_labels(visible_periods, res$time_unit)
  thin <- overview_thin_axis(visible_periods, lab_full, max_labels = 40L)
  clip_vline <- function(vdf) {
    if (is.null(vdf) || nrow(vdf) == 0) return(vdf)
    vdf[vdf$x >= rs & vdf$x <= re, , drop = FALSE]
  }
  clip_rect <- function(rdf) {
    if (is.null(rdf) || nrow(rdf) == 0) return(rdf)
    rdf[rdf$xmax >= rs & rdf$xmin <= re, , drop = FALSE]
  }
  res$df <- pdat
  res$x_limits <- c(rs, re)
  res$x_breaks <- thin$breaks
  res$x_labels <- thin$labels
  res$weekend_vlines <- clip_vline(res$weekend_vlines)
  res$sat_sun_boundary_vlines <- clip_vline(res$sat_sun_boundary_vlines)
  res$month_boundary_vlines <- clip_vline(res$month_boundary_vlines)
  res$weekend_rect <- clip_rect(res$weekend_rect)
  res
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Plotly layout shapes: vertical lines at dates (paper y, full height)
overview_plotly_vline_shapes <- function(dates, linecolor = "rgba(80,80,80,0.45)", linewidth = 1) {
  if (is.null(dates) || !length(dates)) return(list())
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return(list())
  lapply(dates, function(xd) {
    xs <- as.character(xd)
    list(
      type = "line",
      xref = "x",
      x0 = xs, x1 = xs,
      y0 = 0, y1 = 1,
      yref = "paper",
      line = list(color = linecolor, width = linewidth),
      layer = "below"
    )
  })
}

#' Master submission chart: Sat/Sun boundary (days) or month boundary (weeks) vertical lines
overview_master_submission_vline_dates <- function(full_periods, time_unit) {
  time_unit <- match.arg(time_unit, c("days", "weeks", "months"))
  if (!length(full_periods)) return(as.Date(character()))
  fp <- sort(as.Date(full_periods))
  if (identical(time_unit, "days")) {
    w <- lubridate::wday(fp, week_start = 1L)
    return(fp[w == 7L])
  }
  if (identical(time_unit, "weeks") && length(fp) > 1L) {
    ch <- which(format(fp[-1], "%Y-%m") != format(head(fp, -1), "%Y-%m"))
    if (length(ch)) return(fp[ch + 1L])
  }
  as.Date(character())
}
