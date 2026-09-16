# server/annual_survey.R - Annual Survey Impact tab outputs

# ASCII-only copy (avoids mojibake when the R session locale cannot encode Unicode arrows/ellipsis).
.annual_pointer_box <- function(title, body_html, link_id, link_label) {
  shiny::div(
    style = "border-left: 4px solid #5c2f92; background: #f7f4fb; padding: 10px 14px; margin: 8px 0 14px 0; border-radius: 0 6px 6px 0;",
    shiny::tags$p(style = "margin: 0 0 4px 0; font-weight: 600; color: #5c2f92;", title),
    shiny::HTML(body_html),
    shiny::tags$p(
      style = "margin: 8px 0 0 0; font-size: 13px; color: #5f6369;",
      "Go to ",
      shiny::actionLink(link_id, link_label, style = "font-weight: 600;"),
      " and open the Annual Survey section there. ",
      "Pre/Post charts on those tabs stay workshop-only; annual answers are shown in their own collapsible."
    )
  )
}

annual_survey_tab_ui <- function() {
  shiny::tagList(
    shiny::h3("Annual Survey 2026"),
    shiny::p(
      style = "color: #5f6369; max-width: 820px;",
      "Form-order view of the Annual Learner Survey. ",
      "Questions that exist only on the annual form are analyzed ", shiny::tags$strong("here"), ". ",
      "Topics shared with workshop Pre/Post surveys are analyzed on other Impact tabs ",
      "(links below). Following the same person from Pre to Post to Annual uses salted respondent IDs elsewhere."
    ),
    shiny::uiOutput("annual_survey_summary_banner"),
    shiny::tags$details(
      open = TRUE,
      shiny::tags$summary(shiny::h5("Question map (Here vs Link)", style = "color: #5c2f92; cursor: pointer;")),
      shiny::p(
        style = "font-size: 12px; color: #5f6369; max-width: 920px;",
        shiny::tags$strong("Here"),
        " = charts/tables on this tab. ",
        shiny::tags$strong("Link"),
        " = open the matching Impact tab (Financial Wellness, Behavioral, Satisfaction, Learning, or Reach)."
      ),
      shiny::div(style = "max-width: 960px;", DT::dataTableOutput("annual_survey_question_map_table"))
    ),

    shiny::h4("Analyzed here", style = "color: #5c2f92; margin-top: 22px; border-bottom: 2px solid #e8e0f0; padding-bottom: 6px;"),
    shiny::p(
      style = "font-size: 13px; color: #5f6369; max-width: 820px; margin-bottom: 10px;",
      "Annual-only attendance context, money-change bands, and residual open-text / recommend quick looks."
    ),
    shiny::tags$details(
      open = TRUE,
      shiny::tags$summary(shiny::h5("1. Workshop attendance context", style = "color: #5c2f92; cursor: pointer;")),
      shiny::p(style = "font-size: 13px; color: #5f6369;", "Annual-only: when/how people attended, host org, and topics (not on Big Post)."),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("annual_first_attend_plot", height = "280px")),
        shiny::column(6, plotly::plotlyOutput("annual_last_attend_plot", height = "280px"))
      ),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("annual_how_attend_plot", height = "260px")),
        shiny::column(6, plotly::plotlyOutput("annual_topics_plot", height = "280px"))
      ),
      shiny::h6("Host organizations", style = "color: #5c2f92; margin-top: 12px;"),
      shiny::div(style = "max-width: 720px;", DT::dataTableOutput("annual_host_org_table"))
    ),
    shiny::tags$details(
      open = TRUE,
      shiny::tags$summary(shiny::h5("2. Money outcomes since first workshop", style = "color: #5c2f92; cursor: pointer;")),
      shiny::p(
        style = "font-size: 13px; color: #5f6369;",
        "Annual-only self-reported change bands for income, savings, debt, investments, and credit score."
      ),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("annual_income_change_plot", height = "300px")),
        shiny::column(6, plotly::plotlyOutput("annual_savings_change_plot", height = "300px"))
      ),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("annual_debt_change_plot", height = "300px")),
        shiny::column(6, plotly::plotlyOutput("annual_investments_change_plot", height = "300px"))
      ),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("annual_credit_change_plot", height = "300px"))
      )
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("3. Recommend (quick look)", style = "color: #5c2f92; cursor: pointer;")),
      shiny::p(
        style = "font-size: 12px; color: #5f6369; margin-top: 8px;",
        "Full analysis lives on the Satisfaction tab. This is a small Annual-only chart. ",
        shiny::tags$strong("Click a bar"), " to list respondents for that score ",
        "(email / name; Annual Survey has no session_id yet)."
      ),
      plotly::plotlyOutput("annual_recommend_plot", height = "260px"),
      shiny::uiOutput("annual_recommend_drill_heading"),
      DT::dataTableOutput("annual_recommend_drill_table")
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("4. Open text (tables)", style = "color: #5c2f92; cursor: pointer;")),
      shiny::p(
        style = "font-size: 12px; color: #5f6369;",
        "Wordclouds and heavier Learning views live on Learning & Impact Stories. Tables of Annual open responses are here."
      ),
      shiny::tags$details(
        open = FALSE,
        shiny::tags$summary("Takeaways"),
        DT::dataTableOutput("annual_takeaway_table")
      ),
      shiny::tags$details(
        open = FALSE,
        shiny::tags$summary("Proud decisions / goals"),
        DT::dataTableOutput("annual_proud_table")
      ),
      shiny::tags$details(
        open = FALSE,
        shiny::tags$summary("Impact stories"),
        DT::dataTableOutput("annual_story_table")
      )
    ),

    shiny::h4("Analyzed elsewhere", style = "color: #5c2f92; margin-top: 28px; border-bottom: 2px solid #e8e0f0; padding-bottom: 6px;"),
    shiny::p(
      style = "font-size: 13px; color: #5f6369; max-width: 820px; margin-bottom: 10px;",
      "Shared topics with workshop Pre/Post — open the linked Impact tab and use its Annual Survey section."
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("Financial wellness (Compared to before...)", style = "color: #5c2f92; cursor: pointer;")),
      shiny::uiOutput("annual_pointer_wellness")
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("Behavioral readiness", style = "color: #5c2f92; cursor: pointer;")),
      shiny::uiOutput("annual_pointer_behavioral")
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("Satisfaction / recommend", style = "color: #5c2f92; cursor: pointer;")),
      shiny::uiOutput("annual_pointer_satisfaction")
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("Learning & impact stories", style = "color: #5c2f92; cursor: pointer;")),
      shiny::uiOutput("annual_pointer_learning")
    ),
    shiny::tags$details(
      open = FALSE,
      shiny::tags$summary(shiny::h5("Reach / demographics", style = "color: #5c2f92; cursor: pointer;")),
      shiny::uiOutput("annual_pointer_reach"),
      shiny::p(style = "font-size: 12px; color: #5f6369;", "Coverage in current Annual filter (missingness only):"),
      shiny::uiOutput("annual_demo_coverage")
    ),
    shiny::div(style = "min-height: 120px; padding-bottom: 60px;")
  )
}

register_annual_survey_outputs <- function(input, output, session, filtered_annual) {
  .bar_flags <- function() {
    list(
      n = tryCatch(isTRUE(input$opt_show_n), error = function(e) FALSE),
      pct = tryCatch(isTRUE(input$opt_show_pct), error = function(e) FALSE)
    )
  }

  .goto_impact_tab <- function(tab_value) {
    tryCatch(shiny::updateTabsetPanel(session, "impact_tabs", selected = tab_value), silent = TRUE)
  }
  shiny::observeEvent(input$annual_goto_wellness, { .goto_impact_tab("wellness") }, ignoreInit = TRUE)
  shiny::observeEvent(input$annual_goto_behavioral, { .goto_impact_tab("behavioral") }, ignoreInit = TRUE)
  shiny::observeEvent(input$annual_goto_satisfaction, { .goto_impact_tab("satisfaction") }, ignoreInit = TRUE)
  shiny::observeEvent(input$annual_goto_learning, { .goto_impact_tab("learning_stories") }, ignoreInit = TRUE)
  shiny::observeEvent(input$annual_goto_reach, { .goto_impact_tab("reach2") }, ignoreInit = TRUE)

  output$annual_survey_summary_banner <- shiny::renderUI({
    d <- filtered_annual()
    n <- if (is.null(d)) 0L else nrow(d)
    shiny::div(
      class = "summary-box",
      style = "border-left-color: #5c2f92; margin-bottom: 12px; max-width: 720px;",
      shiny::tags$p(
        class = "summary-box-headline",
        shiny::tags$strong("Annual responses in view: "), n
      ),
      shiny::tags$p(
        class = "summary-box-detail",
        "Source: Master Datasheet / ", shiny::tags$code("Annual Survey"), ". ",
        if (n == 0) "No rows for current filters (or sheet empty)." else "Sidebar org/date filters apply when columns match."
      )
    )
  })

  output$annual_survey_question_map_table <- DT::renderDataTable({
    map <- data.frame(
      Section = c(
        "Attendance timing / mode / host / topics",
        "Compared to before... (Likert)",
        "Behaviors taken / planned",
        "Income / savings / debt / investments / credit change",
        "Recommend 5 Buckets",
        "Open takeaways / goals / impact story",
        "Keep in touch / topics to learn more",
        "Demographics + email / zip",
        "Anything else"
      ),
      `Where it lives` = c(
        "Annual-only",
        "Same topics as workshop Post",
        "Same topics as workshop Pre/Post",
        "Annual-only",
        "Same topics as workshop Post (+ light chart here)",
        "Same topics as Learning tab (+ tables here)",
        "Same topics as Learning tab",
        "Same topics as Reach (+ coverage here)",
        "Annual-only / optional later"
      ),
      `Analysis location` = c(
        "Here",
        "Link → Wellness",
        "Link → Behavioral",
        "Here",
        "Link → Satisfaction (+ Here quick look)",
        "Here",
        "Link → Learning",
        "Link → Reach",
        "Here"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    DT::datatable(map, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$annual_pointer_wellness <- shiny::renderUI({
    .annual_pointer_box(
      "Same topics as Financial Wellness",
      paste0(
        "<p style='margin:0;font-size:13px;'>The annual form's <em>Compared to before your 5 Buckets experience...</em> ",
        "items are analyzed under <strong>Financial Wellness &rarr; Annual Survey</strong> ",
        "(Pre / Post / Pre vs Post stay as workshop-only).</p>"
      ),
      "annual_goto_wellness",
      "Financial Wellness"
    )
  })
  output$annual_pointer_behavioral <- shiny::renderUI({
    .annual_pointer_box(
      "Same topics as Behavioral Readiness",
      paste0(
        "<p style='margin:0;font-size:13px;'>The multi-select <em>As a result of my workshop(s)...</em> item ",
        "is analyzed under <strong>Behavioral Readiness &rarr; Annual Survey</strong>.</p>"
      ),
      "annual_goto_behavioral",
      "Behavioral Readiness"
    )
  })
  output$annual_pointer_satisfaction <- shiny::renderUI({
    .annual_pointer_box(
      "Same topics as Satisfaction",
      paste0(
        "<p style='margin:0;font-size:13px;'>Recommend-to-a-friend matches the Big Post recommend / NPS item ",
        "on the Satisfaction tab. A small recommend chart also stays on this Annual tab.</p>"
      ),
      "annual_goto_satisfaction",
      "Satisfaction"
    )
  })
  output$annual_pointer_learning <- shiny::renderUI({
    .annual_pointer_box(
      "Same topics as Learning & Impact Stories",
      paste0(
        "<p style='margin:0;font-size:13px;'>Takeaways, goals, impact stories, keep-in-touch, and \"learn more\" topics ",
        "match Learning & Impact Stories. Heavy wordclouds stay on that tab; response tables are below.</p>"
      ),
      "annual_goto_learning",
      "Learning & Impact Stories"
    )
  })
  output$annual_pointer_reach <- shiny::renderUI({
    .annual_pointer_box(
      "Same topics as Reach",
      paste0(
        "<p style='margin:0;font-size:13px;'>Age, gender, income, education, veteran, disability, and zip ",
        "are the same demographic fields. Reach is the main demographic view.</p>"
      ),
      "annual_goto_reach",
      "Reach"
    )
  })

  # Embedded on Financial Wellness / Behavioral Readiness tabs (Annual Survey collapsible).
  output$impact_tab_annual_wellness_index_hist <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual Financial Wellness Index (no data)"))
      }
      idx <- calculate_annual_compared_index(d)
      idx <- idx[is.finite(idx)]
      if (!length(idx)) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual Financial Wellness Index (no scored items)"))
      }
      show_bars <- tryCatch(isTRUE(input$wellness_index_annual_bars), error = function(e) TRUE)
      show_curves <- tryCatch(isTRUE(input$wellness_index_annual_curves), error = function(e) TRUE)
      if (!show_bars && !show_curves) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Enable Bars and/or Curves"))
      }
      bf <- .bar_flags()
      want_lab <- isTRUE(bf$n) || isTRUE(bf$pct)
      total_n <- length(idx)
      bins <- tryCatch({
        v <- input$wellness_index_annual_bins
        if (is.null(v) || length(v) != 1L || is.na(v) || !is.finite(v)) 24L else as.integer(min(max(round(v), 4L), 60L))
      }, error = function(e) 24L)
      br <- seq(-3, 3, length.out = bins + 1L)
      bw <- 6 / bins
      bar_col <- if (exists("ANNUAL_INDEX_COLOR")) ANNUAL_INDEX_COLOR else "#82c341"
      line_col <- if (exists("ANNUAL_INDEX_LINE_COLOR")) ANNUAL_INDEX_LINE_COLOR else "#4e7a22"
      fill_rgba <- if (exists("ANNUAL_INDEX_FILL_RGBA")) ANNUAL_INDEX_FILL_RGBA else "rgba(130, 195, 65, 0.18)"
      p <- plotly::plot_ly()
      ymax <- 1
      if (show_bars) {
        h <- graphics::hist(idx, breaks = br, plot = FALSE)
        txt <- if (want_lab) {
          vapply(seq_along(h$counts), function(i) {
            if (!is.finite(h$counts[i]) || h$counts[i] <= 0) return("")
            parts <- character(0)
            if (isTRUE(bf$n)) parts <- c(parts, as.character(as.integer(h$counts[i])))
            if (isTRUE(bf$pct) && total_n > 0) parts <- c(parts, sprintf("%.0f%%", h$counts[i] / total_n * 100))
            paste(parts, collapse = "\n")
          }, character(1))
        } else NULL
        has_txt <- !is.null(txt) && any(nzchar(txt))
        p <- p %>% plotly::add_trace(
          x = h$mids, y = h$counts, type = "bar", name = "Count",
          marker = list(color = bar_col, opacity = 0.45, line = list(color = bar_col, width = 1.3)),
          text = if (has_txt) txt else NULL,
          textposition = if (has_txt) "outside" else NULL,
          cliponaxis = FALSE
        )
        ymax <- max(ymax, max(h$counts, 1))
      }
      if (show_curves && total_n >= 2) {
        dens <- stats::density(idx, from = -3, to = 3, n = 256)
        ycurve <- dens$y * total_n * bw
        ymax <- max(ymax, max(ycurve, na.rm = TRUE))
        p <- p %>% plotly::add_trace(
          x = dens$x, y = ycurve, type = "scatter", mode = "lines", name = "Smoothed",
          line = list(color = line_col, width = 2),
          fill = "tozeroy",
          fillcolor = fill_rgba
        )
      }
      mean_shapes <- list()
      if (total_n >= 1 && is.finite(mean(idx)) && (show_bars || show_curves) &&
          exists(".mean_vline_outline_shapes", mode = "function")) {
        mean_shapes <- .mean_vline_outline_shapes(mean(idx), ymax, bar_col)
      }
      headroom <- if (want_lab) 1.22 else 1.05
      p %>%
        plotly::layout(
          title = list(text = "Annual Financial Wellness Index"),
          barmode = "overlay",
          bargap = 0,
          xaxis = list(title = "Index (-3 to +3)", range = c(-3, 3)),
          yaxis = list(title = "Count", range = c(0, ymax * headroom), rangemode = "nonnegative"),
          margin = list(t = 72, b = 100, l = 60, r = 24),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38, yanchor = "top"),
          shapes = mean_shapes
        ) %>%
        plotly::config(displayModeBar = TRUE, responsive = TRUE)
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  output$impact_tab_annual_wellness_index_summary <- shiny::renderUI({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      idx <- calculate_annual_compared_index(d)
      idx <- idx[is.finite(idx)]
      if (!length(idx)) {
        return(shiny::p(style = "font-size: 12px; color: #5f6369;", "No scored financial wellness responses."))
      }
      if (exists("deterministic_summary_box", mode = "function")) {
        deterministic_summary_box(
          scores = idx, mode = "level",
          null_value = 0, positive_threshold = 0,
          scale_label = "(-3 to +3)",
          headline_label = "Above neutral (score above 0)",
          scope_text = if (exists("scope_sentence", mode = "function")) scope_sentence(input) else NULL,
          accent = if (exists("ANNUAL_INDEX_COLOR")) ANNUAL_INDEX_COLOR else "#82c341"
        )
      } else {
        shiny::p(
          style = "font-size: 13px; color: #333; background: #f3f8eb; padding: 10px; border-radius: 6px;",
          shiny::tags$strong("n = "), length(idx),
          " | ", shiny::tags$strong("Mean = "), sprintf("%.2f", mean(idx)),
          " | ", shiny::tags$strong("SD = "), sprintf("%.2f", stats::sd(idx))
        )
      }
    }, error = function(e) NULL)
  })

  output$impact_tab_annual_wellness_item_hists <- shiny::renderUI({
    tryCatch({
      d <- filtered_annual()
      cols <- if (is.null(d)) character(0) else annual_compared_cols(d)
      if (!length(cols)) {
        return(shiny::p(style = "color: #5f6369; font-size: 12px;", "No Compared-to-before columns found."))
      }
      pals <- c("#5c2f92", "#82c341", "#f58220", "#0076be", "#9c27b0", "#00acc1", "#7cb342", "#e65100")
      rows <- lapply(seq_along(cols), function(i) {
        oid <- paste0("impact_tab_annual_wellness_item_", i)
        shiny::div(
          style = if (i %% 2L == 1L) "display: inline-block; width: 49%; vertical-align: top; padding-right: 1%;" else "display: inline-block; width: 49%; vertical-align: top;",
          plotly::plotlyOutput(oid, height = "260px")
        )
      })
      # Register plotly renders for each item
      lapply(seq_along(cols), function(i) {
        local({
          ii <- i
          col_nm <- cols[ii]
          oid <- paste0("impact_tab_annual_wellness_item_", ii)
          color <- pals[((ii - 1L) %% length(pals)) + 1L]
          output[[oid]] <- plotly::renderPlotly({
            tryCatch({
              dd <- filtered_annual()
              bf <- .bar_flags()
              if (is.null(dd) || nrow(dd) == 0 || is.null(col_nm) || !col_nm %in% colnames(dd)) {
                return(plotly::plotly_empty() %>% plotly::layout(title = annual_compared_short_label(col_nm)))
              }
              raw <- dd[[col_nm]]
              levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
              xchr <- trimws(as.character(raw))
              xchr <- xchr[!is.na(xchr) & nzchar(xchr)]
              if (!length(xchr)) {
                return(plotly::plotly_empty() %>% plotly::layout(title = annual_compared_short_label(col_nm)))
              }
              f <- factor(xchr, levels = levels)
              tab <- table(f)
              counts <- as.numeric(tab)
              total_n <- sum(counts)
              txt <- if (isTRUE(bf$n) || isTRUE(bf$pct)) {
                vapply(seq_along(counts), function(j) {
                  if (!is.finite(counts[j]) || counts[j] <= 0) return("")
                  parts <- character(0)
                  if (isTRUE(bf$n)) parts <- c(parts, as.character(as.integer(counts[j])))
                  if (isTRUE(bf$pct) && total_n > 0) parts <- c(parts, sprintf("%.0f%%", counts[j] / total_n * 100))
                  paste(parts, collapse = "\n")
                }, character(1))
              } else NULL
              has_txt <- !is.null(txt) && any(nzchar(txt))
              y_max <- max(counts, 1) * (if (isTRUE(bf$n) && isTRUE(bf$pct)) 1.32 else if (isTRUE(bf$n) || isTRUE(bf$pct)) 1.16 else 1.08)
              plotly::plot_ly(
                x = levels, y = counts, type = "bar",
                marker = list(color = color),
                text = if (has_txt) txt else NULL,
                textposition = if (has_txt) "outside" else NULL,
                cliponaxis = FALSE
              ) %>%
                plotly::layout(
                  title = list(text = annual_compared_short_label(col_nm), font = list(size = 12)),
                  xaxis = list(title = "", categoryorder = "array", categoryarray = levels, tickangle = -25),
                  yaxis = list(title = "Count", range = c(0, y_max), rangemode = "nonnegative"),
                  margin = list(l = 40, r = 10, t = 40, b = 70),
                  showlegend = FALSE
                )
            }, error = function(e) {
              plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
            })
          })
        })
        NULL
      })
      do.call(shiny::tagList, rows)
    }, error = function(e) {
      shiny::p(style = "color: #c62828;", conditionMessage(e))
    })
  })

  output$impact_tab_annual_wellness_agree_plot <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual Compared-to-before (no data)"))
      }
      ct <- annual_compared_agree_table(d)
      if (nrow(ct) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "No Compared-to-before columns found"))
      }
      ct$Item <- factor(ct$Item, levels = rev(as.character(ct$Item)))
      plotly::plot_ly(
        ct, x = ~agree_pct, y = ~Item, type = "bar", orientation = "h",
        marker = list(color = "#5c2f92"),
        text = ~paste0(agree_n, " / ", n, " (", agree_pct, "%)"),
        textposition = "outside", cliponaxis = FALSE,
        hovertemplate = "%{y}<br>Agree+ %{x:.1f}%<extra></extra>"
      ) %>%
        plotly::layout(
          title = "Annual: % Agree or Strongly Agree (Compared to before...)",
          xaxis = list(title = "%", range = c(0, 110)),
          yaxis = list(title = ""),
          margin = list(l = 220, r = 60, t = 48, b = 40),
          showlegend = FALSE
        )
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  output$impact_tab_annual_wellness_likert_plot <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual item distributions (no data)"))
      }
      long <- annual_compared_likert_long(d)
      if (nrow(long) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "No Compared-to-before columns found"))
      }
      levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
      cols <- c("#c62828", "#ef9a9a", "#a5d6a7", "#2e7d32")
      long$Response <- factor(long$Response, levels = levels)
      # Keep item order from agree table when possible
      agree <- annual_compared_agree_table(d)
      item_ord <- if (nrow(agree)) as.character(agree$Item) else unique(long$Item)
      long$Item <- factor(long$Item, levels = rev(item_ord))
      plotly::plot_ly(
        long, x = ~n, y = ~Item, color = ~Response, colors = cols,
        type = "bar", orientation = "h"
      ) %>%
        plotly::layout(
          barmode = "stack",
          title = "Annual: response counts by item",
          xaxis = list(title = "Count", rangemode = "nonnegative"),
          yaxis = list(title = ""),
          margin = list(l = 220, r = 40, t = 48, b = 40),
          legend = list(orientation = "h", y = -0.15)
        )
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  output$impact_tab_annual_behaviors_index_hist <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual Behavior Index (no data)"))
      }
      idx <- calculate_annual_behavior_index(d)
      idx <- idx[is.finite(idx)]
      if (!length(idx)) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual Behavior Index (no answers)"))
      }
      bf <- .bar_flags()
      mx <- max(idx, na.rm = TRUE)
      brks <- seq(-0.5, max(mx, 1) + 0.5, by = 1)
      h <- hist(idx, breaks = brks, plot = FALSE)
      total_n <- length(idx)
      txt <- if (isTRUE(bf$n) || isTRUE(bf$pct)) {
        vapply(seq_along(h$counts), function(i) {
          if (!is.finite(h$counts[i]) || h$counts[i] <= 0) return("")
          parts <- character(0)
          if (isTRUE(bf$n)) parts <- c(parts, as.character(as.integer(h$counts[i])))
          if (isTRUE(bf$pct) && total_n > 0) parts <- c(parts, sprintf("%.0f%%", h$counts[i] / total_n * 100))
          paste(parts, collapse = "\n")
        }, character(1))
      } else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      plotly::plot_ly(
        x = h$mids, y = h$counts, type = "bar",
        marker = list(color = "#0076be"),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        plotly::layout(
          title = "Annual Behavior Index (count of actions)",
          xaxis = list(title = "# actions selected"),
          yaxis = list(title = "Count", rangemode = "nonnegative"),
          margin = list(l = 50, r = 20, t = 48, b = 48),
          showlegend = FALSE
        )
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  output$impact_tab_annual_behaviors_index_summary <- shiny::renderUI({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      idx <- calculate_annual_behavior_index(d)
      idx <- idx[is.finite(idx)]
      if (!length(idx)) return(shiny::p(style = "font-size: 12px; color: #5f6369;", "No Annual behavior answers."))
      shiny::p(
        style = "font-size: 13px; color: #333; background: #f8f4fc; padding: 10px; border-radius: 6px;",
        shiny::tags$strong("n = "), length(idx),
        " · ", shiny::tags$strong("Mean = "), sprintf("%.2f", mean(idx)),
        " · ", shiny::tags$strong("SD = "), sprintf("%.2f", stats::sd(idx))
      )
    }, error = function(e) NULL)
  })

  output$impact_tab_annual_behaviors_plot <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual behaviors (no data)"))
      }
      col <- annual_find_col(d, ANNUAL_COL_PATTERNS$behaviors)
      ct <- annual_count_table(if (is.null(col)) NULL else d[[col]], split_multi = TRUE)
      bf <- .bar_flags()
      annual_bar_plotly(
        ct, "Annual: actions taken / planned (As a result of my workshop...)",
        color = "#0076be", show_n = bf$n, show_pct = bf$pct
      )
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  # Embedded on Satisfaction tab (Annual Survey collapsible).
  output$impact_tab_annual_recommend_hist <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual recommend (no data)"))
      }
      col <- annual_find_col(d, ANNUAL_COL_PATTERNS$recommend)
      if (is.null(col)) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual recommend — column not found"))
      }
      rec_vals <- annual_recommend_scores(d[[col]])
      # Accept 0–10 or 1–10; hist always shows full 0–10 axis for Annual form
      rec_vals <- rec_vals[is.finite(rec_vals) & rec_vals >= 0 & rec_vals <= 10]
      if (!length(rec_vals)) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Annual recommend (no scores)"))
      }
      bf <- .bar_flags()
      h <- hist(rec_vals, breaks = seq(-0.5, 10.5, 1), plot = FALSE)
      total_n <- length(rec_vals)
      txt <- if (isTRUE(bf$n) || isTRUE(bf$pct)) {
        vapply(seq_along(h$counts), function(i) {
          if (!is.finite(h$counts[i]) || h$counts[i] <= 0) return("")
          parts <- character(0)
          if (isTRUE(bf$n)) parts <- c(parts, as.character(as.integer(h$counts[i])))
          if (isTRUE(bf$pct) && total_n > 0) parts <- c(parts, sprintf("%.0f%%", h$counts[i] / total_n * 100))
          paste(parts, collapse = "\n")
        }, character(1))
      } else NULL
      has_txt <- !is.null(txt) && any(nzchar(txt))
      pals <- if (exists("REACH_PALETTE")) REACH_PALETTE else c("#5c2f92", "#82c341", "#f58220", "#0076be")
      cols <- pals[seq_len(11) %% length(pals) + 1]
      headroom <- if (isTRUE(bf$n) && isTRUE(bf$pct)) 1.32 else if (isTRUE(bf$n) || isTRUE(bf$pct)) 1.16 else 1.08
      plotly::plot_ly(
        x = 0:10, y = h$counts, type = "bar",
        marker = list(color = cols, line = list(color = "rgba(255,255,255,0.35)", width = 0.3)),
        text = if (has_txt) txt else NULL,
        textposition = if (has_txt) "outside" else NULL,
        cliponaxis = FALSE
      ) %>%
        plotly::layout(
          title = "Annual — Likelihood to recommend (0–10)",
          xaxis = list(title = "Rating (0–10)", range = c(-0.5, 10.5), dtick = 1),
          yaxis = list(title = "Count", range = c(0, max(h$counts, 1) * headroom), rangemode = "nonnegative"),
          margin = list(t = 50, b = 50),
          showlegend = FALSE
        )
    }, error = function(e) {
      plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
    })
  })

  output$impact_tab_annual_recommend_summary <- shiny::renderUI({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      col <- annual_find_col(d, ANNUAL_COL_PATTERNS$recommend)
      if (is.null(col)) return(NULL)
      r <- annual_recommend_scores(d[[col]])
      r <- r[is.finite(r) & r >= 0 & r <= 10]
      if (!length(r)) return(shiny::p(style = "font-size: 12px; color: #5f6369;", "No Annual recommend scores."))
      promoters <- sum(r >= 9)
      shiny::p(
        style = "font-size: 13px; color: #333; background: #f8f4fc; padding: 10px; border-radius: 6px;",
        shiny::tags$strong("n = "), length(r),
        " · ", shiny::tags$strong("Mean = "), sprintf("%.2f", mean(r)),
        " · ", shiny::tags$strong("Promoters (9–10) = "),
        sprintf("%d (%.0f%%)", promoters, 100 * promoters / length(r))
      )
    }, error = function(e) NULL)
  })

  .annual_cat_plot <- function(pattern, title, split_multi = FALSE, color = "#5c2f92",
                               ordered_levels = NULL, money_kind = NULL) {
    plotly::renderPlotly({
      tryCatch({
        d <- filtered_annual()
        if (is.null(d) || nrow(d) == 0) {
          return(plotly::plotly_empty() %>% plotly::layout(title = paste0(title, " (no data)")))
        }
        col <- annual_find_col(d, pattern)
        bf <- .bar_flags()
        vec <- if (is.null(col)) NULL else d[[col]]
        ct <- if (!is.null(money_kind)) {
          annual_money_count_table(vec, kind = money_kind)
        } else {
          annual_count_table(vec, split_multi = split_multi, ordered_levels = ordered_levels)
        }
        annual_bar_plotly(ct, title, color = color, show_n = bf$n, show_pct = bf$pct)
      }, error = function(e) {
        plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e)))
      })
    })
  }

  output$annual_first_attend_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$first_attend, "First attended", color = "#5c2f92",
    ordered_levels = ANNUAL_ATTEND_TIMING_LEVELS
  )
  output$annual_last_attend_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$last_attend, "Last attended", color = "#82c341",
    ordered_levels = ANNUAL_ATTEND_TIMING_LEVELS
  )
  output$annual_how_attend_plot <- .annual_cat_plot(ANNUAL_COL_PATTERNS$how_attend, "How attended", color = "#f58220")
  output$annual_topics_plot <- .annual_cat_plot(ANNUAL_COL_PATTERNS$topics, "Topics attended", split_multi = TRUE, color = "#0076be")

  output$annual_host_org_table <- DT::renderDataTable({
    d <- filtered_annual()
    if (is.null(d) || nrow(d) == 0) {
      return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
    }
    col <- annual_find_col(d, ANNUAL_COL_PATTERNS$host_org)
    ct <- annual_count_table(if (is.null(col)) NULL else d[[col]])
    DT::datatable(ct, rownames = FALSE, options = list(pageLength = 12, dom = "tp"))
  })

  output$annual_income_change_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$income_change, "Income change", color = "#5c2f92", money_kind = "increase"
  )
  output$annual_savings_change_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$savings_change, "Savings change", color = "#82c341", money_kind = "bipolar"
  )
  output$annual_debt_change_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$debt_change, "Debt change", color = "#f58220", money_kind = "decrease"
  )
  output$annual_investments_change_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$investments_change, "Investments change", color = "#0076be", money_kind = "bipolar"
  )
  output$annual_credit_change_plot <- .annual_cat_plot(
    ANNUAL_COL_PATTERNS$credit_change, "Credit score change", color = "#c9a227", money_kind = "credit"
  )

  annual_recommend_clicked_score <- shiny::reactiveVal(NULL)
  shiny::observeEvent(
    plotly::event_data("plotly_click", source = "annual_recommend"),
    {
      ev <- plotly::event_data("plotly_click", source = "annual_recommend")
      if (is.null(ev) || nrow(ev) == 0) return()
      sc <- suppressWarnings(as.integer(round(ev$x[1])))
      if (!is.finite(sc)) return()
      annual_recommend_clicked_score(sc)
    },
    ignoreNULL = TRUE
  )

  output$annual_recommend_plot <- plotly::renderPlotly({
    tryCatch({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Recommend (no data)"))
      }
      col <- annual_find_col(d, ANNUAL_COL_PATTERNS$recommend)
      scores <- if (is.null(col)) numeric(0) else annual_recommend_scores(d[[col]])
      if (!length(scores)) {
        return(plotly::plotly_empty() %>% plotly::layout(title = "Recommend (no scores)"))
      }
      tab <- table(factor(scores, levels = 0:10))
      df <- data.frame(score = as.integer(names(tab)), n = as.integer(tab), stringsAsFactors = FALSE)
      bf <- .bar_flags()
      txt <- if (bf$n || bf$pct) {
        total <- sum(df$n)
        vapply(seq_len(nrow(df)), function(i) {
          if (df$n[i] <= 0) return("")
          parts <- character(0)
          if (bf$n) parts <- c(parts, as.character(df$n[i]))
          if (bf$pct && total > 0) parts <- c(parts, sprintf("%.0f%%", 100 * df$n[i] / total))
          paste(parts, collapse = "\n")
        }, character(1))
      } else NULL
      plotly::plot_ly(
        df, x = ~score, y = ~n, type = "bar",
        source = "annual_recommend",
        customdata = ~score,
        marker = list(color = "#5c2f92"),
        text = txt, textposition = if (!is.null(txt)) "outside" else NULL, cliponaxis = FALSE,
        hovertemplate = "Score %{x}<br>Count %{y}<extra>Click for respondents</extra>"
      ) %>%
        plotly::layout(
          title = "How likely to recommend (0-10)",
          xaxis = list(title = "Score", dtick = 1),
          yaxis = list(title = "Count", rangemode = "nonnegative"),
          margin = list(t = 48)
        ) %>%
        plotly::event_register("plotly_click") %>%
        plotly::config(displayModeBar = TRUE)
    }, error = function(e) plotly::plotly_empty() %>% plotly::layout(title = paste("Error:", conditionMessage(e))))
  })

  output$annual_recommend_drill_heading <- shiny::renderUI({
    sc <- annual_recommend_clicked_score()
    if (is.null(sc)) {
      return(shiny::p(
        style = "font-size: 12px; color: #5f6369; margin-top: 10px;",
        "No score selected yet."
      ))
    }
    shiny::tags$h6(
      paste0("Respondents who scored ", sc),
      style = "color: #5c2f92; margin-top: 12px;"
    )
  })

  output$annual_recommend_drill_table <- DT::renderDataTable({
    sc <- annual_recommend_clicked_score()
    if (is.null(sc)) {
      return(DT::datatable(
        data.frame(Message = "Click a recommend bar to see respondents for that score."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    d <- filtered_annual()
    if (is.null(d) || nrow(d) == 0) {
      return(DT::datatable(data.frame(Message = "No annual data"), rownames = FALSE, options = list(dom = "t")))
    }
    col <- annual_find_col(d, ANNUAL_COL_PATTERNS$recommend)
    if (is.null(col)) {
      return(DT::datatable(data.frame(Message = "Recommend column not found"), rownames = FALSE, options = list(dom = "t")))
    }
    raw_sc <- suppressWarnings(as.numeric(as.character(d[[col]])))
    keep <- is.finite(raw_sc) & as.integer(round(raw_sc)) == as.integer(sc)
    sub <- d[keep, , drop = FALSE]
    if (nrow(sub) == 0) {
      return(DT::datatable(
        data.frame(Message = paste0("No respondents for score ", sc, " in current filters.")),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    email_col <- annual_find_col(sub, "Email Address")
    fn_col <- annual_find_col(sub, "First Name")
    ln_col <- annual_find_col(sub, "Last Name")
    org_col <- annual_find_col(sub, ANNUAL_COL_PATTERNS$host_org)
    out <- data.frame(
      recommend_score = as.integer(round(raw_sc[keep])),
      stringsAsFactors = FALSE
    )
    has_rid <- "respondent_id" %in% names(sub) &&
      any(!is.na(sub$respondent_id) & nzchar(as.character(sub$respondent_id)))
    if (has_rid) {
      out$respondent_id <- as.character(sub$respondent_id)
    } else if (!is.null(email_col)) {
      out$email <- as.character(sub[[email_col]])
    }
    if (!is.null(fn_col) || !is.null(ln_col)) {
      fn <- if (!is.null(fn_col)) as.character(sub[[fn_col]]) else ""
      ln <- if (!is.null(ln_col)) as.character(sub[[ln_col]]) else ""
      out$name <- trimws(paste(fn, ln))
    }
    if (!is.null(org_col)) out$host_org <- as.character(sub[[org_col]])
    if ("timestamp" %in% names(sub)) out$timestamp <- as.character(sub$timestamp)
    prefer <- c("recommend_score", "respondent_id", "email", "name", "host_org", "timestamp")
    out <- out[, intersect(prefer, names(out)), drop = FALSE]
    DT::datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color: #5f6369; font-size: 12px;",
        if (has_rid) {
          "Identity uses hashed respondent_id (same salt as Pre/Post). Email is not shown when ID is available."
        } else {
          "RESPONDENT_ID_SALT unset — showing email/name. Set the salt to match Pre/Post IDs."
        }
      )
    )
  })

  .open_text_table <- function(pattern) {
    DT::renderDataTable({
      d <- filtered_annual()
      if (is.null(d) || nrow(d) == 0) {
        return(DT::datatable(data.frame(Message = "No data"), rownames = FALSE, options = list(dom = "t")))
      }
      col <- annual_find_col(d, pattern)
      if (is.null(col)) {
        return(DT::datatable(data.frame(Message = "Column not found"), rownames = FALSE, options = list(dom = "t")))
      }
      txt <- trimws(as.character(d[[col]]))
      keep <- !is.na(txt) & nzchar(txt)
      out <- data.frame(Response = txt[keep], stringsAsFactors = FALSE)
      if ("timestamp" %in% names(d)) out$Timestamp <- as.character(d$timestamp[keep])
      DT::datatable(out, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
  }
  output$annual_takeaway_table <- .open_text_table(ANNUAL_COL_PATTERNS$takeaway)
  output$annual_proud_table <- .open_text_table(ANNUAL_COL_PATTERNS$proud_goal)
  output$annual_story_table <- .open_text_table(ANNUAL_COL_PATTERNS$impact_story)

  output$annual_demo_coverage <- shiny::renderUI({
    d <- filtered_annual()
    if (is.null(d) || nrow(d) == 0) return(shiny::p("No annual rows."))
    fields <- list(
      Zip = ANNUAL_COL_PATTERNS$zip,
      Age = ANNUAL_COL_PATTERNS$age,
      Race = ANNUAL_COL_PATTERNS$race,
      Gender = ANNUAL_COL_PATTERNS$gender,
      `Household income` = ANNUAL_COL_PATTERNS$income_hh,
      Education = ANNUAL_COL_PATTERNS$education,
      `First-gen college` = ANNUAL_COL_PATTERNS$first_gen_college,
      `First-gen U.S.` = ANNUAL_COL_PATTERNS$first_gen_us,
      Veteran = ANNUAL_COL_PATTERNS$veteran,
      Disability = ANNUAL_COL_PATTERNS$disability,
      Neurodivergent = ANNUAL_COL_PATTERNS$neurodivergent,
      Email = ANNUAL_COL_PATTERNS$email
    )
    bits <- vapply(names(fields), function(lab) {
      col <- annual_find_col(d, fields[[lab]])
      if (is.null(col)) return(paste0(lab, ": column missing"))
      v <- d[[col]]
      filled <- sum(!is.na(v) & nzchar(trimws(as.character(v))))
      paste0(lab, ": ", filled, "/", nrow(d), " (", round(100 * filled / max(nrow(d), 1), 0), "%)")
    }, character(1))
    shiny::tags$ul(style = "font-size:12px; line-height:1.35;", lapply(bits, shiny::tags$li))
  })
}
