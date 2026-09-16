# favorites.R — localStorage favorites + partner-report-ready catalog

fav_label_ascii <- function(x) {
  x <- as.character(x)
  x <- gsub("\u2014", " - ", x, fixed = TRUE)
  x <- gsub("\u2013", " - ", x, fixed = TRUE)
  x <- gsub("<e2><80><94>", " - ", x, fixed = TRUE)
  trimws(x)
}

#' Figure catalog: id → label, tab (impact_tabs value), output_id
FAVORITES_FIGURE_CATALOG <- data.frame(
  id = c(
    "overview_stats",
    "reach_demographics",
    "wellness_between",
    "wellness_within",
    "wellness_paired_change",
    "behavioral_paired_change",
    "satisfaction_nps",
    "learning_impact_story"
  ),
  label = c(
    "Overview - summary stats",
    "Reach - demographics",
    "Financial Wellness - between subjects",
    "Financial Wellness - within subjects",
    "Financial Wellness - paired change",
    "Behavioral Readiness - paired change",
    "Satisfaction - NPS",
    "Learning - impact story (open text)"
  ),
  tab = c(
    "overview", "reach2", "wellness", "wellness", "wellness",
    "behavioral", "satisfaction", "learning_stories"
  ),
  output_id = c(
    "session_summary_stats",
    "reach2_age_pre",
    "impact_wellness_between_dist",
    "impact_wellness_within_slopes",
    "impact_wellness_paired_change",
    "impact_behavioral_paired_change",
    "big_post_recommendation_hist",
    "impact_post_story_wc_en"
  ),
  stringsAsFactors = FALSE
)
FAVORITES_FIGURE_CATALOG$label <- vapply(FAVORITES_FIGURE_CATALOG$label, fav_label_ascii, character(1))

collect_filter_snapshot <- function(input) {
  orgs <- tryCatch({
    x <- input$selected_org
    if (is.null(x) || length(x) == 0) character(0) else {
      x <- trimws(as.character(x))
      x[x != "All" & nzchar(x) & !is.na(x)]
    }
  }, error = function(e) character(0))
  grps <- tryCatch({
    x <- input$selected_group
    if (is.null(x) || length(x) == 0) character(0) else {
      x <- trimws(as.character(x))
      x[x != "All" & nzchar(x) & !is.na(x)]
    }
  }, error = function(e) character(0))
  list(
    tab_mode = input$tab_mode %||% "impact",
    use_date_filter = isTRUE(input$use_date_filter),
    date_start = as.character(input$date_range[1]),
    date_end = as.character(input$date_range[2]),
    org = orgs,
    group = grps,
    filter_gender = input$filter_gender,
    filter_veteran = input$filter_veteran,
    filter_income = input$filter_income,
    filter_education = input$filter_education,
    filter_language = input$filter_language %||% "All",
    selected_modules = input$selected_modules
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

fav_star_button <- function(catalog_id) {
  actionButton(
    inputId = paste0("fav_star_", catalog_id),
    label = NULL,
    icon = icon("star"),
    class = "btn-link fav-star-btn",
    title = "Save to favorites with current filters"
  )
}

fav_save_catalog_item <- function(session, input, catalog_id) {
  row <- FAVORITES_FIGURE_CATALOG[FAVORITES_FIGURE_CATALOG$id == catalog_id, , drop = FALSE]
  if (nrow(row) != 1L) return(invisible(NULL))
  item <- list(
    id = row$id,
    label = row$label,
    tab = row$tab,
    output_id = row$output_id,
    filters = collect_filter_snapshot(input),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  session$sendCustomMessage("favorites_add", item)
  invisible(item)
}

favorites_tab_ui <- function() {
  tagList(
    h3("Your Favorites"),
    div(class = "alert alert-info", style = "font-size: 12px; padding: 10px 15px;",
      tags$strong("Stored in this browser only."),
      " Use Export/Import JSON to back up or move favorites. Partner report export will pre-select favorites for the current org filter (coming soon)."
    ),
    selectInput("favorite_catalog_pick", "Add current view to favorites",
      choices = setNames(FAVORITES_FIGURE_CATALOG$id, FAVORITES_FIGURE_CATALOG$label)),
    actionButton("favorite_add_btn", "Save favorite with current filters",
      class = "btn-primary", style = "margin-bottom: 12px;"),
    hr(),
    uiOutput("favorites_list_ui"),
    br(),
    fluidRow(
      column(6, downloadButton("favorites_export_json", "Export favorites JSON")),
      column(6, fileInput("favorites_import_json", "Import favorites JSON", accept = ".json"))
    )
  )
}

partner_report_sidebar_ui <- function() {
  actionButton("export_partner_report_btn", "Export Partner Report (coming soon)",
    icon = icon("file-powerpoint"),
    style = "width: 100%; margin-top: 8px;")
}

register_favorites_server <- function(input, output, session) {
  fav_apply_filters <- function(filters) {
    if (is.null(filters)) return()
    if (!is.null(filters$tab_mode)) {
      updateRadioButtons(session, "tab_mode", selected = filters$tab_mode)
    }
    if (!is.null(filters$use_date_filter)) {
      updateCheckboxInput(session, "use_date_filter", value = filters$use_date_filter)
    }
    if (!is.null(filters$date_start) && !is.null(filters$date_end)) {
      updateDateRangeInput(session, "date_range",
        start = as.Date(filters$date_start),
        end = as.Date(filters$date_end))
    }
    if (!is.null(filters$org)) {
      org_sel <- filters$org
      if (identical(org_sel, "All") || (length(org_sel) == 1L && identical(as.character(org_sel), "All"))) {
        org_sel <- character(0)
      }
      org_sel <- as.character(org_sel)
      updateSelectizeInput(session, "selected_org", selected = org_sel)
    }
    if (!is.null(filters$group)) {
      grp_sel <- filters$group
      if (identical(grp_sel, "All") || (length(grp_sel) == 1L && identical(as.character(grp_sel), "All"))) {
        grp_sel <- character(0)
      }
      grp_sel <- as.character(grp_sel)
      updateSelectizeInput(session, "selected_group", selected = grp_sel)
    }
    if (!is.null(filters$filter_gender)) updateCheckboxGroupInput(session, "filter_gender", selected = filters$filter_gender)
    if (!is.null(filters$filter_veteran)) updateCheckboxGroupInput(session, "filter_veteran", selected = filters$filter_veteran)
    if (!is.null(filters$filter_income)) updateCheckboxGroupInput(session, "filter_income", selected = filters$filter_income)
    if (!is.null(filters$filter_education)) updateCheckboxGroupInput(session, "filter_education", selected = filters$filter_education)
    if (!is.null(filters$selected_modules)) updateCheckboxGroupInput(session, "selected_modules", selected = filters$selected_modules)
    if (!is.null(filters$filter_language)) updateSelectInput(session, "filter_language", selected = filters$filter_language)
  }

  for (i in seq_len(nrow(FAVORITES_FIGURE_CATALOG))) {
    local({
      cid <- FAVORITES_FIGURE_CATALOG$id[i]
      observeEvent(input[[paste0("fav_star_", cid)]], {
        item <- fav_save_catalog_item(session, input, cid)
        if (!is.null(item)) {
          showNotification(paste0("Saved: ", item$label), type = "message", duration = 3)
        }
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$favorite_add_btn, {
    req(input$favorite_catalog_pick)
    row <- FAVORITES_FIGURE_CATALOG[FAVORITES_FIGURE_CATALOG$id == input$favorite_catalog_pick, , drop = FALSE]
    if (nrow(row) != 1) return()
    filters <- collect_filter_snapshot(input)
    item <- list(
      id = row$id,
      label = row$label,
      tab = row$tab,
      output_id = row$output_id,
      filters = filters,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    session$sendCustomMessage("favorites_add", item)
    showNotification(paste0("Saved: ", row$label), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  output$favorites_list_ui <- renderUI({
    tags$div(id = "favorites_list_mount",
      tags$p(style = "color:#5f6369;", "Loading favorites from browser storage…"),
      tags$script(HTML("if (typeof Shiny !== 'undefined') Shiny.setInputValue('favorites_refresh', Math.random());")))
  })

  observeEvent(input$favorites_refresh, {
    session$sendCustomMessage("favorites_request_list", list())
  }, ignoreInit = FALSE)

  observeEvent(input$favorites_json_from_browser, {
    raw <- input$favorites_json_from_browser
    if (is.null(raw) || !nzchar(raw)) {
      output$favorites_list_ui <- renderUI({
        p(style = "color: #5f6369;", "No favorites yet. Pick a figure above and click Save.")
      })
      session$userData$favorites_items <- NULL
      return()
    }
    items <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) list())
    if (!length(items)) {
      output$favorites_list_ui <- renderUI({
        p(style = "color: #5f6369;", "No favorites yet.")
      })
      session$userData$favorites_items <- NULL
      return()
    }
    for (j in seq_along(items)) {
      if (!is.null(items[[j]]$label)) items[[j]]$label <- fav_label_ascii(items[[j]]$label)
    }
    session$userData$favorites_items <- items
    output$favorites_list_ui <- renderUI({
      tagList(
        lapply(seq_along(items), function(i) {
          it <- items[[i]]
          wellPanel(
            tags$strong(fav_label_ascii(it$label %||% it$id)),
            tags$br(),
            tags$small(style = "color:#5f6369;",
              "Tab: ", it$tab %||% "-",
              " | Org: ", it$filters$org %||% "All",
              " | Group: ", it$filters$group %||% "All",
              if (!is.null(it$filters$filter_language) && it$filters$filter_language != "All") {
                paste0(" | Language: ", it$filters$filter_language)
              } else {
                ""
              }
            ),
            tags$br(),
            tags$button(
              type = "button",
              class = "btn btn-primary btn-sm",
              style = "margin-right: 6px;",
              onclick = sprintf("Shiny.setInputValue('fav_view_idx', %d, {priority: 'event'});", i),
              "View"
            ),
            actionButton(paste0("fav_remove_", i), "Remove", class = "btn-sm btn-default")
          )
        })
      )
    })
  })

  observeEvent(input$fav_view_idx, {
    i <- as.integer(input$fav_view_idx)
    items <- session$userData$favorites_items
    if (is.null(items) || length(items) == 0 || is.na(i) || i < 1L || i > length(items)) return()
    it <- items[[i]]
    fav_apply_filters(it$filters)
    if (!is.null(it$tab)) updateTabsetPanel(session, "impact_tabs", selected = it$tab)
    showNotification(paste0("Applied filters for: ", fav_label_ascii(it$label %||% it$id)), duration = 3)
  }, ignoreInit = TRUE)

  observe({
    items <- session$userData$favorites_items
    if (is.null(items)) return()
    lapply(seq_along(items), function(i) {
      observeEvent(input[[paste0("fav_remove_", i)]], {
        it <- items[[i]]
        session$sendCustomMessage("favorites_remove", list(id = it$id))
        Sys.sleep(0.2)
        session$sendCustomMessage("favorites_request_list", list())
      }, ignoreInit = TRUE)
    })
  })

  output$favorites_export_json <- downloadHandler(
    filename = function() paste0("5buckets_favorites_", Sys.Date(), ".json"),
    content = function(file) {
      raw <- input$favorites_json_from_browser
      if (is.null(raw) || !nzchar(raw)) raw <- "[]"
      writeLines(raw, file)
    }
  )

  observeEvent(input$favorites_import_json, {
    req(input$favorites_import_json$datapath)
    txt <- paste(readLines(input$favorites_import_json$datapath, warn = FALSE), collapse = "\n")
    session$sendCustomMessage("favorites_import", list(json = txt))
    showNotification("Favorites imported into this browser", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$export_partner_report_btn, {
    showModal(modalDialog(
      title = "Export Partner Report - planned workflow",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tagList(
        tags$ol(
          tags$li("Filter sidebar to the partner org / group and date range."),
          tags$li("Preview figures; check favorites or pick outputs from the catalog."),
          tags$li("Answer a short wizard (title, audience, tone)."),
          tags$li("Generate a branded .pptx (one figure per slide + editable AI caption)."),
          tags$li("Internal team edits in PowerPoint before sending.")
        ),
        p("Favorites saved here will pre-populate the figure checklist when this ships.",
          style = "font-size: 12px; color: #5f6369;")
      )
    ))
  }, ignoreInit = TRUE)
}
