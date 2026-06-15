# respondent_pairing.R — respondent_id coverage stats for Overview / Data quality

compute_respondent_pairing_stats <- function(pre_df, post_df) {
  empty <- list(
    distinct_ids = 0L,
    big_pre_ids = 0L,
    big_post_ids = 0L,
    paired_any = 0L,
    pre_only = 0L,
    post_only = 0L,
    pct_paired = NA_real_
  )
  if (is.null(pre_df)) pre_df <- data.frame()
  if (is.null(post_df)) post_df <- data.frame()
  valid_id <- function(x) {
    !is.na(x) & nzchar(trimws(as.character(x))) & !grepl("^ANON", trimws(as.character(x)), ignore.case = TRUE)
  }
  if (!"respondent_id" %in% colnames(pre_df) && !"respondent_id" %in% colnames(post_df)) {
    return(empty)
  }
  pre_ids <- character(0)
  post_ids <- character(0)
  if ("respondent_id" %in% colnames(pre_df)) {
    pre_ids <- unique(trimws(as.character(pre_df$respondent_id[valid_id(pre_df$respondent_id)])))
  }
  if ("respondent_id" %in% colnames(post_df)) {
    post_ids <- unique(trimws(as.character(post_df$respondent_id[valid_id(post_df$respondent_id)])))
  }
  all_ids <- unique(c(pre_ids, post_ids))
  paired <- intersect(pre_ids, post_ids)
  list(
    distinct_ids = length(all_ids),
    big_pre_ids = length(pre_ids),
    big_post_ids = length(post_ids),
    paired_any = length(paired),
    pre_only = length(setdiff(pre_ids, post_ids)),
    post_only = length(setdiff(post_ids, pre_ids)),
    pct_paired = if (length(all_ids) > 0) round(100 * length(paired) / length(all_ids), 1) else NA_real_
  )
}

respondent_pairing_html <- function(stats) {
  if (is.null(stats) || stats$distinct_ids == 0) {
    return("<p style='color:#5f6369;'><em>No non-anonymous respondent_id values in current filter.</em></p>")
  }
  paste0(
    "<div style='font-size:13px;line-height:1.5;'>",
    "<strong>Respondent ID coverage</strong> (current filters)<br>",
    "Distinct IDs: <strong>", stats$distinct_ids, "</strong> &nbsp;|&nbsp; ",
    "Pre: <strong>", stats$big_pre_ids, "</strong> &nbsp;|&nbsp; ",
    "Post: <strong>", stats$big_post_ids, "</strong><br>",
    "Paired (same ID in Pre and Post): <strong>", stats$paired_any, "</strong>",
    if (!is.na(stats$pct_paired)) paste0(" (", stats$pct_paired, "% of distinct IDs)") else "",
    " &nbsp;|&nbsp; Pre only: ", stats$pre_only,
    " &nbsp;|&nbsp; Post only: ", stats$post_only,
    "<br><span style='color:#5f6369;font-size:11px;'>Pairing uses respondent_id across all sessions in filter; sparse matches are expected when email differs between surveys.</span>",
    "</div>"
  )
}

register_respondent_pairing_outputs <- function(input, output, session, filtered_big_pre, filtered_big_post) {
  render_pairing_banner <- function() {
    st <- compute_respondent_pairing_stats(filtered_big_pre(), filtered_big_post())
    HTML(respondent_pairing_html(st))
  }
  # Separate output IDs — Shiny allows only one output$ per id (Overview was blank when duplicated)
  output$respondent_pairing_banner_overview <- renderUI(render_pairing_banner())
  output$respondent_pairing_banner_dq <- renderUI(render_pairing_banner())

  output$respondent_lookup_table <- renderDT({
    req(input$respondent_lookup_id)
    q <- trimws(input$respondent_lookup_id)
    if (!nzchar(q)) return(datatable(data.frame()))
    pre <- filtered_big_pre()
    post <- filtered_big_post()
    pick <- function(df) {
      if (!"respondent_id" %in% colnames(df)) return(df[0, , drop = FALSE])
      df[grepl(q, df$respondent_id, ignore.case = TRUE), , drop = FALSE]
    }
    pr <- pick(pre)
    po <- pick(post)
    if (nrow(pr)) pr$source <- "Pre"
    if (nrow(po)) po$source <- "Post"
    cols <- unique(c("source", "respondent_id", "session_id", "org_name", "group", "timestamp", intersect(colnames(pr), colnames(po))))
    out <- dplyr::bind_rows(
      if (nrow(pr)) pr[, intersect(cols, colnames(pr)), drop = FALSE] else NULL,
      if (nrow(po)) po[, intersect(cols, colnames(po)), drop = FALSE] else NULL
    )
    if (nrow(out) == 0) return(datatable(data.frame(message = "No rows match.")))
    datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
}
