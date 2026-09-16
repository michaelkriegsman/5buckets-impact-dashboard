# server_register.R — wire extracted server modules after reactives exist

register_dashboard_modules <- function(input, output, session,
                                       filtered_pre, filtered_post, filtered_annual = NULL,
                                       filtered_big_pre = NULL, filtered_little_pre = NULL,
                                       filtered_little_post = NULL, filtered_big_post_only = NULL,
                                       session_summary_data = NULL) {
  register_favorites_server(input, output, session)
  if (!is.null(filtered_big_pre) && !is.null(filtered_little_pre) &&
      !is.null(filtered_little_post) && !is.null(filtered_big_post_only)) {
    register_respondent_pairing_outputs(
      input, output, session,
      filtered_big_pre, filtered_little_pre, filtered_little_post, filtered_big_post_only,
      filtered_annual
    )
    register_export_server(
      input, output, session,
      filtered_pre, filtered_post, filtered_annual,
      filtered_big_pre, filtered_little_pre, filtered_little_post, filtered_big_post_only,
      session_summary_data
    )
  } else {
    # Fallback: treat filtered_pre/post as pools (legacy)
    register_respondent_pairing_outputs(
      input, output, session,
      filtered_pre, filtered_pre, filtered_post, filtered_post,
      filtered_annual
    )
  }
  if (!is.null(filtered_annual)) {
    register_annual_survey_outputs(input, output, session, filtered_annual)
  }
}
