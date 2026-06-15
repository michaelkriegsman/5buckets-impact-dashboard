# server_register.R — wire extracted server modules after reactives exist

register_dashboard_modules <- function(input, output, session, filtered_big_pre, filtered_big_post) {
  register_favorites_server(input, output, session)
  register_respondent_pairing_outputs(input, output, session, filtered_big_pre, filtered_big_post)
}
