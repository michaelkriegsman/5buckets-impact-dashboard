# load_sources.R — data + module sources for app.R (v3)
# Keeps app.R focused on UI/server; retired dated files use *_YYYY-MM.R suffix.

source("global.R")
source("data/process_data.R")
source("data/big_pre_analysis.R")
source("data/big_post_analysis.R")
# Feb 2026 analysis function names (internal *_feb2026); aliased in app.R
get_variable_mapping_table <- get_variable_mapping_table_feb2026
get_big_post_variable_mapping <- get_big_post_variable_mapping_feb2026
calculate_wellness_index <- calculate_wellness_index_feb2026
calculate_behavioral_index <- calculate_behavioral_index_feb2026
calculate_post_impact_index <- calculate_post_impact_index_feb2026
calculate_planned_actions_index <- calculate_planned_actions_index_feb2026
calculate_quality_index <- calculate_quality_index_feb2026
get_past_behaviors_counts <- get_past_behaviors_counts_feb2026
get_planned_actions_counts <- get_planned_actions_counts_feb2026
source("data/open_text_parse.R")
source("data/text_analysis.R")
source("data/question_mapping.R", encoding = "UTF-8")
source("data/overview_timeline.R")
source("data/language_filter.R")
source("modules/favorites.R")
source("server/respondent_pairing.R")
