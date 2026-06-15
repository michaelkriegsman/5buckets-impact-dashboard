# question_mapping_feb2026.R
# Master Workbook - Feb 2026 column names and question groupings
# Source: Pre_Survey_Handler_Feb2026.js, Post_Survey_Handler_Feb2026.js, PORT_PRE_HEADERS, PORT_POST_HEADERS

# =============================================================================
# PRE SUBMISSIONS - Feb 2026
# =============================================================================

PRE_METADATA_COLS <- c(
  "timestamp", "session_id", "org_name", "group", "facilitators", "modules_taught",
  "session_date", "session_start_time", "qa_session_found", "qa_metadata_changed",
  "respondent_id", "is_anonymous", "req", "first_name", "last_name",
  "respondent_name", "respondent_email"
)

PRE_OPENING_COLS <- c(
  "What is one intention you have for today's workshop?",
  "If you are arriving with any questions or curiosities, please share!",
  "How do you hope to feel at the end of today's workshop?"
)

# Financial Wellness - 8 Likert items (Big Pre only)
PRE_FINANCIAL_WELLNESS_COLS <- c(
  "How much do you agree with the following statements? [I know how much money I have right now.]",
  "How much do you agree with the following statements? [I know what I can afford right now.]",
  "How much do you agree with the following statements? [I know where my money goes each month.]",
  "How much do you agree with the following statements? [I feel optimistic about my financial future.]",
  "How much do you agree with the following statements? [I have a healthy relationship with money.]",
  "How much do you agree with the following statements? [I am able to manage stress or anxiety related to my finances.]",
  "How much do you agree with the following statements? [I feel confident planning ahead and making intentional financial choices.]",
  "How much do you agree with the following statements? [I would feel comfortable speaking with financial professionals about my financial needs.]"
)

# Past behaviors - single comma-separated column (Big Pre only)
PRE_PAST_BEHAVIORS_COL <- "Before today's workshop, I have done the following:"

PRE_DEMOGRAPHICS_COLS <- c(
  "Zip Code", "Age Group", "Race/Ethnicity (Select all that apply)", "Gender Identity",
  "Household Income", "What is the highest level of education you have completed? (Select one)",
  "First-Generation Status (College)", "First-Generation Status (U.S.)",
  "Veteran Status", "Disability Status", "Do you identify as neurodivergent?"
)

PRE_SUPPORT_COL <- "Is there anything that would help support your learning experience?"
PRE_ADDITIONAL_COMMENTS_COL <- "additional_comments"

# =============================================================================
# POST SUBMISSIONS - Feb 2026
# =============================================================================

POST_METADATA_COLS <- PRE_METADATA_COLS  # Same structure

POST_TODAY_SESSION_COLS <- c(
  "What is one idea, tool, or insight from today that stood out to you most?",
  "What is one way you could apply something you learned today in your own life?",
  "What made today's session helpful for you?",
  "How satisfied were you with today's 5 Buckets session?"
)

# Compared to before - 9 Likert items (Big Post only)
POST_COMPARED_TO_COLS <- c(
  "Compared to before your 5 Buckets experience... [I have a better understanding of the topics covered.]",
  "Compared to before your 5 Buckets experience... [I am more aware of how much money I have right now.]",
  "Compared to before your 5 Buckets experience... [I am more aware of what I can afford right now.]",
  "Compared to before your 5 Buckets experience... [I am more aware of where my money goes each month.]",
  "Compared to before your 5 Buckets experience... [I feel more optimistic about my financial future.]",
  "Compared to before your 5 Buckets experience... [I have a healthier relationship with money.]",
  "Compared to before your 5 Buckets experience... [I am better able to manage stress or anxiety related to my finances.]",
  "Compared to before your 5 Buckets experience... [I feel more confident planning ahead and making intentional financial choices.]",
  "Compared to before your 5 Buckets experience... [I would feel more comfortable speaking with financial professionals about my financial needs.]"
)

POST_PLANNED_ACTIONS_COL <- "As a result of my workshop(s) with 5 Buckets, I have already done the following, or plan to do so within the next month"

POST_IMPACT_STORY_COL <- "How has participating in this program helped or impacted you? Your story inspires others!"
POST_NPS_COL <- "How likely are you to recommend 5 Buckets to a friend or colleague?"
POST_KEEP_IN_TOUCH_COL <- "We would love to keep in touch! Which of the following opportunities are you interested in? (Select all that apply)"

# Official option prefixes (en-dash). Used to re-aggregate comma-split cells so fragments
# like "job", "club", "or community group" map back to "Open Workshops ...".
KEEP_IN_TOUCH_CANONICAL_LABELS <- c(
  "Focus Groups \u2013 Help us improve our workshops",
  "Volunteer Educator Program \u2013 Get trained to co-facilitate workshops",
  "Open Workshops \u2013 Join upcoming Zoom sessions",
  "Learner Spotlight \u2013 Share your story",
  "Host a Workshop \u2013 Bring 5 Buckets to your school",
  "College Ambassador Program \u2013 Represent 5 Buckets"
)
KEEP_IN_TOUCH_SHORT_LABELS <- c(
  "Focus Groups \u2013 Help us improve our workshops" = "Focus groups",
  "Volunteer Educator Program \u2013 Get trained to co-facilitate workshops" = "Volunteer educator program",
  "Open Workshops \u2013 Join upcoming Zoom sessions" = "Open workshops",
  "Learner Spotlight \u2013 Share your story" = "Learner spotlight",
  "Host a Workshop \u2013 Bring 5 Buckets to your school" = "Host a workshop",
  "College Ambassador Program \u2013 Represent 5 Buckets" = "College ambassador program"
)

POST_DEMOGRAPHICS_COLS <- PRE_DEMOGRAPHICS_COLS
POST_ADDITIONAL_COMMENTS_COL <- "additional_comments"

# =============================================================================
# PRE-POST COMPARABLE PAIRS (for change scores)
# =============================================================================
# Pre column -> Post column (concept alignment). Used for paired change-score analysis.
PRE_POST_COMPARABLE <- list(
  "understanding" = list(
    pre = NA_character_,  # No direct Pre equivalent
    post = "Compared to before your 5 Buckets experience... [I have a better understanding of the topics covered.]"
  ),
  "awareness_money" = list(
    pre = "How much do you agree with the following statements? [I know how much money I have right now.]",
    post = "Compared to before your 5 Buckets experience... [I am more aware of how much money I have right now.]"
  ),
  "awareness_afford" = list(
    pre = "How much do you agree with the following statements? [I know what I can afford right now.]",
    post = "Compared to before your 5 Buckets experience... [I am more aware of what I can afford right now.]"
  ),
  "awareness_where" = list(
    pre = "How much do you agree with the following statements? [I know where my money goes each month.]",
    post = "Compared to before your 5 Buckets experience... [I am more aware of where my money goes each month.]"
  ),
  "optimism" = list(
    pre = "How much do you agree with the following statements? [I feel optimistic about my financial future.]",
    post = "Compared to before your 5 Buckets experience... [I feel more optimistic about my financial future.]"
  ),
  "healthy_relationship" = list(
    pre = "How much do you agree with the following statements? [I have a healthy relationship with money.]",
    post = "Compared to before your 5 Buckets experience... [I have a healthier relationship with money.]"
  ),
  "stress" = list(
    pre = "How much do you agree with the following statements? [I am able to manage stress or anxiety related to my finances.]",
    post = "Compared to before your 5 Buckets experience... [I am better able to manage stress or anxiety related to my finances.]"
  ),
  "confidence" = list(
    pre = "How much do you agree with the following statements? [I feel confident planning ahead and making intentional financial choices.]",
    post = "Compared to before your 5 Buckets experience... [I feel more confident planning ahead and making intentional financial choices.]"
  ),
  "comfort_professionals" = list(
    pre = "How much do you agree with the following statements? [I would feel comfortable speaking with financial professionals about my financial needs.]",
    post = "Compared to before your 5 Buckets experience... [I would feel more comfortable speaking with financial professionals about my financial needs.]"
  )
)

# =============================================================================
# Reach demographics variable mapping for Impact tab
# =============================================================================
get_reach_demographics_mapping_feb2026 <- function() {
  short_names <- c("Zip Code", "Age Group", "Race/Ethnicity", "Gender Identity", "Household Income",
                   "Education", "First-Gen College", "First-Gen U.S.", "Veteran Status", "Disability Status", "Neurodivergent")
  data.frame(
    Variable = short_names,
    Big_Pre = rep("Yes", 11),
    Little_Pre = rep("No", 11),
    Little_Post = rep("No", 11),
    Big_Post = rep("Yes", 11),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Financial Wellness variable mapping for Impact tab
# =============================================================================
get_wellness_variable_mapping_feb2026 <- function() {
  pre_labels <- paste(c("Know amount", "Know afford", "Know where", "Optimism", "Relationship",
                        "Stress", "Confidence", "Comfort professionals"), collapse = "; ")
  post_labels <- paste(c("Understanding", "Awareness: amount", "Awareness: afford", "Awareness: where",
                         "Optimism", "Relationship", "Stress", "Confidence", "Comfort professionals"), collapse = "; ")
  data.frame(
    Survey_Type = c("Big Pre", "Little Pre", "Little Post", "Big Post"),
    Count = c(8, 0, 0, 9),
    Variable_Type = c("Likert (4-pt, -3 to +3)", "-", "-", "Likert (4-pt, -3 to +3)"),
    Items = c(pre_labels, "-", "-", post_labels),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Helper: find column in data by exact or partial match
# =============================================================================
find_col <- function(data, col_name_or_pattern, exact = TRUE) {
  nms <- colnames(data)
  if (exact) {
    idx <- match(col_name_or_pattern, nms)
    if (!is.na(idx)) return(nms[idx])
    return(NULL)
  }
  matches <- grep(col_name_or_pattern, nms, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) return(matches[1])
  return(NULL)
}
