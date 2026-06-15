# big_post_analysis.R - Analysis functions for Big Post survey data

# ============================================================================
# Helper Functions for Visualizations
# ============================================================================

# Order Likert responses properly (Strongly Disagree → Strongly Agree)
# Reuse from big_pre_analysis.R
order_likert_responses <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
  result <- factor(x, levels = levels, ordered = TRUE)
  return(result)
}

# Get ordered Likert table with all levels
get_ordered_likert_table <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
  if (is.null(x) || length(x) == 0) return(structure(rep(0L, 5), names = levels, class = "table"))
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(structure(rep(0L, 5), names = levels, class = "table"))
  counts <- table(factor(x, levels = levels, ordered = TRUE))
  return(counts)
}

# Order satisfaction/rating scales (1-5 or 1-10)
order_rating_scale <- function(x, max_val = 5) {
  levels <- as.character(1:max_val)
  result <- factor(as.character(x), levels = levels, ordered = TRUE)
  return(result)
}

# Order income levels logically (reuse from big_pre_analysis.R)
order_income_levels <- function(income_vec) {
  income_order <- c(
    "If one person under $36k; two people under $41k; family of four under $52k",
    "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
    "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
    "If one person over $97k; two people over $111k; family of four over $139k"
  )
  result <- factor(income_vec, levels = income_order, ordered = TRUE)
  return(result)
}

# Order education levels logically
order_education_levels <- function(edu_vec) {
  # Order: some middle school, middle school, some high school, high school, etc.
  edu_order <- c(
    "Some middle school",
    "Middle school",
    "Some high school",
    "High school",
    "Some college",
    "Associate's degree",
    "Bachelor's degree",
    "Some graduate school",
    "Master's degree",
    "Doctoral degree",
    "Professional degree"
  )
  # Match actual values (case-insensitive)
  edu_lower <- tolower(edu_vec)
  result <- factor(edu_vec, levels = unique(edu_vec)[order(match(edu_lower, tolower(edu_order)))], ordered = TRUE)
  return(result)
}

# Order age groups logically (reuse from big_pre_analysis.R)
order_age_groups <- function(age_vec) {
  age_order <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  result <- factor(age_vec, levels = age_order, ordered = TRUE)
  return(result)
}

# ============================================================================
# Index Calculation Functions
# ============================================================================

# Convert Likert scale to numeric (1-5)
likert_to_numeric <- function(x) {
  result <- rep(NA, length(x))
  result[x == "Strongly Disagree"] <- 1
  result[x == "Disagree"] <- 2
  result[x == "Neutral"] <- 3
  result[x == "Agree"] <- 4
  result[x == "Strongly Agree"] <- 5
  return(as.numeric(result))
}

# Calculate Post-Workshop Impact Index
# Average of 6 impact questions (Understanding, Optimism, Relationship, Stress, Confidence, Comfort)
calculate_post_impact_index <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  
  # Find impact question columns (all start with "Compared to before today's session")
  impact_cols <- grep("Compared to before today's session", colnames(post_data), ignore.case = TRUE, value = TRUE)
  
  if (length(impact_cols) == 0) {
    return(rep(NA, nrow(post_data)))
  }
  
  # Convert all to numeric
  impact_numeric <- sapply(impact_cols, function(col) {
    likert_to_numeric(post_data[[col]])
  })
  
  # Average across all impact questions
  impact_index <- rowMeans(impact_numeric, na.rm = TRUE)
  impact_index[rowSums(!is.na(impact_numeric)) == 0] <- NA
  
  return(impact_index)
}

# Calculate Planned Actions Index
# Count of planned actions (Yes responses)
calculate_planned_actions_index <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  
  # Find planned action columns
  action_cols <- grep("As a result of this workshop, I plan to take", colnames(post_data), ignore.case = TRUE, value = TRUE)
  
  if (length(action_cols) == 0) {
    return(rep(NA, nrow(post_data)))
  }
  
  # Count "Yes" or "Yes!!" responses
  yes_count <- rowSums(
    post_data[, action_cols, drop = FALSE] %in% c("Yes", "Yes!!", "Yes!"), 
    na.rm = TRUE
  )
  
  return(yes_count)
}

# Calculate Workshop Quality Index
# Average of satisfaction ratings
calculate_quality_index <- function(post_data) {
  if (nrow(post_data) == 0) return(numeric(0))
  
  # Find satisfaction columns
  satisfaction_cols <- grep("How satisfied are you", colnames(post_data), ignore.case = TRUE, value = TRUE)
  
  if (length(satisfaction_cols) == 0) {
    return(rep(NA, nrow(post_data)))
  }
  
  # Convert to numeric and average
  satisfaction_numeric <- sapply(satisfaction_cols, function(col) {
    as.numeric(post_data[[col]])
  })
  
  quality_index <- rowMeans(satisfaction_numeric, na.rm = TRUE)
  quality_index[rowSums(!is.na(satisfaction_numeric)) == 0] <- NA
  
  return(quality_index)
}

# ============================================================================
# Variable Mapping Table Data with Response Counts and Central Tendency
# ============================================================================

get_big_post_variable_mapping <- function(post_data = NULL) {
  # Create comprehensive variable mapping table
  # Organized by: Metadata, Little Post + Big Post, Big Post Only
  mapping <- data.frame(
    Question_Text = c(
      # ===== METADATA (14 vars) =====
      "timestamp", "session_id", "org_name", "group", "facilitators", 
      "modules_taught", "session_date", "session_start_time", 
      "qa_metadata_changed", "qa_session_found", "respondent_id", 
      "is_anonymous", "respondent_name", "respondent_email",
      # ===== LITTLE POST + BIG POST (Common Questions) =====
      # Open Text - Learning & Impact (4 vars)
      "What were the most notable or interesting things you learned today?",
      "What is one thing you might change about your finances after this workshop? (if anything)",
      "How has participating in this workshop helped or impacted you? Your story inspires others!",
      "What personal finance topic(s) would you like to learn more about from 5 Buckets?",
      # Workshop Quality & Experience (1 var)
      "How likely are you to recommend 5 Buckets to a friend or colleague?",
      # Zip Code (common to Little Post + Big Post)
      "Zip Code",
      # Open text
      "Anything else you would like to share with our team?",
      # ===== BIG POST ONLY =====
      # Workshop Quality & Experience (2 vars) - Big Post Only
      "How satisfied are you with your 5 Buckets experience?",
      "How satisfied are you with the quality of your facilitator(s)?",
      # Facilitator Ratings (4 vars) - Big Post Only
      "My facilitator was... [Knowledgeable]",
      "My facilitator was... [Interactive]",
      "My facilitator was... [Relatable]",
      "My facilitator was... [Engaging]",
      # Workshop Impact - Financial Wellbeing (6 Likert) - Big Post Only
      "Compared to before today's session with 5 Buckets... [I have a better understanding of the topics covered.]",
      "Compared to before today's session with 5 Buckets... [I feel more optimistic about my financial future.]",
      "Compared to before today's session with 5 Buckets... [I have a healthier relationship with money.]",
      "Compared to before today's session with 5 Buckets... [I feel less stress about my finances.]",
      "Compared to before today's session with 5 Buckets... [I feel more confident I can plan ahead to make smart financial choices for myself.]",
      "Compared to before today's session with 5 Buckets... [I feel more comfortable speaking with financial professionals about my financial needs.]",
      # Planned Actions (8 Yes/No/Maybe) - Big Post Only
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Set a financial goal]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Track my spending for at least one week]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Create or update a personal budget]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Contribute to a savings or investment account]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Check my credit score or credit report]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Share a financial tip or resource with someone in my life]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Talk to someone I trust about money]",
      "As a result of this workshop, I plan to take the following actions in the next 30 days: [Reflect on my thoughts and feelings about money]",
      # Demographics (9 vars) - Big Post Only
      "Age Group",
      "Race (Select all that apply):",
      "Gender Identity:",
      "Household Income:",
      "What is the highest level of education you have completed? (Select one)",
      "First-Generation Status (College):",
      "First-Generation Status (U.S.):",
      "Veteran Status:",
      "Disability Status:",
      # Future Engagement - Only in Big Post
      "We would love to keep in touch! Which of the following opportunities are you interested in? (Select all that apply)"
    ),
    Gross_Category = c(
      # Metadata
      rep("Metadata", 14),
      # Little Post + Big Post
      rep("Workshop Learning & Impact", 4),
      "Workshop Quality & Experience",
      "Demographics",
      "Additional Context",
      # Big Post Only
      rep("Workshop Quality & Experience", 2),
      rep("Facilitator Ratings", 4),
      rep("Workshop Impact - Financial Wellbeing", 6),
      rep("Planned Actions", 8),
      rep("Demographics", 9),
      "Future Engagement"
    ),
    Fine_Category = c(
      # Metadata
      "Timestamp", "Session Link", "Organization", "Group", "Facilitators",
      "Content", "Date", "Time", "QA Flag", "QA Flag", "Identifier",
      "Privacy", "Name", "Contact",
      # Little Post + Big Post - Learning & Impact
      "Notable Learnings", "Planned Changes", "Personal Impact Story", "Future Topics",
      # Little Post + Big Post - Quality & Experience
      "Recommendation (NPS)",
      # Little Post + Big Post - Demographics
      "Location",
      # Little Post + Big Post - Open text
      "Open Feedback",
      # Big Post Only - Quality & Experience
      "Experience Satisfaction", "Facilitator Satisfaction",
      # Big Post Only - Facilitator Ratings
      "Knowledgeable", "Interactive", "Relatable", "Engaging",
      # Big Post Only - Impact - Financial Wellbeing
      "Understanding", "Optimism", "Money Relationship", "Stress Reduction",
      "Planning Confidence", "Professional Comfort",
      # Big Post Only - Planned Actions
      "Goal Setting", "Spending Tracking", "Budgeting", "Saving/Investing",
      "Credit Awareness", "Financial Sharing", "Financial Communication", "Financial Reflection",
      # Big Post Only - Demographics
      "Age", "Race/Ethnicity", "Gender", "Income",
      "Education", "First-Gen College", "First-Gen U.S.", "Veteran", "Disability",
      # Big Post Only - Future Engagement
      "Engagement Opportunities"
    ),
    Response_Type = c(
      # Metadata
      rep("Metadata", 14),
      # Little Post + Big Post
      rep("Open Text", 4),
      "Numeric (1-10)",
      "Text",
      "Open Text",
      # Big Post Only
      rep("Numeric (1-5)", 2),
      rep("Likert (5-point)", 4),
      rep("Likert (5-point)", 6),
      rep("Binary/Ternary", 8),
      c("Categorical", "Multi-select Categorical", "Categorical", "Ordinal Categorical",
        "Ordinal Categorical", "Binary", "Binary", "Binary", "Binary"),
      "Multi-select Categorical"
    ),
    Survey_Type = c(
      # Metadata
      rep("All Surveys", 14),
      # Little Post + Big Post (7 vars)
      rep("Little Post + Big Post", 4),
      "Little Post + Big Post",
      "Little Post + Big Post",
      "Little Post + Big Post",
      # Big Post Only (rest)
      rep("Big Post Only", 2),
      rep("Big Post Only", 4),
      rep("Big Post Only", 6),
      rep("Big Post Only", 8),
      rep("Big Post Only", 9),
      "Big Post Only"
    ),
    stringsAsFactors = FALSE
  )
  
  # If post_data is provided, add response counts and central tendency
  if (!is.null(post_data) && nrow(post_data) > 0) {
    # Find corresponding columns in post_data
    total_responses <- numeric(nrow(mapping))
    central_tendency <- character(nrow(mapping))
    
    for (i in 1:nrow(mapping)) {
      question_text <- mapping$Question_Text[i]
      
      # Try to find matching column
      matching_cols <- grep(question_text, colnames(post_data), ignore.case = TRUE, fixed = TRUE)
      if (length(matching_cols) == 0) {
        # Try partial match for metadata
        if (question_text %in% c("timestamp", "session_id", "org_name", "group", "facilitators",
                                 "modules_taught", "session_date", "session_start_time",
                                 "qa_metadata_changed", "qa_session_found", "respondent_id",
                                 "is_anonymous", "respondent_name", "respondent_email")) {
          matching_cols <- grep(question_text, colnames(post_data), ignore.case = TRUE, value = FALSE)
        }
      }
      
      if (length(matching_cols) > 0) {
        col_data <- post_data[[matching_cols[1]]]
        non_na <- sum(!is.na(col_data))
        total_responses[i] <- non_na
        
        if (non_na > 0) {
          # Calculate central tendency based on response type
          if (mapping$Response_Type[i] == "Numeric (1-10)" || mapping$Response_Type[i] == "Numeric (1-5)") {
            numeric_vals <- as.numeric(col_data)
            numeric_vals <- numeric_vals[!is.na(numeric_vals)]
            if (length(numeric_vals) > 0) {
              central_tendency[i] <- sprintf("Mean: %.2f", mean(numeric_vals))
            }
          } else if (mapping$Response_Type[i] == "Likert (5-point)") {
            # Most common response
            likert_vals <- col_data[!is.na(col_data)]
            if (length(likert_vals) > 0) {
              mode_val <- names(sort(table(likert_vals), decreasing = TRUE))[1]
              central_tendency[i] <- paste0("Mode: ", mode_val)
            }
          } else if (mapping$Response_Type[i] == "Binary/Ternary") {
            # Most common response
            action_vals <- col_data[!is.na(col_data)]
            if (length(action_vals) > 0) {
              mode_val <- names(sort(table(action_vals), decreasing = TRUE))[1]
              central_tendency[i] <- paste0("Mode: ", mode_val)
            }
          } else if (mapping$Response_Type[i] == "Categorical" || mapping$Response_Type[i] == "Ordinal Categorical") {
            # Most common response
            cat_vals <- col_data[!is.na(col_data)]
            if (length(cat_vals) > 0) {
              mode_val <- names(sort(table(cat_vals), decreasing = TRUE))[1]
              central_tendency[i] <- paste0("Mode: ", mode_val)
            }
          } else if (mapping$Response_Type[i] == "Open Text") {
            # Average length
            text_vals <- as.character(col_data[!is.na(col_data)])
            if (length(text_vals) > 0) {
              avg_length <- mean(nchar(text_vals))
              central_tendency[i] <- sprintf("Avg length: %.0f chars", avg_length)
            }
          } else {
            central_tendency[i] <- "-"
          }
        } else {
          central_tendency[i] <- "No responses"
        }
      } else {
        total_responses[i] <- 0
        central_tendency[i] <- "Column not found"
      }
    }
    
    mapping$Total_Responses <- total_responses
    mapping$Central_Tendency <- central_tendency
  } else {
    mapping$Total_Responses <- NA
    mapping$Central_Tendency <- NA
  }
  
  return(mapping)
}

