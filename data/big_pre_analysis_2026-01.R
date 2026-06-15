# big_pre_analysis.R - Analysis functions for Big Pre survey data

# ============================================================================
# Helper Functions for Visualizations
# ============================================================================

# Order Likert responses properly (Strongly Disagree → Strongly Agree)
order_likert_responses <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
  # Ensure all levels are present, even if count is 0
  result <- factor(x, levels = levels, ordered = TRUE)
  return(result)
}

# Get ordered Likert table with all levels
get_ordered_likert_table <- function(x) {
  levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
  if (is.null(x) || length(x) == 0) return(structure(rep(0L, 5), names = levels, class = "table"))
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(structure(rep(0L, 5), names = levels, class = "table"))
  table(factor(x, levels = levels, ordered = TRUE))
}

# Order income levels logically
order_income_levels <- function(income_vec) {
  # Define logical order
  income_order <- c(
    "If one person under $36k; two people under $41k; family of four under $52k",
    "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
    "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
    "If one person over $97k; two people over $111k; family of four over $139k"
  )
  
  # Match and order
  result <- factor(income_vec, levels = income_order, ordered = TRUE)
  return(result)
}

# Order education levels logically
order_education_levels <- function(edu_vec) {
  # Common education order (adjust based on actual values)
  # This is a placeholder - will need to check actual values
  return(edu_vec)  # Will update based on actual data
}

# Order age groups logically
order_age_groups <- function(age_vec) {
  # Define logical order
  age_order <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  result <- factor(age_vec, levels = age_order, ordered = TRUE)
  return(result)
}

# ============================================================================
# Index Calculation Functions
# ============================================================================

# Convert Likert scale to numeric (1-5)
likert_to_numeric <- function(x) {
  # Map all possible Likert responses to numeric values
  result <- rep(NA, length(x))
  
  # Strongly Disagree = 1
  result[x == "Strongly Disagree"] <- 1
  
  # Disagree = 2
  result[x == "Disagree"] <- 2
  
  # Neutral = 3 (if present)
  result[x == "Neutral"] <- 3
  
  # Agree = 4
  result[x == "Agree"] <- 4
  
  # Strongly Agree = 5
  result[x == "Strongly Agree"] <- 5
  
  return(as.numeric(result))
}

# Calculate Financial Wellbeing Index
# Average of: Optimism (Q18), Money Relationship (Q19), Low Stress (reverse Q20)
calculate_wellbeing_index <- function(pre_data) {
  if (nrow(pre_data) == 0) return(numeric(0))
  
  # All 5 wellbeing questions
  optimism_col <- grep("optimistic.*financial future", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  relationship_col <- grep("healthy relationship.*money", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  stress_col <- grep("stressed.*financ", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  confidence_col <- grep("confident.*plan", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  
  if (length(optimism_col) == 0 || length(relationship_col) == 0 || length(stress_col) == 0 ||
      length(confidence_col) == 0 || length(comfort_col) == 0) {
    return(rep(NA, nrow(pre_data)))
  }
  
  optimism <- likert_to_numeric(pre_data[[optimism_col[1]]])
  relationship <- likert_to_numeric(pre_data[[relationship_col[1]]])
  stress <- likert_to_numeric(pre_data[[stress_col[1]]])
  confidence <- likert_to_numeric(pre_data[[confidence_col[1]]])
  comfort <- likert_to_numeric(pre_data[[comfort_col[1]]])
  
  # Reverse stress (high stress = low wellbeing)
  stress_reversed <- 6 - stress  # 5 becomes 1, 1 becomes 5
  
  # Average of all 5 questions
  wellbeing <- rowMeans(cbind(optimism, relationship, stress_reversed, confidence, comfort), na.rm = TRUE)
  wellbeing[is.na(optimism) & is.na(relationship) & is.na(stress) & is.na(confidence) & is.na(comfort)] <- NA
  
  return(wellbeing)
}

# Calculate Self-Efficacy Index
# Average of: Planning Confidence (Q21), Professional Comfort (Q22)
calculate_efficacy_index <- function(pre_data) {
  if (nrow(pre_data) == 0) return(numeric(0))
  
  confidence_col <- grep("confident.*plan", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  comfort_col <- grep("comfortable.*speaking.*financial professional", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  
  if (length(confidence_col) == 0 || length(comfort_col) == 0) {
    return(rep(NA, nrow(pre_data)))
  }
  
  confidence <- likert_to_numeric(pre_data[[confidence_col[1]]])
  comfort <- likert_to_numeric(pre_data[[comfort_col[1]]])
  
  efficacy <- rowMeans(cbind(confidence, comfort), na.rm = TRUE)
  efficacy[is.na(confidence) & is.na(comfort)] <- NA
  
  return(efficacy)
}

# Calculate Behavioral Index
# Count of "Yes" responses to behavior questions (Q23-Q30)
calculate_behavioral_index <- function(pre_data) {
  if (nrow(pre_data) == 0) return(numeric(0))
  
  behavior_cols <- grep("Before today's workshop, I have", colnames(pre_data), ignore.case = TRUE, value = TRUE)
  
  if (length(behavior_cols) == 0) {
    return(rep(NA, nrow(pre_data)))
  }
  
  # Count "Yes" responses
  yes_count <- rowSums(pre_data[, behavior_cols, drop = FALSE] == "Yes", na.rm = TRUE)
  
  return(yes_count)
}

# ============================================================================
# Variable Mapping Table Data
# ============================================================================

get_variable_mapping_table <- function() {
  # Create comprehensive variable mapping table
  mapping <- data.frame(
    Question_Text = c(
      # Metadata (14 vars)
      "timestamp", "session_id", "org_name", "group", "facilitators", 
      "modules_taught", "session_date", "session_start_time", 
      "qa_metadata_changed", "qa_session_found", "respondent_id", 
      "is_anonymous", "respondent_name", "respondent_email",
      # Common questions (3 vars - Big Pre + Little Pre)
      "What is one intention you have for today's workshop?",
      "If you are arriving with any questions or curiosities, please share!",
      "How do you hope to feel at the end of today's workshop?",
      # Big Pre only - Financial Wellbeing (5 Likert)
      "How much do you agree with the following statements? [I feel optimistic about my financial future.]",
      "How much do you agree with the following statements? [I have a healthy relationship with money.]",
      "How much do you agree with the following statements? [I feel stressed about my finances.]",
      "How much do you agree with the following statements? [I feel confident I can plan ahead to make smart financial choices for myself.]",
      "How much do you agree with the following statements? [I feel comfortable speaking with financial professionals about my financial needs.]",
      # Big Pre only - Financial Behaviors (8 Yes/No/Maybe)
      "Before today's workshop, I have... [Set a financial goal]",
      "Before today's workshop, I have... [Tracked my spending for at least one week]",
      "Before today's workshop, I have... [Created or updated a personal budget]",
      "Before today's workshop, I have... [Contributed to a savings or investment account]",
      "Before today's workshop, I have... [Checked my credit score or credit report]",
      "Before today's workshop, I have... [Shared a financial tip or resource with someone in my life]",
      "Before today's workshop, I have... [Talked to someone I trust about money]",
      "Before today's workshop, I have... [Reflected on my thoughts and feelings about money]",
      # Demographics (9 + zip code)
      "Age Group",
      "Zip Code",
      "Race/Ethnicity (Select all that apply):",
      "Gender Identity:",
      "Household Income:",
      "What is the highest level of education you have completed? (Select one)",
      "First-Generation Status (College):",
      "First-Generation Status (U.S.):",
      "Veteran Status:",
      "Disability Status:",
      # Open text
      "Anything else you'd like to share with our team?"
    ),
    Gross_Category = c(
      rep("Metadata", 14),
      rep("Workshop Intentions", 3),
      rep("Financial Wellbeing", 5),
      rep("Financial Behaviors", 8),
      rep("Demographics", 10),
      "Additional Context"
    ),
    Fine_Category = c(
      # Metadata
      "Timestamp", "Session Link", "Organization", "Group", "Facilitators",
      "Content", "Date", "Time", "QA Flag", "QA Flag", "Identifier",
      "Privacy", "Name", "Contact",
      # Common questions
      "Intention", "Curiosities", "Hoped Feelings",
      # Financial Wellbeing
      "Future Outlook", "Money Relationship", "Financial Stress",
      "Planning Confidence", "Professional Comfort",
      # Financial Behaviors
      "Goal Setting", "Spending Awareness", "Budgeting", "Saving/Investing",
      "Credit Awareness", "Financial Sharing", "Financial Communication", "Financial Reflection",
      # Demographics
      "Age", "Location", "Race/Ethnicity", "Gender", "Income",
      "Education", "First-Gen College", "First-Gen U.S.", "Veteran", "Disability",
      # Open text
      "Open Feedback"
    ),
    Response_Type = c(
      rep("Metadata", 14),
      rep("Open Text", 3),
      rep("Likert (5-point)", 5),
      rep("Ternary (Yes/No/Maybe)", 8),
      c("Categorical", "Text", "Multi-select Categorical", "Categorical", "Ordinal Categorical",
        "Ordinal Categorical", "Binary", "Binary", "Binary", "Binary"),
      "Open Text"
    ),
    Survey_Type = c(
      rep("All Surveys", 14),
      rep("Big Pre + Little Pre", 3),
      rep("Big Pre Only", 24)
    ),
    stringsAsFactors = FALSE
  )
  
  return(mapping)
}

