# text_analysis.R - Text analysis for open-ended responses (delimiter-aware)

source("data/open_text_parse.R")

HAS_TM <- requireNamespace("tm", quietly = TRUE)
HAS_SENTIMENT <- requireNamespace("SentimentAnalysis", quietly = TRUE)

filter_nonresponses <- function(text_vec) {
  text_vec <- ensure_text_vector(text_vec)
  if (length(text_vec) == 0) return(text_vec)
  text_trim <- trimws(text_vec)
  text_lower <- tolower(text_trim)
  nonresponses <- c(
    "no", "nope", "nah", "none", "nothing", "n/a", "na", "—", "-", "--",
    "not really", "nothing really", "nothing to add", "no additional", "no comment", "nothing else",
    "nil", "null", "same", "idk", "none.", "n/a.", "na."
  )
  keep <- !text_lower %in% nonresponses &
    !grepl("^(no|nope|nah|none|nothing|n/a|na|nil)$", text_lower, ignore.case = TRUE) &
    !grepl("^(no|nope)\\s*\\.?$", text_lower, ignore.case = TRUE)
  nch <- nchar(text_trim)
  keep <- keep & (nch >= 2 | text_lower %in% c("ok", "ty"))
  text_vec[keep & !is.na(text_vec) & text_trim != ""]
}

prepare_text_for_analysis <- function(text_vec, lang_mode = c("all", "english")) {
  lang_mode <- match.arg(lang_mode)
  vec <- open_text_for_mode(text_vec, lang_mode)
  filter_nonresponses(vec)
}

# Domain / filler words that swamp the clouds without adding meaning. Applied in
# BOTH "all" and "english" modes (the brand name, generic verbs, survey scaffolding).
WORDCLOUD_DOMAIN_STOPWORDS <- c(
  "buckets", "bucket", "workshop", "workshops", "session", "sessions", "class", "classes",
  "today", "todays", "program", "programs", "course", "thing", "things", "stuff",
  "really", "just", "like", "lot", "lots", "get", "got", "getting", "one", "also",
  "make", "made", "making", "take", "took", "want", "wanted", "know", "knowing",
  "going", "able", "use", "used", "using", "day", "time", "way", "ways", "feel",
  "5buckets", "buckets5",
  # Spanish filler / brand scaffolding
  "muy", "mucho", "mucha", "cosa", "cosas", "hoy", "taller", "clase", "puede", "hacer", "ahora"
)

# Pragmatic Spanish stopword fallback if tm has no Spanish list installed.
WORDCLOUD_SPANISH_STOPWORDS_FALLBACK <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas", "de", "del", "y", "o", "que",
  "en", "a", "con", "por", "para", "es", "son", "se", "su", "sus", "mi", "mis", "me",
  "lo", "le", "les", "no", "si", "mas", "pero", "como", "este", "esta", "estos", "estas",
  "al", "ya", "muy", "yo", "tu", "te", "nos", "ha", "han", "fue", "ser", "soy", "esto"
)

create_word_freq <- function(text_vec, lang_mode = c("all", "english")) {
  text_vec <- prepare_text_for_analysis(text_vec, lang_mode)
  if (length(text_vec) == 0) {
    return(data.frame(word = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  if (!HAS_TM) {
    warning("tm package not available.")
    return(data.frame(word = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  text <- paste(text_vec, collapse = " ")
  corpus <- tm::Corpus(tm::VectorSource(text))
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  # Multilingual stopword removal in BOTH modes (English + Spanish), plus domain words.
  stop_words <- character(0)
  tryCatch({ stop_words <- c(stop_words, tm::stopwords("english")) }, error = function(e) NULL)
  spanish_sw <- tryCatch(tm::stopwords("spanish"), error = function(e) character(0))
  if (!length(spanish_sw)) spanish_sw <- WORDCLOUD_SPANISH_STOPWORDS_FALLBACK
  stop_words <- unique(c(stop_words, spanish_sw, WORDCLOUD_DOMAIN_STOPWORDS))
  tryCatch({
    corpus <- tm::tm_map(corpus, tm::removeWords, stop_words)
  }, error = function(e) NULL)
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  # Drop 1-2 character tokens (mostly noise) while keeping multi-char CJK tokens.
  tdm <- tm::TermDocumentMatrix(corpus, control = list(wordLengths = c(3, Inf)))
  matrix <- as.matrix(tdm)
  if (length(matrix) == 0) {
    return(data.frame(word = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  word_freq <- sort(rowSums(matrix), decreasing = TRUE)
  data.frame(
    word = names(word_freq),
    freq = as.numeric(word_freq),
    stringsAsFactors = FALSE
  )
}

# Language-balanced frequencies for the "All languages" cloud. Groups responses by
# detected language (English vs each non-English bucket), computes word frequencies
# per bucket, normalizes each bucket to a common top weight, then merges — so a few
# dozen English responses don't completely drown out Spanish / Chinese words.
# Partial rebalancing (top-K per language), not full equalization.
balanced_word_freq_all <- function(raw_text_vec, top_k = 30L) {
  cells <- ensure_text_vector(raw_text_vec)
  if (!length(cells)) return(data.frame(word = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  buckets <- list()
  for (x in cells) {
    sp <- split_open_text_cell(x)
    orig <- sp$original
    if (is.na(orig) || !nzchar(trimws(orig))) next
    lang <- if (!isTRUE(sp$has_delim)) {
      "en"
    } else {
      nt <- sp$detected_lang_note
      if (is.null(nt) || is.na(nt) || !nzchar(nt)) "other" else tolower(trimws(nt))
    }
    buckets[[lang]] <- c(buckets[[lang]], orig)
  }
  if (!length(buckets)) return(create_word_freq(raw_text_vec, lang_mode = "all"))
  parts <- list()
  for (lang in names(buckets)) {
    wf <- create_word_freq(buckets[[lang]], lang_mode = "all")
    if (is.null(wf) || nrow(wf) == 0) next
    wf <- utils::head(wf, top_k)
    mx <- max(wf$freq)
    if (is.finite(mx) && mx > 0) wf$freq <- wf$freq / mx * 100
    parts[[lang]] <- wf
  }
  if (!length(parts)) return(create_word_freq(raw_text_vec, lang_mode = "all"))
  merged <- do.call(rbind, parts)
  agg <- stats::aggregate(freq ~ word, data = merged, FUN = max)
  agg[order(-agg$freq), , drop = FALSE]
}

calculate_sentiment <- function(text_vec, lang_mode = "english") {
  text_vec <- prepare_text_for_analysis(text_vec, lang_mode)
  total_responses <- length(text_vec)
  if (total_responses == 0) {
    return(list(score = 0, sentiment = "Neutral", positive = 0, negative = 0, neutral = 0, total = 0, na_count = 0))
  }
  if (!HAS_SENTIMENT) {
    return(list(score = 0, sentiment = "Not Available", positive = 0, negative = 0, neutral = 0, total = total_responses, na_count = 0))
  }
  sentiment <- SentimentAnalysis::analyzeSentiment(text_vec)
  scores <- sentiment$SentimentGI
  avg_score <- mean(scores, na.rm = TRUE)
  positive_count <- sum(scores > 0.1, na.rm = TRUE)
  negative_count <- sum(scores < -0.1, na.rm = TRUE)
  neutral_count <- sum(scores >= -0.1 & scores <= 0.1, na.rm = TRUE)
  overall_sentiment <- if (avg_score > 0.1) "Positive" else if (avg_score < -0.1) "Negative" else "Neutral"
  list(
    score = avg_score,
    sentiment = overall_sentiment,
    positive = positive_count,
    negative = negative_count,
    neutral = neutral_count,
    total = total_responses,
    na_count = 0L
  )
}

sentiment_units_blurb <- function() {
  paste0(
    "<p style=\"font-size:11px;color:#5f6369;margin-top:10px;margin-bottom:0;\">",
    "<strong>About these scores:</strong> ",
    "Sentiment uses the General Inquirer lexicon (SentimentGI). ",
    "Open-text sentiment uses the <strong>English</strong> side when responses include the ",
    "<code>", OPEN_TEXT_DELIM, "</code> bilingual delimiter.",
    "</p>"
  )
}

sentiment_panel_html <- function(text_vec, include_lexicon_blurb = FALSE, lang_mode = "english") {
  bi <- open_text_bilingual_summary(text_vec)
  sentiment <- calculate_sentiment(text_vec, lang_mode = lang_mode)
  analyzed_count <- sentiment$total
  en_note <- if (bi$with_delim > 0) {
    paste0(" <em>(", bi$with_delim, " bilingual / ", bi$monolingual, " monolingual)</em>")
  } else ""
  body <- paste0(
    "<h5 style=\"margin-top:0;\">Sentiment (English)</h5>",
    "<p><strong>Mean SentimentGI:</strong> ", sprintf("%.3f", sentiment$score), " (", sentiment$sentiment, ")</p>",
    if (analyzed_count > 0) {
      paste0(
        "<p><strong>Positive:</strong> ", sentiment$positive,
        " &nbsp; <strong>Neutral:</strong> ", sentiment$neutral,
        " &nbsp; <strong>Negative:</strong> ", sentiment$negative, "</p>"
      )
    } else "<p><em>No scored responses.</em></p>",
    "<p style=\"font-size:12px;color:#5f6369;\"><strong>Retained:</strong> ", analyzed_count, en_note, "</p>"
  )
  if (isTRUE(include_lexicon_blurb)) body <- paste0(body, sentiment_units_blurb())
  body
}
