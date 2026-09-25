# export_canonicalize.R — Part 3 canonicalization for Reach / Report exports
#
# Apply before publishing demographic or behavior percentages so EN/ES/ZH
# and case variants do not silently split counts.

.canon_lower_trim <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  tolower(gsub("[[:space:]]+", " ", x))
}

# HUD-style short income bands (Bay Area labels compressed to four levels).
INCOME_BAND_SHORT <- c(
  "under_30" = "Under 30% AMI",
  "30_50" = "30–50% AMI",
  "50_80" = "50–80% AMI",
  "over_80" = "Over 80% AMI",
  "pna" = "Prefer not to answer"
)

#' Map a raw household-income cell to a short band label (or NA).
canonicalize_income_band <- function(x) {
  raw <- as.character(x)
  key <- .canon_lower_trim(raw)
  out <- rep(NA_character_, length(key))
  # Prefer-not
  pna <- grepl("prefer not|prefer-not|no answer|no contesto|不愿", key)
  out[pna] <- INCOME_BAND_SHORT[["pna"]]
  # Very low / under 30% — "under $36k" style / "under 30"
  under <- !pna & (
    grepl("under \\$36|under 36|menos de \\$36|低于|under 30%|below 30", key) |
      grepl("if one person under", key)
  )
  # Ambiguous strings that include under-30 phrasing as first band
  under <- under | (!pna & grepl("one person under \\$36", key) & !grepl("\\$36k-\\$60|\\$36-\\$60", key))
  out[under] <- INCOME_BAND_SHORT[["under_30"]]
  # 30-50
  b30 <- is.na(out) & grepl("\\$36k-\\$60|\\$36-\\$60|36k-60|30–50|30-50%|30-50 of", key)
  out[b30] <- INCOME_BAND_SHORT[["30_50"]]
  # 50-80
  b50 <- is.na(out) & grepl("\\$60k-\\$97|\\$60-\\$97|60k-97|50–80|50-80%", key)
  out[b50] <- INCOME_BAND_SHORT[["50_80"]]
  # over 80
  over <- is.na(out) & grepl("over \\$97|over 97|above 80|over 80%|more than \\$97", key)
  out[over] <- INCOME_BAND_SHORT[["over_80"]]
  # Fallback: if string still empty / unknown leave NA
  out[!nzchar(key)] <- NA_character_
  out
}

#' LMI = under 30 + 30-50 + 50-80; very low = under 30 + 30-50. Excludes PNA from denom.
income_composite_shares <- function(bands) {
  b <- as.character(bands)
  b <- b[!is.na(b) & b != INCOME_BAND_SHORT[["pna"]]]
  n <- length(b)
  if (!n) {
    return(list(n = 0L, lmi_pct = NA_real_, very_low_income_pct = NA_real_))
  }
  lmi <- b %in% INCOME_BAND_SHORT[c("under_30", "30_50", "50_80")]
  vl <- b %in% INCOME_BAND_SHORT[c("under_30", "30_50")]
  list(
    n = as.integer(n),
    lmi_pct = round(100 * mean(lmi), 1),
    very_low_income_pct = round(100 * mean(vl), 1)
  )
}

canonicalize_gender <- function(x) {
  k <- .canon_lower_trim(x)
  out <- trimws(as.character(x))
  out[k == "male" | k == "hombre" | k == "m"] <- "Male"
  out[k == "female" | k == "mujer" | k == "f"] <- "Female"
  out[k == "non-binary" | k == "nonbinary" | k == "non binary"] <- "Non-binary"
  out[grepl("prefer not", k)] <- "Prefer not to answer"
  out[!nzchar(k)] <- NA_character_
  out
}

canonicalize_education <- function(x) {
  # Prefer existing dashboard normalizer when present
  if (exists(".normalize_education_for_dashboard", mode = "function")) {
    return(.normalize_education_for_dashboard(x))
  }
  k <- .canon_lower_trim(x)
  out <- trimws(as.character(x))
  out[k == "some high school"] <- "Some high school"
  out[k == "high school diploma or ged" | k == "high school"] <- "High school diploma or GED"
  out[k == "associate degree"] <- "Associate degree"
  out[!nzchar(k)] <- NA_character_
  out
}

#' People of color: any race mention except white-only (and PNA).
#' @param race_col character vector (may be multi-select comma-separated)
poc_and_hispanic_shares <- function(race_col) {
  raw <- trimws(as.character(race_col))
  raw[is.na(raw)] <- ""
  pna <- grepl("prefer not", raw, ignore.case = TRUE) | !nzchar(raw)
  usable <- raw[!pna]
  n <- length(usable)
  if (!n) {
    return(list(n = 0L, poc_pct = NA_real_, hispanic_latino_pct = NA_real_))
  }
  k <- tolower(usable)
  white_only <- grepl("^white", k) & !grepl(",", k) &
    !grepl("hispanic|latina|latino|black|asian|pacific|native|indigenous|middle eastern|other", k)
  hispanic <- grepl("hispanic|latina|latino|latinx", k)
  poc <- !white_only
  list(
    n = as.integer(n),
    poc_pct = round(100 * mean(poc), 1),
    hispanic_latino_pct = round(100 * mean(hispanic), 1)
  )
}

#' Youth 12-24 share from age-group labels.
youth_12_24_share <- function(age_col) {
  raw <- trimws(as.character(age_col))
  raw <- raw[!is.na(raw) & nzchar(raw) & !grepl("prefer not", raw, ignore.case = TRUE)]
  n <- length(raw)
  if (!n) return(list(n = 0L, youth_12_24_pct = NA_real_))
  k <- tolower(raw)
  youth <- grepl("12-17|13-17|14-17|16-17|16-18|18-24|12-24", k) |
    grepl("^under 18|^18$|teen", k)
  list(n = as.integer(n), youth_12_24_pct = round(100 * mean(youth), 1))
}

# Partner-group lookup (extend as partners are classified). Unmapped → NA (reported, not "Other").
PARTNER_GROUP_LOOKUP <- c(
  "Mercy Housing" = "housing",
  "Giants Community Fund" = "schools",
  "ARISE High School" = "schools",
  "Children's Home Society" = "schools",
  "San Mateo PAL" = "workforce",
  "ICA Cristo Rey Academy" = "schools",
  "Cristo Rey De La Salle" = "schools",
  "Fall Financial Wellness Series" = "open_workshops",
  "Spring Financial Wellness Series" = "open_workshops",
  "SD Mayer Summer Series" = "schools"
)

partner_group_for_org <- function(org) {
  o <- trimws(as.character(org))
  unname(PARTNER_GROUP_LOOKUP[o])
}

#' Fiscal year label for a Date (Jul 1 – Jun 30). FY2026 = 2025-07-01 … 2026-06-30.
fiscal_year_label <- function(dates) {
  d <- as.Date(dates)
  yr <- as.integer(format(d, "%Y"))
  mo <- as.integer(format(d, "%m"))
  fy <- ifelse(!is.na(mo) & mo >= 7L, yr + 1L, yr)
  ifelse(is.na(fy), NA_character_, paste0("FY", fy))
}

SUPPRESSION_N_DEFAULT <- 10L

suppress_pct <- function(n, N, threshold = SUPPRESSION_N_DEFAULT) {
  n <- as.integer(n)
  N <- as.integer(N)
  ifelse(is.na(N) | N < threshold, NA_real_, round(100 * n / pmax(N, 1L), 1))
}
