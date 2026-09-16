# Data Map enrichment helpers (column matrix, income, joins, shapes)

.export_income_order <- function() {
  if (exists("INCOME_ORDER") && length(INCOME_ORDER)) return(as.character(INCOME_ORDER))
  c(
    "If one person under $36k; two people under $41k; family of four under $52k",
    "If one person $36k-$60k; two people $41k-$69k; family of four $52k-$87k",
    "If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k",
    "If one person over $97k; two people over $111k; family of four over $139k"
  )
}

.export_income_short_labels <- function() {
  if (exists("INCOME_SHORT_LABELS") && length(INCOME_SHORT_LABELS)) {
    return(INCOME_SHORT_LABELS)
  }
  setNames(
    c("<$52k", "$52k-$87k", "$87k-$139k", ">$139k"),
    .export_income_order()
  )
}

.export_education_order <- function() {
  if (exists("EDUCATION_ORDER") && length(EDUCATION_ORDER)) return(as.character(EDUCATION_ORDER))
  c(
    "Middle school",
    "Some high school",
    "High school diploma or equivalent (GED)",
    "Trade school or technical certificate",
    "Some college, no degree",
    "Associate's degree",
    "Bachelor's degree",
    "Master's degree",
    "Professional degree (JD, MD, etc.)",
    "Doctorate (PhD, EdD, etc.)"
  )
}

# Design + empirical analysis matrix for ZIP CSVs.
.export_column_analysis_matrix_md <- function() {
  paste(
    c(
      "Master Pre/Post CSVs are **wide** (many question columns appear on both Big and Little files).",
      "Sparse non-null values on Little rows are **not** a license to analyze Big-only constructs there.",
      "",
      "Legend: **USE** = correct universe; **IGNORE** = may exist / sparse — do not use; **ABSENT** = not applicable.",
      "",
      "| Analytic construct | `pre_all` | `little_pre` | `big_pre` | `post_all` | `little_post` | `big_post` | `annual` |",
      "| --- | --- | --- | --- | --- | --- | --- | --- |",
      "| Typing flags `is_big_pre` / `is_big_post` | USE | USE | USE | USE | USE | USE | ABSENT |",
      "| Session keys (`session_id`, `org_name`, `group`, `modules_taught`) | USE | USE | USE | USE | USE | USE | ABSENT (host org text instead) |",
      "| Financial Wellness Likert (8 Pre items) | IGNORE | IGNORE | **USE** | ABSENT | ABSENT | ABSENT | ABSENT |",
      "| Past behaviors multi-select | IGNORE | IGNORE | **USE** | ABSENT | ABSENT | ABSENT | ABSENT |",
      "| Pre opening open-text | USE | USE | USE | ABSENT | ABSENT | ABSENT | ABSENT |",
      "| Demographics (age/gender/income/…) | sparse | IGNORE | **USE** | sparse | IGNORE | **USE** | **USE** |",
      "| Session satisfaction (1–6) | ABSENT | ABSENT | ABSENT | USE | **USE** | **USE** | ABSENT |",
      "| Post learning open-text (stood out / apply / helpful) | ABSENT | ABSENT | ABSENT | USE | **USE** | **USE** | ABSENT |",
      "| Compared-to-before Likert (Post Master headers) | ABSENT | ABSENT | ABSENT | IGNORE | IGNORE | **USE** | ABSENT |",
      "| Planned actions multi-select | ABSENT | ABSENT | ABSENT | IGNORE | IGNORE | **USE** | ABSENT |",
      "| NPS recommend 0–10 | ABSENT | ABSENT | ABSENT | sparse | sparse | **USE** | **USE** (same question family) |",
      "| Impact story open-text | ABSENT | ABSENT | ABSENT | sparse | sparse | **USE** | **USE** |",
      "| Keep-in-touch multi-select | ABSENT | ABSENT | ABSENT | sparse | sparse | **USE** | pattern match |",
      "| Annual Compared-to grid (Annual headers — wording differs) | ABSENT | ABSENT | ABSENT | ABSENT | ABSENT | ABSENT | **USE** |",
      "| `respondent_id` joins | USE | USE | USE | USE | USE | USE | **USE** |",
      "",
      "**Empirical note (full export):** wellness/past-behavior columns can show ~10% fill on `little_pre` and",
      "compared-to/planned ~6% on `little_post`. Treat those as leakage / mis-forms — still **IGNORE**;",
      "restrict indices to `big_pre` / `big_post` where fill is typically >90%.",
      "",
      "**Annual header trap:** Annual Compared-to columns are *not* identical strings to Post Master.",
      "They often include spaces around `...` and slight wording drift (e.g. \"understanding **about**\" vs \"understanding **of**\").",
      "Resolve Annual columns with `ANNUAL_COL_PATTERNS` / `annual_find_col()` — never copy-paste Post Master headers onto `annual.csv`."
    ),
    collapse = "\n"
  )
}

.export_income_bands_md <- function() {
  bands <- .export_income_order()
  shorts <- .export_income_short_labels()
  rows <- paste0(
    "| `", bands, "` | ",
    ifelse(bands %in% names(shorts), unname(shorts[bands]), "_(no short)_"),
    " |"
  )
  paste(
    c(
      "Household Income is a **single-choice categorical** field. Values are long multi-person band strings.",
      "**Never invent dollar cutpoints or rebin** beyond the official short labels below.",
      "Match on the **full string** (trim whitespace); do not parse the dollars.",
      "",
      "| Full value (exact) | Chart short label |",
      "| --- | --- |",
      rows,
      "",
      "Order low → high as listed. Unknown / free-text extras: keep separate; do not coerce into a band."
    ),
    collapse = "\n"
  )
}

.export_join_recipes_md <- function() {
  past <- if (exists("PRE_PAST_BEHAVIORS_COL")) PRE_PAST_BEHAVIORS_COL else "Past behaviors col"
  planned <- if (exists("POST_PLANNED_ACTIONS_COL")) POST_PLANNED_ACTIONS_COL else "Planned actions col"
  paste(
    c(
      "Follow these recipes exactly. Prefer Results MD tables when present; use CSV only for custom cuts.",
      "",
      "### Recipe A — Paired Pre→Post people (within-person)",
      "",
      "1. Load `big_pre.csv` and `big_post.csv`.",
      "2. Keep rows with non-empty `respondent_id` not starting with `ANON`.",
      "3. Inner-join on `respondent_id` (person grain). If multiple rows/ID, take latest `session_date` or dedupe explicitly and report rule.",
      "4. For concept keys in §9, read Pre absolute Likert and Post compared-to Likert separately.",
      "5. **Do not** subtract Pre mean from Post mean as a shared scale. Report N pairs, Pre distribution, Post perceived-change distribution,",
      "   and (optional) within-person ordinal movement with the caveat that items are not identical.",
      "",
      "### Recipe B — Annual continuers",
      "",
      "1. Load `big_pre.csv` (or `big_post.csv`) and `annual.csv`.",
      "2. Inner-join on `respondent_id` (email already hashed; **no email in ZIP**).",
      "3. Resolve Annual Compared-to / behaviors / recommend via header patterns (§12), not Post Master names.",
      "4. Report continuer N and outcomes; do not claim representativeness of all workshop attendees.",
      "",
      "### Recipe C — Session satisfaction by modules taught",
      "",
      "1. Use `little_post.csv` **and** `big_post.csv` (or `post_all.csv`). Satisfaction is mid-series relevant.",
      "2. Column: `How satisfied were you with today's 5 Buckets session?` (1–6).",
      "3. `modules_taught` is often pipe-joined (`Mindset|Manage`). Split on `|` to atoms; for \"mean by module\",",
      "   explode rows so a session teaching Grow+Protect contributes to both module means (document that choice).",
      "4. Report N responses per module atom.",
      "",
      "### Recipe D — Behavioral readiness (past → planned)",
      "",
      paste0("1. Past index: `big_pre.csv` column `", past, "`."),
      paste0("2. Planned index: `big_post.csv` column `", planned, "`."),
      "3. Parse: split commas → drop lone `Yes!!`/`Maybe...`/`Nope` tokens → rematch to canonical labels → count distinct (0–8).",
      "4. Cross-section: compare distributions. Paired: Recipe A IDs only.",
      "",
      "### Recipe E — NPS",
      "",
      "1. Primary: `big_post.csv` recommend column (0–10).",
      "2. Promoters 9–10; passives 7–8; detractors ≤6; NPS = %promoters − %detractors.",
      "3. Annual recommend uses the same banding after locating the column via patterns.",
      "",
      "### Recipe F — Reach demographics",
      "",
      "1. Prefer `big_pre.csv` (and optionally `big_post.csv` / `annual.csv`).",
      "2. Do not use `little_*` for demographic distributions (mostly empty).",
      "3. Income: exact full strings (§ income bands). Education: normalize to education order. Race: multi-select — rematch canonical labels.",
      "",
      "### Anti-joins (forbidden)",
      "",
      "- Join Annual on name/email (stripped).",
      "- Join Pre/Post only on `session_id` and call it person-level change.",
      "- Union `pre_all`+`post_all` and compute a single \"wellness\" column."
    ),
    collapse = "\n"
  )
}

.export_example_shapes_md <- function() {
  paste(
    c(
      "Illustrative cell shapes (anonymized patterns from real Master exports):",
      "",
      "| Field | Example shape |",
      "| --- | --- |",
      "| `modules_taught` | `Mindset\\|Manage` (pipe-joined atoms) |",
      "| Past behaviors | `Contributed to a savings or investment account, Talked to someone I trust about money, Reflected on my thoughts and feelings about money` |",
      "| Household Income | `If one person $60k-$97k; two people $69-$111k; family of four $87k-$139k` |",
      "| Race/Ethnicity | single label **or** comma-joined multi-select |",
      "| Likert cell | `Agree` / `Strongly Agree` / `Disagree` / `Strongly Disagree` (not numeric in raw CSV) |",
      "| Open text bilingual | `texto original (%-^-%) [es language detected] English translation` |",
      "| `respondent_id` | 64-char hex SHA-256; or `ANON…` if unpaired |",
      "",
      "Raw Likert is **text**. Convert with the fixed map in §8 before means."
    ),
    collapse = "\n"
  )
}
