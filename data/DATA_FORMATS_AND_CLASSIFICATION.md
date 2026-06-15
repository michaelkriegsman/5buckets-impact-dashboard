# Survey data formats & dashboard classification

## Big Pre / Big Post vs Little Pre / Little Post

The dashboard classifies rows in `identify_survey_type()` (`data/process_data.R`) using **Program Manager** joined on `session_id`:

| Classification | Rule |
|----------------|------|
| **Big Pre** | `sessions_in_series == 1` (single-session series), **or** `session_number == 1` with both PM fields present. |
| **Big Post** | `sessions_in_series == 1`, **or** `session_number == sessions_in_series` with both PM fields present. |
| **Little Pre / Post** | Everything else, including rows with **no PM match** (NA `session_number` or `sessions_in_series` after join). |

**Important:** Rows without a valid PM join are **not** treated as Big Pre/Post. That keeps Little surveys out of **past behaviors** (Big Pre only) and **planned behaviors** (Big Post only). If Master shows “all 8” planned actions for everyone on Big Post, check (1) **grid → Master** logic: the single aggregated column must list only `[Label]` texts where the form answer was **`Yes!!`** (not `Maybe...` / `Nope`); see `Pre_Survey_Handler_Feb2026.js`, `Post_Survey_Handler_Feb2026.js`, and `Repair_Master_Past_Planned_Aggregates_Feb2026.js`; (2) that every post row has a `session_id` that exists in Program Manager with correct `session_number` and `sessions_in_series`.

When `program_manager_data` is **empty**, the function still defaults all rows to Big Pre and Big Post (legacy behavior for files without PM).

## Planned series length (Len1–Len6, heatmaps, org tables)

- **`calculate_organization_journeys()`** uses, per org+group series: `max(sessions_in_series)` from PM where values are present &gt; 0; if none, it falls back to the **number of session rows** in Master for that series.
- **Overview → Responses by Series** matrices use the same rule (see `journey_response_matrices` in `app_feb2026.R`), capped at 6 for bucketing.

So a group with **one** Master row but PM `sessions_in_series = 6` counts as **length 6**, not 1.

## Behavior / index columns

Column names and parsing live in `question_mapping_feb2026.R`, `big_pre_analysis_feb2026.R`, and `big_post_analysis_feb2026.R`. Mid-series question text or option-list changes can shift comma-separated values or column names; ported historical rows may not match new parsers—spot-check orgs like Arise in Master vs dashboard counts when numbers look wrong.
