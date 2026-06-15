# UI modules (v3 refactor)

`app.R` still contains the full UI for stability. Extract tab-specific UI here incrementally:

- `ui_sidebar.R` — sidebar filters (planned)
- `ui_impact.R` — Impact-centric `tabsetPanel` (planned)
- `ui_survey.R` — Survey-centric tabs (planned)

Run locally: `Rscript run_app.R` → http://127.0.0.1:3838
