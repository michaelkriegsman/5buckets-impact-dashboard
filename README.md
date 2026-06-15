# 5 Buckets Impact Dashboard

Shiny web application for analyzing 5 Buckets workshop survey data.

## Quick Start

### 1. Install Packages
```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
  "googlesheets4", "googledrive", "lubridate", "digest", "jsonlite",
  "wordcloud2", "tm", "SentimentAnalysis"
))
```

### 2. Set Up Google Sheets Authentication

**Option A: OAuth (Development)**
```r
library(googlesheets4)
gs4_auth()  # Run once, opens browser
```

**Option B: Service Account (Production)**
1. Create service account in Google Cloud Console
2. Download JSON key
3. Share Google Sheets with service account email
4. Set environment variable: `GOOGLE_SERVICE_ACCOUNT_KEY=/path/to/key.json`

### 3. Set Sheet IDs

**Get Sheet IDs from Google Sheets URLs:**
- Format: `https://docs.google.com/spreadsheets/d/[SHEET_ID]/edit`

**Set in `global.R` or as environment variables:**
```r
MASTER_PRE_SHEET_ID <- "your-sheet-id-here"
MASTER_POST_SHEET_ID <- "your-sheet-id-here"
PROGRAM_MANAGER_SHEET_ID <- "your-sheet-id-here"
```

### 4. Run Locally
```bash
chmod +x run_local.sh
./run_local.sh
```

Access: `http://127.0.0.1:3838`

```bash
Rscript run_app.R
# or
./run_local.sh
```

**Naming:** current app is `app.R`; retired Jan 2026 shell is `app_2026-01.R`. Archive notes: `FEB2026_DASHBOARD_README_2026-02.md`.

**Client production deploy:** `DEPLOY_CLIENT_POSIT.md` (shinyapps.io Standard + service account + optional custom domain).

### 5. Deploy to shinyapps.io

```r
library(rsconnect)
rsconnect::setAccountInfo(
  name = "your-account-name",
  token = "your-token",
  secret = "your-secret"
)

setwd("Impact Dashboard")  # or full path to this folder
deployApp(appName = "5buckets-impact-dashboard")
```

**Set Environment Variables in shinyapps.io dashboard:**
- `MASTER_PRE_SHEET_ID`
- `MASTER_POST_SHEET_ID`
- `PROGRAM_MANAGER_SHEET_ID`

## Project Structure

```
Impact Dashboard/
├── app.R                 # Main Shiny app (v3)
├── load_sources.R        # Sources global.R + data + modules
├── global.R              # Master/PM load, normalization
├── run_app.R             # Local launcher (port 3838)
├── modules/favorites.R   # localStorage favorites + partner-report stub
├── server/               # Extracted server helpers (pairing, register)
├── ui/                   # UI extraction (incremental refactor)
├── data/                 # Analysis, open-text parse, timestamps
└── www/                  # favorites.js (browser persistence)
```

## Data Structure Hierarchy

The dashboard analyzes data at multiple levels:

```
Organization
  └── Group (optional - some orgs have no groups)
      └── Journey (series of sessions)
          └── Session (individual workshop)
              ├── Big Pre Survey (first session or single session)
              ├── Little Pre Survey (middle sessions)
              ├── Little Post Survey (middle sessions)
              └── Big Post Survey (last session or single session)
                  └── Individual Responses
                      └── Variables (questions, demographics, etc.)
```

**Key Concepts**:
- **Journey**: A complete series of sessions (e.g., 1/3, 2/3, 3/3)
- **Big Pre/Post**: First and last surveys in a series (or only survey in single-session workshops)
- **Little Pre/Post**: Middle surveys in multi-session series
- **Session**: Individual workshop event with its own pre/post surveys
- **Group**: Subdivision within an organization (optional - some orgs have no groups)

## Brand Colors

**Primary:**
- Purple: `#5c2f92`
- Green: `#82c341`
- Gray: `#797d82`

**Secondary (use 30% or less):**
- Orange: `#f58220`
- Yellow: `#fdb71a`
- Blue: `#0076be`
- Gray: `#5f6369`

## File naming convention (2026 onward)

- **Current / in-use files have no date suffix.** When a file is superseded, **rename the old file with a date suffix** (e.g. `app_2026-02.R`) and let the un-suffixed name carry the current build.
- Reason: stale dated names like `*_feb2026.R` make active code look out of date months later. Going forward, dates only appear when something has been retired.
- **Pending rename (do later, when stable):** `app_feb2026.R` → `app.R`, `run_app_feb2026.R` → `run_app.R`, `data/question_mapping_feb2026.R` → `data/question_mapping.R`, etc. The current `app.R` (older Big Pre / Big Post / Within Session shell) should be renamed to `app_2026-01.R` first to free the name.

## Current Status

### ✅ Completed

**Milestone 1: Project Setup & CoachDashboard Review** ✅
- Created project structure (`Impact Dashboard/` folder)
- Reviewed CoachDashboard patterns from Still Yet Moving project
- Set up basic file structure (app.R, global.R, data/, modules/, utils/)

**Milestone 2: Data Connection & Basic App Shell** ✅
- Google Sheets authentication configured (OAuth via googlesheets4)
- Sheet IDs configured from existing Apps Scripts:
  - Master Pre: `1Ae12Ikl15shjan-40GDj-OLUXgJB-IXtHd0CT1Eb8Eo` (Pre Submissions)
  - Master Post: `1xEzJf8CX9vZmAzMZW9EQ9dYYCvGjG7h8tQ_3OcJ4ig0` (Post Submissions)
  - Program Manager: `14I2_rMR4Ny7-2MkD535vX9Pyt4AlX0AhAMAlUboiJ6M` (Workshops)
- Data loading functions implemented
- 5 Buckets brand colors applied
- Google Sheets hyperlinks added

**Milestone 3: Session Summary Tab** ✅
- Session-level data aggregation implemented
- Summary statistics table (total sessions, response counts, averages)
- "By Organization" table showing journey counts by length (Len1-Len6)
- Filtered summary stats linked to session details table
- Interactive session details table (sortable, filterable)
- Data processing functions in `data/process_data.R`

**Milestone 4: Tab Structure & Filtering** ✅
- Reorganized tabs: Session Summary, Big Pre, Big Post, Pairs, Within Session
- Filtering by organization and group (not by individual session for tabs 1-4)
- Survey type identification (big vs little pre/post) based on session position
- Journey statistics functions implemented

**Milestone 5: Big Pre & Big Post Tabs** ✅
- Big Pre tab with summary statistics (total, with user_id, without user_id)
- Big Post tab with summary statistics (total, with user_id, without user_id)
- Filtering by organization and group across multiple sessions
- Survey type classification logic

**Milestone 6: Within Session Tab** ✅
- Organization and group selection (group is optional for orgs without groups)
- Journey detection for selected org/group combination
- Journey info display (sessions in series, total sessions, response counts)
- Response flow plot (line chart showing pre/post responses over time)
- Session details table
- Handles organizations without groups (e.g., "Fall Financial")

### ⏳ Next Phase: Big Post Survey Implementation (TOP PRIORITY)

**Goal**: Implement comprehensive Big Post survey analysis, mirroring the Big Pre tab structure

**Status**: Big Pre tab is complete with full visualizations. Big Post tab currently has only summary statistics.

**Implementation Plan**: See `POST_SURVEY_IMPLEMENTATION_PLAN.md` for detailed roadmap.

**Priority Tasks**:
1. **Map Big Post Variables** (Phase 1)
   - Inspect post-survey data structure
   - Document all questions, response types, and semantic categories
   - Create `POST_VARIABLE_MAPPING.md`

2. **Create Analysis Functions** (Phase 2)
   - Build `data/big_post_analysis.R` with functions for:
     - Financial Wellbeing & Impact
     - Workshop Quality & Experience
     - New Behaviors
     - Demographics (if applicable)

3. **Implement Visualizations** (Phase 3)
   - Financial Wellbeing & Impact (Likert scales, radar charts)
   - Workshop Quality & Experience (ratings, NPS)
   - New Behaviors (heatmaps, behavioral indices)
   - Workshop Feedback & Engagement (wordclouds, sentiment analysis)
   - Extend Group By functionality to all Big Post visualizations

4. **Pre-Post Comparison Preparation** (Phase 4)
   - Identify comparable question pairs
   - Document for Pairs tab implementation

### Future Phase: Pairs Tab & Advanced Analytics

**Tasks**:
1. **Pairs Tab**: Pre-post comparison visualizations
2. **Within Session Tab**: Journey-specific post-survey analysis
3. **Impact Metrics**: Journey-level aggregations combining pre and post

**Key Considerations**:
- Respect the structure: sessions → journeys → individuals → groups → organizations
- Handle both single-session and multi-session journeys
- Account for big vs little survey differences
- `respondent_id` pairing is sparse but supported (Overview/Data quality banner; Financial Wellbeing within-subjects uses ID match across sessions in filter)

## Development Notes

- **Local Testing**: App runs on `http://127.0.0.1:7983` via `run_local.sh` (or custom port)
- **Data Source**: Google Sheets (authenticated via OAuth)
- **Brand Colors**: Primary (#5c2f92 purple, #82c341 green) and secondary colors applied
- **Error Handling**: Robust handling for missing columns and data issues
- **Survey Types**: Automatically identifies big vs little pre/post based on session position in series

## Current Tab Structure

1. **Session Summary**: High-level session metrics, organization journey counts, session details
2. **Big Pre**: Analysis of big pre-survey responses (first session of series or single sessions)
3. **Big Post**: Analysis of big post-survey responses (last session of series or single sessions)
4. **Pairs**: Pre-post pair analysis (placeholder - ready for implementation)
5. **Within Session**: Journey-specific analysis for selected organization/group combination

## Known Issues & Findings

- **Respondent ID Matching**: Pairing uses `respondent_id` when the same non-anonymous ID appears in Big Pre and Big Post (any session in the current filter). Match rates are often low when learners use different emails; session- and org-level views remain primary.
- **Big Pre Count**: Currently showing 19 big pre responses (expected 22) - needs verification of counting methodology
- **Intact Journeys**: Journey tables include a Paired Big Pre–Post column where IDs align; sparse pairing is expected

See `JOURNEY_STATS_FINDINGS.md` for detailed troubleshooting notes.

## Next Steps

1. **Variable Mapping Project**
   - Inspect all 4 survey types (Big Pre, Little Pre, Little Post, Big Post)
   - Document all questions and response types
   - Identify which variables best measure impact

2. **Visualization Design**
   - Map variables to appropriate visualizations
   - Design impact metrics considering hierarchical structure
   - Plan content for each tab (Big Pre, Big Post, Pairs, Within Session)

3. **Implementation**
   - Build visualizations and summary statistics for each tab
   - Implement impact metrics with respect to session/journey/group/organization structure
   - Test with real data

See `IMPACT_DASHBOARD_PLAN.md` for original development plan (now superseded by current structure).

---

## Future feature — Export Partner Report (PowerPoint)

Long-term goal: an in-dashboard **“Export Partner Report”** button that turns the currently filtered view into a partner-ready `.pptx` slide deck the internal team can edit before sending.

### Intended workflow

1. **Filter to the partner data.** Sidebar filters narrow Master Pre/Post to a target org (and group/journey if needed) and any demographic/time-range subset relevant to the report.
2. **Preview & pick figures.** A new “Partner report” panel previews every chart/table the dashboard can produce for that filter and shows a **checkbox next to each one**. Internal team selects which to include.
3. **Answer customization questions.** A small wizard captures:
   - Partner name / org display label
   - Report title + reporting period
   - Audience (board, funder, internal review, etc.)
   - Tone preset (formal, warm, mixed) for AI text
   - Whether to include identifying follow-up data (default: no)
4. **AI-drafted lay summaries.** For each selected figure/table, generate a short, plain-language summary highlighting what to focus on (e.g. “Optimism rose from 3.4 → 4.1 across these 18 learners”). Editable in the next step.
5. **Generate `.pptx` from a 5 Buckets template.** One figure or table per slide. Branding (purple #5c2f92, green #82c341, fonts, logo) baked into the template. Each slide includes: title, figure/table, AI-drafted caption, and an editable text block for narrative.
6. **Internal editing pass.** Deck opens in PowerPoint / Google Slides; team edits caption text, swaps photos, and finalizes.
7. **Send to partner.** PPTX is the deliverable; partner reads offline. No live hosting needed for v1.

### Open design choices (to revisit later)

- **PPTX engine:** `officer` + `mschart` (R-native, common) vs. `python-pptx` via `reticulate`. `officer` is the path of least resistance.
- **Embedded interactivity:** PowerPoint supports limited interactivity. Options:
  - Static high-DPI PNGs of plotly/ggplot figures (simple, reliable).
  - PowerPoint **animations** for staged reveals (still static charts).
  - Hyperlinks from a slide back to a live filtered Impact Dashboard view (if hosted).
  - **PPTX `<chart>` objects** so partner can hover/edit values (works for native bar/line; not for plotly/wordclouds).
  - Embedded **HTML add-in** slides via the “Web Viewer” PowerPoint add-in for true interactive widgets (heavier setup).
- **Template:** A blank 16:9 5 Buckets-branded PPTX in `Impact Dashboard/templates/` (TBD) — Mike will supply the actual template; code reads it via `officer::read_pptx()` and writes content into named placeholders.
- **AI captions:** Cached per (figure, filter set) so re-runs don’t re-pay for unchanged slides.

### Where it will live in the app

- **Placeholder button:** A “Export Partner Report (coming soon)” action button in the sidebar / header that opens a dialog explaining the planned workflow. To be added when scope is locked.
- **Module folder:** `Impact Dashboard/modules/partner_report/` (does not exist yet) — will house the figure registry, the preview UI, and the PPTX builder.

### Decision needed from Mike (when ready)

- A draft of the target PPTX template (one figure-slide and one summary-slide is enough).
- The list of figures/tables you want in the “catalog” (subset of current dashboard outputs).
- Whether v1 should auto-send by email or stop at file download.

*Status:* not started. Tracking lives here until a dedicated `PARTNER_REPORT_PLAN.md` is created.

