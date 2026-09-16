# Impact Dashboard — plan & backlog

**Last updated:** 2026-09-15  
**Repo:** `/Users/mak/5buckets-impact-dashboard`  
**Visual board:** Cursor canvas `impact-dash-status.canvas.tsx` (collapsible sections)  
**Working rule:** Build & stabilize **local** first (`http://127.0.0.1:3840`). Deploy only when you say go.

---

## Strategy pivot (locked direction)

| Before | After |
|--------|--------|
| Primary deliverable = live interactive dashboard | **Primary deliverable = filtered export → secondary AI** |
| Humans read charts in-app | Dashboard **selects & organizes** data; secondary AI **digests** `.md` + can re-query Sheets |
| Partner PPTX was the long-term export story | **Four exports:** human **PDF**, analysis **Markdown**, data-mapping **Markdown**, filtered **CSV ZIP** |

Knowledge we already encoded in the dashboard (variable meaning, Big/Little typing, series keys, indices, pairing, language, open-text delimiter) must be **exported as a familiarity pack** the secondary AI can reuse when it needs follow-on analyses on live Sheets data.

---

## Priority queue (active)

### ~~P0 — Data reliability (filter state)~~ ✅ Done (2026-09-15)

**Was:** Toggle filters off/on, return to “no filter,” and numbers differed from initial load (e.g. **1138 → 693**).

**Fixed:** Module selection resets when org/group scope expands to all; session summary ignores sticky module subsets at all-scope; group dropdown no longer force-resets on click; loading overlay + no-op `update*Input` guards stopped reactive storms.

**Exit met:** Initial load ≡ clear-all-filters after org/group visits.

### P0 — Multi-select org & group filters (in progress)

Replace single org/group with multi-select (include set). Empty selection = no restriction. Group choices scoped to currently selected orgs. Wire through `session_summary_data`, `filtered_pre/post`, Annual, favorites restore.

**UX:** selectize multi with remove_button; placeholder “All organizations / All groups”.

### P1 — Export pathway (4 sidebar downloads) — upgraded

| Output | Audience | Filter-aware? | Role |
|--------|----------|---------------|------|
| **PDF report** | Humans / partners | Yes (+ section scope) | Summary KPIs for current filters / selected sections |
| **Report (.md for AI)** | Secondary AI | Yes (+ section scope) | Hierarchical numeric dump mirroring Overview→Annual tabs + figure recipes |
| **Data Map (.md for AI)** | Secondary AI | **No** (full schema) | Familiarity pack: typing, scoring, dashboard index, recipe schema, workflow |
| **CSV ZIP** | Secondary AI | Yes | Respondent-level pools (PII stripped); optional `figures/` PNGs |

Implemented in `modules/export.R`. Sidebar: section checkboxes. Results MD is **numeric + figure recipes** (efficient for AI). Data ZIP is CSVs only.

### P2 — Plain-English auto-interpret (in-app)

Expand deterministic summary boxes across substantive outcomes (feeds PDF captions and `.md` narrative blocks).

### P2 — Remaining polish / backlog

- Zip map: richer labels + split markers on zoom (substantial)  
- Click Annual recommend / Post sat / NPS → modal → User Journey  
- Hist bins → respondents; generalize bar→modal  
- Connect URL slug; Annual HH-income form label alignment  

---

## Ops / ship checklist (before next Connect deploy)

- [ ] Smoke series tables after NA label fix (`data/process_data.R`)  
- [ ] Share **Mercy PM** with service account as Viewer (`12XH-SbHwHGYHVPSfx7DrShuUC18btE2XnTKMiilU8iI`)  
- [ ] Confirm **`RESPONDENT_ID_SALT`** on Connect Cloud Variables (local `.Renviron` = SET)  
- [ ] Smoke Annual + User Journey + Reach bar→modal on local  
- [ ] Smoke multi-select org/group + clear-all ≡ baseline totals  
- [ ] Commit + push only when local is stable  

**URLs:** Live https://5buckets-impact-dashboard.share.connect.posit.cloud/ · Local http://127.0.0.1:3840

---

## Pipeline snapshot

| Track | Status |
|-------|--------|
| Standard Big Post EN/ES/ZH | Live |
| Standard Pre / Little Post ES/ZH | Deferred |
| Mercy Little + Big Post EN/ES/ZH | Live (dual-write) |
| Mercy Pre custom | Not built (filtered copy) |
| Annual → Master tab | Live data; dash UI local until push |

---

## Open questions

1. ~~PDF engine preference (`pagedown` / `quarto` / `officer`→PDF)?~~ → **pagedown when available; text PDF fallback**
2. ~~Should `.md` export include raw respondent-level rows, aggregates only, or both (privacy)?~~ → **Analysis MD = aggregates; CSV ZIP = row-level (PII stripped)**
3. ~~Familiarity pack format: single markdown bible vs modular JSON + recipes?~~ → **Generated data-mapping Markdown from code (v1)**
4. ~~Multi-select UX: checkbox list vs selectize multiple?~~ → **selectize multiple**
5. Still defer Mercy Pre custom + standard Pre/Little ES/ZH?
6. Enrich human PDF with charts / plain-English captions (P2 auto-interpret)?

### Review notes

- 2026-09-08 AM: series empty on live → fixed locally; Mercy Big Post confirmed live in code.  
- 2026-09-08 PM: **New top priorities** — filter reliability, multi-select org/group, PDF+MD export, secondary-AI data familiarity. Strategy pivot to AI-assisted reporting.
- 2026-09-15: Filter reliability fixed (modules sticky subset). Loading UX: header stays visible; overlay covers menus only. Multi-select org/group next.
- 2026-09-15 PM: Phase 3 export scaffold — 4 sidebar downloads (PDF, analysis MD, data-mapping MD, filtered CSV ZIP) in `modules/export.R`.
- 2026-09-15 eve: Report MD hierarchical by tab + section scope + optional PNGs; renamed Report / Data Map; Data Map gains dashboard index + recipe schema + workflow.
