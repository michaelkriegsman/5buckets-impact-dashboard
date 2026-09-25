#!/usr/bin/env Rscript
# Diagnose filter-break: module selection after org change
suppressPackageStartupMessages(source("load_sources.R"))

# Helpers copied from app.R (avoid sourcing shinyApp)
MODULE_FILTER_ORDER <- c(
  "Earn", "Save", "Spend", "Grow", "Protect", "Share", "Borrow", "Plan", "Invest", "Other"
)
.pm_modules_atoms_from_cell <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || !nzchar(trimws(as.character(x))))))
    return(character(0))
  raw <- trimws(as.character(x))
  segs <- trimws(unlist(strsplit(raw, "[,;|/]+")))
  segs <- segs[nzchar(segs)]
  unique(segs)
}
.unique_module_labels_from_df_col <- function(vec) {
  atoms <- character(0)
  for (x in vec) atoms <- c(atoms, .pm_modules_atoms_from_cell(x))
  unique(atoms)
}
.order_module_labels_for_sidebar <- function(atoms) {
  atoms <- unique(as.character(atoms))
  atoms <- atoms[!is.na(atoms) & nzchar(atoms)]
  ord <- intersect(MODULE_FILTER_ORDER, atoms)
  c(ord, sort(setdiff(atoms, MODULE_FILTER_ORDER)))
}
.program_manager_module_choices <- function(pm, post_df = NULL) {
  mods <- character(0)
  if (!is.null(pm) && nrow(pm) > 0) {
    cn <- colnames(pm)
    col <- if ("modules_taught" %in% cn) "modules_taught" else {
      hit <- grep("module", cn, ignore.case = TRUE, value = TRUE)
      if (length(hit)) hit[[1]] else NA_character_
    }
    if (!is.na(col) && nzchar(col)) mods <- .unique_module_labels_from_df_col(pm[[col]])
  }
  if (!length(mods) && !is.null(post_df) && nrow(post_df) > 0 && "modules_taught" %in% colnames(post_df)) {
    mods <- .unique_module_labels_from_df_col(post_df[["modules_taught"]])
  }
  if (!length(mods)) mods <- MODULE_FILTER_ORDER
  .order_module_labels_for_sidebar(mods)
}
.session_row_matches_selected_modules <- function(modules_taught_vec, selected_modules) {
  if (is.null(selected_modules) || !length(selected_modules)) return(rep(TRUE, length(modules_taught_vec)))
  vapply(seq_along(modules_taught_vec), function(i) {
    atoms <- .pm_modules_atoms_from_cell(modules_taught_vec[i])
    length(intersect(atoms, selected_modules)) > 0L
  }, logical(1))
}

pre <- load_master_pre(); post <- load_master_post(); pm <- load_program_manager(); ann <- load_master_annual()
ss <- create_session_summary_from_master(pre, post, pm)
mods_full <- .program_manager_module_choices(pm, post)
cat("BASELINE raw pre+post+ann =", nrow(pre) + nrow(post) + nrow(ann), "\n")
cat("ss rows", nrow(ss), " | full mods (", length(mods_full), "):", paste(mods_full, collapse = ", "), "\n\n")

apply_mod_filter <- function(sel_mod) {
  full_mods <- .program_manager_module_choices(pm, post)
  # Exact logic from session_summary_data
  filter_on <- length(sel_mod) > 0 && length(full_mods) > 0 &&
    !identical(sort(unique(sel_mod)), sort(unique(full_mods)))
  ss2 <- ss
  if (filter_on && "modules_taught" %in% colnames(ss)) {
    keep <- .session_row_matches_selected_modules(ss$modules_taught, sel_mod)
    ss2 <- ss[keep, , drop = FALSE]
  }
  sids <- trimws(as.character(ss2$session_id))
  # filtered_pre also keeps rows that don't match session_id if no match — approximate strict path
  pre2 <- pre
  post2 <- post
  if (nrow(ss2) > 0 && "session_id" %in% names(pre)) {
    match_pre <- trimws(as.character(pre$session_id)) %in% sids
    if (any(match_pre)) pre2 <- pre[match_pre, , drop = FALSE]
  }
  if (nrow(ss2) > 0 && "session_id" %in% names(post)) {
    match_post <- trimws(as.character(post$session_id)) %in% sids
    if (any(match_post)) post2 <- post[match_post, , drop = FALSE]
  }
  list(filter_on = filter_on, ss = nrow(ss2), pre = nrow(pre2), post = nrow(post2),
       total = nrow(pre2) + nrow(post2) + nrow(ann), sel = sel_mod)
}

print_res <- function(label, r) {
  cat(sprintf("%-55s filter_on=%-5s ss=%2d pre=%3d post=%3d TOTAL=%4d  sel=[%s]\n",
              label, r$filter_on, r$ss, r$pre, r$post, r$total, paste(r$sel, collapse = ",")))
}

print_res("A) empty modules (UI before observer)", apply_mod_filter(character(0)))
print_res("B) all modules selected (after observer)", apply_mod_filter(mods_full))

# Orgs
orgs <- unique(trimws(as.character(pm$org_name)))
orgs <- orgs[!is.na(orgs) & nzchar(orgs)]
cat("\n--- After visiting each org (stuck with that org's modules) then All ---\n")
for (o in orgs) {
  pm_o <- pm[trimws(as.character(pm$org_name)) == o, , drop = FALSE]
  mods_o <- .program_manager_module_choices(pm_o, post)
  r <- apply_mod_filter(mods_o)
  if (r$total < nrow(pre) + nrow(post) + nrow(ann)) {
    print_res(paste0("ORG ", o), r)
  }
}

# How many sessions have empty modules_taught?
if ("modules_taught" %in% names(ss)) {
  mt <- trimws(as.character(ss$modules_taught))
  empty <- is.na(mt) | !nzchar(mt)
  cat("\nss modules_taught empty:", sum(empty), "/", nrow(ss), "\n")
  # sessions that fail match against Earn-only etc
  for (test in list(mods_full[1], mods_full[1:2], character(0))) {
    if (!length(test) && length(test) == 0) next
  }
}

# Exact user path: pick org with few modules
cat("\n--- Detail: Mercy Housing scoped modules ---\n")
pm_m <- pm[grepl("Mercy", as.character(pm$org_name), ignore.case = TRUE), , drop = FALSE]
mods_m <- .program_manager_module_choices(pm_m, post)
cat("Mercy pm rows", nrow(pm_m), "mods:", paste(mods_m, collapse = ", "), "\n")
r <- apply_mod_filter(mods_m)
print_res("stuck Mercy mods while All Orgs", r)

# Also: sessions kept vs dropped
keep <- .session_row_matches_selected_modules(ss$modules_taught, mods_m)
cat("sessions kept", sum(keep), "dropped", sum(!keep), "\n")
if (sum(!keep)) {
  dropped <- ss[!keep, c("org_name", "group", "modules_taught"), drop = FALSE]
  print(utils::head(dropped, 12))
}
