#!/usr/bin/env Rscript
# One-shot: verify Mercy PM join + conditional clean scan
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  setwd(normalizePath(file.path(dirname(file_arg), "..")))
}
source("load_sources.R")
pre <- load_master_pre()
post <- load_master_post()
pm <- combine_program_managers_for_typing(load_program_manager(), load_mercy_program_manager())
post_t <- identify_survey_type(post, pm)
s6 <- post_t[
  grepl("MERCY", post_t$session_id) &
    grepl("S6-6", post_t$session_id) &
    !grepl("_TEST_", post_t$session_id),
]
message(
  "Non-test Mercy S6-6 posts: ", nrow(s6),
  " big_post=", sum(s6$is_big_post %in% TRUE, na.rm = TRUE),
  " little=", sum(!(s6$is_big_post %in% TRUE), na.rm = TRUE)
)
print(as.data.frame(dplyr::count(
  dplyr::as_tibble(s6),
  session_id, session_number, sessions_in_series, is_big_post
)))
message(
  "Total Mercy big_post: ",
  sum(post_t$is_big_post %in% TRUE & grepl("MERCY", post_t$session_id), na.rm = TRUE)
)

all_sid <- c(as.character(pre$session_id), as.character(post$session_id))
orgs <- c(as.character(pre$org_name), as.character(post$org_name))
grps <- c(as.character(pre$group), as.character(post$group))
message("\n=== Conditional clean scan ===")
message("1969 ids: ", sum(grepl("^1969", all_sid), na.rm = TRUE))
message(
  "Field empty sid/org: ",
  sum(
    grepl("Field empty", all_sid, ignore.case = TRUE) |
      grepl("Field empty", orgs, ignore.case = TRUE),
    na.rm = TRUE
  )
)
message(
  "TEST: ",
  sum(grepl("_TEST_", all_sid) | tolower(trimws(grps)) == "test", na.rm = TRUE)
)
message(
  "ARISE Schoool: ",
  sum(grepl("SCHOOOL|Schoool", paste(all_sid, orgs)), na.rm = TRUE)
)
message(
  "06/-2 session_date: ",
  sum(grepl("06/-2", as.character(post$session_date)), na.rm = TRUE)
)
bad <- unique(all_sid[grepl("^1969|Field empty|SCHOOOL|_TEST_", all_sid)])
message("Bad/test unique session_ids:")
print(bad)
