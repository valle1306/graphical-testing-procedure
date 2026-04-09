script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_reactive_guard_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(dplyr))

source(file.path(project_root, "R", "server", "common_helpers.R"), local = TRUE)
observe <- function(...) invisible(NULL)
source(file.path(project_root, "R", "server", "sequential_settings.R"), local = TRUE)

same_sanitized_table <- function(left, right, sanitizer) {
  left <- sanitizer(left)
  right <- sanitizer(right)
  isTRUE(all.equal(
    as.data.frame(left, stringsAsFactors = FALSE),
    as.data.frame(right, stringsAsFactors = FALSE),
    check.attributes = FALSE
  ))
}

build_schedule <- function(plan_tbl, existing_tbl = NULL) {
  schedule_tbl <- suppressWarnings(build_default_gs_analysis_schedule(plan_tbl))
  if (!is.null(existing_tbl)) {
    schedule_tbl <- suppressWarnings(merge_gs_schedule_with_existing(schedule_tbl, existing_tbl))
  }
  schedule_tbl
}

stored_plan <- sanitize_gs_hypothesis_plan_tbl(
  data.frame(
    id = c(1L, 2L),
    hypothesis = c("H1", "H2"),
    planned_analyses = c(2L, 1L),
    alpha_spending = c("OF", "Pocock"),
    custom_cumulative_alpha = c("", ""),
    hsd_gamma = c(-4, -4),
    haybittle_p1 = c(3e-04, 3e-04),
    stringsAsFactors = FALSE
  )
)

stored_schedule <- build_schedule(stored_plan)
stored_schedule$information_fraction[stored_schedule$schedule_key == gs_schedule_key(1L, 1L)] <- 0.4
stored_schedule <- sanitize_gs_analysis_schedule_tbl(stored_schedule)

transient_plan <- sanitize_gs_hypothesis_plan_tbl(stored_plan)
transient_schedule <- build_schedule(transient_plan, stored_schedule)

stopifnot(same_sanitized_table(transient_plan, stored_plan, sanitize_gs_hypothesis_plan_tbl))
stopifnot(same_sanitized_table(transient_schedule, stored_schedule, sanitize_gs_analysis_schedule_tbl))

updated_plan <- stored_plan
updated_plan$planned_analyses[updated_plan$hypothesis == "H1"] <- 3L
updated_plan <- sanitize_gs_hypothesis_plan_tbl(updated_plan)
updated_schedule <- build_schedule(updated_plan, stored_schedule)
h1_rows <- updated_schedule[updated_schedule$hypothesis == "H1", , drop = FALSE]

stopifnot(!same_sanitized_table(updated_plan, stored_plan, sanitize_gs_hypothesis_plan_tbl))
stopifnot(!same_sanitized_table(updated_schedule, stored_schedule, sanitize_gs_analysis_schedule_tbl))
stopifnot(nrow(h1_rows) == 3L)
stopifnot(all(c(gs_schedule_key(1L, 1L), gs_schedule_key(1L, 2L), gs_schedule_key(1L, 3L)) %in% h1_rows$schedule_key))
stopifnot(
  identical(
    updated_schedule$information_fraction[updated_schedule$schedule_key == gs_schedule_key(1L, 1L)],
    stored_schedule$information_fraction[stored_schedule$schedule_key == gs_schedule_key(1L, 1L)]
  )
)

invalid_transient_plan <- sanitize_gs_hypothesis_plan_tbl(stored_plan)
invalid_transient_schedule <- build_schedule(invalid_transient_plan, stored_schedule)

stopifnot(same_sanitized_table(invalid_transient_plan, stored_plan, sanitize_gs_hypothesis_plan_tbl))
stopifnot(same_sanitized_table(invalid_transient_schedule, stored_schedule, sanitize_gs_analysis_schedule_tbl))

cat("Group sequential reactive-guard verification scaffold\n\n")
cat("Expected behavior:\n")
cat("- Unchanged collected tables compare equal, so transient invalid numeric states do not rewrite stored state.\n")
cat("- A committed K change produces a different stored plan and a regenerated schedule with the new stage rows.\n")
cat("- Existing overlapping schedule rows are preserved during regeneration.\n")
cat("\nStored schedule:\n")
print(stored_schedule)
cat("\nUpdated schedule after H1 K changes from 2 to 3:\n")
print(updated_schedule)
