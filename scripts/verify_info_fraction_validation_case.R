script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_info_fraction_validation_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

setwd(project_root)
source(file.path(project_root, "app.R"), local = globalenv())

make_plan_tbl <- function() {
  tibble::tibble(
    id = 1L,
    hypothesis = "H1",
    planned_analyses = 3L,
    alpha_spending = "OF",
    custom_cumulative_alpha = "",
    hsd_gamma = -4
  )
}

make_schedule_tbl <- function(fractions) {
  tibble::tibble(
    schedule_key = c("1_1", "1_2", "1_3"),
    analysis_round = c(1L, 2L, 3L),
    hypothesis = "H1",
    hypothesis_id = 1L,
    hypothesis_stage = c(1L, 2L, 3L),
    planned_analyses = 3L,
    information_fraction = as.numeric(fractions),
    is_final = c(FALSE, FALSE, TRUE)
  )
}

shiny::testServer(server, {
  plan_tbl <- make_plan_tbl()

  zero_case <- validate_gs_analysis_schedule(
    schedule_tbl = make_schedule_tbl(c(0, 2 / 3, 1)),
    plan_tbl = plan_tbl,
    notify = FALSE
  )
  stopifnot(!isTRUE(zero_case$ok))
  stopifnot(identical(zero_case$message, "Information fractions for interim analyses must be in (0, 1) and incremental."))

  one_case <- validate_gs_analysis_schedule(
    schedule_tbl = make_schedule_tbl(c(1, 1, 1)),
    plan_tbl = plan_tbl,
    notify = FALSE
  )
  stopifnot(!isTRUE(one_case$ok))
  stopifnot(identical(one_case$message, "Information fractions for interim analyses must be in (0, 1) and incremental."))

  middle_endpoint_case <- validate_gs_analysis_schedule(
    schedule_tbl = make_schedule_tbl(c(0.4, 1, 1)),
    plan_tbl = plan_tbl,
    notify = FALSE
  )
  stopifnot(!isTRUE(middle_endpoint_case$ok))
  stopifnot(identical(middle_endpoint_case$message, "Information fractions for interim analyses must be in (0, 1) and incremental."))

  valid_case <- validate_gs_analysis_schedule(
    schedule_tbl = make_schedule_tbl(c(1 / 3, 2 / 3, 1)),
    plan_tbl = plan_tbl,
    notify = FALSE
  )
  stopifnot(isTRUE(valid_case$ok))
})

cat("Information fraction validation verification passed.\n")
cat("- Interim-analysis fraction = 0 is rejected.\n")
cat("- Interim-analysis fraction = 1 is rejected.\n")
cat("- Any non-final fraction = 1 is rejected.\n")
cat("- Strictly increasing fractions ending at 1 remain valid.\n")
