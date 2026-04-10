script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_reactivity_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

options(gmt.debug.reactivity = TRUE)
setwd(project_root)
source(file.path(project_root, "app.R"), local = globalenv())

shiny::testServer(server, {
  metric_calls <- function(label) {
    metrics <- snapshot_reactivity_metrics(label)
    if (!nrow(metrics)) {
      return(0L)
    }
    as.integer(metrics$calls[[1]])
  }
  flush_session <- function() {
    if (is.function(session$flushReact)) {
      session$flushReact()
    }
    invisible(NULL)
  }

  rv$gs_hypothesis_plan <- build_default_gs_hypothesis_plan(rv$nodes, empty_gs_hypothesis_plan())
  rv$gs_analysis_schedule <- build_default_gs_analysis_schedule(rv$gs_hypothesis_plan)
  set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  flush_session()

  baseline_plan <- isolate(rv$gs_hypothesis_plan)
  baseline_schedule <- isolate(rv$gs_analysis_schedule)

  session$setInputs(
    gs_plan_k_1 = baseline_plan$planned_analyses[baseline_plan$id == 1L],
    gs_plan_rule_1 = baseline_plan$alpha_spending[baseline_plan$id == 1L]
  )
  flush_session()

  stopifnot(same_gs_hypothesis_plan_tbl(rv$gs_hypothesis_plan, baseline_plan))
  stopifnot(same_gs_analysis_schedule_tbl(rv$gs_analysis_schedule, baseline_schedule))

  reset_reactivity_metrics()
  boundary_calls_before_invalid <- metric_calls("build_gs_boundary_schedule")

  session$setInputs(gs_plan_k_1 = 0)
  flush_session()

  stopifnot(same_gs_hypothesis_plan_tbl(rv$gs_hypothesis_plan, baseline_plan))
  stopifnot(same_gs_analysis_schedule_tbl(rv$gs_analysis_schedule, baseline_schedule))
  stopifnot(identical(metric_calls("build_gs_boundary_schedule"), boundary_calls_before_invalid))

  boundary_calls_before_k_change <- metric_calls("build_gs_boundary_schedule")

  session$setInputs(gs_plan_k_1 = 3)
  flush_session()

  updated_plan <- rv$gs_hypothesis_plan
  updated_schedule <- rv$gs_analysis_schedule
  unchanged_before <- baseline_schedule %>%
    dplyr::filter(.data$hypothesis_id != 1L) %>%
    dplyr::arrange(.data$schedule_key)
  unchanged_after <- updated_schedule %>%
    dplyr::filter(.data$hypothesis_id != 1L) %>%
    dplyr::arrange(.data$schedule_key)
  h1_rows <- updated_schedule %>%
    dplyr::filter(.data$hypothesis_id == 1L) %>%
    dplyr::arrange(.data$hypothesis_stage)

  stopifnot(updated_plan$planned_analyses[updated_plan$id == 1L] == 3L)
  stopifnot(nrow(h1_rows) == 3L)
  stopifnot(identical(h1_rows$analysis_round, c(1L, 2L, 3L)))
  stopifnot(all.equal(h1_rows$information_fraction, c(1 / 3, 2 / 3, 1), tolerance = 1e-8))
  stopifnot(same_gs_analysis_schedule_tbl(unchanged_after, unchanged_before))
  stopifnot(metric_calls("build_gs_boundary_schedule") > boundary_calls_before_k_change)
})

cat("Group sequential reactivity regression passed.\n")
cat("- No-op plan inputs do not rewrite stored plan/schedule state.\n")
cat("- Invalid transient K input falls back to stored state without boundary recompute.\n")
cat("- Changing H1 K rebuilds only H1 schedule rows and keeps other hypotheses untouched.\n")
