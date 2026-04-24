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

  rv$nodes <- tibble::tibble(
    id = c(1L, 2L, 3L),
    x = c(-160, 0, 160),
    y = c(0, 0, 0),
    hypothesis = c("H1", "H2", "H3"),
    alpha = c(0.01, 0.015, 0)
  )
  rv$edges <- tibble::tibble(
    id = integer(),
    from = integer(),
    to = integer(),
    weight = numeric()
  )
  rv$gs_hypothesis_plan <- sanitize_gs_hypothesis_plan_tbl(tibble::tibble(
    id = c(1L, 2L, 3L),
    hypothesis = c("H1", "H2", "H3"),
    planned_analyses = c(2L, 2L, 1L),
    planned_max_info = c(100, 100, 100),
    alpha_spending = c("OF", "OF", "OF"),
    custom_cumulative_alpha = c("", "", ""),
    hsd_gamma = c(-4, -4, -4),
    haybittle_p1 = c(0.0003, 0.0003, 0.0003)
  ))
  rv$gs_analysis_schedule <- sanitize_gs_analysis_schedule_tbl(tibble::tibble(
    schedule_key = c("1__1", "2__1", "3__1", "1__2", "2__2"),
    analysis_round = c(1L, 1L, 1L, 2L, 2L),
    hypothesis = c("H1", "H2", "H3", "H1", "H2"),
    hypothesis_id = c(1L, 2L, 3L, 1L, 2L),
    hypothesis_stage = c(1L, 1L, 1L, 2L, 2L),
    planned_analyses = c(2L, 2L, 1L, 2L, 2L),
    information_fraction = c(0.5, 0.5, 1, 1, 1),
    is_final = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  ))
  set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
  schedule_signature_before_h2_edit <- rv$gs_analysis_schedule_round_signature

  session$setInputs(
    gs_plan_k_1 = 2,
    gs_plan_rule_1 = "OF",
    gs_plan_k_2 = 2,
    gs_plan_rule_2 = "OF",
    gs_plan_k_3 = 1,
    gs_plan_rule_3 = "OF",
    gs_schedule_round_1__1 = 1,
    gs_schedule_info_1__1 = "0.5",
    gs_schedule_round_2__1 = 1,
    gs_schedule_info_2__1 = "0.5",
    gs_schedule_round_3__1 = 1,
    gs_schedule_info_3__1 = "1",
    gs_schedule_round_1__2 = 2,
    gs_schedule_info_1__2 = "1",
    gs_schedule_round_2__2 = 2,
    gs_schedule_info_2__2 = "1"
  )
  flush_session()

  session$setInputs(gs_schedule_round_2__1 = 2)
  flush_session()

  h2_rows <- rv$gs_analysis_schedule %>%
    dplyr::filter(.data$hypothesis == "H2") %>%
    dplyr::arrange(.data$hypothesis_stage)
  stopifnot(identical(h2_rows$analysis_round, c(2L, 3L)))
  stopifnot(!identical(rv$gs_analysis_schedule_round_signature, schedule_signature_before_h2_edit))
  stopifnot(isTRUE(validate_gs_analysis_schedule(
    schedule_tbl = rv$gs_analysis_schedule,
    plan_tbl = rv$gs_hypothesis_plan,
    notify = FALSE
  )$ok))

  schedule_display <- gs_analysis_schedule_display_tbl(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
  stopifnot(identical(
    schedule_display$schedule_key,
    c("1__1", "3__1", "1__2", "2__1", "2__2")
  ))

  schedule_html <- output$gs_analysis_schedule_ui$html
  stopifnot(grepl("Interim Analysis \\(Look\\)", schedule_html))
  stopifnot(grepl("Interim Analysis 1 of 2", schedule_html, fixed = TRUE))
  stopifnot(grepl("Final Analysis 2 of 2", schedule_html, fixed = TRUE))

  rv$gs_hypothesis_plan <- sanitize_gs_hypothesis_plan_tbl(tibble::tibble(
    id = c(1L, 2L),
    hypothesis = c("H1", "H2"),
    planned_analyses = c(2L, 2L),
    planned_max_info = c(100, 100),
    alpha_spending = c("OF", "OF"),
    custom_cumulative_alpha = c("", ""),
    hsd_gamma = c(-4, -4),
    haybittle_p1 = c(0.0003, 0.0003)
  ))
  rv$gs_analysis_history <- empty_gs_analysis_history()
  session$setInputs(gs_analysis_round = "2")
  flush_session()
  rv$gs_boundary_preview <- sanitize_gs_boundary_preview_tbl(tibble::tibble(
    hypothesis = c("H1", "H2"),
    alpha_spending = c("OF", "OF"),
    planned_analyses = c(2L, 2L),
    analysis = c(2L, 1L),
    timing = c(1, 0.5),
    current_alpha = c(0, 0.015),
    stage_alpha = c(0, 0.015),
    cumulative_alpha_spent = c(0.01, 0.0075),
    z_boundary = c(NA_real_, 3.247596),
    p_boundary = c(NA_real_, 0.0005819217),
    status = c("Inactive", "Ready"),
    analysis_round = c(2L, 2L),
    hypothesis_stage = c(2L, 1L),
    schedule_key = c("1__2", "2__1"),
    is_final = c(TRUE, FALSE),
    max_info = c(100, 100)
  ))
  flush_session()

  round2_display_state <- gs_analysis_round_state(
    preview_tbl = rv$gs_boundary_preview,
    history_tbl = rv$gs_analysis_history,
    selected_round = 2L
  )
  stopifnot(identical(as.character(round2_display_state$remaining_rows$hypothesis), c("H1", "H2")))
  stopifnot(identical(as.character(round2_display_state$actionable_rows$hypothesis), "H2"))

  round2_entry_html <- output$gs_round_entry_ui$html
  stopifnot(grepl("gs_round_info_2__1", round2_entry_html, fixed = TRUE))
  stopifnot(!grepl("gs_round_info_1__2", round2_entry_html, fixed = TRUE))
  stopifnot(grepl(
    "Only active, testable hypotheses are shown here. Inactive rows are hidden.",
    round2_entry_html,
    fixed = TRUE
  ))

  rv$nodes <- tibble::tibble(
    id = c(1L, 2L),
    x = c(-110, 110),
    y = c(0, 0),
    hypothesis = c("H1", "H2"),
    alpha = c(0.01, 0.015)
  )
  rv$edges <- tibble::tibble(
    id = c(1L, 2L),
    from = c(1L, 2L),
    to = c(2L, 1L),
    weight = c(1, 1)
  )
  rv$gs_hypothesis_plan <- sanitize_gs_hypothesis_plan_tbl(tibble::tibble(
    id = c(1L, 2L),
    hypothesis = c("H1", "H2"),
    planned_analyses = c(2L, 1L),
    planned_max_info = c(240, 100),
    alpha_spending = c("OF", "OF"),
    custom_cumulative_alpha = c("", ""),
    hsd_gamma = c(-4, -4),
    haybittle_p1 = c(0.0003, 0.0003)
  ))
  rv$gs_analysis_schedule <- sanitize_gs_analysis_schedule_tbl(tibble::tibble(
    schedule_key = c("1__1", "2__1", "1__2"),
    analysis_round = c(1L, 1L, 2L),
    hypothesis = c("H1", "H2", "H1"),
    hypothesis_id = c(1L, 2L, 1L),
    hypothesis_stage = c(1L, 1L, 2L),
    planned_analyses = c(2L, 1L, 2L),
    information_fraction = c(0.5, 1, 1),
    is_final = c(FALSE, TRUE, TRUE)
  ))
  rv$gs_analysis_history <- empty_gs_analysis_history()
  rv$ts_object <- NULL
  rv$ts_summary <- NULL
  rv$planned_max_info <- numeric(0)
  set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)

  session$setInputs(
    gs_plan_k_1 = 2,
    gs_plan_max_info_1 = 240,
    gs_plan_rule_1 = "OF",
    gs_plan_k_2 = 1,
    gs_plan_max_info_2 = 100,
    gs_plan_rule_2 = "OF",
    gs_schedule_round_1__1 = 1,
    gs_schedule_info_1__1 = "0.5",
    gs_schedule_round_2__1 = 1,
    gs_schedule_info_2__1 = "1",
    gs_schedule_round_1__2 = 2,
    gs_schedule_info_1__2 = "1",
    gs_analysis_round = "1"
  )
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  flush_session()

  round_entry_html <- output$gs_round_entry_ui$html
  stopifnot(grepl("gs_round_info_1__1", round_entry_html, fixed = TRUE))
  stopifnot(grepl("value=\"120\"", round_entry_html, fixed = TRUE))

  stopifnot(isTRUE(initialize_batch_gs_object(reset_history = TRUE)))
  session$setInputs(
    gs_analysis_round = "1",
    gs_round_info_1__1 = 120,
    gs_round_p_1__1 = 0.5,
    gs_round_info_2__1 = 100,
    gs_round_p_2__1 = 0.5
  )
  flush_session()

  submission <- collect_round_submission()
  h1_submission <- submission$history_rows %>%
    dplyr::filter(hypothesis == "H1") %>%
    dplyr::slice(1)
  stopifnot(abs(as.numeric(h1_submission$observed_info) - 120) < 1e-12)
  stopifnot(abs(as.numeric(h1_submission$max_info) - 240) < 1e-12)
  stopifnot(any(grepl(
    "Analysis Time 1 / Look 1: alpha at submission 0.01",
    build_round_submit_log(submission$history_rows),
    fixed = TRUE
  )))

  configure_single_hypothesis_runtime_case <- function(planned_analyses, info_fraction, analysis_round, is_final) {
    rv$nodes <- tibble::tibble(id = 1L, x = 0, y = 0, hypothesis = "H1", alpha = 0.025)
    rv$edges <- tibble::tibble(id = integer(), from = integer(), to = integer(), weight = numeric())
    rv$gs_hypothesis_plan <- sanitize_gs_hypothesis_plan_tbl(tibble::tibble(
      id = 1L, hypothesis = "H1", planned_analyses = planned_analyses, planned_max_info = 100,
      alpha_spending = "OF", custom_cumulative_alpha = "", hsd_gamma = -4, haybittle_p1 = 0.0003
    ))
    rv$gs_analysis_schedule <- sanitize_gs_analysis_schedule_tbl(tibble::tibble(
      schedule_key = paste0("1__", seq_along(info_fraction)),
      analysis_round = analysis_round,
      hypothesis = "H1",
      hypothesis_id = 1L,
      hypothesis_stage = seq_along(info_fraction),
      planned_analyses = planned_analyses,
      information_fraction = info_fraction,
      is_final = is_final
    ))
    rv$gs_analysis_history <- empty_gs_analysis_history()
    rv$ts_object <- NULL
    rv$ts_summary <- NULL
    rv$planned_max_info <- numeric(0)
    set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
    runtime_inputs <- list(
      gs_plan_k_1 = planned_analyses,
      gs_plan_max_info_1 = 100,
      gs_plan_rule_1 = "OF"
    )
    for (i in seq_along(info_fraction)) {
      runtime_inputs[[paste0("gs_schedule_round_1__", i)]] <- analysis_round[[i]]
      runtime_inputs[[paste0("gs_schedule_info_1__", i)]] <- as.character(info_fraction[[i]])
    }
    runtime_inputs$gs_analysis_round <- as.character(analysis_round[[1]])
    do.call(session$setInputs, runtime_inputs)
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    flush_session()
    stopifnot(isTRUE(initialize_batch_gs_object(reset_history = TRUE)))
  }

  configure_single_hypothesis_runtime_case(
    planned_analyses = 3L,
    info_fraction = c(1 / 3, 2 / 3, 1),
    analysis_round = c(1L, 2L, 3L),
    is_final = c(FALSE, FALSE, TRUE)
  )
  session$setInputs(gs_round_info_1__1 = 33.33333, gs_round_p_1__1 = 0.5)
  flush_session()
  decimal_error <- tryCatch(collect_round_submission(), error = function(e) e)
  stopifnot(inherits(decimal_error, "error"))
  stopifnot(identical(decimal_error$message, "Enter a positive whole-number observed information count for H1 look 1."))

  configure_single_hypothesis_runtime_case(planned_analyses = 1L, info_fraction = 1, analysis_round = 1L, is_final = TRUE)
  session$setInputs(gs_round_info_1__1 = 120, gs_round_p_1__1 = 0.5)
  flush_session()
  final_submission <- collect_round_submission()
  final_row <- final_submission$history_rows %>% dplyr::slice(1)
  stopifnot(abs(as.numeric(final_row$observed_info) - 120) < 1e-12)
  stopifnot(abs(as.numeric(final_row$max_info) - 120) < 1e-12)
  stopifnot(isTRUE(replay_group_sequential_history(final_submission$history_rows)))
  stopifnot(abs(as.numeric(rv$gs_analysis_history$max_info[[1]]) - 120) < 1e-12)
})

cat("Group sequential reactivity regression passed.\n")
cat("- No-op plan inputs do not rewrite stored plan/schedule state.\n")
cat("- Invalid transient K input falls back to stored state without boundary recompute.\n")
cat("- Changing H1 K rebuilds only H1 schedule rows and keeps other hypotheses untouched.\n")
cat("- Editing one analysis time updates schedule state immediately and auto-cascades later looks.\n")
cat("- Runtime submission rejects non-integer observed information counts before TrialSimulator is called.\n")
cat("- Final-look submissions carry the actual observed count into frozen runtime max info for replay.\n")
