script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_analysis_status_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(TrialSimulator))

format_plain_number <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value)) {
    return(character(0))
  }
  out <- rep("", length(value))
  keep <- !is.na(value) & is.finite(value)
  if (!any(keep)) {
    return(out)
  }
  formatted <- format(value[keep], trim = TRUE, scientific = FALSE, nsmall = 0)
  formatted <- sub("([0-9])0+$", "\\1", formatted)
  out[keep] <- sub("\\.$", "", formatted)
  out
}
observe <- function(...) invisible(NULL)
observeEvent <- function(...) invisible(NULL)

source(file.path(project_root, "R", "server", "common_helpers.R"), local = TRUE)
source(file.path(project_root, "R", "server", "sequential_settings.R"), local = TRUE)
source(file.path(project_root, "R", "server", "sequential_boundaries.R"), local = TRUE)

rv <- new.env(parent = emptyenv())
rv$nodes <- tibble::tibble(
  id = 1:4,
  hypothesis = c("H1", "H2", "H3", "H4"),
  alpha = c(0.01, 0.04, 0, 0)
)
rv$gs_analysis_history <- empty_gs_analysis_history()
rv$gs_boundary_preview_message <- NULL
rv$ts_object <- NULL

get_current_allocations <- function() {
  if (is.null(rv$ts_object)) {
    return(stats::setNames(rv$nodes$alpha, rv$nodes$hypothesis))
  }
  stats::setNames(
    vapply(rv$nodes$hypothesis, function(hyp) {
      hid <- rv$ts_object$get_hid(hyp)
      if (!rv$ts_object$is_in_graph(hid)) {
        return(0)
      }
      rv$ts_object$get_alpha(hid)
    }, numeric(1)),
    rv$nodes$hypothesis
  )
}

build_ts_status_table <- function() {
  if (is.null(rv$ts_object)) {
    return(tibble::tibble(
      hypothesis = rv$nodes$hypothesis,
      current_alpha = as.numeric(rv$nodes$alpha),
      decision = rep("ready", nrow(rv$nodes)),
      in_graph = rep(TRUE, nrow(rv$nodes)),
      testable = rv$nodes$alpha > 0
    ))
  }
  decisions <- rv$ts_object$get_current_decision()
  allocations <- get_current_allocations()
  status_tbl <- tibble::tibble(
    hypothesis = rv$nodes$hypothesis,
    current_alpha = as.numeric(allocations[rv$nodes$hypothesis]),
    decision = unname(decisions[rv$nodes$hypothesis]),
    in_graph = vapply(rv$nodes$hypothesis, function(hyp) {
      rv$ts_object$is_in_graph(rv$ts_object$get_hid(hyp))
    }, logical(1)),
    testable = vapply(rv$nodes$hypothesis, function(hyp) {
      rv$ts_object$is_testable(rv$ts_object$get_hid(hyp))
    }, logical(1))
  )
  status_tbl$decision[!status_tbl$in_graph] <- "reject"
  status_tbl
}

warning_messages <- character(0)
withCallingHandlers(
  {
    placeholder_example <- gs_custom_cumulative_alpha_placeholder(
      data.frame(planned_analyses = c(4L, 2L)),
      0.01
    )
    instruction_example <- gs_custom_cumulative_alpha_instruction(
      data.frame(planned_analyses = c(4L, 2L)),
      0.01
    )
    stopifnot(identical(placeholder_example, "e.g. 0.0025, 0.005, 0.0075, 0.01"))
    stopifnot(grepl("4 cumulative alpha values", instruction_example, fixed = TRUE))
    stopifnot(grepl("automatically rescaled", instruction_example, fixed = TRUE))
  },
  warning = function(w) {
    warning_messages <<- c(warning_messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(length(warning_messages) == 0L)
stopifnot(grepl("gs-chip-submission", gs_submission_chip_html(2L), fixed = TRUE))
stopifnot(grepl("Submission 2", gs_submission_chip_html(2L), fixed = TRUE))
stopifnot(grepl("gs-chip-round-stage", gs_round_stage_chip_html(2L, 2L), fixed = TRUE))
stopifnot(grepl("Analysis Time 2 / Look 2", gs_round_stage_chip_html(2L, 2L), fixed = TRUE))
stopifnot(grepl("gs-chip-reject", gs_decision_chip_html("Reject"), fixed = TRUE))
stopifnot(grepl("gs-chip-keep", gs_decision_chip_html("Do not reject"), fixed = TRUE))
stopifnot(grepl("gs-chip-pending", gs_decision_chip_html("Pending"), fixed = TRUE))
stopifnot(identical(
  gs_round_submission_feedback_text(1L, 1L, 1L),
  "Saved analysis time 1 with 1 saved result across 1 hypothesis."
))
stopifnot(identical(
  gs_round_submission_feedback_text(1L, 2L, 2L),
  "Saved analysis time 1 with 2 saved results across 2 hypotheses."
))
stopifnot(identical(
  gs_round_submission_feedback_text(1L, 3L, 2L, 2L),
  "Saved analysis time 1 with 3 saved results across 2 hypotheses. Now showing Analysis Time 2."
))
stopifnot(identical(
  gs_round_stage_label(c(2L, 2L), c(1L, 2L), is_final = c(FALSE, TRUE)),
  c("Analysis Time 2 / Look 1", "Analysis Time 2 / Look 2 (final)")
))
stopifnot(identical(
  gs_analysis_round_closed_state_message(),
  "All actionable analysis times have been submitted."
))

empty_submitted_display <- gs_submitted_analyses_display_tbl(empty_gs_analysis_history())
stopifnot(identical(
  names(empty_submitted_display),
  c(
    "Submission",
    "Analysis Time / Look",
    "Hypothesis",
    "Alpha Spending",
    "Info Fraction",
    "Alpha At Submission",
    "P",
    "Boundary p",
    "Boundary z",
    "Decision"
  )
))
stopifnot(nrow(empty_submitted_display) == 0L)

empty_boundary_review_display <- gs_boundary_review_display_tbl(empty_gs_boundary_preview())
stopifnot(identical(
  names(empty_boundary_review_display),
  c(
    "Analysis Time",
    "Hypothesis",
    "Interim Analysis (Look)",
    "Total Analyses",
    "Info Fraction",
    "Alpha Spending",
    "Current Alpha",
    "Look Alpha",
    "Cumulative Alpha",
    "Boundary p",
    "Boundary z",
    "Status"
  )
))
stopifnot(nrow(empty_boundary_review_display) == 0L)

mismatch_history <- sanitize_gs_analysis_history_tbl(tibble::tibble(
  submission = c(9L, 9L),
  schedule_key = c("99__1", "99__2"),
  analysis_round = c(2L, 2L),
  hypothesis = c("HX", "HY"),
  hypothesis_stage = c(1L, 2L),
  alpha_spending = c("OF", "OF"),
  runtime_spending_code = c("asOF", "asOF"),
  information_fraction = c(0.5, 1),
  current_alpha = c(0.01, 0.01),
  cumulative_alpha_spent = c(0.005, 0.01),
  p_value = c(0.02, 0.6),
  boundary_p = c(0.001, 0.01),
  boundary_z = c(3, 2),
  decision = c("Do not reject", "Do not reject"),
  is_final = c(FALSE, TRUE),
  max_info = c(100, 100)
))
mismatch_display <- gs_submitted_analyses_display_tbl(mismatch_history)
stopifnot(identical(
  as.character(mismatch_display$`Analysis Time / Look`),
  c("Analysis Time 2 / Look 1", "Analysis Time 2 / Look 2 (final)")
))
stopifnot(identical(
  build_round_submit_log(mismatch_history)[[1]],
  "Submitted group sequential analysis time 2 with 2 saved results across 2 hypotheses."
))

mismatch_status_tbl <- tibble::tibble(
  hypothesis = "HX",
  current_alpha = 0.01,
  decision = "Pending",
  in_graph = TRUE,
  testable = TRUE
)
mismatch_live <- gs_live_analysis_state_tbl(
  status_tbl = mismatch_status_tbl,
  schedule_tbl = empty_gs_analysis_schedule(),
  history_tbl = mismatch_history
)
stopifnot(identical(as.character(mismatch_live$`Last Submitted Analysis Time / Look`), "Analysis Time 2 / Look 1"))
stopifnot(identical(as.character(mismatch_live$`Next Analysis Time`), "—"))

display_plan_tbl <- sanitize_gs_hypothesis_plan_tbl(tibble::tibble(
  id = 1:3,
  hypothesis = c("H1", "H2", "H3"),
  planned_analyses = c(2L, 2L, 1L),
  alpha_spending = c("OF", "OF", "OF"),
  custom_cumulative_alpha = c("", "", ""),
  hsd_gamma = c(-4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003)
))
display_schedule_tbl <- sanitize_gs_analysis_schedule_tbl(tibble::tibble(
  schedule_key = c("1__1", "2__1", "1__2", "2__2", "3__1"),
  analysis_round = c(1L, 1L, 3L, 2L, 2L),
  hypothesis = c("H1", "H2", "H1", "H2", "H3"),
  hypothesis_id = c(1L, 2L, 1L, 2L, 3L),
  hypothesis_stage = c(1L, 1L, 2L, 2L, 1L),
  planned_analyses = c(2L, 2L, 2L, 2L, 1L),
  information_fraction = c(0.5, 0.5, 1, 1, 1),
  is_final = c(FALSE, FALSE, TRUE, TRUE, TRUE)
))
display_tbl <- gs_analysis_schedule_display_tbl(
  plan_tbl = display_plan_tbl,
  schedule_tbl = display_schedule_tbl
)
stopifnot(identical(as.integer(display_tbl$analysis_round), c(1L, 1L, 2L, 2L, 3L)))
stopifnot(identical(as.character(display_tbl$hypothesis), c("H1", "H2", "H2", "H3", "H1")))
stopifnot(identical(as.integer(display_tbl$hypothesis_stage), c(1L, 1L, 2L, 1L, 2L)))
stopifnot(identical(length(gs_round_choice_values(display_tbl)), 3L))

state_preview <- sanitize_gs_boundary_preview_tbl(tibble::tibble(
  hypothesis = c("H1", "H2", "H3"),
  alpha_spending = c("OF", "OF", "OF"),
  planned_analyses = c(1L, 1L, 1L),
  analysis = c(1L, 1L, 1L),
  timing = c(1, 1, 1),
  current_alpha = c(0.01, 0.01, 0.01),
  stage_alpha = c(0.01, 0.01, 0.01),
  cumulative_alpha_spent = c(0.01, 0.01, 0.01),
  z_boundary = c(2, 2, 2),
  p_boundary = c(0.05, 0.05, 0.05),
  status = c("Ready", "Not currently testable", "Ready"),
  analysis_round = c(1L, 2L, 3L),
  hypothesis_stage = c(1L, 1L, 1L),
  schedule_key = c("1__1", "2__1", "3__1"),
  is_final = c(TRUE, TRUE, TRUE),
  max_info = c(100, 100, 100)
))
state_before <- gs_analysis_round_state(
  preview_tbl = state_preview,
  history_tbl = empty_gs_analysis_history(),
  selected_round = 1L
)
stopifnot(identical(state_before$actionable_rounds, c(1L, 3L)))
stopifnot(identical(state_before$next_actionable_round, 1L))
stopifnot(identical(state_before$remaining_rows$analysis_round, 1L))

state_after <- gs_analysis_round_state(
  preview_tbl = state_preview,
  history_tbl = sanitize_gs_analysis_history_tbl(tibble::tibble(
    submission = 1L,
    schedule_key = "1__1",
    analysis_round = 1L,
    hypothesis = "H1",
    hypothesis_stage = 1L,
    alpha_spending = "OF",
    runtime_spending_code = "asOF",
    information_fraction = 1,
    current_alpha = 0.01,
    cumulative_alpha_spent = 0.01,
    p_value = 0.01,
    boundary_p = 0.05,
    boundary_z = 2,
    decision = "Do not reject",
    is_final = TRUE,
    max_info = 100
  )),
  selected_round = 1L
)
stopifnot(isTRUE(state_after$selected_round_complete))
stopifnot(identical(state_after$actionable_rounds, c(3L)))
stopifnot(identical(state_after$next_actionable_round, 3L))
complete_state <- gs_analysis_round_state(
  preview_tbl = sanitize_gs_boundary_preview_tbl(dplyr::mutate(state_preview, status = "Not currently testable")),
  history_tbl = empty_gs_analysis_history(),
  selected_round = 2L
)
stopifnot(isTRUE(complete_state$is_complete))
stopifnot(identical(complete_state$actionable_rounds, integer(0)))
stopifnot(identical(
  gs_resolve_analysis_round_target(
    selected_round = 2L,
    actionable_rounds = c(1L, 2L, 3L),
    next_actionable_round = 1L,
    force_first_actionable = FALSE
  ),
  2L
))
stopifnot(identical(
  gs_resolve_analysis_round_target(
    selected_round = 2L,
    actionable_rounds = c(1L, 2L, 3L),
    next_actionable_round = 1L,
    force_first_actionable = TRUE
  ),
  1L
))
stopifnot(is.na(gs_resolve_analysis_round_target(
  selected_round = 2L,
  actionable_rounds = integer(0),
  next_actionable_round = NA_integer_,
  force_first_actionable = TRUE
)))

plan_tbl <- tibble::tibble(
  id = 1:4,
  hypothesis = c("H1", "H2", "H3", "H4"),
  planned_analyses = c(4L, 1L, 1L, 1L),
  alpha_spending = c("Custom", "OF", "OF", "OF"),
  custom_cumulative_alpha = c("0.0025, 0.005, 0.0075, 0.01", "", "", ""),
  hsd_gamma = c(-4, -4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003, 0.0003)
)
plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
schedule_tbl <- build_default_gs_analysis_schedule(plan_tbl)

preview_before <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
h1_stage1 <- preview_before %>% dplyr::filter(hypothesis == "H1", hypothesis_stage == 1L) %>% dplyr::slice(1)
h2_stage1 <- preview_before %>% dplyr::filter(hypothesis == "H2", hypothesis_stage == 1L) %>% dplyr::slice(1)

orig_build_ts_status_table <- build_ts_status_table
build_ts_status_table <- function() {
  tibble::tibble(
    hypothesis = rv$nodes$hypothesis,
    current_alpha = c(0.01, 0.02, 0.03, 0.04),
    decision = c("ready", "ready", "ready", "ready"),
    in_graph = c(TRUE, TRUE, TRUE, FALSE),
    testable = c(TRUE, TRUE, FALSE, FALSE)
  )
}
boundary_status_preview <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
stopifnot(identical(
  as.character(boundary_status_preview$status[match("H3", boundary_status_preview$hypothesis)]),
  "Not currently testable"
))
stopifnot(identical(
  as.character(boundary_status_preview$status[match("H4", boundary_status_preview$hypothesis)]),
  "Inactive"
))
build_ts_status_table <- orig_build_ts_status_table

rv$ts_object <- TrialSimulator::GraphicalTesting$new(
  alpha = rv$nodes$alpha,
  transition = matrix(
    c(
      0,    0,    0.8, 0.2,
      0.25, 0,    0,   0.75,
      0,    0,    0,   1,
      0,    0,    0,   0
    ),
    nrow = 4,
    byrow = TRUE
  ),
  alpha_spending = c("asUser", "asOF", "asOF", "asOF"),
  planned_max_info = rep(100L, 4),
  hypotheses = rv$nodes$hypothesis,
  silent = TRUE
)

round1 <- data.frame(
  order = c(1L, 1L),
  hypotheses = c("H1", "H2"),
  p = c(0.05, 0.02),
  info = c(25L, 100L),
  is_final = c(FALSE, TRUE),
  max_info = c(100L, 100L),
  alpha_spent = c(
    gs_custom_alpha_spent_fraction("H1", 1L, plan_tbl = plan_tbl),
    NA_real_
  ),
  stringsAsFactors = FALSE
)

invisible(rv$ts_object$test(round1))

rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
  tibble::tibble(
    submission = c(1L, 1L),
    schedule_key = c("1__1", "2__1"),
    analysis_round = c(1L, 1L),
    hypothesis = c("H1", "H2"),
    hypothesis_stage = c(1L, 1L),
    alpha_spending = c("Custom", "OF"),
    runtime_spending_code = c("asUser", "asOF"),
    information_fraction = c(0.25, 1),
    current_alpha = c(0.01, 0.04),
    cumulative_alpha_spent = c(as.numeric(h1_stage1$cumulative_alpha_spent), NA_real_),
    p_value = c(0.05, 0.02),
    boundary_p = c(as.numeric(h1_stage1$p_boundary), as.numeric(h2_stage1$p_boundary)),
    boundary_z = c(as.numeric(h1_stage1$z_boundary), as.numeric(h2_stage1$z_boundary)),
    decision = c("Do not reject", "Reject"),
    is_final = c(FALSE, TRUE),
    max_info = c(100, 100)
  )
)

preview_after <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
h1_after_stage2 <- preview_after %>%
  dplyr::filter(hypothesis == "H1", hypothesis_stage == 2L) %>%
  dplyr::slice(1)

round2 <- data.frame(
  order = 2L,
  hypotheses = "H1",
  p = 0.7,
  info = 50L,
  is_final = FALSE,
  max_info = 100L,
  alpha_spent = gs_custom_alpha_spent_fraction("H1", 2L, plan_tbl = plan_tbl),
  stringsAsFactors = FALSE
)

runtime_validation <- validate_gs_runtime_alpha_spent(
  stage_df = round2,
  ready_rows = preview_after %>% dplyr::filter(hypothesis == "H1", hypothesis_stage == 2L),
  history_tbl = rv$gs_analysis_history
)
stopifnot(isTRUE(runtime_validation$ok))

invisible(rv$ts_object$test(round2))

rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
  dplyr::bind_rows(
    rv$gs_analysis_history,
    tibble::tibble(
      submission = 2L,
      schedule_key = "1__2",
      analysis_round = 2L,
      hypothesis = "H1",
      hypothesis_stage = 2L,
      alpha_spending = "Custom",
      runtime_spending_code = "asUser",
      information_fraction = 0.5,
      current_alpha = as.numeric(h1_after_stage2$current_alpha),
      cumulative_alpha_spent = as.numeric(h1_after_stage2$cumulative_alpha_spent),
      p_value = 0.7,
      boundary_p = as.numeric(h1_after_stage2$p_boundary),
      boundary_z = as.numeric(h1_after_stage2$z_boundary),
      decision = "Do not reject",
      is_final = FALSE,
      max_info = 100
    )
  )
)

preview_after_round2 <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
boundary_review_display <- gs_boundary_review_display_tbl(preview_after_round2)
status_tbl <- build_ts_status_table()
live_state <- gs_live_analysis_state_tbl(status_tbl = status_tbl, schedule_tbl = schedule_tbl, history_tbl = rv$gs_analysis_history)
live_state_empty_history <- gs_live_analysis_state_tbl(status_tbl = status_tbl, schedule_tbl = schedule_tbl, history_tbl = empty_gs_analysis_history())
h1_live_row <- live_state %>% dplyr::filter(Hypothesis == "H1") %>% dplyr::slice(1)
h2_live_row <- live_state %>% dplyr::filter(Hypothesis == "H2") %>% dplyr::slice(1)

remaining_rounds <- gs_remaining_analysis_rounds(schedule_tbl = schedule_tbl, history_tbl = rv$gs_analysis_history)

stopifnot(is.null(rv$gs_boundary_preview_message))
stopifnot(all(c("analysis_round", "hypothesis_stage", "schedule_key", "is_final", "max_info") %in% names(preview_after)))
stopifnot(all(c(
  "Analysis Time",
  "Hypothesis",
  "Interim Analysis (Look)",
  "Total Analyses",
  "Info Fraction",
  "Alpha Spending",
  "Current Alpha",
  "Look Alpha",
  "Cumulative Alpha",
  "Boundary p",
  "Boundary z",
  "Status"
) %in% names(boundary_review_display)))
stopifnot(all(c(
  "Hypothesis",
  "Current Alpha",
  "Last Submitted Analysis Time / Look",
  "Last Observed p",
  "Decision",
  "In Graph",
  "Testable",
  "Next Analysis Time"
) %in% names(live_state)))
stopifnot(abs(as.numeric(h1_after_stage2$current_alpha) - 0.02) < 1e-12)
stopifnot(abs(as.numeric(h1_after_stage2$cumulative_alpha_spent) - 0.01) < 1e-12)
stopifnot(abs(round2$alpha_spent[[1]] - 0.5) < 1e-12)
stopifnot(identical(as.integer(remaining_rounds), c(3L, 4L)))
stopifnot(identical(as.character(h1_live_row$`Last Submitted Analysis Time / Look`), "Analysis Time 2 / Look 2"))
stopifnot(identical(as.character(h1_live_row$`Last Observed p`), "0.7"))
stopifnot(identical(as.character(h1_live_row$Decision), "Do not reject"))
stopifnot(identical(as.character(h1_live_row$`Next Analysis Time`), "3"))
stopifnot(identical(as.character(h2_live_row$Decision), "Reject"))
stopifnot(identical(as.character(h2_live_row$`In Graph`), "No"))
stopifnot(nrow(live_state_empty_history) == 4L)
stopifnot(all(live_state_empty_history$`Last Submitted Analysis Time / Look` == ""))
stopifnot(all(c("analysis_round", "hypothesis_stage", "schedule_key", "is_final", "max_info") %in% names(preview_after_round2)))

rv$ts_object <- NULL
reset_history <- empty_gs_analysis_history()
reset_preview <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
reset_status_tbl <- build_ts_status_table()
reset_live_state <- gs_live_analysis_state_tbl(
  status_tbl = reset_status_tbl,
  schedule_tbl = schedule_tbl,
  history_tbl = reset_history
)
reset_round_state <- gs_analysis_round_state(
  preview_tbl = reset_preview,
  history_tbl = reset_history,
  selected_round = 2L
)
reset_target_round <- gs_resolve_analysis_round_target(
  selected_round = 2L,
  actionable_rounds = reset_round_state$actionable_rounds,
  next_actionable_round = reset_round_state$next_actionable_round,
  force_first_actionable = TRUE
)
reset_display_state <- gs_analysis_round_state(
  preview_tbl = reset_preview,
  history_tbl = reset_history,
  selected_round = reset_target_round
)
reset_h1_row <- reset_live_state %>% dplyr::filter(Hypothesis == "H1") %>% dplyr::slice(1)
reset_h3_row <- reset_live_state %>% dplyr::filter(Hypothesis == "H3") %>% dplyr::slice(1)
stopifnot(identical(reset_target_round, 1L))
stopifnot(identical(reset_round_state$next_actionable_round, 1L))
stopifnot(all(reset_live_state$`Last Submitted Analysis Time / Look` == ""))
stopifnot(all(reset_live_state$`Last Observed p` == ""))
stopifnot(identical(as.character(reset_h1_row$Decision), "Pending"))
stopifnot(identical(as.character(reset_h1_row$`Next Analysis Time`), "1"))
stopifnot(identical(as.character(reset_h3_row$Decision), "Not testable"))
stopifnot(all(reset_display_state$remaining_rows$analysis_round == 1L))

cat("Group sequential analysis-status regression scaffold\n\n")
cat("Expected behavior:\n")
cat("- Mixed Custom + OF submissions do not invalidate future boundary preview rows.\n")
cat("- Custom cumulative alpha is rescaled by the current recycled alpha using the original cumulative proportions.\n")
cat("- Analysis Time 2 with H1 p = 0.7 submits successfully and advances the next analysis time to 3.\n")
cat("- Live Analysis State stays renderable and carries the latest per-hypothesis result.\n")
cat("- Custom-alpha helper coercion no longer emits seq_len(length.out=...) warnings.\n")
cat("- Analysis reset resolves the selector back to the first actionable analysis time and clears live results.\n")
cat("\nLive Analysis State after Analysis Time 2:\n")
print(live_state)
cat("\nRemaining analysis times after Analysis Time 2:\n")
print(remaining_rounds)
cat("\nH1 preview rows after H2 rejection recycled alpha:\n")
print(preview_after %>% dplyr::filter(hypothesis == "H1") %>% dplyr::arrange(hypothesis_stage))
cat("\nLive Analysis State after simulated reset:\n")
print(reset_live_state)
