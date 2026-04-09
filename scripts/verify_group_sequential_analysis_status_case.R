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

format_plain_number <- function(x) format(as.numeric(x), trim = TRUE, scientific = FALSE)
observe <- function(...) invisible(NULL)

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
  },
  warning = function(w) {
    warning_messages <<- c(warning_messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(length(warning_messages) == 0L)

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
    as.numeric(h1_stage1$cumulative_alpha_spent) / as.numeric(h1_stage1$current_alpha),
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
status_tbl <- build_ts_status_table()
status_display <- gs_status_display_tbl(status_tbl = status_tbl, preview_tbl = preview_after, history_tbl = rv$gs_analysis_history)
status_display_empty_preview <- gs_status_display_tbl(status_tbl = status_tbl, preview_tbl = empty_gs_boundary_preview(), history_tbl = rv$gs_analysis_history)

h1_after_stage2 <- preview_after %>%
  dplyr::filter(hypothesis == "H1", hypothesis_stage == 2L) %>%
  dplyr::slice(1)
h1_status_row <- status_display %>%
  dplyr::filter(Hypothesis == "H1") %>%
  dplyr::slice(1)
h2_status_row <- status_display %>%
  dplyr::filter(Hypothesis == "H2") %>%
  dplyr::slice(1)

remaining_rounds <- gs_remaining_analysis_rounds(schedule_tbl = schedule_tbl, history_tbl = rv$gs_analysis_history)

stopifnot(is.null(rv$gs_boundary_preview_message))
stopifnot(all(c("analysis_round", "hypothesis_stage", "schedule_key", "is_final", "max_info") %in% names(preview_after)))
stopifnot(abs(as.numeric(h1_after_stage2$current_alpha) - 0.02) < 1e-12)
stopifnot(abs(as.numeric(h1_after_stage2$cumulative_alpha_spent) - 0.005) < 1e-12)
stopifnot(identical(as.integer(remaining_rounds), c(2L, 3L, 4L)))
stopifnot(identical(as.character(h1_status_row$`Next Round`), "2"))
stopifnot(identical(as.character(h2_status_row$Decision), "reject"))
stopifnot(identical(as.character(h2_status_row$`In Graph`), "No"))
stopifnot(nrow(status_display_empty_preview) == 4L)
stopifnot(all(status_display_empty_preview$`Next Round` == ""))

cat("Group sequential analysis-status regression scaffold\n\n")
cat("Expected behavior:\n")
cat("- Mixed Custom + OF submissions do not invalidate future boundary preview rows.\n")
cat("- Custom cumulative alpha stays anchored to the original design alpha after recycling.\n")
cat("- Remaining analysis rounds still come from the stored schedule after round 1 is submitted.\n")
cat("- Current Status display stays renderable even when preview rows are empty.\n")
cat("- Custom-alpha helper coercion no longer emits seq_len(length.out=...) warnings.\n")
cat("\nStatus display after round 1:\n")
print(status_display)
cat("\nRemaining rounds after round 1:\n")
print(remaining_rounds)
cat("\nH1 preview rows after H2 rejection recycled alpha:\n")
print(preview_after %>% dplyr::filter(hypothesis == "H1") %>% dplyr::arrange(hypothesis_stage))
