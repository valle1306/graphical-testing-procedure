script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_frozen_round_recycling_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(TrialSimulator))
suppressPackageStartupMessages(library(dplyr))
source(file.path(project_root, "R", "server", "common_helpers.R"), local = TRUE)

get_all_alpha <- function(gt_obj, hyp_names) {
  vapply(hyp_names, function(h) {
    hid <- gt_obj$get_hid(h)
    if (!gt_obj$is_in_graph(hid)) {
      return(0)
    }
    gt_obj$get_alpha(hid)
  }, numeric(1))
}

simulate_frozen_round <- function(
  graph,
  submission,
  analysis_round,
  hypotheses,
  stage_numbers,
  p_values,
  info_fractions
) {
  current_alpha <- get_all_alpha(graph, hypotheses)
  boundary_tbls <- lapply(seq_along(hypotheses), function(i) {
    compute_boundary_schedule(
      total_alpha = current_alpha[[hypotheses[[i]]]],
      spending_type = "OF",
      timing = c(0.5, 1)
    )
  })

  history_rows <- tibble::tibble(
    submission = rep(as.integer(submission), length(hypotheses)),
    schedule_key = vapply(seq_along(hypotheses), function(i) gs_schedule_key(i, stage_numbers[[i]]), character(1)),
    analysis_round = rep(as.integer(analysis_round), length(hypotheses)),
    hypothesis = hypotheses,
    hypothesis_stage = as.integer(stage_numbers),
    alpha_spending = rep("OF", length(hypotheses)),
    runtime_spending_code = rep(gs_runtime_spending_code("OF"), length(hypotheses)),
    information_fraction = as.numeric(info_fractions),
    current_alpha = as.numeric(current_alpha[hypotheses]),
    cumulative_alpha_spent = vapply(seq_along(hypotheses), function(i) {
      boundary_tbls[[i]]$cumulative_alpha_spent[[stage_numbers[[i]]]]
    }, numeric(1)),
    p_value = as.numeric(p_values),
    boundary_p = vapply(seq_along(hypotheses), function(i) {
      boundary_tbls[[i]]$p_boundary[[stage_numbers[[i]]]]
    }, numeric(1)),
    boundary_z = vapply(seq_along(hypotheses), function(i) {
      boundary_tbls[[i]]$z_boundary[[stage_numbers[[i]]]]
    }, numeric(1)),
    decision = ifelse(
      as.numeric(p_values) <= vapply(seq_along(hypotheses), function(i) {
        boundary_tbls[[i]]$p_boundary[[stage_numbers[[i]]]]
      }, numeric(1)) + 1e-12,
      "Reject",
      "Do not reject"
    ),
    is_final = as.logical(stage_numbers == 2L),
    max_info = rep(100, length(hypotheses))
  )
  history_rows <- sanitize_gs_analysis_history_tbl(history_rows)

  reject_hypotheses <- history_rows$hypothesis[
    vapply(history_rows$decision, normalize_gs_decision_label, character(1)) == "Reject"
  ]
  invisible(apply_frozen_gs_rejections(graph, reject_hypotheses))

  list(
    history_rows = history_rows,
    summary_rows = build_gs_round_result_summary(history_rows),
    alpha_after = get_all_alpha(graph, c("H1", "H2", "H3"))
  )
}

transition <- matrix(
  c(
    0,   0.75, 0.25,
    0.50, 0,    0.50,
    0,   0,    0
  ),
  nrow = 3,
  byrow = TRUE
)
rownames(transition) <- colnames(transition) <- c("H1", "H2", "H3")

graph <- TrialSimulator::GraphicalTesting$new(
  alpha = c(0.01, 0.015, 0),
  transition = transition,
  alpha_spending = c("asOF", "asOF", "asOF"),
  planned_max_info = c(100L, 100L, 100L),
  hypotheses = c("H1", "H2", "H3"),
  silent = TRUE
)

round1 <- simulate_frozen_round(
  graph = graph,
  submission = 1L,
  analysis_round = 1L,
  hypotheses = c("H1", "H2"),
  stage_numbers = c(1L, 1L),
  p_values = c(0.01, 0.01),
  info_fractions = c(0.5, 0.5)
)

stopifnot(all(round1$history_rows$decision == c("Do not reject", "Do not reject")))
stopifnot(abs(round1$alpha_after["H1"] - 0.01) < 1e-12)
stopifnot(abs(round1$alpha_after["H2"] - 0.015) < 1e-12)
stopifnot(abs(round1$alpha_after["H3"] - 0) < 1e-12)

round2 <- simulate_frozen_round(
  graph = graph,
  submission = 2L,
  analysis_round = 2L,
  hypotheses = c("H1", "H2"),
  stage_numbers = c(2L, 2L),
  p_values = c(0.0001, 0.02),
  info_fractions = c(1, 1)
)

stopifnot(identical(unname(round2$history_rows$decision), c("Reject", "Do not reject")))
stopifnot(abs(round2$alpha_after["H1"] - 0) < 1e-12)
stopifnot(abs(round2$alpha_after["H2"] - 0.0225) < 1e-12)
stopifnot(abs(round2$alpha_after["H3"] - 0.0025) < 1e-12)

combined_history <- dplyr::bind_rows(round1$history_rows, round2$history_rows)
latest_decisions <- latest_gs_history_decision_map(combined_history, c("H1", "H2", "H3"))
stopifnot(identical(unname(latest_decisions), c("Reject", "Do not reject", "Pending")))

expected_summary <- c("Reject", "Do not reject")
stopifnot(identical(unname(round2$summary_rows$decision), expected_summary))
stopifnot(identical(round2$summary_rows$max_allocated_alpha, c(0.01, 0.015)))

# Control case: if both hypotheses reject, the full alpha can recycle onward.
control_graph <- TrialSimulator::GraphicalTesting$new(
  alpha = c(0.01, 0.015, 0),
  transition = transition,
  alpha_spending = c("asOF", "asOF", "asOF"),
  planned_max_info = c(100L, 100L, 100L),
  hypotheses = c("H1", "H2", "H3"),
  silent = TRUE
)
invisible(simulate_frozen_round(
  graph = control_graph,
  submission = 1L,
  analysis_round = 1L,
  hypotheses = c("H1", "H2"),
  stage_numbers = c(1L, 1L),
  p_values = c(0.01, 0.01),
  info_fractions = c(0.5, 0.5)
))
control_round2 <- simulate_frozen_round(
  graph = control_graph,
  submission = 2L,
  analysis_round = 2L,
  hypotheses = c("H1", "H2"),
  stage_numbers = c(2L, 2L),
  p_values = c(0.0001, 0.0001),
  info_fractions = c(1, 1)
)
stopifnot(identical(unname(control_round2$history_rows$decision), c("Reject", "Reject")))
stopifnot(abs(control_round2$alpha_after["H3"] - 0.025) < 1e-12)

cat("Frozen round recycling verification case\n")
cat("- Round 2 keeps H2 non-rejected when its p-value misses the pre-submit boundary.\n")
cat("- H1 rejection recycles alpha to H2 and H3 without changing H2's decision.\n")
cat("- If both H1 and H2 reject, H3 receives the full recycled alpha.\n")
cat("\nRound 2 summary rows:\n")
print(round2$summary_rows)
cat("\nAlpha after round 2:\n")
print(round2$alpha_after)
cat("\nLatest decisions:\n")
print(latest_decisions)
cat("\nControl alpha after round 2:\n")
print(control_round2$alpha_after)
