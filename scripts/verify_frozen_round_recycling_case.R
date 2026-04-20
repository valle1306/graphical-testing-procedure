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
suppressPackageStartupMessages(library(jsonlite))

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

build_transition_matrix <- function(nodes_tbl, edges_tbl) {
  transition <- matrix(0, nrow(nodes_tbl), nrow(nodes_tbl))
  rownames(transition) <- colnames(transition) <- nodes_tbl$hypothesis
  if (!nrow(edges_tbl)) {
    return(transition)
  }
  for (i in seq_len(nrow(edges_tbl))) {
    from_hyp <- nodes_tbl$hypothesis[match(edges_tbl$from[[i]], nodes_tbl$id)]
    to_hyp <- nodes_tbl$hypothesis[match(edges_tbl$to[[i]], nodes_tbl$id)]
    transition[[from_hyp, to_hyp]] <- as.numeric(edges_tbl$weight[[i]])
  }
  transition
}

get_all_alpha <- function(gt_obj, hyp_names) {
  stats::setNames(
    vapply(hyp_names, function(hyp) {
      hid <- gt_obj$get_hid(hyp)
      if (!gt_obj$is_in_graph(hid)) {
        return(0)
      }
      gt_obj$get_alpha(hid)
    }, numeric(1)),
    hyp_names
  )
}

build_graphical_testing_object <- function(nodes_tbl, transition) {
  TrialSimulator::GraphicalTesting$new(
    alpha = as.numeric(nodes_tbl$alpha),
    transition = transition,
    alpha_spending = rep("asOF", nrow(nodes_tbl)),
    planned_max_info = rep(100L, nrow(nodes_tbl)),
    hypotheses = as.character(nodes_tbl$hypothesis),
    silent = TRUE
  )
}

simulate_frozen_round <- function(
  graph,
  submission,
  round_number,
  hypotheses,
  plan_tbl,
  schedule_tbl
) {
  round_rows <- schedule_tbl %>%
    dplyr::filter(analysis_round == !!round_number, hypothesis %in% hypotheses) %>%
    dplyr::mutate(hypothesis = factor(hypothesis, levels = hypotheses)) %>%
    dplyr::arrange(hypothesis, hypothesis_stage) %>%
    dplyr::mutate(hypothesis = as.character(hypothesis))
  stopifnot(nrow(round_rows) == length(hypotheses))

  current_alpha <- get_all_alpha(graph, round_rows$hypothesis)
  boundary_tbls <- lapply(seq_len(nrow(round_rows)), function(i) {
    plan_row <- plan_tbl %>%
      dplyr::filter(hypothesis == round_rows$hypothesis[[i]]) %>%
      dplyr::slice(1)
    hypothesis_schedule <- schedule_tbl %>%
      dplyr::filter(hypothesis == round_rows$hypothesis[[i]]) %>%
      dplyr::arrange(hypothesis_stage)
    compute_boundary_schedule(
      total_alpha = current_alpha[[round_rows$hypothesis[[i]]]],
      spending_type = plan_row$alpha_spending[[1]],
      timing = hypothesis_schedule$information_fraction
    )
  })

  history_rows <- tibble::tibble(
    submission = rep(as.integer(submission), nrow(round_rows)),
    schedule_key = round_rows$schedule_key,
    analysis_round = round_rows$analysis_round,
    hypothesis = round_rows$hypothesis,
    hypothesis_stage = round_rows$hypothesis_stage,
    alpha_spending = rep("OF", nrow(round_rows)),
    runtime_spending_code = rep(gs_runtime_spending_code("OF"), nrow(round_rows)),
    information_fraction = round_rows$information_fraction,
    current_alpha = as.numeric(current_alpha[round_rows$hypothesis]),
    cumulative_alpha_spent = vapply(seq_len(nrow(round_rows)), function(i) {
      boundary_tbls[[i]]$cumulative_alpha_spent[[round_rows$hypothesis_stage[[i]]]]
    }, numeric(1)),
    p_value = as.numeric(round_rows$p_value),
    boundary_p = vapply(seq_len(nrow(round_rows)), function(i) {
      boundary_tbls[[i]]$p_boundary[[round_rows$hypothesis_stage[[i]]]]
    }, numeric(1)),
    boundary_z = vapply(seq_len(nrow(round_rows)), function(i) {
      boundary_tbls[[i]]$z_boundary[[round_rows$hypothesis_stage[[i]]]]
    }, numeric(1)),
    decision = ifelse(
      as.numeric(round_rows$p_value) <= vapply(seq_len(nrow(round_rows)), function(i) {
        boundary_tbls[[i]]$p_boundary[[round_rows$hypothesis_stage[[i]]]]
      }, numeric(1)) + 1e-12,
      "Reject",
      "Do not reject"
    ),
    is_final = round_rows$is_final,
    max_info = rep(100, nrow(round_rows))
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

fixture_path <- file.path(project_root, "examples", "frozen_round_recycling_case.json")
fixture <- jsonlite::fromJSON(fixture_path)
nodes_tbl <- tibble::as_tibble(fixture$nodes)
edges_tbl <- tibble::as_tibble(fixture$edges)
plan_tbl <- tibble::as_tibble(fixture$gs_hypothesis_plan)
schedule_tbl <- tibble::as_tibble(fixture$gs_analysis_schedule)

if (!"haybittle_p1" %in% names(plan_tbl)) {
  plan_tbl$haybittle_p1 <- rep(3e-04, nrow(plan_tbl))
}

nodes_tbl$alpha <- as.numeric(nodes_tbl$alpha)
plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
transition <- build_transition_matrix(nodes_tbl, edges_tbl)

rv <- new.env(parent = emptyenv())
rv$nodes <- nodes_tbl
rv$gs_analysis_history <- empty_gs_analysis_history()
rv$gs_boundary_preview_message <- NULL
rv$ts_object <- build_graphical_testing_object(nodes_tbl, transition)

get_current_allocations <- function() {
  get_all_alpha(rv$ts_object, rv$nodes$hypothesis)
}

build_ts_status_table <- function() {
  allocations <- get_current_allocations()
  decision_map <- if (nrow(rv$gs_analysis_history)) {
    latest_gs_history_decision_map(rv$gs_analysis_history, rv$nodes$hypothesis)
  } else {
    stats::setNames(rep("Pending", nrow(rv$nodes)), rv$nodes$hypothesis)
  }
  status_tbl <- tibble::tibble(
    hypothesis = rv$nodes$hypothesis,
    current_alpha = as.numeric(allocations[rv$nodes$hypothesis]),
    decision = unname(decision_map[rv$nodes$hypothesis]),
    in_graph = vapply(rv$nodes$hypothesis, function(hyp) {
      rv$ts_object$is_in_graph(rv$ts_object$get_hid(hyp))
    }, logical(1)),
    testable = vapply(rv$nodes$hypothesis, function(hyp) {
      rv$ts_object$is_testable(rv$ts_object$get_hid(hyp))
    }, logical(1))
  )
  status_tbl$decision[!status_tbl$in_graph] <- "Reject"
  status_tbl
}

round1_inputs <- schedule_tbl %>%
  dplyr::filter(analysis_round == 1L, hypothesis %in% c("H1", "H2")) %>%
  dplyr::mutate(p_value = c(0.01, 0.01))
schedule_tbl$expected_p_value <- NA_real_
schedule_tbl$expected_p_value[match(round1_inputs$schedule_key, schedule_tbl$schedule_key)] <- round1_inputs$p_value

round1 <- simulate_frozen_round(
  graph = rv$ts_object,
  submission = 1L,
  round_number = 1L,
  hypotheses = c("H1", "H2"),
  plan_tbl = plan_tbl,
  schedule_tbl = schedule_tbl %>%
    dplyr::mutate(p_value = expected_p_value)
)
rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(dplyr::bind_rows(rv$gs_analysis_history, round1$history_rows))

stopifnot(identical(unname(round1$history_rows$decision), c("Do not reject", "Do not reject")))
stopifnot(abs(round1$alpha_after["H1"] - 0.01) < 1e-12)
stopifnot(abs(round1$alpha_after["H2"] - 0.015) < 1e-12)
stopifnot(abs(round1$alpha_after["H3"] - 0) < 1e-12)

round2_inputs <- schedule_tbl %>%
  dplyr::filter(analysis_round == 2L, hypothesis %in% c("H1", "H2")) %>%
  dplyr::mutate(p_value = c(0.0001, 0.02))
schedule_tbl$expected_p_value <- NA_real_
schedule_tbl$expected_p_value[match(round2_inputs$schedule_key, schedule_tbl$schedule_key)] <- round2_inputs$p_value

round2 <- simulate_frozen_round(
  graph = rv$ts_object,
  submission = 2L,
  round_number = 2L,
  hypotheses = c("H1", "H2"),
  plan_tbl = plan_tbl,
  schedule_tbl = schedule_tbl %>%
    dplyr::mutate(p_value = expected_p_value)
)
rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(dplyr::bind_rows(rv$gs_analysis_history, round2$history_rows))

stopifnot(identical(unname(round2$history_rows$decision), c("Reject", "Do not reject")))
stopifnot(abs(round2$alpha_after["H1"] - 0) < 1e-12)
stopifnot(abs(round2$alpha_after["H2"] - 0.0225) < 1e-12)
stopifnot(abs(round2$alpha_after["H3"] - 0.0025) < 1e-12)

h2_round2_history <- round2$history_rows %>%
  dplyr::filter(hypothesis == "H2") %>%
  dplyr::slice(1)
stopifnot(abs(as.numeric(h2_round2_history$current_alpha) - 0.015) < 1e-12)

status_after_round2 <- build_ts_status_table()
preview_after_round2 <- build_gs_boundary_schedule(
  plan_tbl = plan_tbl,
  schedule_tbl = dplyr::select(schedule_tbl, -expected_p_value),
  notify = FALSE
)
live_state_after_round2 <- gs_live_analysis_state_tbl(
  status_tbl = status_after_round2,
  schedule_tbl = dplyr::select(schedule_tbl, -expected_p_value),
  history_tbl = rv$gs_analysis_history
)
round2_state_after_recycling <- gs_analysis_round_state(
  preview_tbl = preview_after_round2,
  history_tbl = rv$gs_analysis_history,
  selected_round = 2L
)

h2_preview_row <- preview_after_round2 %>%
  dplyr::filter(hypothesis == "H2", hypothesis_stage == 2L) %>%
  dplyr::slice(1)
h3_preview_row <- preview_after_round2 %>%
  dplyr::filter(hypothesis == "H3", hypothesis_stage == 1L) %>%
  dplyr::slice(1)
h2_live_row <- live_state_after_round2 %>%
  dplyr::filter(Hypothesis == "H2") %>%
  dplyr::slice(1)
h3_live_row <- live_state_after_round2 %>%
  dplyr::filter(Hypothesis == "H3") %>%
  dplyr::slice(1)

stopifnot(abs(as.numeric(h2_preview_row$current_alpha) - 0.0225) < 1e-12)
stopifnot(abs(as.numeric(h3_preview_row$current_alpha) - 0.0025) < 1e-12)
stopifnot(identical(as.character(h3_preview_row$status), "Ready"))
stopifnot(identical(as.character(h2_live_row$`Current Alpha`), "0.0225"))
stopifnot(identical(as.character(h2_live_row$Decision), "Do not reject"))
stopifnot(identical(as.character(h3_live_row$`Current Alpha`), "0.0025"))
stopifnot(identical(as.character(h3_live_row$Decision), "Pending"))
stopifnot(identical(round2_state_after_recycling$actionable_rounds, 2L))
stopifnot(identical(round2_state_after_recycling$next_actionable_round, 2L))
stopifnot(identical(as.character(round2_state_after_recycling$remaining_rows$hypothesis), "H3"))
stopifnot(identical(as.character(round2_state_after_recycling$actionable_rows$hypothesis), "H3"))

combined_history <- dplyr::bind_rows(round1$history_rows, round2$history_rows)
latest_decisions <- latest_gs_history_decision_map(combined_history, c("H1", "H2", "H3"))
stopifnot(identical(unname(latest_decisions), c("Reject", "Do not reject", "Pending")))

expected_summary <- c("Reject", "Do not reject")
stopifnot(identical(unname(round2$summary_rows$decision), expected_summary))
stopifnot(identical(round2$summary_rows$max_allocated_alpha, c(0.01, 0.015)))

# Control case: if both hypotheses reject, the full alpha can recycle onward.
control_graph <- build_graphical_testing_object(nodes_tbl, transition)
control_round1_inputs <- dplyr::select(schedule_tbl, -expected_p_value) %>%
  dplyr::filter(analysis_round == 1L, hypothesis %in% c("H1", "H2")) %>%
  dplyr::mutate(p_value = c(0.01, 0.01))
control_schedule <- dplyr::select(schedule_tbl, -expected_p_value)
control_schedule$p_value <- NA_real_
control_schedule$p_value[match(control_round1_inputs$schedule_key, control_schedule$schedule_key)] <- control_round1_inputs$p_value
invisible(simulate_frozen_round(
  graph = control_graph,
  submission = 1L,
  round_number = 1L,
  hypotheses = c("H1", "H2"),
  plan_tbl = plan_tbl,
  schedule_tbl = control_schedule
))

control_round2_inputs <- dplyr::select(schedule_tbl, -expected_p_value) %>%
  dplyr::filter(analysis_round == 2L, hypothesis %in% c("H1", "H2")) %>%
  dplyr::mutate(p_value = c(0.0001, 0.0001))
control_schedule$p_value <- NA_real_
control_schedule$p_value[match(control_round2_inputs$schedule_key, control_schedule$schedule_key)] <- control_round2_inputs$p_value
control_round2 <- simulate_frozen_round(
  graph = control_graph,
  submission = 2L,
  round_number = 2L,
  hypotheses = c("H1", "H2"),
  plan_tbl = plan_tbl,
  schedule_tbl = control_schedule
)
stopifnot(identical(unname(control_round2$history_rows$decision), c("Reject", "Reject")))
stopifnot(abs(control_round2$alpha_after["H3"] - 0.025) < 1e-12)

cat("Frozen round recycling verification case\n")
cat("- Round 2 keeps H2 non-rejected when its p-value misses the pre-submit boundary.\n")
cat("- H2 keeps Alpha At Submission = 0.015 while post-submit current alpha increases to 0.0225.\n")
cat("- H3 stays at 0.0025 after mixed round-2 outcomes and only reaches 0.025 when both H1 and H2 reject.\n")
cat("- The boundary preview, live analysis state, and round-entry helper all agree on the post-submit state.\n")
cat("\nRound 2 summary rows:\n")
print(round2$summary_rows)
cat("\nBoundary preview after round 2:\n")
print(preview_after_round2 %>% dplyr::select(hypothesis, hypothesis_stage, current_alpha, status, analysis_round))
cat("\nLive analysis state after round 2:\n")
print(live_state_after_round2)
cat("\nRound-entry state after round 2:\n")
print(round2_state_after_recycling$remaining_rows %>% dplyr::select(hypothesis, hypothesis_stage, current_alpha, status, analysis_round))
cat("\nControl alpha after round 2:\n")
print(control_round2$alpha_after)
