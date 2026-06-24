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
  alpha_lookup <- stats::setNames(as.character(plan_tbl$alpha_spending), as.character(plan_tbl$hypothesis))
  trajectory_before <- tibble::as_tibble(graph$get_trajectory())
  stage_df <- round_rows %>%
    dplyr::transmute(
      order = as.integer(analysis_round),
      hypotheses = as.character(hypothesis),
      p = as.numeric(p_value),
      info = as.integer(round(100 * information_fraction)),
      is_final = as.logical(is_final),
      max_info = as.integer(100),
      alpha_spent = NA_real_
    )
  withCallingHandlers(
    graph$test(as.data.frame(stage_df, stringsAsFactors = FALSE)),
    message = function(m) invokeRestart("muffleMessage")
  )
  trajectory_after <- tibble::as_tibble(graph$get_trajectory())
  trajectory_before_keys <- if (nrow(trajectory_before)) {
    trajectory_before %>%
      dplyr::transmute(
        analysis_round = as.integer(order),
        hypothesis = as.character(hypothesis),
        hypothesis_stage = as.integer(stages)
      )
  } else {
    tibble::tibble(
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_stage = integer()
    )
  }
  batch_results <- trajectory_after %>%
    dplyr::transmute(
      .package_result_order = dplyr::row_number(),
      analysis_round = as.integer(order),
      hypothesis = as.character(hypothesis),
      hypothesis_stage = as.integer(stages),
      runtime_spending_code = as.character(typeOfDesign),
      current_alpha = as.numeric(alpha),
      cumulative_alpha_spent = as.numeric(alphaSpent),
      p_value = as.numeric(obs_p_value),
      boundary_p = as.numeric(stageLevels),
      boundary_z = as.numeric(criticalValues),
      decision = ifelse(
        tolower(as.character(decision)) == "reject",
        "Reject",
        "Do not reject"
      )
    ) %>%
    dplyr::anti_join(
      trajectory_before_keys,
      by = c("analysis_round", "hypothesis", "hypothesis_stage")
    ) %>%
    dplyr::filter(analysis_round == round_number)
  history_rows <- round_rows %>%
    dplyr::transmute(
      submission = as.integer(submission),
      schedule_key = schedule_key,
      analysis_round = analysis_round,
      hypothesis = hypothesis,
      hypothesis_stage = hypothesis_stage,
      alpha_spending = unname(alpha_lookup[hypothesis]),
      information_fraction = information_fraction,
      is_final = is_final,
      max_info = 100
    ) %>%
    dplyr::left_join(
      batch_results,
      by = c("analysis_round", "hypothesis", "hypothesis_stage")
    ) %>%
    dplyr::arrange(.package_result_order) %>%
    dplyr::select(-.package_result_order) %>%
    sanitize_gs_analysis_history_tbl()

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

stopifnot(identical(unname(round2$history_rows$decision), c("Reject", "Reject")))
stopifnot(abs(round2$alpha_after["H1"] - 0) < 1e-12)
stopifnot(abs(round2$alpha_after["H2"] - 0) < 1e-12)
stopifnot(abs(round2$alpha_after["H3"] - 0.025) < 1e-12)

h2_round2_history <- round2$history_rows %>%
  dplyr::filter(hypothesis == "H2") %>%
  dplyr::slice(1)
stopifnot(abs(as.numeric(h2_round2_history$current_alpha) - 0.0225) < 1e-12)

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

h3_preview_row <- preview_after_round2 %>%
  dplyr::filter(hypothesis == "H3", hypothesis_stage == 1L) %>%
  dplyr::slice(1)
h2_live_row <- live_state_after_round2 %>%
  dplyr::filter(Hypothesis == "H2") %>%
  dplyr::slice(1)
h1_live_row <- live_state_after_round2 %>%
  dplyr::filter(Hypothesis == "H1") %>%
  dplyr::slice(1)
h3_live_row <- live_state_after_round2 %>%
  dplyr::filter(Hypothesis == "H3") %>%
  dplyr::slice(1)

stopifnot(abs(as.numeric(h3_preview_row$current_alpha) - 0.025) < 1e-12)
stopifnot(identical(as.character(h3_preview_row$status), "Ready"))
stopifnot(identical(as.character(h1_live_row$`Next Analysis Time`), "—"))
stopifnot(abs(as.numeric(h2_live_row$`Current Alpha`) - 0) < 1e-12)
stopifnot(identical(as.character(h2_live_row$Decision), "Reject"))
stopifnot(identical(as.character(h2_live_row$`Next Analysis Time`), "—"))
stopifnot(abs(as.numeric(h3_live_row$`Current Alpha`) - 0.025) < 1e-12)
stopifnot(identical(as.character(h3_live_row$Decision), "Pending"))
stopifnot(identical(round2_state_after_recycling$actionable_rounds, 2L))
stopifnot(identical(round2_state_after_recycling$next_actionable_round, 2L))
stopifnot(identical(as.character(round2_state_after_recycling$remaining_rows$hypothesis), "H3"))
stopifnot(identical(as.character(round2_state_after_recycling$actionable_rows$hypothesis), "H3"))

combined_history <- dplyr::bind_rows(round1$history_rows, round2$history_rows)
latest_decisions <- latest_gs_history_decision_map(combined_history, c("H1", "H2", "H3"))
stopifnot(identical(unname(latest_decisions), c("Reject", "Reject", "Pending")))

expected_summary <- stats::setNames(round2$summary_rows$decision, round2$summary_rows$hypothesis)
expected_alpha <- stats::setNames(round2$summary_rows$max_allocated_alpha, round2$summary_rows$hypothesis)
stopifnot(identical(unname(expected_summary[c("H1", "H2")]), c("Reject", "Reject")))
stopifnot(abs(expected_alpha[["H1"]] - 0.01) < 1e-12)
stopifnot(abs(expected_alpha[["H2"]] - 0.0225) < 1e-12)

retest_graph <- build_graphical_testing_object(nodes_tbl, transition)
retest_schedule_tbl <- schedule_tbl
retest_schedule_tbl$p_value <- NA_real_
retest_round1_inputs <- retest_schedule_tbl %>%
  dplyr::filter(analysis_round == 1L, hypothesis %in% c("H1", "H2")) %>%
  dplyr::mutate(p_value = c(0.00027, 0.00056))
retest_schedule_tbl$p_value[match(retest_round1_inputs$schedule_key, retest_schedule_tbl$schedule_key)] <- retest_round1_inputs$p_value
retest_round1 <- simulate_frozen_round(
  graph = retest_graph,
  submission = 1L,
  round_number = 1L,
  hypotheses = c("H1", "H2"),
  plan_tbl = plan_tbl,
  schedule_tbl = retest_schedule_tbl
)
stopifnot(identical(as.character(retest_round1$history_rows$hypothesis), c("H1", "H2", "H1")))
stopifnot(identical(as.character(retest_round1$history_rows$decision), c("Do not reject", "Reject", "Reject")))
stopifnot(abs(as.numeric(retest_round1$history_rows$current_alpha[[3]]) - 0.0175) < 1e-12)

cat("Frozen round recycling verification case\n")
cat("- Round 2 rejects H2 after H1 recycles alpha within the same analysis round.\n")
cat("- H2 history stores the package-tested alpha of 0.0225, not the stale pre-submit 0.015.\n")
cat("- H3 becomes ready at alpha 0.025 after the round-2 package decisions are committed.\n")
cat("- The boundary preview, live analysis state, and round-entry helper all agree on the post-submit state.\n")
cat("- Same-analysis recycling keeps package event order in frozen history (H1, H2, H1 retest).\n")
cat("\nRound 2 summary rows:\n")
print(round2$summary_rows)
cat("\nBoundary preview after round 2:\n")
print(preview_after_round2 %>% dplyr::select(hypothesis, hypothesis_stage, current_alpha, status, analysis_round))
cat("\nLive analysis state after round 2:\n")
print(live_state_after_round2)
cat("\nRound-entry state after round 2:\n")
print(round2_state_after_recycling$remaining_rows %>% dplyr::select(hypothesis, hypothesis_stage, current_alpha, status, analysis_round))
