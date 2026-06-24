script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_batch_analysis_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(TrialSimulator))

graph <- TrialSimulator::GraphicalTesting$new(
  alpha = c(0.02, 0.015, 0.015, 0),
  transition = matrix(
    c(
      0,   0,   0,   1.0,
      0,   0,   0.5, 0.5,
      0,   0,   0,   1.0,
      0,   0,   0,   0
    ),
    nrow = 4,
    byrow = TRUE
  ),
  alpha_spending = c("asOF", "asP", "asOF", "asOF"),
  planned_max_info = rep(100L, 4),
  hypotheses = c("H1", "H2", "H3", "H4"),
  silent = TRUE
)

round1 <- data.frame(
  order = c(1L, 1L),
  hypotheses = c("H1", "H2"),
  p = c(0.001, 0.001),
  info = c(50L, 50L),
  is_final = c(FALSE, FALSE),
  max_info = c(100L, 100L),
  alpha_spent = c(NA_real_, NA_real_),
  stringsAsFactors = FALSE
)

round2 <- data.frame(
  order = c(2L, 2L),
  hypotheses = c("H3", "H4"),
  p = c(0.002, 0.004),
  info = c(100L, 100L),
  is_final = c(TRUE, TRUE),
  max_info = c(100L, 100L),
  alpha_spent = c(NA_real_, NA_real_),
  stringsAsFactors = FALSE
)

invisible(graph$test(round1))
results_after_round1 <- graph$get_current_testing_results()
alpha_after_round1 <- vapply(
  c("H1", "H2", "H3", "H4"),
  function(hyp) {
    hid <- graph$get_hid(hyp)
    if (!graph$is_in_graph(hid)) {
      return(0)
    }
    graph$get_alpha(hid)
  },
  numeric(1)
)

invisible(graph$test(round2))
results_after_round2 <- graph$get_current_testing_results()
alpha_after_round2 <- vapply(
  c("H1", "H2", "H3", "H4"),
  function(hyp) {
    hid <- graph$get_hid(hyp)
    if (!graph$is_in_graph(hid)) {
      return(0)
    }
    graph$get_alpha(hid)
  },
  numeric(1)
)

stopifnot(nrow(results_after_round1) > 0)
stopifnot(nrow(results_after_round2) > 0)
stopifnot(all(alpha_after_round1 >= 0))
stopifnot(all(alpha_after_round2 >= 0))
stopifnot(alpha_after_round1["H3"] > 0)
stopifnot(alpha_after_round1["H4"] > 0)

cat("Group sequential batch-analysis verification scaffold\n\n")
cat("Expected behavior:\n")
cat("- One submitted analysis look can include multiple hypotheses.\n")
cat("- Rejecting hypotheses in the same round recycles alpha before the next round.\n")
cat("- Non-active hypotheses remain absent from the batch input.\n")
cat("- The design can be exercised round-by-round rather than one hypothesis at a time.\n")
cat("\nResults after round 1:\n")
print(results_after_round1)
cat("\nAlpha after round 1:\n")
print(alpha_after_round1)
cat("\nResults after round 2:\n")
print(results_after_round2)
cat("\nAlpha after round 2:\n")
print(alpha_after_round2)
