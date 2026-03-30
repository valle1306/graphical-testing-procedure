script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_complex_sequential_case.R")
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
  planned_max_info = c(100L, 120L, 150L, 100L),
  hypotheses = c("H1", "H2", "H3", "H4"),
  silent = TRUE
)

stage1 <- data.frame(
  order = c(1L, 1L),
  hypotheses = c("H1", "H2"),
  p = c(0.001, 0.001),
  info = c(60L, 80L),
  is_final = c(FALSE, FALSE),
  max_info = c(100L, 120L),
  alpha_spent = c(NA_real_, NA_real_)
)

stage2 <- data.frame(
  order = c(2L, 2L),
  hypotheses = c("H3", "H4"),
  p = c(0.002, 0.004),
  info = c(150L, 100L),
  is_final = c(TRUE, TRUE),
  max_info = c(150L, 100L),
  alpha_spent = c(NA_real_, NA_real_)
)

invisible(graph$test(stage1))
results_after_stage1 <- graph$get_current_testing_results()
alpha_after_stage1 <- vapply(
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

invisible(graph$test(stage2))
results_after_stage2 <- graph$get_current_testing_results()
alpha_after_stage2 <- vapply(
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

cat("Complex verification case: four hypotheses, branching edges, two analysis stages\n\n")
cat("Stage 1 expected: H1 reject, H2 reject, alpha moves to H3 and H4.\n")
print(results_after_stage1)
cat("\nAlpha after stage 1:\n")
print(alpha_after_stage1)

cat("\nStage 2 expected: H3 reject at final, H4 reject at final.\n")
print(results_after_stage2)
cat("\nAlpha after stage 2:\n")
print(alpha_after_stage2)
