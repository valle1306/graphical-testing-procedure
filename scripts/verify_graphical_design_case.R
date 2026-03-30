script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_graphical_design_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(TrialSimulator))

graph <- TrialSimulator::GraphicalTesting$new(
  alpha = c(0.025, 0.025, 0),
  transition = matrix(
    c(
      0, 0, 1,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  ),
  alpha_spending = c("asOF", "asOF", "asOF"),
  planned_max_info = c(100L, 100L, 100L),
  hypotheses = c("H1", "H2", "H3"),
  silent = TRUE
)

invisible(graph$reject_a_hypothesis("H1"))

decisions <- graph$get_current_decision()
alpha_after <- vapply(
  c("H1", "H2", "H3"),
  function(hyp) {
    hid <- graph$get_hid(hyp)
    if (!graph$is_in_graph(hid)) {
      return(0)
    }
    graph$get_alpha(hid)
  },
  numeric(1)
)

cat("Classic graphical-design verification case\n\n")
cat("Expected: rejecting H1 transfers all of H1 alpha to H3.\n\n")
cat("Current decisions:\n")
print(decisions)
cat("\nCurrent alpha after rejecting H1:\n")
print(alpha_after)
