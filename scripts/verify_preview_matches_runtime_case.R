#!/usr/bin/env Rscript
# The boundary the user reviews must be the boundary the app applies.
#
# This is the regression test for the defect described in
# docs/verification_report.md. The Group Sequential Design tab previews a
# boundary with gsDesign at a design-time information fraction; the Analysis tab
# decides rejections with TrialSimulator (rpact) at observed_info /
# planned_max_info. Those two fractions used to be independent inputs that
# nothing reconciled, so the app displayed one boundary and applied another,
# differing by as much as 1.7% relative on the p scale.
#
# The engines themselves agree exactly. What this case pins down is that the app
# now hands them the same information fractions:
#
#   1. On plan (observed count == planned count), previewed == applied, exactly.
#   2. Off plan, the boundary is legitimately recomputed at the observed
#      information, and the app does not pretend otherwise.

project_root <- normalizePath(file.path(dirname(sub(
  "--file=", "", commandArgs(FALSE)[grep("--file=", commandArgs(FALSE))][1]
)), ".."), winslash = "/", mustWork = TRUE)
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

suppressPackageStartupMessages({
  library(TrialSimulator)
  library(gsDesign)
  library(tibble)
})

source(file.path(project_root, "Haybittle-Peto.r"))
normalize_spending_rule <- function(rule) as.character(rule)[[1]]
source(file.path(project_root, "R", "server", "sequential_boundary_helpers.R"))

alpha <- 0.025
planned_max_info <- 387
planned_counts <- c(205, 285, 387)
design_fractions <- planned_counts / planned_max_info

# ---- 1. the information grid is shared ------------------------------------
stopifnot(identical(
  as.numeric(gs_planned_info_counts(design_fractions, planned_max_info)),
  as.numeric(planned_counts)
))
runtime_timing <- gs_runtime_timing(design_fractions, planned_max_info)
stopifnot(max(abs(runtime_timing - planned_counts / planned_max_info)) < 1e-12)

# ---- 2. ON PLAN: previewed boundary == applied boundary --------------------
preview <- compute_boundary_schedule(
  total_alpha = alpha, spending_type = "OF", timing = runtime_timing
)

on_plan <- GroupSequentialTest$new(
  alpha = alpha, alpha_spending = "asOF", planned_max_info = planned_max_info
)
invisible(on_plan$test(planned_counts, c(FALSE, FALSE, TRUE), c(0.5, 0.5, 0.5)))
applied <- on_plan$get_trajectory()

cat("On plan (observed == planned == 205/285/387):\n")
cmp <- tibble(
  look = seq_along(planned_counts),
  previewed_z = preview$z_boundary,
  applied_z = as.numeric(applied$criticalValues),
  previewed_p = preview$p_boundary,
  applied_p = as.numeric(applied$stageLevels)
)
print(as.data.frame(cmp), row.names = FALSE)

z_gap <- max(abs(cmp$previewed_z - cmp$applied_z))
p_gap <- max(abs(cmp$previewed_p - cmp$applied_p))
cat(sprintf("\nmax |previewed - applied|: z %.3e, p %.3e\n", z_gap, p_gap))

# Tolerance is set by rpact's own root-finding, not by any approximation of
# ours. Before the fix this gap was 2.3e-04 on the p scale.
stopifnot(z_gap < 1e-5)
stopifnot(p_gap < 1e-6)
cat("OK: the boundary shown at design time is the boundary applied at analysis.\n")

# ---- 3. OFF PLAN: the boundary legitimately moves --------------------------
# The trial over-runs: the final analysis happens at 393 units, not the planned
# 387. The applied boundary must then differ from the preview, and the app must
# not display the stale preview (see gs_round_boundary_* in
# R/server/sequential_outputs.R).
over_run <- GroupSequentialTest$new(
  alpha = alpha, alpha_spending = "asOF", planned_max_info = planned_max_info
)
invisible(over_run$test(c(205, 285, 393), c(FALSE, FALSE, TRUE), c(0.09, 0.006, 0.002)))
traj <- over_run$get_trajectory()

cat("\nOff plan (final look over-runs to 393):\n")
print(as.data.frame(traj[, c(
  "stages", "informationRates", "criticalValues", "stageLevels",
  "obs_p_value", "decision"
)]), row.names = FALSE)

# Looks 1 and 2 ran to plan, so they still match the preview exactly.
stopifnot(abs(as.numeric(traj$criticalValues)[1] - preview$z_boundary[1]) < 1e-5)
stopifnot(abs(as.numeric(traj$criticalValues)[2] - preview$z_boundary[2]) < 1e-5)
# The final look did not, so it must differ.
stopifnot(abs(as.numeric(traj$criticalValues)[3] - preview$z_boundary[3]) > 1e-4)
cat("\nOK: on-plan looks match the preview; the over-run look is recomputed.\n")

# ---- 4. the decisions the manuscript reports -------------------------------
decisions <- as.character(traj$decision)
stopifnot(identical(tolower(decisions[1]), "accept"))
stopifnot(identical(tolower(decisions[2]), "reject"))
cat("OK: H1 is not rejected at look 1 (p = 0.09) and is rejected at look 2 (p = 0.006).\n")

cat("\nverify_preview_matches_runtime_case.R passed.\n")
