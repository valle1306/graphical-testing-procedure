script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_design_signature_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

# Source common_helpers.R in a mock environment with the helpers it needs
mock_env <- new.env(parent = globalenv())
mock_env$rv <- list()
mock_env$format_plain_number <- function(x) format(as.numeric(x), trim = TRUE, scientific = FALSE)

library(dplyr)

# Source common_helpers to get sanitize and signature functions
source(file.path(project_root, "R", "server", "common_helpers.R"), local = mock_env)

# Build two different plan tables
plan_a <- data.frame(
  id = 1:3,
  hypothesis = c("H1", "H2", "H3"),
  planned_analyses = c(2L, 1L, 1L),
  alpha_spending = c("O'Brien-Fleming", "Pocock", "Haybittle-Peto"),
  custom_cumulative_alpha = c("", "", ""),
  hsd_gamma = c(-4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003),
  stringsAsFactors = FALSE
)

plan_b <- data.frame(
  id = 1:3,
  hypothesis = c("H1", "H2", "H3"),
  planned_analyses = c(3L, 1L, 1L),
  alpha_spending = c("O'Brien-Fleming", "Pocock", "Haybittle-Peto"),
  custom_cumulative_alpha = c("", "", ""),
  hsd_gamma = c(-4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003),
  stringsAsFactors = FALSE
)

plan_c <- plan_a
plan_c$haybittle_p1[3] <- 0.001

schedule_a <- data.frame(
  schedule_key = c("H1_1", "H1_2", "H2_1", "H3_1"),
  analysis_round = c(1L, 2L, 2L, 2L),
  hypothesis = c("H1", "H1", "H2", "H3"),
  hypothesis_id = c(1L, 1L, 2L, 3L),
  hypothesis_stage = c(1L, 2L, 1L, 1L),
  planned_analyses = c(2L, 2L, 1L, 1L),
  information_fraction = c(0.5, 1.0, 1.0, 1.0),
  is_final = c(FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

schedule_b <- data.frame(
  schedule_key = c("H1_1", "H1_2", "H1_3", "H2_1", "H3_1"),
  analysis_round = c(1L, 2L, 3L, 3L, 3L),
  hypothesis = c("H1", "H1", "H1", "H2", "H3"),
  hypothesis_id = c(1L, 1L, 1L, 2L, 3L),
  hypothesis_stage = c(1L, 2L, 3L, 1L, 1L),
  planned_analyses = c(3L, 3L, 3L, 1L, 1L),
  information_fraction = c(0.33, 0.67, 1.0, 1.0, 1.0),
  is_final = c(FALSE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

sig_a <- mock_env$gs_current_design_signature(plan_tbl = plan_a, schedule_tbl = schedule_a)
sig_b <- mock_env$gs_current_design_signature(plan_tbl = plan_b, schedule_tbl = schedule_b)
sig_c <- mock_env$gs_current_design_signature(plan_tbl = plan_c, schedule_tbl = schedule_a)
sig_a2 <- mock_env$gs_current_design_signature(plan_tbl = plan_a, schedule_tbl = schedule_a)

# Test 1: Same inputs produce same signature
stopifnot(identical(sig_a, sig_a2))

# Test 2: Different inputs produce different signatures
stopifnot(!identical(sig_a, sig_b))
stopifnot(!identical(sig_a, sig_c))

# Test 3: Signatures are non-empty strings
stopifnot(is.character(sig_a) && nzchar(sig_a))
stopifnot(is.character(sig_b) && nzchar(sig_b))
stopifnot(is.character(sig_c) && nzchar(sig_c))

# Test 4: Signature contains expected hypothesis names
stopifnot(grepl("H1", sig_a, fixed = TRUE))
stopifnot(grepl("H2", sig_a, fixed = TRUE))
stopifnot(grepl("H3", sig_a, fixed = TRUE))

# Test 5: Changing only the schedule changes the signature
sig_mixed <- mock_env$gs_current_design_signature(plan_tbl = plan_a, schedule_tbl = schedule_b)
stopifnot(!identical(sig_a, sig_mixed))

cat("Design-signature verification case\n\n")
cat("Expected behavior:\n")
cat("- Same plan + schedule produce identical signatures (deterministic).\n")
cat("- Different plans produce different signatures.\n")
cat("- Changing only the schedule also changes the signature.\n")
cat("- Signatures are non-empty and contain hypothesis names.\n")
cat("\nSignature A:", sig_a, "\n")
cat("Signature B:", sig_b, "\n")
cat("Signature C:", sig_c, "\n")
cat("Signature A (recomputed):", sig_a2, "\n")
cat("Signature mixed (plan A + schedule B):", sig_mixed, "\n")
cat("\nAll design-signature assertions passed.\n")
