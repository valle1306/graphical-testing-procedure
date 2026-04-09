# Verify Hwang-Shih-DeCani spending function support end-to-end
# Tests: boundary computation, comparison with gsDesign, mixed-rule designs,
# and graphical testing with reject/accept and weight reallocation.
local_lib <- file.path(getwd(), ".Rlibs")
if (dir.exists(local_lib)) .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))

library(dplyr)
library(tibble)
library(gsDesign)
library(TrialSimulator)

source(file.path("R", "server", "common_helpers.R"), local = TRUE)
source("Haybittle-Peto.r", local = TRUE)

# Provide format_plain_number (defined in app.R server closure at runtime)
format_plain_number <- function(x) format(as.numeric(x), trim = TRUE, scientific = FALSE)

# Test 1: HSD boundary computation matches gsDesign directly
cat("Test 1: HSD boundary computation vs gsDesign...\n")

timing <- c(0.5, 1.0)
total_alpha <- 0.025
gamma_val <- -4

# Our implementation
our_result <- compute_boundary_schedule(
  total_alpha = total_alpha,
  spending_type = "HSD",
  timing = timing,
  hsd_gamma = gamma_val
)

# Direct gsDesign
gs_ref <- gsDesign::gsDesign(
  k = 2, alpha = total_alpha, timing = timing,
  sfu = gsDesign::sfHSD, sfupar = gamma_val, test.type = 1
)

cat("  Our z-boundaries:", format_plain_number(our_result$z_boundary[1]), format_plain_number(our_result$z_boundary[2]), "\n")
cat("  gsDesign z-bounds:", format_plain_number(gs_ref$upper$bound[1]), format_plain_number(gs_ref$upper$bound[2]), "\n")
stopifnot(all(abs(our_result$z_boundary - gs_ref$upper$bound) < 1e-4))
cat("  PASS: z-boundaries match gsDesign\n")

# Test 2: HSD with different gamma values
cat("Test 2: HSD with varying gamma...\n")

timing3 <- c(1/3, 2/3, 1)
for (gamma in c(-8, -4, -2, -1, 0.5, 1, 2, 4)) {
  our <- compute_boundary_schedule(total_alpha = 0.025, spending_type = "HSD", timing = timing3, hsd_gamma = gamma)
  ref <- gsDesign::gsDesign(k = 3, alpha = 0.025, timing = timing3, sfu = gsDesign::sfHSD, sfupar = gamma, test.type = 1)
  stopifnot(all(abs(our$z_boundary - ref$upper$bound) < 1e-4))
}
cat("  PASS: all gamma values (-8 to 4) produce correct boundaries\n")

# Test 3: HSD appears in rule choices and normalizes correctly
cat("Test 3: Rule choices and normalization...\n")
choices <- gs_rule_choices()
stopifnot("Hwang-Shih-DeCani" %in% names(choices))
stopifnot(choices[["Hwang-Shih-DeCani"]] == "HSD")
stopifnot(normalize_spending_rule("Hwang-Shih-DeCani") == "HSD")
stopifnot(normalize_spending_rule("Lan-DeMets Hwang-Shih-DeCani") == "HSD")
stopifnot(normalize_spending_rule("HSD") == "HSD")
cat("  PASS: HSD in choices and normalization\n")


# Test 4: HSD runtime code maps to asUser
cat("Test 4: Runtime spending code for HSD...\n")
stopifnot(gs_runtime_spending_code("HSD") == "asUser")
stopifnot(gs_runtime_spending_code("Hwang-Shih-DeCani") == "asUser")
cat("  PASS: HSD maps to asUser\n")

# Test 5: hypothesis plan with hsd_gamma persists through sanitization
cat("Test 5: hsd_gamma in plan table...\n")
plan <- tibble::tibble(
  id = 1:2,
  hypothesis = c("H1", "H2"),
  planned_analyses = c(3L, 1L),
  alpha_spending = c("HSD", "OF"),
  custom_cumulative_alpha = c("", ""),
  hsd_gamma = c(-4, -4),
  haybittle_p1 = c(0.0003, 0.0003)
)
sanitized <- sanitize_gs_hypothesis_plan_tbl(plan)
stopifnot("hsd_gamma" %in% names(sanitized))
stopifnot("haybittle_p1" %in% names(sanitized))
stopifnot(sanitized$hsd_gamma[1] == -4)
stopifnot(sanitized$hsd_gamma[2] == -4)
stopifnot(abs(sanitized$haybittle_p1[1] - 0.0003) < 1e-12)

# Test with missing hsd_gamma column
plan_no_gamma <- plan %>% select(-hsd_gamma)
sanitized2 <- sanitize_gs_hypothesis_plan_tbl(plan_no_gamma)
stopifnot("hsd_gamma" %in% names(sanitized2))
stopifnot(sanitized2$hsd_gamma[1] == -4)  # default
plan_no_p1 <- plan %>% select(-haybittle_p1)
sanitized3 <- sanitize_gs_hypothesis_plan_tbl(plan_no_p1)
stopifnot("haybittle_p1" %in% names(sanitized3))
stopifnot(abs(sanitized3$haybittle_p1[1] - 0.0003) < 1e-12)
cat("  PASS: hsd_gamma persists and defaults correctly\n")

# Test 6: Design signature includes hsd_gamma and haybittle_p1
cat("Test 6: Design signature includes hsd_gamma and haybittle_p1...\n")
schedule <- tibble::tibble(
  schedule_key = c("1__1", "1__2", "1__3", "2__1"),
  analysis_round = c(1L, 2L, 3L, 3L),
  hypothesis = c("H1", "H1", "H1", "H2"),
  hypothesis_id = c(1L, 1L, 1L, 2L),
  hypothesis_stage = c(1L, 2L, 3L, 1L),
  planned_analyses = c(3L, 3L, 3L, 1L),
  information_fraction = c(1/3, 2/3, 1, 1),
  is_final = c(FALSE, FALSE, TRUE, TRUE)
)
sig1 <- gs_current_design_signature(plan, schedule)
plan2 <- plan
plan2$hsd_gamma[1] <- -2
sig2 <- gs_current_design_signature(plan2, schedule)
stopifnot(sig1 != sig2)
plan3 <- plan
plan3$haybittle_p1[1] <- 0.001
sig3 <- gs_current_design_signature(plan3, schedule)
stopifnot(sig1 != sig3)
cat("  PASS: changing gamma or p1 changes signature\n")

# Test 7: Haybittle-Peto boundary computation matches helper function
cat("Test 7: Haybittle-Peto boundary computation...\n")
hp_our <- compute_boundary_schedule(
  total_alpha = 0.025,
  spending_type = "Haybittle-Peto",
  timing = c(0.5, 1.0),
  haybittle_p1 = 0.0003
)
hp_ref <- HP(p1 = 0.0003, overall.alpha = 0.025, timing = c(0.5, 1.0))
stopifnot(all(abs(hp_our$z_boundary - hp_ref$z) < 1e-4))
stopifnot(all(abs(hp_our$p_boundary - hp_ref$p) < 1e-4))
cat("  PASS: Haybittle-Peto boundaries match helper\n")

# Test 8: Custom cumulative alpha parsing uses absolute values
cat("Test 8: Custom cumulative alpha parsing...\n")
custom_ok <- parse_custom_cumulative_alpha("0.001, 0.025", planned_analyses = 2, total_alpha = 0.025)
stopifnot(isTRUE(custom_ok$ok))
stopifnot(abs(custom_ok$absolute_values[1] - 0.001) < 1e-12)
stopifnot(abs(custom_ok$absolute_values[2] - 0.025) < 1e-12)
stopifnot(abs(custom_ok$proportions[1] - 0.04) < 1e-8)
stopifnot(abs(custom_ok$proportions[2] - 1.0) < 1e-8)
custom_bad <- parse_custom_cumulative_alpha("0.5, 1", planned_analyses = 2, total_alpha = 0.025)
stopifnot(!isTRUE(custom_bad$ok))
cat("  PASS: custom cumulative alpha is validated as absolute values\n")

# Test 9: Mixed-rule design with HSD + graphical rejection and reallocation
cat("Test 9: Mixed-rule graphical testing with rejection and weight reallocation...\n")

# 3-hypothesis graph: H1 (alpha=0.01, HSD), H2 (alpha=0.04, OF), H3 (alpha=0, Pocock)
# Transition: H1->H2=0.5, H1->H3=0.5, H2->H1=0.5, H2->H3=0.5, H3->H1=0.5, H3->H2=0.5
alphas <- c(0.01, 0.04, 0)
names(alphas) <- c("H1", "H2", "H3")
transition <- matrix(
  c(0, 0.5, 0.5,
    0.5, 0, 0.5,
    0.5, 0.5, 0),
  nrow = 3, byrow = TRUE
)
rownames(transition) <- colnames(transition) <- c("H1", "H2", "H3")

# H1 uses HSD with gamma=-4, 2 looks; H2 uses OF, 1 look; H3 uses Pocock, 1 look
# All route to asUser for HSD, asOF for OF, asP for Pocock
spending_codes <- c("asUser", "asOF", "asP")

gt <- GraphicalTesting$new(
  alpha = alphas,
  transition = transition,
  alpha_spending = spending_codes,
  planned_max_info = c(100L, 100L, 100L),
  hypotheses = c("H1", "H2", "H3"),
  silent = TRUE
)

# Helper to extract current alpha for all hypotheses
get_all_alpha <- function(gt_obj, hyp_names) {
  vapply(hyp_names, function(h) {
    hid <- gt_obj$get_hid(h)
    if (!gt_obj$is_in_graph(hid)) return(0)
    gt_obj$get_alpha(hid)
  }, numeric(1))
}

# Verify initial state
initial_alpha <- get_all_alpha(gt, c("H1", "H2", "H3"))
cat("  Initial alphas: H1=", format_plain_number(initial_alpha[["H1"]]),
    " H2=", format_plain_number(initial_alpha[["H2"]]),
    " H3=", format_plain_number(initial_alpha[["H3"]]), "\n")
stopifnot(abs(initial_alpha[["H1"]] - 0.01) < 1e-12)
stopifnot(abs(initial_alpha[["H2"]] - 0.04) < 1e-12)
stopifnot(abs(initial_alpha[["H3"]] - 0) < 1e-12)

# H1 HSD boundaries for 2 looks at information fractions 0.5 and 1.0
h1_boundaries <- compute_boundary_schedule(
  total_alpha = 0.01, spending_type = "HSD", timing = c(0.5, 1.0), hsd_gamma = -4
)
cat("  H1 boundary p at look 1:", format_plain_number(h1_boundaries$p_boundary[1]), "\n")
cat("  H1 boundary p at look 2:", format_plain_number(h1_boundaries$p_boundary[2]), "\n")

# Submit look 1: H1 with very small p-value (should reject), H2 with p=0.5 (should not reject)
stage_df_1 <- data.frame(
  order = c(1L, 1L),
  hypotheses = c("H1", "H2"),
  p = c(0.001, 0.5),
  info = c(50L, 100L),
  is_final = c(FALSE, TRUE),
  max_info = c(100L, 100L),
  alpha_spent = c(h1_boundaries$cumulative_alpha_spent[1] / 0.01, NA_real_),  # proportion for asUser
  stringsAsFactors = FALSE
)

r1 <- gt$test(stage_df_1)
cat("  After round 1:\n")
print(r1)

# H1 should be rejected (p=0.001 < boundary)
h1_row <- r1[r1$hypothesis == "H1", ]
stopifnot(h1_row$decision == "reject")

# H2 should NOT be rejected (p=0.5 >> 0.04)
h2_row <- r1[r1$hypothesis == "H2", ]
stopifnot(h2_row$decision != "reject")

# H1's alpha (0.01) should be redistributed: 0.5 to H2, 0.5 to H3
# H2 should now have 0.04 + 0.01*0.5 = 0.045
# H3 should now have 0 + 0.01*0.5 = 0.005
alpha_r1 <- get_all_alpha(gt, c("H1", "H2", "H3"))
cat("  Alpha after round 1:", paste(names(alpha_r1), format_plain_number(alpha_r1), sep = "=", collapse = ", "), "\n")

h2_new_alpha <- alpha_r1[["H2"]]
h3_new_alpha <- alpha_r1[["H3"]]
cat("  Expected H2 alpha: 0.045, got:", format_plain_number(h2_new_alpha), "\n")
cat("  Expected H3 alpha: 0.005, got:", format_plain_number(h3_new_alpha), "\n")
stopifnot(abs(h2_new_alpha - 0.045) < 1e-8)
stopifnot(abs(h3_new_alpha - 0.005) < 1e-8)

# Total alpha conserved
total_remaining <- sum(alpha_r1)
cat("  Total alpha remaining:", format_plain_number(total_remaining), "(expected 0.05)\n")
stopifnot(abs(total_remaining - 0.05) < 1e-8)

cat("  PASS: HSD rejection triggers correct alpha reallocation\n")

# Test 10: Single-look HSD behaves like final analysis
cat("Test 10: Single-look HSD...\n")
single <- compute_boundary_schedule(total_alpha = 0.025, spending_type = "HSD", timing = c(1), hsd_gamma = -4)
stopifnot(abs(single$z_boundary - qnorm(1 - 0.025)) < 1e-8)
stopifnot(abs(single$cumulative_alpha_spent - 0.025) < 1e-8)
cat("  PASS: single-look uses full alpha at z =", format_plain_number(single$z_boundary), "\n")

cat("\nAll HSD verification assertions passed.\n")
