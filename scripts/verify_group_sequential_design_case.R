script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_design_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

gs_default_rounds <- function(planned_analyses, total_rounds) {
  planned_analyses <- max(1L, as.integer(planned_analyses[[1]]))
  total_rounds <- max(planned_analyses, as.integer(total_rounds[[1]]))
  if (planned_analyses == 1L) {
    return(total_rounds)
  }
  candidate <- round(seq(1, total_rounds, length.out = planned_analyses))
  candidate[[1]] <- 1L
  candidate[[planned_analyses]] <- total_rounds
  candidate <- as.integer(candidate)
  for (i in seq.int(2L, length(candidate))) {
    if (candidate[[i]] <= candidate[[i - 1L]]) {
      candidate[[i]] <- candidate[[i - 1L]] + 1L
    }
  }
  if (tail(candidate, 1) > total_rounds) {
    candidate <- seq.int(total_rounds - planned_analyses + 1L, total_rounds)
  }
  as.integer(candidate)
}

planned_hypotheses <- data.frame(
  hypothesis = c("H1", "H2", "H3"),
  planned_analyses = c(2L, 1L, 1L),
  alpha_spending_function = c("O'Brien-Fleming", "Pocock", "Haybittle-Peto"),
  stringsAsFactors = FALSE
)

total_rounds <- max(planned_hypotheses$planned_analyses)
analysis_schedule <- do.call(
  rbind,
  lapply(seq_len(nrow(planned_hypotheses)), function(i) {
    hyp <- planned_hypotheses[i, ]
    rounds <- gs_default_rounds(hyp$planned_analyses, total_rounds)
    stages <- seq_len(hyp$planned_analyses)
    data.frame(
      analysis_round = rounds,
      hypothesis = hyp$hypothesis,
      hypothesis_stage = stages,
      information_fraction = if (hyp$planned_analyses == 1L) {
        1
      } else {
        round(stages / hyp$planned_analyses, 2)
      },
      alpha_spending_function = hyp$alpha_spending_function,
      stringsAsFactors = FALSE
    )
  })
)
analysis_schedule <- analysis_schedule[order(
  analysis_schedule$analysis_round,
  analysis_schedule$hypothesis,
  analysis_schedule$hypothesis_stage
), ]
row.names(analysis_schedule) <- NULL

expected_rounds <- data.frame(
  analysis_round = c(1L, 2L, 2L, 2L),
  hypothesis = c("H1", "H1", "H2", "H3"),
  hypothesis_stage = c(1L, 2L, 1L, 1L),
  information_fraction = c(0.5, 1.0, 1.0, 1.0),
  stringsAsFactors = FALSE
)

stopifnot(nrow(analysis_schedule) == nrow(expected_rounds))
stopifnot(identical(analysis_schedule$analysis_round, expected_rounds$analysis_round))
stopifnot(identical(analysis_schedule$hypothesis, expected_rounds$hypothesis))
stopifnot(identical(analysis_schedule$hypothesis_stage, expected_rounds$hypothesis_stage))
stopifnot(all.equal(analysis_schedule$information_fraction, expected_rounds$information_fraction, tolerance = 1e-8))

cat("Group sequential design verification scaffold\n\n")
cat("Expected behavior:\n")
cat("- One row per hypothesis in the design table.\n")
cat("- Final-only hypotheses have exactly one scheduled analysis at the overall final round and information fraction 1.0.\n")
cat("- Multi-look hypotheses produce increasing analysis rounds ending at 1.0.\n")
cat("- The derived schedule can be used to render a read-only boundary review panel.\n")
cat("\nDerived schedule:\n")
print(analysis_schedule)
