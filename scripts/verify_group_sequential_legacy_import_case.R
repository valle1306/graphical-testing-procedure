script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_legacy_import_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

legacy_gs_settings <- data.frame(
  id = c(1L, 2L, 3L),
  hypothesis = c("H1", "H2", "H3"),
  use_override = c(TRUE, FALSE, TRUE),
  alpha_spending = c("OF", "Pocock", "Custom"),
  planned_analyses = c(2L, 1L, 3L),
  info_timing = c("0.5, 1", "1", "0.33, 0.67, 1"),
  spending_values = c("", "", "0.2, 0.6, 1"),
  hsd_gamma = c(-4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003),
  stringsAsFactors = FALSE
)

normalize_rule <- function(value) {
  if (identical(value, "OF")) {
    return("Lan-DeMets O'Brien-Fleming")
  }
  if (identical(value, "Pocock")) {
    return("Pocock")
  }
  if (identical(value, "Haybittle-Peto")) {
    return("Haybittle-Peto")
  }
  "Custom cumulative alpha"
}

parse_numeric_sequence <- function(text_value) {
  pieces <- unlist(strsplit(gsub("[\r\n\t]", " ", as.character(text_value[[1]])), "[,;[:space:]]+"))
  pieces <- pieces[nzchar(pieces)]
  suppressWarnings(as.numeric(pieces))
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
  as.integer(candidate)
}

normalized_plan <- data.frame(
  id = legacy_gs_settings$id,
  hypothesis = legacy_gs_settings$hypothesis,
  planned_analyses = as.integer(legacy_gs_settings$planned_analyses),
  alpha_spending = unname(vapply(legacy_gs_settings$alpha_spending, normalize_rule, character(1))),
  stringsAsFactors = FALSE
)
normalized_plan$custom_cumulative_alpha <- ifelse(
  normalized_plan$alpha_spending == "Custom cumulative alpha",
  legacy_gs_settings$spending_values,
  ""
)
normalized_plan$hsd_gamma <- legacy_gs_settings$hsd_gamma
normalized_plan$haybittle_p1 <- legacy_gs_settings$haybittle_p1
row.names(normalized_plan) <- NULL

total_rounds <- max(normalized_plan$planned_analyses)
derived_schedule <- do.call(
  rbind,
  lapply(seq_len(nrow(normalized_plan)), function(i) {
    planned_analyses <- normalized_plan$planned_analyses[[i]]
    timings <- parse_numeric_sequence(legacy_gs_settings$info_timing[[i]])
    data.frame(
      analysis_round = gs_default_rounds(planned_analyses, total_rounds),
      hypothesis = normalized_plan$hypothesis[[i]],
      hypothesis_stage = seq_len(planned_analyses),
      information_fraction = timings,
      stringsAsFactors = FALSE
    )
  })
)
derived_schedule <- derived_schedule[order(
  derived_schedule$analysis_round,
  derived_schedule$hypothesis,
  derived_schedule$hypothesis_stage
), ]
row.names(derived_schedule) <- NULL

stopifnot(nrow(normalized_plan) == 3L)
stopifnot(identical(normalized_plan$planned_analyses, c(2L, 1L, 3L)))
stopifnot(normalized_plan$alpha_spending[1] == "Lan-DeMets O'Brien-Fleming")
stopifnot(normalized_plan$alpha_spending[3] == "Custom cumulative alpha")
stopifnot(abs(normalized_plan$haybittle_p1[1] - 0.0003) < 1e-12)
stopifnot(derived_schedule$analysis_round[derived_schedule$hypothesis == "H2"][[1]] == 3L)
stopifnot(all(diff(derived_schedule$information_fraction[derived_schedule$hypothesis == "H3"]) > 0))

cat("Group sequential legacy-import verification scaffold\n\n")
cat("Expected behavior:\n")
cat("- Legacy `gs_settings` input can still be normalized during a transition period.\n")
cat("- The app can map old rule names onto the new public terminology.\n")
cat("- Planned analyses and timing strings remain import-compatible while the redesign lands.\n")
cat("- Final-only hypotheses align to the overall final analysis round after import.\n")
cat("\nNormalized legacy plan:\n")
print(normalized_plan)
cat("\nDerived schedule:\n")
print(derived_schedule)
