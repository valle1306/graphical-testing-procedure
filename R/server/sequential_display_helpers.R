# Shared display helpers

## This file contains helper functions for rendering the current design, schedule, and analysis state in the UI. 
## These are used across multiple server modules, so they are kept separate from the event handling and 
## state management logic in sequential_events.R and sequential_state.R.

coerce_scalar_integer <- function(x, default = NA_integer_, minimum = NULL) {
  values <- suppressWarnings(as.integer(unlist(x, recursive = TRUE, use.names = FALSE)))
  values <- values[is.finite(values) & !is.na(values)]
  if (!length(values)) {
    return(default)
  }
  value <- values[[1]]
  if (!is.null(minimum) && is.finite(minimum) && value < minimum) {
    return(as.integer(minimum))
  }
  value
}

coerce_scalar_numeric <- function(x, default = NA_real_) {
  values <- suppressWarnings(as.numeric(unlist(x, recursive = TRUE, use.names = FALSE)))
  values <- values[is.finite(values) & !is.na(values)]
  if (!length(values)) {
    return(default)
  }
  values[[1]]
}

format_hypothesis_list <- function(x) {
  values <- unique(trimws(as.character(x)))
  values <- values[nzchar(values)]
  if (!length(values)) {
    return("none")
  }
  paste(values, collapse = ", ")
}

default_info_timing_string <- function(k = 2L) {
  k <- coerce_scalar_integer(k, default = 1L, minimum = 1L)
  if (k == 1L) {
    return("1")
  }
  paste(vapply(seq_len(k), function(i) format_plain_number(i / k), character(1)), collapse = ", ")
}

format_numeric_sequence <- function(values) {
  values <- as.numeric(values)
  if (!length(values)) {
    return("")
  }
  paste(vapply(values, function(x) format(x, trim = TRUE, scientific = FALSE), character(1)), collapse = ", ")
}

gs_scalar_display_text <- function(value) {
  if (is.null(value) || !length(value)) {
    return("")
  }
  value <- value[[1]]
  if (is.null(value) || is.na(value)) {
    return("")
  }
  as.character(value)
}

gs_chip_html <- function(label, variant = "neutral", title = NULL) {
  label_text <- gs_scalar_display_text(label)
  if (!nzchar(label_text)) {
    return("")
  }
  title_text <- gs_scalar_display_text(title)
  title_attr <- if (nzchar(title_text)) {
    sprintf(' title="%s"', htmltools::htmlEscape(title_text, attribute = TRUE))
  } else {
    ""
  }
  sprintf(
    '<span class="gs-chip gs-chip-%s"%s>%s</span>',
    as.character(variant),
    title_attr,
    htmltools::htmlEscape(label_text)
  )
}

gs_submission_label <- function(submission) {
  submission <- coerce_scalar_integer(submission, default = NA_integer_, minimum = 1L)
  if (is.na(submission)) {
    return("")
  }
  sprintf("Submission %s", submission)
}

gs_round_stage_label <- function(analysis_round, hypothesis_stage, is_final = FALSE) {
  analysis_round <- suppressWarnings(as.integer(unlist(analysis_round, recursive = TRUE, use.names = FALSE)))
  hypothesis_stage <- suppressWarnings(as.integer(unlist(hypothesis_stage, recursive = TRUE, use.names = FALSE)))
  is_final <- as.logical(unlist(is_final, recursive = TRUE, use.names = FALSE))
  if (!length(analysis_round) || !length(hypothesis_stage)) {
    return(character())
  }
  if (!length(is_final)) {
    is_final <- FALSE
  }
  n <- max(length(analysis_round), length(hypothesis_stage), length(is_final))
  analysis_round <- rep(analysis_round, length.out = n)
  hypothesis_stage <- rep(hypothesis_stage, length.out = n)
  is_final <- rep(is_final, length.out = n)
  out <- rep("", n)
  keep <- !is.na(analysis_round) & is.finite(analysis_round) & !is.na(hypothesis_stage) & is.finite(hypothesis_stage)
  if (any(keep)) {
    out[keep] <- sprintf(
      "Round %s / Stage %s%s",
      analysis_round[keep],
      hypothesis_stage[keep],
      ifelse(is_final[keep], " (final)", "")
    )
  }
  out
}

gs_submission_chip_html <- function(submission) {
  gs_chip_html(gs_submission_label(submission), "submission", title = "Analysis batch")
}

gs_round_stage_chip_html <- function(analysis_round, hypothesis_stage, is_final = FALSE) {
  gs_chip_html(
    gs_round_stage_label(analysis_round, hypothesis_stage, is_final = is_final),
    "round-stage",
    title = "Round / stage"
  )
}

gs_decision_chip_html <- function(decision) {
  normalized <- normalize_gs_decision_label(decision)
  if (!nzchar(gs_scalar_display_text(normalized))) {
    normalized <- gs_scalar_display_text(decision)
  }
  if (!nzchar(normalized)) {
    return("")
  }
  variant <- switch(
    normalized,
    "Reject" = "reject",
    "Do not reject" = "keep",
    "Pending" = "pending",
    "Not testable" = "not-testable",
    "neutral"
  )
  gs_chip_html(normalized, variant, title = "Analysis decision")
}

default_custom_cumulative_alpha_values <- function(planned_analyses, total_alpha = NULL) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = NA_integer_, minimum = 1L)
  total_alpha <- coerce_scalar_numeric(total_alpha, default = NA_real_)
  if (is.na(planned_analyses) || planned_analyses < 1L || !is.finite(total_alpha) || total_alpha <= 0) {
    return(numeric(0))
  }
  signif((seq_len(planned_analyses) / planned_analyses) * total_alpha, digits = 6)
}

gs_custom_cumulative_alpha_placeholder <- function(planned_analyses, total_alpha = NULL) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = 1L, minimum = 1L)
  example_values <- default_custom_cumulative_alpha_values(planned_analyses, total_alpha)
  if (length(example_values)) {
    return(sprintf("e.g. %s", format_numeric_sequence(example_values)))
  }
  sprintf("Enter %d increasing values ending at the hypothesis alpha", planned_analyses)
}

gs_custom_cumulative_alpha_instruction <- function(planned_analyses, total_alpha = NULL) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = 1L, minimum = 1L)
  total_alpha <- coerce_scalar_numeric(total_alpha, default = NA_real_)
  final_text <- if (is.finite(total_alpha) && total_alpha > 0) {
    sprintf("The last number should be the initial allocated alpha for the hypothesis: %s", format_plain_number(total_alpha))
  } else {
    "The last number should be the initial allocated alpha for the hypothesis."
  }
  sprintf(
    paste0(
      "Enter %d cumulative alpha value%s, one per planned analysis. ",
      "The numbers must be incremental. %s ",
      "If alpha is later recycled into this hypothesis, future custom looks are automatically rescaled using the same cumulative proportions."
    ),
    planned_analyses,
    ifelse(planned_analyses == 1L, "", "s"),
    final_text
  )
}

format_alpha_snapshot <- function(allocations = get_current_allocations()) {
  if (is.null(allocations) || !length(allocations)) {
    return("Current alpha: none.")
  }
  values <- vapply(as.numeric(allocations), format_plain_number, character(1))
  paste(
    "Current alpha:",
    paste(sprintf("%s=%s", names(allocations), values), collapse = ", "),
    sep = " "
  )
}

normalize_gs_decision_label <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_character_)
  }
  value <- trimws(as.character(x[[1]]))
  if (!length(value) || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  lower <- tolower(value)
  if (identical(lower, "reject")) {
    return("Reject")
  }
  if (lower %in% c("do not reject", "donotreject", "accept", "accepted", "not reject")) {
    return("Do not reject")
  }
  if (identical(lower, "pending")) {
    return("Pending")
  }
  value
}

empty_gs_boundary_review_display <- function() {
  tibble::tibble(
    Round = integer(),
    Hypothesis = character(),
    Stage = integer(),
    `Total Analyses` = integer(),
    `Info Fraction` = character(),
    `Alpha Spending` = character(),
    `Current Alpha` = character(),
    `Stage Alpha` = character(),
    `Cumulative Alpha` = character(),
    `Boundary p` = character(),
    `Boundary z` = character(),
    Status = character()
  )
}

gs_boundary_review_display_tbl <- function(preview_tbl = rv$gs_boundary_preview) {
  preview_tbl <- sanitize_gs_boundary_preview_tbl(preview_tbl)
  if (!nrow(preview_tbl)) {
    return(empty_gs_boundary_review_display())
  }
  preview_tbl %>%
    dplyr::transmute(
      Round = analysis_round,
      Hypothesis = hypothesis,
      Stage = hypothesis_stage,
      `Total Analyses` = planned_analyses,
      `Info Fraction` = format_plain_number(timing),
      `Alpha Spending` = as.character(alpha_spending),
      `Current Alpha` = ifelse(is.na(current_alpha), "", format_plain_number(current_alpha)),
      `Stage Alpha` = ifelse(is.na(stage_alpha), "", format_plain_number(stage_alpha)),
      `Cumulative Alpha` = ifelse(is.na(cumulative_alpha_spent), "", format_plain_number(cumulative_alpha_spent)),
      `Boundary p` = ifelse(is.na(p_boundary), "", format_plain_number(p_boundary)),
      `Boundary z` = ifelse(is.na(z_boundary), "", format_plain_number(z_boundary)),
      Status = status
    )
}

empty_gs_analysis_status_display <- function() {
  tibble::tibble(
    Hypothesis = character(),
    `Current Alpha` = character(),
    Decision = character(),
    `In Graph` = character(),
    Testable = character(),
    `Next Global Round` = character()
  )
}

gs_status_display_tbl <- function(
  status_tbl = build_ts_status_table(),
  preview_tbl = rv$gs_boundary_preview,
  history_tbl = rv$gs_analysis_history
) {
  if (is.null(status_tbl) || !nrow(status_tbl)) {
    return(empty_gs_analysis_status_display())
  }
  status_tbl <- tibble::as_tibble(status_tbl)
  next_round_tbl <- gs_preview_next_round_tbl(preview_tbl = preview_tbl, history_tbl = history_tbl)
  status_tbl %>%
    dplyr::left_join(next_round_tbl, by = "hypothesis") %>%
    dplyr::transmute(
      Hypothesis = as.character(hypothesis),
      `Current Alpha` = format(as.numeric(current_alpha), trim = TRUE, scientific = FALSE),
      Decision = as.character(decision),
      `In Graph` = ifelse(as.logical(in_graph), "Yes", "No"),
      Testable = ifelse(as.logical(testable), "Yes", "No"),
      `Next Global Round` = ifelse(is.na(`Next Round`), "", as.character(`Next Round`))
    )
}

empty_gs_live_analysis_state_display <- function() {
  tibble::tibble(
    Hypothesis = character(),
    `Current Alpha` = character(),
    `Last Submitted Round / Stage` = character(),
    `Last Observed p` = character(),
    Decision = character(),
    `In Graph` = character(),
    Testable = character(),
    `Next Global Round` = character()
  )
}

gs_last_history_by_hypothesis_tbl <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(tibble::tibble(
      hypothesis = character(),
      last_round_stage = character(),
      last_p_value = character(),
      last_decision = character()
    ))
  }
  history_tbl %>%
    dplyr::arrange(submission, analysis_round, hypothesis_stage) %>%
    dplyr::group_by(hypothesis) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      hypothesis = as.character(hypothesis),
      last_round_stage = gs_round_stage_label(analysis_round, hypothesis_stage),
      last_p_value = format_plain_number(p_value),
      last_decision = as.character(decision)
    )
}

gs_submitted_analyses_display_tbl <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(tibble::tibble(
      Submission = character(),
      `Round / Stage` = character(),
      Hypothesis = character(),
      `Alpha Spending` = character(),
      `Info Fraction` = character(),
      `Alpha At Submission` = character(),
      P = character(),
      `Boundary p` = character(),
      `Boundary z` = character(),
      Decision = character()
    ))
  }
  history_tbl %>%
    dplyr::transmute(
      Submission = as.character(submission),
      `Round / Stage` = gs_round_stage_label(analysis_round, hypothesis_stage, is_final = is_final),
      Hypothesis = as.character(hypothesis),
      `Alpha Spending` = as.character(alpha_spending),
      `Info Fraction` = format_plain_number(information_fraction),
      `Alpha At Submission` = format_plain_number(current_alpha),
      P = format_plain_number(p_value),
      `Boundary p` = format_plain_number(boundary_p),
      `Boundary z` = format_plain_number(boundary_z),
      Decision = as.character(decision)
    )
}

gs_live_analysis_state_tbl <- function(
  status_tbl = build_ts_status_table(),
  schedule_tbl = rv$gs_analysis_schedule,
  history_tbl = rv$gs_analysis_history
) {
  if (is.null(status_tbl) || !nrow(status_tbl)) {
    return(empty_gs_live_analysis_state_display())
  }
  status_tbl <- tibble::as_tibble(status_tbl)
  next_round_tbl <- gs_schedule_next_round_tbl(schedule_tbl = schedule_tbl, history_tbl = history_tbl)
  history_last_tbl <- gs_last_history_by_hypothesis_tbl(history_tbl = history_tbl)
  status_tbl %>%
    dplyr::left_join(history_last_tbl, by = "hypothesis") %>%
    dplyr::left_join(next_round_tbl, by = "hypothesis") %>%
    dplyr::transmute(
      Hypothesis = as.character(hypothesis),
      `Current Alpha` = format_plain_number(current_alpha),
      `Last Submitted Round / Stage` = dplyr::coalesce(last_round_stage, ""),
      `Last Observed p` = dplyr::coalesce(last_p_value, ""),
      Decision = dplyr::case_when(
        !is.na(last_decision) & nzchar(last_decision) ~ last_decision,
        !as.logical(in_graph) ~ "Reject",
        !as.logical(testable) ~ "Not testable",
        TRUE ~ "Pending"
      ),
      `In Graph` = ifelse(as.logical(in_graph), "Yes", "No"),
      Testable = ifelse(as.logical(testable), "Yes", "No"),
      `Next Global Round` = ifelse(is.na(`Next Round`), "", as.character(`Next Round`))
    )
}
