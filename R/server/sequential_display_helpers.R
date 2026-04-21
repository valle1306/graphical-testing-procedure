
# ---- Scalar coercion helpers --------------------------------------------
# Shiny inputs can arrive as NULL, empty vectors, lists, or character strings
# that failed to parse. These coercers flatten whatever came in and return a
# single safe scalar so downstream display code never has to guard against
# unexpected shapes.

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

# ---- Text / sequence formatters -----------------------------------------
# Utilities for turning raw vectors (hypotheses, analyses, numbers) into
# clean strings suitable for chips, placeholders, and readable labels.

# Deduplicate and drop blank entries, then comma-join. Returns the literal
# "none" when nothing remains so UI copy never renders an empty string.
format_hypothesis_list <- function(x) {
  values <- unique(trimws(as.character(x)))
  values <- values[nzchar(values)]
  if (!length(values)) {
    return("none")
  }
  paste(values, collapse = ", ")
}

# Build the default information-fraction schedule for k planned analyses:
# equally spaced fractions ending at 1 (e.g. k = 3 -> "0.333, 0.667, 1").
# Single-analysis designs are always at full information.
default_info_timing_string <- function(k = 2L) {
  k <- coerce_scalar_integer(k, default = 1L, minimum = 1L)
  if (k == 1L) {
    return("1")
  }
  paste(vapply(seq_len(k), function(i) format_plain_number(i / k), character(1)), collapse = ", ")
}

# Comma-separated non-scientific formatting for alpha/timing lists used in
# placeholders and previews (keeps 0.0001 readable instead of 1e-04).
format_numeric_sequence <- function(values) {
  values <- as.numeric(values)
  if (!length(values)) {
    return("")
  }
  paste(vapply(values, function(x) format(x, trim = TRUE, scientific = FALSE), character(1)), collapse = ", ")
}

# Reduce any input (NULL / vector / NA) to a single plain character string
# for HTML or label interpolation. Returns "" for anything missing so callers
# can use nzchar() as a presence check.
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

# ---- Chip / HTML label helpers ------------------------------------------
# Chips are the small pill-shaped UI elements used throughout the sequential
# tab (submission number, round/stage, decision). These helpers build the raw
# HTML so renderUI calls stay short.

# Build a <span class="gs-chip gs-chip-{variant}"> tag with optional tooltip.
# Both label and title are htmlEscape'd so user-supplied text cannot break
# out of the attribute or inject markup.
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

# Produce the "Submission N" label shown on history rows. Returns "" when
# submission cannot be coerced to a positive integer.
gs_submission_label <- function(submission) {
  submission <- coerce_scalar_integer(submission, default = NA_integer_, minimum = 1L)
  if (is.na(submission)) {
    return("")
  }
  sprintf("Submission %s", submission)
}

# Vectorized label formatter for round/stage pairs. Accepts scalar or vector
# inputs (recycled to a common length), handles NAs by emitting "", and
# appends " (final)" when is_final is TRUE for the corresponding row.
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
      "Analysis Time %s / Look %s%s",
      analysis_round[keep],
      hypothesis_stage[keep],
      ifelse(is_final[keep], " (final)", "")
    )
  }
  out
}

# UI wording distinguishes hypothesis-specific interim looks from the final
# look, even though both map back to the same stored hypothesis_stage integer.
gs_schedule_stage_label <- function(hypothesis_stage, planned_analyses, is_final = FALSE) {
  hypothesis_stage <- suppressWarnings(as.integer(unlist(hypothesis_stage, recursive = TRUE, use.names = FALSE)))
  planned_analyses <- suppressWarnings(as.integer(unlist(planned_analyses, recursive = TRUE, use.names = FALSE)))
  is_final <- as.logical(unlist(is_final, recursive = TRUE, use.names = FALSE))
  if (!length(hypothesis_stage) || !length(planned_analyses)) {
    return(character())
  }
  if (!length(is_final)) {
    is_final <- FALSE
  }
  n <- max(length(hypothesis_stage), length(planned_analyses), length(is_final))
  hypothesis_stage <- rep(hypothesis_stage, length.out = n)
  planned_analyses <- rep(planned_analyses, length.out = n)
  is_final <- rep(is_final, length.out = n)
  out <- rep("", n)
  keep <- !is.na(hypothesis_stage) & is.finite(hypothesis_stage) &
    !is.na(planned_analyses) & is.finite(planned_analyses)
  if (any(keep)) {
    out[keep] <- ifelse(
      is_final[keep],
      sprintf("Final Analysis %s of %s", hypothesis_stage[keep], planned_analyses[keep]),
      sprintf("Interim Analysis %s of %s", hypothesis_stage[keep], planned_analyses[keep])
    )
  }
  out
}
# Convenience wrapper: submission-number chip with "Analysis batch" tooltip.
gs_submission_chip_html <- function(submission) {
  gs_chip_html(gs_submission_label(submission), "submission", title = "Analysis batch")
}

# Convenience wrapper: round/stage chip with "Round / stage" tooltip.
gs_round_stage_chip_html <- function(analysis_round, hypothesis_stage, is_final = FALSE) {
  gs_chip_html(
    gs_round_stage_label(analysis_round, hypothesis_stage, is_final = is_final),
    "round-stage",
    title = "Analysis time / look"
  )
}

# Decision chip: normalizes the raw decision string to a canonical label
# ("Reject", "Do not reject", "Pending", "Not testable") and maps it to a
# CSS variant so the chip color reflects the outcome. Falls back to the
# "neutral" variant when the decision is unrecognized.
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

# ---- Custom cumulative alpha helpers ------------------------------------
# When the user selects the "custom" alpha-spending function, they must enter
# a cumulative alpha value for each planned analysis. These helpers provide
# the default schedule, a placeholder example, and the instruction text.

# Seed values for the custom alpha-spending input: a linear (Pocock-like)
# schedule that hits the total alpha at the final analysis (e.g. planned = 4
# and total_alpha = 0.025 -> c(0.00625, 0.0125, 0.01875, 0.025)). Returns an
# empty numeric vector when inputs are invalid so the caller can fall back
# to a descriptive placeholder.
default_custom_cumulative_alpha_values <- function(planned_analyses, total_alpha = NULL) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = NA_integer_, minimum = 1L)
  total_alpha <- coerce_scalar_numeric(total_alpha, default = NA_real_)
  if (is.na(planned_analyses) || planned_analyses < 1L || !is.finite(total_alpha) || total_alpha <= 0) {
    return(numeric(0))
  }
  signif((seq_len(planned_analyses) / planned_analyses) * total_alpha, digits = 6)
}

# Placeholder shown inside the custom-alpha textInput. When total_alpha is
# known we show a concrete example ("e.g. 0.00625, 0.0125, ..."); otherwise
# we fall back to a generic prompt describing the required length.
gs_custom_cumulative_alpha_placeholder <- function(planned_analyses, total_alpha = NULL) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = 1L, minimum = 1L)
  example_values <- default_custom_cumulative_alpha_values(planned_analyses, total_alpha)
  if (length(example_values)) {
    return(sprintf("e.g. %s", format_numeric_sequence(example_values)))
  }
  sprintf("Enter %d increasing values ending at the hypothesis alpha", planned_analyses)
}

# Longer helper text rendered next to the custom-alpha input. Explains the
# monotonicity requirement, the expected final value, and the recycling
# rescaling behavior so users understand why their entries may be rescaled
# later when alpha is propagated through the graph.
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

# ---- Status / decision text helpers -------------------------------------

# One-line summary of the current alpha allocation across hypotheses, used
# in the sequential panel header and in decision toasts. Defaults to pulling
# from the reactive allocation via get_current_allocations().
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

# Canonicalize the many spellings a decision string might arrive with
# (case variations, "accept" vs "do not reject", etc.) into one of the
# four canonical labels: "Reject", "Do not reject", "Pending". Unknown
# inputs are passed through unchanged so user-supplied custom decisions
# remain visible; NULL/blank returns NA_character_.
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

# ---- Display table builders ---------------------------------------------
# The sequential tab renders several DT/datatables; these functions shape
# the raw reactive state (rv$gs_boundary_preview, rv$gs_analysis_history,
# ts_status_table) into the column layout the UI expects. Each has a paired
# `empty_*` builder so renderDT never sees NULL/zero-column tibbles.

# Zero-row placeholder for the boundary review table. Column types match the
# populated version below so DT keeps the same column widths across renders.
empty_gs_boundary_review_display <- function() {
  tibble::tibble(
    `Analysis Time` = integer(),
    Hypothesis = character(),
    `Interim Analysis (Look)` = integer(),
    `Total Analyses` = integer(),
    `Info Fraction` = character(),
    `Alpha Spending` = character(),
    `Current Alpha` = character(),
    `Look Alpha` = character(),
    `Cumulative Alpha` = character(),
    `Boundary p` = character(),
    `Boundary z` = character(),
    Status = character()
  )
}

# Build the "Boundary review" table from the reactive preview. Renames the
# internal snake_case columns to human labels and formats every numeric
# column via format_plain_number, replacing NAs with "" so blank cells read
# cleanly instead of showing "NA".
gs_boundary_review_display_tbl <- function(preview_tbl = rv$gs_boundary_preview) {
  preview_tbl <- sanitize_gs_boundary_preview_tbl(preview_tbl)
  if (!nrow(preview_tbl)) {
    return(empty_gs_boundary_review_display())
  }
  preview_tbl %>%
    dplyr::transmute(
      `Analysis Time` = analysis_round,
      Hypothesis = hypothesis,
      `Interim Analysis (Look)` = hypothesis_stage,
      `Total Analyses` = planned_analyses,
      `Info Fraction` = format_plain_number(timing),
      `Alpha Spending` = vapply(alpha_spending, gs_spending_rule_label, character(1)),
      `Current Alpha` = ifelse(is.na(current_alpha), "", format_plain_number(current_alpha)),
      `Look Alpha` = ifelse(is.na(stage_alpha), "", format_plain_number(stage_alpha)),
      `Cumulative Alpha` = ifelse(is.na(cumulative_alpha_spent), "", format_plain_number(cumulative_alpha_spent)),
      `Boundary p` = ifelse(is.na(p_boundary), "", format_plain_number(p_boundary)),
      `Boundary z` = ifelse(is.na(z_boundary), "", format_plain_number(z_boundary)),
      Status = status
    )
}

# Zero-row placeholder for the per-hypothesis status summary table.
empty_gs_analysis_status_display <- function() {
  tibble::tibble(
    Hypothesis = character(),
    `Current Alpha` = character(),
    Decision = character(),
    `In Graph` = character(),
    Testable = character(),
    `Next Analysis Time` = character()
  )
}

# Compact status table joining the graph state (current alpha, in_graph,
# testable, decision) with the next planned round derived from the boundary
# preview. Used for the "Analysis status" summary card; logicals are
# rendered as "Yes"/"No" for readability.
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
      `Next Analysis Time` = ifelse(is.na(`Next Round`), "", as.character(`Next Round`))
    )
}

# Zero-row placeholder for the live analysis state table (richer variant of
# the status table, additionally includes last-submitted round/stage and p).
empty_gs_live_analysis_state_display <- function() {
  tibble::tibble(
    Hypothesis = character(),
    `Current Alpha` = character(),
    `Last Submitted Analysis Time / Look` = character(),
    `Last Observed p` = character(),
    Decision = character(),
    `In Graph` = character(),
    Testable = character(),
    `Next Analysis Time` = character()
  )
}

# For each hypothesis, pull the most recent row from the analysis history
# (sorted by submission, round, stage then taking the tail). Used as the
# "last submitted" join input for the live state table, so each hypothesis
# shows the latest round/stage/p/decision the user has submitted.
gs_last_history_by_hypothesis_tbl <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(tibble::tibble(
      hypothesis = character(),
      last_analysis_time_look = character(),
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
      last_analysis_time_look = gs_round_stage_label(analysis_round, hypothesis_stage),
      last_p_value = format_plain_number(p_value),
      last_decision = as.character(decision)
    )
}

# Full chronological listing of every analysis the user has submitted —
# one row per submission/hypothesis/stage. Feeds the "Submitted analyses"
# history table. All numeric columns routed through format_plain_number to
# avoid scientific notation.
gs_submitted_analyses_display_tbl <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(tibble::tibble(
      Submission = character(),
      `Analysis Time / Look` = character(),
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
      `Analysis Time / Look` = gs_round_stage_label(analysis_round, hypothesis_stage, is_final = is_final),
      Hypothesis = as.character(hypothesis),
      `Alpha Spending` = vapply(alpha_spending, gs_spending_rule_label, character(1)),
      `Info Fraction` = format_plain_number(information_fraction),
      `Alpha At Submission` = format_plain_number(current_alpha),
      P = format_plain_number(p_value),
      `Boundary p` = format_plain_number(boundary_p),
      `Boundary z` = format_plain_number(boundary_z),
      Decision = as.character(decision)
    )
}

# Live analysis state table — combines the current graph status with the
# last-submitted history row and the next scheduled round per hypothesis.
# The Decision column uses a fallback ladder: prefer the user's recorded
# decision, else "Reject" (already out of graph), else "Not testable"
# (still in graph but blocked by the testability predicate), else
# "Pending". This mirrors what the sequential workflow shows immediately
# after each submission.
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
      `Last Submitted Analysis Time / Look` = dplyr::coalesce(last_analysis_time_look, ""),
      `Last Observed p` = dplyr::coalesce(last_p_value, ""),
      Decision = dplyr::case_when(
        !is.na(last_decision) & nzchar(last_decision) ~ last_decision,
        !as.logical(in_graph) ~ "Reject",
        !as.logical(testable) ~ "Not testable",
        TRUE ~ "Pending"
      ),
      `In Graph` = ifelse(as.logical(in_graph), "Yes", "No"),
      Testable = ifelse(as.logical(testable), "Yes", "No"),
      # Rejected hypotheses are out of the graph, so a future scheduled time is
      # no longer actionable and should display as blanked-out UI state.
      `Next Analysis Time` = dplyr::case_when(
        !as.logical(in_graph) ~ "—",
        is.na(`Next Round`) ~ "—",
        TRUE ~ as.character(`Next Round`)
      )
    )
}
