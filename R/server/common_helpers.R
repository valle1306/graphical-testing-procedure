# ── Shared empty-table constructors ─────────────────────────────────────────

empty_gs_boundary_preview <- function() {
  tibble::tibble(
    hypothesis = character(),
    alpha_spending = character(),
    planned_analyses = integer(),
    analysis = integer(),
    timing = numeric(),
    current_alpha = numeric(),
    stage_alpha = numeric(),
    cumulative_alpha_spent = numeric(),
    z_boundary = numeric(),
    p_boundary = numeric(),
    status = character(),
    analysis_round = integer(),
    hypothesis_stage = integer(),
    schedule_key = character(),
    is_final = logical(),
    max_info = numeric()
  )
}

empty_gs_stage_history <- function() {
  tibble::tibble(
    submission = integer(),
    hypothesis = character(),
    alpha_spending = character(),
    analysis = integer(),
    timing = numeric(),
    current_alpha = numeric(),
    p_value = numeric(),
    boundary_p = numeric(),
    boundary_z = numeric(),
    decision = character()
  )
}

empty_gs_hypothesis_plan <- function() {
  tibble::tibble(
    id = integer(),
    hypothesis = character(),
    planned_analyses = integer(),
    alpha_spending = character(),
    custom_cumulative_alpha = character(),
    hsd_gamma = numeric(),
    haybittle_p1 = numeric()
  )
}

# ── Shared display helpers ──────────────────────────────────────────────────

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

empty_gs_analysis_schedule <- function() {
  tibble::tibble(
    schedule_key = character(),
    analysis_round = integer(),
    hypothesis = character(),
    hypothesis_id = integer(),
    hypothesis_stage = integer(),
    planned_analyses = integer(),
    information_fraction = numeric(),
    is_final = logical()
  )
}

empty_gs_analysis_history <- function() {
  tibble::tibble(
    submission = integer(),
    schedule_key = character(),
    analysis_round = integer(),
    hypothesis = character(),
    hypothesis_stage = integer(),
    alpha_spending = character(),
    runtime_spending_code = character(),
    information_fraction = numeric(),
    current_alpha = numeric(),
    cumulative_alpha_spent = numeric(),
    p_value = numeric(),
    boundary_p = numeric(),
    boundary_z = numeric(),
    decision = character(),
    is_final = logical(),
    max_info = numeric()
  )
}

sanitize_gs_hypothesis_plan_tbl <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_gs_hypothesis_plan())
  }
  out <- tibble::as_tibble(df)
  out$id <- as.integer(out$id)
  out$hypothesis <- as.character(out$hypothesis)
  out$planned_analyses <- as.integer(out$planned_analyses)
  out$alpha_spending <- vapply(out$alpha_spending, normalize_spending_rule, character(1))
  if (!"custom_cumulative_alpha" %in% names(out)) {
    out$custom_cumulative_alpha <- rep("", nrow(out))
  }
  out$custom_cumulative_alpha <- as.character(out$custom_cumulative_alpha)
  if (!"hsd_gamma" %in% names(out)) {
    out$hsd_gamma <- rep(-4, nrow(out))
  }
  out$hsd_gamma <- suppressWarnings(as.numeric(out$hsd_gamma))
  out$hsd_gamma[is.na(out$hsd_gamma)] <- -4
  if (!"haybittle_p1" %in% names(out)) {
    out$haybittle_p1 <- rep(3e-04, nrow(out))
  }
  out$haybittle_p1 <- suppressWarnings(as.numeric(out$haybittle_p1))
  out$haybittle_p1[is.na(out$haybittle_p1)] <- 3e-04
  out %>%
    dplyr::select(id, hypothesis, planned_analyses, alpha_spending, custom_cumulative_alpha, hsd_gamma, haybittle_p1)
}

sanitize_gs_analysis_schedule_tbl <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_gs_analysis_schedule())
  }
  out <- tibble::as_tibble(df)
  if (!"schedule_key" %in% names(out)) {
    out$schedule_key <- paste(out$hypothesis_id, out$hypothesis_stage, sep = "__")
  }
  out$schedule_key <- as.character(out$schedule_key)
  out$analysis_round <- as.integer(out$analysis_round)
  out$hypothesis <- as.character(out$hypothesis)
  out$hypothesis_id <- as.integer(out$hypothesis_id)
  out$hypothesis_stage <- as.integer(out$hypothesis_stage)
  out$planned_analyses <- as.integer(out$planned_analyses)
  out$information_fraction <- as.numeric(out$information_fraction)
  if (!"is_final" %in% names(out)) {
    out$is_final <- out$hypothesis_stage == out$planned_analyses
  }
  out$is_final <- as.logical(out$is_final)
  out %>%
    dplyr::select(
      schedule_key,
      analysis_round,
      hypothesis,
      hypothesis_id,
      hypothesis_stage,
      planned_analyses,
      information_fraction,
      is_final
    )
}

sanitize_gs_analysis_history_tbl <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_gs_analysis_history())
  }
  out <- tibble::as_tibble(df)
  defaults <- empty_gs_analysis_history()
  for (nm in names(defaults)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- defaults[[nm]]
    }
  }
  out$submission <- as.integer(out$submission)
  out$schedule_key <- as.character(out$schedule_key)
  out$analysis_round <- as.integer(out$analysis_round)
  out$hypothesis <- as.character(out$hypothesis)
  out$hypothesis_stage <- as.integer(out$hypothesis_stage)
  out$alpha_spending <- as.character(out$alpha_spending)
  out$runtime_spending_code <- as.character(out$runtime_spending_code)
  out$information_fraction <- as.numeric(out$information_fraction)
  out$current_alpha <- as.numeric(out$current_alpha)
  out$cumulative_alpha_spent <- as.numeric(out$cumulative_alpha_spent)
  out$p_value <- as.numeric(out$p_value)
  out$boundary_p <- as.numeric(out$boundary_p)
  out$boundary_z <- as.numeric(out$boundary_z)
  out$decision <- as.character(out$decision)
  out$decision <- vapply(out$decision, normalize_gs_decision_label, character(1))
  out$is_final <- as.logical(out$is_final)
  out$max_info <- as.numeric(out$max_info)
  out %>%
    dplyr::select(names(defaults))
}

sanitize_gs_boundary_preview_tbl <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_gs_boundary_preview())
  }
  out <- tibble::as_tibble(df)
  defaults <- empty_gs_boundary_preview()
  for (nm in names(defaults)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- defaults[[nm]]
    }
  }
  out$hypothesis <- as.character(out$hypothesis)
  out$alpha_spending <- as.character(out$alpha_spending)
  out$planned_analyses <- as.integer(out$planned_analyses)
  out$analysis <- as.integer(out$analysis)
  out$timing <- as.numeric(out$timing)
  out$current_alpha <- as.numeric(out$current_alpha)
  out$stage_alpha <- as.numeric(out$stage_alpha)
  out$cumulative_alpha_spent <- as.numeric(out$cumulative_alpha_spent)
  out$z_boundary <- as.numeric(out$z_boundary)
  out$p_boundary <- as.numeric(out$p_boundary)
  out$status <- as.character(out$status)
  out$analysis_round <- as.integer(out$analysis_round)
  out$hypothesis_stage <- as.integer(out$hypothesis_stage)
  out$schedule_key <- as.character(out$schedule_key)
  out$is_final <- as.logical(out$is_final)
  out$max_info <- as.numeric(out$max_info)
  out %>%
    dplyr::select(names(defaults))
}

gs_design_alpha_lookup <- function(
  nodes_tbl = if (exists("rv", inherits = TRUE) && !is.null(rv$nodes)) rv$nodes else NULL,
  fallback = NULL
) {
  if (!is.null(nodes_tbl) && NROW(nodes_tbl) > 0 && all(c("hypothesis", "alpha") %in% names(nodes_tbl))) {
    return(stats::setNames(as.numeric(nodes_tbl$alpha), as.character(nodes_tbl$hypothesis)))
  }
  if (!is.null(fallback) && length(fallback)) {
    return(stats::setNames(as.numeric(fallback), names(fallback)))
  }
  if (exists("get_current_allocations", mode = "function")) {
    allocations <- get_current_allocations()
    return(stats::setNames(as.numeric(allocations), names(allocations)))
  }
  numeric(0)
}

set_gs_round_feedback <- function(text = NULL, type = c("success", "error")) {
  if (is.null(text) || !nzchar(trimws(as.character(text[[1]])))) {
    rv$gs_round_feedback <- NULL
    return(invisible(NULL))
  }
  rv$gs_round_feedback <- list(
    text = as.character(text[[1]]),
    type = match.arg(type)
  )
  invisible(NULL)
}

gs_round_submission_feedback_text <- function(
  analysis_round,
  hypothesis_count,
  next_round = NA_integer_
) {
  analysis_round <- coerce_scalar_integer(analysis_round, default = NA_integer_, minimum = 1L)
  hypothesis_count <- coerce_scalar_integer(hypothesis_count, default = NA_integer_, minimum = 0L)
  next_round <- coerce_scalar_integer(next_round, default = NA_integer_, minimum = 1L)

  if (is.na(analysis_round) || is.na(hypothesis_count)) {
    return("")
  }

  noun <- if (identical(hypothesis_count, 1L)) "hypothesis" else "hypotheses"
  message <- sprintf(
    "Saved analysis round %s for %s %s.",
    analysis_round,
    hypothesis_count,
    noun
  )
  if (!is.na(next_round) && !identical(next_round, analysis_round)) {
    message <- sprintf("%s Now showing Round %s.", message, next_round)
  }
  message
}

gs_rule_choices <- function(include_custom = TRUE) {
  choices <- c(
    "O'Brien-Fleming" = "OF",
    "Pocock" = "Pocock",
    "Hwang-Shih-DeCani" = "HSD",
    "Haybittle-Peto" = "Haybittle-Peto"
  )
  if (isTRUE(include_custom)) {
    choices <- c(choices, "Custom cumulative alpha" = "Custom")
  }
  choices
}

gs_runtime_spending_code <- function(rule) {
  normalized <- normalize_spending_rule(rule)
  dplyr::case_when(
    identical(normalized, "OF") ~ "asOF",
    identical(normalized, "Pocock") ~ "asP",
    TRUE ~ "asUser"
  )
}

gs_schedule_key <- function(hypothesis_id, hypothesis_stage) {
  paste(as.integer(hypothesis_id), as.integer(hypothesis_stage), sep = "__")
}

gs_default_rounds <- function(planned_analyses, total_rounds) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = 1L, minimum = 1L)
  total_rounds <- max(planned_analyses, coerce_scalar_integer(total_rounds, default = planned_analyses, minimum = planned_analyses))
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

gs_current_design_signature <- function(
  plan_tbl = rv$gs_hypothesis_plan,
  schedule_tbl = rv$gs_analysis_schedule
) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  paste(
    paste(
      paste(
        plan_tbl$id,
        plan_tbl$hypothesis,
        plan_tbl$planned_analyses,
        plan_tbl$alpha_spending,
        trimws(plan_tbl$custom_cumulative_alpha),
        format_plain_number(plan_tbl$hsd_gamma),
        format_plain_number(plan_tbl$haybittle_p1),
        sep = ":"
      ),
      collapse = "|"
    ),
    paste(
      paste(
        schedule_tbl$schedule_key,
        schedule_tbl$analysis_round,
        schedule_tbl$hypothesis,
        schedule_tbl$hypothesis_stage,
        format_plain_number(schedule_tbl$information_fraction),
        sep = ":"
      ),
      collapse = "|"
    ),
    sep = "||"
  )
}

gs_submitted_schedule_keys <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  unique(history_tbl$schedule_key[nzchar(history_tbl$schedule_key)])
}

gs_history_to_legacy_stage_history <- function(history_tbl = rv$gs_analysis_history) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(empty_gs_stage_history())
  }
  history_tbl %>%
    dplyr::transmute(
      submission = submission,
      hypothesis = hypothesis,
      alpha_spending = alpha_spending,
      analysis = hypothesis_stage,
      timing = information_fraction,
      current_alpha = current_alpha,
      p_value = p_value,
      boundary_p = boundary_p,
      boundary_z = boundary_z,
      decision = decision
    )
}

legacy_settings_from_group_sequential_design <- function(
  plan_tbl = rv$gs_hypothesis_plan,
  schedule_tbl = rv$gs_analysis_schedule
) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  if (!nrow(plan_tbl)) {
    return(tibble::tibble(
      id = integer(),
      hypothesis = character(),
      use_override = logical(),
      alpha_spending = character(),
      planned_analyses = integer(),
      info_timing = character(),
      spending_values = character(),
      hsd_gamma = numeric(),
      haybittle_p1 = numeric()
    ))
  }
  rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    hypothesis_rows <- schedule_tbl %>%
      dplyr::filter(hypothesis_id == plan_tbl$id[[i]]) %>%
      dplyr::arrange(hypothesis_stage)
    tibble::tibble(
      id = plan_tbl$id[[i]],
      hypothesis = plan_tbl$hypothesis[[i]],
      use_override = TRUE,
      alpha_spending = plan_tbl$alpha_spending[[i]],
      planned_analyses = plan_tbl$planned_analyses[[i]],
      info_timing = if (nrow(hypothesis_rows)) {
        paste(vapply(hypothesis_rows$information_fraction, format_plain_number, character(1)), collapse = ", ")
      } else {
        default_info_timing_string(plan_tbl$planned_analyses[[i]])
      },
      spending_values = if (identical(plan_tbl$alpha_spending[[i]], "Custom")) {
        trimws(plan_tbl$custom_cumulative_alpha[[i]])
      } else {
        ""
      },
      hsd_gamma = plan_tbl$hsd_gamma[[i]],
      haybittle_p1 = plan_tbl$haybittle_p1[[i]]
    )
  })
  dplyr::bind_rows(rows)
}

build_round_submit_log <- function(history_rows) {
  if (is.null(history_rows) || !nrow(history_rows)) {
    return("No analysis rows were submitted.")
  }
  decision_summary <- paste(
    sprintf(
      "%s stage %s: %s",
      history_rows$hypothesis,
      history_rows$hypothesis_stage,
      tolower(history_rows$decision)
    ),
    collapse = "; "
  )
  c(
    sprintf(
      "Submitted group sequential analysis round %s for %s hypothesis%s.",
      history_rows$analysis_round[[1]],
      nrow(history_rows),
      ifelse(nrow(history_rows) == 1L, "", "es")
    ),
    sprintf("Decisions: %s.", decision_summary),
    format_alpha_snapshot()
  )
}

build_gs_round_result_summary <- function(history_rows) {
  history_rows <- sanitize_gs_analysis_history_tbl(history_rows)
  if (!nrow(history_rows)) {
    return(tibble::tibble(
      hypothesis = character(),
      obs_p_value = numeric(),
      max_allocated_alpha = numeric(),
      decision = character(),
      stages = integer(),
      order = integer(),
      typeOfDesign = character()
    ))
  }
  history_rows %>%
    dplyr::transmute(
      hypothesis = hypothesis,
      obs_p_value = p_value,
      max_allocated_alpha = current_alpha,
      decision = vapply(decision, normalize_gs_decision_label, character(1)),
      stages = hypothesis_stage,
      order = analysis_round,
      typeOfDesign = runtime_spending_code
    )
}

latest_gs_history_decision_map <- function(
  history_tbl = rv$gs_analysis_history,
  hypotheses = rv$nodes$hypothesis
) {
  hypotheses <- unique(as.character(hypotheses))
  result <- stats::setNames(rep("Pending", length(hypotheses)), hypotheses)
  if (!length(hypotheses)) {
    return(result)
  }
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    return(result)
  }
  latest_rows <- history_tbl %>%
    dplyr::filter(hypothesis %in% hypotheses) %>%
    dplyr::arrange(hypothesis, submission, analysis_round, hypothesis_stage) %>%
    dplyr::group_by(hypothesis) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  if (nrow(latest_rows)) {
    result[latest_rows$hypothesis] <- vapply(latest_rows$decision, normalize_gs_decision_label, character(1))
  }
  result
}

apply_frozen_gs_rejections <- function(ts_object, rejected_hypotheses) {
  rejected_hypotheses <- rejected_hypotheses[!is.na(rejected_hypotheses)]
  rejected_hypotheses <- unique(trimws(as.character(rejected_hypotheses)))
  rejected_hypotheses <- rejected_hypotheses[nzchar(rejected_hypotheses)]
  if (is.null(ts_object) || !length(rejected_hypotheses)) {
    return(invisible(FALSE))
  }
  for (hyp in rejected_hypotheses) {
    withCallingHandlers(
      ts_object$reject_a_hypothesis(hyp),
      message = function(m) invokeRestart("muffleMessage")
    )
  }
  invisible(TRUE)
}

gs_round_choice_values <- function(schedule_tbl = rv$gs_analysis_schedule) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  sort(unique(schedule_tbl$analysis_round[is.finite(schedule_tbl$analysis_round) & !is.na(schedule_tbl$analysis_round)]))
}

gs_remaining_analysis_rounds <- function(
  schedule_tbl = rv$gs_analysis_schedule,
  history_tbl = rv$gs_analysis_history
) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  if (!nrow(schedule_tbl)) {
    return(integer(0))
  }
  submitted_keys <- gs_submitted_schedule_keys(history_tbl)
  remaining_tbl <- schedule_tbl %>%
    dplyr::filter(!schedule_key %in% submitted_keys)
  if (!nrow(remaining_tbl)) {
    return(integer(0))
  }
  sort(unique(remaining_tbl$analysis_round[is.finite(remaining_tbl$analysis_round) & !is.na(remaining_tbl$analysis_round)]))
}

gs_preview_next_round_tbl <- function(
  preview_tbl = rv$gs_boundary_preview,
  history_tbl = rv$gs_analysis_history
) {
  preview_tbl <- sanitize_gs_boundary_preview_tbl(preview_tbl)
  if (!nrow(preview_tbl)) {
    return(tibble::tibble(hypothesis = character(), `Next Round` = integer()))
  }
  preview_tbl %>%
    dplyr::filter(!schedule_key %in% gs_submitted_schedule_keys(history_tbl)) %>%
    dplyr::group_by(hypothesis) %>%
    dplyr::summarise(`Next Round` = min(analysis_round), .groups = "drop")
}

gs_schedule_next_round_tbl <- function(
  schedule_tbl = rv$gs_analysis_schedule,
  history_tbl = rv$gs_analysis_history
) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  if (!nrow(schedule_tbl)) {
    return(tibble::tibble(hypothesis = character(), `Next Round` = integer()))
  }
  schedule_tbl %>%
    dplyr::filter(!schedule_key %in% gs_submitted_schedule_keys(history_tbl)) %>%
    dplyr::group_by(hypothesis) %>%
    dplyr::summarise(`Next Round` = min(analysis_round), .groups = "drop")
}

gs_analysis_schedule_round_signature <- function(schedule_tbl = rv$gs_analysis_schedule) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  if (!nrow(schedule_tbl)) {
    return("")
  }
  schedule_tbl %>%
    dplyr::arrange(schedule_key) %>%
    dplyr::transmute(
      signature = paste(
        schedule_key,
        ifelse(is.na(analysis_round), "NA", as.character(analysis_round)),
        sep = ":"
      )
    ) %>%
    dplyr::pull(signature) %>%
    paste(collapse = "|")
}

set_gs_analysis_schedule_round_signature <- function(schedule_tbl = rv$gs_analysis_schedule) {
  if (!exists("rv", inherits = TRUE)) {
    return(invisible(NULL))
  }
  signature <- gs_analysis_schedule_round_signature(schedule_tbl)
  if (!identical(rv$gs_analysis_schedule_round_signature, signature)) {
    rv$gs_analysis_schedule_round_signature <- signature
  }
  invisible(rv$gs_analysis_schedule_round_signature)
}

gs_analysis_schedule_display_tbl <- function(
  plan_tbl = rv$gs_hypothesis_plan,
  schedule_tbl = rv$gs_analysis_schedule
) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  if (!nrow(plan_tbl)) {
    return(empty_gs_analysis_schedule())
  }
  schedule_tbl <- merge_gs_schedule_with_existing(
    build_default_gs_analysis_schedule(plan_tbl),
    schedule_tbl
  )
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  if (!nrow(schedule_tbl)) {
    return(schedule_tbl)
  }
  schedule_tbl %>%
    dplyr::arrange(analysis_round, hypothesis, hypothesis_stage)
}

gs_resolve_analysis_round_target <- function(
  selected_round = NA_integer_,
  actionable_rounds = integer(0),
  next_actionable_round = NA_integer_,
  force_first_actionable = FALSE
) {
  selected_round <- coerce_scalar_integer(selected_round, default = NA_integer_, minimum = 1L)
  next_actionable_round <- coerce_scalar_integer(next_actionable_round, default = NA_integer_, minimum = 1L)
  actionable_rounds <- as.integer(actionable_rounds)
  actionable_rounds <- actionable_rounds[is.finite(actionable_rounds) & !is.na(actionable_rounds)]
  actionable_rounds <- sort(unique(actionable_rounds))

  if (!length(actionable_rounds)) {
    return(NA_integer_)
  }
  if (isTRUE(force_first_actionable)) {
    return(actionable_rounds[[1]])
  }
  if (!is.na(selected_round) && selected_round %in% actionable_rounds) {
    return(selected_round)
  }
  if (!is.na(next_actionable_round) && next_actionable_round %in% actionable_rounds) {
    return(next_actionable_round)
  }
  actionable_rounds[[1]]
}

gs_analysis_round_state <- function(
  preview_tbl = rv$gs_boundary_preview,
  history_tbl = rv$gs_analysis_history,
  selected_round = NA_integer_
) {
  preview_tbl <- sanitize_gs_boundary_preview_tbl(preview_tbl)
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  selected_round <- coerce_scalar_integer(selected_round, default = NA_integer_, minimum = 1L)
  submitted_keys <- gs_submitted_schedule_keys(history_tbl)
  remaining_tbl <- preview_tbl %>%
    dplyr::filter(!schedule_key %in% submitted_keys) %>%
    dplyr::arrange(analysis_round, hypothesis, hypothesis_stage)
  actionable_tbl <- remaining_tbl %>%
    dplyr::filter(status == "Ready")
  actionable_rounds <- sort(unique(
    actionable_tbl$analysis_round[is.finite(actionable_tbl$analysis_round) & !is.na(actionable_tbl$analysis_round)]
  ))
  next_actionable_round <- if (length(actionable_rounds)) actionable_rounds[[1]] else NA_integer_
  remaining_rows <- remaining_tbl %>%
    dplyr::filter(analysis_round == selected_round) %>%
    dplyr::arrange(hypothesis, hypothesis_stage)
  actionable_rows <- remaining_rows %>%
    dplyr::filter(status == "Ready")
  list(
    selected_round = selected_round,
    remaining_rows = remaining_rows,
    actionable_rows = actionable_rows,
    actionable_rounds = actionable_rounds,
    next_actionable_round = next_actionable_round,
    has_remaining_rows = nrow(remaining_rows) > 0L,
    has_actionable_rows = nrow(actionable_rows) > 0L,
    selected_round_complete = !nrow(actionable_rows),
    is_complete = !length(actionable_rounds)
  )
}

gs_analysis_round_closed_state_message <- function() {
  "All actionable analysis rounds have been submitted."
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

gs_custom_alpha_profile <- function(plan_row, design_alpha, current_alpha = design_alpha) {
  plan_row <- sanitize_gs_hypothesis_plan_tbl(plan_row)
  if (!nrow(plan_row)) {
    return(list(ok = FALSE, message = "Custom alpha profile requires one hypothesis row."))
  }
  design_alpha <- coerce_scalar_numeric(design_alpha, default = NA_real_)
  current_alpha <- coerce_scalar_numeric(current_alpha, default = design_alpha)
  if (!is.finite(design_alpha) || design_alpha <= 0) {
    return(list(ok = FALSE, message = "Custom cumulative alpha requires a positive initial design alpha."))
  }
  spend_info <- parse_custom_cumulative_alpha(
    plan_row$custom_cumulative_alpha[[1]],
    plan_row$planned_analyses[[1]],
    total_alpha = design_alpha,
    allow_legacy_proportions = FALSE,
    allow_total_mismatch = FALSE
  )
  if (!isTRUE(spend_info$ok)) {
    return(spend_info)
  }
  if (!is.finite(current_alpha) || current_alpha <= 0) {
    current_alpha <- design_alpha
  }
  cumulative_alpha <- as.numeric(spend_info$proportions) * current_alpha
  list(
    ok = TRUE,
    message = NULL,
    proportions = as.numeric(spend_info$proportions),
    cumulative_alpha = cumulative_alpha,
    stage_alpha = c(cumulative_alpha[[1]], diff(cumulative_alpha))
  )
}

gs_custom_alpha_profile_for_hypothesis <- function(
  hypothesis,
  plan_tbl = rv$gs_hypothesis_plan,
  design_alpha_lookup = gs_design_alpha_lookup(),
  current_alpha_lookup = get_current_allocations()
) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  plan_row <- plan_tbl %>% dplyr::filter(hypothesis == !!as.character(hypothesis)) %>% dplyr::slice(1)
  design_alpha <- as.numeric(design_alpha_lookup[[as.character(hypothesis)]])
  current_alpha <- as.numeric(current_alpha_lookup[[as.character(hypothesis)]])
  gs_custom_alpha_profile(plan_row, design_alpha = design_alpha, current_alpha = current_alpha)
}

gs_custom_alpha_spent_fraction <- function(
  hypothesis,
  hypothesis_stage,
  plan_tbl = rv$gs_hypothesis_plan,
  design_alpha_lookup = gs_design_alpha_lookup(),
  current_alpha_lookup = get_current_allocations()
) {
  profile <- gs_custom_alpha_profile_for_hypothesis(
    hypothesis = hypothesis,
    plan_tbl = plan_tbl,
    design_alpha_lookup = design_alpha_lookup,
    current_alpha_lookup = current_alpha_lookup
  )
  if (!isTRUE(profile$ok)) {
    return(NA_real_)
  }
  hypothesis_stage <- coerce_scalar_integer(hypothesis_stage, default = NA_integer_, minimum = 1L)
  if (is.na(hypothesis_stage) || hypothesis_stage > length(profile$proportions)) {
    return(NA_real_)
  }
  as.numeric(profile$proportions[[hypothesis_stage]])
}

validate_gs_runtime_alpha_spent <- function(
  stage_df,
  ready_rows,
  history_tbl = rv$gs_analysis_history,
  plan_tbl = rv$gs_hypothesis_plan
) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  ready_rows <- sanitize_gs_boundary_preview_tbl(ready_rows)
  stage_df <- tibble::as_tibble(stage_df)
  if (!nrow(stage_df) || !nrow(ready_rows)) {
    return(list(ok = TRUE, message = NULL))
  }
  stage_df <- stage_df %>%
    dplyr::mutate(
      order = as.integer(order),
      hypotheses = as.character(hypotheses),
      alpha_spent = as.numeric(alpha_spent),
      is_final = as.logical(is_final)
    )
  for (i in seq_len(nrow(stage_df))) {
    runtime_code <- gs_runtime_spending_code(ready_rows$alpha_spending[[i]])
    if (!identical(runtime_code, "asUser")) {
      next
    }
    current_fraction <- as.numeric(stage_df$alpha_spent[[i]])
    if (!is.finite(current_fraction)) {
      return(list(
        ok = FALSE,
        message = sprintf(
          "%s stage %s is missing a valid cumulative alpha-spending fraction.",
          ready_rows$hypothesis[[i]],
          ready_rows$hypothesis_stage[[i]]
        )
      ))
    }
    prior_rows <- history_tbl %>%
      dplyr::filter(hypothesis == ready_rows$hypothesis[[i]], runtime_spending_code == "asUser") %>%
      dplyr::arrange(analysis_round, hypothesis_stage)
    if (!nrow(prior_rows)) {
      next
    }
    prior_fractions <- vapply(seq_len(nrow(prior_rows)), function(j) {
      prior_current_alpha <- as.numeric(prior_rows$current_alpha[[j]])
      prior_cumulative <- as.numeric(prior_rows$cumulative_alpha_spent[[j]])
      if (isTRUE(prior_rows$is_final[[j]])) {
        return(1.0)
      }
      if (!is.finite(prior_current_alpha) || prior_current_alpha <= 0) {
        return(NA_real_)
      }
      prior_cumulative / prior_current_alpha
    }, numeric(1))
    prior_fractions <- prior_fractions[is.finite(prior_fractions)]
    if (length(prior_fractions) && current_fraction <= tail(prior_fractions, 1) + 1e-12) {
      return(list(
        ok = FALSE,
        message = sprintf(
          "%s stage %s must increase cumulative alpha spending beyond the previous submitted look.",
          ready_rows$hypothesis[[i]],
          ready_rows$hypothesis_stage[[i]]
        )
      ))
    }
  }
  list(ok = TRUE, message = NULL)
}

# Boundary computation helpers (moved from app.R server closure)

normalize_spending_rule <- function(x) {
  value <- trimws(as.character(x[[1]]))
  dplyr::case_when(
    identical(value, "asOF") ~ "OF",
    identical(value, "asP") ~ "Pocock",
    identical(value, "asUser") ~ "Custom",
    identical(value, "O'Brien-Fleming") ~ "OF",
    identical(value, "Lan-DeMets O'Brien-Fleming") ~ "OF",
    identical(value, "Hwang-Shih-DeCani") ~ "HSD",
    identical(value, "Lan-DeMets Hwang-Shih-DeCani") ~ "HSD",
    identical(value, "Custom cumulative alpha") ~ "Custom",
    TRUE ~ value
  )
}

parse_numeric_sequence <- function(text_value) {
  if (is.null(text_value) || !length(text_value)) {
    return(numeric(0))
  }
  normalized <- gsub("[\r\n\t]", " ", as.character(text_value[[1]]))
  pieces <- unlist(strsplit(normalized, "[,;[:space:]]+"))
  pieces <- pieces[nzchar(pieces)]
  if (!length(pieces)) {
    return(numeric(0))
  }
  suppressWarnings(as.numeric(pieces))
}

parse_custom_cumulative_alpha <- function(
  text_value,
  planned_analyses,
  total_alpha = NULL,
  allow_legacy_proportions = FALSE,
  allow_total_mismatch = FALSE
) {
  planned_analyses <- coerce_scalar_integer(planned_analyses, default = NA_integer_, minimum = 1L)
  if (is.na(planned_analyses) || planned_analyses < 1L) {
    return(list(ok = FALSE, message = "Planned analyses must be a positive integer."))
  }
  values <- parse_numeric_sequence(text_value)
  if (length(values) != planned_analyses) {
    return(list(
      ok = FALSE,
      message = sprintf("Enter %d cumulative alpha values.", planned_analyses)
    ))
  }
  if (any(!is.finite(values)) || any(values < 0) || any(values > 1 + 1e-8)) {
    return(list(ok = FALSE, message = "Cumulative alpha values must stay between 0 and 1."))
  }
  if (any(diff(values) <= 0)) {
    return(list(ok = FALSE, message = "Cumulative alpha values must increase from one analysis to the next."))
  }

  absolute_values <- values
  proportions <- NULL
  if (!is.null(total_alpha)) {
    total_alpha <- coerce_scalar_numeric(total_alpha, default = NA_real_)
    if (!is.finite(total_alpha) || total_alpha <= 0) {
      return(list(ok = FALSE, message = "The hypothesis alpha must be positive before evaluating custom cumulative alpha."))
    }
    if (isTRUE(allow_legacy_proportions) && max(values) <= 1 + 1e-8 && abs(tail(values, 1) - 1) <= 1e-8) {
      absolute_values <- values * total_alpha
    } else if (!isTRUE(allow_total_mismatch) && abs(tail(values, 1) - total_alpha) > 1e-8) {
      return(list(
        ok = FALSE,
        message = sprintf(
          "The final cumulative alpha value must equal %s.",
          format(total_alpha, trim = TRUE, scientific = FALSE)
        )
      ))
    }
    proportions <- absolute_values / total_alpha
  }

  list(
    ok = TRUE,
    message = NULL,
    raw_values = values,
    absolute_values = absolute_values,
    proportions = proportions
  )
}

parse_information_timing <- function(text_value, planned_analyses) {
  planned_analyses <- suppressWarnings(as.integer(planned_analyses[[1]]))
  if (is.na(planned_analyses) || planned_analyses < 1L) {
    return(list(ok = FALSE, message = "Planned analyses must be a positive integer."))
  }
  values <- parse_numeric_sequence(text_value)
  if (length(values) != planned_analyses) {
    return(list(
      ok = FALSE,
      message = sprintf("Enter %d cumulative information values.", planned_analyses)
    ))
  }
  if (any(!is.finite(values)) || any(values <= 0)) {
    return(list(ok = FALSE, message = "Information timing values must be positive numbers."))
  }
  if (any(diff(values) <= 0)) {
    return(list(ok = FALSE, message = "Information timing values must increase from one analysis to the next."))
  }
  if (max(values) > 1 + 1e-8) {
    values <- values / max(values)
  }
  if (abs(tail(values, 1) - 1) > 1e-8) {
    return(list(ok = FALSE, message = "The final cumulative information value must be 1, or the final raw information count."))
  }
  list(ok = TRUE, values = values)
}

parse_spending_proportions <- function(text_value, planned_analyses) {
  parse_custom_cumulative_alpha(
    text_value = text_value,
    planned_analyses = planned_analyses,
    total_alpha = NULL,
    allow_legacy_proportions = FALSE,
    allow_total_mismatch = FALSE
  )
}

normalize_imported_custom_cumulative_alpha <- function(plan_tbl, nodes_tbl = NULL) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  if (!nrow(plan_tbl)) {
    return(plan_tbl)
  }
  if (is.null(nodes_tbl) || !nrow(nodes_tbl)) {
    return(plan_tbl)
  }
  alpha_lookup <- stats::setNames(as.numeric(nodes_tbl$alpha), as.character(nodes_tbl$hypothesis))
  rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    if (!identical(plan_tbl$alpha_spending[[i]], "Custom")) {
      return(plan_tbl[i, , drop = FALSE])
    }
    alpha_now <- as.numeric(alpha_lookup[[plan_tbl$hypothesis[[i]]]])
    if (length(alpha_now) != 1L || !is.finite(alpha_now) || alpha_now <= 0) {
      return(plan_tbl[i, , drop = FALSE])
    }
    spend_info <- parse_custom_cumulative_alpha(
      plan_tbl$custom_cumulative_alpha[[i]],
      plan_tbl$planned_analyses[[i]],
      total_alpha = alpha_now,
      allow_legacy_proportions = TRUE,
      allow_total_mismatch = TRUE
    )
    if (!isTRUE(spend_info$ok)) {
      return(plan_tbl[i, , drop = FALSE])
    }
    plan_tbl$custom_cumulative_alpha[[i]] <- format_numeric_sequence(spend_info$absolute_values)
    plan_tbl[i, , drop = FALSE]
  })
  sanitize_gs_hypothesis_plan_tbl(dplyr::bind_rows(rows))
}

build_information_correlation <- function(timing) {
  timing <- as.numeric(timing)
  k <- length(timing)
  corr <- matrix(1, nrow = k, ncol = k)
  if (k > 1L) {
    for (i in seq_len(k - 1L)) {
      for (j in seq.int(i + 1L, k)) {
        corr[i, j] <- corr[j, i] <- sqrt(timing[i] / timing[j])
      }
    }
  }
  corr
}

compute_cumulative_alpha_from_z <- function(z_values, timing) {
  corr <- build_information_correlation(timing)
  vapply(seq_along(z_values), function(k) {
    if (k == 1L) {
      return(1 - stats::pnorm(z_values[[1]]))
    }
    1 - as.numeric(mvtnorm::pmvnorm(
      lower = rep(-Inf, k),
      upper = z_values[seq_len(k)],
      corr = corr[seq_len(k), seq_len(k), drop = FALSE],
      abseps = 1e-8,
      maxpts = 100000
    ))
  }, numeric(1))
}

solve_custom_stage_boundary <- function(previous_z, target_stage_alpha, corr_prefix) {
  stage_index <- length(previous_z) + 1L
  if (!is.finite(target_stage_alpha) || target_stage_alpha <= 0) {
    return(Inf)
  }
  boundary_fn <- function(z_value) {
    as.numeric(mvtnorm::pmvnorm(
      lower = c(rep(-Inf, stage_index - 1L), z_value),
      upper = c(previous_z, Inf),
      corr = corr_prefix,
      abseps = 1e-8,
      maxpts = 100000
    )) - target_stage_alpha
  }
  lower <- -8
  upper <- 8
  f_lower <- boundary_fn(lower)
  f_upper <- boundary_fn(upper)
  while (is.finite(f_lower) && f_lower < 0 && lower > -40) {
    lower <- lower - 4
    f_lower <- boundary_fn(lower)
  }
  while (is.finite(f_upper) && f_upper > 0 && upper < 40) {
    upper <- upper + 4
    f_upper <- boundary_fn(upper)
  }
  if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
    stop("Unable to solve the custom spending boundary. Please check the cumulative spending inputs.")
  }
  stats::uniroot(boundary_fn, interval = c(lower, upper), tol = 1e-8)$root
}

solve_custom_boundaries <- function(cumulative_alpha, timing) {
  cumulative_alpha <- as.numeric(cumulative_alpha)
  stage_alpha <- c(cumulative_alpha[[1]], diff(cumulative_alpha))
  z_values <- numeric(length(cumulative_alpha))
  z_values[[1]] <- stats::qnorm(1 - cumulative_alpha[[1]])
  if (length(cumulative_alpha) > 1L) {
    corr <- build_information_correlation(timing)
    for (k in 2:length(cumulative_alpha)) {
      z_values[[k]] <- solve_custom_stage_boundary(
        previous_z = z_values[seq_len(k - 1L)],
        target_stage_alpha = stage_alpha[[k]],
        corr_prefix = corr[seq_len(k), seq_len(k), drop = FALSE]
      )
    }
  }
  tibble::tibble(
    analysis = seq_along(cumulative_alpha),
    timing = timing,
    stage_alpha = stage_alpha,
    cumulative_alpha_spent = cumulative_alpha,
    z_boundary = z_values,
    p_boundary = 1 - stats::pnorm(z_values)
  )
}

compute_boundary_schedule <- function(
  total_alpha,
  spending_type,
  timing,
  spending_values = NULL,
  hsd_gamma = -4,
  haybittle_p1 = 3e-04
) {
  total_alpha <- as.numeric(total_alpha[[1]])
  spending_type <- normalize_spending_rule(spending_type)
  timing <- as.numeric(timing)
  if (!is.finite(total_alpha) || total_alpha <= 0) {
    return(tibble::tibble(
      analysis = seq_along(timing),
      timing = timing,
      stage_alpha = NA_real_,
      cumulative_alpha_spent = NA_real_,
      z_boundary = NA_real_,
      p_boundary = NA_real_
    ))
  }
  if (length(timing) == 1L) {
    z_values <- stats::qnorm(1 - total_alpha)
    return(tibble::tibble(
      analysis = 1L,
      timing = timing,
      stage_alpha = total_alpha,
      cumulative_alpha_spent = total_alpha,
      z_boundary = z_values,
      p_boundary = 1 - stats::pnorm(z_values)
    ))
  }
  if (identical(spending_type, "Haybittle-Peto")) {
    hp_p1 <- suppressWarnings(as.numeric(haybittle_p1[[1]]))
    if (!is.finite(hp_p1)) {
      hp_p1 <- 3e-04
    }
    hp <- HP(p1 = hp_p1, overall.alpha = total_alpha, timing = timing)
    return(tibble::tibble(
      analysis = seq_along(timing),
      timing = timing,
      stage_alpha = as.numeric(hp$alpha),
      cumulative_alpha_spent = as.numeric(hp$cum.alpha),
      z_boundary = as.numeric(hp$z),
      p_boundary = as.numeric(hp$p)
    ))
  }
  if (identical(spending_type, "Custom")) {
    if (is.null(spending_values) || !length(spending_values)) {
      stop("Custom cumulative spending proportions are required for the custom rule.")
    }
    return(solve_custom_boundaries(
      cumulative_alpha = total_alpha * as.numeric(spending_values),
      timing = timing
    ))
  }
  gs_spending_fun <- switch(
    spending_type,
    "OF" = gsDesign::sfLDOF,
    "Pocock" = gsDesign::sfLDPocock,
    "HSD" = gsDesign::sfHSD,
    stop(sprintf("Unsupported spending rule: %s", spending_type))
  )
  gs_sfpar <- if (identical(spending_type, "HSD")) {
    suppressWarnings(as.numeric(hsd_gamma[[1]]))
  } else {
    NULL
  }
  gs_obj <- gsDesign::gsDesign(
    k = length(timing),
    alpha = total_alpha,
    timing = timing,
    sfu = gs_spending_fun,
    sfupar = if (!is.null(gs_sfpar) && is.finite(gs_sfpar)) gs_sfpar else NULL,
    test.type = 1
  )
  z_values <- as.numeric(gs_obj$upper$bound)
  cumulative_alpha <- compute_cumulative_alpha_from_z(z_values, timing)
  tibble::tibble(
    analysis = seq_along(timing),
    timing = timing,
    stage_alpha = c(cumulative_alpha[[1]], diff(cumulative_alpha)),
    cumulative_alpha_spent = cumulative_alpha,
    z_boundary = z_values,
    p_boundary = 1 - stats::pnorm(z_values)
  )
}

# ── Auto-layout helper ──────────────────────────────────────────────────────

auto_layout_nodes <- function(nodes_tbl) {
  if (!nrow(nodes_tbl)) return(nodes_tbl)
  n <- nrow(nodes_tbl)
  alpha <- as.numeric(nodes_tbl$alpha)
  alpha[is.na(alpha)] <- 0

  primary <- which(alpha > 0)
  secondary <- which(alpha == 0)

  if (!length(primary)) {
    primary <- seq_len(n)
    secondary <- integer(0)
  }
  if (!length(secondary)) {
    primary <- seq_len(n)
    secondary <- integer(0)
  }

  h_spacing <- 180
  v_spacing <- 200

  assign_row <- function(indices) {
    k <- length(indices)
    total_width <- (k - 1) * h_spacing
    start_x <- -total_width / 2
    x_vals <- start_x + (seq_len(k) - 1) * h_spacing
    x_vals
  }

  nodes_tbl$x <- 0
  nodes_tbl$y <- 0

  if (length(primary)) {
    nodes_tbl$x[primary] <- assign_row(primary)
    nodes_tbl$y[primary] <- 0
  }
  if (length(secondary)) {
    nodes_tbl$x[secondary] <- assign_row(secondary)
    nodes_tbl$y[secondary] <- v_spacing
  }

  nodes_tbl$x <- round(nodes_tbl$x)
  nodes_tbl$y <- round(nodes_tbl$y)
  nodes_tbl
}
