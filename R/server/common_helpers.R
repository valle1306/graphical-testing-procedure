empty_gs_hypothesis_plan <- function() {
  tibble::tibble(
    id = integer(),
    hypothesis = character(),
    planned_analyses = integer(),
    alpha_spending = character(),
    custom_cumulative_alpha = character()
  )
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
  out %>%
    dplyr::select(id, hypothesis, planned_analyses, alpha_spending, custom_cumulative_alpha)
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
  out$is_final <- as.logical(out$is_final)
  out$max_info <- as.numeric(out$max_info)
  out %>%
    dplyr::select(names(defaults))
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

gs_rule_choices <- function(include_custom = TRUE) {
  choices <- c(
    "O'Brien-Fleming" = "OF",
    "Pocock" = "Pocock",
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
      spending_values = character()
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
      }
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

gs_round_choice_values <- function(schedule_tbl = rv$gs_analysis_schedule) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  sort(unique(schedule_tbl$analysis_round[is.finite(schedule_tbl$analysis_round) & !is.na(schedule_tbl$analysis_round)]))
}
