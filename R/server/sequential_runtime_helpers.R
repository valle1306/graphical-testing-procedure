# Shared runtime helpers

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
