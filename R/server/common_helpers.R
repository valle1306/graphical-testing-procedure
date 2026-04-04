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
    status = character()
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
    custom_cumulative_alpha = character()
  )
}

# ── Shared display helpers ──────────────────────────────────────────────────

format_hypothesis_list <- function(x) {
  values <- unique(trimws(as.character(x)))
  values <- values[nzchar(values)]
  if (!length(values)) {
    return("none")
  }
  paste(values, collapse = ", ")
}

default_info_timing_string <- function(k = 2L) {
  k <- suppressWarnings(as.integer(k[[1]]))
  if (is.na(k) || k < 1L) {
    k <- 1L
  }
  if (k == 1L) {
    return("1")
  }
  paste(vapply(seq_len(k), function(i) format_plain_number(i / k), character(1)), collapse = ", ")
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

# Boundary computation helpers (moved from app.R server closure)

normalize_spending_rule <- function(x) {
  value <- trimws(as.character(x[[1]]))
  dplyr::case_when(
    identical(value, "asOF") ~ "OF",
    identical(value, "asP") ~ "Pocock",
    identical(value, "asUser") ~ "Custom",
    identical(value, "O'Brien-Fleming") ~ "OF",
    identical(value, "Lan-DeMets O'Brien-Fleming") ~ "OF",
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
  planned_analyses <- suppressWarnings(as.integer(planned_analyses[[1]]))
  if (is.na(planned_analyses) || planned_analyses < 1L) {
    return(list(ok = FALSE, message = "Planned analyses must be a positive integer."))
  }
  values <- parse_numeric_sequence(text_value)
  if (length(values) != planned_analyses) {
    return(list(
      ok = FALSE,
      message = sprintf("Enter %d cumulative spending proportions.", planned_analyses)
    ))
  }
  if (any(!is.finite(values)) || any(values < 0) || any(values > 1)) {
    return(list(ok = FALSE, message = "Cumulative spending proportions must stay between 0 and 1."))
  }
  if (any(diff(values) < 0)) {
    return(list(ok = FALSE, message = "Cumulative spending proportions must be non-decreasing."))
  }
  if (abs(tail(values, 1) - 1) > 1e-8) {
    return(list(ok = FALSE, message = "The final cumulative spending proportion must be 1."))
  }
  list(ok = TRUE, values = values)
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

compute_boundary_schedule <- function(total_alpha, spending_type, timing, spending_values = NULL) {
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
    hp <- HP(overall.alpha = total_alpha, timing = timing)
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
    stop(sprintf("Unsupported spending rule: %s", spending_type))
  )
  gs_obj <- gsDesign::gsDesign(
    k = length(timing),
    alpha = total_alpha,
    timing = timing,
    sfu = gs_spending_fun,
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
