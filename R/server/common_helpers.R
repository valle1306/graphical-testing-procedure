# ── Shared empty-table constructors ─────────────────────────────────────────

## sources the boundary construction and display helper functions if they aren't already 
## in the environment, since those are used by multiple server modules. 
## This avoids circular sourcing dependencies between the helper files.

if (!exists("profile_reactivity", mode = "function")) {
  profile_reactivity <- function(label, expr, note = NULL) {
    force(expr)
  }
}

helper_source_specs <- list(
  list(symbol = "normalize_spending_rule", path = "sequential_boundary_helpers.R"),
  list(symbol = "gs_scalar_display_text", path = "sequential_display_helpers.R"),
  list(symbol = "gs_design_alpha_lookup", path = "sequential_runtime_helpers.R")
)
for (helper_spec in helper_source_specs) {
  if (!exists(helper_spec$symbol, mode = "function")) {
    helper_path <- file.path(getwd(), "R", "server", helper_spec$path)
    if (file.exists(helper_path)) {
      source(helper_path, local = TRUE)
    }
  }
}

missing_tibble_column <- function(template_col, n) {
  if (is.integer(template_col)) {
    return(rep(NA_integer_, n))
  }
  if (is.numeric(template_col)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(template_col)) {
    return(rep(NA, n))
  }
  if (is.character(template_col)) {
    return(rep("", n))
  }
  rep(NA, n)
}

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
    planned_max_info = numeric(),
    alpha_spending = character(),
    custom_cumulative_alpha = character(),
    hsd_gamma = numeric(),
    haybittle_p1 = numeric()
  )
}

# ── Shared display helpers ──────────────────────────────────────────────────

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
    observed_info = numeric(),
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
  if (!"planned_max_info" %in% names(out)) {
    out$planned_max_info <- rep(100, nrow(out))
  }
  out$planned_max_info <- suppressWarnings(as.numeric(out$planned_max_info))
  out$planned_max_info[!is.finite(out$planned_max_info) | out$planned_max_info <= 0] <- 100
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
    dplyr::select(id, hypothesis, planned_analyses, planned_max_info, alpha_spending, custom_cumulative_alpha, hsd_gamma, haybittle_p1)
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
      out[[nm]] <- missing_tibble_column(defaults[[nm]], nrow(out))
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
  out$observed_info <- as.numeric(out$observed_info)
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
      out[[nm]] <- missing_tibble_column(defaults[[nm]], nrow(out))
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
