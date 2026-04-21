# ── Boundary computation helpers (moved from common_helpers.R) ──────────────
# Every function here helps turn user inputs (spending rule, total alpha,
# timing of peeks) into a table of z-score / p-value cut-offs for each peek.
# Top-level entry point: `compute_boundary_schedule()` at the bottom.

# ---------------------------------------------------------------------------
# normalize_spending_rule(x)
# ---------------------------------------------------------------------------
# Collapses every synonym ("asOF", "O'Brien-Fleming", "Lan-DeMets O'Brien-
# Fleming", ...) into one short tag ("OF", "Pocock", "HSD", "Custom") so the
# rest of the file can do simple equality checks.
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

# ---------------------------------------------------------------------------
# parse_numeric_sequence(text_value)
# ---------------------------------------------------------------------------
# Turns a free-text string like "0.01, 0.02; 0.03" (commas, semicolons,
# spaces, tabs, or newlines all allowed) into a numeric vector.
# Returns numeric(0) if the input is empty.
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

# ---------------------------------------------------------------------------
# parse_custom_cumulative_alpha(text_value, planned_analyses, total_alpha,
#                               allow_legacy_proportions, allow_total_mismatch)
# ---------------------------------------------------------------------------
# Validates a user-supplied "custom cumulative alpha" list.
# Rules it checks:
#   * Count matches planned_analyses.
#   * Every value is finite, between 0 and 1, and strictly increasing.
#   * If `total_alpha` is given: the final value must equal it (or, when
#     `allow_legacy_proportions = TRUE`, values that are proportions ending
#     at 1 get rescaled by total_alpha).
# Returns a list: `ok = TRUE/FALSE`, a message (if not ok), plus the raw
# values, absolute alpha values, and proportions (cumulative / total_alpha).
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

# ---------------------------------------------------------------------------
# parse_information_timing(text_value, planned_analyses)
# ---------------------------------------------------------------------------
# Validates the list of "how far through the study" values (timing of each
# peek). Values must be positive and strictly increasing. If the user gave
# raw information counts (e.g., 50, 100, 150), we divide by the max so the
# list ends at 1 (fraction of total information).
# Returns list(ok, message) on failure, or list(ok = TRUE, values = ...).
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

# ---------------------------------------------------------------------------
# parse_spending_proportions(text_value, planned_analyses)
# ---------------------------------------------------------------------------
# "Is this text a valid list of cumulative proportions between 0 and 1 that increases
#  over planned_analyses peeks?" — parse_spending_proportions answers yes/no + 
#  returns the cleaned numbers.
parse_spending_proportions <- function(text_value, planned_analyses) {
  parse_custom_cumulative_alpha(
    text_value = text_value,
    planned_analyses = planned_analyses,
    total_alpha = NULL,
    allow_legacy_proportions = FALSE,
    allow_total_mismatch = FALSE
  )
}

# ---------------------------------------------------------------------------
# normalize_imported_custom_cumulative_alpha(plan_tbl, nodes_tbl)
# ---------------------------------------------------------------------------
# normalize_imported_custom_cumulative_alpha walks through an imported hypothesis plan and, 
# for every hypothesis using the "Custom" spending rule, rewrites its stored cumulative-alpha
# text into absolute values (rescaling legacy saves that stored proportions ending at 1) 
# so downstream code always sees one consistent format.

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

# ── Correlation + boundary math ─────────────────────────────────────────────

# ---------------------------------------------------------------------------
# build_information_correlation(timing)
# ---------------------------------------------------------------------------
# Peeks at the data are correlated because later peeks include all the data
# from earlier ones. For two peeks at timing t_i and t_j (with t_i <= t_j),
# the correlation of their z-statistics is sqrt(t_i / t_j).
# This function returns the full k-by-k correlation matrix used by the
# multivariate-normal probability calls below.
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

# ---------------------------------------------------------------------------
# compute_cumulative_alpha_from_z(z_values, timing)
# ---------------------------------------------------------------------------
# Given the z-score boundary for each peek, returns the total probability
# of crossing AT OR BEFORE peek k, for every k. Peek 1 is just 1 - pnorm(z1);
# later peeks need a multivariate-normal probability (via mvtnorm::pmvnorm)
# because we must account for the chance we already stopped earlier.
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

# ---------------------------------------------------------------------------
# solve_custom_stage_boundary(previous_z, target_stage_alpha, corr_prefix)
# ---------------------------------------------------------------------------
# Used only when the user chose the "Custom cumulative alpha" rule in Step 1
# of the Group-Sequential wizard. There is no closed-form formula for that
# case, so for each peek after the first we have to SEARCH for the z-score
# cutoff that exactly spends `target_stage_alpha` of extra error at this peek
# (given we already survived the previous peeks at `previous_z`).
# Uses stats::uniroot — a binary-search root finder. The while-loops widen
# the search window (up to +/- 40) if the sign change is not bracketed yet.
# Called once per peek (peek 2 onwards) by solve_custom_boundaries().
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

# ---------------------------------------------------------------------------
# solve_custom_boundaries(cumulative_alpha, timing)
# ---------------------------------------------------------------------------
# Turns a user-supplied cumulative-alpha list + timing list into the full
# per-peek boundary table (same columns that Step 3 "Boundary Review" shows).
# Steps:
#   1. `stage_alpha` = extra spent at each peek = differences of the cumul.
#   2. Peek 1 boundary is simple: qnorm(1 - cumulative_alpha[1]).
#   3. Each later peek calls solve_custom_stage_boundary() with the z-values
#      already solved for earlier peeks.
# Returns a tibble with columns: analysis, timing, stage_alpha,
# cumulative_alpha_spent, z_boundary, p_boundary.
# Called by compute_boundary_schedule() when spending_type == "Custom".
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

# ---------------------------------------------------------------------------
# compute_boundary_schedule(total_alpha, spending_type, timing,
#                           spending_values, hsd_gamma, haybittle_p1)
# ---------------------------------------------------------------------------
# The core "boundary engine" of the Group-Sequential Design wizard.
# compute_boundary_schedule is the main entry point: given a total alpha, a spending 
# rule, and the timing of each planned peek, it dispatches to the right engine — gsDesign 
# for OF/Pocock/HSD, HP() for Haybittle-Peto, solve_custom_boundaries() for Custom, or a 
# trivial single-peek shortcut — and returns one row per analysis with stage_alpha, 
# cumulative_alpha_spent, z_boundary, and p_boundary
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
