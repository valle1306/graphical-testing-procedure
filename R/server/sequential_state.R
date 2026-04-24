# Build the finalized graph transition matrix in hypothesis order so the
# testing object and the frozen design state share the same indexing.
build_transition_matrix_from_graph <- function() {
  n <- nrow(rv$nodes)
  mat <- matrix(0, n, n)
  if (n > 0) {
    rownames(mat) <- colnames(mat) <- rv$nodes$hypothesis
    if (nrow(rv$edges) > 0) {
      for (i in seq_len(nrow(rv$edges))) {
        from_idx <- which(rv$nodes$id == rv$edges$from[[i]])
        to_idx <- which(rv$nodes$id == rv$edges$to[[i]])
        if (length(from_idx) == 1L && length(to_idx) == 1L) {
          mat[from_idx, to_idx] <- rv$edges$weight[[i]]
        }
      }
    }
  }
  mat
}

# Initialize the live testing object from the current finalized design inputs.
# This is the bridge between editable design tables and runtime analysis state.
initialize_batch_gs_object <- function(reset_history = FALSE) {
  plan_tbl <- collect_gs_hypothesis_plan(persist = TRUE)
  schedule_tbl <- collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = TRUE)
  validation <- validate_gs_analysis_schedule(schedule_tbl = schedule_tbl, plan_tbl = plan_tbl, notify = TRUE)
  if (!isTRUE(validation$ok)) {
    # Keep runtime state empty when the finalized design tables no longer
    # produce a valid schedule.
    set_ts_log(validation$message)
    set_gs_round_feedback(validation$message, type = "error")
    rv$ts_object <- NULL
    rv$ts_summary <- NULL
    return(FALSE)
  }

  rv$transition <- build_transition_matrix_from_graph()
  transition_validation <- validate_transition_matrix(rv$transition, rv$nodes$hypothesis)
  if (!isTRUE(transition_validation$valid)) {
    # Do not carry forward a graphical design whose transition structure
    # no longer matches the hypotheses available to the runtime object.
    set_ts_log(transition_validation$message)
    set_gs_round_feedback(transition_validation$message, type = "error")
    showNotification(transition_validation$message, type = "error", duration = 8)
    rv$ts_object <- NULL
    rv$ts_summary <- NULL
    return(FALSE)
  }

  # TrialSimulator's runtime object uses package-specific spending codes. The
  # app's boundary preview keeps the design-time rule names and computes those
  # boundaries separately from this initialization step.
  rv$alpha_spending <- vapply(plan_tbl$alpha_spending, gs_runtime_spending_code, character(1))
  rv$planned_max_info <- as.numeric(plan_tbl$planned_max_info)

  tryCatch({
    rv$ts_object <- NULL
    withCallingHandlers(
      {
        rv$ts_object <- GraphicalTesting$new(
          alpha = rv$nodes$alpha,
          transition = rv$transition,
          alpha_spending = rv$alpha_spending,
          planned_max_info = rv$planned_max_info,
          hypotheses = rv$nodes$hypothesis,
          # The app renders its own visNetwork graph, so suppress the package's
          # ggplot side-effects during runtime submissions.
          silent = TRUE
        )
      },
      message = function(m) invokeRestart("muffleMessage")
    )
    if (isTRUE(reset_history)) {
      # Replay and import paths rebuild the runtime object first, then restore
      # submitted history against a clean analysis timeline.
      rv$gs_analysis_history <- empty_gs_analysis_history()
      rv$gs_stage_history <- empty_gs_stage_history()
    }
    # Lock the runtime object to the exact finalized plan/schedule pair that
    # produced it before refreshing derived status and boundary preview state.
    rv$gs_applied_design_signature <- gs_current_design_signature(plan_tbl, schedule_tbl)
    bump_ts_state()
    refresh_ts_state()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(plan_tbl, schedule_tbl, notify = FALSE)
    set_ts_log(
      c(
        sprintf("Initialized group sequential object for %s hypotheses.", nrow(plan_tbl)),
        "Group sequential testing uses one-sided alpha and one-sided p-values.",
        format_alpha_snapshot()
      )
    )
    TRUE
  }, error = function(e) {
    set_ts_log(paste("Error during group sequential setup:", e$message))
    set_gs_round_feedback(paste("Group sequential setup error:", e$message), type = "error")
    rv$ts_object <- NULL
    rv$ts_summary <- NULL
    FALSE
  })
}

# Collect one submitted analysis time from the current round-entry UI.
# The returned rows are sanitized runtime records ready to append to history.
collect_round_submission <- function() {
  selected_round <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)

  preview_tbl <- sanitize_gs_boundary_preview_tbl(rv$gs_boundary_preview)
  if (!nrow(preview_tbl)) {
    set_gs_round_feedback("No validated boundary schedule is available for analysis-time submission.", type = "error")
    return(NULL)
  }

  state <- gs_analysis_round_state(
    preview_tbl = preview_tbl,
    history_tbl = rv$gs_analysis_history,
    selected_round = selected_round
  )
  round_value <- gs_resolve_analysis_round_target(
    selected_round = selected_round,
    actionable_rounds = state$actionable_rounds,
    next_actionable_round = state$next_actionable_round,
    force_first_actionable = isTRUE(rv$gs_force_first_actionable_round)
  )
  if (!identical(round_value, selected_round)) {
    # Programmatic round selection can redirect the submission UI to the first
    # actionable round; recompute the round state against that target.
    state <- gs_analysis_round_state(
      preview_tbl = preview_tbl,
      history_tbl = rv$gs_analysis_history,
      selected_round = round_value
    )
  }
  if (is.na(round_value) || round_value < 1L) {
    set_gs_round_feedback("Choose a valid analysis time before submitting.", type = "error")
    showNotification("Choose a valid analysis time before submitting.", type = "error")
    return(NULL)
  }

  if (!state$has_remaining_rows) {
    set_gs_round_feedback(sprintf("Analysis time %s has no remaining scheduled hypotheses to submit.", round_value), type = "error")
    showNotification(sprintf("Analysis time %s has no remaining scheduled hypotheses to submit.", round_value), type = "error")
    return(NULL)
  }

  ready_rows <- state$actionable_rows
  if (!nrow(ready_rows)) {
    set_gs_round_feedback(sprintf("No active hypotheses are ready at analysis time %s.", round_value), type = "error")
    showNotification(sprintf("No active hypotheses are ready at analysis time %s.", round_value), type = "error")
    return(NULL)
  }

  history_submission <- if (nrow(rv$gs_analysis_history)) {
    max(rv$gs_analysis_history$submission, na.rm = TRUE) + 1L
  } else {
    1L
  }
  max_info_lookup <- stats::setNames(as.numeric(rv$planned_max_info), as.character(rv$nodes$hypothesis))

  # Only hypotheses that are still ready in the selected global round should
  # produce runtime submission rows.
  stage_rows <- lapply(seq_len(nrow(ready_rows)), function(i) {
    schedule_key <- ready_rows$schedule_key[[i]]
    p_value <- read_scalar_numeric_input(paste0("gs_round_p_", schedule_key))
    if (is.na(p_value) || p_value < 0 || p_value > 1) {
      stop(sprintf("Enter a one-sided p-value between 0 and 1 for %s look %s.", ready_rows$hypothesis[[i]], ready_rows$hypothesis_stage[[i]]))
    }
    # This is a live TrialSimulator input, not the design-time information
    # fraction that the boundary preview used to build the planned look table.
    observed_info <- gs_require_observed_info_count(
      observed_info = read_scalar_numeric_input(paste0("gs_round_info_", schedule_key)),
      hypothesis = ready_rows$hypothesis[[i]],
      hypothesis_stage = ready_rows$hypothesis_stage[[i]]
    )
    runtime_code <- gs_runtime_spending_code(ready_rows$alpha_spending[[i]])
    planned_max_info <- as.numeric(max_info_lookup[[ready_rows$hypothesis[[i]]]])
    if (!is.finite(planned_max_info) || planned_max_info <= 0) {
      planned_max_info <- 100
    }
    tibble::tibble(
      order = as.integer(ready_rows$analysis_round[[i]]),
      hypotheses = ready_rows$hypothesis[[i]],
      p = as.numeric(p_value),
      info = as.numeric(observed_info),
      is_final = isTRUE(ready_rows$is_final[[i]]),
      # TrialSimulator replays a final look against the submitted terminal
      # count. The design-time boundary preview still comes from the finalized
      # plan; only the frozen runtime history switches max_info at the final
      # look to preserve the package's replay contract.
      max_info = if (isTRUE(ready_rows$is_final[[i]])) as.numeric(observed_info) else as.numeric(planned_max_info),
      alpha_spent = if (identical(runtime_code, "asUser")) {
        if (isTRUE(ready_rows$is_final[[i]])) {
          1.0
        } else if (identical(ready_rows$alpha_spending[[i]], "Custom")) {
          gs_custom_alpha_spent_fraction(
            hypothesis = ready_rows$hypothesis[[i]],
            hypothesis_stage = ready_rows$hypothesis_stage[[i]]
          )
        } else {
          hyp_alpha <- as.numeric(ready_rows$current_alpha[[i]])
          if (!is.finite(hyp_alpha) || hyp_alpha <= 0) 0 else as.numeric(ready_rows$cumulative_alpha_spent[[i]]) / hyp_alpha
        }
      } else {
        NA_real_
      }
    )
  })

  stage_df <- dplyr::bind_rows(stage_rows)
  runtime_validation <- validate_gs_runtime_alpha_spent(
    stage_df = stage_df,
    ready_rows = ready_rows,
    history_tbl = rv$gs_analysis_history
  )
  if (!isTRUE(runtime_validation$ok)) {
    # Preserve monotone cumulative spending for custom runtime looks before any
    # new rows enter frozen submitted history.
    set_gs_round_feedback(runtime_validation$message, type = "error")
    stop(runtime_validation$message)
  }
  trajectory_before <- tryCatch(
    tibble::as_tibble(rv$ts_object$get_trajectory()),
    error = function(e) tibble::tibble()
  )
  withCallingHandlers(
    rv$ts_object$test(as.data.frame(stage_df, stringsAsFactors = FALSE)),
    message = function(m) invokeRestart("muffleMessage")
  )
  trajectory_after <- tryCatch(
    tibble::as_tibble(rv$ts_object$get_trajectory()),
    error = function(e) tibble::tibble()
  )
  trajectory_before_keys <- if (nrow(trajectory_before)) {
    trajectory_before %>%
      dplyr::transmute(
        analysis_round = as.integer(order),
        hypothesis = as.character(hypothesis),
        hypothesis_stage = as.integer(stages)
      )
  } else {
    tibble::tibble(
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_stage = integer()
    )
  }
  batch_results <- trajectory_after %>%
    dplyr::transmute(
      # Preserve the package-emitted event order so same-analysis retests stay
      # in the frozen history exactly as GraphicalTesting processed them. These
      # boundary/decision columns come from TrialSimulator's runtime result,
      # not from the app's design-time preview table.
      .package_result_order = dplyr::row_number(),
      analysis_round = as.integer(order),
      hypothesis = as.character(hypothesis),
      hypothesis_stage = as.integer(stages),
      runtime_spending_code = as.character(typeOfDesign),
      current_alpha = as.numeric(alpha),
      cumulative_alpha_spent = as.numeric(alphaSpent),
      boundary_z = as.numeric(criticalValues),
      boundary_p = as.numeric(stageLevels),
      p_value = as.numeric(obs_p_value),
      decision = dplyr::if_else(
        tolower(as.character(decision)) == "reject",
        "Reject",
        "Do not reject"
      )
    ) %>%
    dplyr::anti_join(
      trajectory_before_keys,
      by = c("analysis_round", "hypothesis", "hypothesis_stage")
    ) %>%
    dplyr::filter(analysis_round == round_value)
  history_rows <- ready_rows %>%
    dplyr::transmute(
      submission = history_submission,
      schedule_key = schedule_key,
      analysis_round = analysis_round,
      hypothesis = hypothesis,
      hypothesis_stage = hypothesis_stage,
      alpha_spending = alpha_spending,
      information_fraction = timing,
      is_final = is_final,
      observed_info = stage_df$info,
      max_info = stage_df$max_info
    ) %>%
    dplyr::left_join(
      batch_results,
      by = c("analysis_round", "hypothesis", "hypothesis_stage")
    ) %>%
    dplyr::arrange(.package_result_order) %>%
    dplyr::select(-.package_result_order)
  if (anyNA(history_rows$decision) || anyNA(history_rows$boundary_p)) {
    stop("Unable to capture the submitted testing results for this analysis time.")
  }

  list(
    stage_df = as.data.frame(stage_df, stringsAsFactors = FALSE),
    history_rows = sanitize_gs_analysis_history_tbl(history_rows)
  )
}

# Rebuild runtime state from frozen submitted history after import or reset.
# This preserves submitted decisions as historical facts rather than
# recalculating them from the current live boundary preview.
replay_group_sequential_history <- function(history_tbl) {
  history_tbl <- sanitize_gs_analysis_history_tbl(history_tbl)
  if (!nrow(history_tbl)) {
    rv$gs_analysis_history <- history_tbl
    rv$gs_stage_history <- empty_gs_stage_history()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    return(TRUE)
  }
  if (!isTRUE(initialize_batch_gs_object(reset_history = TRUE))) {
    return(FALSE)
  }
  for (submission_id in sort(unique(history_tbl$submission))) {
    batch_rows <- history_tbl %>%
      dplyr::filter(submission == submission_id) %>%
      dplyr::mutate(
        hypothesis = factor(hypothesis, levels = rv$nodes$hypothesis)
      ) %>%
      dplyr::arrange(analysis_round, hypothesis, hypothesis_stage) %>%
      dplyr::mutate(
        hypothesis = as.character(hypothesis),
        observed_info = dplyr::if_else(
          is.finite(observed_info) & observed_info > 0,
          observed_info,
          as.numeric(max_info) * as.numeric(information_fraction)
        ),
        max_info = dplyr::if_else(
          is.finite(max_info) & max_info > 0,
          max_info,
          as.numeric(rv$planned_max_info[match(hypothesis, rv$nodes$hypothesis)])
        ),
        alpha_spent = dplyr::if_else(
          runtime_spending_code == "asUser",
          dplyr::if_else(
            is_final,
            1.0,
            dplyr::if_else(
              is.finite(current_alpha) & current_alpha > 0,
              cumulative_alpha_spent / current_alpha,
              0
            )
          ),
          NA_real_
        )
      )
    stage_df <- batch_rows %>%
      dplyr::transmute(
        order = as.integer(analysis_round),
        hypotheses = hypothesis,
        p = as.numeric(p_value),
        info = as.numeric(observed_info),
        is_final = as.logical(is_final),
        max_info = as.numeric(max_info),
        alpha_spent = as.numeric(alpha_spent)
      )
    withCallingHandlers(
      rv$ts_object$test(as.data.frame(stage_df, stringsAsFactors = FALSE)),
      message = function(m) invokeRestart("muffleMessage")
    )
    bump_ts_state()
  }
  rv$gs_analysis_history <- history_tbl
  rv$gs_stage_history <- gs_history_to_legacy_stage_history(history_tbl)
  rv$gs_applied_design_signature <- gs_current_design_signature()
  refresh_ts_state()
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  TRUE
}

# Export the finalized design tables together with the frozen submitted
# analysis history and a legacy-compatible settings view.
serialize_group_sequential_export <- function() {
  plan_tbl <- collect_gs_hypothesis_plan(persist = TRUE)
  schedule_tbl <- collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = TRUE)
  list(
    gs_hypothesis_plan = as.data.frame(plan_tbl, stringsAsFactors = FALSE),
    gs_analysis_schedule = as.data.frame(schedule_tbl, stringsAsFactors = FALSE),
    gs_analysis_history = as.data.frame(rv$gs_analysis_history, stringsAsFactors = FALSE),
    gs_settings = as.data.frame(
      legacy_settings_from_group_sequential_design(plan_tbl, schedule_tbl),
      stringsAsFactors = FALSE
    )
  )
}

# Build a modern hypothesis plan from legacy import settings when the dedicated
# plan table is missing.
derive_group_sequential_plan_from_legacy <- function(gs_settings) {
  if (is.null(gs_settings) || !nrow(gs_settings)) {
    return(build_default_gs_hypothesis_plan(rv$nodes, empty_gs_hypothesis_plan()))
  }
  gs_settings <- tibble::as_tibble(gs_settings)
  if (is.null(gs_settings$id)) {
    gs_settings$id <- rv$nodes$id[match(gs_settings$hypothesis, rv$nodes$hypothesis)]
  }
  if (is.null(gs_settings$alpha_spending)) {
    gs_settings$alpha_spending <- rep("OF", nrow(gs_settings))
  }
  if (is.null(gs_settings$planned_analyses)) {
    gs_settings$planned_analyses <- rep(1L, nrow(gs_settings))
  }
  if (is.null(gs_settings$spending_values)) {
    gs_settings$spending_values <- rep("", nrow(gs_settings))
  }
  if (is.null(gs_settings$hsd_gamma)) {
    gs_settings$hsd_gamma <- rep(-4, nrow(gs_settings))
  }
  if (is.null(gs_settings$haybittle_p1)) {
    gs_settings$haybittle_p1 <- rep(3e-04, nrow(gs_settings))
  }
  if (is.null(gs_settings$planned_max_info)) {
    gs_settings$planned_max_info <- rep(100, nrow(gs_settings))
  }
  sanitize_gs_hypothesis_plan_tbl(
    gs_settings %>%
      dplyr::transmute(
        id = as.integer(id),
        hypothesis = as.character(hypothesis),
        planned_analyses = as.integer(planned_analyses),
        planned_max_info = suppressWarnings(as.numeric(planned_max_info)),
        alpha_spending = vapply(alpha_spending, normalize_spending_rule, character(1)),
        custom_cumulative_alpha = as.character(spending_values),
        hsd_gamma = suppressWarnings(as.numeric(hsd_gamma)),
        haybittle_p1 = suppressWarnings(as.numeric(haybittle_p1))
      )
  )
}

# Recover schedule timing from legacy settings, but fall back to the current
# default schedule whenever imported timing is missing or invalid.
derive_group_sequential_schedule_from_legacy <- function(plan_tbl, gs_settings) {
  default_tbl <- build_default_gs_analysis_schedule(plan_tbl)
  if (is.null(gs_settings) || !nrow(gs_settings)) {
    return(default_tbl)
  }
  gs_settings <- tibble::as_tibble(gs_settings)
  rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    legacy_idx <- match(plan_tbl$id[[i]], as.integer(gs_settings$id))
    planned_analyses <- plan_tbl$planned_analyses[[i]]
    if (is.na(legacy_idx)) {
      return(default_tbl %>% dplyr::filter(hypothesis_id == plan_tbl$id[[i]]))
    }
    timing_info <- parse_information_timing(gs_settings$info_timing[[legacy_idx]], planned_analyses)
    if (!isTRUE(timing_info$ok)) {
      return(default_tbl %>% dplyr::filter(hypothesis_id == plan_tbl$id[[i]]))
    }
    hypothesis_rows <- default_tbl %>% dplyr::filter(hypothesis_id == plan_tbl$id[[i]]) %>% dplyr::arrange(hypothesis_stage)
    hypothesis_rows$information_fraction <- timing_info$values
    hypothesis_rows
  })
  sanitize_gs_analysis_schedule_tbl(dplyr::bind_rows(rows))
}

# Load imported design/runtime state into the current app session while
# preserving the existing finalized-design and replay workflow guards.
load_group_sequential_design_from_import <- function(dat) {
  imported_plan <- dat$gs_hypothesis_plan
  imported_schedule <- dat$gs_analysis_schedule
  imported_history <- dat$gs_analysis_history
  imported_legacy <- dat$gs_settings

  if (!is.null(imported_plan) && is.list(imported_plan) && !is.data.frame(imported_plan)) {
    imported_plan <- as.data.frame(imported_plan, stringsAsFactors = FALSE)
  }
  if (!is.null(imported_schedule) && is.list(imported_schedule) && !is.data.frame(imported_schedule)) {
    imported_schedule <- as.data.frame(imported_schedule, stringsAsFactors = FALSE)
  }
  if (!is.null(imported_history) && is.list(imported_history) && !is.data.frame(imported_history)) {
    imported_history <- as.data.frame(imported_history, stringsAsFactors = FALSE)
  }
  if (!is.null(imported_legacy) && is.list(imported_legacy) && !is.data.frame(imported_legacy)) {
    imported_legacy <- as.data.frame(imported_legacy, stringsAsFactors = FALSE)
  }

  list_to_vector <- function(x) if (is.list(x)) unlist(x) else x
  if (!is.null(imported_plan) && is.data.frame(imported_plan)) imported_plan[] <- lapply(imported_plan, list_to_vector)
  if (!is.null(imported_schedule) && is.data.frame(imported_schedule)) imported_schedule[] <- lapply(imported_schedule, list_to_vector)
  if (!is.null(imported_history) && is.data.frame(imported_history)) imported_history[] <- lapply(imported_history, list_to_vector)
  if (!is.null(imported_legacy) && is.data.frame(imported_legacy)) imported_legacy[] <- lapply(imported_legacy, list_to_vector)

  plan_tbl <- if (!is.null(imported_plan) && is.data.frame(imported_plan) && nrow(imported_plan)) {
    sanitize_gs_hypothesis_plan_tbl(imported_plan)
  } else {
    derive_group_sequential_plan_from_legacy(imported_legacy)
  }
  plan_tbl <- normalize_imported_custom_cumulative_alpha(plan_tbl, rv$nodes)

  schedule_tbl <- if (!is.null(imported_schedule) && is.data.frame(imported_schedule) && nrow(imported_schedule)) {
    sanitize_gs_analysis_schedule_tbl(imported_schedule)
  } else {
    derive_group_sequential_schedule_from_legacy(plan_tbl, imported_legacy)
  }

  history_tbl <- if (!is.null(imported_history) && is.data.frame(imported_history) && nrow(imported_history)) {
    sanitize_gs_analysis_history_tbl(imported_history)
  } else {
    empty_gs_analysis_history()
  }

  rv$gs_hypothesis_plan <- plan_tbl
  rv$gs_analysis_schedule <- schedule_tbl
  set_gs_analysis_schedule_round_signature(schedule_tbl)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(plan_tbl, schedule_tbl)

  # Import resets the wizard/runtime ownership first; any saved history is then
  # replayed against that clean design state.
  reset_group_sequential_runtime_state()
  rv$gs_design_finalized <- FALSE
  rv$gs_applied_design_signature <- ""
  rv$gs_finalize_feedback <- NULL
  rv$gs_wizard_step <- 1L
  rv$gs_round_selection_programmatic <- FALSE
  rv$gs_force_first_actionable_round <- FALSE
  if (nrow(history_tbl)) {
    replay_ok <- tryCatch(replay_group_sequential_history(history_tbl), error = function(e) e)
    if (inherits(replay_ok, "error") || !isTRUE(replay_ok)) {
      # A failed replay should keep the imported design but drop the stale
      # frozen runtime history rather than leaving partially restored state.
      rv$gs_analysis_history <- empty_gs_analysis_history()
      rv$gs_stage_history <- empty_gs_stage_history()
      rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
      set_ts_log("Imported group sequential design, but could not replay the saved analysis history.")
      showNotification("Imported design, but could not replay the saved analysis history.", type = "warning", duration = 8)
    }
  } else {
    rv$gs_analysis_history <- empty_gs_analysis_history()
    rv$gs_stage_history <- empty_gs_stage_history()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  }
}
