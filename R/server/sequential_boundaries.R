build_gs_boundary_schedule <- function(
  plan_tbl = collect_gs_hypothesis_plan(persist = FALSE),
  schedule_tbl = collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = FALSE),
  notify = FALSE
) {
  validation <- validate_gs_analysis_schedule(schedule_tbl = schedule_tbl, plan_tbl = plan_tbl, notify = notify)
  if (!isTRUE(validation$ok)) {
    schedule_tbl <- arrange_gs_analysis_schedule_display(schedule_tbl)
    if (!nrow(schedule_tbl)) {
      return(empty_gs_boundary_preview())
    }
    return(schedule_tbl %>%
      dplyr::transmute(
        hypothesis = hypothesis,
        alpha_spending = plan_tbl$alpha_spending[match(hypothesis_id, plan_tbl$id)],
        planned_analyses = planned_analyses,
        analysis = hypothesis_stage,
        timing = information_fraction,
        current_alpha = NA_real_,
        stage_alpha = NA_real_,
        cumulative_alpha_spent = NA_real_,
        z_boundary = NA_real_,
        p_boundary = NA_real_,
        status = validation$message,
        analysis_round = analysis_round,
        hypothesis_stage = hypothesis_stage,
        schedule_key = schedule_key,
        is_final = is_final,
        max_info = rep(100, n())
      ) %>%
      dplyr::arrange(analysis_round, hypothesis, hypothesis_stage))
  }

  schedule_tbl <- validation$schedule
  current_alpha <- get_current_allocations()
  status_tbl <- build_ts_status_table()

  preview_rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    hypothesis_rows <- schedule_tbl %>%
      dplyr::filter(hypothesis_id == plan_tbl$id[[i]]) %>%
      dplyr::arrange(hypothesis_stage)
    alpha_now <- as.numeric(current_alpha[[plan_tbl$hypothesis[[i]]]])
    status_row <- if (!is.null(status_tbl) && nrow(status_tbl)) {
      status_tbl[match(plan_tbl$hypothesis[[i]], status_tbl$hypothesis), , drop = FALSE]
    } else {
      NULL
    }

    current_status <- "Ready"
    if (!is.null(status_row) && nrow(status_row)) {
      if (!isTRUE(status_row$in_graph[[1]])) {
        current_status <- "Rejected"
      } else if (!isTRUE(status_row$testable[[1]]) || !is.finite(alpha_now) || alpha_now <= 0) {
        current_status <- "Not testable"
      }
    } else if (!is.finite(alpha_now) || alpha_now <= 0) {
      current_status <- "Not testable"
    }

    output_rows <- hypothesis_rows %>%
      dplyr::transmute(
        hypothesis = hypothesis,
        alpha_spending = plan_tbl$alpha_spending[[i]],
        planned_analyses = planned_analyses,
        analysis = hypothesis_stage,
        timing = information_fraction,
        current_alpha = alpha_now,
        stage_alpha = NA_real_,
        cumulative_alpha_spent = NA_real_,
        z_boundary = NA_real_,
        p_boundary = NA_real_,
        status = current_status,
        analysis_round = analysis_round,
        hypothesis_stage = hypothesis_stage,
        schedule_key = schedule_key,
        is_final = is_final,
        max_info = rep(100, n())
      )

    if (!identical(current_status, "Ready")) {
      return(output_rows)
    }

    spending_values <- NULL
    if (identical(plan_tbl$alpha_spending[[i]], "Custom")) {
      spend_info <- parse_spending_proportions(
        plan_tbl$custom_cumulative_alpha[[i]],
        plan_tbl$planned_analyses[[i]]
      )
      if (!isTRUE(spend_info$ok)) {
        output_rows$status <- paste("Error:", spend_info$message)
        return(output_rows)
      }
      spending_values <- spend_info$values
    }

    hsd_gamma_val <- if ("hsd_gamma" %in% names(plan_tbl)) plan_tbl$hsd_gamma[[i]] else -4

    boundary_tbl <- tryCatch(
      compute_boundary_schedule(
        total_alpha = alpha_now,
        spending_type = plan_tbl$alpha_spending[[i]],
        timing = hypothesis_rows$information_fraction,
        spending_values = spending_values,
        hsd_gamma = hsd_gamma_val
      ),
      error = function(e) e
    )

    if (inherits(boundary_tbl, "error")) {
      output_rows$status <- paste("Error:", conditionMessage(boundary_tbl))
      return(output_rows)
    }

    output_rows$stage_alpha <- boundary_tbl$stage_alpha
    output_rows$cumulative_alpha_spent <- boundary_tbl$cumulative_alpha_spent
    output_rows$z_boundary <- boundary_tbl$z_boundary
    output_rows$p_boundary <- boundary_tbl$p_boundary
    output_rows
  })

  dplyr::bind_rows(preview_rows) %>%
    dplyr::arrange(analysis_round, hypothesis, hypothesis_stage)
}

observe({
  ts_state_tick()
  rv$gs_hypothesis_plan
  rv$gs_analysis_schedule
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
})
