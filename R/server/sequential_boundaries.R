set_gs_boundary_preview_message <- function(text = NULL) {
  if (!exists("rv", inherits = TRUE)) {
    return(invisible(NULL))
  }
  if (is.null(text) || !length(text) || !nzchar(trimws(as.character(text[[1]])))) {
    rv$gs_boundary_preview_message <- NULL
  } else {
    rv$gs_boundary_preview_message <- as.character(text[[1]])
  }
  invisible(NULL)
}

build_gs_boundary_schedule <- function(
  plan_tbl = collect_gs_hypothesis_plan(persist = FALSE),
  schedule_tbl = collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = FALSE),
  notify = FALSE
) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  validation <- validate_gs_analysis_schedule(
    schedule_tbl = schedule_tbl,
    plan_tbl = plan_tbl,
    allow_custom_alpha_mismatch = FALSE,
    notify = notify
  )
  if (!isTRUE(validation$ok)) {
    set_gs_boundary_preview_message(validation$message)
    return(sanitize_gs_boundary_preview_tbl(empty_gs_boundary_preview()))
  }

  schedule_tbl <- validation$schedule
  set_gs_boundary_preview_message(NULL)
  current_alpha <- get_current_allocations()
  design_alpha_lookup <- gs_design_alpha_lookup(fallback = current_alpha)
  status_tbl <- build_ts_status_table()

  tryCatch(
    {
      preview_rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
        hypothesis_rows <- schedule_tbl %>%
          dplyr::filter(hypothesis_id == plan_tbl$id[[i]]) %>%
          dplyr::arrange(hypothesis_stage)
        alpha_now <- as.numeric(current_alpha[[plan_tbl$hypothesis[[i]]]])
        design_alpha_now <- as.numeric(design_alpha_lookup[[plan_tbl$hypothesis[[i]]]])
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
        boundary_total_alpha <- alpha_now
        if (identical(plan_tbl$alpha_spending[[i]], "Custom")) {
          profile <- gs_custom_alpha_profile(
            plan_tbl[i, , drop = FALSE],
            design_alpha = design_alpha_now,
            current_alpha = alpha_now
          )
          if (!isTRUE(profile$ok)) {
            stop(sprintf("%s custom cumulative alpha error: %s", plan_tbl$hypothesis[[i]], profile$message))
          }
          spending_values <- profile$proportions
          boundary_total_alpha <- alpha_now
        }

        hsd_gamma_val <- if ("hsd_gamma" %in% names(plan_tbl)) plan_tbl$hsd_gamma[[i]] else -4
        haybittle_p1_val <- if ("haybittle_p1" %in% names(plan_tbl)) plan_tbl$haybittle_p1[[i]] else 3e-04

        boundary_tbl <- tryCatch(
          compute_boundary_schedule(
            total_alpha = boundary_total_alpha,
            spending_type = plan_tbl$alpha_spending[[i]],
            timing = hypothesis_rows$information_fraction,
            spending_values = spending_values,
            hsd_gamma = hsd_gamma_val,
            haybittle_p1 = haybittle_p1_val
          ),
          error = function(e) e
        )

        if (inherits(boundary_tbl, "error")) {
          stop(conditionMessage(boundary_tbl))
        }

        output_rows$stage_alpha <- boundary_tbl$stage_alpha
        output_rows$cumulative_alpha_spent <- boundary_tbl$cumulative_alpha_spent
        output_rows$z_boundary <- boundary_tbl$z_boundary
        output_rows$p_boundary <- boundary_tbl$p_boundary
        output_rows
      })

      sanitize_gs_boundary_preview_tbl(
        dplyr::bind_rows(preview_rows) %>%
          dplyr::arrange(analysis_round, hypothesis, hypothesis_stage)
      )
    },
    error = function(e) {
      set_gs_boundary_preview_message(conditionMessage(e))
      sanitize_gs_boundary_preview_tbl(empty_gs_boundary_preview())
    }
  )
}

observe({
  ts_state_tick()
  rv$gs_hypothesis_plan
  rv$gs_analysis_schedule
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
})
