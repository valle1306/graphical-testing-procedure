# ---- Wizard navigation ----
observeEvent(input$gs_wizard_next_1, {
  persist_group_sequential_design_state(update_boundary = FALSE)
  rv$gs_wizard_step <- 2L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step2")
})

observeEvent(input$gs_wizard_next_2, {
  persist_group_sequential_design_state(update_boundary = TRUE)
  rv$gs_wizard_step <- 3L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step3")
})

observeEvent(input$gs_wizard_back_2, {
  rv$gs_wizard_step <- 1L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step1")
})

observeEvent(input$gs_wizard_back_3, {
  rv$gs_wizard_step <- 2L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step2")
})

# ---- Analysis round observers ----
observeEvent(input$gs_analysis_round, {
  set_gs_round_feedback(NULL)
}, ignoreInit = TRUE)

observeEvent(input$gs_submit_round, {
  set_gs_round_feedback(NULL)

  if (nrow(rv$gs_analysis_history) && nzchar(rv$gs_applied_design_signature)) {
    current_signature <- gs_current_design_signature()
    if (!identical(current_signature, rv$gs_applied_design_signature)) {
      msg <- "The group sequential design changed after earlier submissions. Reset Analysis State before continuing."
      set_gs_round_feedback(msg, type = "error")
      showNotification(msg, type = "error", duration = 8)
      return(invisible(NULL))
    }
  }

  if (is.null(rv$ts_object) && !isTRUE(initialize_batch_gs_object(reset_history = TRUE))) {
    return(invisible(NULL))
  }

  submission <- tryCatch(collect_round_submission(), error = function(e) e)
  if (inherits(submission, "error")) {
    set_ts_log(paste("Round submission error:", submission$message))
    set_gs_round_feedback(submission$message, type = "error")
    showNotification(submission$message, type = "error", duration = 8)
    return(invisible(NULL))
  }
  if (is.null(submission)) {
    return(invisible(NULL))
  }

  tryCatch({
    invisible(rv$ts_object$test(submission$stage_df))
    rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
      dplyr::bind_rows(rv$gs_analysis_history, submission$history_rows)
    )
    rv$gs_stage_history <- gs_history_to_legacy_stage_history(rv$gs_analysis_history)
    bump_ts_state()
    refresh_ts_state()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    set_ts_log(build_round_submit_log(submission$history_rows))
    set_gs_round_feedback(
      sprintf(
        "Saved analysis round %s for %s hypothesis%s.",
        submission$history_rows$analysis_round[[1]],
        nrow(submission$history_rows),
        ifelse(nrow(submission$history_rows) == 1L, "", "es")
      ),
      type = "success"
    )
    session$sendCustomMessage("adjust-datatables", list())
  }, error = function(e) {
    set_ts_log(paste("Group sequential batch analysis error:", e$message))
    set_gs_round_feedback(paste("Group sequential batch analysis error:", e$message), type = "error")
    showNotification(paste("Group sequential batch analysis error:", e$message), type = "error", duration = 8)
  })
})

observeEvent(input$gs_reset_analysis_state, {
  reset_group_sequential_state()
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  set_gs_round_feedback(NULL)
  showNotification("Analysis state reset. The group sequential design tables were kept.", type = "message")
})

observeEvent(input$gs_reset_design_defaults, {
  rv$gs_hypothesis_plan <- build_default_gs_hypothesis_plan(rv$nodes, empty_gs_hypothesis_plan())
  rv$gs_analysis_schedule <- build_default_gs_analysis_schedule(rv$gs_hypothesis_plan)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
  reset_group_sequential_state()
  sync_group_sequential_inputs(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  rv$gs_design_finalized <- FALSE
  rv$gs_finalize_feedback <- NULL
  rv$gs_wizard_step <- 1L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step1")
  set_gs_round_feedback(NULL)
  showNotification("Group sequential design reset to defaults generated from the graph.", type = "message")
})

observeEvent(input$gs_finalize_design, {
  rv$gs_finalize_feedback <- NULL
  design_state <- persist_group_sequential_design_state(update_boundary = FALSE)
  plan_tbl <- design_state$plan
  schedule_tbl <- design_state$schedule
  validation <- validate_gs_analysis_schedule(
    schedule_tbl = schedule_tbl,
    plan_tbl = plan_tbl,
    allow_custom_alpha_mismatch = FALSE,
    notify = FALSE
  )
  if (!isTRUE(validation$ok)) {
    rv$gs_finalize_feedback <- list(
      text = paste("Cannot finalize:", validation$message),
      type = "error"
    )
    showNotification(paste("Cannot finalize:", validation$message), type = "error", duration = 8)
    return(invisible(NULL))
  }
  boundary_preview <- tryCatch(
    build_gs_boundary_schedule(notify = FALSE),
    error = function(e) e
  )
  if (inherits(boundary_preview, "error")) {
    rv$gs_finalize_feedback <- list(
      text = paste("Cannot finalize:", conditionMessage(boundary_preview)),
      type = "error"
    )
    showNotification(paste("Cannot finalize:", conditionMessage(boundary_preview)), type = "error", duration = 8)
    return(invisible(NULL))
  }
  if (is.null(boundary_preview) || !nrow(boundary_preview)) {
    rv$gs_finalize_feedback <- list(
      text = "Cannot finalize: boundary schedule is empty. Check that hypotheses have alpha > 0.",
      type = "error"
    )
    showNotification("Cannot finalize: boundary schedule is empty.", type = "error", duration = 8)
    return(invisible(NULL))
  }
  rv$gs_boundary_preview <- boundary_preview
  sig <- gs_current_design_signature()
  rv$gs_applied_design_signature <- sig
  rv$gs_design_finalized <- TRUE
  rv$gs_finalize_feedback <- list(
    text = "Design finalized. You can now proceed to the Analysis tab to submit results.",
    type = "success"
  )
  showNotification("Design finalized successfully.", type = "message", duration = 5)
})

observeEvent(input$gs_go_to_analysis, {
  updateNavbarPage(session, "nav", selected = "Analysis")
})

observeEvent(input$gs_edit_design, {
  if (nrow(rv$gs_analysis_history)) {
    showNotification(
      "Cannot edit design after submitting analysis rounds. Reset analysis state first.",
      type = "error", duration = 8
    )
    return(invisible(NULL))
  }
  rv$gs_design_finalized <- FALSE
  rv$gs_applied_design_signature <- ""
  rv$gs_finalize_feedback <- NULL
  rv$gs_wizard_step <- 1L
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step1")
  updateNavbarPage(session, "nav", selected = "Group Sequential Design")
  showNotification("Design unlocked for editing. Returned to the Group Sequential Design tab.", type = "message", duration = 5)
})
