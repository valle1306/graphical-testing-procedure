# ------------------------------------------------------------------------------
# This file handles the buttons on the group-sequential pages:
# Next / Back (wizard), Submit round, Reset, Finalize, Edit, and Go to Analysis.
# Each click saves the form, runs the right checks, and updates what the user
# sees on screen. The real math lives in sequential_state.R and
# sequential_boundaries.R -- this file just wires the buttons to that logic.
# ------------------------------------------------------------------------------

# ---- Wizard navigation ----
# The design wizard has 3 steps. "Next" saves the form first, then moves on.
# "Back" just changes which step is visible.
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

# Runs whenever the user picks a round in the "analysis round" dropdown.
# If the code (not the user) set the value, we silently reset the flag and
# leave. Otherwise it's a real user pick, so we clear any old feedback.
observeEvent(input$gs_analysis_round, {
  if (isTRUE(rv$gs_round_selection_programmatic)) {
    rv$gs_round_selection_programmatic <- FALSE
    rv$gs_force_first_actionable_round <- FALSE
    return(invisible(NULL))
  }
  rv$gs_force_first_actionable_round <- FALSE
  set_gs_round_feedback(NULL)
}, ignoreInit = TRUE)

# The big "Submit Round" button on the Analysis tab.
# Steps (in plain English):
#   1. Make sure the design hasn't been edited since the user started.
#   2. If there's no live test object yet, build one.
#   3. Read the user's p-values for this round (collect_round_submission).
#   4. Apply any rejections to the test object, append to history,
#      rebuild the boundary preview, and show a success message.
# Any error along the way becomes a friendly red notification.
observeEvent(input$gs_submit_round, {
  set_gs_round_feedback(NULL)

  # Step 1: block submission if the plan/schedule changed after earlier rounds.
  # Compare the design's current "fingerprint" to the one saved when the user
  # first submitted; if they differ, the history wouldn't line up.
  if (nrow(rv$gs_analysis_history) && nzchar(rv$gs_applied_design_signature)) {
    current_signature <- gs_current_design_signature()
    if (!identical(current_signature, rv$gs_applied_design_signature)) {
      msg <- "The group sequential design changed after earlier submissions. Reset Analysis State before continuing."
      set_gs_round_feedback(msg, type = "error")
      showNotification(msg, type = "error", duration = 8)
      return(invisible(NULL))
    }
  }

  # Step 2: first-time submit? Build the test object from scratch. If that
  # fails, initialize_batch_gs_object already showed the error -- just bail.
  if (is.null(rv$ts_object) && !isTRUE(initialize_batch_gs_object(reset_history = TRUE))) {
    return(invisible(NULL))
  }

  # Step 3: read + validate the user's p-values for this round.
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

  # Step 4: commit the round. This is where we actually change state.
  tryCatch({
    # Apply rejections to the live GraphicalTesting object so alpha transfers
    # on the graph match what the user saw in this round.
    reject_hypotheses <- submission$history_rows$hypothesis[
      vapply(submission$history_rows$decision, normalize_gs_decision_label, character(1)) == "Reject"
    ]
    invisible(apply_frozen_gs_rejections(rv$ts_object, reject_hypotheses))

    # Append this round's rows to the history tables (both new + legacy shapes).
    rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
      dplyr::bind_rows(rv$gs_analysis_history, submission$history_rows)
    )
    rv$gs_stage_history <- gs_history_to_legacy_stage_history(rv$gs_analysis_history)

    # Refresh downstream state: result summary, graph colors, boundary preview.
    bump_ts_state()
    refresh_ts_state()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    set_ts_log(build_round_submit_log(submission$history_rows))

    # Figure out which round to point the user to next, then show a green
    # success message telling them what just happened and what's next.
    post_submit_state <- gs_analysis_round_state(
      preview_tbl = rv$gs_boundary_preview,
      history_tbl = rv$gs_analysis_history,
      selected_round = submission$history_rows$analysis_round[[1]]
    )
    next_display_round <- gs_resolve_analysis_round_target(
      selected_round = post_submit_state$selected_round,
      actionable_rounds = post_submit_state$actionable_rounds,
      next_actionable_round = post_submit_state$next_actionable_round,
      force_first_actionable = FALSE
    )
    set_gs_round_feedback(
      gs_round_submission_feedback_text(
        analysis_round = submission$history_rows$analysis_round[[1]],
        hypothesis_count = nrow(submission$history_rows),
        next_round = next_display_round
      ),
      type = "success"
    )
    # Nudge the client to resize DataTables whose visible columns changed.
    session$sendCustomMessage("adjust-datatables", list())
  }, error = function(e) {
    # Anything unexpected in Step 4: log it and show a red notification.
    set_ts_log(paste("Group sequential batch analysis error:", e$message))
    set_gs_round_feedback(paste("Group sequential batch analysis error:", e$message), type = "error")
    showNotification(paste("Group sequential batch analysis error:", e$message), type = "error", duration = 8)
  })
})

# "Reset Analysis State" button: throw away submitted rounds and results,
# but KEEP the design (plan/schedule/graph) so the user can start testing over.
observeEvent(input$gs_reset_analysis_state, {
  reset_group_sequential_runtime_state()
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  rv$gs_round_selection_programmatic <- FALSE
  rv$gs_force_first_actionable_round <- TRUE
  set_gs_round_feedback(NULL)
  showNotification("Analysis state reset. The group sequential design tables were kept.", type = "message")
})

# "Reset Design to Defaults" button: throw away the user's plan + schedule
# and rebuild both from scratch using sensible defaults derived from the
# current graph. Also sends the user back to Step 1 of the wizard.
observeEvent(input$gs_reset_design_defaults, {
  rv$gs_hypothesis_plan <- build_default_gs_hypothesis_plan(rv$nodes, empty_gs_hypothesis_plan())
  rv$gs_analysis_schedule <- build_default_gs_analysis_schedule(rv$gs_hypothesis_plan)
  set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
  invalidate_group_sequential_design_state()
  sync_group_sequential_inputs(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  rv$gs_round_selection_programmatic <- FALSE
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step1")
  set_gs_round_feedback(NULL)
  showNotification("Group sequential design reset to defaults generated from the graph.", type = "message")
})

# "Finalize Design" button. Locks in the design so the Analysis tab can use it.
# Three things must pass before we lock:
#   1. plan + schedule validate against each other,
#   2. the boundary preview can actually be computed without errors,
#   3. the boundary preview isn't empty (e.g. all hypotheses had alpha = 0).
# If all three pass, save a "fingerprint" of the design so we can later
# detect any edits and flip the gs_design_finalized flag to TRUE.
observeEvent(input$gs_finalize_design, {
  rv$gs_finalize_feedback <- NULL
  design_state <- persist_group_sequential_design_state(update_boundary = FALSE)
  plan_tbl <- design_state$plan
  schedule_tbl <- design_state$schedule

  # Check 1: plan and schedule fit together.
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

  # Check 2: the boundary math runs cleanly.
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

  # Check 3: there's actually something to test (at least one row).
  if (is.null(boundary_preview) || !nrow(boundary_preview)) {
    rv$gs_finalize_feedback <- list(
      text = "Cannot finalize: boundary schedule is empty. Check that hypotheses have alpha > 0.",
      type = "error"
    )
    showNotification("Cannot finalize: boundary schedule is empty.", type = "error", duration = 8)
    return(invisible(NULL))
  }

  # All checks passed -- save the preview, record the design fingerprint
  # (used later to detect if the user edited the design mid-analysis),
  # mark the design as locked, and show a success banner.
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

# "Edit Design" button. The inverse of Finalize: unlock the design so the
# user can change the plan/schedule. BUT if any rounds have been submitted,
# editing would break the history -- force them to Reset Analysis State first.
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
  rv$gs_round_selection_programmatic <- FALSE
  updateTabsetPanel(session, "gs_wizard_tabs", selected = "step1")
  updateNavbarPage(session, "nav", selected = "Group Sequential Design")
  showNotification("Design unlocked for editing. Returned to the Group Sequential Design tab.", type = "message", duration = 5)
})
