# ------------------------------------------------------------------------------
# Group-sequential event observers.
#
# This file is the "button wiring" for the sequential workflow:
#   1. move through the design wizard,
#   2. finalize or unlock the design,
#   3. submit one analysis round at a time,
#   4. reset runtime state when the user wants to start over.
#
# The underlying calculations are located in:
# - plan/schedule collection and validation happen in sequential_settings.R
# - boundary construction happens in sequential_boundaries.R
# - test-object setup and round payload construction happen in sequential_state.R
#
# The observers here mainly decide when those pieces run and which reactive
# values in rv should be updated in response to a user action.
# ------------------------------------------------------------------------------

# ---- Wizard navigation ----
# The wizard has three steps:
#   Step 1 = hypothesis plan
#   Step 2 = analysis timing / global rounds
#   Step 3 = boundary review
#
# "Next" buttons persist the current form state before moving forward so the
# later steps always read from rv rather than from stale input defaults.
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

# ---- Analysis round selection ----

# The analysis-round dropdown is sometimes changed by the server, not the user.
# When that happens we do not want to treat it as a fresh user choice, because
# that would clear feedback messages and interfere with the "jump to the next
# actionable round" behavior used after resets and successful submissions.
observeEvent(input$gs_analysis_round, {
  if (isTRUE(rv$gs_round_selection_programmatic)) {
    rv$gs_round_selection_programmatic <- FALSE
    rv$gs_force_first_actionable_round <- FALSE
    return(invisible(NULL))
  }
  rv$gs_force_first_actionable_round <- FALSE
  set_gs_round_feedback(NULL)
}, ignoreInit = TRUE)

# ---- Round submission ----
# This is the main runtime action in the Analysis tab.
#
# High-level flow:
#   1. refuse submission if the design changed after earlier saved rounds,
#   2. build the live test object on the first submission,
#   3. collect and validate all p-values for the selected global round,
#   4. apply the round to the live object and freeze the submitted history,
#   5. refresh the preview / live state so the next round is ready.
observeEvent(input$gs_submit_round, {
  set_gs_round_feedback(NULL)

  # If the user already submitted earlier rounds, those rows belong to one
  # specific finalized design. We compare the current plan/schedule signature
  # to the saved signature and stop if they no longer match, because continuing
  # would mix history from two different designs.
  if (nrow(rv$gs_analysis_history) && nzchar(rv$gs_applied_design_signature)) {
    current_signature <- gs_current_design_signature()
    if (!identical(current_signature, rv$gs_applied_design_signature)) {
      msg <- "The group sequential design changed after earlier submissions. Reset Analysis State before continuing."
      set_gs_round_feedback(msg, type = "error")
      showNotification(msg, type = "error", duration = 8)
      return(invisible(NULL))
    }
  }

  # The first successful submission needs a live GraphicalTesting object.
  # Later rounds reuse and mutate that same object so alpha recycling follows
  # the exact sequence of saved decisions.
  if (is.null(rv$ts_object) && !isTRUE(initialize_batch_gs_object(reset_history = TRUE))) {
    return(invisible(NULL))
  }

  # Build the round payload from the currently selected analysis round:
  # - figure out which rows are still actionable,
  # - read one-sided p-values from the UI,
  # - validate them,
  # - return the history rows we would append if submission succeeds.
  #
  # Any failure here should happen before we mutate runtime state or append
  # anything to frozen history.
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

  # Once we reach this block, the submission is valid and we commit it in one
  # place so the live object, frozen history, boundary preview, and UI state
  # stay in sync.
  tryCatch({
    # collect_round_submission() already called GraphicalTesting$test(), so the
    # live object now contains the package-truth decisions for this batch.
    rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
      dplyr::bind_rows(rv$gs_analysis_history, submission$history_rows)
    )
    rv$gs_stage_history <- gs_history_to_legacy_stage_history(rv$gs_analysis_history)

    # Recompute derived runtime outputs from the updated live object/history:
    # status tables, graph appearance, and the boundary preview shown for the
    # remaining analyses.
    bump_ts_state()
    refresh_ts_state()
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    set_ts_log(build_round_submit_log(submission$history_rows))

    # After submission, recompute which global round should be shown next.
    # This keeps the UI pointed at the next actionable work instead of leaving
    # the user on a round that is now complete.
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
        saved_result_count = nrow(submission$history_rows),
        hypothesis_count = dplyr::n_distinct(submission$history_rows$hypothesis),
        next_round = next_display_round
      ),
      type = "success"
    )
    # Some DataTables gain or lose visible columns after state changes, so
    # nudge the client to recalculate widths after the submission completes.
    session$sendCustomMessage("adjust-datatables", list())
  }, error = function(e) {
    # Anything unexpected during the commit phase still goes through the shared
    # log + feedback path so the user sees one consistent failure surface even
    # if the error came from deeper runtime code.
    set_ts_log(paste("Group sequential batch analysis error:", e$message))
    set_gs_round_feedback(paste("Group sequential batch analysis error:", e$message), type = "error")
    showNotification(paste("Group sequential batch analysis error:", e$message), type = "error", duration = 8)
  })
})

# This button clears only runtime analysis artifacts:
# - the live test object,
# - frozen submitted rounds,
# - live summary state,
# - current boundary preview.
#
# It keeps the design inputs so the user can rerun the same
# sequential design from a clean analysis state.
observeEvent(input$gs_reset_analysis_state, {
  reset_group_sequential_runtime_state()
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  rv$gs_round_selection_programmatic <- FALSE
  rv$gs_force_first_actionable_round <- TRUE
  set_gs_round_feedback(NULL)
  showNotification("Analysis state reset. The group sequential design tables were kept.", type = "message")
})

# This is a stronger reset than "Reset Analysis State". It discards the current
# hypothesis plan and analysis schedule, rebuilds both from the current graph,
# clears finalized/runtime state, and sends the user back to step 1 so they can
# review the new defaults from the beginning.
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

# Finalization is the explicit handoff from design-time editing to analysis.
# We only lock the design if three things are true:
#   1. the plan and schedule validate together,
#   2. the boundary schedule can actually be computed,
#   3. the computed preview contains at least one testable row.
#
# If finalization succeeds, we save the exact design signature so later round
# submissions can verify they still match the locked design.
observeEvent(input$gs_finalize_design, {
  rv$gs_finalize_feedback <- NULL
  design_state <- persist_group_sequential_design_state(update_boundary = FALSE)
  plan_tbl <- design_state$plan
  schedule_tbl <- design_state$schedule

  # Validation here checks the design as currently entered in the wizard, not
  # whatever happened to be saved earlier in rv.
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

  # A validated schedule is not enough on its own; we also require the boundary
  # computation to run cleanly for the current design.
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

  # An empty preview usually means there is nothing actionable to test, for
  # example because every hypothesis is inactive or has zero alpha.
  if (is.null(boundary_preview) || !nrow(boundary_preview)) {
    rv$gs_finalize_feedback <- list(
      text = "Cannot finalize: boundary schedule is empty. Check that hypotheses have alpha > 0.",
      type = "error"
    )
    showNotification("Cannot finalize: boundary schedule is empty.", type = "error", duration = 8)
    return(invisible(NULL))
  }

  # Finalization stores both the preview and the signature of the exact design
  # that produced it. That signature is later used to block submissions if the
  # user edits the design after saving analysis history.
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

# "Edit Design" is the inverse of finalization: it unlocks the wizard so the
# user can change plan and schedule inputs again.
#
# But if any analysis rounds were already submitted, those rows belong to the
# previously locked design. Unlocking the design at that point would sever the
# connection between frozen history and the design that produced it, so we
# require the user to reset runtime analysis state first.
observeEvent(input$gs_edit_design, {
  if (nrow(rv$gs_analysis_history)) {
    showNotification(
      "Cannot edit design after submitting analysis times. Reset analysis state first.",
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
