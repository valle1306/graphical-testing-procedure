# ------------------------------------------------------------------------------
# Group-sequential outputs.
#
# This file builds the visible UI for the sequential workflow after the
# underlying plan, schedule, boundary preview, and runtime state already exist.
#
# Broadly, these outputs do four jobs:
#   1. render the design wizard inputs and helper text,
#   2. render the boundary-review / finalization surfaces,
#   3. render the analysis-round entry UI and feedback,
#   4. render live and frozen analysis summaries.
#
# The calculation-heavy logic lives in helper/state files. The code here is
# mostly about translating reactive state in rv into visible controls, tables,
# and messages that stay synchronized with the current workflow stage.
# ------------------------------------------------------------------------------

# For each hypothesis row in the plan table, render the rule-specific parameter
# control that belongs in the "Rule Parameters" column.
#
# The control depends on the currently selected spending rule:
# - Custom -> free-text cumulative alpha values
# - HSD -> gamma numeric input
# - Haybittle-Peto -> p1 numeric input
# - other built-in rules -> no extra parameter UI
observe({
  node_tbl <- sanitize_nodes_tbl(rv$nodes)
  if (!nrow(node_tbl)) {
    return(invisible(NULL))
  }

  lapply(seq_len(nrow(node_tbl)), function(i) {
    local({
      node_id <- as.integer(node_tbl$id[[i]])
      output_id <- paste0("gs_plan_params_", node_id)

      output[[output_id]] <- renderUI({
        profile_reactivity("renderUI:gs_plan_params", {
          plan_tbl <- build_default_gs_hypothesis_plan(rv$nodes, isolate(rv$gs_hypothesis_plan))
          plan_row <- plan_tbl %>%
            dplyr::filter(.data$id == !!node_id) %>%
            dplyr::slice(1)

          if (!nrow(plan_row)) {
            return(tags$div(class = "gs-muted", "Auto from selected rule"))
          }

          selected_rule <- read_scalar_character_input(paste0("gs_plan_rule_", node_id))
          if (is.null(selected_rule)) {
            selected_rule <- plan_row$alpha_spending[[1]]
          }
          selected_rule <- normalize_spending_rule(selected_rule)

          planned_analyses <- read_scalar_integer_input(
            paste0("gs_plan_k_", node_id),
            default = plan_row$planned_analyses[[1]]
          )
          if (is.na(planned_analyses) || planned_analyses < 1L) {
            planned_analyses <- plan_row$planned_analyses[[1]]
          }

          design_alpha <- rv$nodes$alpha[match(plan_row$hypothesis[[1]], rv$nodes$hypothesis)]
          if (!length(design_alpha) || !is.finite(design_alpha[[1]])) {
            design_alpha <- NA_real_
          }

          if (identical(selected_rule, "Custom")) {
            return(
              textInput(
                inputId = paste0("gs_plan_custom_", node_id),
                label = NULL,
                value = plan_row$custom_cumulative_alpha[[1]],
                placeholder = gs_custom_cumulative_alpha_placeholder(planned_analyses, design_alpha[[1]])
              )
            )
          }

          if (identical(selected_rule, "HSD")) {
            return(
              numericInput(
                inputId = paste0("gs_plan_gamma_", node_id),
                label = NULL,
                value = plan_row$hsd_gamma[[1]],
                step = 0.5,
                width = "100%"
              )
            )
          }

          if (identical(selected_rule, "Haybittle-Peto")) {
            return(
              numericInput(
                inputId = paste0("gs_plan_haybittle_p1_", node_id),
                label = NULL,
                value = plan_row$haybittle_p1[[1]],
                min = 0,
                step = 0.0001,
                width = "100%"
              )
            )
          }

          tags$div(class = "gs-muted", "Auto from selected rule")
        }, note = sprintf("id=%s", node_id))
      })
    })
  })

  invisible(NULL)
})

# Render the wizard stepper at the top of the Group Sequential Design flow.
# It reflects both the current step and whether the design has already been
# finalized, in which case every prior step is shown as complete.
output$gs_stepper_indicator <- renderUI({
  step <- rv$gs_wizard_step
  finalized <- isTRUE(rv$gs_design_finalized)
  labels <- c("Hypothesis Plan", "Analysis Timing", "Boundary Review")
  step_tags <- lapply(seq_along(labels), function(i) {
    cls <- if (finalized || i < step) {
      "gs-step completed"
    } else if (i == step) {
      "gs-step active"
    } else {
      "gs-step"
    }
    circle_content <- if (finalized || i < step) "\u2713" else as.character(i)
    tag <- tags$div(
      class = cls,
      tags$div(class = "gs-step-circle", circle_content),
      tags$span(class = "gs-step-label", labels[[i]])
    )
    if (i < length(labels)) {
      conn_cls <- if (finalized || i < step) "gs-step-connector done" else "gs-step-connector"
      tagList(tag, tags$div(class = conn_cls))
    } else {
      tag
    }
  })
  tags$div(class = "gs-stepper", step_tags)
})

# Show a compact plain-language summary of the current graph and schedule so the
# user can tell what design context the wizard is operating on.
output$gs_design_context <- renderUI({
  if (!nrow(rv$nodes)) {
    return(tags$span("Create hypotheses in the Design tab first. The group sequential design tables will appear here automatically."))
  }
  plan_tbl <- rv$gs_hypothesis_plan
  if (!nrow(plan_tbl)) {
    plan_tbl <- build_default_gs_hypothesis_plan(rv$nodes, isolate(rv$gs_hypothesis_plan))
  }
  schedule_signature <- rv$gs_analysis_schedule_round_signature
  schedule_tbl <- isolate(gs_analysis_schedule_display_tbl(plan_tbl = plan_tbl, schedule_tbl = rv$gs_analysis_schedule))
  round_values <- gs_round_choice_values(schedule_tbl)
  tags$span(
    sprintf(
      "Current graph hypotheses: %s. This design currently spans %s analysis time%s and uses one-sided testing throughout.",
      format_hypothesis_list(rv$nodes$hypothesis),
      length(round_values),
      ifelse(length(round_values) == 1L, "", "s")
    )
  )
})

# Render the Step 1 table where each hypothesis gets:
# - a number of planned looks,
# - an alpha-spending rule,
# - any rule-specific parameters.
#
# The table is rebuilt from the current graph plus any persisted plan state.
output$gs_hypothesis_plan_ui <- renderUI({
  profile_reactivity("renderUI:gs_hypothesis_plan", {
    node_signature <- paste(rv$nodes$id, rv$nodes$hypothesis, collapse = "|")
    if (!nrow(rv$nodes)) {
      return(tags$p("Add at least one hypothesis in the Design tab before building the group sequential design."))
    }
    plan_tbl <- isolate(build_default_gs_hypothesis_plan(rv$nodes, rv$gs_hypothesis_plan))
    tags$div(
      class = "gs-table-shell",
      tags$table(
        class = "gs-input-table",
        tags$thead(
          tags$tr(
            tags$th("Hypothesis"),
            tags$th("Planned Analyses (K)"),
            tags$th("Alpha Spending Function"),
            tags$th("Rule Parameters")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(plan_tbl)), function(i) {
            id <- plan_tbl$id[[i]]
            planned_analyses <- max(1L, as.integer(plan_tbl$planned_analyses[[i]]))
            selected_rule <- normalize_spending_rule(plan_tbl$alpha_spending[[i]])
            tags$tr(
              tags$td(tags$strong(plan_tbl$hypothesis[[i]])),
              tags$td(
                numericInput(
                  inputId = paste0("gs_plan_k_", id),
                  label = NULL,
                  value = planned_analyses,
                  min = 1,
                  step = 1,
                  width = "100%"
                )
              ),
              tags$td(
                selectInput(
                  inputId = paste0("gs_plan_rule_", id),
                  label = NULL,
                  choices = gs_rule_choices(include_custom = TRUE),
                  selected = selected_rule,
                  width = "100%"
                )
              ),
              tags$td(uiOutput(paste0("gs_plan_params_", id)))
            )
          })
        )
      )
    )
  }, note = sprintf("nodes=%s", nrow(rv$nodes)))
})

# Keep schedule-validation feedback separate from the schedule input table.
# That lets validation update on every timing edit without forcing the table
# itself to re-render and interrupt the user's typing.
output$gs_schedule_validation_ui <- renderUI({
  if (!nrow(rv$gs_hypothesis_plan)) return(NULL)
  # The direct dependency on rv$gs_analysis_schedule is intentional. That table
  # is updated as the user edits timing fields, so validation stays current
  # while the input table remains stable.
  schedule_tbl <- rv$gs_analysis_schedule
  plan_tbl <- isolate(rv$gs_hypothesis_plan)
  validation <- validate_gs_analysis_schedule(
    schedule_tbl = schedule_tbl,
    plan_tbl = plan_tbl,
    notify = FALSE
  )
  if (isTRUE(validation$ok)) return(NULL)
  tags$div(class = "gs-inline-note", style = "color:#991b1b;", validation$message)
})

# Render the Step 2 schedule table. Each row corresponds to one planned look
# for one hypothesis and lets the user edit:
# - the global analysis round where that look happens,
# - the information fraction at that look.
#
# The table groups rows visually by analysis round so the schedule is easier to
# scan as a global timeline rather than as isolated per-hypothesis rows.
output$gs_analysis_schedule_ui <- renderUI({
  plan_tbl <- rv$gs_hypothesis_plan
  if (!nrow(plan_tbl) && nrow(rv$nodes)) {
    plan_tbl <- build_default_gs_hypothesis_plan(rv$nodes, isolate(rv$gs_hypothesis_plan))
  }
  if (!nrow(plan_tbl)) {
    return(tags$p("Define the hypothesis setup first."))
  }
  # Re-render only when the committed round assignments change. This avoids
  # shuffling rows while the user types information fractions, but still lets
  # the display snap back into round order after a real round edit.
  schedule_signature <- rv$gs_analysis_schedule_round_signature
  schedule_tbl <- isolate(gs_analysis_schedule_display_tbl(plan_tbl, rv$gs_analysis_schedule))
  if (!nrow(schedule_tbl)) {
    return(tags$p("Define the hypothesis setup first."))
  }
  round_group_start <- c(TRUE, diff(schedule_tbl$analysis_round) != 0L)
  round_group_index <- cumsum(round_group_start)
  round_group_class <- ifelse(round_group_index %% 2L == 1L, "gs-round-group-odd", "gs-round-group-even")
  tags$div(
    class = "gs-table-shell",
    tags$table(
      class = "gs-input-table",
      tags$thead(
        tags$tr(
          tags$th("Analysis Time"),
          tags$th("Hypothesis"),
          tags$th("Hypothesis Stage"),
          tags$th("Information Fraction")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(schedule_tbl)), function(i) {
          key <- schedule_tbl$schedule_key[[i]]
          row_class <- round_group_class[[i]]
          if (isTRUE(round_group_start[[i]])) {
            row_class <- paste(row_class, "gs-round-group-start")
          }
          tags$tr(
            class = row_class,
            tags$td(
              numericInput(
                inputId = paste0("gs_schedule_round_", key),
                label = NULL,
                value = schedule_tbl$analysis_round[[i]],
                min = 1,
                step = 1,
                width = "100%"
              )
            ),
            tags$td(tags$strong(schedule_tbl$hypothesis[[i]])),
            tags$td(
              sprintf(
                "%s of %s%s",
                schedule_tbl$hypothesis_stage[[i]],
                schedule_tbl$planned_analyses[[i]],
                ifelse(isTRUE(schedule_tbl$is_final[[i]]), " (final)", "")
              )
            ),
            tags$td(
              textInput(
                inputId = paste0("gs_schedule_info_", key),
                label = NULL,
                value = format_plain_number(schedule_tbl$information_fraction[[i]]),
                placeholder = "e.g. 0.5"
              )
            )
          )
        })
      )
    )
  )
})

# Step 3 boundary-review table. This shows the computed design-time boundary
# schedule the user is about to lock in, one row per (round, hypothesis, stage).
output$gs_boundary_schedule_table <- renderDT({
  quiet_jsonlite_warning({
    preview_tbl <- rv$gs_boundary_preview
    display_tbl <- gs_boundary_review_display_tbl(preview_tbl)
    datatable(
      display_tbl,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(dom = "t", pageLength = 12, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
    )
  })
})

# Show success/error feedback for the "Finalize Design" action.
output$gs_finalize_feedback <- renderUI({
  feedback <- rv$gs_finalize_feedback
  if (is.null(feedback) || !length(feedback$text) || !nzchar(feedback$text[[1]])) {
    return(NULL)
  }
  alert_class <- if (identical(feedback$type, "error")) "alert-danger" else "alert-success"
  tags$div(class = paste("alert", alert_class), style = "margin-top: 12px; margin-bottom: 0;", feedback$text[[1]])
})

# Show boundary-preview computation problems in the boundary-review step.
output$gs_boundary_preview_feedback <- renderUI({
  message <- rv$gs_boundary_preview_message
  if (is.null(message) || !length(message) || !nzchar(trimws(as.character(message[[1]])))) {
    return(NULL)
  }
  tags$div(class = "alert alert-danger", style = "margin-top: 12px; margin-bottom: 12px;", as.character(message[[1]]))
})

# Show the same boundary-preview message again on the Analysis tab so analysis
# surfaces can explain why round entry is unavailable.
output$gs_analysis_preview_feedback <- renderUI({
  message <- rv$gs_boundary_preview_message
  if (is.null(message) || !length(message) || !nzchar(trimws(as.character(message[[1]])))) {
    return(NULL)
  }
  tags$div(class = "alert alert-danger", style = "margin-bottom: 12px;", as.character(message[[1]]))
})

# Render the global analysis-round dropdown.
#
# This dropdown is driven by the boundary preview plus submission history:
# - already-submitted rows are removed,
# - only actionable rounds remain selectable,
# - once all rounds are complete, the control is replaced by a closed-state note.
output$gs_analysis_round_ui <- renderUI({
  preview_tbl <- rv$gs_boundary_preview
  if (is.null(preview_tbl) || !nrow(preview_tbl)) {
    return(tags$p("Define a valid boundary schedule before submitting analysis results."))
  }

  selected_round <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  state <- gs_analysis_round_state(
    preview_tbl = preview_tbl,
    history_tbl = rv$gs_analysis_history,
    selected_round = selected_round
  )
  if (isTRUE(state$is_complete)) {
    return(
      tags$div(
        class = "gs-inline-note gs-complete-note",
        gs_analysis_round_closed_state_message()
      )
    )
  }

  display_round <- gs_resolve_analysis_round_target(
    selected_round = state$selected_round,
    actionable_rounds = state$actionable_rounds,
    next_actionable_round = state$next_actionable_round,
    force_first_actionable = isTRUE(rv$gs_force_first_actionable_round)
  )
  if (is.na(display_round)) {
    return(tags$p("Choose an actionable analysis round to enter one-sided p-values."))
  }

  selectInput(
    "gs_analysis_round",
    "Global Analysis Round",
    choices = stats::setNames(
      as.character(state$actionable_rounds),
      paste("Round", state$actionable_rounds)
    ),
    selected = as.character(display_round),
    width = "100%"
  )
})

# Keep the selected round synchronized with the currently actionable rounds.
#
# This observer is what nudges the UI to the first/next valid round after
# submissions or resets. When it changes the dropdown itself, it marks the
# change as programmatic so the event handler in sequential_events.R knows not
# to treat it as a user edit.
observe({
  selected_round <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  state <- gs_analysis_round_state(
    preview_tbl = rv$gs_boundary_preview,
    history_tbl = rv$gs_analysis_history,
    selected_round = selected_round
  )
  if (!length(state$actionable_rounds)) {
    rv$gs_round_selection_programmatic <- FALSE
    rv$gs_force_first_actionable_round <- FALSE
    return(invisible(NULL))
  }
  target_round <- gs_resolve_analysis_round_target(
    selected_round = selected_round,
    actionable_rounds = state$actionable_rounds,
    next_actionable_round = state$next_actionable_round,
    force_first_actionable = isTRUE(rv$gs_force_first_actionable_round)
  )
  if (!identical(selected_round, target_round)) {
    rv$gs_round_selection_programmatic <- TRUE
  }
  updateSelectInput(
    session,
    "gs_analysis_round",
    choices = stats::setNames(as.character(state$actionable_rounds), paste("Round", state$actionable_rounds)),
    selected = as.character(target_round)
  )
})

# Render per-round success/error feedback in the Analysis tab. Once all rounds
# are complete, this feedback is hidden because there is no longer an active
# round-entry surface it should attach to.
output$gs_round_feedback <- renderUI({
  feedback <- rv$gs_round_feedback
  if (is.null(feedback) || !length(feedback$text) || !nzchar(feedback$text[[1]])) {
    return(NULL)
  }
  state <- gs_analysis_round_state(
    preview_tbl = rv$gs_boundary_preview,
    history_tbl = rv$gs_analysis_history,
    selected_round = read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  )
  if (isTRUE(state$is_complete)) {
    return(NULL)
  }
  alert_class <- if (identical(feedback$type, "error")) "alert-danger" else "alert-success"
  tags$div(class = paste("alert", alert_class), style = "margin-top: 12px;", feedback$text[[1]])
})

# Render the per-hypothesis entry table for the currently displayed analysis
# round. This is the UI the user fills in before clicking "Submit Analysis Round".
#
# Each row comes from the remaining boundary-preview rows for the selected round.
# Ready rows get a p-value input; non-ready rows are shown as informational only.
output$gs_round_entry_ui <- renderUI({
  preview_tbl <- rv$gs_boundary_preview
  if (is.null(preview_tbl) || !nrow(preview_tbl)) {
    return(tags$p("Define a valid boundary schedule before submitting analysis results."))
  }

  selected_round <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  state <- gs_analysis_round_state(
    preview_tbl = preview_tbl,
    history_tbl = rv$gs_analysis_history,
    selected_round = selected_round
  )
  if (isTRUE(state$is_complete)) {
    return(NULL)
  }

  display_round <- gs_resolve_analysis_round_target(
    selected_round = state$selected_round,
    actionable_rounds = state$actionable_rounds,
    next_actionable_round = state$next_actionable_round,
    force_first_actionable = isTRUE(rv$gs_force_first_actionable_round)
  )
  if (is.na(display_round)) {
    return(tags$p("Choose an actionable analysis round to enter one-sided p-values."))
  }

  display_state <- if (identical(display_round, state$selected_round)) {
    state
  } else {
    gs_analysis_round_state(
      preview_tbl = preview_tbl,
      history_tbl = rv$gs_analysis_history,
      selected_round = display_round
    )
  }
  if (!nrow(display_state$remaining_rows)) {
    return(tags$p(sprintf("No hypotheses are scheduled at analysis round %s.", display_round)))
  }

  tags$div(
    class = "gs-table-shell",
    tags$table(
      class = "gs-input-table",
      tags$thead(
        tags$tr(
          tags$th("Hypothesis"),
          tags$th("Stage"),
          tags$th("Current Alpha"),
          tags$th("Planned Information Fraction"),
          tags$th("Boundary p"),
          tags$th("Entered One-Sided p"),
          tags$th("Decision")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(display_state$remaining_rows)), function(i) {
          key <- display_state$remaining_rows$schedule_key[[i]]
          tags$tr(
            tags$td(tags$strong(display_state$remaining_rows$hypothesis[[i]])),
            tags$td(
              sprintf(
                "%s of %s%s",
                display_state$remaining_rows$hypothesis_stage[[i]],
                display_state$remaining_rows$planned_analyses[[i]],
                ifelse(isTRUE(display_state$remaining_rows$is_final[[i]]), " (final)", "")
              )
            ),
            tags$td(format_plain_number(display_state$remaining_rows$current_alpha[[i]])),
            tags$td(format_plain_number(display_state$remaining_rows$timing[[i]])),
            tags$td(
              if (is.na(display_state$remaining_rows$p_boundary[[i]])) {
                ""
              } else {
                format_plain_number(display_state$remaining_rows$p_boundary[[i]])
              }
            ),
            tags$td(
              if (identical(display_state$remaining_rows$status[[i]], "Ready")) {
                numericInput(
                  inputId = paste0("gs_round_p_", key),
                  label = NULL,
                  value = NA_real_,
                  min = 0,
                  max = 1,
                  step = 0.0001,
                  width = "100%"
                )
              } else {
                tags$div(class = "gs-muted", "Not applicable")
              }
            ),
            tags$td(
              if (identical(display_state$remaining_rows$status[[i]], "Ready")) {
                "Computed on submit"
              } else {
                display_state$remaining_rows$status[[i]]
              }
            )
          )
        })
      )
    ),
    if (any(display_state$remaining_rows$status != "Ready")) {
      tags$div(
        class = "gs-inline-note",
        "Only hypotheses that are still active in the graph and ready for one-sided testing accept p-values at this round."
      )
    }
  )
})

# Show the submit button only when there is still at least one actionable round.
output$gs_submit_round_ui <- renderUI({
  preview_tbl <- rv$gs_boundary_preview
  if (is.null(preview_tbl) || !nrow(preview_tbl)) {
    return(NULL)
  }
  state <- gs_analysis_round_state(
    preview_tbl = preview_tbl,
    history_tbl = rv$gs_analysis_history,
    selected_round = read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  )
  if (isTRUE(state$is_complete)) {
    return(NULL)
  }
  tags$div(
    class = "gs-actions",
    actionButton("gs_submit_round", "Submit Analysis Round", class = "btn btn-warning")
  )
})

# Frozen submission history table. Each row is a saved analysis result and keeps
# the alpha, boundary, and decision values that were true at the time of submit.
output$gs_submitted_analyses_table <- renderDT({
  quiet_jsonlite_warning({
    history_tbl <- rv$gs_analysis_history
    display_tbl <- gs_submitted_analyses_display_tbl(history_tbl)
    display_tbl <- display_tbl %>%
      dplyr::mutate(
        Submission = vapply(Submission, gs_submission_chip_html, character(1)),
        `Round / Stage` = vapply(
          `Round / Stage`,
          function(x) gs_chip_html(x, "round-stage", title = "Round / stage"),
          character(1)
        ),
        Decision = vapply(Decision, gs_decision_chip_html, character(1))
      )
    datatable(
      display_tbl,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      escape = FALSE,
      options = list(dom = "t", pageLength = 12, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
    )
  })
})

# Live analysis-state table. Unlike the submitted-history table, this view is a
# current summary derived from the latest runtime state and points the user to
# the next scheduled global round for each hypothesis.
output$gs_live_analysis_state_table <- renderDT({
  quiet_jsonlite_warning({
    status_tbl <- build_ts_status_table()
    display_tbl <- gs_live_analysis_state_tbl(
      status_tbl = status_tbl,
      schedule_tbl = rv$gs_analysis_schedule,
      history_tbl = rv$gs_analysis_history
    )
    display_tbl <- display_tbl %>%
      dplyr::mutate(
        `Last Submitted Round / Stage` = vapply(
          `Last Submitted Round / Stage`,
          function(x) {
            if (!nzchar(gs_scalar_display_text(x))) {
              return("")
            }
            gs_chip_html(x, "round-stage", title = "Latest submitted round / stage")
          },
          character(1)
        ),
        Decision = vapply(Decision, gs_decision_chip_html, character(1))
      )
    datatable(
      display_tbl,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      escape = FALSE,
      options = list(dom = "t", pageLength = 12, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
    )
  })
})

# Summary card at the top of the Analysis tab.
#
# If no design is finalized, this card acts as a workflow gate and explains why
# analysis submission is not ready yet. If the design is finalized, it instead
# shows a compact locked-design summary and, before any submissions occur, an
# "Edit Design" action.
output$gs_analysis_design_summary <- renderUI({
  if (!isTRUE(rv$gs_design_finalized)) {
    return(
      div(
        class = "gs-card",
        style = "border-color:#fbbf24; background:linear-gradient(180deg, #fffbeb 0%, #fefce8 100%);",
        tags$h4(class = "gs-section-title", style = "color:#92400e;", "No finalized design"),
        tags$p(
          style = "color:#78350f; margin:0;",
          "Go to the Group Sequential Design tab and click ",
          tags$strong("Finalize Design"),
          " before submitting analysis rounds."
        )
      )
    )
  }
  has_submissions <- nrow(rv$gs_analysis_history) > 0
  plan_tbl <- rv$gs_hypothesis_plan
  if (!nrow(plan_tbl) && nrow(rv$nodes)) {
    plan_tbl <- build_default_gs_hypothesis_plan(rv$nodes, isolate(rv$gs_hypothesis_plan))
  }
  schedule_signature <- rv$gs_analysis_schedule_round_signature
  schedule_tbl <- isolate(gs_analysis_schedule_display_tbl(plan_tbl = plan_tbl, schedule_tbl = rv$gs_analysis_schedule))
  round_values <- gs_round_choice_values(schedule_tbl)
  div(
    class = "gs-card",
    style = "border-color:#86efac; background:linear-gradient(180deg, #f0fdf4 0%, #f8fff8 100%);",
    tags$h4(class = "gs-section-title", style = "color:#166534;", "Locked design"),
    tags$p(
      style = "color:#14532d; margin:0;",
      sprintf(
        "Hypotheses: %s. Analysis times: %s. Design signature applied.",
        format_hypothesis_list(rv$nodes$hypothesis),
        length(round_values)
      )
    ),
    if (!has_submissions) {
      div(
        style = "margin-top: 10px;",
        actionButton("gs_edit_design", "Edit Design", class = "btn btn-outline-secondary btn-sm")
      )
    }
  )
})

# The primary action at the bottom of the design wizard changes meaning after
# finalization: before lock it is "Finalize Design"; after lock it becomes
# "Go to Analysis".
output$gs_finalize_action_ui <- renderUI({
  if (isTRUE(rv$gs_design_finalized)) {
    return(actionButton("gs_go_to_analysis", "Go to Analysis", class = "btn btn-primary"))
  }
  actionButton("gs_finalize_design", "Finalize Design", class = "btn btn-primary")
})
