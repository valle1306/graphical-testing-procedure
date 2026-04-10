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

output$gs_design_context <- renderUI({
  if (!nrow(rv$nodes)) {
    return(tags$span("Create hypotheses in the Design tab first. The group sequential design tables will appear here automatically."))
  }
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
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

output$gs_hypothesis_plan_ui <- renderUI({
  if (!nrow(rv$nodes)) {
    return(tags$p("Add at least one hypothesis in the Design tab before building the group sequential design."))
  }
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
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
          selected_rule <- read_scalar_character_input(paste0("gs_plan_rule_", id))
          if (is.null(selected_rule)) {
            selected_rule <- plan_tbl$alpha_spending[[i]]
          }
          selected_rule <- normalize_spending_rule(selected_rule)
          rule_input <- selectInput(
            inputId = paste0("gs_plan_rule_", id),
            label = NULL,
            choices = gs_rule_choices(include_custom = TRUE),
            selected = selected_rule,
            width = "100%"
          )

          hsd_gamma_val <- if ("hsd_gamma" %in% names(plan_tbl)) plan_tbl$hsd_gamma[[i]] else -4
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
            tags$td(rule_input),
            tags$td(
              if (identical(selected_rule, "Custom")) {
                textInput(
                  inputId = paste0("gs_plan_custom_", id),
                  label = NULL,
                  value = plan_tbl$custom_cumulative_alpha[[i]],
                  placeholder = "e.g. 0.5, 1"
                )
              } else if (identical(selected_rule, "HSD")) {
                numericInput(
                  inputId = paste0("gs_plan_gamma_", id),
                  label = NULL,
                  value = hsd_gamma_val,
                  step = 0.5,
                  width = "100%"
                )
              } else {
                tags$div(class = "gs-muted", "Auto from selected rule")
              }
            )
          )
        })
      )
    )
  )
})

# Validation message in a separate output so it updates on every fraction change
# without recreating the input table (which would interrupt the user's typing).
output$gs_schedule_validation_ui <- renderUI({
  if (!nrow(rv$gs_hypothesis_plan)) return(NULL)
  # Reactive dependency on rv$gs_analysis_schedule is intentional here:
  # the observer in sequential_settings.R updates it on every input change,
  # so validation reflects the latest typed values without touching the inputs.
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

output$gs_analysis_schedule_ui <- renderUI({
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
  if (!nrow(plan_tbl)) {
    return(tags$p("Define the hypothesis setup first."))
  }
  # Re-render only when the committed round assignments change. That keeps the
  # table stable while the user edits information fractions, but lets the row
  # order snap into round order once a round change is committed.
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

output$gs_finalize_feedback <- renderUI({
  feedback <- rv$gs_finalize_feedback
  if (is.null(feedback) || !length(feedback$text) || !nzchar(feedback$text[[1]])) {
    return(NULL)
  }
  alert_class <- if (identical(feedback$type, "error")) "alert-danger" else "alert-success"
  tags$div(class = paste("alert", alert_class), style = "margin-top: 12px; margin-bottom: 0;", feedback$text[[1]])
})

output$gs_boundary_preview_feedback <- renderUI({
  message <- rv$gs_boundary_preview_message
  if (is.null(message) || !length(message) || !nzchar(trimws(as.character(message[[1]])))) {
    return(NULL)
  }
  tags$div(class = "alert alert-danger", style = "margin-top: 12px; margin-bottom: 12px;", as.character(message[[1]]))
})

output$gs_analysis_preview_feedback <- renderUI({
  message <- rv$gs_boundary_preview_message
  if (is.null(message) || !length(message) || !nzchar(trimws(as.character(message[[1]])))) {
    return(NULL)
  }
  tags$div(class = "alert alert-danger", style = "margin-bottom: 12px;", as.character(message[[1]]))
})

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
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
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

output$gs_finalize_action_ui <- renderUI({
  if (isTRUE(rv$gs_design_finalized)) {
    return(actionButton("gs_go_to_analysis", "Go to Analysis", class = "btn btn-primary"))
  }
  actionButton("gs_finalize_design", "Finalize Design", class = "btn btn-primary")
})
