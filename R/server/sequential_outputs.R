output$gs_design_context <- renderUI({
  if (!nrow(rv$nodes)) {
    return(tags$span("Create hypotheses in the Design tab first. The group sequential design tables will appear here automatically."))
  }
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
  schedule_tbl <- collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = FALSE)
  round_values <- gs_round_choice_values(schedule_tbl)
  tags$span(
    sprintf(
      "Current graph hypotheses: %s. This design currently spans %s global analysis round%s and uses one-sided testing throughout.",
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
          tags$th("Planned Looks (K)"),
          tags$th("Alpha Spending Function"),
          tags$th("Cumulative Alpha by Look")
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
          if (planned_analyses == 1L) {
            rule_input <- shinyjs::disabled(rule_input)
          }
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
              if (planned_analyses == 1L) {
                tags$div(class = "gs-muted", "Final-only: the full alpha is used at the final analysis.")
              } else if (identical(selected_rule, "Custom")) {
                textInput(
                  inputId = paste0("gs_plan_custom_", id),
                  label = NULL,
                  value = plan_tbl$custom_cumulative_alpha[[i]],
                  placeholder = "e.g. 0.5, 1"
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

output$gs_analysis_schedule_ui <- renderUI({
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
  if (!nrow(plan_tbl)) {
    return(tags$p("Define the hypothesis setup first."))
  }
  schedule_tbl <- collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = FALSE)
  validation <- validate_gs_analysis_schedule(schedule_tbl = schedule_tbl, plan_tbl = plan_tbl, notify = FALSE)
  schedule_tbl <- validation$schedule
  tagList(
    if (!isTRUE(validation$ok)) {
      tags$div(class = "gs-inline-note", style = "color:#991b1b;", validation$message)
    },
    tags$div(
      class = "gs-table-shell",
      tags$table(
        class = "gs-input-table",
        tags$thead(
          tags$tr(
            tags$th("Global Round"),
            tags$th("Hypothesis"),
            tags$th("Look j of K"),
            tags$th("Information Fraction")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(schedule_tbl)), function(i) {
            key <- schedule_tbl$schedule_key[[i]]
            tags$tr(
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
                  "Look %s of %s%s",
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
  )
})

output$gs_boundary_schedule_table <- renderDT({
  quiet_jsonlite_warning({
    preview_tbl <- rv$gs_boundary_preview
    if (is.null(preview_tbl) || !nrow(preview_tbl)) {
      return(datatable(
        data.frame(
          Round = integer(),
          Hypothesis = character(),
          Stage = integer(),
          `Info Fraction` = numeric(),
          Rule = character(),
          `Boundary p` = numeric(),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
      ))
    }
    display_tbl <- preview_tbl %>%
      dplyr::transmute(
        Round = analysis_round,
        Hypothesis = hypothesis,
        Stage = hypothesis_stage,
        `Planned Analyses` = planned_analyses,
        `Info Fraction` = format(timing, trim = TRUE, scientific = FALSE),
        Rule = alpha_spending,
        `Current Alpha` = ifelse(is.na(current_alpha), "", format(current_alpha, trim = TRUE, scientific = FALSE)),
        `Stage Alpha` = ifelse(is.na(stage_alpha), "", format(stage_alpha, trim = TRUE, scientific = FALSE)),
        `Cumulative Alpha` = ifelse(is.na(cumulative_alpha_spent), "", format(cumulative_alpha_spent, trim = TRUE, scientific = FALSE)),
        `Boundary p` = ifelse(is.na(p_boundary), "", format(p_boundary, trim = TRUE, scientific = FALSE)),
        `Boundary z` = ifelse(is.na(z_boundary), "", format(z_boundary, trim = TRUE, scientific = FALSE)),
        Status = status
      )
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

observe({
  preview_tbl <- rv$gs_boundary_preview
  round_values <- if (!is.null(preview_tbl) && nrow(preview_tbl)) {
    sort(unique(preview_tbl$analysis_round))
  } else {
    integer()
  }
  selected_round <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  if (!length(round_values)) {
    updateSelectInput(session, "gs_analysis_round", choices = character(0), selected = character(0))
  } else {
    if (is.na(selected_round) || !selected_round %in% round_values) {
      selected_round <- round_values[[1]]
    }
    updateSelectInput(
      session,
      "gs_analysis_round",
      choices = stats::setNames(as.character(round_values), paste("Round", round_values)),
      selected = as.character(selected_round)
    )
  }
})

output$gs_round_feedback <- renderUI({
  feedback <- rv$gs_round_feedback
  if (is.null(feedback) || !length(feedback$text) || !nzchar(feedback$text[[1]])) {
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

  round_value <- read_scalar_integer_input("gs_analysis_round", default = NA_integer_)
  if (is.na(round_value)) {
    return(tags$p("Choose an analysis round to enter one-sided p-values."))
  }

  round_rows <- preview_tbl %>%
    dplyr::filter(analysis_round == round_value) %>%
    dplyr::arrange(hypothesis, hypothesis_stage)
  if (!nrow(round_rows)) {
    return(tags$p(sprintf("No hypotheses are scheduled at analysis round %s.", round_value)))
  }

  submitted_keys <- gs_submitted_schedule_keys()
  remaining_rows <- round_rows %>% dplyr::filter(!schedule_key %in% submitted_keys)
  if (!nrow(remaining_rows)) {
    return(tags$p(sprintf("All scheduled hypotheses for analysis round %s have already been submitted.", round_value)))
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
        lapply(seq_len(nrow(remaining_rows)), function(i) {
          key <- remaining_rows$schedule_key[[i]]
          tags$tr(
            tags$td(tags$strong(remaining_rows$hypothesis[[i]])),
            tags$td(
              sprintf(
                "%s of %s%s",
                remaining_rows$hypothesis_stage[[i]],
                remaining_rows$planned_analyses[[i]],
                ifelse(isTRUE(remaining_rows$is_final[[i]]), " (final)", "")
              )
            ),
            tags$td(format_plain_number(remaining_rows$current_alpha[[i]])),
            tags$td(format_plain_number(remaining_rows$timing[[i]])),
            tags$td(
              if (is.na(remaining_rows$p_boundary[[i]])) {
                ""
              } else {
                format_plain_number(remaining_rows$p_boundary[[i]])
              }
            ),
            tags$td(
              if (identical(remaining_rows$status[[i]], "Ready")) {
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
              if (identical(remaining_rows$status[[i]], "Ready")) {
                "Computed on submit"
              } else {
                remaining_rows$status[[i]]
              }
            )
          )
        })
      )
    ),
    if (any(remaining_rows$status != "Ready")) {
      tags$div(
        class = "gs-inline-note",
        "Only hypotheses that are still active in the graph and ready for one-sided testing accept p-values at this round."
      )
    }
  )
})

output$gs_submitted_analyses_table <- renderDT({
  quiet_jsonlite_warning({
    history_tbl <- rv$gs_analysis_history
    if (is.null(history_tbl) || !nrow(history_tbl)) {
      return(datatable(
        data.frame(
          Submission = integer(),
          Round = integer(),
          Hypothesis = character(),
          Stage = integer(),
          P = numeric(),
          `Boundary p` = numeric(),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
      ))
    }
    display_tbl <- history_tbl %>%
      dplyr::transmute(
        Submission = submission,
        Round = analysis_round,
        Hypothesis = hypothesis,
        Stage = hypothesis_stage,
        Rule = alpha_spending,
        `Info Fraction` = format(information_fraction, trim = TRUE, scientific = FALSE),
        `Current Alpha` = format(current_alpha, trim = TRUE, scientific = FALSE),
        P = format(p_value, trim = TRUE, scientific = FALSE),
        `Boundary p` = format(boundary_p, trim = TRUE, scientific = FALSE),
        `Boundary z` = format(boundary_z, trim = TRUE, scientific = FALSE),
        Decision = decision
      )
    datatable(
      display_tbl,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(dom = "t", pageLength = 12, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
    )
  })
})

output$gs_analysis_status_table <- renderDT({
  quiet_jsonlite_warning({
    status_tbl <- build_ts_status_table()
    if (is.null(status_tbl) || !nrow(status_tbl)) {
      return(datatable(
        data.frame(
          Hypothesis = character(),
          `Current Alpha` = numeric(),
          Decision = character(),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
      ))
    }
    next_round_tbl <- rv$gs_boundary_preview %>%
      dplyr::filter(!schedule_key %in% gs_submitted_schedule_keys()) %>%
      dplyr::group_by(hypothesis) %>%
      dplyr::summarise(`Next Round` = min(analysis_round), .groups = "drop")
    display_tbl <- status_tbl %>%
      dplyr::left_join(next_round_tbl, by = c("hypothesis" = "hypothesis")) %>%
      dplyr::transmute(
        Hypothesis = hypothesis,
        `Current Alpha` = format(current_alpha, trim = TRUE, scientific = FALSE),
        Decision = decision,
        `In Graph` = ifelse(in_graph, "Yes", "No"),
        Testable = ifelse(testable, "Yes", "No"),
        `Next Round` = ifelse(is.na(`Next Round`), "", as.character(`Next Round`))
      )
    datatable(
      display_tbl,
      rownames = FALSE,
      class = "compact stripe hover nowrap",
      options = list(dom = "t", pageLength = 12, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
    )
  })
})
