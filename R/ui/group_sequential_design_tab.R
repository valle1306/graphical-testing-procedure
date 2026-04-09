build_group_sequential_design_tab <- function() {
  tabPanel(
    "Group Sequential Design",
    fluidPage(
      tags$head(
        tags$style(
          HTML("
            .gs-card {
              border: 1px solid #dee2e6;
              border-radius: 14px;
              background: #ffffff;
              padding: 16px;
              margin-bottom: 16px;
              box-shadow: 0 8px 24px rgba(15, 23, 42, 0.05);
            }
            .gs-title {
              margin: 0 0 4px 0;
              font-weight: 600;
            }
            .gs-help {
              margin: 0 0 14px 0;
              color: #6c757d;
              font-size: 0.94rem;
            }
            .gs-section-title {
              margin: 0 0 8px 0;
              font-size: 1.05rem;
              font-weight: 600;
              color: #0f172a;
            }
            .gs-table-note,
            .gs-inline-note {
              color: #64748b;
              font-size: 0.88rem;
              margin-bottom: 12px;
            }
            .gs-table-shell {
              overflow-x: auto;
            }
            .gs-table-shell table.dataTable th,
            .gs-table-shell table.dataTable td {
              white-space: nowrap;
              vertical-align: top;
            }
            .gs-input-table {
              width: 100%;
              border-collapse: separate;
              border-spacing: 0;
            }
            .gs-input-table thead th {
              background: #0f4c68;
              color: #ffffff;
              font-weight: 600;
              padding: 10px 12px;
              border: 1px solid #d7e1ea;
            }
            .gs-input-table tbody td {
              padding: 10px 12px;
              border: 1px solid #e2e8f0;
              background: #f8fafc;
              vertical-align: top;
            }
            .gs-input-table .form-group {
              margin-bottom: 0;
            }
            .gs-input-table input,
            .gs-input-table select {
              min-width: 120px;
            }
            .gs-actions {
              display: flex;
              gap: 10px;
              flex-wrap: wrap;
              margin-top: 14px;
            }
            .gs-overview-note {
              padding: 12px 14px;
              border-radius: 12px;
              border: 1px solid #dbeafe;
              background: linear-gradient(180deg, #eff6ff 0%, #f8fbff 100%);
              color: #1e3a8a;
            }
            .gs-muted {
              color: #64748b;
              font-size: 0.88rem;
              padding-top: 8px;
            }
            /* Stepper indicator */
            .gs-stepper {
              display: flex;
              align-items: center;
              justify-content: center;
              gap: 0;
              margin-bottom: 20px;
              padding: 0 16px;
            }
            .gs-step {
              display: flex;
              align-items: center;
              gap: 8px;
              cursor: default;
            }
            .gs-step-circle {
              width: 32px;
              height: 32px;
              border-radius: 50%;
              display: flex;
              align-items: center;
              justify-content: center;
              font-weight: 700;
              font-size: 0.9rem;
              border: 2px solid #cbd5e1;
              background: #ffffff;
              color: #64748b;
              transition: all 0.2s ease;
            }
            .gs-step.active .gs-step-circle {
              border-color: #0f4c68;
              background: #0f4c68;
              color: #ffffff;
            }
            .gs-step.completed .gs-step-circle {
              border-color: #16a34a;
              background: #16a34a;
              color: #ffffff;
            }
            .gs-step-label {
              font-size: 0.88rem;
              font-weight: 500;
              color: #94a3b8;
              transition: color 0.2s ease;
            }
            .gs-step.active .gs-step-label {
              color: #0f172a;
              font-weight: 600;
            }
            .gs-step.completed .gs-step-label {
              color: #16a34a;
            }
            .gs-step-connector {
              flex: 1;
              height: 2px;
              background: #e2e8f0;
              margin: 0 12px;
              min-width: 40px;
              transition: background 0.2s ease;
            }
            .gs-step-connector.done {
              background: #16a34a;
            }
            .gs-wizard-nav {
              display: flex;
              justify-content: space-between;
              align-items: center;
              padding: 12px 0 0;
              margin-top: 8px;
              border-top: 1px solid #e2e8f0;
            }
            @media (max-width: 992px) {
              .gs-input-table,
              .gs-input-table thead,
              .gs-input-table tbody,
              .gs-input-table th,
              .gs-input-table td,
              .gs-input-table tr {
                display: block;
              }
              .gs-input-table thead {
                display: none;
              }
              .gs-input-table tbody td {
                border-top: 0;
              }
              .gs-step-label {
                display: none;
              }
            }
          ")
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h3(class = "gs-title", "Group Sequential Design"),
            tags$p(
              class = "gs-help",
              "Walk through each step to define the group sequential design. You can go back to any previous step before finalizing."
            ),
            div(class = "gs-overview-note", uiOutput("gs_design_context"))
          )
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput("gs_stepper_indicator")
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tabsetPanel(
              id = "gs_wizard_tabs",
              type = "hidden",
              tabPanelBody(
                "step1",
                tags$h4(class = "gs-section-title", "Step 1. Hypotheses and Alpha Spending Functions"),
                tags$p(
                  class = "gs-table-note",
                  "Each row comes from the Design graph. Set the planned looks (K) for that hypothesis, choose the alpha spending function, and enter parameters when needed."
                ),
                uiOutput("gs_hypothesis_plan_ui"),
                div(
                  class = "gs-wizard-nav",
                  actionButton("gs_reset_design_defaults", "Reset Defaults", class = "btn btn-secondary btn-sm"),
                  actionButton("gs_wizard_next_1", "Next: Analysis Timing \u2192", class = "btn btn-primary")
                )
              ),
              tabPanelBody(
                "step2",
                tags$h4(class = "gs-section-title", "Step 2. Planned Analysis Times for Entire Study"),
                tags$p(
                  class = "gs-table-note",
                  "The schedule is generated from the hypothesis plan. Adjust the global analysis round or information fraction when a hypothesis needs a different timing."
                ),
                uiOutput("gs_analysis_schedule_ui"),
                div(
                  class = "gs-wizard-nav",
                  actionButton("gs_wizard_back_2", "\u2190 Back: Hypotheses and Alpha Spending Functions", class = "btn btn-outline-secondary"),
                  actionButton("gs_wizard_next_2", "Next: Boundary Review \u2192", class = "btn btn-primary")
                )
              ),
              tabPanelBody(
                "step3",
                tags$h4(class = "gs-section-title", "Step 3. Boundary Review"),
                tags$p(
                  class = "gs-table-note",
                  "Review the design-time one-sided boundaries. When everything looks correct, finalize the design to proceed to the Analysis tab."
                ),
                uiOutput("gs_boundary_preview_feedback"),
                div(class = "gs-table-shell", DTOutput("gs_boundary_schedule_table")),
                uiOutput("gs_finalize_feedback"),
                div(
                  class = "gs-wizard-nav",
                  actionButton("gs_wizard_back_3", "\u2190 Back: Analysis Timing", class = "btn btn-outline-secondary"),
                  actionButton("gs_finalize_design", "Finalize Design", class = "btn btn-primary")
                )
              )
            )
          )
        )
      )
    )
  )
}
