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
              "Define the study design here. Set how many interim looks each hypothesis receives, choose alpha spending functions, review the derived boundary schedule, then finalize the design before moving to the Analysis tab."
            ),
            div(class = "gs-overview-note", uiOutput("gs_design_context"))
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Step 1. Hypothesis Plan"),
            tags$p(
              class = "gs-table-note",
              "Each row comes from the Design graph. Set the planned looks (K) for that hypothesis, choose the alpha spending function, and enter cumulative alpha by look only when using a custom spending rule."
            ),
            uiOutput("gs_hypothesis_plan_ui"),
            div(
              class = "gs-actions",
              actionButton("gs_reset_design_defaults", "Reset Design Defaults", class = "btn btn-secondary")
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Step 2. Analysis Timing"),
            tags$p(
              class = "gs-table-note",
              "The schedule is generated from the hypothesis plan above. Adjust the global analysis round or information fraction when a hypothesis needs a different timing."
            ),
            uiOutput("gs_analysis_schedule_ui")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Step 3. Boundary Review (read-only)"),
            tags$p(
              class = "gs-table-note",
              "This table is read-only. It shows the design-time one-sided boundaries derived from the hypothesis plan, analysis timing, and current alpha allocations in the graph."
            ),
            div(class = "gs-table-shell", DTOutput("gs_boundary_schedule_table")),
            div(
              class = "gs-actions",
              actionButton("gs_finalize_design", "Finalize Design", class = "btn btn-primary"),
              uiOutput("gs_finalize_feedback")
            )
          )
        )
      )
    )
  )
}
