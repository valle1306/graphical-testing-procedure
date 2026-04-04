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
              "Define per-hypothesis group sequential plans first, then review the generated global analysis schedule and one-sided boundary schedule before moving to Analysis."
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
            tags$h4(class = "gs-section-title", "Step 1. Hypothesis Setup"),
            tags$p(
              class = "gs-table-note",
              "Each row comes from the Design graph. Set the total planned analyses for that hypothesis, choose the alpha spending function, and enter custom cumulative alpha only when needed."
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
            tags$h4(class = "gs-section-title", "Step 2. Analysis Schedule"),
            tags$p(
              class = "gs-table-note",
              "The schedule is generated from the hypothesis setup. Adjust the global analysis round or information fraction when a hypothesis needs a different study design."
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
            tags$h4(class = "gs-section-title", "Boundary Schedule"),
            tags$p(
              class = "gs-table-note",
              "This review table is read-only. It shows the current one-sided boundary schedule implied by the graph, the design tables, and any alpha already recycled in the live graph."
            ),
            div(class = "gs-table-shell", DTOutput("gs_boundary_schedule_table"))
          )
        )
      )
    )
  )
}
