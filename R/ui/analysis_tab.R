build_analysis_tab <- function() {
  tabPanel(
    "Analysis",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            div(
              style = "display:flex; justify-content:space-between; gap:16px; flex-wrap:wrap; align-items:flex-start;",
              div(
                style = "min-width:0; flex:1 1 320px;",
                tags$h3(class = "gs-title", "Analysis"),
                tags$p(
                  class = "gs-help",
                  "Submit one full analysis round at a time. Enter one-sided p-values for every hypothesis scheduled at that round and let the app update the graph and alpha recycling together."
                )
              ),
              div(
                class = "gs-actions",
                actionButton("gs_reset_analysis_state", "Reset Analysis State", class = "btn btn-secondary")
              )
            ),
            tags$details(
              class = "gs-card",
              style = "margin-top: 12px; margin-bottom: 0;",
              tags$summary(style = "cursor:pointer; font-weight:600; color:#0f766e;", "Open live graph, status, and activity"),
              div(
                style = "margin-top: 12px;",
                tabsetPanel(
                  id = "gs_analysis_overview_panel",
                  selected = "Graph",
                  tabPanel("Graph", div(class = "gs-table-shell", visNetworkOutput("seq_graph", height = "360px"))),
                  tabPanel("Status", div(class = "gs-table-shell", DTOutput("ts_status_table"))),
                  tabPanel("Activity", div(style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:10px; padding:10px 12px;", verbatimTextOutput("ts_log")))
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Submit One Analysis Round"),
            tags$p(
              class = "gs-table-note",
              "Choose the global analysis round, then enter one-sided p-values for every active hypothesis scheduled at that look."
            ),
            selectInput("gs_analysis_round", "Analysis Round", choices = character(0), width = "100%"),
            uiOutput("gs_round_entry_ui"),
            div(
              class = "gs-actions",
              actionButton("gs_submit_round", "Submit Analysis Round", class = "btn btn-warning")
            ),
            uiOutput("gs_round_feedback")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Current Results"),
            div(class = "gs-table-shell", DTOutput("ts_result_table"))
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Submitted Analyses"),
            div(class = "gs-table-shell", DTOutput("gs_submitted_analyses_table"))
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "gs-card",
            tags$h4(class = "gs-section-title", "Current Status"),
            div(class = "gs-table-shell", DTOutput("gs_analysis_status_table"))
          )
        )
      )
    )
  )
}
