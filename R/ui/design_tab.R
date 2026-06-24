build_design_tab <- function() {
  tabPanel(
    "Design",
    fluidPage(
      useShinyjs(),
      tags$head(tags$style(HTML("
    #ctx-menu {
      position: fixed; z-index: 10000; display: none;
      background: #fff; border: 1px solid #ddd; border-radius: 6px;
      box-shadow: 0 6px 18px rgba(0,0,0,.15); min-width: 180px;
    }
    #ctx-menu .ctx-item {
      width: 100%; border: 0; background: transparent; text-align: left;
      padding: 8px 12px; cursor: pointer;
    }
    #ctx-menu .ctx-item:hover { background: #f5f5f5; }
    .design-toolbar {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      align-items: end;
      padding: 14px 16px;
      margin-bottom: 14px;
      border: 1px solid #e5e7eb;
      border-radius: 14px;
      background: #ffffff;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.05);
    }
    .design-toolbar .form-group {
      margin-bottom: 0;
      min-width: 220px;
    }
    .design-note {
      color: #64748b;
      font-size: 0.9rem;
      padding-bottom: 8px;
    }
    .design-output-card {
      border: 1px solid #e5e7eb;
      border-radius: 14px;
      background: #ffffff;
      padding: 14px 16px;
      box-shadow: 0 8px 24px rgba(15, 23, 42, 0.05);
    }
    .design-output-log {
      min-height: 150px;
      max-height: 260px;
      overflow-y: auto;
      background: #f8fafc;
      border: 1px solid #e2e8f0;
      border-radius: 10px;
      padding: 10px 12px;
    }
    .design-output-log pre {
      margin: 0;
      white-space: pre-wrap;
      word-break: break-word;
      background: transparent;
      border: 0;
      padding: 0;
    }
  "))),
      titlePanel("Graph Design"),
      
      # Toggle button (small link above the row)
      div(style = "margin-bottom: 8px;",
          actionLink("toggle_panel", label = "Hide data panel")
      ),
      div(
        class = "design-toolbar",
        actionButton("design_run_ts", "Create Object", class = "btn btn-warning"),
        selectInput("design_graph_selected", "Hypothesis", choices = NULL, width = "220px"),
        actionButton("design_reject_ts", "Reject Selected", class = "btn btn-outline-warning"),
        actionButton("design_clear_results", "Clear Results", class = "btn btn-info"),
        actionButton("design_auto_layout", "Auto Layout", class = "btn btn-outline-secondary"),
        tags$span(class = "design-note", "Use these controls for classic graphical testing. Open Group Sequential Design to plan interim analyses, and Analysis to submit one-sided results by analysis time.")
      ),
      
      fluidRow(
        # ----- Left column: data panel (read-only tables + import/export) -----
        column(
          width = 3, id = "left-col", 
          h4("Nodes"),
          DTOutput("nodes_table"),
          tags$hr(),
          h4("Edges"),
          DTOutput("edges_table"),
          tags$hr(),
          fileInput("upload_graph", "Import JSON graph"),
          downloadButton("download_graph", "Export graph")
        ),
        # ----- Right column: graph canvas -----
        column(
          width = 9, id = "right-col", 
          uiOutput("graph_ui"),
          tags$div(
            id = "ctx-menu",
            actionButton("ctx_add_node",   "Add node here",        class = "ctx-item"),
            actionButton("ctx_del_node",   "Delete this node",     class = "ctx-item"),
            actionButton("ctx_edge_start", "Start edge from here", class = "ctx-item"),
            actionButton("ctx_del_edge",   "Delete this edge",     class = "ctx-item")
          )
        )
      ),
      fluidRow(
        column(
          12,
          tags$hr(),
          div(
            class = "design-output-card",
            tags$p(
              style = "color:#6c757d; margin-bottom: 12px;",
              "Build the graph here. Group Sequential Design adds the study-design tables and boundary schedule. Analysis submits one-sided results one analysis time at a time."
            ),
            tags$div(style = "font-weight: 600; margin-bottom: 8px;", "Output"),
            div(class = "design-output-log", verbatimTextOutput("design_ts_log"))
          )
        )
      )
    )
  )
}
