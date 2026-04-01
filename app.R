library(shiny)
library(visNetwork)
library(shinyjs)
library(dplyr)
library(DT)
library(TrialSimulator)
library(jsonlite)

for (helper_file in c("alpha_spending_function.r", "Haybittle-Peto.r")) {
  helper_path <- file.path(getwd(), helper_file)
  if (file.exists(helper_path)) {
    sys.source(helper_path, envir = globalenv())
  }
}

is_named_vector_jsonlite_warning <- function(message) {
  grepl("keep_vec_names=TRUE", message, fixed = TRUE) ||
    grepl("named vectors will be translated into arrays", message, fixed = TRUE) ||
    grepl("Input to asJSON", message, fixed = TRUE)
}

if (!isTRUE(getOption("gmt.jsonlite_warning_handler_registered"))) {
  options(gmt.jsonlite_warning_handler_registered = TRUE)
}

ui <- navbarPage(
  "Graphical Multiple Testing",
  id = "nav",
  theme = bslib::bs_theme(
    version = 5,
    primary = "#0F766E", secondary = "#D97706",
    base_font   = bslib::font_google("Source Sans 3"),
    heading_font= bslib::font_google("Source Sans 3"),
    "font-size-base" = "0.93rem",
    "line-height-base" = 1.45
  )
  
  ,
  
  # ----------- HOME -----------
  tabPanel(
    "Home",
    fluidPage(
      tags$head(
        tags$style(HTML("
        .feature-card {
          border-radius: 10px;
          padding: 10px 12px;   /* was 20px */
          margin-bottom: 12px;  /* was 20px */
          background-color: #f8f9fa;
          box-shadow: 0 1px 2px rgba(0,0,0,0.08);
        }
        .feature-title {
          font-size: 16px;      /* was 20px */
          margin-bottom: 4px;
        }
        .feature-text {
          font-size: 13px;      /* was 16px */
          line-height: 1.35;
        }
        
        /* Hero header */
        .hero {
          display: flex;
          align-items: center;         /* vertical centering */
          justify-content: center;     /* center on the page */
          gap: 32px;
          margin: 12px 0 8px 0;
        }
        .landing-logo {
          height: 220px;               /* bigger than before (was 180px) */
          max-width: 100%;
          border-radius: 16px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
          object-fit: contain;
          display: block;
        }
        .brand { text-align: left; max-width: 780px; }
        .app-title { margin: 0 0 6px 0; font-weight: 600; }
        .app-subtitle { margin: 0; font-size: 16px; color: #6c757d; }
      
        /* Stack vertically on small screens */
        @media (max-width: 992px) {
          .hero { flex-direction: column; text-align: center; }
          .brand { text-align: center; }
          .landing-logo { height: 160px; }
        }
      "))
      ),
      
      # Hero: logo on left, title/subtitle on right
      div(class = "hero",
          tags$video(
            class = "landing-logo",
            autoplay = NA, loop = NA, muted = NA, playsinline = NA,
            tags$source(src = "logo.mp4", type = "video/mp4"),
            "Your browser does not support the logo video."
          ),
          div(class = "brand",
              tags$h2(class = "app-title", "Graphical Approach for Multiple Testing"),
              tags$p(class = "app-subtitle",
                     "An interactive tool for visualizing and designing multiple testing strategies in clinical trials.")
          )
      ),
      br(),
      # New Introduction Section
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h3("🧬 Introduction"),
                        tags$p("With the rise of innovative trial designs—such as adaptive and platform studies—clinical researchers and statisticians face growing complexity in managing multiple hypotheses while preserving statistical rigor. Traditional fixed-sequence procedures often fall short in these evolving contexts."),
                        tags$p("Graphical Approach for Multiple Testing Procedure is a visual and interactive tool built to meet this challenge. Grounded in the graphical methodology pioneered by Bretz et al., it offers a transparent way to design, test, and dynamically redistribute alpha levels across networks of hypotheses."),
                        tags$p("This app was developed in collaboration with methodologists, researchers, and developers committed to making advanced statistical design accessible, reproducible, and human-centered. While still evolving, it is built on the open-source TrialSimulator R package and maintained by a volunteer team.")
               )
        )
      ),
      # Features Cards - 6 features in 2 rows
      fluidRow(
        column(4,
               div(class = "feature-card fc-1",
                   div(class = "feature-title", "🎯 Interactive Graph Design"),
                   div(class = "feature-text", "Right-click empty canvas to add nodes, drag to position, and visually design your hypothesis network with intuitive controls.")
               )
        ),
        column(4,
               div(class = "feature-card fc-2",
                   div(class = "feature-title", "🔗 Smart Edge Management"),
                   div(class = "feature-text", "Right-click nodes to start edges, click targets to connect, and double-click to edit weights with automatic validation.")
               )
        ),
        column(4,
               div(class = "feature-card fc-3",
                   div(class = "feature-title", "⚡ Real-time Editing"),
                   div(class = "feature-text", "Double-click nodes or edges to edit properties instantly. All changes reflect immediately with live validation.")
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "feature-card fc-4",
                   div(class = "feature-title", "🧮 Alpha Management"),
                   div(class = "feature-text", "Allocate alpha levels with automatic sum validation (≤ 1), supporting dynamic α-spending rules for sequential testing.")
               )
        ),
        column(4,
               div(class = "feature-card fc-5",
                   div(class = "feature-title", "📊 Testing Simulation"),
                   div(class = "feature-text", "Create test objects and simulate hypothesis rejections with real-time graph updates showing rejected hypotheses.")
               )
        ),
        column(4,
               div(class = "feature-card fc-6",
                   div(class = "feature-title", "💾 Data Management"),
                   div(class = "feature-text", "Import/export graphs as JSON files, with live data tables showing nodes and edges for easy collaboration.")
               )
        )
      ),
      # References
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h3("📘 References"),
                        tags$ul(
                          tags$li(
                            tags$span("Bretz F., Maurer W., Brannath W., Posch M. (2009). "),
                            tags$i("A graphical approach to sequentially rejective multiple testing procedures."),
                            " Stat Med 28(4): 586–604. ",
                            tags$a(href = "https://doi.org/10.1002/sim.3495", "https://doi.org/10.1002/sim.3495")
                          ),
                          tags$li(
                            tags$span("TrialSimulator R package: "),
                            tags$a(href = "https://zhangh12.github.io/TrialSimulator/", 
                                   "zhangh12.github.io/TrialSimulator/")
                          )
                        )
               )
        )
      ),
      # Footer
      fluidRow(
        column(12, align = "center",
               tags$hr(),
               tags$p("Developed by Phan Nguyen Huong Le & MengYang Yi, advised by Dr. Han Zhang & Dr. Philip He")
        )
      )
    )
  ),
  
  # ----------- DESIGN TAB (Modern functionality) -----------
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
        tags$span(class = "design-note", "Use these controls for classic graphical testing. Open Sequential only for interim analyses.")
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
              "Build the graph here. The Sequential tab adds staged analyses, boundary preview, and analysis history."
            ),
            tags$div(style = "font-weight: 600; margin-bottom: 8px;", "Output"),
            div(class = "design-output-log", verbatimTextOutput("design_ts_log"))
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Sequential",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .seq-card {
            border: 1px solid #dee2e6;
            border-radius: 14px;
            background: #ffffff;
            padding: 16px;
            margin-bottom: 16px;
            box-shadow: 0 8px 24px rgba(15, 23, 42, 0.05);
          }
          .seq-title {
            margin: 0 0 4px 0;
            font-weight: 600;
          }
          .seq-help {
            margin: 0 0 16px 0;
            color: #6c757d;
            font-size: 0.94rem;
          }
          .seq-grid {
            display: grid;
            gap: 16px;
          }
          .seq-subtle {
            color: #64748b;
            font-size: 0.85rem;
          }
          .seq-actions {
            display: flex;
            gap: 10px;
            flex-wrap: wrap;
            margin-bottom: 16px;
          }
          .seq-topbar {
            display: flex;
            justify-content: space-between;
            gap: 16px;
            align-items: flex-start;
            flex-wrap: wrap;
          }
          .seq-title-block {
            min-width: 0;
            flex: 1 1 320px;
          }
          .seq-quick-grid {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 12px;
            align-items: end;
            margin-bottom: 12px;
          }
          .seq-callout {
            margin: 12px 0 0 0;
            padding: 10px 12px;
            border-radius: 10px;
            border: 1px solid #dbeafe;
            background: #eff6ff;
            color: #1e3a8a;
          }
          .seq-feedback {
            margin-top: 12px;
            padding: 10px 12px;
            border-radius: 10px;
            border: 1px solid transparent;
            font-size: 0.9rem;
          }
          .seq-feedback-success {
            border-color: #bbf7d0;
            background: #f0fdf4;
            color: #166534;
          }
          .seq-feedback-error {
            border-color: #fecaca;
            background: #fef2f2;
            color: #991b1b;
          }
          .seq-advanced-accordion {
            margin-top: 12px;
          }
          .seq-compact-note {
            color: #475569;
            font-size: 0.88rem;
            margin-top: 6px;
          }
          .seq-advanced {
            margin-top: 16px;
            padding-top: 12px;
            border-top: 1px solid #eef2f7;
          }
          .seq-advanced summary {
            cursor: pointer;
            color: #0f766e;
            font-weight: 600;
            margin-bottom: 12px;
          }
          .seq-section {
            margin-top: 12px;
            margin-bottom: 10px;
            font-weight: 600;
          }
          .seq-meta {
            color: #6c757d;
            font-size: 0.84rem;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
          .seq-empty {
            min-height: 420px;
            display: flex;
            align-items: center;
            justify-content: center;
            border: 1px dashed #cbd5e1;
            border-radius: 12px;
            color: #64748b;
            background: linear-gradient(135deg, rgba(240,249,255,0.9), rgba(248,250,252,0.9));
          }
          .seq-tabset .nav-tabs {
            margin-bottom: 16px;
          }
          .seq-main-tabs {
            margin-top: 16px;
          }
          .seq-overview {
            margin-top: 8px;
            margin-bottom: 12px;
          }
          .seq-overview .nav-tabs {
            margin-top: 12px;
            margin-bottom: 12px;
          }
          .seq-table {
            overflow-x: auto;
          }
          .seq-table table.dataTable th,
          .seq-table table.dataTable td {
            white-space: nowrap;
            vertical-align: top;
          }
          .seq-graph {
            border: 1px solid #e2e8f0;
            border-radius: 12px;
            overflow: hidden;
            background: linear-gradient(180deg, #f8fbfd 0%, #ffffff 100%);
          }
          .seq-log {
            min-height: 420px;
            max-height: 560px;
            overflow-y: auto;
            background: #f8fafc;
            border: 1px solid #e2e8f0;
            border-radius: 10px;
            padding: 10px 12px;
            margin-bottom: 14px;
          }
          .seq-log pre {
            margin: 0;
            white-space: pre-wrap;
            word-break: break-word;
            background: transparent;
            border: 0;
            padding: 0;
          }
          .seq-details summary {
            display: flex;
            align-items: center;
            justify-content: space-between;
            cursor: pointer;
            font-weight: 600;
            color: #0f172a;
            list-style: none;
            padding: 12px 16px;
            border-radius: 12px;
            border: 1px solid #bfdbfe;
            background: linear-gradient(180deg, #eff6ff 0%, #f8fbff 100%);
            box-shadow: 0 6px 18px rgba(59, 130, 246, 0.08);
            transition: border-color 0.2s ease, box-shadow 0.2s ease, background 0.2s ease;
          }
          .seq-details summary:hover {
            border-color: #60a5fa;
            box-shadow: 0 8px 22px rgba(59, 130, 246, 0.12);
            background: linear-gradient(180deg, #e0f2fe 0%, #f8fbff 100%);
          }
          .seq-details summary::-webkit-details-marker {
            display: none;
          }
          .seq-details summary::after {
            content: 'Show';
            color: #0f766e;
            font-weight: 500;
            font-size: 0.84rem;
            letter-spacing: 0.01em;
            border: 1px solid #99f6e4;
            background: #f0fdfa;
            border-radius: 999px;
            padding: 4px 10px;
          }
          .seq-details[open] summary::after {
            content: 'Hide';
          }
          .seq-details .seq-log {
            min-height: 220px;
            max-height: 360px;
            margin-top: 12px;
            margin-bottom: 0;
          }
          .seq-overview .seq-log {
            min-height: 200px;
            max-height: 300px;
          }
          .seq-tabset .nav > li > a,
          .seq-tabset .nav > li > button {
            color: #0f766e;
          }
          @media (max-width: 992px) {
            .seq-meta {
              white-space: normal;
            }
            .seq-quick-grid {
              grid-template-columns: 1fr;
            }
          }
        ")),
        tags$script(HTML("
          function adjustVisibleDataTables() {
            if (window.jQuery && $.fn.dataTable) {
              setTimeout(function() {
                $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
              }, 60);
            }
          }
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"], button[data-bs-toggle=\"tab\"]', function() {
            adjustVisibleDataTables();
          });
          Shiny.addCustomMessageHandler('adjust-datatables', function() {
            adjustVisibleDataTables();
          });
        "))
      ),
      fluidRow(
        column(
          12,
          div(
            class = "seq-card",
            div(
              class = "seq-topbar",
              div(
                class = "seq-title-block",
                tags$h3(class = "seq-title", "Sequential Analysis"),
                tags$p(class = "seq-help", "Use the Test tab for the common case. Open Plan only if you need a different sequential rule or timing.")
              ),
              div(
                class = "seq-actions",
                actionButton("reset_gs", "Reset Sequential", class = "btn btn-secondary")
              )
            ),
            tags$details(
              class = "seq-details seq-overview",
              tags$summary("Open graph, status, and activity"),
              div(
                class = "seq-tabset",
                tabsetPanel(
                  id = "seq_overview_panel",
                  selected = "Graph",
                  tabPanel("Graph", div(class = "seq-graph", visNetworkOutput("seq_graph", height = "360px"))),
                  tabPanel("Status", div(class = "seq-table", DTOutput("ts_status_table"))),
                  tabPanel("Activity", div(class = "seq-log", verbatimTextOutput("ts_log")))
                )
              )
            ),
            div(
              class = "seq-tabset seq-main-tabs",
              tabsetPanel(
                id = "seq_workflow",
                selected = "Test",
                tabPanel(
                  "Test",
                  tags$p(class = "seq-subtle", "Use the shared defaults unless you need an exception. The app updates boundaries automatically."),
                  uiOutput("seq_test_context"),
                  uiOutput("gs_stage_inputs"),
                  div(
                    class = "seq-actions",
                    actionButton("run_stage_gs", "Apply Test", class = "btn btn-warning")
                  ),
                  uiOutput("seq_test_feedback")
                ),
                tabPanel(
                  "Plan",
                  tags$div(class = "seq-section", "Quick Setup"),
                  tags$p(class = "seq-subtle", "Set one shared rule and timing plan. Most users can stop here."),
                  div(
                    class = "seq-quick-grid",
                    div(
                      selectInput(
                        "seq_quick_rule",
                        "Shared rule",
                        choices = c(
                          "O'Brien-Fleming" = "OF",
                          "Pocock" = "Pocock",
                          "Haybittle-Peto" = "Haybittle-Peto"
                        ),
                        selected = "OF",
                        width = "100%"
                      )
                    ),
                    div(
                      selectInput(
                        "seq_quick_plan",
                        "Timing template",
                        choices = c(
                          "Final only" = "final_only",
                          "Recommended: one interim (50%, 100%)" = "one_interim",
                          "Two interims (33%, 67%, 100%)" = "two_interims",
                          "Three interims (25%, 50%, 75%, 100%)" = "three_interims"
                        ),
                        selected = "one_interim",
                        width = "100%"
                      )
                    )
                  ),
                  tags$div(class = "seq-section", "Advanced Overrides"),
                  tags$p(class = "seq-subtle", "Pick only the hypotheses that should differ from the shared setup."),
                  bslib::accordion(
                    class = "seq-advanced-accordion",
                    bslib::accordion_panel(
                      title = "Advanced: customize individual hypotheses",
                      uiOutput("gs_settings_ui")
                    )
                  )
                ),
                tabPanel(
                  "Review",
                  tags$p(class = "seq-subtle", "Preview updates automatically. History records each submitted one-sided analysis."),
                  div(
                    class = "seq-tabset",
                    tabsetPanel(
                      id = "seq_panel",
                      selected = "Preview",
                      tabPanel("Preview", div(class = "seq-table", DTOutput("gs_boundary_table"))),
                      tabPanel("History", div(class = "seq-table", DTOutput("gs_history_table")))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  focus_and_select <- function(input_id) {
    shinyjs::runjs(sprintf("
    $('#shiny-modal').one('shown.bs.modal', function(){
      var el = document.getElementById('%s');
      if (el) { el.focus(); el.select(); }
    });", input_id))
  }
  
  format_plain_number <- function(x) {
    format(as.numeric(x), trim = TRUE, scientific = FALSE)
  }
  
  read_scalar_character_input <- function(input_id) {
    value <- input[[input_id]]
    if (is.null(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
      return(NULL)
    }
    as.character(value)
  }
  
  read_scalar_numeric_input <- function(input_id) {
    raw_value <- input[[input_id]]
    if (is.null(raw_value) || length(raw_value) != 1) {
      return(NA_real_)
    }
    value <- suppressWarnings(as.numeric(raw_value))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      return(NA_real_)
    }
    value
  }

  read_scalar_integer_input <- function(input_id, default = NA_integer_) {
    raw_value <- input[[input_id]]
    if (is.null(raw_value) || length(raw_value) < 1) {
      return(default)
    }
    value <- suppressWarnings(as.integer(raw_value[[1]]))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      return(default)
    }
    value
  }

  scalar_text_or_default <- function(value, default = "") {
    if (is.null(value) || !length(value) || is.na(value[[1]])) {
      return(default)
    }
    as.character(value[[1]])
  }

  same_trimmed_text <- function(left, right) {
    identical(trimws(scalar_text_or_default(left)), trimws(scalar_text_or_default(right)))
  }
  
  quiet_jsonlite_warning <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        msg <- conditionMessage(w)
        if (is_named_vector_jsonlite_warning(msg)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  
  with_edge_label <- function(df) {
    if (nrow(df) == 0) return(df)
    key        <- paste(pmin(df$from, df$to), pmax(df$from, df$to), sep = "_")
    dup_counts <- ave(key, key, FUN = length)
    has_pair   <- dup_counts > 1
    smooth_list <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
      if (has_pair[i]) {
        smooth_list[[i]] <- list(enabled = TRUE, type = "curvedCW", roundness = 0.20)
      } else {
        smooth_list[[i]] <- list(enabled = FALSE)
      }
    }
    out <- df
    out$label <- format(df$weight, trim = TRUE, scientific = FALSE)
    out$arrows <- "to"
    out$smooth <- smooth_list
    out$hoverWidth     <- 3
    out$selectionWidth <- 3
    out$font <- lapply(seq_len(nrow(df)), function(i) list(background = "white"))
    out
  }

  sanitize_nodes_tbl <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(tibble::as_tibble(df))
    }
    out <- tibble::as_tibble(df)
    for (col in intersect(c("id", "x", "y", "alpha"), names(out))) {
      out[[col]] <- unname(out[[col]])
    }
    out
  }

  sanitize_edges_tbl <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(tibble::as_tibble(df))
    }
    out <- tibble::as_tibble(df)
    for (col in intersect(c("id", "from", "to", "weight"), names(out))) {
      out[[col]] <- unname(out[[col]])
    }
    out
  }

  set_ts_log <- function(...) {
    log_lines <- unlist(list(...), use.names = FALSE)
    log_lines <- trimws(as.character(log_lines))
    rv$ts_log <- paste(log_lines[nzchar(log_lines)], collapse = "\n")
    invisible(NULL)
  }

  set_seq_test_feedback <- function(text = NULL, type = c("success", "error")) {
    if (is.null(text) || !nzchar(trimws(as.character(text[[1]])))) {
      rv$seq_test_feedback <- NULL
      return(invisible(NULL))
    }
    rv$seq_test_feedback <- list(
      text = as.character(text[[1]]),
      type = match.arg(type)
    )
    invisible(NULL)
  }

  format_hypothesis_list <- function(x) {
    values <- unique(trimws(as.character(x)))
    values <- values[nzchar(values)]
    if (!length(values)) {
      return("none")
    }
    paste(values, collapse = ", ")
  }

  format_alpha_snapshot <- function(allocations = get_current_allocations()) {
    if (is.null(allocations) || !length(allocations)) {
      return("Current alpha: none.")
    }
    values <- vapply(as.numeric(allocations), format_plain_number, character(1))
    paste(
      "Current alpha:",
      paste(sprintf("%s=%s", names(allocations), values), collapse = ", "),
      sep = " "
    )
  }

  build_object_log <- function() {
    c(
      sprintf("Created test object for %d hypotheses.", nrow(rv$nodes)),
      "Sequential boundaries use one-sided alpha and one-sided p-values.",
      format_alpha_snapshot()
    )
  }

  default_info_timing_string <- function(k = 2L) {
    k <- suppressWarnings(as.integer(k[[1]]))
    if (is.na(k) || k < 1L) {
      k <- 1L
    }
    if (k == 1L) {
      return("1")
    }
    paste(vapply(seq_len(k), function(i) format_plain_number(i / k), character(1)), collapse = ", ")
  }

  default_spending_values_string <- function(k = 2L) {
    default_info_timing_string(k)
  }

  quick_plan_defaults <- function(plan_key = "one_interim") {
    switch(
      as.character(plan_key[[1]]),
      final_only = list(planned_analyses = 1L, info_timing = "1"),
      one_interim = list(planned_analyses = 2L, info_timing = "0.5, 1"),
      two_interims = list(planned_analyses = 3L, info_timing = "0.33, 0.67, 1"),
      three_interims = list(planned_analyses = 4L, info_timing = "0.25, 0.5, 0.75, 1"),
      list(planned_analyses = 2L, info_timing = "0.5, 1")
    )
  }

  current_shared_sequential_defaults <- function() {
    shared_rule <- read_scalar_character_input("seq_quick_rule")
    if (is.null(shared_rule)) {
      shared_rule <- "OF"
    }
    shared_plan_key <- read_scalar_character_input("seq_quick_plan")
    if (is.null(shared_plan_key)) {
      shared_plan_key <- "one_interim"
    }
    shared_plan <- quick_plan_defaults(shared_plan_key)
    list(
      alpha_spending = normalize_spending_rule(shared_rule),
      planned_analyses = as.integer(shared_plan$planned_analyses),
      info_timing = shared_plan$info_timing,
      spending_values = ""
    )
  }

  normalize_spending_rule <- function(x) {
    value <- trimws(as.character(x[[1]]))
    dplyr::case_when(
      identical(value, "asOF") ~ "OF",
      identical(value, "asP") ~ "Pocock",
      identical(value, "asUser") ~ "Custom",
      identical(value, "O'Brien-Fleming") ~ "OF",
      identical(value, "Lan-DeMets O'Brien-Fleming") ~ "OF",
      identical(value, "Custom cumulative alpha") ~ "Custom",
      TRUE ~ value
    )
  }

  parse_numeric_sequence <- function(text_value) {
    if (is.null(text_value) || !length(text_value)) {
      return(numeric(0))
    }
    normalized <- gsub("[\r\n\t]", " ", as.character(text_value[[1]]))
    pieces <- unlist(strsplit(normalized, "[,;[:space:]]+"))
    pieces <- pieces[nzchar(pieces)]
    if (!length(pieces)) {
      return(numeric(0))
    }
    suppressWarnings(as.numeric(pieces))
  }

  parse_information_timing <- function(text_value, planned_analyses) {
    planned_analyses <- suppressWarnings(as.integer(planned_analyses[[1]]))
    if (is.na(planned_analyses) || planned_analyses < 1L) {
      return(list(ok = FALSE, message = "Planned analyses must be a positive integer."))
    }
    values <- parse_numeric_sequence(text_value)
    if (length(values) != planned_analyses) {
      return(list(
        ok = FALSE,
        message = sprintf("Enter %d cumulative information values.", planned_analyses)
      ))
    }
    if (any(!is.finite(values)) || any(values <= 0)) {
      return(list(ok = FALSE, message = "Information timing values must be positive numbers."))
    }
    if (any(diff(values) <= 0)) {
      return(list(ok = FALSE, message = "Information timing values must increase from one analysis to the next."))
    }
    if (max(values) > 1 + 1e-8) {
      values <- values / max(values)
    }
    if (abs(tail(values, 1) - 1) > 1e-8) {
      return(list(ok = FALSE, message = "The final cumulative information value must be 1, or the final raw information count."))
    }
    list(ok = TRUE, values = values)
  }

  parse_spending_proportions <- function(text_value, planned_analyses) {
    planned_analyses <- suppressWarnings(as.integer(planned_analyses[[1]]))
    if (is.na(planned_analyses) || planned_analyses < 1L) {
      return(list(ok = FALSE, message = "Planned analyses must be a positive integer."))
    }
    values <- parse_numeric_sequence(text_value)
    if (length(values) != planned_analyses) {
      return(list(
        ok = FALSE,
        message = sprintf("Enter %d cumulative spending proportions.", planned_analyses)
      ))
    }
    if (any(!is.finite(values)) || any(values < 0) || any(values > 1)) {
      return(list(ok = FALSE, message = "Cumulative spending proportions must stay between 0 and 1."))
    }
    if (any(diff(values) < 0)) {
      return(list(ok = FALSE, message = "Cumulative spending proportions must be non-decreasing."))
    }
    if (abs(tail(values, 1) - 1) > 1e-8) {
      return(list(ok = FALSE, message = "The final cumulative spending proportion must be 1."))
    }
    list(ok = TRUE, values = values)
  }

  build_information_correlation <- function(timing) {
    timing <- as.numeric(timing)
    k <- length(timing)
    corr <- matrix(1, nrow = k, ncol = k)
    if (k > 1L) {
      for (i in seq_len(k - 1L)) {
        for (j in seq.int(i + 1L, k)) {
          corr[i, j] <- corr[j, i] <- sqrt(timing[i] / timing[j])
        }
      }
    }
    corr
  }

  compute_cumulative_alpha_from_z <- function(z_values, timing) {
    corr <- build_information_correlation(timing)
    vapply(seq_along(z_values), function(k) {
      if (k == 1L) {
        return(1 - stats::pnorm(z_values[[1]]))
      }
      1 - as.numeric(mvtnorm::pmvnorm(
        lower = rep(-Inf, k),
        upper = z_values[seq_len(k)],
        corr = corr[seq_len(k), seq_len(k), drop = FALSE],
        abseps = 1e-8,
        maxpts = 100000
      ))
    }, numeric(1))
  }

  solve_custom_stage_boundary <- function(previous_z, target_stage_alpha, corr_prefix) {
    stage_index <- length(previous_z) + 1L
    if (!is.finite(target_stage_alpha) || target_stage_alpha <= 0) {
      return(Inf)
    }
    boundary_fn <- function(z_value) {
      as.numeric(mvtnorm::pmvnorm(
        lower = c(rep(-Inf, stage_index - 1L), z_value),
        upper = c(previous_z, Inf),
        corr = corr_prefix,
        abseps = 1e-8,
        maxpts = 100000
      )) - target_stage_alpha
    }
    lower <- -8
    upper <- 8
    f_lower <- boundary_fn(lower)
    f_upper <- boundary_fn(upper)
    while (is.finite(f_lower) && f_lower < 0 && lower > -40) {
      lower <- lower - 4
      f_lower <- boundary_fn(lower)
    }
    while (is.finite(f_upper) && f_upper > 0 && upper < 40) {
      upper <- upper + 4
      f_upper <- boundary_fn(upper)
    }
    if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
      stop("Unable to solve the custom spending boundary. Please check the cumulative spending inputs.")
    }
    stats::uniroot(boundary_fn, interval = c(lower, upper), tol = 1e-8)$root
  }

  solve_custom_boundaries <- function(cumulative_alpha, timing) {
    cumulative_alpha <- as.numeric(cumulative_alpha)
    stage_alpha <- c(cumulative_alpha[[1]], diff(cumulative_alpha))
    z_values <- numeric(length(cumulative_alpha))
    z_values[[1]] <- stats::qnorm(1 - cumulative_alpha[[1]])
    if (length(cumulative_alpha) > 1L) {
      corr <- build_information_correlation(timing)
      for (k in 2:length(cumulative_alpha)) {
        z_values[[k]] <- solve_custom_stage_boundary(
          previous_z = z_values[seq_len(k - 1L)],
          target_stage_alpha = stage_alpha[[k]],
          corr_prefix = corr[seq_len(k), seq_len(k), drop = FALSE]
        )
      }
    }
    tibble::tibble(
      analysis = seq_along(cumulative_alpha),
      timing = timing,
      stage_alpha = stage_alpha,
      cumulative_alpha_spent = cumulative_alpha,
      z_boundary = z_values,
      p_boundary = 1 - stats::pnorm(z_values)
    )
  }

  compute_boundary_schedule <- function(total_alpha, spending_type, timing, spending_values = NULL) {
    total_alpha <- as.numeric(total_alpha[[1]])
    spending_type <- normalize_spending_rule(spending_type)
    timing <- as.numeric(timing)
    if (!is.finite(total_alpha) || total_alpha <= 0) {
      return(tibble::tibble(
        analysis = seq_along(timing),
        timing = timing,
        stage_alpha = NA_real_,
        cumulative_alpha_spent = NA_real_,
        z_boundary = NA_real_,
        p_boundary = NA_real_
      ))
    }
    if (length(timing) == 1L) {
      z_values <- stats::qnorm(1 - total_alpha)
      return(tibble::tibble(
        analysis = 1L,
        timing = timing,
        stage_alpha = total_alpha,
        cumulative_alpha_spent = total_alpha,
        z_boundary = z_values,
        p_boundary = 1 - stats::pnorm(z_values)
      ))
    }
    if (identical(spending_type, "Haybittle-Peto")) {
      hp <- HP(overall.alpha = total_alpha, timing = timing)
      return(tibble::tibble(
        analysis = seq_along(timing),
        timing = timing,
        stage_alpha = as.numeric(hp$alpha),
        cumulative_alpha_spent = as.numeric(hp$cum.alpha),
        z_boundary = as.numeric(hp$z),
        p_boundary = as.numeric(hp$p)
      ))
    }
    if (identical(spending_type, "Custom")) {
      if (is.null(spending_values) || !length(spending_values)) {
        stop("Custom cumulative spending proportions are required for the custom rule.")
      }
      return(solve_custom_boundaries(
        cumulative_alpha = total_alpha * as.numeric(spending_values),
        timing = timing
      ))
    }
    gs_spending_fun <- switch(
      spending_type,
      "OF" = gsDesign::sfLDOF,
      "Pocock" = gsDesign::sfLDPocock,
      stop(sprintf("Unsupported spending rule: %s", spending_type))
    )
    gs_obj <- gsDesign::gsDesign(
      k = length(timing),
      alpha = total_alpha,
      timing = timing,
      sfu = gs_spending_fun,
      test.type = 1
    )
    z_values <- as.numeric(gs_obj$upper$bound)
    cumulative_alpha <- compute_cumulative_alpha_from_z(z_values, timing)
    tibble::tibble(
      analysis = seq_along(timing),
      timing = timing,
      stage_alpha = c(cumulative_alpha[[1]], diff(cumulative_alpha)),
      cumulative_alpha_spent = cumulative_alpha,
      z_boundary = z_values,
      p_boundary = 1 - stats::pnorm(z_values)
    )
  }

  resolve_gs_settings <- function(settings, notify = TRUE) {
    if (!nrow(settings)) {
      if (isTRUE(notify)) {
        showNotification("Create at least one hypothesis before configuring sequential boundaries.", type = "error")
      }
      return(NULL)
    }
    allowed_rules <- c("OF", "Pocock", "Haybittle-Peto", "Custom")
    resolved_rows <- lapply(seq_len(nrow(settings)), function(i) {
      rule <- normalize_spending_rule(settings$alpha_spending[[i]])
      if (!rule %in% allowed_rules) {
        if (isTRUE(notify)) {
          showNotification(sprintf("Unsupported spending rule for %s.", settings$hypothesis[[i]]), type = "error")
        }
        return(NULL)
      }
      planned_analyses <- suppressWarnings(as.integer(settings$planned_analyses[[i]]))
      if (!is.finite(planned_analyses) || planned_analyses < 1L) {
        if (isTRUE(notify)) {
          showNotification(sprintf("Planned analyses for %s must be a positive integer.", settings$hypothesis[[i]]), type = "error")
        }
        return(NULL)
      }
      timing_info <- parse_information_timing(settings$info_timing[[i]], planned_analyses)
      if (!isTRUE(timing_info$ok)) {
        if (isTRUE(notify)) {
          showNotification(sprintf("%s timing error: %s", settings$hypothesis[[i]], timing_info$message), type = "error", duration = 8)
        }
        return(NULL)
      }
      custom_values <- NULL
      if (identical(rule, "Custom")) {
        spend_info <- parse_spending_proportions(settings$spending_values[[i]], planned_analyses)
        if (!isTRUE(spend_info$ok)) {
          if (isTRUE(notify)) {
            showNotification(sprintf("%s custom-spending error: %s", settings$hypothesis[[i]], spend_info$message), type = "error", duration = 8)
          }
          return(NULL)
        }
        custom_values <- spend_info$values
      }
      tibble::tibble(
        id = settings$id[[i]],
        hypothesis = settings$hypothesis[[i]],
        use_override = isTRUE(settings$use_override[[i]]),
        alpha_spending = rule,
        planned_analyses = planned_analyses,
        info_timing = paste(vapply(timing_info$values, format_plain_number, character(1)), collapse = ", "),
        spending_values = if (length(custom_values)) {
          paste(vapply(custom_values, format_plain_number, character(1)), collapse = ", ")
        } else {
          ""
        },
        timing_values = list(timing_info$values),
        spending_proportions = list(custom_values)
      )
    })
    if (any(vapply(resolved_rows, is.null, logical(1)))) {
      return(NULL)
    }
    dplyr::bind_rows(resolved_rows)
  }

  build_boundary_preview <- function(settings = NULL, notify = FALSE) {
    if (is.null(settings)) {
      settings <- collect_gs_settings(persist = FALSE)
    }
    resolved <- resolve_gs_settings(settings, notify = notify)
    if (is.null(resolved) || !nrow(resolved)) {
      return(empty_gs_boundary_preview())
    }
    current_alpha <- get_current_allocations()
    status_tbl <- build_ts_status_table()
    preview_rows <- lapply(seq_len(nrow(resolved)), function(i) {
      hyp <- resolved$hypothesis[[i]]
      alpha_now <- as.numeric(current_alpha[[hyp]])
      status_row <- if (!is.null(status_tbl) && nrow(status_tbl)) {
        status_tbl[match(hyp, status_tbl$hypothesis), , drop = FALSE]
      } else {
        NULL
      }
      current_status <- "Ready"
      if (!is.null(status_row) && nrow(status_row)) {
        if (!isTRUE(status_row$in_graph[[1]])) {
          current_status <- "Rejected"
        } else if (!isTRUE(status_row$testable[[1]]) || !is.finite(alpha_now) || alpha_now <= 0) {
          current_status <- "Not testable"
        }
      } else if (!is.finite(alpha_now) || alpha_now <= 0) {
        current_status <- "Not testable"
      }
      base_rows <- tibble::tibble(
        hypothesis = hyp,
        alpha_spending = resolved$alpha_spending[[i]],
        planned_analyses = resolved$planned_analyses[[i]],
        analysis = seq_along(resolved$timing_values[[i]]),
        timing = resolved$timing_values[[i]],
        current_alpha = alpha_now,
        stage_alpha = NA_real_,
        cumulative_alpha_spent = NA_real_,
        z_boundary = NA_real_,
        p_boundary = NA_real_,
        status = current_status
      )
      if (!identical(current_status, "Ready")) {
        return(base_rows)
      }
      schedule <- tryCatch(
        compute_boundary_schedule(
          total_alpha = alpha_now,
          spending_type = resolved$alpha_spending[[i]],
          timing = resolved$timing_values[[i]],
          spending_values = resolved$spending_proportions[[i]]
        ),
        error = function(e) e
      )
      if (inherits(schedule, "error")) {
        base_rows$status <- paste("Error:", conditionMessage(schedule))
        return(base_rows)
      }
      base_rows$stage_alpha <- schedule$stage_alpha
      base_rows$cumulative_alpha_spent <- schedule$cumulative_alpha_spent
      base_rows$z_boundary <- schedule$z_boundary
      base_rows$p_boundary <- schedule$p_boundary
      base_rows
    })
    dplyr::bind_rows(preview_rows)
  }

  build_preview_log <- function(preview_tbl = rv$gs_boundary_preview) {
    if (is.null(preview_tbl) || !nrow(preview_tbl)) {
      return("No planned boundaries are available yet.")
    }
    c(
      "Refreshed one-sided rejection boundaries for the current alpha allocations.",
      vapply(seq_len(nrow(preview_tbl)), function(i) {
        if (!identical(preview_tbl$status[[i]], "Ready")) {
          return(sprintf(
            "%s analysis %s: %s.",
            preview_tbl$hypothesis[[i]],
            preview_tbl$analysis[[i]],
            preview_tbl$status[[i]]
          ))
        }
        sprintf(
          "%s analysis %s (timing %s): p <= %s, z >= %s.",
          preview_tbl$hypothesis[[i]],
          preview_tbl$analysis[[i]],
          format_plain_number(preview_tbl$timing[[i]]),
          format_plain_number(preview_tbl$p_boundary[[i]]),
          format_plain_number(preview_tbl$z_boundary[[i]])
        )
      }, character(1))
    )
  }

  build_stage_apply_log <- function(stage_row) {
    decision_text <- as.character(stage_row$decision[[1]])
    c(
      sprintf(
        "Tested %s at planned analysis %s (timing %s).",
        stage_row$hypothesis[[1]],
        stage_row$analysis[[1]],
        format_plain_number(stage_row$timing[[1]])
      ),
      sprintf(
        "One-sided p-value=%s, rejection boundary=%s, z boundary=%s.",
        format_plain_number(stage_row$p_value[[1]]),
        format_plain_number(stage_row$boundary_p[[1]]),
        format_plain_number(stage_row$boundary_z[[1]])
      ),
      if (identical(decision_text, "Reject")) {
        sprintf("Decision: reject %s and recycle alpha.", stage_row$hypothesis[[1]])
      } else {
        sprintf("Decision: do not reject %s.", stage_row$hypothesis[[1]])
      },
      "Remaining hypotheses were recalculated using the updated alpha allocations.",
      format_alpha_snapshot()
    )
  }

  build_design_reject_log <- function(selected_hypothesis) {
    c(
      sprintf("Rejected %s in the graphical procedure.", selected_hypothesis),
      format_alpha_snapshot()
    )
  }

  validate_transition_matrix <- function(mat, hypotheses = rownames(mat), tol = 1e-6) {
    if (is.null(mat) || !length(mat)) {
      return(list(valid = TRUE, message = NULL))
    }
    row_sums <- rowSums(mat)
    invalid_rows <- which(!(abs(row_sums) < tol | abs(row_sums - 1) < tol))
    if (!length(invalid_rows)) {
      return(list(valid = TRUE, message = NULL))
    }
    if (is.null(hypotheses) || length(hypotheses) != nrow(mat)) {
      hypotheses <- paste0("Row ", seq_len(nrow(mat)))
    }
    details <- paste(
      vapply(
        invalid_rows,
        function(i) sprintf("%s = %s", hypotheses[i], format_plain_number(row_sums[i])),
        character(1)
      ),
      collapse = "; "
    )
    list(
      valid = FALSE,
      message = paste0(
        "Outgoing edge weights must sum to 0 or 1 for each hypothesis. ",
        "Invalid totals: ", details,
        ". If a hypothesis has outgoing edges, make their weights add to 1."
      )
    )
  }
  
  next_hypothesis <- function(existing) {
    k <- 1L
    while (paste0("H", k) %in% existing) k <- k + 1L
    paste0("H", k)
  }
  
  is_valid_alpha_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0(\\.\\d+)?|1(\\.0+)?)$", s)
  }
  
  is_valid_weight_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0\\.[0-9]+|1(\\.0+)?)$", s)
  }
  
  cancel_pending_js <- "Shiny.setInputValue('cancel_pending', Math.random(), {priority:'event'});"
  
  rv <- reactiveValues(
    nodes = tibble::tibble(
      id    = 1:3,
      x     = c(-150, 0, 150),
      y     = c(-40, 40, -20),
      hypothesis = c("H1", "H2", "H3"),
      alpha      = c(0, 0, 0)
    ),
    edges = tibble::tibble(
      id = integer(), from = integer(), to = integer(), weight = numeric()
    ),
    ctx = list(node = NULL, edge = NULL, canvas = c(0,0), 
               edit_node_id = NULL, edit_edge_id = NULL),
    pending_source = NULL,
    ts_object = NULL,
    ts_log = "",
    seq_test_feedback = NULL,
    seq_skip_next_feedback_clear = FALSE,
    ts_summary = NULL,
    gs_settings = tibble::tibble(
      id = 1:3,
      hypothesis = c("H1", "H2", "H3"),
      use_override = rep(FALSE, 3),
      alpha_spending = rep("OF", 3),
      planned_analyses = rep(2L, 3),
      info_timing = rep("0.5, 1", 3),
      spending_values = rep("", 3)
    ),
    gs_boundary_preview = tibble::tibble(
      hypothesis = character(),
      alpha_spending = character(),
      planned_analyses = integer(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      stage_alpha = numeric(),
      cumulative_alpha_spent = numeric(),
      z_boundary = numeric(),
      p_boundary = numeric(),
      status = character()
    ),
    gs_stage_history = tibble::tibble(
      submission = integer(),
      hypothesis = character(),
      alpha_spending = character(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      p_value = numeric(),
      boundary_p = numeric(),
      boundary_z = numeric(),
      decision = character()
    ),
    alpha_spending = character(0),
    planned_max_info = numeric(0),
    transition = matrix(0, 0, 0)
  )
  
  update_manual_reject_choices <- function(choices = rv$nodes$hypothesis) {
    selected_choice <- if (length(choices)) choices[[1]] else character(0)
    updateSelectInput(session, "design_graph_selected", choices = choices, selected = selected_choice)
  }

  update_sequential_test_choices <- function(choices = NULL) {
    if (is.null(choices)) {
      status_tbl <- build_ts_status_table()
      choices <- if (!is.null(status_tbl) && nrow(status_tbl)) {
        status_tbl$hypothesis[status_tbl$in_graph]
      } else {
        rv$nodes$hypothesis
      }
    }
    selected_choice <- isolate(input$gs_test_hypothesis)
    if (is.null(selected_choice) || !selected_choice %in% choices) {
      selected_choice <- if (length(choices)) choices[[1]] else character(0)
    }
    updateSelectInput(session, "gs_test_hypothesis", choices = choices, selected = selected_choice)
  }
  
  tables_tick <- reactiveVal(0)
  bump_tables <- function() tables_tick(tables_tick() + 1)
  ts_state_tick <- reactiveVal(0)
  bump_ts_state <- function() ts_state_tick(ts_state_tick() + 1)
  
  empty_gs_boundary_preview <- function() {
    tibble::tibble(
      hypothesis = character(),
      alpha_spending = character(),
      planned_analyses = integer(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      stage_alpha = numeric(),
      cumulative_alpha_spent = numeric(),
      z_boundary = numeric(),
      p_boundary = numeric(),
      status = character()
    )
  }
  
  empty_gs_stage_history <- function() {
    tibble::tibble(
      submission = integer(),
      hypothesis = character(),
      alpha_spending = character(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      p_value = numeric(),
      boundary_p = numeric(),
      boundary_z = numeric(),
      decision = character()
    )
  }
  
  collect_gs_settings <- function(persist = FALSE) {
    existing <- rv$gs_settings
    if (!nrow(rv$nodes)) {
      out <- tibble::tibble(
        id = integer(),
        hypothesis = character(),
        use_override = logical(),
        alpha_spending = character(),
        planned_analyses = integer(),
        info_timing = character(),
        spending_values = character()
      )
    } else {
      shared_defaults <- current_shared_sequential_defaults()
      override_hypotheses <- input$gs_override_hypotheses
      if (is.null(override_hypotheses) && !is.null(existing) && nrow(existing) && "use_override" %in% names(existing)) {
        override_hypotheses <- existing$hypothesis[which(as.logical(existing$use_override))]
      }
      override_hypotheses <- unique(trimws(as.character(override_hypotheses)))
      override_hypotheses <- override_hypotheses[nzchar(override_hypotheses)]
      out <- tibble::tibble(
        id = rv$nodes$id,
        hypothesis = rv$nodes$hypothesis,
        use_override = rv$nodes$hypothesis %in% override_hypotheses,
        alpha_spending = vapply(rv$nodes$id, function(id) {
          hyp <- rv$nodes$hypothesis[match(id, rv$nodes$id)]
          if (!hyp %in% override_hypotheses) {
            return(shared_defaults$alpha_spending)
          }
          input_val <- read_scalar_character_input(paste0("gs_asf_", id))
          if (!is.null(input_val)) {
            return(input_val)
          }
          existing_idx <- match(id, existing$id)
          if (!is.null(existing) && nrow(existing) && !is.na(existing_idx)) {
            return(normalize_spending_rule(existing$alpha_spending[existing_idx]))
          }
          "OF"
        }, character(1)),
        planned_analyses = vapply(rv$nodes$id, function(id) {
          hyp <- rv$nodes$hypothesis[match(id, rv$nodes$id)]
          if (!hyp %in% override_hypotheses) {
            return(shared_defaults$planned_analyses)
          }
          input_val <- read_scalar_numeric_input(paste0("gs_k_", id))
          if (!is.na(input_val)) {
            return(as.integer(round(input_val)))
          }
          existing_idx <- match(id, existing$id)
          if (!is.null(existing) && nrow(existing) && !is.na(existing_idx)) {
            return(as.integer(existing$planned_analyses[existing_idx]))
          }
          2L
        }, integer(1)),
        info_timing = vapply(rv$nodes$id, function(id) {
          hyp <- rv$nodes$hypothesis[match(id, rv$nodes$id)]
          if (!hyp %in% override_hypotheses) {
            return(shared_defaults$info_timing)
          }
          input_val <- read_scalar_character_input(paste0("gs_timing_", id))
          if (!is.null(input_val)) {
            return(input_val)
          }
          existing_idx <- match(id, existing$id)
          if (!is.null(existing) && nrow(existing) && !is.na(existing_idx) && "info_timing" %in% names(existing)) {
            return(as.character(existing$info_timing[existing_idx]))
          }
          default_info_timing_string(2L)
        }, character(1)),
        spending_values = vapply(rv$nodes$id, function(id) {
          hyp <- rv$nodes$hypothesis[match(id, rv$nodes$id)]
          if (!hyp %in% override_hypotheses) {
            return(shared_defaults$spending_values)
          }
          input_val <- read_scalar_character_input(paste0("gs_spend_", id))
          if (!is.null(input_val)) {
            return(input_val)
          }
          existing_idx <- match(id, existing$id)
          if (!is.null(existing) && nrow(existing) && !is.na(existing_idx) && "spending_values" %in% names(existing)) {
            return(as.character(existing$spending_values[existing_idx]))
          }
          ""
        }, character(1))
      )
    }
    if (isTRUE(persist)) {
      rv$gs_settings <- out
    }
    out
  }
  
  get_current_allocations <- function() {
    ts_state_tick()
    if (is.null(rv$ts_object)) {
      return(stats::setNames(rv$nodes$alpha, rv$nodes$hypothesis))
    }
    stats::setNames(
      vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch({
          hid <- rv$ts_object$get_hid(hyp)
          if (!rv$ts_object$is_in_graph(hid)) {
            return(0)
          }
          rv$ts_object$get_alpha(hid)
        }, error = function(e) {
          rv$nodes$alpha[match(hyp, rv$nodes$hypothesis)]
        })
      }, numeric(1)),
      rv$nodes$hypothesis
    )
  }
  
  build_ts_status_table <- function() {
    ts_state_tick()
    if (!nrow(rv$nodes)) {
      return(NULL)
    }
    if (is.null(rv$ts_object)) {
      return(tibble::tibble(
        hypothesis = rv$nodes$hypothesis,
        current_alpha = as.numeric(rv$nodes$alpha),
        decision = rep("ready", nrow(rv$nodes)),
        in_graph = rep(TRUE, nrow(rv$nodes)),
        testable = rv$nodes$alpha > 0
      ))
    }
    decisions <- tryCatch(rv$ts_object$get_current_decision(), error = function(e) {
      stats::setNames(rep("accept", nrow(rv$nodes)), rv$nodes$hypothesis)
    })
    allocations <- get_current_allocations()
    status_tbl <- tibble::tibble(
      hypothesis = rv$nodes$hypothesis,
      current_alpha = as.numeric(allocations[rv$nodes$hypothesis]),
      decision = unname(decisions[rv$nodes$hypothesis]),
      in_graph = vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_in_graph(rv$ts_object$get_hid(hyp)), error = function(e) TRUE)
      }, logical(1)),
      testable = vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_testable(rv$ts_object$get_hid(hyp)), error = function(e) FALSE)
      }, logical(1))
    )
    status_tbl$decision[!status_tbl$in_graph] <- "reject"
    status_tbl
  }
  
  build_graph_nodes <- function() {
    allocations <- get_current_allocations()
    status_tbl <- build_ts_status_table()
    result <- sanitize_nodes_tbl(rv$nodes) %>%
      mutate(
        x = as.numeric(x),
        y = as.numeric(y),
        alpha_display = as.numeric(allocations[hypothesis]),
        label = paste0(hypothesis, "\nalpha=", format_plain_number(alpha_display))
      )
    result$color <- "#8ecae6"
    font_colors <- rep("#0f172a", nrow(result))
    result$borderWidth <- 2
    if (!is.null(status_tbl) && nrow(status_tbl)) {
      inactive <- status_tbl$hypothesis[status_tbl$in_graph & !status_tbl$testable]
      rejected <- status_tbl$hypothesis[!status_tbl$in_graph | status_tbl$decision == "reject"]
      result$color[result$hypothesis %in% inactive] <- "#cbd5e1"
      result$color[result$hypothesis %in% rejected] <- "#dc2626"
      font_colors[result$hypothesis %in% rejected] <- "#111827"
    }
    result$font <- lapply(font_colors, function(color) {
      list(color = color, strokeWidth = 3, strokeColor = "#ffffff", vadjust = 0)
    })
    result
  }

  build_graph_edges <- function() {
    base_edges <- sanitize_edges_tbl(rv$edges)
    if (is.null(rv$ts_object) || !nrow(base_edges)) {
      display_edges <- with_edge_label(base_edges)
      if (nrow(display_edges)) {
        display_edges$color <- "#5aa6cf"
      }
      return(display_edges)
    }
    current_edges <- lapply(seq_len(nrow(base_edges)), function(i) {
      from_hyp <- rv$nodes$hypothesis[match(base_edges$from[i], rv$nodes$id)]
      to_hyp <- rv$nodes$hypothesis[match(base_edges$to[i], rv$nodes$id)]
      if (is.na(from_hyp) || is.na(to_hyp)) {
        return(NULL)
      }
      from_hid <- tryCatch(rv$ts_object$get_hid(from_hyp), error = function(e) NA_integer_)
      to_hid <- tryCatch(rv$ts_object$get_hid(to_hyp), error = function(e) NA_integer_)
      if (is.na(from_hid) || is.na(to_hid)) {
        return(NULL)
      }
      weight <- tryCatch(rv$ts_object$get_weight(from_hid, to_hid), error = function(e) base_edges$weight[i])
      tibble::tibble(
        id = base_edges$id[i],
        from = base_edges$from[i],
        to = base_edges$to[i],
        weight = as.numeric(weight)
      )
    })
    current_edges <- dplyr::bind_rows(current_edges) %>%
      sanitize_edges_tbl() %>%
      dplyr::filter(is.finite(weight) & weight > 1e-12)
    display_edges <- with_edge_label(current_edges)
    if (nrow(display_edges)) {
      display_edges$color <- "#5aa6cf"
    }
    display_edges
  }
  
  update_graph_views <- function() {
    nodes_data <- build_graph_nodes()
    edges_data <- build_graph_edges()
    quiet_jsonlite_warning({
      visNetworkProxy("graph") %>%
        visSetData(nodes = nodes_data, edges = edges_data) %>%
        visRedraw()
      visNetworkProxy("seq_graph") %>%
        visSetData(nodes = nodes_data, edges = edges_data) %>%
        visRedraw()
      if (nrow(rv$nodes)) {
        for (i in seq_len(nrow(rv$nodes))) {
          visNetworkProxy("graph") %>% visMoveNode(id = rv$nodes$id[i], x = rv$nodes$x[i], y = rv$nodes$y[i])
          visNetworkProxy("seq_graph") %>% visMoveNode(id = rv$nodes$id[i], x = rv$nodes$x[i], y = rv$nodes$y[i])
        }
      }
    })
    invisible(NULL)
  }

  schedule_graph_refresh <- function(adjust_tables = TRUE) {
    session$onFlushed(function() {
      isolate({
        update_graph_views()
        if (isTRUE(adjust_tables)) {
          session$sendCustomMessage("adjust-datatables", list())
        }
      })
    }, once = TRUE)
    invisible(NULL)
  }
  
  refresh_ts_state <- function() {
    status_tbl <- build_ts_status_table()
    if (is.null(rv$ts_object)) {
      rv$ts_summary <- NULL
      update_manual_reject_choices(rv$nodes$hypothesis)
      update_sequential_test_choices(rv$nodes$hypothesis)
      schedule_graph_refresh()
      return(invisible(NULL))
    }
    latest <- tryCatch(rv$ts_object$get_current_testing_results(), error = function(e) NULL)
    if (is.null(latest) || !nrow(latest)) {
      latest <- status_tbl
    }
    rv$ts_summary <- latest
    active_hyps <- if (!is.null(status_tbl) && nrow(status_tbl)) {
      status_tbl$hypothesis[status_tbl$in_graph & status_tbl$testable]
    } else {
      rv$nodes$hypothesis
    }
    if (!length(active_hyps)) {
      active_hyps <- rv$nodes$hypothesis
    }
    update_manual_reject_choices(active_hyps)
    update_sequential_test_choices(active_hyps)
    schedule_graph_refresh()
    invisible(NULL)
  }
  
  reset_group_sequential_state <- function(reset_log = TRUE) {
    rv$ts_object <- NULL
    if (isTRUE(reset_log)) {
      set_ts_log("")
    }
    set_seq_test_feedback(NULL)
    rv$seq_skip_next_feedback_clear <- FALSE
    rv$ts_summary <- NULL
    rv$gs_boundary_preview <- empty_gs_boundary_preview()
    rv$gs_stage_history <- empty_gs_stage_history()
    update_manual_reject_choices(rv$nodes$hypothesis)
    update_sequential_test_choices(rv$nodes$hypothesis)
    bump_ts_state()
    schedule_graph_refresh()
    invisible(NULL)
  }
  
  create_trialsimulator_objects <- function() {
    settings <- collect_gs_settings(persist = TRUE)
    resolved_settings <- resolve_gs_settings(settings, notify = TRUE)
    if (is.null(resolved_settings)) {
      return(FALSE)
    }
    rv$gs_settings <- resolved_settings %>%
      dplyr::select(id, hypothesis, use_override, alpha_spending, planned_analyses, info_timing, spending_values)
    rv$alpha_spending <- rep("asOF", nrow(rv$nodes))
    rv$planned_max_info <- rep(100L, nrow(rv$nodes))
    n <- nrow(rv$nodes)
    mat <- matrix(0, n, n)
    if (n > 0) {
      rownames(mat) <- colnames(mat) <- rv$nodes$hypothesis
      if (nrow(rv$edges) > 0) {
        for (i in seq_len(nrow(rv$edges))) {
          from_idx <- which(rv$nodes$id == rv$edges$from[i])
          to_idx <- which(rv$nodes$id == rv$edges$to[i])
          if (length(from_idx) == 1 && length(to_idx) == 1) {
            mat[from_idx, to_idx] <- rv$edges$weight[i]
          }
        }
      }
    }
    validation <- validate_transition_matrix(mat, rv$nodes$hypothesis)
    if (!isTRUE(validation$valid)) {
      rv$transition <- mat
      set_ts_log(validation$message)
      set_seq_test_feedback(validation$message, type = "error")
      rv$ts_object <- NULL
      rv$ts_summary <- NULL
      showNotification(validation$message, type = "error", duration = 8)
      return(FALSE)
    }
    rv$transition <- mat
    TRUE
  }

  refresh_boundary_preview <- function(notify = FALSE) {
    settings <- collect_gs_settings(persist = TRUE)
    preview_tbl <- build_boundary_preview(settings, notify = notify)
    rv$gs_boundary_preview <- preview_tbl
    invisible(preview_tbl)
  }
  
  initialize_ts_object <- function(reset_history = FALSE) {
    if (!isTRUE(create_trialsimulator_objects())) {
      rv$ts_object <- NULL
      rv$ts_summary <- NULL
      bump_ts_state()
      schedule_graph_refresh()
      return(FALSE)
    }
    tryCatch({
      rv$ts_object <- NULL
      log_message <- function(m) {
        invokeRestart("muffleMessage")
      }
      withCallingHandlers(
        {
          rv$ts_object <- GraphicalTesting$new(
            alpha = rv$nodes$alpha,
            transition = rv$transition,
            alpha_spending = rv$alpha_spending,
            planned_max_info = rv$planned_max_info,
            hypotheses = rv$nodes$hypothesis,
            silent = FALSE
          )
        },
        message = log_message
      )
      bump_ts_state()
      if (isTRUE(reset_history)) {
        rv$gs_boundary_preview <- empty_gs_boundary_preview()
        rv$gs_stage_history <- empty_gs_stage_history()
      }
      refresh_ts_state()
      refresh_boundary_preview(notify = FALSE)
      set_ts_log(build_object_log())
      TRUE
    }, error = function(e) {
      set_ts_log(paste("Error during TrialSimulator setup:", e$message))
      set_seq_test_feedback(paste("Sequential setup error:", e$message), type = "error")
      rv$ts_object <- NULL
      rv$ts_summary <- NULL
      FALSE
    })
  }
  
  collect_single_stage_test <- function() {
    if (!nrow(rv$nodes)) {
      set_seq_test_feedback("Create at least one hypothesis before entering sequential analyses.", type = "error")
      showNotification("Create at least one hypothesis before entering sequential analyses.", type = "error")
      return(NULL)
    }
    preview_tbl <- refresh_boundary_preview(notify = TRUE)
    if (is.null(preview_tbl) || !nrow(preview_tbl)) {
      if (nzchar(rv$ts_log)) {
        set_seq_test_feedback(rv$ts_log, type = "error")
      }
      return(NULL)
    }
    selected_hypothesis <- read_scalar_character_input("gs_test_hypothesis")
    if (is.null(selected_hypothesis) || !selected_hypothesis %in% rv$nodes$hypothesis) {
      set_seq_test_feedback("Choose a hypothesis to test.", type = "error")
      showNotification("Choose a hypothesis to test.", type = "error")
      return(NULL)
    }
    analysis_number <- read_scalar_integer_input("gs_test_analysis", default = NA_integer_)
    if (is.na(analysis_number) || analysis_number < 1L) {
      set_seq_test_feedback("Planned analysis number must be an integer greater than or equal to 1.", type = "error")
      showNotification("Planned analysis number must be an integer greater than or equal to 1.", type = "error")
      return(NULL)
    }
    p_value <- read_scalar_numeric_input("gs_test_p")
    if (is.na(p_value) || p_value < 0 || p_value > 1) {
      set_seq_test_feedback(sprintf("Enter a one-sided p-value between 0 and 1 for %s.", selected_hypothesis), type = "error")
      showNotification(sprintf("The one-sided p-value for %s must be between 0 and 1.", selected_hypothesis), type = "error")
      return(NULL)
    }
    boundary_row <- preview_tbl %>%
      dplyr::filter(hypothesis == selected_hypothesis, analysis == analysis_number) %>%
      dplyr::slice(1)
    if (!nrow(boundary_row)) {
      max_analysis <- preview_tbl %>%
        dplyr::filter(hypothesis == selected_hypothesis) %>%
        dplyr::summarise(max_analysis = max(analysis, na.rm = TRUE)) %>%
        dplyr::pull(max_analysis)
      set_seq_test_feedback(
        sprintf("%s has %s planned analyses. Choose a value between 1 and %s.", selected_hypothesis, max_analysis, max_analysis),
        type = "error"
      )
      showNotification(
        sprintf("%s has %s planned analyses. Choose a value between 1 and %s.", selected_hypothesis, max_analysis, max_analysis),
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    if (!identical(boundary_row$status[[1]], "Ready")) {
      set_seq_test_feedback(
        sprintf("%s cannot be tested right now: %s.", selected_hypothesis, boundary_row$status[[1]]),
        type = "error"
      )
      showNotification(
        sprintf("%s cannot be tested right now: %s.", selected_hypothesis, boundary_row$status[[1]]),
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    tibble::tibble(
      submission = nrow(rv$gs_stage_history) + 1L,
      hypothesis = selected_hypothesis,
      alpha_spending = boundary_row$alpha_spending[[1]],
      analysis = analysis_number,
      timing = boundary_row$timing[[1]],
      current_alpha = boundary_row$current_alpha[[1]],
      p_value = p_value,
      boundary_p = boundary_row$p_boundary[[1]],
      boundary_z = boundary_row$z_boundary[[1]],
      decision = ifelse(p_value <= boundary_row$p_boundary[[1]] + 1e-12, "Reject", "Do not reject")
    )
  }
  
  output$graph <- renderVisNetwork({
    quiet_jsonlite_warning(
      visNetwork(build_graph_nodes(), build_graph_edges()) %>%
        visNodes(
          font = list(size = 16)
        ) %>%
        visEdges(
          font = list(background = "white"),
          hoverWidth = 3,
          selectionWidth = 3
        ) %>%
        visPhysics(
          enabled = FALSE,
          stabilization = FALSE
        ) %>%
        visOptions(
          highlightNearest = FALSE,
          manipulation = list(enabled = FALSE)
        ) %>%
        visInteraction(
          selectConnectedEdges = FALSE, 
          hoverConnectedEdges = FALSE,
          dragNodes = TRUE,
          dragView = TRUE
        ) %>% 
        visEvents(
          dragEnd = "
          function(params) {
            if (params.nodes && params.nodes.length > 0) {
              var nodeId = params.nodes[0];
              var pos = this.getPositions([nodeId])[nodeId];
              Shiny.setInputValue('node_dragged', {
                id: nodeId,
                x: pos.x,
                y: pos.y
              }, {priority: 'event'});
            }
          }
        ",
        oncontext = "
          function(params) {
            params.event.preventDefault();
            Shiny.setInputValue('any_context', { }, {priority: 'event'});
        
            var pointer = params.pointer;
            var nodeId = this.getNodeAt(pointer.DOM);
            var edgeId = null;
            if (!nodeId) {
              edgeId = this.getEdgeAt(pointer.DOM);
            }
            
            var showNode = !!nodeId;
            var showEdge = !showNode && !!edgeId;
            var showBlank = !showNode && !showEdge; 
        
            Shiny.setInputValue('ctx_event', {
              node: nodeId || null,
              edge: edgeId || null,
              canvas: pointer.canvas
            }, {priority: 'event'});
        
            var menu = document.getElementById('ctx-menu');
            menu.style.left = (params.event.clientX) + 'px';
            menu.style.top  = (params.event.clientY) + 'px';
            menu.style.display = 'block';
        
            document.getElementById('ctx_add_node').style.display   = showBlank ? 'block' : 'none';
            document.getElementById('ctx_del_node').style.display   = showNode  ? 'block' : 'none';
            document.getElementById('ctx_edge_start').style.display = showNode  ? 'block' : 'none';
            document.getElementById('ctx_del_edge').style.display   = showEdge ? 'block' : 'none';
          }
        ",
        doubleClick = "
          function(params) {
            var eid = (params.edges && params.edges.length) ? params.edges[0] : null;
            if(eid !== null) {
              Shiny.setInputValue('dbl_edge', eid, {priority: 'event'});
              return;
            }
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            if(nid !== null) {
              Shiny.setInputValue('dbl_node', nid, {priority: 'event'});
            }
          }
        ",
        click = "
          function(params) {
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            var eid = (params.edges && params.edges.length) ? params.edges[0] : null;
            Shiny.setInputValue('click_event', {node: nid, edge: eid}, {priority: 'event'});
          }
        "
        )
    )
  })
  
  output$seq_graph <- renderVisNetwork({
    quiet_jsonlite_warning(
      visNetwork(build_graph_nodes(), build_graph_edges()) %>%
        visNodes(font = list(size = 16)) %>%
        visEdges(
          font = list(background = "white"),
          hoverWidth = 3,
          selectionWidth = 3
        ) %>%
        visPhysics(enabled = FALSE, stabilization = FALSE) %>%
        visInteraction(
          selectConnectedEdges = FALSE,
          hoverConnectedEdges = FALSE,
          dragNodes = FALSE,
          dragView = TRUE,
          zoomView = TRUE
        )
    )
  })
  
  output$graph_ui <- renderUI({
    visNetworkOutput("graph", height = "640px")
  })
  
  output$ts_status_table <- renderDT({
    quiet_jsonlite_warning({
      df <- build_ts_status_table()
      if (is.null(df) || !nrow(df)) {
        return(datatable(
          data.frame(
            hypothesis = character(),
            current_alpha = numeric(),
            decision = character(),
            testable = logical(),
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
        ))
      }
      display_df <- df %>%
        transmute(
          Hypothesis = hypothesis,
          `Current alpha` = format(current_alpha, trim = TRUE, scientific = FALSE),
          Decision = decision,
          Testable = ifelse(testable, "Yes", "No"),
          Active = ifelse(in_graph, "Yes", "No")
        )
      datatable(
        display_df,
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, autoWidth = FALSE, scrollX = TRUE)
      )
    })
  })
  
  outputOptions(output, "graph", suspendWhenHidden = FALSE)
  outputOptions(output, "seq_graph", suspendWhenHidden = FALSE)
  
  panel_visible <- reactiveVal(TRUE)
  
  observe({
    rv$nodes
    update_manual_reject_choices(rv$nodes$hypothesis)
    update_sequential_test_choices(rv$nodes$hypothesis)
  })

  observeEvent(input$toggle_panel, {
    if (isTRUE(panel_visible())) {
      shinyjs::hide("left-col")
      runjs("
      var rc = document.getElementById('right-col');
      if (rc) { rc.classList.remove('col-sm-9'); rc.classList.add('col-sm-12'); }
    ")
      updateActionButton(session, "toggle_panel", label = "Show data panel")
      panel_visible(FALSE)
    } else {
      shinyjs::show("left-col")
      runjs("
      var rc = document.getElementById('right-col');
      if (rc) { rc.classList.remove('col-sm-12'); rc.classList.add('col-sm-9'); }
    ")
      updateActionButton(session, "toggle_panel", label = "Hide data panel")
      panel_visible(TRUE)
    }
  })
  
  output$nodes_table <- renderDT({
    tables_tick()
    isolate(quiet_jsonlite_warning({
      if (!nrow(rv$nodes)) {
        return(datatable(data.frame(hypothesis=character(), alpha=numeric()),
                         rownames=FALSE,
                         options=list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE, info=FALSE)
        ))
      }
      df <- rv$nodes |> dplyr::transmute(hypothesis, alpha = as.numeric(alpha))
      df$alpha <- vapply(df$alpha, function(x) format(x, trim=TRUE, scientific=FALSE), character(1))
      datatable(df, rownames=FALSE,
                options=list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE, info=FALSE)
      )
    }))
  })
  
  output$edges_table <- renderDT({
    tables_tick()
    isolate(quiet_jsonlite_warning({
      if (!nrow(rv$edges)) {
        return(datatable(data.frame(from=character(), to=character(), weight=numeric()),
                         rownames=FALSE,
                         options=list(dom='ft', paging=FALSE, searching=FALSE, ordering=FALSE, info=FALSE)
        ))
      }
      from_h <- rv$nodes$hypothesis[match(rv$edges$from, rv$nodes$id)]
      to_h   <- rv$nodes$hypothesis[match(rv$edges$to,   rv$nodes$id)]
      df <- data.frame(from=from_h, to=to_h, weight=rv$edges$weight, stringsAsFactors=FALSE)
      df$weight <- vapply(df$weight, function(x) format(x, trim=TRUE, scientific=FALSE), character(1))
      datatable(df, rownames=FALSE,
                options=list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE, info=FALSE)
      )
    }))
  })
  
  output$gs_settings_ui <- renderUI({
    if (!nrow(rv$nodes)) {
      return(tags$p("Add at least one hypothesis to configure sequential boundary settings."))
    }
    settings <- rv$gs_settings
    if (!nrow(settings) ||
        nrow(settings) != nrow(rv$nodes) ||
        !identical(settings$id, rv$nodes$id) ||
        !identical(settings$hypothesis, rv$nodes$hypothesis)) {
      settings <- collect_gs_settings(persist = TRUE)
    }
    shared_defaults <- current_shared_sequential_defaults()
    override_hypotheses <- settings$hypothesis[settings$use_override]
    tagList(
      selectizeInput(
        "gs_override_hypotheses",
        "Override only these hypotheses",
        choices = settings$hypothesis,
        selected = override_hypotheses,
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = "Choose only exceptions",
          plugins = list("remove_button")
        )
      ),
      if (!length(override_hypotheses)) {
        tags$div(
          class = "seq-compact-note",
          sprintf(
            "No overrides selected. All hypotheses currently use %s with %s analysis%s at timing %s.",
            shared_defaults$alpha_spending,
            shared_defaults$planned_analyses,
            ifelse(shared_defaults$planned_analyses == 1L, "", "es"),
            shared_defaults$info_timing
          )
        )
      } else {
        tagList(
          tags$div(class = "seq-compact-note", "Remove a hypothesis here anytime to return it to the shared setup."),
          fluidRow(
            column(2, tags$b("Hypothesis")),
            column(3, tags$b("Rule")),
            column(2, tags$b("Analyses")),
            column(3, tags$b("Timing")),
            column(2, tags$b("Custom spend"))
          ),
          lapply(override_hypotheses, function(hyp) {
            i <- match(hyp, settings$hypothesis)
            id <- settings$id[i]
            selected_rule <- read_scalar_character_input(paste0("gs_asf_", id))
            if (is.null(selected_rule)) {
              selected_rule <- settings$alpha_spending[i]
            }
            fluidRow(
              style = "margin-bottom: 8px;",
              column(2, tags$div(style = "padding-top: 8px;", tags$strong(hyp))),
              column(
                3,
                selectInput(
                  inputId = paste0("gs_asf_", id),
                  label = NULL,
                  choices = c(
                    "O'Brien-Fleming" = "OF",
                    "Pocock" = "Pocock",
                    "Haybittle-Peto" = "Haybittle-Peto",
                    "Custom cumulative alpha" = "Custom"
                  ),
                  selected = settings$alpha_spending[i],
                  width = "100%"
                )
              ),
              column(
                2,
                numericInput(
                  inputId = paste0("gs_k_", id),
                  label = NULL,
                  value = settings$planned_analyses[i],
                  min = 1,
                  step = 1,
                  width = "100%"
                )
              ),
              column(
                3,
                textInput(
                  inputId = paste0("gs_timing_", id),
                  label = NULL,
                  value = settings$info_timing[i],
                  placeholder = "e.g. 0.5, 1"
                )
              ),
              column(
                2,
                if (identical(normalize_spending_rule(selected_rule), "Custom")) {
                  textInput(
                    inputId = paste0("gs_spend_", id),
                    label = NULL,
                    value = settings$spending_values[i],
                    placeholder = "e.g. 0.4, 1"
                  )
                } else {
                  tags$div(class = "seq-meta", style = "padding-top: 8px;", "Auto from rule")
                }
              )
            )
          })
        )
      }
    )
  })

  output$seq_test_context <- renderUI({
    if (!nrow(rv$nodes)) {
      return(NULL)
    }
    settings <- collect_gs_settings(persist = FALSE)
    shared_defaults <- current_shared_sequential_defaults()
    override_count <- sum(as.logical(settings$use_override), na.rm = TRUE)
    message <- if (!override_count) {
      sprintf(
        "Current default: %s with %s analysis%s at timing %s for all hypotheses.",
        shared_defaults$alpha_spending,
        shared_defaults$planned_analyses,
        ifelse(shared_defaults$planned_analyses == 1L, "", "es"),
        shared_defaults$info_timing
      )
    } else {
      sprintf(
        "Current default: %s with %s analysis%s at timing %s, plus %s override%s in Plan.",
        shared_defaults$alpha_spending,
        shared_defaults$planned_analyses,
        ifelse(shared_defaults$planned_analyses == 1L, "", "es"),
        shared_defaults$info_timing,
        override_count,
        ifelse(override_count == 1L, "", "s")
      )
    }
    tags$div(class = "seq-compact-note", message)
  })

  output$seq_test_feedback <- renderUI({
    feedback <- rv$seq_test_feedback
    if (is.null(feedback) || !length(feedback$text) || !nzchar(feedback$text[[1]])) {
      return(NULL)
    }
    feedback_class <- if (identical(feedback$type, "error")) {
      "seq-feedback seq-feedback-error"
    } else {
      "seq-feedback seq-feedback-success"
    }
    tags$div(class = feedback_class, feedback$text[[1]])
  })
  
  output$gs_stage_inputs <- renderUI({
    if (!nrow(rv$nodes)) {
      return(tags$p("Create hypotheses in the graph first."))
    }
    settings <- collect_gs_settings(persist = FALSE)
    status_tbl <- build_ts_status_table()
    selectable_hypotheses <- if (!is.null(status_tbl) && nrow(status_tbl)) {
      status_tbl$hypothesis[status_tbl$in_graph]
    } else {
      settings$hypothesis
    }
    if (!length(selectable_hypotheses)) {
      selectable_hypotheses <- settings$hypothesis
    }
    selected_hypothesis <- isolate(input$gs_test_hypothesis)
    if (is.null(selected_hypothesis) || !selected_hypothesis %in% selectable_hypotheses) {
      selected_hypothesis <- selectable_hypotheses[[1]]
    }
    selected_analysis <- read_scalar_integer_input("gs_test_analysis", default = 1L)
    selected_p <- isolate(input$gs_test_p)
    preview_tbl <- tryCatch({
      if (nrow(rv$gs_boundary_preview)) {
        rv$gs_boundary_preview
      } else {
        build_boundary_preview(settings = settings, notify = FALSE)
      }
    }, error = function(e) {
      empty <- empty_gs_boundary_preview()
      if (length(selectable_hypotheses)) {
        empty <- tibble::tibble(
          hypothesis = selected_hypothesis,
          alpha_spending = character(),
          planned_analyses = integer(),
          analysis = integer(),
          timing = numeric(),
          current_alpha = numeric(),
          stage_alpha = numeric(),
          cumulative_alpha_spent = numeric(),
          z_boundary = numeric(),
          p_boundary = numeric(),
          status = character()
        )
      }
      empty
    })
    analysis_choices <- preview_tbl %>%
      dplyr::filter(hypothesis == selected_hypothesis) %>%
      dplyr::pull(analysis) %>%
      unique()
    if (!length(analysis_choices)) {
      planned_lookup <- settings$planned_analyses[match(selected_hypothesis, settings$hypothesis)]
      planned_analyses <- if (length(planned_lookup) && !is.na(planned_lookup[[1]])) {
        as.integer(planned_lookup[[1]])
      } else {
        1L
      }
      analysis_choices <- seq_len(max(1L, planned_analyses))
    }
    if (!selected_analysis %in% analysis_choices) {
      selected_analysis <- analysis_choices[[1]]
    }
    summary_row <- preview_tbl %>%
      dplyr::filter(hypothesis == selected_hypothesis, analysis == selected_analysis) %>%
      dplyr::slice(1)
    summary_text <- NULL
    callout <- NULL
    if (nrow(summary_row)) {
      if (identical(summary_row$status[[1]], "Ready")) {
        summary_text <- sprintf(
          "Current alpha %s | timing %s | one-sided rejection boundary p <= %s (z >= %s).",
          format_plain_number(summary_row$current_alpha[[1]]),
          format_plain_number(summary_row$timing[[1]]),
          format_plain_number(summary_row$p_boundary[[1]]),
          format_plain_number(summary_row$z_boundary[[1]])
        )
      } else {
        summary_text <- sprintf(
          "Current alpha %s | status: %s.",
          format_plain_number(summary_row$current_alpha[[1]]),
          summary_row$status[[1]]
        )
      }
    }
    if (!nrow(preview_tbl) || !any(preview_tbl$status == "Ready", na.rm = TRUE)) {
      callout <- tags$div(
        class = "seq-callout",
        tags$strong("Nothing is testable yet. "),
        "Allocate positive alpha in the Design tab. Boundaries will appear here automatically."
      )
    }
    tagList(
      callout,
      fluidRow(
        column(
          4,
          selectInput(
            "gs_test_hypothesis",
            "Hypothesis",
            choices = selectable_hypotheses,
            selected = selected_hypothesis,
            width = "100%"
          )
        ),
        column(
          4,
          selectInput(
            "gs_test_analysis",
            "Analysis",
            choices = as.character(analysis_choices),
            selected = as.character(selected_analysis),
            width = "100%"
          )
        ),
        column(
          4,
          numericInput(
            "gs_test_p",
            "One-sided p-value",
            value = selected_p,
            min = 0,
            max = 1,
            step = 0.0001,
            width = "100%"
          )
        )
      ),
      if (!is.null(summary_text)) {
        tags$div(class = "seq-meta", summary_text)
      }
    )
  })

  observeEvent(list(input$gs_test_hypothesis, input$gs_test_analysis, input$gs_test_p), {
    if (isTRUE(rv$seq_skip_next_feedback_clear)) {
      rv$seq_skip_next_feedback_clear <- FALSE
      return(invisible(NULL))
    }
    set_seq_test_feedback(NULL)
  }, ignoreInit = TRUE)

  observe({
    if (!nrow(rv$nodes)) {
      return()
    }
    live_settings <- collect_gs_settings(persist = FALSE)
    old_settings <- rv$gs_settings
    same_rows <- nrow(old_settings) == nrow(live_settings) &&
      identical(old_settings$id, live_settings$id) &&
      identical(old_settings$hypothesis, live_settings$hypothesis)
    if (!same_rows) {
      rv$gs_settings <- live_settings
      return()
    }
    settings_changed <- !identical(as.logical(old_settings$use_override), as.logical(live_settings$use_override)) ||
      !identical(old_settings$alpha_spending, live_settings$alpha_spending) ||
      !identical(as.integer(old_settings$planned_analyses), as.integer(live_settings$planned_analyses)) ||
      !identical(as.character(old_settings$info_timing), as.character(live_settings$info_timing)) ||
      !identical(as.character(old_settings$spending_values), as.character(live_settings$spending_values))
    if (!settings_changed) {
      return()
    }
    for (i in seq_len(nrow(live_settings))) {
      if (!isTRUE(live_settings$use_override[i])) {
        next
      }
      old_rule <- normalize_spending_rule(old_settings$alpha_spending[i])
      new_rule <- normalize_spending_rule(live_settings$alpha_spending[i])
      old_default_timing <- default_info_timing_string(old_settings$planned_analyses[i])
      new_default_timing <- default_info_timing_string(live_settings$planned_analyses[i])
      current_timing <- read_scalar_character_input(paste0("gs_timing_", live_settings$id[i]))
      if ((!is.null(current_timing) && same_trimmed_text(current_timing, old_default_timing)) ||
          (is.null(current_timing) && same_trimmed_text(old_settings$info_timing[i], old_default_timing))) {
        updateTextInput(session, paste0("gs_timing_", live_settings$id[i]), value = new_default_timing)
      }
      old_default_spend <- if (identical(old_rule, "Custom")) {
        default_spending_values_string(old_settings$planned_analyses[i])
      } else {
        ""
      }
      new_default_spend <- if (identical(new_rule, "Custom")) {
        default_spending_values_string(live_settings$planned_analyses[i])
      } else {
        ""
      }
      current_spend <- read_scalar_character_input(paste0("gs_spend_", live_settings$id[i]))
      old_saved_spend <- scalar_text_or_default(old_settings$spending_values[i])
      if (identical(new_rule, "Custom") &&
          ((!is.null(current_spend) && (!nzchar(scalar_text_or_default(current_spend)) || same_trimmed_text(current_spend, old_default_spend))) ||
            (is.null(current_spend) && (!nzchar(old_saved_spend) || same_trimmed_text(old_saved_spend, old_default_spend))))) {
        updateTextInput(session, paste0("gs_spend_", live_settings$id[i]), value = new_default_spend)
      } else if (!identical(new_rule, "Custom") && nzchar(scalar_text_or_default(current_spend, old_saved_spend))) {
        updateTextInput(session, paste0("gs_spend_", live_settings$id[i]), value = "")
      }
    }
    rv$gs_settings <- live_settings
  })

  observe({
    if (!nrow(rv$nodes)) {
      rv$gs_boundary_preview <- empty_gs_boundary_preview()
      return()
    }
    ts_state_tick()
    settings <- collect_gs_settings(persist = FALSE)
    rv$gs_boundary_preview <- build_boundary_preview(settings = settings, notify = FALSE)
  })
  
  output$gs_boundary_table <- renderDT({
    quiet_jsonlite_warning({
      df <- rv$gs_boundary_preview
      if (is.null(df) || !nrow(df)) {
        return(datatable(
          data.frame(
            Hypothesis = character(),
            Rule = character(),
            Analysis = integer(),
            Timing = numeric(),
            Alpha = numeric(),
            `Boundary p` = numeric(),
            `Boundary z` = numeric(),
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
        ))
      }
      display_df <- df %>%
        transmute(
          Hypothesis = hypothesis,
          Rule = alpha_spending,
          Analysis = analysis,
          Timing = format(timing, trim = TRUE, scientific = FALSE),
          Alpha = format(current_alpha, trim = TRUE, scientific = FALSE),
          `Stage alpha` = ifelse(is.na(stage_alpha), "", format(stage_alpha, trim = TRUE, scientific = FALSE)),
          `Cumulative alpha` = ifelse(is.na(cumulative_alpha_spent), "", format(cumulative_alpha_spent, trim = TRUE, scientific = FALSE)),
          `Boundary p` = ifelse(is.na(p_boundary), "", format(p_boundary, trim = TRUE, scientific = FALSE)),
          `Boundary z` = ifelse(is.na(z_boundary), "", format(z_boundary, trim = TRUE, scientific = FALSE)),
          Status = status
        )
      datatable(
        display_df,
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        options = list(dom = 't', pageLength = 8, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
      )
    })
  })
  
  output$gs_history_table <- renderDT({
    quiet_jsonlite_warning({
      df <- rv$gs_stage_history
      if (is.null(df) || !nrow(df)) {
        return(datatable(
          data.frame(
            Submission = integer(),
            Hypothesis = character(),
            Rule = character(),
            Analysis = integer(),
            P = numeric(),
            `Boundary p` = numeric(),
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
        ))
      }
      display_df <- df %>%
        transmute(
          Submission = submission,
          Hypothesis = hypothesis,
          Rule = alpha_spending,
          Analysis = analysis,
          Timing = format(timing, trim = TRUE, scientific = FALSE),
          `Current alpha` = format(current_alpha, trim = TRUE, scientific = FALSE),
          P = format(p_value, trim = TRUE, scientific = FALSE),
          `Boundary p` = format(boundary_p, trim = TRUE, scientific = FALSE),
          `Boundary z` = format(boundary_z, trim = TRUE, scientific = FALSE),
          Decision = decision
        )
      datatable(
        display_df,
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        options = list(dom = 't', pageLength = 8, lengthChange = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE, autoWidth = FALSE)
      )
    })
  })
  
  observeEvent(input$node_dragged, {
    node_id <- input$node_dragged$id
    rv$nodes <- rv$nodes %>%
      mutate(
        x = ifelse(id == node_id, input$node_dragged$x, x),
        y = ifelse(id == node_id, input$node_dragged$y, y)
      ) %>%
      sanitize_nodes_tbl()
    quiet_jsonlite_warning({
      visNetworkProxy("seq_graph") %>% visMoveNode(id = node_id, x = input$node_dragged$x, y = input$node_dragged$y)
    })
  })
  
  observe({
    runjs("
      document.addEventListener('click', function(){
        var m = document.getElementById('ctx-menu');
        if (m) m.style.display='none';
      });
      document.addEventListener('keydown', function(e){
        if (e.key === 'Escape') {
          Shiny.setInputValue('cancel_pending', Math.random(), {priority:'event'});
        }
      });
    ")
  })
  
  observeEvent(input$ctx_event, {
    canvas_coords <- suppressWarnings(as.numeric(unlist(input$ctx_event$canvas)))
    if (length(canvas_coords) < 2 || any(!is.finite(canvas_coords[1:2]))) {
      canvas_coords <- c(0, 0)
    }
    rv$ctx$node   <- input$ctx_event$node
    rv$ctx$edge   <- input$ctx_event$edge
    rv$ctx$canvas <- unname(canvas_coords[1:2])
  })
  
  observeEvent(input$any_context, {
    if (!is.null(rv$pending_source)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
    }
  }, ignoreInit = TRUE)
  
  # ---------- Node: add / delete / edit ----------
  observeEvent(input$ctx_add_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- ifelse(nrow(rv$nodes)==0, 1, max(rv$nodes$id)+1)
    default_h <- next_hypothesis(rv$nodes$hypothesis)
    default_a <- "0"
    base_row <- tibble::tibble(
      id = nid,
      x  = as.numeric(rv$ctx$canvas[1]),
      y  = as.numeric(rv$ctx$canvas[2]),
      hypothesis = default_h,
      alpha      = as.numeric(default_a)
    )
    rv$nodes <- dplyr::bind_rows(rv$nodes, base_row) %>%
      sanitize_nodes_tbl()
    collect_gs_settings(persist = TRUE)
    reset_group_sequential_state()
    bump_tables()
    update_manual_reject_choices(rv$nodes$hypothesis)
    
    # Immediately open the edit modal for the new node (Dr. Zhang's intended UX)
    # Set edit target so the existing save logic can be reused
    rv$ctx$edit_node_id <- nid
    showModal(modalDialog(
      title = paste("Edit node", nid),
      textInput("edit_node_hypo",  "Hypothesis (unique, case-sensitive)", value = default_h, placeholder = "e.g., H1"),
      textInput("edit_node_alpha", "Alpha (0–1, no scientific notation)", value = default_a),
      easyClose = FALSE,
      footer = tagList(
        actionButton("cancel_new_node", "Cancel"),
        actionButton("save_node_edit", "Save", class = "btn-primary")
      )
    ))
    focus_and_select("edit_node_alpha")
  })
  
  observeEvent(input$ctx_del_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- rv$ctx$node
    if (!is.null(nid)) {
      rv$edges <- dplyr::filter(rv$edges, !(from == nid | to == nid))
      rv$nodes <- dplyr::filter(rv$nodes, id != nid) %>%
        sanitize_nodes_tbl()
      collect_gs_settings(persist = TRUE)
      reset_group_sequential_state()
      bump_tables()
      update_manual_reject_choices(rv$nodes$hypothesis)
    }
  })
  
  observeEvent(input$dbl_node, {
    nid <- input$dbl_node
    nd <- rv$nodes %>% dplyr::filter(id == nid) %>% dplyr::slice(1)
    rv$ctx$edit_node_id <- nid
    showModal(modalDialog(
      title = paste("Edit node", nid),
      textInput("edit_node_hypo",  "Hypothesis (unique, case-sensitive)", value = nd$hypothesis, placeholder = "e.g., H1"),
      textInput("edit_node_alpha", "Alpha (0–1, no scientific notation)", value = format(nd$alpha, trim = TRUE, scientific = FALSE)),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_node_edit", "Save", class = "btn-primary")
      )
    ))
    focus_and_select("edit_node_alpha")
  })
  
  observeEvent(input$save_node_edit, {
    id <- rv$ctx$edit_node_id
    if (is.null(id)) return(invisible(NULL))
    h_new <- input$edit_node_hypo
    a_str <- input$edit_node_alpha
    existing <- rv$nodes$hypothesis[rv$nodes$id != id]
    if (is.null(h_new) || !nzchar(h_new)) {
      showNotification("Hypothesis cannot be empty.", type = "error")
      return(invisible(NULL))
    }
    if (h_new %in% existing) {
      showNotification(sprintf("Hypothesis '%s' already exists. Please choose a unique value.", h_new), type = "error")
      return(invisible(NULL))
    }
    if (!is_valid_alpha_str(a_str)) {
      showNotification("Alpha must be a plain decimal within [0, 1], no scientific notation.", type = "error")
      return(invisible(NULL))
    }
    a_val <- as.numeric(a_str)
    others_sum <- sum(rv$nodes$alpha[rv$nodes$id != id], na.rm = TRUE)
    if (others_sum + a_val > 1 + 1e-12) {
      msg <- sprintf("Total alpha would be %.6f (> 1). Please reduce this node's alpha.", others_sum + a_val)
      showNotification(msg, type = "error")
      return(invisible(NULL))
    }
    current_x <- rv$nodes$x[rv$nodes$id == id]
    current_y <- rv$nodes$y[rv$nodes$id == id]
    rv$nodes <- rv$nodes %>%
      mutate(
        hypothesis = ifelse(id == !!id, h_new, hypothesis),
        alpha      = ifelse(id == !!id, a_val, alpha)
      ) %>%
      sanitize_nodes_tbl()
    collect_gs_settings(persist = TRUE)
    reset_group_sequential_state()
    rv$ctx$edit_node_id <- NULL
    removeModal()
    bump_tables()
    update_manual_reject_choices(rv$nodes$hypothesis)
  })
  
  # If the user cancels the initial edit on a newly-created node, remove it
  observeEvent(input$cancel_new_node, {
    id_to_remove <- rv$ctx$edit_node_id
    if (!is.null(id_to_remove)) {
      rv$nodes <- rv$nodes[rv$nodes$id != id_to_remove, ] %>%
        sanitize_nodes_tbl()
      collect_gs_settings(persist = TRUE)
      reset_group_sequential_state()
      bump_tables()
      update_manual_reject_choices(rv$nodes$hypothesis)
    }
    rv$ctx$edit_node_id <- NULL
    removeModal()
  })
  
  # ---------- Create edge ----------
  observeEvent(input$ctx_edge_start, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    src <- rv$ctx$node
    if (is.null(src)) return(invisible(NULL))
    rv$pending_source <- src
    visNetworkProxy("graph") %>% visSelectNodes(id = src)
    showNotification(sprintf("Select a target node for edge from %s", src),
                     type = "message", duration = 2)
  })
  
  observeEvent(input$cancel_pending, {
    if (!is.null(rv$pending_source)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Canceled.", type = "default", duration = 1.5)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$click_event, {
    if (is.null(rv$pending_source)) return(invisible(NULL))
    nid <- input$click_event$node
    if (is.null(nid)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Canceled.", type = "default", duration = 1.5)
      return(invisible(NULL))
    }
    src <- rv$pending_source
    tgt <- nid
    if (tgt == src) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Self-loop is not allowed.", type = "error")
      return(invisible(NULL))
    }
    exists_ab <- any(rv$edges$from == src & rv$edges$to == tgt)
    if (exists_ab) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification(sprintf("Edge %s → %s already exists.", src, tgt), type = "error")
      return(invisible(NULL))
    }
    rv$edge_new <- list(from = src, to = tgt)
    showModal(modalDialog(
      title = sprintf("New edge: %s \u2192 %s", src, tgt),
      textInput("new_edge_weight", "Weight (0–1, no scientific notation)", value = "1"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_edge", "Save", class = "btn-primary")
      )
    ))
    focus_and_select("new_edge_weight")
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_new_edge, {
    req(rv$edge_new)
    w_str <- input$new_edge_weight
    if (!is_valid_weight_str(w_str)) {
      showNotification("Weight must be a plain decimal in (0, 1], no scientific notation.", type = "error")
      return(invisible(NULL))
    }
    w_val <- as.numeric(w_str)
    from <- rv$edge_new$from
    to   <- rv$edge_new$to
    eid <- ifelse(nrow(rv$edges) == 0, 1L, max(rv$edges$id) + 1L)
    rv$edges <- bind_rows(
      rv$edges,
      tibble::tibble(id = eid, from = from, to = to, weight = w_val)
    ) %>%
      sanitize_edges_tbl()
    reset_group_sequential_state()
    removeModal()
    rv$edge_new <- NULL
    rv$pending_source <- NULL
    quiet_jsonlite_warning({
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
    })
    bump_tables()
  })
  
  # Delete edge
  observeEvent(input$ctx_del_edge, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    eid <- rv$ctx$edge
    if (!is.null(eid) && nrow(rv$edges) > 0 && eid %in% rv$edges$id) {
      rv$edges <- dplyr::filter(rv$edges, id != eid) %>%
        sanitize_edges_tbl()
      reset_group_sequential_state()
      bump_tables()
    }
  })
  
  # Edit edge
  observeEvent(input$dbl_edge, {
    if (!is.null(rv$pending_source)) return(invisible(NULL))
    eid <- input$dbl_edge
    if (is.null(eid) || !nrow(rv$edges)) return(invisible(NULL))
    ed <- rv$edges %>% dplyr::filter(id == eid) %>% dplyr::slice(1)
    if (!nrow(ed)) return(invisible(NULL))
    rv$ctx$edit_edge_id <- eid
    showModal(modalDialog(
      title = sprintf("Edit edge: %s \u2192 %s", ed$from, ed$to),
      textInput("edit_edge_weight", "Weight (0–1, no scientific notation)",
                value = format(ed$weight, trim = TRUE, scientific = FALSE)),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edge_edit", "Save", class = "btn-primary")
      )
    ))
    focus_and_select("edit_edge_weight")
  })
  
  observeEvent(input$save_edge_edit, {
    eid <- rv$ctx$edit_edge_id
    if (is.null(eid)) return(invisible(NULL))
    w_str <- input$edit_edge_weight
    if (!is_valid_weight_str(w_str)) {
      showNotification("Weight must be a plain decimal in (0, 1], no scientific notation.", type = "error")
      return(invisible(NULL))
    }
    w_val <- as.numeric(w_str)
    rv$edges <- rv$edges %>%
      mutate(weight = ifelse(id == !!eid, w_val, weight)) %>%
      sanitize_edges_tbl()
    reset_group_sequential_state()
    rv$ctx$edit_edge_id <- NULL
    removeModal()
    bump_tables()
  })
  
  reject_hypothesis_in_graph <- function(selected_hypothesis) {
    req(rv$ts_object, selected_hypothesis)
    tryCatch({
      log_message <- function(m) { invokeRestart("muffleMessage") }
      withCallingHandlers(
        rv$ts_object$reject_a_hypothesis(selected_hypothesis),
        message = log_message
      )
      bump_ts_state()
      refresh_ts_state()
      refresh_boundary_preview(notify = FALSE)
      TRUE
    }, error = function(e) {
      set_ts_log(paste("Reject error:", e$message))
      FALSE
    })
  }
  
  observeEvent(input$run_stage_gs, {
    req(nrow(rv$nodes) > 0)
    set_seq_test_feedback(NULL)
    if (is.null(rv$ts_object) && !isTRUE(initialize_ts_object(reset_history = TRUE))) {
      return(invisible(NULL))
    }
    stage_row <- collect_single_stage_test()
    if (is.null(stage_row)) return(invisible(NULL))
    tryCatch({
      if (identical(stage_row$decision[[1]], "Reject")) {
        rejected_ok <- reject_hypothesis_in_graph(stage_row$hypothesis[[1]])
        if (!isTRUE(rejected_ok)) {
          return(invisible(NULL))
        }
      } else {
        refresh_boundary_preview(notify = FALSE)
      }
      rv$gs_stage_history <- dplyr::bind_rows(rv$gs_stage_history, stage_row)
      set_ts_log(build_stage_apply_log(stage_row))
      set_seq_test_feedback(
        sprintf(
          "Saved %s analysis %s. Decision: %s. Review updates automatically in the Review tab.",
          stage_row$hypothesis[[1]],
          stage_row$analysis[[1]],
          tolower(stage_row$decision[[1]])
        ),
        type = "success"
      )
      rv$seq_skip_next_feedback_clear <- TRUE
      updateNumericInput(session, "gs_test_p", value = NA_real_)
      session$sendCustomMessage("adjust-datatables", list())
    }, error = function(e) {
      set_ts_log(paste("Stage analysis error:", e$message))
      set_seq_test_feedback(paste("Stage analysis error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$reset_gs, {
    reset_group_sequential_state()
    collect_gs_settings(persist = TRUE)
    updateTabsetPanel(session, "seq_workflow", selected = "Test")
    updateTabsetPanel(session, "seq_panel", selected = "Preview")
    showNotification("Analysis reset.", type = "message")
  })
  
  manual_reject_hypothesis <- function(selected_hypothesis) {
    rejected_ok <- reject_hypothesis_in_graph(selected_hypothesis)
    if (isTRUE(rejected_ok)) {
      set_ts_log(build_design_reject_log(selected_hypothesis))
    }
  }
  
  observeEvent(input$design_reject_ts, {
    manual_reject_hypothesis(input$design_graph_selected)
  })
  
  observeEvent(input$design_run_ts, {
    req(nrow(rv$nodes) > 0)
    initialize_ts_object(reset_history = TRUE)
  })
  
  observeEvent(input$design_clear_results, {
    reset_group_sequential_state()
  })
  
  output$ts_result_table <- renderDT({
    req(rv$ts_summary)
    quiet_jsonlite_warning({
      df <- rv$ts_summary
      if (all(c("hypothesis", "obs_p_value", "max_allocated_alpha", "decision", "stages", "order", "typeOfDesign") %in% names(df))) {
        df <- df %>%
          transmute(
            Hypothesis = hypothesis,
            `Observed p` = format(obs_p_value, trim = TRUE, scientific = FALSE),
            `Allocated alpha` = format(max_allocated_alpha, trim = TRUE, scientific = FALSE),
            Decision = decision,
            Stage = stages,
            Order = order,
            Spending = typeOfDesign
          )
      }
      datatable(
        df,
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE, autoWidth = FALSE)
      )
    })
  })

  output$design_ts_log <- renderText({
    rv$ts_log
  })
  
  output$ts_log <- renderText({
    rv$ts_log
  })
  
  ##### ---- IMPORT/EXPORT HANDLERS ---- #####
  # ==== EXPORT HANDLER ====
  output$download_graph <- downloadHandler(
    filename = function() paste0("graph-", Sys.Date(), ".json"),
    content = function(file) {
      settings_out <- collect_gs_settings(persist = TRUE)
      # Convert to data frames and ensure no named vectors
      nodes_out <- as.data.frame(rv$nodes, stringsAsFactors = FALSE)
      edges_out <- as.data.frame(rv$edges, stringsAsFactors = FALSE)
      settings_out <- as.data.frame(settings_out, stringsAsFactors = FALSE)
      
      # Remove names from all columns to prevent jsonlite warnings
      for (col in names(nodes_out)) {
        if (is.vector(nodes_out[[col]])) {
          names(nodes_out[[col]]) <- NULL
        }
      }
      for (col in names(edges_out)) {
        if (is.vector(edges_out[[col]])) {
          names(edges_out[[col]]) <- NULL
        }
      }
      for (col in names(settings_out)) {
        if (is.vector(settings_out[[col]])) {
          names(settings_out[[col]]) <- NULL
        }
      }
      
      # Create the export data
      export_data <- list(
        nodes = nodes_out,
        edges = edges_out,
        gs_settings = settings_out
      )
      
      # Use write_json with explicit parameters to avoid warnings
      write_json(
        export_data, 
        file, 
        pretty = TRUE, 
        auto_unbox = TRUE,
        keep_vec_names = FALSE  # This prevents the warning
      )
    }
  )
  
  # ==== IMPORT HANDLER ====
  observeEvent(input$upload_graph, {
    req(input$upload_graph)
    dat <- fromJSON(input$upload_graph$datapath, simplifyDataFrame=TRUE)
    
    # Defensive: if data are weird or come as lists:
    nodes <- dat$nodes
    edges <- dat$edges
    gs_settings <- dat$gs_settings
    
    # If nodes/edges is a list: convert to data.frame
    if (is.list(nodes) && !is.data.frame(nodes)) nodes <- as.data.frame(nodes, stringsAsFactors=FALSE)
    if (is.list(edges) && !is.data.frame(edges)) edges <- as.data.frame(edges, stringsAsFactors=FALSE)
    
    # If imported columns are lists (not atomic): unlist
    list_to_vector <- function(x) if(is.list(x)) unlist(x) else x
    nodes[] <- lapply(nodes, list_to_vector)
    edges[] <- lapply(edges, list_to_vector)
    
    # Ensure correct types:
    if(!is.null(nodes$id)) nodes$id <- as.integer(nodes$id)
    if(!is.null(edges$id)) edges$id <- as.integer(edges$id)
    
    rv$nodes <- sanitize_nodes_tbl(nodes)
    rv$edges <- sanitize_edges_tbl(edges)
    if (!is.null(gs_settings)) {
      if (is.list(gs_settings) && !is.data.frame(gs_settings)) {
        gs_settings <- as.data.frame(gs_settings, stringsAsFactors = FALSE)
      }
      gs_settings[] <- lapply(gs_settings, list_to_vector)
      if (!is.null(gs_settings$id)) gs_settings$id <- as.integer(gs_settings$id)
      if (is.null(gs_settings$alpha_spending)) {
        gs_settings$alpha_spending <- rep("OF", nrow(gs_settings))
      }
      if (is.null(gs_settings$use_override)) {
        gs_settings$use_override <- rep(TRUE, nrow(gs_settings))
      }
      gs_settings$use_override <- as.logical(gs_settings$use_override)
      gs_settings$alpha_spending <- vapply(gs_settings$alpha_spending, normalize_spending_rule, character(1))
      if (!is.null(gs_settings$planned_max_info) && is.null(gs_settings$planned_analyses)) {
        gs_settings$planned_analyses <- rep(2L, nrow(gs_settings))
      }
      if (is.null(gs_settings$planned_analyses)) {
        gs_settings$planned_analyses <- rep(2L, nrow(gs_settings))
      }
      gs_settings$planned_analyses <- as.integer(gs_settings$planned_analyses)
      if (is.null(gs_settings$info_timing)) {
        gs_settings$info_timing <- vapply(gs_settings$planned_analyses, default_info_timing_string, character(1))
      }
      if (is.null(gs_settings$spending_values)) {
        gs_settings$spending_values <- ifelse(
          gs_settings$alpha_spending == "Custom",
          vapply(gs_settings$planned_analyses, default_spending_values_string, character(1)),
          ""
        )
      }
      rv$gs_settings <- tibble::as_tibble(gs_settings) %>%
        dplyr::select(id, hypothesis, use_override, alpha_spending, planned_analyses, info_timing, spending_values)
    } else {
      rv$gs_settings <- tibble::tibble(
        id = rv$nodes$id,
        hypothesis = rv$nodes$hypothesis,
        use_override = rep(FALSE, nrow(rv$nodes)),
        alpha_spending = rep("OF", nrow(rv$nodes)),
        planned_analyses = rep(2L, nrow(rv$nodes)),
        info_timing = rep("0.5, 1", nrow(rv$nodes)),
        spending_values = rep("", nrow(rv$nodes))
      )
    }
    collect_gs_settings(persist = TRUE)
    bump_tables()
    
    update_manual_reject_choices(rv$nodes$hypothesis)
    reset_group_sequential_state()
  })
}

shinyApp(ui, server)
