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

for (ui_module in c(
  file.path("R", "ui", "group_sequential_design_tab.R"),
  file.path("R", "ui", "analysis_tab.R")
)) {
  ui_module_path <- file.path(getwd(), ui_module)
  if (file.exists(ui_module_path)) {
    sys.source(ui_module_path, envir = globalenv())
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
        tags$span(class = "design-note", "Use these controls for classic graphical testing. Open Group Sequential Design to plan interim analyses, and Analysis to submit one-sided results by round.")
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
              "Build the graph here. Group Sequential Design adds the study-design tables and boundary schedule. Analysis submits one-sided results one round at a time."
            ),
            tags$div(style = "font-weight: 600; margin-bottom: 8px;", "Output"),
            div(class = "design-output-log", verbatimTextOutput("design_ts_log"))
          )
        )
      )
    )
  ),
  build_group_sequential_design_tab(),
  build_analysis_tab()
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
               edit_node_id = NULL, edit_edge_id = NULL, pending_delete_edge_id = NULL),
    pending_source = NULL,
    ts_object = NULL,
    ts_log = "",
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
    gs_hypothesis_plan = tibble::tibble(
      id = integer(),
      hypothesis = character(),
      planned_analyses = integer(),
      alpha_spending = character(),
      custom_cumulative_alpha = character()
    ),
    gs_analysis_schedule = tibble::tibble(
      schedule_key = character(),
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_id = integer(),
      hypothesis_stage = integer(),
      planned_analyses = integer(),
      information_fraction = numeric(),
      is_final = logical()
    ),
    gs_analysis_history = tibble::tibble(
      submission = integer(),
      schedule_key = character(),
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_stage = integer(),
      alpha_spending = character(),
      runtime_spending_code = character(),
      information_fraction = numeric(),
      current_alpha = numeric(),
      cumulative_alpha_spent = numeric(),
      p_value = numeric(),
      boundary_p = numeric(),
      boundary_z = numeric(),
      decision = character(),
      is_final = logical(),
      max_info = numeric()
    ),
    gs_round_feedback = NULL,
    gs_applied_design_signature = "",
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
    rv$ts_summary <- NULL
    rv$gs_boundary_preview <- empty_gs_boundary_preview()
    rv$gs_stage_history <- empty_gs_stage_history()
    rv$gs_analysis_history <- rv$gs_analysis_history[0, ]
    rv$gs_round_feedback <- NULL
    rv$gs_applied_design_signature <- ""
    update_manual_reject_choices(rv$nodes$hypothesis)
    update_sequential_test_choices(rv$nodes$hypothesis)
    bump_ts_state()
    schedule_graph_refresh()
    invisible(NULL)
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
    if (is.null(eid) || !nrow(rv$edges) || !eid %in% rv$edges$id) {
      return(invisible(NULL))
    }
    ed <- rv$edges %>% dplyr::filter(id == eid) %>% dplyr::slice(1)
    from_hyp <- rv$nodes$hypothesis[match(ed$from[[1]], rv$nodes$id)]
    to_hyp <- rv$nodes$hypothesis[match(ed$to[[1]], rv$nodes$id)]
    edge_label <- if (!is.na(from_hyp) && !is.na(to_hyp)) {
      sprintf("%s \u2192 %s", from_hyp, to_hyp)
    } else {
      sprintf("%s \u2192 %s", ed$from[[1]], ed$to[[1]])
    }
    rv$ctx$pending_delete_edge_id <- eid
    showModal(modalDialog(
      title = sprintf("Delete edge: %s", edge_label),
      sprintf("Delete the edge %s? This will reset the current sequential state.", edge_label),
      easyClose = FALSE,
      footer = tagList(
        actionButton("cancel_delete_edge", "Cancel"),
        actionButton("confirm_delete_edge", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$cancel_delete_edge, {
    rv$ctx$pending_delete_edge_id <- NULL
    removeModal()
  })
  
  observeEvent(input$confirm_delete_edge, {
    eid <- rv$ctx$pending_delete_edge_id
    rv$ctx$pending_delete_edge_id <- NULL
    removeModal()
    if (is.null(eid) || !nrow(rv$edges) || !eid %in% rv$edges$id) {
      return(invisible(NULL))
    }
    rv$edges <- dplyr::filter(rv$edges, id != eid) %>%
      sanitize_edges_tbl()
    reset_group_sequential_state()
    bump_tables()
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
      rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
      TRUE
    }, error = function(e) {
      set_ts_log(paste("Reject error:", e$message))
      FALSE
    })
  }
  
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
    initialize_batch_gs_object(reset_history = TRUE)
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

  for (server_module in c(
    file.path("R", "server", "common_helpers.R"),
    file.path("R", "server", "sequential_settings.R"),
    file.path("R", "server", "sequential_boundaries.R"),
    file.path("R", "server", "sequential_state.R"),
    file.path("R", "server", "sequential_outputs.R"),
    file.path("R", "server", "sequential_events.R")
  )) {
    server_module_path <- file.path(getwd(), server_module)
    if (file.exists(server_module_path)) {
      source(server_module_path, local = TRUE)
    }
  }
  
  ##### ---- IMPORT/EXPORT HANDLERS ---- #####
  # ==== EXPORT HANDLER ====
  output$download_graph <- downloadHandler(
    filename = function() paste0("graph-", Sys.Date(), ".json"),
    content = function(file) {
      strip_vector_names <- function(df) {
        if (is.null(df) || !is.data.frame(df)) {
          return(df)
        }
        for (col in names(df)) {
          if (is.vector(df[[col]])) {
            names(df[[col]]) <- NULL
          }
        }
        df
      }

      export_data <- c(
        list(
          nodes = strip_vector_names(as.data.frame(rv$nodes, stringsAsFactors = FALSE)),
          edges = strip_vector_names(as.data.frame(rv$edges, stringsAsFactors = FALSE))
        ),
        lapply(serialize_group_sequential_export(), strip_vector_names)
      )

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
    dat <- fromJSON(input$upload_graph$datapath, simplifyDataFrame = TRUE)

    coerce_import_df <- function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      if (is.list(x) && !is.data.frame(x)) {
        x <- as.data.frame(x, stringsAsFactors = FALSE)
      }
      if (is.data.frame(x)) {
        x[] <- lapply(x, function(col) {
          if (is.list(col)) {
            unlist(col, recursive = TRUE, use.names = FALSE)
          } else {
            col
          }
        })
      }
      x
    }

    nodes <- coerce_import_df(dat$nodes)
    edges <- coerce_import_df(dat$edges)

    if (!is.null(nodes$id)) nodes$id <- as.integer(nodes$id)
    if (!is.null(edges$id)) edges$id <- as.integer(edges$id)

    rv$nodes <- sanitize_nodes_tbl(nodes)
    rv$edges <- sanitize_edges_tbl(edges)

    load_group_sequential_design_from_import(dat)
    bump_tables()
    update_manual_reject_choices(rv$nodes$hypothesis)
  })
}

shinyApp(ui, server)
