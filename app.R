library(shiny)
library(visNetwork)
library(shinyjs)
library(dplyr)
library(DT)
library(TrialSimulator)
library(jsonlite)

ui <- navbarPage(
  "My Application",
  id = "nav",
  theme = bslib::bs_theme(
  version = 5,
  primary = "#6F56F3", secondary = "#06B6D4",
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
            poster = "graphical_testing_image.png",
            tags$source(src = "logo.mp4", type = "video/mp4"),
            # Fallback image if video can't play:
            tags$img(src = "graphical_testing_image.png", height = "220px")
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
  "))),
      titlePanel("Directed Graph Designer"),
      
      # Toggle button (small link above the row)
      div(style = "margin-bottom: 8px;",
          actionLink("toggle_panel", label = "Hide data panel")
      ),
      
      # Controls for TrialSimulator
      div(style = "margin-bottom: 10px;",
          actionButton("run_ts", "Create Test Object", class = "btn btn-warning"),
          actionButton("reject_ts", "Reject Selected Hypothesis", class = "btn btn-warning"),
          actionButton("edit_mode", "Edit Mode", class = "btn btn-info")
      ),
      selectInput("graph_selected", "Select hypothesis to reject", choices = NULL),
      
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
      # ------------Test Results Section-------------
      fluidRow(
        column(12,
               tags$hr(),
               h4(tags$b("Test Results"), style = "color:purple"),
               verbatimTextOutput("ts_log"),
               dataTableOutput("ts_result_table")
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
  
  with_node_label <- function(df) {
    result <- df %>%
      mutate(label = paste0(hypothesis, "\n",
                            format(alpha, trim = TRUE, scientific = FALSE)))
    # Add default styling
    result$color <- "lightblue"
    result$font.color <- "black"
    if (!is.null(rv$ts_summary)) {
      if ("rejected" %in% names(rv$ts_summary)) {
        rejected_hyps <- rv$ts_summary$hypothesis[rv$ts_summary$rejected == TRUE]
        result$color <- ifelse(result$hypothesis %in% rejected_hyps, "red", "lightblue")
        result$font.color <- ifelse(result$hypothesis %in% rejected_hyps, "white", "black")
      }
    }
    result
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
    ts_summary = NULL,
    alpha_spending = character(0),
    planned_max_info = numeric(0),
    transition = matrix(0, 0, 0)
  )
  
  tables_tick <- reactiveVal(0)
  bump_tables <- function() tables_tick(tables_tick() + 1)
  
  create_trialsimulator_objects <- function() {
    rv$alpha_spending <- rep("asOF", nrow(rv$nodes))
    rv$planned_max_info <- rep(100, nrow(rv$nodes))
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
    rv$transition <- mat
    invisible(NULL)
  }
  
  output$graph <- renderVisNetwork({
    nodes_data <- with_node_label(rv$nodes)
    visNetwork(nodes_data, with_edge_label(rv$edges)) %>%
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
  })
  
  output$graph_ui <- renderUI({
    if (is.null(rv$ts_object)) {
      visNetworkOutput("graph", height = "640px")
    } else {
      plotOutput("ts_plot", height = "640px")
    }
  })
  
  output$ts_plot <- renderPlot({
    req(rv$ts_object)
    print(rv$ts_object)
  })
  
  outputOptions(output, "graph", suspendWhenHidden = FALSE)
  
  panel_visible <- reactiveVal(TRUE)
  
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
    isolate({
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
    })
  })
  
  output$edges_table <- renderDT({
    tables_tick()
    isolate({
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
    })
  })
  
  observeEvent(input$node_dragged, {
    node_id <- input$node_dragged$id
    rv$nodes <- rv$nodes %>%
      mutate(
        x = ifelse(id == node_id, input$node_dragged$x, x),
        y = ifelse(id == node_id, input$node_dragged$y, y)
      )
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
    rv$ctx$node   <- input$ctx_event$node
    rv$ctx$edge   <- input$ctx_event$edge
    rv$ctx$canvas <- unlist(input$ctx_event$canvas)
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
      x  = rv$ctx$canvas[1],
      y  = rv$ctx$canvas[2],
      hypothesis = default_h,
      alpha      = as.numeric(default_a)
    )
    rv$nodes <- dplyr::bind_rows(rv$nodes, base_row)
    bump_tables()
    updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
    
    # Update the graph immediately without modal interference
    nodes_data <- with_node_label(rv$nodes)
    visNetworkProxy("graph") %>% visUpdateNodes(nodes_data)
    
    # Show notification that node was created
    showNotification(paste("Node", default_h, "created. Double-click to edit alpha value."), 
                     type = "message", duration = 3)
  })
  
  observeEvent(input$ctx_del_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- rv$ctx$node
    if (!is.null(nid)) {
      rv$edges <- dplyr::filter(rv$edges, !(from == nid | to == nid))
      rv$nodes <- dplyr::filter(rv$nodes, id != nid)
      nodes_data <- with_node_label(rv$nodes)
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes_data) %>%
        visUpdateEdges(with_edge_label(rv$edges))
      bump_tables()
      for(i in seq_len(nrow(rv$nodes))) {
        visNetworkProxy("graph") %>%
          visMoveNode(id = rv$nodes$id[i], x = rv$nodes$x[i], y = rv$nodes$y[i])
      }
      updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
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
      )
    removeModal()
    nodes_data <- with_node_label(rv$nodes)
    visNetworkProxy("graph") %>% 
      visUpdateNodes(nodes_data) %>%
      visMoveNode(id = id, x = current_x, y = current_y)
    bump_tables()
    updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
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
    positions <- rv$nodes %>% select(id, x, y)
    rv$edges <- bind_rows(rv$edges,
                          tibble::tibble(id = eid, from = from, to = to, weight = w_val))
    removeModal()
    rv$edge_new <- NULL
    rv$pending_source <- NULL
    visNetworkProxy("graph") %>%
      visUpdateEdges(with_edge_label(rv$edges)) %>%
      visSelectNodes(id = NULL)
    bump_tables()
    for(i in seq_len(nrow(positions))) {
      visNetworkProxy("graph") %>%
        visMoveNode(id = positions$id[i], x = positions$x[i], y = positions$y[i])
    }
  })
  
  # Delete edge
  observeEvent(input$ctx_del_edge, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    eid <- rv$ctx$edge
    if (!is.null(eid) && nrow(rv$edges) > 0 && eid %in% rv$edges$id) {
      positions <- rv$nodes %>% select(id, x, y)
      rv$edges <- dplyr::filter(rv$edges, id != eid)
      visNetworkProxy("graph") %>%
        visUpdateEdges(with_edge_label(rv$edges))
      bump_tables()
      for(i in seq_len(nrow(positions))) {
        visNetworkProxy("graph") %>%
          visMoveNode(id = positions$id[i], x = positions$x[i], y = positions$y[i])
      }
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
    positions <- rv$nodes %>% select(id, x, y)
    rv$edges <- rv$edges %>% mutate(
      weight = ifelse(id == !!eid, w_val, weight)
    )
    removeModal()
    visNetworkProxy("graph") %>%
      visUpdateEdges(with_edge_label(rv$edges))
    bump_tables()
    for(i in seq_len(nrow(positions))) {
      visNetworkProxy("graph") %>%
        visMoveNode(id = positions$id[i], x = positions$x[i], y = positions$y[i])
    }
  })
  
  # TrialSimulator integration
  observeEvent(input$run_ts, {
    req(nrow(rv$nodes) > 0)
    create_trialsimulator_objects()
    tryCatch({
      rv$ts_object <- NULL
      log_lines <- NULL
      log_message <- function(m) { log_lines <<- c(log_lines, conditionMessage(m)); invokeRestart("muffleMessage") }
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
      rv$ts_log <- paste(log_lines, collapse = "\n")
      rv$ts_summary <- rv$ts_object$get_current_testing_results()
      updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
    }, error = function(e) {
      rv$ts_log <- paste("Error during TrialSimulator setup:", e$message)
      rv$ts_object <- NULL
      rv$ts_summary <- NULL
    })
  })
  
  observeEvent(input$reject_ts, {
    req(rv$ts_object, input$graph_selected)
    tryCatch({
      log_lines <- NULL
      log_message <- function(m) { log_lines <<- c(log_lines, conditionMessage(m)); invokeRestart("muffleMessage") }
      withCallingHandlers(
        rv$ts_object$reject_a_hypothesis(input$graph_selected),
        message = log_message
      )
      rv$ts_log <- paste(log_lines, collapse = "\n")
      rv$ts_summary <- rv$ts_object$get_current_testing_results()
      output$ts_plot <- renderPlot({ print(rv$ts_object) })
      if (!is.null(rv$ts_summary) && "rejected" %in% names(rv$ts_summary)) {
        active_hyps <- rv$ts_summary$hypothesis[rv$ts_summary$rejected == FALSE]
        updateSelectInput(session, "graph_selected", choices = active_hyps)
      }
    }, error = function(e) {
      rv$ts_log <- paste("Reject error:", e$message)
    })
  })
  
  observeEvent(input$edit_mode, {
    rv$ts_object <- NULL
    rv$ts_log <- ""
    rv$ts_summary <- NULL
    nodes_data <- with_node_label(rv$nodes)
    visNetworkProxy("graph") %>% visUpdateNodes(nodes_data)
    updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
  })
  
  output$ts_result_table <- renderDataTable({
    req(rv$ts_summary)
    rv$ts_summary
  })
  
  output$ts_log <- renderText({
    rv$ts_log
  })
  
  ##### ---- IMPORT/EXPORT HANDLERS ---- #####
  # ==== EXPORT HANDLER ====
  output$download_graph <- downloadHandler(
    filename = function() paste0("graph-", Sys.Date(), ".json"),
    content = function(file) {
      # Convert to data frames and ensure no named vectors
      nodes_out <- as.data.frame(rv$nodes, stringsAsFactors = FALSE)
      edges_out <- as.data.frame(rv$edges, stringsAsFactors = FALSE)
      
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
      
      # Create the export data
      export_data <- list(
        nodes = nodes_out,
        edges = edges_out
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
    
    rv$nodes <- tibble::as_tibble(nodes)
    rv$edges <- tibble::as_tibble(edges)
    bump_tables()
    
    updateSelectInput(session, "graph_selected", choices = rv$nodes$hypothesis)
    rv$ts_object <- NULL
    rv$ts_log <- ""
    rv$ts_summary <- NULL
  })
}

shinyApp(ui, server)