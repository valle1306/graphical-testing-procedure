# app.R
library(shiny)
library(visNetwork)
library(shinyjs)
library(dplyr)
library(DT)

ui <- fluidPage(
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
  
  tabsetPanel(id = "main_tabs",
              # --- Tab 1: Landing (placeholder content for now) ---
              tabPanel(
                title = "Landing",
                fluidRow(
                  column(
                    width = 12,
                    h3("Welcome"),
                    p("This app lets you interactively design a directed graph for statistical workflows."),
                    p("Use the 'Design' tab to add/delete nodes, create directed edges, and edit attributes."),
                    tags$hr(),
                    p(em("Note: This landing content is a placeholder and can be refined later."))
                  )
                )
              ),
              
              # --- Tab 2: Design (your existing graph UI moved here) ---
              tabPanel(
                title = "Design",
                fluidRow(
                  # ----- Left column: data panel (read-only tables) -----
                  column(
                    width = 3,
                    h4("Nodes"),
                    DTOutput("nodes_table"),
                    tags$hr(),
                    h4("Edges"),
                    DTOutput("edges_table")
                  ),
                  # ----- Right column: graph canvas -----
                  column(
                    width = 9,
                    visNetworkOutput("graph", height = "640px"),
                    # context menu must remain inside Design tab
                    tags$div(
                      id = "ctx-menu",
                      actionButton("ctx_add_node",   "Add node here",        class = "ctx-item"),
                      actionButton("ctx_del_node",   "Delete this node",     class = "ctx-item"),
                      actionButton("ctx_edge_start", "Start edge from here", class = "ctx-item"),
                      actionButton("ctx_del_edge",   "Delete this edge",     class = "ctx-item")
                    )
                  )
                )
              )
  )
)

server <- function(input, output, session) {
  
  # ---------- Helpers ----------
  # Node label: hypothesis on line 1, alpha on line 2 (no scientific notation)
  with_node_label <- function(df) {
    df %>%
      mutate(label = paste0(hypothesis, "\n",
                            format(alpha, trim = TRUE, scientific = FALSE)))
  }
  
  # Edge label & shape with proper curve handling
  with_edge_label <- function(df) {
    if (nrow(df) == 0) return(df)
    
    # undirected pair key to detect reverse edges
    key        <- paste(pmin(df$from, df$to), pmax(df$from, df$to), sep = "_")
    dup_counts <- ave(key, key, FUN = length)
    has_pair   <- dup_counts > 1  # TRUE when both directions exist
    
    # Per-edge smooth config
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
  
  # Next available hypothesis name: H1, H2, ...
  next_hypothesis <- function(existing) {
    k <- 1L
    while (paste0("H", k) %in% existing) k <- k + 1L
    paste0("H", k)
  }
  
  # Alpha string validator: plain decimal in [0,1], no scientific notation
  is_valid_alpha_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0(\\.\\d+)?|1(\\.0+)?)$", s)
  }
  
  # Weight string validator: plain decimal in (0,1], no scientific notation, not 0
  is_valid_weight_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0\\.[0-9]+|1(\\.0+)?)$", s)
  }
  
  cancel_pending_js <- "Shiny.setInputValue('cancel_pending', Math.random(), {priority:'event'});"
  
  # ---------- State ----------
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
    pending_source = NULL  # when not NULL, we're in PendingTarget(source)
  )
  
  tables_tick <- reactiveVal(0)
  bump_tables <- function() tables_tick(tables_tick() + 1)
  
  # ---------- Render network ----------
  output$graph <- renderVisNetwork({
    nodes_data <- with_node_label(rv$nodes)
    
    visNetwork(nodes_data, with_edge_label(rv$edges)) %>%
      visNodes(
        font = list(size = 16)
        # Removed fixed property to allow free dragging
      ) %>%
      visEdges(
        font = list(background = "white"),
        hoverWidth = 3,
        selectionWidth = 3
      ) %>%
      visPhysics(
        enabled = FALSE,  # Completely disable physics
        stabilization = FALSE  # Disable stabilization
      ) %>%
      visOptions(
        highlightNearest = FALSE, 
        nodesIdSelection = TRUE,
        manipulation = list(enabled = FALSE)  # Disable built-in manipulation
      ) %>%
      visInteraction(
        selectConnectedEdges = FALSE, 
        hoverConnectedEdges = FALSE,
        dragNodes = TRUE,  # Allow manual dragging
        dragView = TRUE    # Allow panning
      ) %>% 
      visEvents(
        # Track node position changes when dragged
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
        # Right-click anywhere -> show menu
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
        # Double-click node/edge -> open editor
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
        # General click
        click = "
          function(params) {
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            var eid = (params.edges && params.edges.length) ? params.edges[0] : null;
            Shiny.setInputValue('click_event', {node: nid, edge: eid}, {priority: 'event'});
          }
        "
      )
  })
  
  # Keep the visNetwork output active even when the tab is hidden
  outputOptions(output, "graph", suspendWhenHidden = FALSE)
  
  # ---- Read-only tables for Design page ----
  
  # Nodes table: hypothesis, alpha; no search/sort/selection/edit
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
  
  # Edges table: from, to (as hypothesis), weight; search enabled, no sort/selection/edit
  output$edges_table <- renderDT({
    tables_tick()  # ← 同上
    isolate({
      if (!nrow(rv$edges)) {
        return(datatable(data.frame(from=character(), to=character(), weight=numeric()),
                         rownames=FALSE,
                         options=list(dom='ft', paging=FALSE, searching=TRUE, ordering=FALSE, info=FALSE)
        ))
      }
      from_h <- rv$nodes$hypothesis[match(rv$edges$from, rv$nodes$id)]
      to_h   <- rv$nodes$hypothesis[match(rv$edges$to,   rv$nodes$id)]
      df <- data.frame(from=from_h, to=to_h, weight=rv$edges$weight, stringsAsFactors=FALSE)
      df$weight <- vapply(df$weight, function(x) format(x, trim=TRUE, scientific=FALSE), character(1))
      datatable(df, rownames=FALSE,
                options=list(dom='ft', paging=FALSE, searching=TRUE, ordering=FALSE, info=FALSE)
      )
    })
  })
  
  # Update node position when dragged
  observeEvent(input$node_dragged, {
    node_id <- input$node_dragged$id
    rv$nodes <- rv$nodes %>%
      mutate(
        x = ifelse(id == node_id, input$node_dragged$x, x),
        y = ifelse(id == node_id, input$node_dragged$y, y)
      )
  })
  
  # Global JS: hide context menu on any click & listen for ESC
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
  
  # Right-click context target
  observeEvent(input$ctx_event, {
    rv$ctx$node   <- input$ctx_event$node
    rv$ctx$edge   <- input$ctx_event$edge
    rv$ctx$canvas <- unlist(input$ctx_event$canvas)
  })
  
  # Any right-click cancels PendingTarget first
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
    
    new_node <- with_node_label(base_row) |> 
      as.data.frame(stringsAsFactors = FALSE)
    
    session$onFlushed(function() {
      visNetworkProxy("graph") %>% visUpdateNodes(new_node)
    }, once = TRUE)
    
    # Open node editor immediately (as in original)
    rv$ctx$edit_node_id <- nid
    showModal(modalDialog(
      title = paste("Edit node", nid),
      textInput("edit_node_hypo",  "Hypothesis (unique, case-sensitive)", value = default_h, placeholder = "e.g., H4"),
      textInput("edit_node_alpha", "Alpha (0–1, no scientific notation)", value = default_a, placeholder = "0 or 0.xxx or 1"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_node_edit", "Save", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$ctx_del_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- rv$ctx$node
    if (!is.null(nid)) {
      # Remove incident edges
      rv$edges <- dplyr::filter(rv$edges, !(from == nid | to == nid))
      rv$nodes <- dplyr::filter(rv$nodes, id != nid)
      
      nodes_data <- with_node_label(rv$nodes)
      
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes_data) %>%
        visUpdateEdges(with_edge_label(rv$edges))
      
      bump_tables()
      
      # Restore positions for remaining nodes
      for(i in seq_len(nrow(rv$nodes))) {
        visNetworkProxy("graph") %>%
          visMoveNode(id = rv$nodes$id[i], x = rv$nodes$x[i], y = rv$nodes$y[i])
      }
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
  })
  
  observeEvent(input$save_node_edit, {
    id <- rv$ctx$edit_node_id
    if (is.null(id)) return(invisible(NULL))
    
    h_new <- input$edit_node_hypo
    a_str <- input$edit_node_alpha
    
    # Validation checks
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
    
    # Store current positions before update
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
    
    # Store current positions before adding edge
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
    
    # Restore all node positions after edge update
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
      # Store positions before deletion
      positions <- rv$nodes %>% select(id, x, y)
      
      rv$edges <- dplyr::filter(rv$edges, id != eid)
      visNetworkProxy("graph") %>%
        visUpdateEdges(with_edge_label(rv$edges))
      bump_tables()
      
      # Restore positions
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
    
    # Store positions before update
    positions <- rv$nodes %>% select(id, x, y)
    
    rv$edges <- rv$edges %>% mutate(
      weight = ifelse(id == !!eid, w_val, weight)
    )
    removeModal()
    
    visNetworkProxy("graph") %>%
      visUpdateEdges(with_edge_label(rv$edges))
    bump_tables()
    
    # Restore positions
    for(i in seq_len(nrow(positions))) {
      visNetworkProxy("graph") %>%
        visMoveNode(id = positions$id[i], x = positions$x[i], y = positions$y[i])
    }
  })
  
}

shinyApp(ui, server)