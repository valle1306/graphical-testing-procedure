# app.R
library(shiny)
library(visNetwork)
library(shinyjs)
library(dplyr)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    #ctx-menu {
      position: absolute; z-index: 10000; display: none;
      background: #fff; border: 1px solid #ddd; border-radius: 6px;
      box-shadow: 0 6px 18px rgba(0,0,0,.15); min-width: 180px;
    }
    #ctx-menu .ctx-item {
      width: 100%; border: 0; background: transparent; text-align: left;
      padding: 8px 12px; cursor: pointer;
    }
    #ctx-menu .ctx-item:hover { background: #f5f5f5; }
  "))),
  titlePanel("Nodes + Create Edge (right-click start)"),
  visNetworkOutput("graph", height = "640px"),
  # Context menu (buttons so Shiny can capture clicks)
  tags$div(
    id = "ctx-menu",
    actionButton("ctx_add_node",   "Add node here",        class = "ctx-item"),
    actionButton("ctx_del_node",   "Delete this node",     class = "ctx-item"),
    actionButton("ctx_edge_start", "Start edge from here", class = "ctx-item")
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
  
  # Edge label = weight (no scientific notation)
  with_edge_label <- function(df) {
    if (nrow(df) == 0) return(df)
    df %>%
      mutate(label = format(weight, trim = TRUE, scientific = FALSE),
             arrows = "to")
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
    ctx = list(node = NULL, canvas = c(0,0), edit_node_id = NULL),
    pending_source = NULL  # when not NULL, we're in PendingTarget(source)
  )
  
  # ---------- Render network ----------
  output$graph <- renderVisNetwork({
    visNetwork(with_node_label(rv$nodes),
               with_edge_label(rv$edges)) %>%
      visNodes(font = list(size = 16)) %>%
      visEdges(smooth = list(enabled = TRUE, type = "dynamic")) %>%
      visPhysics(enabled = FALSE) %>%
      visOptions(highlightNearest = FALSE, nodesIdSelection = TRUE) %>%
      visEvents(
        # Right-click anywhere -> show menu; also notify server (for potential cancel)
        oncontext = "
          function(params) {
            params.event.preventDefault();
            // always notify a right-click so server can cancel pending state
            Shiny.setInputValue('any_context', { }, {priority: 'event'});

            var pointer = params.pointer;
            var nodeId = this.getNodeAt(pointer.DOM);

            Shiny.setInputValue('ctx_event', {
              node: nodeId || null,
              canvas: pointer.canvas
            }, {priority: 'event'});

            var menu = document.getElementById('ctx-menu');
            menu.style.left = params.event.pageX + 'px';
            menu.style.top  = params.event.pageY + 'px';
            menu.style.display = 'block';

            // Show items depending on target (blank vs node)
            document.getElementById('ctx_add_node').style.display   = nodeId ? 'none' : 'block';
            document.getElementById('ctx_del_node').style.display   = nodeId ? 'block' : 'none';
            document.getElementById('ctx_edge_start').style.display = nodeId ? 'block' : 'none';
          }
        ",
        # Double-click node -> open node editor
        doubleClick = "
          function(params) {
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            if(nid !== null) {
              Shiny.setInputValue('dbl_node', nid, {priority: 'event'});
            }
          }
        ",
        # General click (to detect clicking blank or selecting a target node)
        click = "
          function(params) {
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            var eid = (params.edges && params.edges.length) ? params.edges[0] : null;
            Shiny.setInputValue('click_event', {node: nid, edge: eid}, {priority: 'event'});
          }
        "
      )
  })
  
  # Global JS: hide context menu on any click & listen for ESC to cancel pending
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
    rv$ctx$canvas <- unlist(input$ctx_event$canvas)
  })
  
  # Any right-click cancels PendingTarget first (then normal menu handling proceeds)
  observeEvent(input$any_context, {
    if (!is.null(rv$pending_source)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL) # clear highlight
    }
  }, ignoreInit = TRUE)
  
  # ---------- Node: add / delete / edit (unchanged except label mapping) ----------
  observeEvent(input$ctx_add_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- ifelse(nrow(rv$nodes)==0, 1, max(rv$nodes$id)+1)
    default_h <- next_hypothesis(rv$nodes$hypothesis)
    default_a <- "0"
    
    rv$nodes <- bind_rows(
      rv$nodes,
      tibble::tibble(
        id = nid,
        x  = rv$ctx$canvas[1], y = rv$ctx$canvas[2],
        hypothesis = default_h,
        alpha      = as.numeric(default_a)
      )
    )
    visNetworkProxy("graph") %>% visUpdateNodes(with_node_label(rv$nodes))
    
    # Open node editor immediately
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
      # Also remove incident edges
      rv$edges <- dplyr::filter(rv$edges, !(from == nid | to == nid))
      rv$nodes <- dplyr::filter(rv$nodes, id != nid)
      visNetworkProxy("graph") %>%
        visUpdateNodes(with_node_label(rv$nodes)) %>%
        visUpdateEdges(with_edge_label(rv$edges))
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
    
    # Uniqueness check (case-sensitive), excluding the current node
    existing <- rv$nodes$hypothesis[rv$nodes$id != id]
    if (is.null(h_new) || !nzchar(h_new)) {
      showNotification("Hypothesis cannot be empty.", type = "error"); return(invisible(NULL))
    }
    if (h_new %in% existing) {
      showNotification(sprintf("Hypothesis '%s' already exists. Please choose a unique value.", h_new), type = "error")
      return(invisible(NULL))
    }
    
    # Alpha validation (plain decimal in [0,1], no scientific notation)
    if (!is_valid_alpha_str(a_str)) {
      showNotification("Alpha must be a plain decimal within [0, 1], no scientific notation.", type = "error")
      return(invisible(NULL))
    }
    a_val <- as.numeric(a_str)
    
    # Global alpha sum constraint: sum(other) + a_val <= 1
    others_sum <- sum(rv$nodes$alpha[rv$nodes$id != id], na.rm = TRUE)
    if (others_sum + a_val > 1 + 1e-12) {
      msg <- sprintf("Total alpha would be %.6f (> 1). Please reduce this node's alpha.", others_sum + a_val)
      showNotification(msg, type = "error")
      return(invisible(NULL))
    }
    
    rv$nodes <- rv$nodes %>%
      mutate(
        hypothesis = ifelse(id == !!id, h_new, hypothesis),
        alpha      = ifelse(id == !!id, a_val, alpha)
      )
    removeModal()
    visNetworkProxy("graph") %>% visUpdateNodes(with_node_label(rv$nodes))
  })
  
  # ---------- Create edge: right-click node -> Start edge from here ----------
  observeEvent(input$ctx_edge_start, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    src <- rv$ctx$node
    if (is.null(src)) return(invisible(NULL))
    
    # Enter PendingTarget
    rv$pending_source <- src
    # Highlight source node (select)
    visNetworkProxy("graph") %>% visSelectNodes(id = src)
    
    showNotification(sprintf("Select a target node for edge from %s", src),
                     type = "message", duration = 2)
  })
  
  # Cancel PendingTarget on ESC / blank click
  observeEvent(input$cancel_pending, {
    if (!is.null(rv$pending_source)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Canceled.", type = "default", duration = 1.5)
    }
  }, ignoreInit = TRUE)
  
  # Click handler: pick target or cancel if clicked blank while pending
  observeEvent(input$click_event, {
    if (is.null(rv$pending_source)) return(invisible(NULL))
    nid <- input$click_event$node
    
    # Clicked blank -> cancel
    if (is.null(nid)) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Canceled.", type = "default", duration = 1.5)
      return(invisible(NULL))
    }
    
    src <- rv$pending_source
    tgt <- nid
    
    # Self-loop not allowed
    if (tgt == src) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification("Self-loop is not allowed.", type = "error")
      return(invisible(NULL))
    }
    
    # Same-direction edge already exists?
    exists_ab <- any(rv$edges$from == src & rv$edges$to == tgt)
    if (exists_ab) {
      rv$pending_source <- NULL
      visNetworkProxy("graph") %>% visSelectNodes(id = NULL)
      showNotification(sprintf("Edge %s → %s already exists.", src, tgt), type = "error")
      return(invisible(NULL))
    }
    
    # Open weight dialog (default 1)
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
  
  # Save new edge (validate weight then create)
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
    # Create edge
    eid <- ifelse(nrow(rv$edges) == 0, 1L, max(rv$edges$id) + 1L)
    rv$edges <- bind_rows(rv$edges,
                          tibble::tibble(id = eid, from = from, to = to, weight = w_val))
    removeModal()
    rv$edge_new <- NULL
    
    # Exit PendingTarget and clear highlight
    rv$pending_source <- NULL
    visNetworkProxy("graph") %>%
      visUpdateEdges(with_edge_label(rv$edges)) %>%
      visSelectNodes(id = NULL)
  })
}

shinyApp(ui, server)
