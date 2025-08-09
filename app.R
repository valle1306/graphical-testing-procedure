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
      box-shadow: 0 6px 18px rgba(0,0,0,.15); min-width: 170px;
    }
    #ctx-menu .ctx-item {
      width: 100%; border: 0; background: transparent; text-align: left;
      padding: 8px 12px; cursor: pointer;
    }
    #ctx-menu .ctx-item:hover { background: #f5f5f5; }
  "))),
  titlePanel("Step 1: Nodes only (context menu + double-click)"),
  visNetworkOutput("graph", height = "600px"),
  # Context menu (buttons so Shiny can capture clicks)
  tags$div(
    id = "ctx-menu",
    actionButton("ctx_add_node", "Add node here", class = "ctx-item"),
    actionButton("ctx_del_node", "Delete this node", class = "ctx-item")
  )
)

server <- function(input, output, session) {
  
  # ---- Helpers ----
  # Compose 2-line label: first line hypothesis, second line alpha (no scientific notation)
  with_node_label <- function(df) {
    df %>%
      mutate(label = paste0(hypothesis, "\n",
                            format(alpha, trim = TRUE, scientific = FALSE)))
  }
  
  # Get next available hypothesis name: H1, H2, ...
  next_hypothesis <- function(existing) {
    k <- 1L
    while (paste0("H", k) %in% existing) k <- k + 1L
    paste0("H", k)
  }
  
  # Validate alpha string: must be plain decimal in [0,1], no scientific notation.
  # Accepts: "0", "0.5", "1", "1.0", "0.00"... Rejects: ".5", "1.", "1e-3", "01", " 0.1 "
  is_valid_alpha_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    # strict pattern: 0 or 0.xxx or 1 or 1.0...
    if (!grepl("^(0(\\.\\d+)?|1(\\.0+)?)$", s)) return(FALSE)
    v <- suppressWarnings(as.numeric(s))
    is.finite(v) && v >= 0 && v <= 1
  }
  
  # ---- State ----
  rv <- reactiveValues(
    # Only store id, position, and the two attributes requested
    nodes = tibble::tibble(
      id    = 1:3,
      x     = c(-150, 0, 150),
      y     = c(-40, 40, -20),
      hypothesis = c("H1", "H2", "H3"),
      alpha      = c(0, 0, 0)
    ),
    ctx = list(node = NULL, canvas = c(0,0), edit_node_id = NULL)
  )
  
  # ---- Render network ----
  output$graph <- renderVisNetwork({
    # Map hypothesis to label for visualization; do not expose "label" as a first-class attribute
    nodes_vis <- with_node_label(rv$nodes)
    visNetwork(nodes_vis, data.frame()) %>%
      visNodes(font = list(size = 16)) %>%
      visPhysics(enabled = FALSE) %>% # fixed positions
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEvents(
        # Right-click: record target node and canvas position, show custom menu
        oncontext = "
          function(params) {
            params.event.preventDefault();
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
            document.getElementById('ctx_add_node').style.display = nodeId ? 'none' : 'block';
            document.getElementById('ctx_del_node').style.display = nodeId ? 'block' : 'none';
          }
        ",
        # Double-click node -> open editor
        doubleClick = "
          function(params) {
            var nid = (params.nodes && params.nodes.length) ? params.nodes[0] : null;
            if(nid !== null) {
              Shiny.setInputValue('dbl_node', nid, {priority: 'event'});
            }
          }
        "
      )
  })
  
  # Hide context menu when clicking anywhere
  observe({
    runjs("
      document.addEventListener('click', function(){
        var m = document.getElementById('ctx-menu');
        if (m) m.style.display='none';
      });
    ")
  })
  
  # Capture right-click context
  observeEvent(input$ctx_event, {
    rv$ctx$node   <- input$ctx_event$node
    rv$ctx$canvas <- unlist(input$ctx_event$canvas)
  })
  
  # Add node at right-click position -> immediately open editor with defaults
  observeEvent(input$ctx_add_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- ifelse(nrow(rv$nodes)==0, 1, max(rv$nodes$id)+1)
    default_h <- next_hypothesis(rv$nodes$hypothesis)
    default_a <- "0"  # keep as string for input prefill
    
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
    
    # Open editor immediately with defaults
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
  
  # Delete the right-clicked node
  observeEvent(input$ctx_del_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- rv$ctx$node
    if (!is.null(nid)) {
      rv$nodes <- dplyr::filter(rv$nodes, id != nid)
      visNetworkProxy("graph") %>% visUpdateNodes(with_node_label(rv$nodes))
    }
  })
  
  # Double-click existing node -> open editor with current values
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
  
  # Persist edits with validation
  observeEvent(input$save_node_edit, {
    id <- rv$ctx$edit_node_id
    if (is.null(id)) return(invisible(NULL))
    
    h_new <- input$edit_node_hypo
    a_str <- input$edit_node_alpha
    
    # Validate hypothesis uniqueness (case-sensitive), excluding the current node
    existing <- rv$nodes$hypothesis[rv$nodes$id != id]
    if (is.null(h_new) || !nzchar(h_new)) {
      showNotification("Hypothesis cannot be empty.", type = "error")
      return(invisible(NULL))
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
    
    # global alpha sum constraint: sum of all alphas must be <= 1
    others_sum <- sum(rv$nodes$alpha[rv$nodes$id != id], na.rm = TRUE)
    # small epsilon to avoid float noise when exactly 1
    if (others_sum + a_val > 1 + 1e-12) {
      msg <- sprintf("Total alpha would be %.6f (> 1). Please reduce this node's alpha.", others_sum + a_val)
      showNotification(msg, type = "error")
      return(invisible(NULL))
    }
    
    # Apply updates
    rv$nodes <- rv$nodes %>%
      mutate(
        hypothesis = ifelse(id == !!id, h_new, hypothesis),
        alpha      = ifelse(id == !!id, a_val, alpha)
      )
    removeModal()
    visNetworkProxy("graph") %>% visUpdateNodes(with_node_label(rv$nodes))
  })
}

shinyApp(ui, server)
