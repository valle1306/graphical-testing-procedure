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
  titlePanel("Step 1: Context Menu + Double-Click (Nodes only)"),
  visNetworkOutput("graph", height = "600px"),
  # Context menu (buttons so Shiny can capture clicks)
  tags$div(
    id = "ctx-menu",
    actionButton("ctx_add_node", "Add node here", class = "ctx-item"),
    actionButton("ctx_del_node", "Delete this node", class = "ctx-item")
  )
)

server <- function(input, output, session) {
  
  # Reactive state: nodes + last context (node id / canvas position)
  rv <- reactiveValues(
    nodes = tibble::tibble(
      id    = 1:3,
      label = c("Alpha", "Beta", "Gamma"),
      x     = c(-150, 0, 150),
      y     = c(-40, 40, -20),
      hypothesis = c("H1", "H0", "Exploratory"),
      alpha      = c(0.025, 0.025, 0.05)
    ),
    ctx = list(node = NULL, canvas = c(0,0), edit_node_id = NULL)
  )
  
  # Render network (no edges at this step)
  output$graph <- renderVisNetwork({
    visNetwork(rv$nodes, data.frame()) %>%
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
  
  # Add node at right-click position
  observeEvent(input$ctx_add_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- ifelse(nrow(rv$nodes)==0, 1, max(rv$nodes$id)+1)
    rv$nodes <- bind_rows(
      rv$nodes,
      tibble::tibble(
        id = nid,
        label = paste0("Node ", nid),
        x = rv$ctx$canvas[1], y = rv$ctx$canvas[2],
        hypothesis = "",
        alpha = 0.05
      )
    )
    visNetworkProxy("graph") %>% visUpdateNodes(rv$nodes)
  })
  
  # Delete the right-clicked node
  observeEvent(input$ctx_del_node, {
    runjs("document.getElementById('ctx-menu').style.display='none';")
    nid <- rv$ctx$node
    if (!is.null(nid)) {
      rv$nodes <- dplyr::filter(rv$nodes, id != nid)
      visNetworkProxy("graph") %>% visUpdateNodes(rv$nodes)
    }
  })
  
  # Open editor on double-click
  observeEvent(input$dbl_node, {
    nid <- input$dbl_node
    nd <- rv$nodes %>% dplyr::filter(id == nid) %>% dplyr::slice(1)
    rv$ctx$edit_node_id <- nid
    showModal(modalDialog(
      title = paste("Edit node", nd$id),
      textInput("edit_node_label", "Label", value = nd$label),
      textInput("edit_node_hypo",  "Hypothesis", value = nd$hypothesis, placeholder = "e.g., H1: treatment > control"),
      numericInput("edit_node_alpha", "Alpha (0–1)", value = nd$alpha, min = 0, max = 1, step = 0.001),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_node_edit", "Save", class = "btn-primary")
      )
    ))
  })
  
  # Persist edits (label, hypothesis, alpha)
  observeEvent(input$save_node_edit, {
    removeModal()
    id <- rv$ctx$edit_node_id
    # basic validation for alpha
    a <- input$edit_node_alpha
    if (is.na(a) || a < 0 || a > 1) {
      showNotification("Alpha must be in [0, 1]. Edit discarded.", type = "error")
      return(invisible(NULL))
    }
    rv$nodes <- rv$nodes %>%
      mutate(
        label      = ifelse(id == !!id, input$edit_node_label, label),
        hypothesis = ifelse(id == !!id, input$edit_node_hypo,  hypothesis),
        alpha      = ifelse(id == !!id, a, alpha)
      )
    visNetworkProxy("graph") %>% visUpdateNodes(rv$nodes)
  })
}

shinyApp(ui, server)
