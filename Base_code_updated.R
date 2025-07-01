library(shiny)
library(visNetwork)
library(DT)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Interactive Graphical Testing Editor (Start from Scratch)"),
  
  # Centered buttons
  fluidRow(
    column(12, align = "center",
           actionButton("add_node", "Add Node", class = "btn btn-primary"),
           actionButton("add_edge", "Add Edge", class = "btn btn-success"),
           actionButton("delete_node", "Delete Node", class = "btn btn-danger"),
           actionButton("delete_edge", "Delete Edge", class = "btn btn-danger")
    )
  ),
  br(),
  
  fluidRow(
    column(3,
           wellPanel(
             tags$div(style = "min-height: 650px;",
                      fileInput("upload", "Upload JSON Graph File"),
                      downloadButton("export", "Export"),
                      tags$hr(),
                      h5(tags$b("Node Table"), style = "color:blue"),
                      dataTableOutput("node_table"),
                      tags$hr(),
                      h5(tags$b("Edge Table"), style = "color:darkgreen"),
                      dataTableOutput("edge_table")
             )
           )
    ),
    column(9,
           visNetworkOutput("graph", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    nodes = data.frame(label = character(0), alpha = numeric(0), stringsAsFactors = FALSE),
    edges = data.frame(from = character(0), to = character(0), weight = numeric(0), label = character(0), smooth = I(list()), stringsAsFactors = FALSE)
  )
  
  # add node 
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Add Node",
      textInput("node_label", "Label (e.g., H1: abc)", ""),
      numericInput("node_alpha", "Alpha", value = NA, step = 0.01),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_node", "Add")
      )
    ))
  })
  
  observeEvent(input$confirm_add_node, {
    req(input$node_label)
    new_node <- data.frame(
      label = input$node_label,
      alpha = input$node_alpha,
      title = paste0("α = ", format(input$node_alpha, nsmall = 2)),
      stringsAsFactors = FALSE
    )
    rv$nodes <- rbind(rv$nodes, new_node)
    removeModal()
  })
  
  # add edge 
  observeEvent(input$add_edge, {
    if (nrow(rv$nodes) < 2) {
      showModal(modalDialog(
        "You need at least two nodes to create an edge.", easyClose=TRUE
      ))
      return()
    }
    showModal(modalDialog(
      title = "Add Edge",
      selectInput("edge_from", "From Node", choices = rv$nodes$label),
      selectInput("edge_to", "To Node", choices = rv$nodes$label, selected=rv$nodes$label[2]),
      numericInput("edge_weight", "Weight", value = NA, min=0, max=1, step = 0.01),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_edge", "Add")
      )
    ))
  })
  
  observeEvent(input$confirm_add_edge, {
    req(input$edge_from, input$edge_to)
    existing_reverse <- with(rv$edges, which(from == input$edge_to & to == input$edge_from))
    curvature_type <- if (length(existing_reverse) > 0) {
      list(enabled = TRUE, type = "curvedCW")
    } else {
      list(enabled = FALSE)
    }
    new_edge <- data.frame(
      from = input$edge_from, 
      to = input$edge_to,    
      weight = input$edge_weight,
      label = as.character(input$edge_weight),
      smooth = I(list(curvature_type)),
      stringsAsFactors = FALSE
    )
    rv$edges <- rbind(rv$edges, new_edge)
    removeModal()
  })
  
  # double click to edit node & edge
  observeEvent(input$graph_doubleClick, {
    # If node double-clicked
    if (!is.null(input$graph_doubleClick$nodes) && length(input$graph_doubleClick$nodes) > 0) {
      node_label <- input$graph_doubleClick$nodes[[1]]
      node_row <- rv$nodes[rv$nodes$label == node_label, ]
      if (nrow(node_row) == 1) {
        showModal(modalDialog(
          title = "Edit Node",
          textInput("edit_node_label", "Node Label", value = node_row$label),
          numericInput("edit_node_alpha", "Alpha", value = node_row$alpha, step = 0.01),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_edit_node_dblclk", "Update")
          )
        ))
        session$userData$editing_node_label <- node_label
      }
      return()
    }
    # If edge double-clicked
    if (!is.null(input$graph_doubleClick$edges) && length(input$graph_doubleClick$edges) > 0) {
      # edge = list(from, to)
      edge_info <- input$graph_doubleClick$edges[[1]]
      if (is.list(edge_info)) {
        from <- edge_info$from
        to <- edge_info$to
      } else {
        edge_string <- edge_info
        fromto <- strsplit(edge_string, "->")[[1]]
        from <- fromto[1]
        to <- fromto[2]
      }
      edge_row <- rv$edges[rv$edges$from == from & rv$edges$to == to, ]
      if (nrow(edge_row) == 1) {
        showModal(modalDialog(
          title = "Edit Edge Weight",
          numericInput("edit_edge_weight", "New Weight", value = edge_row$weight, step = 0.01),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_edit_edge", "Update")
          )
        ))
        session$userData$editing_edge_from <- from
        session$userData$editing_edge_to <- to
      }
      return()
    }
  })
  
  #confirm the edit of node & edge
  observeEvent(input$confirm_edit_node_dblclk, {
    node_label <- session$userData$editing_node_label
    idx <- which(rv$nodes$label == node_label)
    if (length(idx) == 1) {
      rv$nodes$label[idx] <- input$edit_node_label
      rv$nodes$alpha[idx] <- input$edit_node_alpha
      rv$nodes$title[idx] <- paste0("α = ", format(input$edit_node_alpha, nsmall = 2))
      # Also update edges referencing old label to new label, if label was changed
      old_label <- node_label
      new_label <- input$edit_node_label
      if (old_label != new_label) {
        rv$edges$from[rv$edges$from == old_label] <- new_label
        rv$edges$to[rv$edges$to == old_label] <- new_label
      }
    }
    removeModal()
  })
  
  observeEvent(input$confirm_edit_edge, {
    from <- session$userData$editing_edge_from
    to <- session$userData$editing_edge_to
    idx <- which(rv$edges$from == from & rv$edges$to == to)
    if (length(idx) == 1) {
      rv$edges$weight[idx] <- input$edit_edge_weight
      rv$edges$label[idx] <- as.character(input$edit_edge_weight)
    }
    removeModal()
  })
  
  
  # delete node
  observeEvent(input$delete_node, {
    selected_node <- input$graph_selected
    if (!is.null(selected_node)) {
      sel_label <- selected_node # label should be character
      rv$nodes <- rv$nodes[rv$nodes$label != sel_label, ]
      rv$edges <- rv$edges[!(rv$edges$from == sel_label | rv$edges$to == sel_label), ]
    }
  })
  
  # delete edge
  observeEvent(input$delete_edge, {
    selected_edges <- input$graph_selectedEdges
    if (!is.null(selected_edges)) {
      for (edge in selected_edges) {
        # edge is string "from->to"
        fromto <- strsplit(edge, "->")[[1]]
        if(length(fromto)==2){
          from <- fromto[1]
          to <- fromto[2]
          rv$edges <- rv$edges[!(rv$edges$from == from & rv$edges$to == to), ]
        }
      }
    }
  })
  
  # render graph
  output$graph <- renderVisNetwork({
    gnodes <- rv$nodes
    # visNetwork 'id' must be unique; use 'label' for id here since label has to be unique (required for selection, editing, etc)
    gnodes$id <- gnodes$label
    gedges <- rv$edges
    # Make an 'id' for each edge as "from->to" for selection
    if(nrow(gedges) > 0){
      gedges$id <- paste0(gedges$from, "->", gedges$to)
    } else {
      gedges$id <- character(0)
    }
    visNetwork(gnodes, gedges) %>%
      visNodes(
        shape = "ellipse",
        color = list(background = "lightblue", border = "blue"),
        font = list(size = 20),
        shadow = TRUE
      ) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.6)),
        smooth = TRUE,
        font = list(align = "middle", size = 16),
        color = list(color = "blue")
      ) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE),
        manipulation = FALSE
      ) %>%
      visPhysics(
        solver = "repulsion",
        repulsion = list(
          nodeDistance = 200,
          centralGravity = 0.1,
          springLength = 200,
          springConstant = 0.02,
          damping = 0.09
        ),
        stabilization = list(enabled = TRUE, iterations = 200)
      ) %>%
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        hover = TRUE
      ) %>%
      visEvents(
        doubleClick = "function(params) { Shiny.setInputValue('graph_doubleClick', params, {priority: 'event'}); }",
        select = "function(nodes) { 
            Shiny.setInputValue('graph_selected', nodes.nodes.length > 0 ? nodes.nodes[0] : null); 
            Shiny.setInputValue('graph_selectedEdges', nodes.edges); 
        }"
      )
  })
  
  # render tables
  output$node_table <- renderDataTable({
    rv$nodes[, c("label", "alpha")]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
  output$edge_table <- renderDataTable({
    rv$edges[, c("from", "to", "weight")]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
  # file import & export
  observeEvent(input$upload, {
    req(input$upload)
    json_data <- fromJSON(input$upload$datapath)
    rv$nodes <- json_data$nodes
    rv$edges <- json_data$edges
  })
  
  output$export <- downloadHandler(
    filename = function() {
      paste0("graph-", Sys.Date(), ".json")
    },
    content = function(file) {
      write_json(list(nodes = rv$nodes, edges = rv$edges), file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
}

shinyApp(ui, server)





