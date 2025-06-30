library(shiny)
library(visNetwork)

ui <- fluidPage(
  titlePanel("Interactive Graphical Testing Editor (Start from Scratch)"),
  fluidRow(
    column(12,
           actionButton("add_node", label = "Add Node", icon = icon("plus-circle")),
           actionButton("add_edge_mode", label = "Add Edge", icon = icon("pen")),
           actionButton("edit_node_mode", label = "Edit Node", icon = icon("edit")),
           actionButton("delete_selected", label = "Delete selected", icon = icon("times-circle"))
    )
  ),
  visNetworkOutput("graph", height = "600px"),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  graph_data <- reactiveValues(
    nodes = data.frame(
      id = numeric(0),
      label = character(0),
      alpha = numeric(0),
      title = character(0),
      shape = character(0),
      font = I(list()),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = numeric(0),
      from = numeric(0),
      to = numeric(0),
      label = character(0),
      weight = numeric(0),
      title = character(0),
      arrows = character(0),
      stringsAsFactors = FALSE
    ),
    node_counter = 0,
    edge_counter = 0
  )
  
  # Render the graph
  output$graph <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(shape = "ellipse") %>%
      visEdges(arrows = "to") %>%
      visOptions(manipulation = FALSE) %>%
      visEvents(
        doubleClick = "function(properties) {
          if (properties.nodes.length > 0) {
            Shiny.setInputValue('node_dblclick', properties.nodes[0], {priority: 'event'});
          } else if (properties.edges.length > 0) {
            Shiny.setInputValue('edge_dblclick', properties.edges[0], {priority: 'event'});
          }
        }"
      )
  })
  
  # Add Node
  observeEvent(input$add_node, {
    graph_data$node_counter <- graph_data$node_counter + 1
    new_id <- graph_data$node_counter
    new_label <- paste0("H", new_id)
    
    graph_data$nodes <- rbind(
      graph_data$nodes,
      data.frame(
        id = new_id,
        label = new_label,
        alpha = 0,
        title = "α = 0",
        shape = "ellipse",
        font = I(list(list(valign = "middle", color = "black"))),
        stringsAsFactors = FALSE
      )
    )
  })
  
  # Add Edge Mode
  observeEvent(input$add_edge_mode, {
    showModal(modalDialog(
      title = "Add Edge",
      numericInput("from_id", "From Node ID:", min = 1, value = 1),
      numericInput("to_id", "To Node ID:", min = 1, value = 1),
      numericInput("edge_weight", "Weight:", value = 1, step = 0.1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_edge", "Add Edge")
      )
    ))
  })
  
  observeEvent(input$confirm_add_edge, {
    graph_data$edge_counter <- graph_data$edge_counter + 1
    edge_id <- graph_data$edge_counter
    
    graph_data$edges <- rbind(
      graph_data$edges,
      data.frame(
        id = edge_id,
        from = input$from_id,
        to = input$to_id,
        label = as.character(input$edge_weight),
        weight = input$edge_weight,
        title = paste("Weight:", input$edge_weight),
        arrows = "to",
        stringsAsFactors = FALSE
      )
    )
    removeModal()
  })
  
  # Edit Node Alpha on Double Click
  observeEvent(input$node_dblclick, {
    node_id <- input$node_dblclick
    node_row <- graph_data$nodes[graph_data$nodes$id == node_id, ]
    
    showModal(modalDialog(
      title = paste("Edit Node", node_id),
      numericInput("new_alpha", "Alpha (0-1):", value = node_row$alpha, min = 0, max = 1, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_node_edit", "Save")
      )
    ))
  })
  
  observeEvent(input$save_node_edit, {
    node_id <- input$node_dblclick
    new_alpha <- input$new_alpha
    graph_data$nodes[graph_data$nodes$id == node_id, "alpha"] <- new_alpha
    graph_data$nodes[graph_data$nodes$id == node_id, "title"] <- paste("α =", new_alpha)
    removeModal()
  })
  
  # Edit Edge Weight on Double Click
  observeEvent(input$edge_dblclick, {
    edge_id <- input$edge_dblclick
    edge_row <- graph_data$edges[graph_data$edges$id == edge_id, ]
    
    showModal(modalDialog(
      title = paste("Edit Edge", edge_id),
      numericInput("new_weight", "Weight:", value = edge_row$weight, min = 0, step = 0.1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edge_edit", "Save")
      )
    ))
  })
  
  observeEvent(input$save_edge_edit, {
    edge_id <- input$edge_dblclick
    new_weight <- input$new_weight
    
    if (edge_id %in% graph_data$edges$id) {
      graph_data$edges[graph_data$edges$id == edge_id, "weight"] <- new_weight
      graph_data$edges[graph_data$edges$id == edge_id, "label"] <- as.character(new_weight)
      graph_data$edges[graph_data$edges$id == edge_id, "title"] <- paste("Weight:", new_weight)
    } else {
      showNotification("Edge ID not found. Could not update weight.", type = "error")
    }
    removeModal()
  })
  
  # Delete Selected Node/Edge
  observeEvent(input$delete_selected, {
    visNetworkProxy("graph") %>%
      visGetSelectedNodes() %>%
      visGetSelectedEdges()
    
    observeEvent(input$graph_selectedNodes, {
      selected_nodes <- input$graph_selectedNodes
      if (length(selected_nodes) > 0) {
        graph_data$nodes <- subset(graph_data$nodes, !(id %in% selected_nodes))
        graph_data$edges <- subset(graph_data$edges, !(from %in% selected_nodes | to %in% selected_nodes))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$graph_selectedEdges, {
      selected_edges <- input$graph_selectedEdges
      if (length(selected_edges) > 0) {
        graph_data$edges <- subset(graph_data$edges, !(id %in% selected_edges))
      }
    }, ignoreInit = TRUE)
  })
  
  output$debug <- renderPrint({
    list(
      nodes = graph_data$nodes,
      edges = graph_data$edges
    )
  })
}

shinyApp(ui, server)







