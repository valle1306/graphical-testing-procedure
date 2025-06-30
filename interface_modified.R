library(shiny)
library(visNetwork)

ui <- fluidPage(
<<<<<<< HEAD
  titlePanel("Interactive Graphical Testing Editor (Start from Scratch)"),
=======
  titlePanel("Interactive Graphical Testing Editor (visNetwork)"),
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  visNetworkOutput("graph"),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  graph_data <- reactiveValues(
    nodes = data.frame(
<<<<<<< HEAD
      id = character(),
      label = character(),
      alpha = numeric(),
      title = character(),
      shape = character(),
      font = I(list()),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = numeric(),
      from = character(),
      to = character(),
      weight = numeric(),
      title = character(),
      label = character(),
=======
      id = 1:5,
      label = c('H1: UPCR IgA', 'H2: eGFR GN', 'H3: eGFR GN 10wk',
                'H4: eGFR IgA', 'H5: 2nd Endpoints'),
      alpha = c(0.01, 0.04, 0, 0, 0),
      title = paste("Alpha:", c(0.01, 0.04, 0, 0, 0)),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = 1:5,
      from = c(1, 2, 4, 4, 5),
      to   = c(4, 4, 2, 5, 2),
      weight = c(1, 1, 0.5, 0.5, 0.5),
      title = paste("Weight:", c(1, 1, 0.5, 0.5, 0.5)),
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
      stringsAsFactors = FALSE
    ),
    node_names = character(),
    node_alpha = numeric(),
<<<<<<< HEAD
    edge_matrix = matrix(0, 0, 0)
  )
  
  graph_changed <- reactiveVal(0)

  update_edge_matrix <- function() {
    ids <- graph_data$nodes$id
    k <- length(ids)
    edge_mat <- matrix(0, k, k)
    rownames(edge_mat) <- ids
    colnames(edge_mat) <- ids
=======
    edge_matrix = NULL
  )
  
  update_edge_matrix <- function() {
    k <- nrow(graph_data$nodes)
    node_ids <- graph_data$nodes$id
    edge_mat <- matrix(0, k, k)
    rownames(edge_mat) <- node_ids
    colnames(edge_mat) <- node_ids
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
    
    for (i in seq_len(nrow(graph_data$edges))) {
      from <- graph_data$edges$from[i]
      to <- graph_data$edges$to[i]
      weight <- graph_data$edges$weight[i]
<<<<<<< HEAD
      if (from %in% rownames(edge_mat) && to %in% colnames(edge_mat)) {
        edge_mat[from, to] <- weight
      }
    }
    graph_data$edge_matrix <- edge_mat
  }
  
  observeEvent(graph_changed(), {
=======
      edge_mat[as.character(from), as.character(to)] <- weight
    }
    
    graph_data$edge_matrix <- edge_mat
  }
  
  observe({
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
    graph_data$node_names <- graph_data$nodes$label
    graph_data$node_alpha <- graph_data$nodes$alpha
    update_edge_matrix()
  })
<<<<<<< HEAD

=======
  
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  output$graph <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(
<<<<<<< HEAD
        manipulation = list(
          enabled = TRUE,
          initiallyActive = TRUE,
          addNode = "function(data, callback) {
            var nodeName = prompt('Enter node label:', 'H');
            if(nodeName != null) {
              data.label = nodeName;
              Shiny.setInputValue('add_node', data, {priority: 'event'});
              callback(data);
            }
          }",
          addEdge = "function(data, callback) {
            Shiny.setInputValue('add_edge', data, {priority: 'event'});
            callback(data);
          }",
          deleteNode = "function(data, callback) {
            Shiny.setInputValue('delete_node', data.nodes, {priority: 'event'});
            callback(data);
          }",
          deleteEdge = "function(data, callback) {
            Shiny.setInputValue('delete_edge', data.edges, {priority: 'event'});
            callback(data);
          }"
        ),
=======
        manipulation = TRUE,
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
        highlightNearest = FALSE,
        nodesIdSelection = FALSE
      ) %>%
      visInteraction(selectConnectedEdges = FALSE) %>%
      visEvents(
        doubleClick = "function(properties) {
          if(properties.nodes.length > 0){
            Shiny.setInputValue('node_dblclick', properties.nodes[0], {priority: 'event'});
          } else if(properties.edges.length > 0){
            Shiny.setInputValue('edge_dblclick', properties.edges[0], {priority: 'event'});
          }
        }"
      )
  })
  
<<<<<<< HEAD
  observeEvent(input$add_node, {
    new_id <- input$add_node$id
    new_label <- input$add_node$label
    
    graph_data$nodes <- rbind(
      graph_data$nodes,
      data.frame(
        id = new_id,
        label = new_label,
        alpha = 0,
        title = "Alpha: 0",
        shape = "ellipse",
        font = I(list(list(valign = "middle", color = "black"))),
        stringsAsFactors = FALSE
      )
    )
    graph_changed(graph_changed() + 1)
  })
  
  observeEvent(input$add_edge, {
    new_id <- if (nrow(graph_data$edges) == 0) 1 else max(graph_data$edges$id) + 1
    graph_data$edges <- rbind(
      graph_data$edges,
      data.frame(
        id = new_id,
        from = input$add_edge$from,
        to = input$add_edge$to,
        weight = 1,
        title = "Weight: 1",
        label = "1",
        stringsAsFactors = FALSE
      )
    )
    graph_changed(graph_changed() + 1)
  })

=======
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  observeEvent(input$node_dblclick, {
    node_id <- input$node_dblclick
    node <- graph_data$nodes[graph_data$nodes$id == node_id, ]
    
    showModal(modalDialog(
<<<<<<< HEAD
      title = paste("Edit Node Alpha", node$label),
=======
      title = paste("Edit Node Alpha", node_id),
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
      numericInput("edit_alpha", "Alpha (0-1):", value = node$alpha, min = 0, max = 1, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit_node", "Save")
      )
    ))
  })
<<<<<<< HEAD

=======
  
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  observeEvent(input$save_edit_node, {
    req(input$node_dblclick)
    node_id <- input$node_dblclick
    new_alpha <- input$edit_alpha
    
    graph_data$nodes[graph_data$nodes$id == node_id, "alpha"] <- new_alpha
    graph_data$nodes[graph_data$nodes$id == node_id, "title"] <- paste("Alpha:", new_alpha)
    
<<<<<<< HEAD
    graph_changed(graph_changed() + 1)
    removeModal()
  })

=======
    removeModal()
  })
  
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  observeEvent(input$edge_dblclick, {
    edge_id <- input$edge_dblclick
    edge <- graph_data$edges[graph_data$edges$id == edge_id, ]
    
    showModal(modalDialog(
      title = paste("Edit Edge", edge_id),
      numericInput("edit_weight", "Weight:", value = edge$weight, min = 0, step = 0.1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit_edge", "Save")
      )
    ))
  })
<<<<<<< HEAD

=======
  
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  observeEvent(input$save_edit_edge, {
    req(input$edge_dblclick)
    edge_id <- input$edge_dblclick
    new_weight <- input$edit_weight
    
<<<<<<< HEAD
    if (edge_id %in% graph_data$edges$id) {
      graph_data$edges[graph_data$edges$id == edge_id, "weight"] <- new_weight
      graph_data$edges[graph_data$edges$id == edge_id, "title"] <- paste("Weight:", new_weight)
      graph_data$edges[graph_data$edges$id == edge_id, "label"] <- as.character(new_weight)
      
      graph_changed(graph_changed() + 1)
    } else {
      showNotification("Edge ID not found. Could not update weight.", type = "error")
    }
    
    removeModal()
  })

  observeEvent(input$delete_node, {
    ids_to_delete <- input$delete_node
    graph_data$nodes <- subset(graph_data$nodes, !(id %in% ids_to_delete))
    graph_data$edges <- subset(graph_data$edges, !(from %in% ids_to_delete | to %in% ids_to_delete))
    graph_changed(graph_changed() + 1)
  })

  observeEvent(input$delete_edge, {
    ids_to_delete <- input$delete_edge
    graph_data$edges <- subset(graph_data$edges, !(id %in% ids_to_delete))
    graph_changed(graph_changed() + 1)
  })

=======
    graph_data$edges[graph_data$edges$id == edge_id, "weight"] <- new_weight
    graph_data$edges[graph_data$edges$id == edge_id, "title"] <- paste("Weight:", new_weight)
    
    update_edge_matrix()
    removeModal()
  })
  
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
  output$debug <- renderPrint({
    list(
      node_names = graph_data$node_names,
      node_alpha = graph_data$node_alpha,
      edge_matrix = graph_data$edge_matrix
    )
  })
}

shinyApp(ui, server)
<<<<<<< HEAD



=======
>>>>>>> d64e534144b9dba90b7a9a04d3d04fc3d097544c
