library(shiny)
library(visNetwork)

ui <- fluidPage(
  titlePanel("TrialSimulator_graphical_testing"),
  visNetworkOutput("graph"),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  graph_data <- reactiveValues(
    nodes = data.frame(
      id = numeric(),
      label = character(),
      alpha = numeric(),
      title = character(),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = numeric(),
      from = numeric(),
      to = numeric(),
      weight = numeric(),
      title = character(),
      stringsAsFactors = FALSE
    ),
    node_names = character(),
    node_alpha = numeric(),
    edge_matrix = NULL
  )
  
  update_edge_matrix <- function() {
    k <- nrow(graph_data$nodes)
    node_ids <- graph_data$nodes$id
    edge_mat <- matrix(0, k, k)
    rownames(edge_mat) <- node_ids
    colnames(edge_mat) <- node_ids
    
    for (i in seq_len(nrow(graph_data$edges))) {
      from <- graph_data$edges$from[i]
      to <- graph_data$edges$to[i]
      weight <- graph_data$edges$weight[i]
      edge_mat[as.character(from), as.character(to)] <- weight
    }
    
    graph_data$edge_matrix <- edge_mat
  }
  
  observe({
    graph_data$node_names <- graph_data$nodes$label
    graph_data$node_alpha <- graph_data$nodes$alpha
    update_edge_matrix()
  })
  
  output$graph <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(
        manipulation = list(
          enabled = TRUE,
          initiallyActive = TRUE,
          addNode = function(nodeData, callback) {
            node_id <- if (nrow(graph_data$nodes) == 0) 1 else max(graph_data$nodes$id) + 1
            label <- paste("Node", node_id)
            alpha <- 0
            title <- paste("Alpha:", alpha)
            
            new_node <- data.frame(id = node_id, label = label, alpha = alpha, title = title, stringsAsFactors = FALSE)
            graph_data$nodes <- rbind(graph_data$nodes, new_node)
            callback(new_node)
          },
          addEdge = function(edgeData, callback) {
            edge_id <- if (nrow(graph_data$edges) == 0) 1 else max(graph_data$edges$id) + 1
            weight <- 1
            title <- paste("Weight:", weight)
            
            new_edge <- data.frame(id = edge_id, from = edgeData$from, to = edgeData$to, weight = weight, title = title, stringsAsFactors = FALSE)
            graph_data$edges <- rbind(graph_data$edges, new_edge)
            update_edge_matrix()
            callback(new_edge)
          },
          deleteNode = FALSE,
          deleteEdge = FALSE,
          editNode = FALSE,
          editEdge = FALSE
        ),
        highlightNearest = FALSE,
        nodesIdSelection = FALSE
      ) %>%
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        selectConnectedEdges = FALSE
      ) %>%
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
  
  observeEvent(input$node_dblclick, {
    node_id <- input$node_dblclick
    node <- graph_data$nodes[graph_data$nodes$id == node_id, ]
    
    showModal(modalDialog(
      title = paste("Edit Node Alpha", node_id),
      numericInput("edit_alpha", "Alpha (0-1):", value = node$alpha, min = 0, max = 1, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit_node", "Save")
      )
    ))
  })
  
  observeEvent(input$save_edit_node, {
    req(input$node_dblclick)
    node_id <- input$node_dblclick
    new_alpha <- input$edit_alpha
    
    graph_data$nodes[graph_data$nodes$id == node_id, "alpha"] <- new_alpha
    graph_data$nodes[graph_data$nodes$id == node_id, "title"] <- paste("Alpha:", new_alpha)
    
    removeModal()
  })
  
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
  
  observeEvent(input$save_edit_edge, {
    req(input$edge_dblclick)
    edge_id <- input$edge_dblclick
    new_weight <- input$edit_weight
    
    graph_data$edges[graph_data$edges$id == edge_id, "weight"] <- new_weight
    graph_data$edges[graph_data$edges$id == edge_id, "title"] <- paste("Weight:", new_weight)
    
    update_edge_matrix()
    removeModal()
  })
  
  output$debug <- renderPrint({
    list(
      node_names = graph_data$node_names,
      node_alpha = graph_data$node_alpha,
      edge_matrix = graph_data$edge_matrix
    )
  })
}

shinyApp(ui, server)