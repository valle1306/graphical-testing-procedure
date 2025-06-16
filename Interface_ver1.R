library(shiny)
library(visNetwork)
library(htmlwidgets)

# === UI ===
ui <- fluidPage(
  titlePanel("Interactive Graphical Testing Editor (visNetwork)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_node", "Add Node"),
      actionButton("delete_selected", "Delete Selected Node/Edge"),
      numericInput("alpha_input", "Edit Alpha of Selected Node:", value = 0.01, step = 0.01),
      actionButton("update_alpha", "Update Alpha"),
      actionButton("reject_hypothesis", "Reject Selected Hypothesis (Redistribute α)"),
      hr(),
      numericInput("edge_weight_input", "Edit Weight of Selected Edge:", value = 1, step = 0.1),
      actionButton("update_edge_weight", "Update Edge Weight"),
      hr(),
      numericInput("new_edge_from", "New Edge: From Node ID", value = 1, step = 1),
      numericInput("new_edge_to", "New Edge: To Node ID", value = 2, step = 1),
      numericInput("new_edge_weight", "New Edge: Weight", value = 1, step = 0.001),
      actionButton("add_edge", "Add Edge"),
      hr(),
      verbatimTextOutput("graph_summary")
    ),
    mainPanel(
      visNetworkOutput("graph", height = "600px")
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # Alpha redistribution logic
  redistribute_alpha <- function(alphas, remove_id) {
    if (!remove_id %in% names(alphas)) return(alphas)
    removed_alpha <- alphas[[as.character(remove_id)]]
    remaining_ids <- setdiff(names(alphas), as.character(remove_id))
    remaining_alphas <- alphas[remaining_ids]
    total <- sum(remaining_alphas)
    if (total == 0) {
      # Uniform redistribution
      new_alphas <- rep(removed_alpha / length(remaining_ids), length(remaining_ids))
    } else {
      # Proportional redistribution
      new_alphas <- remaining_alphas + (remaining_alphas / total) * removed_alpha
    }
    names(new_alphas) <- remaining_ids
    return(new_alphas)
  }
  
  # Initialize nodes
  nodes <- reactiveVal(
    data.frame(
      id = 1:5,
      label = c('H1: UPCR IgA', 'H2: eGFR GN', 'H3: eGFR GN 10wk',
                'H4: eGFR IgA', 'H5: 2nd Endpoints'),
      alpha = c(0.01, 0.04, 0, 0, 0),
      title = paste0("\u03B1 = ", c(0.01, 0.04, 0, 0, 0)),
      stringsAsFactors = FALSE
    )
  )
  
  # Initialize edges
  edges <- reactiveVal(
    data.frame(
      from = c(1, 2, 4, 4, 5),
      to   = c(4, 4, 2, 5, 2),
      label = c("1", "1", "0.5", "0.5", "0.5"),
      arrows = "to",
      stringsAsFactors = FALSE
    )
  )
  
  # Render visNetwork graph
  output$graph <- renderVisNetwork({
    visNetwork(nodes(), edges()) %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visNodes(shape = "ellipse") %>%
      visEdges(smooth = TRUE) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE,
        manipulation = TRUE
      )
  })
  
  # Add node
  observeEvent(input$add_node, {
    current <- nodes()
    new_id <- ifelse(nrow(current) == 0, 1, max(current$id) + 1)
    new_node <- data.frame(
      id = new_id,
      label = paste0("H", new_id, ": New"),
      alpha = 0,
      title = "α = 0",
      stringsAsFactors = FALSE
    )
    nodes(rbind(current, new_node))
  })
  
  # Delete node or edge
  observeEvent(input$delete_selected, {
    req(input$graph_selected | input$graph_selectedEdges)
    current_nodes <- nodes()
    current_edges <- edges()
    
    if (!is.null(input$graph_selected)) {
      sel_nodes <- input$graph_selected
      current_nodes <- current_nodes[!(current_nodes$id %in% sel_nodes), ]
      current_edges <- current_edges[!(current_edges$from %in% sel_nodes | current_edges$to %in% sel_nodes), ]
    }
    if (!is.null(input$graph_selectedEdges)) {
      sel_edges <- input$graph_selectedEdges
      current_edges <- current_edges[!(paste0(current_edges$from, "-", current_edges$to) %in% sel_edges), ]
    }
    
    nodes(current_nodes)
    edges(current_edges)
  })
  
  # Update alpha
  observeEvent(input$update_alpha, {
    req(input$graph_selected)
    sel <- input$graph_selected[1]
    current <- nodes()
    if (sel %in% current$id) {
      current[current$id == sel, "alpha"] <- input$alpha_input
      current[current$id == sel, "title"] <- paste0("\u03B1 = ", input$alpha_input)
      nodes(current)
    }
  })
  
  # Reject hypothesis and redistribute α
  observeEvent(input$reject_hypothesis, {
    req(input$graph_selected)
    sel <- input$graph_selected[1]
    current <- nodes()
    
    alphas <- setNames(current$alpha, current$id)
    new_alphas <- redistribute_alpha(alphas, sel)
    
    updated_nodes <- current[current$id != sel, ]
    updated_nodes$alpha <- as.numeric(new_alphas[as.character(updated_nodes$id)])
    updated_nodes$title <- paste0("α = ", updated_nodes$alpha)
    
    updated_edges <- edges()
    updated_edges <- updated_edges[!(updated_edges$from == sel | updated_edges$to == sel), ]
    
    nodes(updated_nodes)
    edges(updated_edges)
  })
  
  # Update edge weight
  observeEvent(input$update_edge_weight, {
    req(input$graph_selectedEdges)
    sel_edge <- input$graph_selectedEdges[1]
    current_edges <- edges()
    edge_ids <- paste0(current_edges$from, "-", current_edges$to)
    idx <- which(edge_ids == sel_edge)
    if (length(idx) > 0) {
      current_edges$label[idx] <- as.character(input$edge_weight_input)
      edges(current_edges)
    }
  })
  
  # Add edge manually
  observeEvent(input$add_edge, {
    current_edges <- isolate(edges())  # ensures fresh state
    new_edge <- data.frame(
      from = input$new_edge_from,
      to = input$new_edge_to,
      label = as.character(input$new_edge_weight),
      arrows = "to",
      stringsAsFactors = FALSE
    )
    edges(rbind(current_edges, new_edge))
  })
  
  
  # Print graph summary
  output$graph_summary <- renderPrint({
    node_df <- nodes()
    edge_df <- edges()
    
    n <- nrow(node_df)
    transition <- matrix(0, nrow = n, ncol = n)
    rownames(transition) <- colnames(transition) <- node_df$label
    
    for (i in seq_len(nrow(edge_df))) {
      from_id <- edge_df$from[i]
      to_id <- edge_df$to[i]
      weight <- as.numeric(edge_df$label[i])
      if (!is.na(weight)) {
        row_name <- node_df$label[match(from_id, node_df$id)]
        col_name <- node_df$label[match(to_id, node_df$id)]
        transition[row_name, col_name] <- weight
      }
    }
    
    list(
      node_names = node_df$label,
      alpha_values = node_df$alpha,
      transition_matrix = transition
    )
  })
}

# === LAUNCH ===
shinyApp(ui, server)
