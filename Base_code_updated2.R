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
           actionButton("edit_node", "Edit Node", class = "btn btn-warning"),
           actionButton("edit_edge", "Edit Edge", class = "btn btn-info"),
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
    nodes = data.frame(id = numeric(0), label = character(0), alpha = numeric(0), stringsAsFactors = FALSE),
    edges = data.frame(id = numeric(0), from = numeric(0), to = numeric(0), weight = numeric(0), label = character(0), smooth = I(list()), stringsAsFactors = FALSE),
    node_id = 1,
    edge_id = 1
  )
  
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Add Node",
      textInput("node_label", "Label (e.g., H1: abc)", ""),
      numericInput("node_alpha", "Alpha", value = 0.05, step = 0.01),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_node", "Add")
      )
    ))
  })
  
  observeEvent(input$confirm_add_node, {
    new_node <- data.frame(
      id = rv$node_id,
      label = input$node_label,
      alpha = input$node_alpha,
      title = paste0("α = ", format(input$node_alpha, nsmall = 2))
    )
    rv$nodes <- rbind(rv$nodes, new_node)
    rv$node_id <- rv$node_id + 1
    removeModal()
  })
  
  observeEvent(input$add_edge, {
    showModal(modalDialog(
      title = "Add Edge",
      numericInput("edge_from", "From Node ID", value = 1),
      numericInput("edge_to", "To Node ID", value = 2),
      numericInput("edge_weight", "Weight", value = 0.05, step = 0.01),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_edge", "Add")
      )
    ))
  })
  
  observeEvent(input$confirm_add_edge, {
    existing_reverse <- with(rv$edges, which(from == input$edge_to & to == input$edge_from))
    curvature_type <- if (length(existing_reverse) > 0) {
      list(enabled = TRUE, type = "curvedCW")
    } else {
      list(enabled = FALSE)
    }
    
    new_edge <- data.frame(
      id = rv$edge_id,
      from = input$edge_from,
      to = input$edge_to,
      weight = input$edge_weight,
      label = as.character(input$edge_weight),
      smooth = I(list(curvature_type))
    )
    rv$edges <- rbind(rv$edges, new_edge)
    rv$edge_id <- rv$edge_id + 1
    removeModal()
  })
  
  observeEvent(input$edit_edge, {
    selected_edge <- input$graph_selectedEdges
    if (!is.null(selected_edge)) {
      edge_id <- as.numeric(selected_edge[1])
      edge_row <- rv$edges[rv$edges$id == edge_id, ]
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
        session$userData$editing_edge_id <- edge_id
      }
    }
  })
  
  observeEvent(input$confirm_edit_edge, {
    edge_id <- session$userData$editing_edge_id
    idx <- which(rv$edges$id == edge_id)
    if (length(idx) == 1) {
      rv$edges$weight[idx] <- input$edit_edge_weight
      rv$edges$label[idx] <- as.character(input$edit_edge_weight)
    }
    removeModal()
  })
  
  observeEvent(input$edit_node, {
    showModal(modalDialog(
      title = "Edit Node Alpha",
      numericInput("edit_node_id", "Node ID", value = 1),
      numericInput("edit_alpha", "New Alpha", value = 0.05, step = 0.01),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_node", "Update")
      )
    ))
  })
  
  observeEvent(input$confirm_edit_node, {
    node_idx <- which(rv$nodes$id == input$edit_node_id)
    if (length(node_idx) == 1) {
      rv$nodes$alpha[node_idx] <- input$edit_alpha
      rv$nodes$title[node_idx] <- paste0("α = ", format(input$edit_alpha, nsmall = 2))
    }
    removeModal()
  })
  
  observeEvent(input$delete_node, {
    selected_node <- input$graph_selected
    if (!is.null(selected_node)) {
      rv$nodes <- rv$nodes[rv$nodes$id != selected_node, ]
      rv$edges <- rv$edges[!(rv$edges$from == selected_node | rv$edges$to == selected_node), ]
    }
  })
  
  observeEvent(input$delete_edge, {
    selected_edges <- input$graph_selectedEdges
    if (!is.null(selected_edges)) {
      edge_ids <- as.numeric(selected_edges)
      rv$edges <- rv$edges[!rv$edges$id %in% edge_ids, ]
    }
  })
  
  output$graph <- renderVisNetwork({
    visNetwork(rv$nodes, rv$edges) %>%
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
      )
  })
  
  output$node_table <- renderDataTable({
    rv$nodes[, c("id", "label", "alpha")]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
  output$edge_table <- renderDataTable({
    rv$edges[, c("id", "from", "to", "weight")]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
  observeEvent(input$upload, {
    req(input$upload)
    json_data <- fromJSON(input$upload$datapath)
    rv$nodes <- json_data$nodes
    rv$edges <- json_data$edges
    rv$node_id <- ifelse(nrow(rv$nodes) == 0, 1, max(rv$nodes$id) + 1)
    rv$edge_id <- ifelse(nrow(rv$edges) == 0, 1, max(rv$edges$id) + 1)
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







