library(shiny)
library(visNetwork)
library(DT)
library(jsonlite)
library(dplyr)
library(TrialSimulator) 

ui <- fluidPage(
  titlePanel("Interactive Graphical Testing Editor (Start from Scratch)"),
  fluidRow(
    column(12, align = "center",
           actionButton("add_node", "Add Node", class = "btn btn-primary"),
           actionButton("add_edge", "Add Edge", class = "btn btn-success"),
           actionButton("delete_node", "Delete Node", class = "btn btn-danger"),
           actionButton("delete_edge", "Delete Edge", class = "btn btn-danger"),
           actionButton("run_gt", "Create Test Object", class = "btn btn-warning"),
           actionButton("reject_gt", "Reject Selected Hypothesis", class = "btn btn-warning")
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
                      DT::dataTableOutput("node_table"),
                      tags$hr(),
                      h5(tags$b("Edge Table"), style = "color:darkgreen"),
                      DT::dataTableOutput("edge_table")
             )
           )
    ),
    column(9,
           selectInput("graph_selected", "Select hypothesis to reject", choices = NULL),
           uiOutput("main_graph_ui")
    )
  ),
  fluidRow(
    column(12,
           tags$hr(),
           h4(tags$b("Test Results"), style = "color:purple"),
           verbatimTextOutput("gt_log"),
           dataTableOutput("gt_result_table")
    )
  )
  
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    nodes = data.frame(label = character(0), alpha = numeric(0), title = character(0), id = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = character(0), to = character(0), weight = numeric(0), label = character(0), smooth = I(list()), id = character(0), stringsAsFactors = FALSE),
    gt_object = NULL,
    gt_log = "",
    gt_summary = NULL,
    alpha = numeric(0),
    hypotheses = character(0),
    alpha_spending = character(0),
    planned_max_info = numeric(0),
    transition = matrix(0, 0, 0)
  )
  
  output$main_graph_ui <- renderUI({
    if (is.null(rv$gt_object)) {
      visNetworkOutput("graph", height = "600px")
    } else {
      plotOutput("gt_plot", height = "600px")
    }
  })
  
  # Helper for transition matrix
  get_transition_matrix <- function(nodes, edges) {
    n <- nrow(nodes)
    mat <- matrix(0, n, n)
    rownames(mat) <- colnames(mat) <- nodes$label
    for (i in seq_len(nrow(edges))) {
      from <- edges$from[i]
      to <- edges$to[i]
      weight <- edges$weight[i]
      mat[from, to] <- weight
    }
    return(mat)
  }
  
  # --------- Node/Edge Editing -----------
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Add Node",
      textInput("node_label", "Label (e.g., H1: abc)", ""),
      numericInput("node_alpha", "Alpha", value = "", min = 0, max = 1, step = 0.01),
      easyClose = FALSE,
      footer = tagList(modalButton("Cancel"), actionButton("confirm_add_node", "Add"))
    ))
  })
  observeEvent(input$confirm_add_node, {
    req(input$node_label)
    if(input$node_label %in% rv$nodes$label) {
      showModal(modalDialog("Node label must be unique!"))
      return()
    }
    new_node <- data.frame(label=input$node_label, alpha=input$node_alpha,
                           title=paste0("α = ", format(input$node_alpha, nsmall = 2)),
                           id = input$node_label,
                           stringsAsFactors=FALSE)
    rv$nodes <- rbind(rv$nodes, new_node)
    updateSelectInput(session, "graph_selected", choices = rv$nodes$label)
    removeModal()
    # Update five objects for GT
    create_graphicaltesting_objects()
    # On edit, clear previous GT object and results
    rv$gt_object <- NULL
    rv$gt_log <- ""
    rv$gt_summary <- NULL
  })
  observeEvent(input$add_edge, {
    if(nrow(rv$nodes) < 2) {
      showModal(modalDialog("You need at least two nodes to create an edge.", easyClose=TRUE))
      return()
    }
    showModal(modalDialog(
      title = "Add Edge",
      selectInput("edge_from", "From Node", choices = rv$nodes$label),
      selectInput("edge_to", "To Node", choices = rv$nodes$label, selected=rv$nodes$label[2]),
      numericInput("edge_weight", "Weight", value = 0.5, min=0, max=1, step=0.01),
      easyClose = FALSE,
      footer = tagList(modalButton("Cancel"), actionButton("confirm_add_edge", "Add"))
    ))
  })
  observeEvent(input$confirm_add_edge, {
    if (is.null(input$edge_from) || is.null(input$edge_to)) return()
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
      id = paste0(input$edge_from, "->", input$edge_to),
      stringsAsFactors = FALSE
    )
    rv$edges <- rbind(rv$edges, new_edge)
    removeModal()
    create_graphicaltesting_objects()
    rv$gt_object <- NULL
    rv$gt_log <- ""
    rv$gt_summary <- NULL
  })
  observeEvent(input$delete_node, {
    selected_node <- input$graph_selected
    if (!is.null(selected_node)) {
      sel_label <- selected_node
      rv$nodes <- rv$nodes[rv$nodes$label != sel_label, ]
      rv$edges <- rv$edges[!(rv$edges$from == sel_label | rv$edges$to == sel_label), ]
      updateSelectInput(session, "graph_selected", choices = rv$nodes$label)
      create_graphicaltesting_objects()
      rv$gt_object <- NULL
      rv$gt_log <- ""
      rv$gt_summary <- NULL
    }
  })
  observeEvent(input$delete_edge, {
    selected_edges <- input$graph_selectedEdges
    if (!is.null(selected_edges)) {
      for (edge in selected_edges) {
        fromto <- strsplit(edge, "->")[[1]]
        if(length(fromto)==2){
          from <- fromto[1]
          to <- fromto[2]
          rv$edges <- rv$edges[!(rv$edges$from == from & rv$edges$to == to), ]
        }
      }
      create_graphicaltesting_objects()
      rv$gt_object <- NULL
      rv$gt_log <- ""
      rv$gt_summary <- NULL
    }
  })
  
  # ----------- File import/export -----------
  observeEvent(input$upload, {
    req(input$upload)
    json_data <- fromJSON(input$upload$datapath)
    # --- Nodes ---
    if (!is.null(json_data$nodes) && is.data.frame(json_data$nodes)) {
      raw_nodes <- json_data$nodes
      cleaned_nodes <- data.frame(
        label = as.character(raw_nodes$label),
        alpha = as.numeric(raw_nodes$alpha),
        title = paste0("α = ", format(raw_nodes$alpha, nsmall = 2)),
        id = as.character(raw_nodes$label),
        stringsAsFactors = FALSE
      )
      rv$nodes <- cleaned_nodes
      updateSelectInput(session, "graph_selected", choices = rv$nodes$label)
    } else {
      rv$nodes <- data.frame(label = character(0), alpha = numeric(0), title = character(0), id = character(0), stringsAsFactors = FALSE)
    }
    # --- Edges ---
    if (!is.null(json_data$edges) && is.data.frame(json_data$edges)) {
      edges <- json_data$edges
      if (!"label" %in% colnames(edges)) edges$label <- as.character(edges$weight)
      if (!"smooth" %in% colnames(edges)) edges$smooth <- replicate(nrow(edges), list(list(enabled = FALSE)), simplify = FALSE)
      edges$id <- paste0(edges$from, "->", edges$to)
      rv$edges <- edges
    } else {
      rv$edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), label = character(0), smooth = I(list()), id = character(0), stringsAsFactors = FALSE)
    }
    create_graphicaltesting_objects()
    # --- NEW: Clear and re-create GT object for new graph
    rv$gt_object <- NULL
    rv$gt_log <- ""
    rv$gt_summary <- NULL
    if (length(rv$alpha) > 0) {
      tryCatch({
        log_lines <- NULL
        log_message <- function(m) { log_lines <<- c(log_lines, conditionMessage(m)); invokeRestart("muffleMessage") }
        withCallingHandlers(
          {
            rv$gt_object <- GraphicalTesting$new(
              alpha = rv$alpha,
              transition = rv$transition,
              alpha_spending = rv$alpha_spending,
              planned_max_info = rv$planned_max_info,
              hypotheses = rv$hypotheses,
              silent = FALSE
            )
          }, 
          message = log_message
        )
        rv$gt_log <- paste(log_lines, collapse = "\n")
        rv$gt_summary <- rv$gt_object$get_current_testing_results()
        output$gt_plot <- renderPlot({ print(rv$gt_object) })
      }, error = function(e) {
        rv$gt_log <- paste("Error during GraphicalTesting setup:", e$message)
        rv$gt_object <- NULL
        rv$gt_summary <- NULL
      })
    }
  })
  
  output$export <- downloadHandler(
    filename = function() { paste0("graph-", Sys.Date(), ".json") },
    content = function(file) {
      export_nodes <- rv$nodes
      export_edges <- rv$edges
      if ("smooth" %in% colnames(export_edges)) {
        export_edges$smooth <- lapply(export_edges$smooth, function(x) {
          if (is.null(x) || length(x) == 0) list(enabled = FALSE) else x
        })
      }
      write_json(list(nodes = export_nodes, edges = export_edges),
                 path = file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  
  # --------- Helper for updating GT objects ---------
  create_graphicaltesting_objects <- function() {
    rv$alpha <- as.numeric(rv$nodes$alpha)
    rv$hypotheses <- as.character(rv$nodes$label)
    rv$alpha_spending <- rep("asOF", length(rv$alpha))
    rv$planned_max_info <- rep(100, length(rv$alpha))
    # Transition matrix
    n <- nrow(rv$nodes)
    mat <- matrix(0, n, n)
    rownames(mat) <- colnames(mat) <- rv$nodes$label
    if (nrow(rv$edges) > 0) {
      for (i in seq_len(nrow(rv$edges))) {
        from <- as.character(rv$edges$from[i])
        to <- as.character(rv$edges$to[i])
        w <- as.numeric(rv$edges$weight[i])
        mat[from, to] <- w
      }
    }
    rv$transition <- mat
    invisible(NULL)
  }
  
  # --------- Graph ---------
  output$graph <- renderVisNetwork({
    gnodes <- rv$nodes; gnodes$id <- gnodes$label
    gedges <- rv$edges
    gedges$id <- if(nrow(gedges)>0) paste0(gedges$from, "->", gedges$to) else character(0)
    visNetwork(gnodes, gedges) %>%
      visNodes(
        shape = "ellipse",
        color = list(background = "lightblue", border = "blue"),
        font = list(size = 20), shadow = TRUE
      ) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.6)),
        smooth = TRUE,
        font = list(align = "middle", size = 16),
        color = list(color = "blue")
      ) %>%
      visOptions(
        highlightNearest = TRUE,
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
        dragNodes = TRUE, dragView = TRUE, hover = TRUE
      ) %>%
      visEvents(
        doubleClick = "function(params) { Shiny.setInputValue('graph_doubleClick', params, {priority: 'event'}); }",
        select = "function(nodes) {
            Shiny.setInputValue('graph_selected', nodes.nodes.length > 0 ? nodes.nodes[0] : null);
            Shiny.setInputValue('graph_selectedEdges', nodes.edges);
        }"
      )
  })
  
  output$node_table <- DT::renderDataTable({
    req(nrow(rv$nodes) > 0)
    cols <- intersect(c("label", "alpha"), colnames(rv$nodes))
    if (length(cols) == 0) return(data.frame())  # Show nothing if missing expected cols
    DT::datatable(
      rv$nodes[, cols, drop = FALSE],
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
  
  output$edge_table <- renderDataTable({
    rv$edges[, c("from", "to", "weight")]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  
  # ----------GraphicalTesting integration----------------
  observeEvent(input$run_gt, {
    req(length(rv$alpha) == length(rv$hypotheses), nrow(rv$transition) == length(rv$alpha))
    tryCatch({
      rv$gt_object <- NULL
      log_lines <- NULL
      log_message <- function(m) { log_lines <<- c(log_lines, conditionMessage(m)); invokeRestart("muffleMessage") }
      withCallingHandlers(
        {
          rv$gt_object <- GraphicalTesting$new(
            alpha = rv$alpha,
            transition = rv$transition,
            alpha_spending = rv$alpha_spending,
            planned_max_info = rv$planned_max_info,
            hypotheses = rv$hypotheses,
            silent = FALSE
          )
        }, 
        message = log_message
      )
      rv$gt_log <- paste(log_lines, collapse = "\n")
      rv$gt_summary <- rv$gt_object$get_current_testing_results()
      output$gt_plot <- renderPlot({ print(rv$gt_object) })
    }, error = function(e) {
      rv$gt_log <- paste("Error during GraphicalTesting setup:", e$message)
      rv$gt_object <- NULL
      rv$gt_summary <- NULL
    })
  })
  observeEvent(input$reject_gt, {
    req(rv$gt_object, input$graph_selected)
    tryCatch({
      log_lines <- NULL
      log_message <- function(m) { log_lines <<- c(log_lines, conditionMessage(m)); invokeRestart("muffleMessage") }
      withCallingHandlers(
        rv$gt_object$reject_a_hypothesis(input$graph_selected),
        message=log_message
      )
      rv$gt_log <- paste(log_lines, collapse = "\n")
      rv$gt_summary <- rv$gt_object$get_current_testing_results()
      output$gt_plot <- renderPlot({ print(rv$gt_object) })
    }, error = function(e) {
      rv$gt_log <- paste("Reject error:", e$message)
    })
  })
  output$gt_result_table <- renderDataTable({
    req(rv$gt_summary)
    rv$gt_summary
  })
  output$gt_log <- renderText({
    rv$gt_log
  })
}

shinyApp(ui, server)