library(shiny)
library(visNetwork)
library(DT)
library(jsonlite)
library(dplyr)
library(TrialSimulator)

ui <- navbarPage(
  "My Application",
  
  # ----------- HOME -----------
  tabPanel(
    "Home",
    fluidPage(
      tags$head(
        tags$style(HTML("
        .feature-card {
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 20px;
          background-color: #f8f9fa;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .feature-title {
          font-size: 20px;
          font-weight: bold;
          margin-bottom: 8px;
          color: #007bff;
        }
        .feature-text {
          font-size: 16px;
          color: #444;
        }
      "))
      ),
      # Title and Image
      fluidRow(
        column(12, align = "center",
               tags$img(src = "graphical_testing_image.png", height = "180px"),
               tags$h2("Graphical Approach for Multiple Testing Procedure"),
               tags$p("An interactive tool for visualizing and designing multiple testing strategies in clinical trials.")
        )
      ),
      br(),
      # New Introduction Section
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h4("🧬 Introduction"),
                        tags$p("With the rise of innovative trial designs—such as adaptive and platform studies—clinical researchers and statisticians face growing complexity in managing multiple hypotheses while preserving statistical rigor. Traditional fixed-sequence procedures often fall short in these evolving contexts."),
                        tags$p("Graphical Approach for Multiple Testing Procedure is a visual and interactive tool built to meet this challenge. Grounded in the graphical methodology pioneered by Bretz et al., it offers a transparent way to design, test, and dynamically redistribute alpha levels across networks of hypotheses."),
                        tags$p("This app was developed in collaboration with methodologists, researchers, and developers committed to making advanced statistical design accessible, reproducible, and human-centered. While still evolving, it is built on the open-source TrialSimulator R package and maintained by a volunteer team.")
               )
        )
      ),
      # Key Features Cards
      fluidRow(
        column(4,
               div(class = "feature-card",
                   div(class = "feature-title", "📊 Visualize Hypotheses"),
                   div(class = "feature-text", "Draw, connect, and edit null hypotheses with intuitive graphs.")
               )
        ),
        column(4,
               div(class = "feature-card",
                   div(class = "feature-title", "🧮 Allocate Alpha Dynamically"),
                   div(class = "feature-text", "Create and run testing procedures using dynamic α-spending rules.")
               )
        ),
        column(4,
               div(class = "feature-card",
                   div(class = "feature-title", "📁 Import & Export"),
                   div(class = "feature-text", "Upload saved JSON graphs or export your design to share with collaborators.")
               )
        )
      ),
      # References
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h4("📘 References"),
                        tags$ul(
                          tags$li(
                            tags$span("Bretz F., Maurer W., Brannath W., Posch M. (2009). "),
                            tags$i("A graphical approach to sequentially rejective multiple testing procedures."),
                            " Stat Med 28(4): 586–604. ",
                            tags$a(href = "https://doi.org/10.1002/sim.3495", "https://doi.org/10.1002/sim.3495")
                          ),
                          tags$li(
                            tags$span("TrialSimulator R package: "),
                            tags$a(href = "https://zhangh12.github.io/TrialSimulator/", 
                                   "zhangh12.github.io/TrialSimulator/")
                          )
                        )
               )
        )
      ),
      # Footer
      fluidRow(
        column(12, align = "center",
               tags$hr(),
               tags$p("Developed using the TrialSimulator package for modular and reproducible graphical testing design.")
        )
      )
    )
  ),
  
  # ----------- GRAPHICAL TEST -----------
  tabPanel(
    "Graphical Test",
    fluidPage(
      titlePanel("Graphical Approach for Multiple Testing Procedure"),
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
                 fileInput("upload", "Upload JSON Graph File"),
                 downloadButton("export", "Export")
               ),
               wellPanel(
                 h5(tags$b("Node Table"), style = "color:blue"),
                 DT::dataTableOutput("node_table"),
                 tags$hr(),
                 h5(tags$b("Edge Table"), style = "color:darkgreen"),
                 DT::dataTableOutput("edge_table")
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
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    nodes = data.frame(label = character(0), alpha = numeric(0), title = character(0), id = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = character(0), to = character(0), weight = numeric(0), label = character(0), 
                       smooth = I(list()), id = character(0), stringsAsFactors = FALSE),
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
    create_graphicaltesting_objects()
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
  
  # Double click to edit node & edge
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
  
  # Confirm the edit of node & edge
  observeEvent(input$confirm_edit_node_dblclk, {
    node_label <- session$userData$editing_node_label
    idx <- which(rv$nodes$label == node_label)
    if (length(idx) == 1) {
      old_label <- node_label
      new_label <- input$edit_node_label
      
      rv$nodes$label[idx] <- new_label
      rv$nodes$alpha[idx] <- input$edit_node_alpha
      rv$nodes$title[idx] <- paste0("α = ", format(input$edit_node_alpha, nsmall = 2))
      rv$nodes$id[idx] <- new_label
      
      # Update edges referencing old label to new label, if label was changed
      if (old_label != new_label) {
        rv$edges$from[rv$edges$from == old_label] <- new_label
        rv$edges$to[rv$edges$to == old_label] <- new_label
        # Update edge IDs too
        rv$edges$id <- paste0(rv$edges$from, "->", rv$edges$to)
      }
      
      # Update dropdown choices
      updateSelectInput(session, "graph_selected", choices = rv$nodes$label)
      
      # Update GT objects and reset GT state
      create_graphicaltesting_objects()
      rv$gt_object <- NULL
      rv$gt_log <- ""
      rv$gt_summary <- NULL
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
      
      # Update GT objects and reset GT state
      create_graphicaltesting_objects()
      rv$gt_object <- NULL
      rv$gt_log <- ""
      rv$gt_summary <- NULL
    }
    removeModal()
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
      rv$edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), label = character(0), 
                             smooth = I(list()), id = character(0), stringsAsFactors = FALSE)
    }
    create_graphicaltesting_objects()
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
    if (nrow(rv$edges) == 0) return(data.frame())
    cols <- intersect(c("from", "to", "weight"), colnames(rv$edges))
    if (length(cols) == 0) return(data.frame())
    rv$edges[, cols, drop = FALSE]
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