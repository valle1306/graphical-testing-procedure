library(shiny)
library(visNetwork)
library(DT)
library(jsonlite)
library(TrialSimulator)

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
                      actionButton("run_gt", "Run Graphical Test", class = "btn btn-warning"),
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

