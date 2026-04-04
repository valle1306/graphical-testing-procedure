build_home_tab <- function() {
  tabPanel(
    "Home",
    fluidPage(
      tags$head(
        tags$style(HTML("
        .feature-card {
          border-radius: 10px;
          padding: 10px 12px;   /* was 20px */
          margin-bottom: 12px;  /* was 20px */
          background-color: #f8f9fa;
          box-shadow: 0 1px 2px rgba(0,0,0,0.08);
        }
        .feature-title {
          font-size: 16px;      /* was 20px */
          margin-bottom: 4px;
        }
        .feature-text {
          font-size: 13px;      /* was 16px */
          line-height: 1.35;
        }
        
        /* Hero header */
        .hero {
          display: flex;
          align-items: center;         /* vertical centering */
          justify-content: center;     /* center on the page */
          gap: 32px;
          margin: 12px 0 8px 0;
        }
        .landing-logo {
          height: 220px;               /* bigger than before (was 180px) */
          max-width: 100%;
          border-radius: 16px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
          object-fit: contain;
          display: block;
        }
        .brand { text-align: left; max-width: 780px; }
        .app-title { margin: 0 0 6px 0; font-weight: 600; }
        .app-subtitle { margin: 0; font-size: 16px; color: #6c757d; }
      
        /* Stack vertically on small screens */
        @media (max-width: 992px) {
          .hero { flex-direction: column; text-align: center; }
          .brand { text-align: center; }
          .landing-logo { height: 160px; }
        }
      "))
      ),
      
      # Hero: logo on left, title/subtitle on right
      div(class = "hero",
          tags$video(
            class = "landing-logo",
            autoplay = NA, loop = NA, muted = NA, playsinline = NA,
            tags$source(src = "logo.mp4", type = "video/mp4"),
            "Your browser does not support the logo video."
          ),
          div(class = "brand",
              tags$h2(class = "app-title", "Graphical Approach for Multiple Testing"),
              tags$p(class = "app-subtitle",
                     "An interactive tool for visualizing and designing multiple testing strategies in clinical trials.")
          )
      ),
      br(),
      # New Introduction Section
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h3("\U0001f9ec Introduction"),
                        tags$p("With the rise of innovative trial designs\u2014such as adaptive and platform studies\u2014clinical researchers and statisticians face growing complexity in managing multiple hypotheses while preserving statistical rigor. Traditional fixed-sequence procedures often fall short in these evolving contexts."),
                        tags$p("Graphical Approach for Multiple Testing Procedure is a visual and interactive tool built to meet this challenge. Grounded in the graphical methodology pioneered by Bretz et al., it offers a transparent way to design, test, and dynamically redistribute alpha levels across networks of hypotheses."),
                        tags$p("This app was developed in collaboration with methodologists, researchers, and developers committed to making advanced statistical design accessible, reproducible, and human-centered. While still evolving, it is built on the open-source TrialSimulator R package and maintained by a volunteer team.")
               )
        )
      ),
      # Features Cards - 6 features in 2 rows
      fluidRow(
        column(4,
               div(class = "feature-card fc-1",
                   div(class = "feature-title", "\U0001f3af Interactive Graph Design"),
                   div(class = "feature-text", "Right-click empty canvas to add nodes, drag to position, and visually design your hypothesis network with intuitive controls.")
               )
        ),
        column(4,
               div(class = "feature-card fc-2",
                   div(class = "feature-title", "\U0001f517 Smart Edge Management"),
                   div(class = "feature-text", "Right-click nodes to start edges, click targets to connect, and double-click to edit weights with automatic validation.")
               )
        ),
        column(4,
               div(class = "feature-card fc-3",
                   div(class = "feature-title", "\u26a1 Real-time Editing"),
                   div(class = "feature-text", "Double-click nodes or edges to edit properties instantly. All changes reflect immediately with live validation.")
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "feature-card fc-4",
                   div(class = "feature-title", "\U0001f9ee Alpha Management"),
                   div(class = "feature-text", "Allocate alpha levels with automatic sum validation (\u2264 1), supporting dynamic \u03b1-spending rules for sequential testing.")
               )
        ),
        column(4,
               div(class = "feature-card fc-5",
                   div(class = "feature-title", "\U0001f4ca Testing Simulation"),
                   div(class = "feature-text", "Create test objects and simulate hypothesis rejections with real-time graph updates showing rejected hypotheses.")
               )
        ),
        column(4,
               div(class = "feature-card fc-6",
                   div(class = "feature-title", "\U0001f4be Data Management"),
                   div(class = "feature-text", "Import/export graphs as JSON files, with live data tables showing nodes and edges for easy collaboration.")
               )
        )
      ),
      # References
      fluidRow(
        column(12,
               tags$div(style = "padding: 20px;",
                        tags$h3("\U0001f4d8 References"),
                        tags$ul(
                          tags$li(
                            tags$span("Bretz F., Maurer W., Brannath W., Posch M. (2009). "),
                            tags$i("A graphical approach to sequentially rejective multiple testing procedures."),
                            " Stat Med 28(4): 586\u2013604. ",
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
               tags$p("Developed by Phan Nguyen Huong Le & MengYang Yi, advised by Dr. Han Zhang & Dr. Philip He")
        )
      )
    )
  )
}
