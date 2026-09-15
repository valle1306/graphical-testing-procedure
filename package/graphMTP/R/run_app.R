#' Launch the graphMTP application
#'
#' @param ... Arguments passed to shiny::runApp, such as port and launch.browser.
#' @return The value returned by shiny::runApp, invisibly.
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "graphMTP", mustWork = TRUE)
  invisible(shiny::runApp(appDir = app_dir, ...))
}
