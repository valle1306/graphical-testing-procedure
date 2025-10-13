# Debug runner for the Shiny app
options(shiny.trace = TRUE)
options(warn = 1)

# On error, print traceback but don't immediately quit so we can see logs
err_handler <- function(e) {
  cat("--- RUNAPP ERROR ---\n")
  cat(conditionMessage(e), "\n")
  cat("Traceback:\n")
  traceback()
  quit(status = 1)
}

tryCatch({
  shiny::runApp('C:/Users/lpnhu/Downloads/graphical-testing-procedure', port = 4567, launch.browser = FALSE)
}, error = err_handler)
