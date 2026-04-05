# Debug runner for the Shiny app
options(shiny.trace = TRUE)
options(warn = 1)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args[startsWith(args, file_arg)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}

project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
log_path <- file.path(project_root, "app_run.log")

local_library <- file.path(project_root, ".Rlibs")
if (dir.exists(local_library)) {
  .libPaths(c(normalizePath(local_library, winslash = "/", mustWork = FALSE), .libPaths()))
}

required_packages <- c(
  "shiny", "visNetwork", "shinyjs", "dplyr", "DT",
  "jsonlite", "bslib", "tibble", "TrialSimulator", "gsDesign", "mvtnorm"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

log_con <- file(log_path, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

cat("Project root:", project_root, "\n")
cat("Log file:", log_path, "\n")

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Missing packages:",
      paste(missing_packages, collapse = ", "),
      "\nRun `Rscript scripts/install_packages.R` first."
    )
  )
}

if (!file.exists(file.path(project_root, "app.R"))) {
  stop("Could not find app.R in the project root: ", project_root)
}

err_handler <- function(e) {
  cat("--- RUNAPP ERROR ---\n")
  cat(conditionMessage(e), "\n")
  cat("Traceback:\n")
  traceback(2)
  quit(status = 1)
}

port_is_in_use <- function(port, host = "127.0.0.1") {
  con <- try(
    socketConnection(host = host, port = port, open = "r+", blocking = TRUE, timeout = 1),
    silent = TRUE
  )
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  close(con)
  TRUE
}

find_available_port <- function(start_port, host = "127.0.0.1", attempts = 25L) {
  for (candidate in seq.int(start_port, length.out = attempts)) {
    if (!isTRUE(port_is_in_use(candidate, host = host))) {
      return(candidate)
    }
  }
  stop(sprintf("No available port found in range %s-%s.", start_port, start_port + attempts - 1L))
}

host <- Sys.getenv("SHINY_HOST", unset = "127.0.0.1")
requested_port <- suppressWarnings(as.integer(Sys.getenv("SHINY_PORT", unset = "4567")))
if (is.na(requested_port) || requested_port < 1L) {
  requested_port <- 4567L
}
selected_port <- find_available_port(requested_port, host = host)

if (!identical(selected_port, requested_port)) {
  cat(sprintf("Requested port %s is already in use. Falling back to %s.\n", requested_port, selected_port))
}

cat(sprintf("Launching app at http://%s:%s\n", host, selected_port))

tryCatch({
  shiny::runApp(project_root, port = selected_port, launch.browser = FALSE, host = host)
}, error = err_handler)
