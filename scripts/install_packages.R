options(repos = c(CRAN = "https://cloud.r-project.org"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args[startsWith(args, file_arg)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
local_library <- file.path(project_root, ".Rlibs")
if (!dir.exists(local_library)) {
  dir.create(local_library, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(normalizePath(local_library, winslash = "/", mustWork = FALSE), .libPaths()))

required_packages <- c(
  "shiny",
  "visNetwork",
  "shinyjs",
  "dplyr",
  "DT",
  "jsonlite",
  "bslib",
  "tibble",
  "TrialSimulator",
  "gsDesign",
  "mvtnorm"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0) {
  cat("All required packages are already installed.\n")
} else {
  cat("Using library:", .libPaths()[1], "\n")
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE, lib = .libPaths()[1])
}
