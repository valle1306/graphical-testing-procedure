options(repos = c(CRAN = "https://cloud.r-project.org"))

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
local_library <- file.path(project_root, ".Rlibs")

if (dir.exists(local_library)) {
  .libPaths(c(normalizePath(local_library, winslash = "/", mustWork = FALSE), .libPaths()))
}