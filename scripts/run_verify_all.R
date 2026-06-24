args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args[startsWith(args, file_arg)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}

project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

verify_scripts <- sort(list.files(
  file.path(project_root, "scripts"),
  pattern = "^verify_.*\\.R$",
  full.names = TRUE
))

if (length(verify_scripts) == 0) {
  stop("No verification scripts found in scripts/.")
}

rscript_bin <- file.path(R.home("bin"), "Rscript")
lib_paths <- unique(Filter(nzchar, c(
  if (dir.exists(local_lib)) normalizePath(local_lib, winslash = "/", mustWork = TRUE) else NULL,
  Sys.getenv("R_LIBS"),
  Sys.getenv("R_LIBS_USER")
)))
lib_env <- paste(lib_paths, collapse = .Platform$path.sep)

cat("Project root:", project_root, "\n")
cat("Verification scripts:", length(verify_scripts), "\n\n")

passed <- character(0)
for (script in verify_scripts) {
  script_name <- basename(script)
  cat("[RUN ]", script_name, "\n")
  status <- system2(
    rscript_bin,
    args = script,
    env = if (nzchar(lib_env)) c(sprintf("R_LIBS=%s", lib_env)) else character(0)
  )
  if (!identical(status, 0L)) {
    cat("\n[FAIL]", script_name, "\n")
    cat("Passed before failure:", length(passed), "/", length(verify_scripts), "\n")
    if (length(passed) > 0) {
      cat("Completed:", paste(basename(passed), collapse = ", "), "\n")
    }
    quit(status = status)
  }
  passed <- c(passed, script)
  cat("[PASS]", script_name, "\n\n")
}

cat("Verification summary:", length(passed), "/", length(verify_scripts), "passed.\n")

