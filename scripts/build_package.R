# Assemble the installable package from the maintained app sources.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][1])
root <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
stage_parent <- tempfile("graphMTP-package-")
dir.create(stage_parent)
stopifnot(file.copy(file.path(root, "package", "graphMTP"), stage_parent, recursive = TRUE))
stage <- file.path(stage_parent, "graphMTP")
app <- file.path(stage, "inst", "app")
dir.create(app, recursive = TRUE)
for (item in c("app.R", "alpha_spending_function.r", "Haybittle-Peto.r", "R", "www", "examples", "README.md", "LICENSE")) {
  stopifnot(file.copy(file.path(root, item), app, recursive = TRUE))
}
# Source ASCII requested by JSS: encode non-ASCII literals as R Unicode escapes
# and transliterate comments without changing rendered application text.
for (f in list.files(app, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  lines <- vapply(lines, function(line) {
    chars <- utf8ToInt(enc2utf8(line))
    paste(vapply(chars, function(ch) if (ch < 128) intToUtf8(ch) else
      if (ch <= 65535) sprintf("\\u%04x", ch) else sprintf("\\U%08x", ch), ""), collapse = "")
  }, "")
  writeLines(lines, f, useBytes = TRUE)
}
out <- commandArgs(TRUE)
out <- if (length(out)) normalizePath(out[[1]], mustWork = TRUE) else root
old <- setwd(out)
on.exit(setwd(old))
status <- system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", shQuote(stage)))
if (status != 0) stop("Package build failed.")
