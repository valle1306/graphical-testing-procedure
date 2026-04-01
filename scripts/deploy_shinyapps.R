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
if (dir.exists(local_library)) {
  .libPaths(c(normalizePath(local_library, winslash = "/", mustWork = FALSE), .libPaths()))
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    paste(
      "Package 'rsconnect' is required for deployment.",
      "Install it with:",
      "install.packages('rsconnect')"
    ),
    call. = FALSE
  )
}

account_name <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "u3fenv-valerie-le")
app_name <- Sys.getenv("SHINYAPPS_APP_NAME", unset = "graphical-testing-procedure")
token <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

if (!nchar(token) || !nchar(secret)) {
  registered_accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
} else {
  registered_accounts <- NULL
}

if ((is.null(registered_accounts) || !nrow(registered_accounts)) && nchar(token) && nchar(secret)) {
  rsconnect::setAccountInfo(
    name = account_name,
    token = token,
    secret = secret
  )
  registered_accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
}

if (is.null(registered_accounts) || !nrow(registered_accounts)) {
  stop(
    paste(
      "No shinyapps.io account is registered on this machine.",
      "Run rsconnect::setAccountInfo(name = '", account_name, "', token = '<token>', secret = '<secret>')",
      "or set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET before running this script.",
      sep = ""
    ),
    call. = FALSE
  )
}

app_files <- c(
  "app.R",
  "alpha_spending_function.r",
  "Haybittle-Peto.r",
  file.path(
    "www",
    list.files(file.path(project_root, "www"), recursive = TRUE, all.files = FALSE, full.names = FALSE)
  )
)
app_files <- unique(app_files)
app_files <- app_files[file.exists(file.path(project_root, app_files))]

cat("Deploying", app_name, "to shinyapps.io account", account_name, "\n")

rsconnect::deployApp(
  appDir = project_root,
  appName = app_name,
  account = account_name,
  appFiles = app_files,
  launch.browser = FALSE,
  forceUpdate = TRUE
)
