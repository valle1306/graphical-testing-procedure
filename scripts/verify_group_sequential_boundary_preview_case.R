script_arg <- "--file="
script_path <- sub(script_arg, "", commandArgs(trailingOnly = FALSE)[grep(script_arg, commandArgs(trailingOnly = FALSE))][1])
if (is.na(script_path) || !nzchar(script_path)) {
  script_path <- file.path(getwd(), "scripts", "verify_group_sequential_boundary_preview_case.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(jsonlite))

format_plain_number <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value)) {
    return(character(0))
  }
  out <- rep("", length(value))
  keep <- !is.na(value) & is.finite(value)
  if (!any(keep)) {
    return(out)
  }
  formatted <- format(value[keep], trim = TRUE, scientific = FALSE, nsmall = 0)
  formatted <- sub("([0-9])0+$", "\\1", formatted)
  out[keep] <- sub("\\.$", "", formatted)
  out
}

source(file.path(project_root, "R", "server", "common_helpers.R"), local = TRUE)
observe <- function(...) invisible(NULL)
observeEvent <- function(...) invisible(NULL)
source(file.path(project_root, "R", "server", "sequential_settings.R"), local = TRUE)
source(file.path(project_root, "R", "server", "sequential_boundaries.R"), local = TRUE)

rv <- new.env(parent = emptyenv())
rv$gs_boundary_preview_message <- NULL

upload_path <- file.path(project_root, "examples", "upload_example.json")
upload_data <- jsonlite::fromJSON(upload_path)
nodes_tbl <- tibble::as_tibble(upload_data$nodes)
alpha_lookup <- stats::setNames(as.numeric(nodes_tbl$alpha), as.character(nodes_tbl$hypothesis))

get_current_allocations <- function() {
  alpha_lookup
}

build_ts_status_table <- function() {
  tibble::tibble(
    hypothesis = as.character(nodes_tbl$hypothesis),
    current_alpha = as.numeric(alpha_lookup[nodes_tbl$hypothesis]),
    decision = rep("ready", nrow(nodes_tbl)),
    in_graph = rep(TRUE, nrow(nodes_tbl)),
    testable = as.numeric(nodes_tbl$alpha) > 0
  )
}

stopifnot(
  identical(
    gs_custom_cumulative_alpha_placeholder(4L, 0.01),
    "e.g. 0.0025, 0.005, 0.0075, 0.01"
  )
)
stopifnot(grepl("4 cumulative alpha values", gs_custom_cumulative_alpha_instruction(4L, 0.01), fixed = TRUE))
stopifnot(grepl("0.01", gs_custom_cumulative_alpha_instruction(4L, 0.01), fixed = TRUE))
stopifnot(!isTRUE(parse_custom_cumulative_alpha("0.0025, 0.005", 4L, total_alpha = 0.01)$ok))
stopifnot(!isTRUE(parse_custom_cumulative_alpha("0.0025, 0.0025, 0.0075, 0.01", 4L, total_alpha = 0.01)$ok))
stopifnot(!isTRUE(parse_custom_cumulative_alpha("0.0025, 0.005, 0.0075, 0.009", 4L, total_alpha = 0.01)$ok))

plan_tbl <- tibble::tibble(
  id = as.integer(nodes_tbl$id),
  hypothesis = as.character(nodes_tbl$hypothesis),
  planned_analyses = c(4L, 1L, 1L, 1L),
  alpha_spending = c("Custom", "OF", "OF", "OF"),
  custom_cumulative_alpha = c("0.0025, 0.005", "", "", ""),
  hsd_gamma = c(-4, -4, -4, -4),
  haybittle_p1 = c(0.0003, 0.0003, 0.0003, 0.0003)
)
plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)

schedule_tbl <- suppressWarnings(build_default_gs_analysis_schedule(plan_tbl))
h1_schedule <- schedule_tbl %>% dplyr::filter(hypothesis == "H1") %>% dplyr::arrange(hypothesis_stage)
stopifnot(all.equal(h1_schedule$information_fraction, c(0.25, 0.5, 0.75, 1), tolerance = 1e-8))

preview_invalid <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
stopifnot(nrow(preview_invalid) == 0L)
stopifnot(grepl("Enter 4 cumulative alpha values", rv$gs_boundary_preview_message, fixed = TRUE))

plan_tbl$custom_cumulative_alpha[plan_tbl$hypothesis == "H1"] <- "0.0025, 0.005, 0.0075, 0.01"
preview_valid <- build_gs_boundary_schedule(plan_tbl = plan_tbl, schedule_tbl = schedule_tbl, notify = FALSE)
h1_preview <- preview_valid %>% dplyr::filter(hypothesis == "H1") %>% dplyr::arrange(hypothesis_stage)

stopifnot(is.null(rv$gs_boundary_preview_message))
stopifnot(nrow(h1_preview) == 4L)
stopifnot(all(is.finite(h1_preview$stage_alpha)))
stopifnot(all(is.finite(h1_preview$cumulative_alpha_spent)))
stopifnot(all(is.finite(h1_preview$p_boundary)))
stopifnot(abs(tail(h1_preview$cumulative_alpha_spent, 1) - 0.01) < 1e-12)

cat("Group sequential boundary-preview verification scaffold\n\n")
cat("Upload example case:\n")
cat("- Load examples/upload_example.json\n")
cat("- Set H1 to K = 4 and Custom cumulative alpha\n")
cat("- Enter 0.0025, 0.005, 0.0075, 0.01\n")
cat("\nExpected behavior:\n")
cat("- The custom-alpha placeholder shows four cumulative values ending at 0.01.\n")
cat("- H1 default information fractions are 0.25, 0.5, 0.75, 1.\n")
cat("- Invalid custom-alpha input shows one preview-level validation message.\n")
cat("- Valid custom-alpha input fills the H1 boundary preview rows.\n")
cat("\nValid H1 boundary preview:\n")
print(h1_preview)
