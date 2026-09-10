# Exercise the actual Shiny server without a browser. Used by replication and
# regressions; parsing, boundaries, submissions and replay use production code.
run_manuscript_session <- function(app_root, design, submissions, check_replay = TRUE) {
  app_root <- normalizePath(app_root, winslash = "/", mustWork = TRUE)
  old <- setwd(app_root)
  on.exit(setwd(old), add = TRUE)
  .libPaths(c(file.path(app_root, ".Rlibs"), .libPaths()))
  app_env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(sys.source("app.R", envir = app_env))
  captured <- new.env(parent = emptyenv())
  shiny::testServer(app_env$server, {
    graph <- normalize_graph_import(design)
    rv$gs_suppress_plan_rebuild <- TRUE
    rv$nodes <- graph$nodes
    rv$edges <- graph$edges
    load_group_sequential_design_from_import(design)
    session$flushReact()
    stopifnot(initialize_batch_gs_object(reset_history = TRUE))
    rv$gs_design_finalized <- TRUE
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
    captured$preview <- as.data.frame(rv$gs_boundary_preview)
    snapshots <- list()
    for (i in seq_along(submissions)) {
      batch <- submissions[[i]]
      values <- list(gs_analysis_round = batch$round)
      for (j in seq_len(nrow(batch$values))) {
        key <- batch$values$key[[j]]
        values[[paste0("gs_round_p_", key)]] <- batch$values$p[[j]]
        values[[paste0("gs_round_info_", key)]] <- batch$values$info[[j]]
      }
      do.call(session$setInputs, values)
      result <- collect_round_submission()
      stopifnot(!is.null(result), nrow(result$history_rows) > 0L)
      rv$gs_analysis_history <- sanitize_gs_analysis_history_tbl(
        dplyr::bind_rows(rv$gs_analysis_history, result$history_rows))
      rv$gs_stage_history <- gs_history_to_legacy_stage_history(rv$gs_analysis_history)
      bump_ts_state()
      refresh_ts_state()
      rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
      snapshots[[i]] <- list(history = as.data.frame(result$history_rows),
        alpha = get_current_allocations(), edges = as.data.frame(build_graph_edges()))
    }
    captured$history <- as.data.frame(rv$gs_analysis_history)
    captured$snapshots <- snapshots
    # Exercise the actual download handler, including its numeric precision.
    captured$export <- jsonlite::fromJSON(output$download_graph)
    stopifnot(isTRUE(all.equal(as.data.frame(sanitize_gs_analysis_history_tbl(captured$export$gs_analysis_history)),
      captured$history, tolerance = 1e-12, check.attributes = FALSE)))
    if (check_replay) {
      before <- as.data.frame(rv$ts_object$get_trajectory())
      stopifnot(replay_group_sequential_history(rv$gs_analysis_history))
      after <- as.data.frame(rv$ts_object$get_trajectory())
      stopifnot(isTRUE(all.equal(before, after, tolerance = 1e-6, check.attributes = FALSE)))
    }
  })
  as.list(captured)
}
