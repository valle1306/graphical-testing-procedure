args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][1])
root <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
setwd(root)
.libPaths(c(file.path(root, ".Rlibs"), .libPaths()))
suppressPackageStartupMessages(source("app.R", local = TRUE))
source("scripts/manuscript_session.R")

design <- function(alpha = c(.025, 0), looks = c(2L, 2L)) {
  nodes <- data.frame(id = 1:2, hypothesis = c("H1", "H2"),
    x = c(-100, 100), y = 0, alpha = alpha)
  plan <- data.frame(id = nodes$id, hypothesis = nodes$hypothesis,
    planned_analyses = looks, planned_max_info = 100,
    alpha_spending = "OF", custom_cumulative_alpha = "",
    hsd_gamma = -4, haybittle_p1 = .0003)
  schedule <- do.call(rbind, lapply(1:2, function(i) data.frame(
    schedule_key = paste(i, seq_len(looks[i]), sep = "__"),
    analysis_round = seq_len(looks[i]), hypothesis = nodes$hypothesis[i],
    hypothesis_id = i, hypothesis_stage = seq_len(looks[i]),
    planned_analyses = looks[i], information_fraction = seq_len(looks[i])/looks[i],
    is_final = seq_len(looks[i]) == looks[i])))
  list(nodes = nodes, edges = data.frame(id = 1L, from = 1L, to = 2L, weight = 1),
    gs_hypothesis_plan = plan, gs_analysis_schedule = schedule)
}
batch <- function(round, key, p, info) list(round = round,
  values = data.frame(key = key, p = p, info = info))

# Production same-analysis retests: H2 first fails, then benefits from H1.
both <- run_manuscript_session(root, design(c(.0125, .0125), c(1L, 1L)),
  list(batch(1, c("1__1", "2__1"), c(.001, .02), c(100, 100))))
stopifnot(all(tail(both$history$decision, 2) %in% c("Reject", "Do not reject")),
  any(both$history$hypothesis == "H2" & both$history$decision == "Reject"),
  max(both$history$current_alpha[both$history$hypothesis == "H2"]) == .025)

# H2's untestable first look is skipped; after delayed activation, its actual
# first backend test must still be recorded as the second planned look.
late <- run_manuscript_session(root, design(), list(
  batch(1, "1__1", .5, 50), batch(2, "1__2", .001, 100),
  batch(2, "2__2", .02, 100)))
h2 <- late$history[late$history$hypothesis == "H2", ]
stopifnot(nrow(h2) == 1L, h2$hypothesis_stage == 2L,
  h2$analysis_round == 2L, h2$decision == "Reject")

# All five offered families pass through the production submission and download
# paths. Non-rejections retain three looks for an independent crossing check.
for (rule in c("OF", "Pocock", "HSD", "Haybittle-Peto", "Custom")) {
  d <- design(looks = c(3L, 1L))
  d$gs_hypothesis_plan$alpha_spending[1] <- rule
  d$gs_hypothesis_plan$custom_cumulative_alpha[1] <- ".005,.015,.025"
  d$gs_analysis_schedule$information_fraction[1:3] <- c(.33, .67, 1)
  result <- run_manuscript_session(root, d, list(
    batch(1, "1__1", .5, 33), batch(2, "1__2", .5, 67), batch(3, "1__3", .5, 100)))
  z <- result$history$boundary_z
  timing <- c(.33, .67, 1)
  corr <- outer(timing, timing, function(x, y) sqrt(pmin(x,y)/pmax(x,y)))
  crossing <- 1 - as.numeric(mvtnorm::pmvnorm(upper = z, corr = corr,
    algorithm = mvtnorm::Miwa(steps = 256)))
  stopifnot(length(z) == 3L, abs(crossing - .025) < 1e-5)
  cat(rule, "production crossing probability:", format(crossing, digits = 10), "\n")
}

shiny::testServer(server, {
  rejects <- function(expr) {
    result <- tryCatch({force(expr); NULL}, error = identity)
    stopifnot(inherits(result, "error"))
    invisible(result)
  }
  initialise <- function(d = design()) {
    graph <- normalize_graph_import(d)
    rv$gs_suppress_plan_rebuild <- TRUE
    rv$nodes <- graph$nodes
    rv$edges <- graph$edges
    load_group_sequential_design_from_import(d)
    session$flushReact()
    stopifnot(initialize_batch_gs_object(reset_history = TRUE))
    rv$gs_design_finalized <- TRUE
    rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  }
  initialise()
  plan <- rv$gs_hypothesis_plan
  plan$alpha_spending[2] <- "Custom"
  plan$custom_cumulative_alpha[2] <- ".005,.01"
  stopifnot(!validate_gs_analysis_schedule(plan_tbl = plan)$ok)
  plan$alpha_spending[2] <- "Haybittle-Peto"
  stopifnot(!validate_gs_analysis_schedule(plan_tbl = plan)$ok)
  plan <- rv$gs_hypothesis_plan
  plan$alpha_spending[1] <- "HSD"
  plan$hsd_gamma[1] <- Inf
  stopifnot(!validate_gs_analysis_schedule(plan_tbl = plan)$ok)

  session$setInputs(gs_analysis_round = 2L, gs_round_p_1__2 = .1, gs_round_info_1__2 = 100)
  rejects(collect_round_submission())
  session$setInputs(gs_analysis_round = 1L, gs_round_p_1__1 = .1, gs_round_info_1__1 = 100)
  rejects(collect_round_submission())
  session$setInputs(gs_round_info_1__1 = 50.5)
  rejects(collect_round_submission())

  session$setInputs(gs_round_info_1__1 = 50)
  original_test <- rv$ts_object$test
  original_alpha <- get_current_allocations()
  original_trajectory <- rv$ts_object$get_trajectory()
  unlockBinding("test", rv$ts_object)
  rv$ts_object$test <- function(x) {
    rv$ts_object$reject_a_hypothesis("H1")
    stop("Injected error after a backend mutation.")
  }
  rejects(collect_round_submission())
  stopifnot(identical(get_current_allocations(), original_alpha),
    isTRUE(all.equal(rv$ts_object$get_trajectory(), original_trajectory)))
  # Reinitialise to restore correctly bound R6 methods after this deliberate injection.
  initialise()
  session$setInputs(gs_analysis_round = 1L, gs_round_p_1__1 = .5, gs_round_info_1__1 = 50)
  submission <- collect_round_submission()
  rv$gs_analysis_history <- submission$history_rows
  refresh_ts_state()
  rv$gs_boundary_preview <- build_gs_boundary_schedule(notify = FALSE)
  session$setInputs(gs_analysis_round = 2L, gs_round_p_1__2 = .1, gs_round_info_1__2 = 50)
  rejects(collect_round_submission())

  recorded <- rv$gs_analysis_history
  before <- rv$ts_object$get_trajectory()
  tampered <- recorded
  tampered$boundary_p <- tampered$boundary_p + .001
  rejects(replay_group_sequential_history(tampered))
  stopifnot(isTRUE(all.equal(before, rv$ts_object$get_trajectory())),
    identical(recorded, rv$gs_analysis_history))
  stopifnot(replay_group_sequential_history(recorded))
})
partial <- run_manuscript_session(root, design(), list(batch(1, "1__1", .5, 50)))
bad_import <- both$export
bad_import$gs_analysis_history$boundary_p[1] <- bad_import$gs_analysis_history$boundary_p[1] + .001
import_cases <- list(completed = both$export, partial = partial$export,
  design_only = design(), inconsistent = bad_import)
old_session <- both$export
old_session$software$execution_semantics <- NULL
old_session$software$version <- "0.2.0"
old_session$format_version <- 2L
import_cases$incompatible_completed <- old_session
for (case_name in names(import_cases)) {
  local({
    payload <- import_cases[[case_name]]
    should_lock <- case_name %in% c("completed", "partial")
    shiny::testServer(server, {
      # Exercise the upload observer, not only the internal replay helper.
      saved <- tempfile(fileext = ".json")
      jsonlite::write_json(payload, saved, auto_unbox = TRUE, digits = NA)
      session$setInputs(upload_graph = list(name = "session.json", type = "application/json",
        size = file.info(saved)$size, datapath = saved))
      session$flushReact()
      cat("Import case:", case_name, "locked:", rv$gs_design_finalized, "expected:", should_lock, "\n")
      stopifnot(identical(rv$gs_design_finalized, should_lock))
      summary_html <- output$gs_analysis_design_summary$html
      if (should_lock) {
        stopifnot(nrow(rv$gs_analysis_history) == nrow(payload$gs_analysis_history),
          identical(rv$gs_applied_design_signature, gs_current_design_signature()),
          grepl("Locked design", summary_html, fixed = TRUE),
          !grepl("No finalized design", summary_html, fixed = TRUE))
        session$setInputs(gs_edit_design = 1L)
        stopifnot(isTRUE(rv$gs_design_finalized))
        if (case_name == "completed") stopifnot(nrow(gs_schedule_next_round_tbl()) == 0L)
      } else {
        stopifnot(nrow(rv$gs_analysis_history) == 0L, is.null(rv$ts_object),
          identical(rv$gs_applied_design_signature, ""),
          grepl("No finalized design", summary_html, fixed = TRUE))
      }
    })
  })
}
cat("V2 production runtime, delayed activation, rollback, replay and imported-lock checks passed.\n")
