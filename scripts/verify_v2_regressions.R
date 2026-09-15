args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[startsWith(args, "--file=")][1])
root <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
setwd(root)
.libPaths(c(file.path(root, ".Rlibs"), .libPaths()))
suppressPackageStartupMessages(source("app.R", local = TRUE))

shiny::testServer(server, {
  rejects <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
  fixture <- list(nodes = data.frame(id = 1:3, x = c(-100, 0, 100), y = 0,
    hypothesis = c("H1", "H2", "H3"), alpha = c(.01, .01, .005)),
    edges = data.frame(id = 1:2, from = c(1L, 2L), to = c(2L, 3L), weight = 1))
  stopifnot(nrow(normalize_graph_import(fixture)$nodes) == 3L)
  bad <- fixture; bad$nodes$alpha[1] <- -.1; rejects(normalize_graph_import(bad))
  bad <- fixture; bad$nodes$id[2] <- 1; rejects(normalize_graph_import(bad))
  bad <- fixture; bad$edges$to[1] <- 99; rejects(normalize_graph_import(bad))
  bad <- fixture; bad$edges$weight[1] <- NA; rejects(normalize_graph_import(bad))
  bad <- fixture; bad$edges <- rbind(bad$edges, bad$edges[1, ]); rejects(normalize_graph_import(bad))
  bad <- fixture; bad$edges$to[1] <- 1; rejects(normalize_graph_import(bad))
  session$setInputs(upload_graph = list(datapath = tempfile()))
  stopifnot(is.null(rv$ts_object))

  rv$nodes <- tibble::as_tibble(fixture$nodes)
  rv$edges <- tibble::as_tibble(fixture$edges)
  rv$gs_hypothesis_plan <- build_default_gs_hypothesis_plan(rv$nodes, empty_gs_hypothesis_plan())
  rv$gs_analysis_schedule <- build_default_gs_analysis_schedule(rv$gs_hypothesis_plan)
  sig <- gs_current_design_signature()
  rv$nodes$alpha[1] <- .011
  stopifnot(sig != gs_current_design_signature())
  rv$nodes$alpha[1] <- .01
  rv$edges$weight[1] <- .5
  stopifnot(sig != gs_current_design_signature())
  rv$edges$weight[1] <- 1
  stopifnot(sig == gs_current_design_signature())
  stopifnot(initialize_batch_gs_object(reset_history = TRUE))
  rv$ts_object$reject_a_hypothesis("H2")
  live_edges <- build_graph_edges()
  stopifnot(any(live_edges$from == 1 & live_edges$to == 3 & live_edges$weight == 1))
  stopifnot(!any(live_edges$from == 2 | live_edges$to == 2))

  stopifnot(nrow(HP(timing = 1)) == 1L)
  stopifnot(abs(HP(timing = 1)$p - .025) < 1e-12)
  rejects(HP(p1 = .01, overall.alpha = .005))
  rejects(compute_boundary_schedule(.025, "Custom", c(.5, 1), c(.8, .7)))
  hp <- HP(timing = c(.3, .65, 1))
  corr <- build_information_correlation(c(.3, .65, 1))
  probability <- 1 - as.numeric(mvtnorm::pmvnorm(upper = hp$z, corr = corr,
    algorithm = mvtnorm::Miwa(steps = 256)))
  stopifnot(abs(probability - .025) < 1e-5)
  for (rule in c("OF", "Pocock", "HSD")) for (gamma in
      if (rule == "HSD") c(-8, -4, -2, -1, .5, 1, 2, 4) else -4) {
    timing <- c(.3, .65, 1)
    actual <- compute_boundary_schedule(.025, rule, timing, hsd_gamma = gamma)
    spending <- switch(rule, OF = gsDesign::sfLDOF,
      Pocock = gsDesign::sfLDPocock, HSD = gsDesign::sfHSD)
    reference <- gsDesign::gsDesign(k = 3, alpha = .025, timing = timing,
      sfu = spending, sfupar = if (rule == "HSD") gamma else NULL, test.type = 1)
    stopifnot(max(abs(actual$z_boundary - reference$upper$bound)) < 1e-8)
  }
  custom <- compute_boundary_schedule(.025, "Custom", c(.3, .65, 1), c(.2, .6, 1))
  probability <- 1 - as.numeric(mvtnorm::pmvnorm(upper = custom$z_boundary,
    corr = build_information_correlation(c(.3, .65, 1)),
    algorithm = mvtnorm::Miwa(steps = 256)))
  stopifnot(abs(probability - .025) < 1e-5)
  cat("V2 graph, import, signature and boundary regression assertions passed.\n")
})
source("scripts/manuscript_session.R")
cat("Production session driver loaded successfully.\n")
