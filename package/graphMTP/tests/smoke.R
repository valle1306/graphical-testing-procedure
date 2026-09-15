# Exercise the application shipped inside the installed package, not a checkout.
library(graphMTP)
stopifnot(is.function(run_app))
app_dir <- system.file("app", package = "graphMTP", mustWork = TRUE)
old_dir <- setwd(app_dir)
for (f in list.files(".", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  parse(f, encoding = "UTF-8")
}
app_env <- new.env(parent = globalenv())
suppressPackageStartupMessages(sys.source("app.R", envir = app_env))
stopifnot(inherits(app_env$ui, c("shiny.tag", "shiny.tag.list")), is.function(app_env$server))
shiny::testServer(app_env$server, {
  fixture <- list(nodes = data.frame(id = 1L, hypothesis = "H1", alpha = .025, x = 0, y = 0))
  graph <- normalize_graph_import(fixture)
  stopifnot(nrow(graph$nodes) == 1L, nrow(graph$edges) == 0L)
  boundary <- compute_boundary_schedule(.025, "OF", 1)
  stopifnot(abs(boundary$p_boundary - .025) < 1e-12)
  local <- list(timing=c(.5,1), information=c(50,100), maximum=100,
    rounds=1:2, rule="OF", proportions=NULL)
  engine <- new_guarded_graphical_testing(alpha=.025,transition=matrix(0,1,1),
    alpha_spending="asOF",planned_max_info=100L,hypotheses="H1",silent=TRUE,
    local_specs=list(H1=local))
  engine$test(data.frame(order=1L,hypotheses="H1",p=.5,info=50,is_final=FALSE,max_info=100))
  first <- engine$get_trajectory()
  engine$test(data.frame(order=2L,hypotheses="H1",p=.01,info=100,is_final=TRUE,max_info=100))
  history <- engine$get_trajectory()
  direct <- graphmtp_local_boundaries(local,.025)
  stopifnot(nrow(history)==2L,history$decision[2]=="reject",
    abs(history$stageLevels[2]-direct$stageLevels[2])<1e-12,
    identical(history$stageLevels[1],first$stageLevels[1]))
})
setwd(old_dir)
