args <- commandArgs(FALSE)
entry <- sub("^--file=", "", args[startsWith(args, "--file=")][1])
root <- normalizePath(file.path(dirname(entry), ".."), winslash = "/")
setwd(root)
.libPaths(c(file.path(root, ".Rlibs"), .libPaths()))
suppressPackageStartupMessages(library(dplyr))
source("R/server/numerical_guards.R")
source("scripts/manuscript_session.R")
source("scripts/monitored_recipient.R")
g <- new_guarded_graphical_testing(alpha = c(.000001, 0),
  transition = matrix(c(0,0,1,0), 2), alpha_spending = rep("asOF", 2),
  planned_max_info = rep(100L, 2), hypotheses = c("H1", "H2"), silent = TRUE)
failure <- tryCatch(g$reject_a_hypothesis("H1"), error = identity)
stopifnot(inherits(failure, "error"), g$is_in_graph(g$get_hid("H1")),
  g$get_alpha(g$get_hid("H2")) == 0)
result <- run_manuscript_session(root, monitored_recipient_design(), monitored_recipient_submissions())
h1 <- subset(result$history, hypothesis == "H1")
stopifnot(nrow(h1) == 4L, identical(h1$decision, c("Do not reject", "Do not reject", "Do not reject", "Reject")),
  h1$p_value[2] == h1$p_value[3], h1$observed_info[2] == h1$observed_info[3],
  h1$current_alpha[2] == .0125, h1$current_alpha[3] == .025)
cat("Graph allocation guard preserves state; monitored-recipient session including final look and replay passed.\n")
