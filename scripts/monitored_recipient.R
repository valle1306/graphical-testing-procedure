# One bounded extension of the existing two-hypothesis runtime fixture.
# H1 is the monitored recipient; H2 transfers its entire local level to H1.
monitored_recipient_design <- function() {
  nodes <- data.frame(id = 1:2, hypothesis = c("H1", "H2"),
    x = c(-100, 100), y = 0, alpha = c(.0125, .0125))
  plan <- data.frame(id = 1:2, hypothesis = nodes$hypothesis,
    planned_analyses = 3L, planned_max_info = 100L, alpha_spending = "OF",
    custom_cumulative_alpha = "", hsd_gamma = -4, haybittle_p1 = .0003)
  schedule <- do.call(rbind, lapply(1:2, function(i) data.frame(
    schedule_key = paste(i, 1:3, sep = "__"), analysis_round = 1:3,
    hypothesis = nodes$hypothesis[i], hypothesis_id = i, hypothesis_stage = 1:3,
    planned_analyses = 3L, information_fraction = c(.33, .67, 1),
    is_final = c(FALSE, FALSE, TRUE))))
  list(nodes = nodes, edges = data.frame(id = 1L, from = 2L, to = 1L, weight = 1),
    gs_hypothesis_plan = plan, gs_analysis_schedule = schedule)
}
monitored_recipient_submissions <- function() list(
  list(round = 1L, values = data.frame(key = c("1__1", "2__1"), p = .5, info = 33)),
  list(round = 2L, values = data.frame(key = c("1__2", "2__2"), p = c(.008, .0001), info = 67)),
  list(round = 3L, values = data.frame(key = "1__3", p = .02, info = 100)))
