# Execution semantics are part of a saved completed session's identity.
graphmtp_execution_semantics <- "fixed-information-nested-spending-v1"

# Freeze the information schedule and any proportional spending shape before
# testing. HP is derived at the initial allocation, never at a recycled alpha.
graphmtp_local_spec <- function(plan, schedule, initial_alpha) {
  maximum <- as.numeric(plan$planned_max_info[[1]])
  information <- round(as.numeric(schedule$information_fraction) * maximum)
  if (maximum != round(maximum) || any(information <= 0) ||
      any(diff(information) <= 0) || tail(information, 1) != maximum)
    stop("Fixed information must map to increasing positive whole numbers ending at the planned maximum.")
  timing <- information / maximum
  rule <- as.character(plan$alpha_spending[[1]])
  proportions <- switch(rule,
    "Pocock" = gsDesign::sfLDPocock(1, timing)$spend,
    "HSD" = gsDesign::sfHSD(1, timing, plan$hsd_gamma[[1]])$spend,
    "Custom" = {
      profile <- parse_custom_cumulative_alpha(plan$custom_cumulative_alpha[[1]],
        length(timing), total_alpha = initial_alpha)
      if (!isTRUE(profile$ok) || is.null(profile$proportions))
        stop(paste("Invalid initial custom spending profile:", profile$message))
      profile$proportions
    },
    "Haybittle-Peto" = if (length(timing) == 1L) 1 else
      HP(p1 = plan$haybittle_p1[[1]], overall.alpha = initial_alpha,
        timing = timing)$cum.alpha / initial_alpha,
    "OF" = NULL, stop("Unsupported fixed spending family."))
  if (!is.null(proportions) && (length(proportions) != length(timing) ||
      any(!is.finite(proportions)) || any(proportions <= 0) ||
      any(diff(proportions) <= 0) || abs(tail(proportions, 1) - 1) > 1e-8))
    stop("Fixed cumulative spending proportions must be positive and strictly increase to 1.")
  list(timing = timing, information = information, maximum = maximum,
    rounds = as.integer(schedule$analysis_round), rule = rule,
    proportions = proportions)
}

# Evaluate the same complete local family at interim and final looks. No
# artificial initial look or reconstruction from stored critical values occurs.
graphmtp_local_boundaries <- function(spec, alpha) {
  if (!is.finite(alpha) || alpha < 1e-5 || alpha > 0.3173105)
    stop("Sequential positive local alpha must be in [1e-5, 0.3173105].")
  k <- length(spec$timing)
  code <- if (identical(spec$rule, "OF")) "asOF" else "asUser"
  target <- if (code == "asOF") gsDesign::sfLDOF(alpha, spec$timing)$spend else alpha * spec$proportions
  if (any(c(target[[1]], diff(target)) < 1e-8))
    stop("Each fixed first-crossing spending increment must be at least 1e-8 for supported numerical evaluation.")
  if (k == 1L) return(data.frame(typeOfDesign = code, stages = 1L,
    informationRates = 1, alpha = alpha, sided = 1, alphaSpent = alpha,
    criticalValues = qnorm(alpha, lower.tail = FALSE), stageLevels = alpha))
  args <- list(sided = 1, alpha = alpha, informationRates = spec$timing,
    typeOfDesign = code, tolerance = 1e-10)
  if (code == "asUser") args$userAlphaSpending <- alpha * spec$proportions
  design <- do.call(rpact::getDesignGroupSequential, args)
  result <- as.data.frame(design)[, c("typeOfDesign", "stages", "informationRates",
    "alpha", "sided", "alphaSpent", "criticalValues", "stageLevels")]
  if (any(!is.finite(result$criticalValues)) || any(!is.finite(result$stageLevels)) ||
      any(result$stageLevels <= 0) || abs(tail(result$alphaSpent, 1) - alpha) > 1e-7)
    stop("Boundary solver could not represent this fixed design reliably; analysis unchanged.")
  result
}

new_guarded_graphical_testing <- function(..., local_specs = NULL) {
  guarded <- R6::R6Class("GraphMTPTesting", inherit = TrialSimulator::GraphicalTesting,
    public = list(
      test_hypotheses = function(stats) {
        if (is.null(local_specs)) stop("A prespecified local schedule is required for sequential testing.")
        if (!nrow(stats)) return(invisible(NULL))
        if (length(unique(stats$order)) != 1L || anyDuplicated(stats$hypotheses))
          stop("Submit one analysis time with one input per hypothesis.")
        stages <- integer(nrow(stats))
        for (i in seq_len(nrow(stats))) {
          h <- stats$hypotheses[[i]]
          spec <- local_specs[[h]]
          if (is.null(spec)) stop("Unknown hypothesis in fixed-schedule submission.")
          k <- match(stats$order[[i]], spec$rounds)
          if (is.na(k) || !is.finite(stats$info[[i]]) ||
              stats$info[[i]] != spec$information[[k]] ||
              stats$max_info[[i]] != spec$maximum ||
              !identical(as.logical(stats$is_final[[i]]), k == length(spec$timing)))
            stop("Information and final status must match the prespecified fixed schedule; information adaptations are unsupported.")
          if (!is.finite(stats$p[[i]]) || stats$p[[i]] < 0 || stats$p[[i]] > 1)
            stop("One-sided p-values must be in [0, 1].")
          old <- self$get_trajectory()
          if (is.data.frame(old) && any(old$hypothesis == h & old$order >= stats$order[[i]]))
            stop("A submitted hypothesis look cannot be repeated or submitted out of order.")
          stages[[i]] <- k
        }
        appended <- rep(FALSE, nrow(stats))
        repeat {
          rejected <- FALSE
          for (i in seq_len(nrow(stats))) {
            stat <- stats[i, , drop = FALSE]
            h <- stat$hypotheses[[1]]
            hid <- self$get_hid(h)
            if (!self$is_in_graph(hid) || self$get_alpha(hid) <= 0) next
            k <- stages[[i]]
            boundary <- graphmtp_local_boundaries(local_specs[[h]], self$get_alpha(hid))[k, , drop = FALSE]
            if (!appended[[i]]) {
              private$gst[[hid]]$info <- c(private$gst[[hid]]$info, stat$info)
              private$gst[[hid]]$is_final <- c(private$gst[[hid]]$is_final, stat$is_final)
              private$gst[[hid]]$p <- c(private$gst[[hid]]$p, stat$p)
              private$gst[[hid]]$critical_values <- c(private$gst[[hid]]$critical_values, boundary$criticalValues)
              appended[[i]] <- TRUE
            }
            boundary$hypothesis <- h
            boundary$obs_p_value <- stat$p
            boundary$decision <- if (stat$p < boundary$stageLevels) "reject" else "accept"
            boundary$order <- stat$order
            if (boundary$decision == "reject") self$reject_a_hypothesis(h)
            self$set_trajectory(boundary)
            if (boundary$decision == "reject") {
              rejected <- TRUE
              break
            }
          }
          if (!rejected) break
        }
        invisible(NULL)
      },
      reject_a_hypothesis = function(hypothesis) {
        hid <- self$get_hid(hypothesis)
        for (recipient in self$get_hypotheses_ids()) {
          if (recipient == hid || !self$is_in_graph(recipient)) next
          weight <- self$get_weight(hid, recipient)
          updated <- self$get_alpha(recipient) + self$get_alpha(hid) * weight
          if (weight != 0 && updated < 1e-5)
            stop("Recycled local level is below the supported 1e-5 backend threshold; graph unchanged.")
        }
        super$reject_a_hypothesis(hypothesis)
      }))
  guarded$new(...)
}
