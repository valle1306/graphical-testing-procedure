build_default_gs_hypothesis_plan <- function(
  nodes_tbl = rv$nodes,
  existing_tbl = isolate(rv$gs_hypothesis_plan)
) {
  nodes_tbl <- sanitize_nodes_tbl(nodes_tbl)
  existing_tbl <- sanitize_gs_hypothesis_plan_tbl(existing_tbl)
  if (!nrow(nodes_tbl)) {
    return(empty_gs_hypothesis_plan())
  }
  rows <- lapply(seq_len(nrow(nodes_tbl)), function(i) {
    existing_idx <- match(nodes_tbl$id[[i]], existing_tbl$id)
    planned_analyses <- if (!is.na(existing_idx)) {
      max(1L, as.integer(existing_tbl$planned_analyses[[existing_idx]]))
    } else {
      1L
    }
    alpha_spending <- if (!is.na(existing_idx)) {
      normalize_spending_rule(existing_tbl$alpha_spending[[existing_idx]])
    } else {
      "OF"
    }
    custom_value <- if (!is.na(existing_idx)) {
      as.character(existing_tbl$custom_cumulative_alpha[[existing_idx]])
    } else {
      ""
    }
    hsd_gamma_value <- if (!is.na(existing_idx) && "hsd_gamma" %in% names(existing_tbl)) {
      as.numeric(existing_tbl$hsd_gamma[[existing_idx]])
    } else {
      -4
    }
    if (is.na(hsd_gamma_value)) hsd_gamma_value <- -4
    haybittle_p1_value <- if (!is.na(existing_idx) && "haybittle_p1" %in% names(existing_tbl)) {
      as.numeric(existing_tbl$haybittle_p1[[existing_idx]])
    } else {
      3e-04
    }
    if (is.na(haybittle_p1_value)) haybittle_p1_value <- 3e-04
    tibble::tibble(
      id = as.integer(nodes_tbl$id[[i]]),
      hypothesis = as.character(nodes_tbl$hypothesis[[i]]),
      planned_analyses = planned_analyses,
      alpha_spending = alpha_spending,
      custom_cumulative_alpha = custom_value,
      hsd_gamma = hsd_gamma_value,
      haybittle_p1 = haybittle_p1_value
    )
  })
  sanitize_gs_hypothesis_plan_tbl(dplyr::bind_rows(rows))
}

collect_gs_hypothesis_plan <- function(persist = FALSE) {
  plan_tbl <- build_default_gs_hypothesis_plan()
  if (!nrow(plan_tbl)) {
    if (isTRUE(persist)) {
      rv$gs_hypothesis_plan <- plan_tbl
    }
    return(plan_tbl)
  }
  rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    id <- plan_tbl$id[[i]]
    planned_analyses <- read_scalar_integer_input(paste0("gs_plan_k_", id), default = plan_tbl$planned_analyses[[i]])
    if (is.na(planned_analyses) || planned_analyses < 1L) {
      planned_analyses <- plan_tbl$planned_analyses[[i]]
    }
    rule <- read_scalar_character_input(paste0("gs_plan_rule_", id))
    if (is.null(rule)) {
      rule <- plan_tbl$alpha_spending[[i]]
    }
    rule <- normalize_spending_rule(rule)
    custom_value <- read_scalar_character_input(paste0("gs_plan_custom_", id), reactive = FALSE)
    if (is.null(custom_value)) {
      custom_value <- plan_tbl$custom_cumulative_alpha[[i]]
    }
    hsd_gamma_value <- read_scalar_numeric_input(paste0("gs_plan_gamma_", id), reactive = FALSE)
    if (is.na(hsd_gamma_value)) {
      hsd_gamma_value <- if ("hsd_gamma" %in% names(plan_tbl)) plan_tbl$hsd_gamma[[i]] else -4
    }
    haybittle_p1_value <- read_scalar_numeric_input(paste0("gs_plan_haybittle_p1_", id), reactive = FALSE)
    if (is.na(haybittle_p1_value)) {
      haybittle_p1_value <- if ("haybittle_p1" %in% names(plan_tbl)) plan_tbl$haybittle_p1[[i]] else 3e-04
    }
    tibble::tibble(
      id = id,
      hypothesis = plan_tbl$hypothesis[[i]],
      planned_analyses = as.integer(planned_analyses),
      alpha_spending = rule,
      custom_cumulative_alpha = custom_value,
      hsd_gamma = hsd_gamma_value,
      haybittle_p1 = haybittle_p1_value
    )
  })
  out <- sanitize_gs_hypothesis_plan_tbl(dplyr::bind_rows(rows))
  if (isTRUE(persist)) {
    rv$gs_hypothesis_plan <- out
  }
  out
}

build_default_gs_analysis_schedule <- function(plan_tbl = rv$gs_hypothesis_plan) {
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  if (!nrow(plan_tbl)) {
    return(empty_gs_analysis_schedule())
  }
  total_rounds <- max(plan_tbl$planned_analyses, na.rm = TRUE)
  rows <- lapply(seq_len(nrow(plan_tbl)), function(i) {
    planned_analyses <- max(1L, as.integer(plan_tbl$planned_analyses[[i]]))
    stage_index <- seq_len(planned_analyses)
    rounds <- gs_default_rounds(planned_analyses, total_rounds)
    if (planned_analyses == 1L) {
      fractions <- 1
    } else {
      fractions <- stage_index / planned_analyses
    }
    schedule_keys <- vapply(stage_index, function(stage) {
      gs_schedule_key(plan_tbl$id[[i]], stage)
    }, character(1))
    tibble::tibble(
      schedule_key = schedule_keys,
      analysis_round = as.integer(rounds),
      hypothesis = plan_tbl$hypothesis[[i]],
      hypothesis_id = plan_tbl$id[[i]],
      hypothesis_stage = stage_index,
      planned_analyses = planned_analyses,
      information_fraction = as.numeric(fractions),
      is_final = stage_index == planned_analyses
    )
  })
  sanitize_gs_analysis_schedule_tbl(dplyr::bind_rows(rows))
}

merge_gs_schedule_with_existing <- function(
  default_tbl,
  existing_tbl = isolate(rv$gs_analysis_schedule)
) {
  default_tbl <- sanitize_gs_analysis_schedule_tbl(default_tbl)
  existing_tbl <- sanitize_gs_analysis_schedule_tbl(existing_tbl)
  if (!nrow(default_tbl)) {
    return(default_tbl)
  }
  rows <- lapply(unique(default_tbl$hypothesis_id), function(hypothesis_id) {
    default_rows <- default_tbl %>%
      dplyr::filter(hypothesis_id == !!hypothesis_id) %>%
      dplyr::arrange(hypothesis_stage)
    existing_rows <- existing_tbl %>%
      dplyr::filter(hypothesis_id == !!hypothesis_id) %>%
      dplyr::arrange(hypothesis_stage)

    same_shape <- nrow(existing_rows) == nrow(default_rows) &&
      nrow(default_rows) > 0L &&
      identical(existing_rows$schedule_key, default_rows$schedule_key) &&
      identical(existing_rows$planned_analyses, default_rows$planned_analyses)

    if (!same_shape) {
      return(default_rows)
    }

    for (i in seq_len(nrow(default_rows))) {
      default_rows$analysis_round[[i]] <- existing_rows$analysis_round[[i]]
      default_rows$information_fraction[[i]] <- existing_rows$information_fraction[[i]]
    }

    default_rows
  })
  sanitize_gs_analysis_schedule_tbl(dplyr::bind_rows(rows))
}

collect_gs_analysis_schedule <- function(plan_tbl = collect_gs_hypothesis_plan(persist = FALSE), persist = FALSE) {
  default_tbl <- merge_gs_schedule_with_existing(
    build_default_gs_analysis_schedule(plan_tbl),
    isolate(rv$gs_analysis_schedule)
  )
  if (!nrow(default_tbl)) {
    if (isTRUE(persist)) {
      rv$gs_analysis_schedule <- default_tbl
      set_gs_analysis_schedule_round_signature(default_tbl)
    }
    return(default_tbl)
  }
  rows <- lapply(seq_len(nrow(default_tbl)), function(i) {
    key <- default_tbl$schedule_key[[i]]
    analysis_round <- read_scalar_integer_input(
      paste0("gs_schedule_round_", key),
      default = default_tbl$analysis_round[[i]],
      reactive = FALSE
    )
    if (is.na(analysis_round) || analysis_round < 1L) {
      analysis_round <- default_tbl$analysis_round[[i]]
    }
    info_fraction <- read_scalar_numeric_input(paste0("gs_schedule_info_", key), reactive = FALSE)
    if (is.na(info_fraction)) {
      info_fraction <- default_tbl$information_fraction[[i]]
    }
    tibble::tibble(
      schedule_key = key,
      analysis_round = as.integer(analysis_round),
      hypothesis = default_tbl$hypothesis[[i]],
      hypothesis_id = default_tbl$hypothesis_id[[i]],
      hypothesis_stage = default_tbl$hypothesis_stage[[i]],
      planned_analyses = default_tbl$planned_analyses[[i]],
      information_fraction = as.numeric(info_fraction),
      is_final = isTRUE(default_tbl$is_final[[i]])
    )
  })
  out <- sanitize_gs_analysis_schedule_tbl(dplyr::bind_rows(rows))
  if (isTRUE(persist)) {
    rv$gs_analysis_schedule <- out
    set_gs_analysis_schedule_round_signature(out)
  }
  out
}

validate_gs_analysis_schedule <- function(
  schedule_tbl = collect_gs_analysis_schedule(persist = FALSE),
  plan_tbl = collect_gs_hypothesis_plan(persist = FALSE),
  current_alpha = get_current_allocations(),
  allow_custom_alpha_mismatch = FALSE,
  notify = FALSE
) {
  schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
  plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
  current_alpha <- stats::setNames(as.numeric(current_alpha), names(current_alpha))
  design_alpha_lookup <- gs_design_alpha_lookup(fallback = current_alpha)
  if (!nrow(plan_tbl)) {
    msg <- "Create hypotheses in the Design tab before defining a group sequential schedule."
    if (isTRUE(notify)) {
      showNotification(msg, type = "error", duration = 8)
    }
    return(list(ok = FALSE, message = msg, schedule = empty_gs_analysis_schedule()))
  }
  if (!nrow(schedule_tbl)) {
    msg <- "The analysis schedule is empty."
    if (isTRUE(notify)) {
      showNotification(msg, type = "error", duration = 8)
    }
    return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
  }
  for (i in seq_len(nrow(plan_tbl))) {
    hypothesis_rows <- schedule_tbl %>%
      dplyr::filter(hypothesis_id == plan_tbl$id[[i]]) %>%
      dplyr::arrange(hypothesis_stage)
    expected_analyses <- as.integer(plan_tbl$planned_analyses[[i]])
    if (nrow(hypothesis_rows) != expected_analyses) {
      msg <- sprintf("%s must have %s scheduled analysis row%s.", plan_tbl$hypothesis[[i]], expected_analyses, ifelse(expected_analyses == 1L, "", "s"))
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (any(!is.finite(hypothesis_rows$analysis_round)) || any(hypothesis_rows$analysis_round < 1L)) {
      msg <- sprintf("%s analysis rounds must be positive integers.", plan_tbl$hypothesis[[i]])
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (any(diff(hypothesis_rows$analysis_round) <= 0L)) {
      msg <- sprintf("%s analysis rounds must increase from one stage to the next.", plan_tbl$hypothesis[[i]])
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (any(!is.finite(hypothesis_rows$information_fraction)) || any(hypothesis_rows$information_fraction <= 0) || any(hypothesis_rows$information_fraction > 1 + 1e-8)) {
      msg <- sprintf("%s information fractions must stay within (0, 1].", plan_tbl$hypothesis[[i]])
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (expected_analyses > 1L) {
      non_final_rows <- hypothesis_rows %>% dplyr::filter(!is_final)
      invalid_idx <- which(
        non_final_rows$information_fraction <= 0 |
          non_final_rows$information_fraction >= 1
      )
      if (length(invalid_idx)) {
        invalid_stage <- non_final_rows$hypothesis_stage[[invalid_idx[[1]]]]
        msg <- sprintf(
          "%s stage %s information fraction must be strictly between 0 and 1 for non-final analyses.",
          plan_tbl$hypothesis[[i]],
          invalid_stage
        )
        if (isTRUE(notify)) {
          showNotification(msg, type = "error", duration = 8)
        }
        return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
      }
    }
    if (any(diff(hypothesis_rows$information_fraction) <= 0)) {
      msg <- "Information fractions for interim analyses must be in (0, 1) and incremental."
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (abs(tail(hypothesis_rows$information_fraction, 1) - 1) > 1e-8) {
      msg <- sprintf("%s must end at information fraction 1.0.", plan_tbl$hypothesis[[i]])
      if (isTRUE(notify)) {
        showNotification(msg, type = "error", duration = 8)
      }
      return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
    }
    if (identical(plan_tbl$alpha_spending[[i]], "Custom")) {
      alpha_now <- as.numeric(design_alpha_lookup[[plan_tbl$hypothesis[[i]]]])
      if (length(alpha_now) == 1L && is.finite(alpha_now) && alpha_now > 0) {
        spend_info <- parse_custom_cumulative_alpha(
          plan_tbl$custom_cumulative_alpha[[i]],
          expected_analyses,
          total_alpha = alpha_now,
          allow_legacy_proportions = FALSE,
          allow_total_mismatch = isTRUE(allow_custom_alpha_mismatch)
        )
        if (!isTRUE(spend_info$ok)) {
          msg <- sprintf("%s custom cumulative alpha error: %s", plan_tbl$hypothesis[[i]], spend_info$message)
          if (isTRUE(notify)) {
            showNotification(msg, type = "error", duration = 8)
          }
          return(list(ok = FALSE, message = msg, schedule = schedule_tbl))
        }
      }
    }
  }
  list(
    ok = TRUE,
    message = NULL,
    schedule = schedule_tbl %>% dplyr::arrange(analysis_round, hypothesis, hypothesis_stage)
  )
}

observe({
  node_signature <- paste(rv$nodes$id, rv$nodes$hypothesis, collapse = "|")
  if (!nzchar(node_signature) && !nrow(rv$nodes)) {
    rv$gs_hypothesis_plan <- empty_gs_hypothesis_plan()
    rv$gs_analysis_schedule <- empty_gs_analysis_schedule()
    set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
    rv$gs_settings <- legacy_settings_from_group_sequential_design()
    return()
  }
  rv$gs_hypothesis_plan <- build_default_gs_hypothesis_plan(rv$nodes, isolate(rv$gs_hypothesis_plan))
  rv$gs_analysis_schedule <- merge_gs_schedule_with_existing(
    build_default_gs_analysis_schedule(rv$gs_hypothesis_plan),
    isolate(rv$gs_analysis_schedule)
  )
  set_gs_analysis_schedule_round_signature(rv$gs_analysis_schedule)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(rv$gs_hypothesis_plan, rv$gs_analysis_schedule)
})

observe({
  if (!nrow(rv$nodes)) {
    return()
  }
  plan_tbl <- collect_gs_hypothesis_plan(persist = FALSE)
  schedule_tbl <- merge_gs_schedule_with_existing(
    build_default_gs_analysis_schedule(plan_tbl),
    isolate(rv$gs_analysis_schedule)
  )
  plan_changed <- !same_gs_hypothesis_plan_tbl(plan_tbl, isolate(rv$gs_hypothesis_plan))
  schedule_changed <- !same_gs_analysis_schedule_tbl(schedule_tbl, isolate(rv$gs_analysis_schedule))
  if (!plan_changed && !schedule_changed) {
    return()
  }
  rv$gs_hypothesis_plan <- plan_tbl
  rv$gs_analysis_schedule <- schedule_tbl
  set_gs_analysis_schedule_round_signature(schedule_tbl)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(plan_tbl, schedule_tbl)
})

observe({
  if (!nrow(rv$gs_hypothesis_plan)) {
    empty_schedule <- empty_gs_analysis_schedule()
    current_settings <- isolate(rv$gs_settings)
    has_settings_rows <- !is.null(current_settings) && nrow(current_settings) > 0
    schedule_changed <- !same_gs_analysis_schedule_tbl(empty_schedule, isolate(rv$gs_analysis_schedule))
    if (schedule_changed) {
      rv$gs_analysis_schedule <- empty_schedule
      set_gs_analysis_schedule_round_signature(empty_schedule)
    }
    if (schedule_changed || has_settings_rows) {
      rv$gs_settings <- legacy_settings_from_group_sequential_design()
    }
    return()
  }
  schedule_tbl <- collect_gs_analysis_schedule(
    plan_tbl = isolate(rv$gs_hypothesis_plan),
    persist = FALSE
  )
  if (same_gs_analysis_schedule_tbl(schedule_tbl, isolate(rv$gs_analysis_schedule))) {
    return()
  }
  rv$gs_analysis_schedule <- schedule_tbl
  set_gs_analysis_schedule_round_signature(schedule_tbl)
  rv$gs_settings <- legacy_settings_from_group_sequential_design(rv$gs_hypothesis_plan, schedule_tbl)
})
