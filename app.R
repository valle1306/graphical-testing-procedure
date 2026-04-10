library(shiny)
library(visNetwork)
library(shinyjs)
library(dplyr)
library(DT)
library(TrialSimulator)
library(jsonlite)

for (helper_file in c("alpha_spending_function.r", "Haybittle-Peto.r")) {
  helper_path <- file.path(getwd(), helper_file)
  if (file.exists(helper_path)) {
    sys.source(helper_path, envir = globalenv())
  }
}

for (ui_module in c(
  file.path("R", "ui", "home_tab.R"),
  file.path("R", "ui", "design_tab.R"),
  file.path("R", "ui", "group_sequential_design_tab.R"),
  file.path("R", "ui", "analysis_tab.R")
)) {
  ui_module_path <- file.path(getwd(), ui_module)
  if (file.exists(ui_module_path)) {
    sys.source(ui_module_path, envir = globalenv())
  }
}

is_named_vector_jsonlite_warning <- function(message) {
  grepl("keep_vec_names=TRUE", message, fixed = TRUE) ||
    grepl("named vectors will be translated into arrays", message, fixed = TRUE) ||
    grepl("Input to asJSON", message, fixed = TRUE)
}

if (!isTRUE(getOption("gmt.jsonlite_warning_handler_registered"))) {
  options(gmt.jsonlite_warning_handler_registered = TRUE)
}

ui <- navbarPage(
  "Graphical Multiple Testing",
  id = "nav",
  theme = bslib::bs_theme(
    version = 5,
    primary = "#0F766E", secondary = "#D97706",
    base_font   = bslib::font_google("Source Sans 3"),
    heading_font= bslib::font_google("Source Sans 3"),
    "font-size-base" = "0.93rem",
    "line-height-base" = 1.45
  )
  ,
  build_home_tab(),
  build_design_tab(),
  build_group_sequential_design_tab(),
  build_analysis_tab()
)

server <- function(input, output, session) {
  
  focus_and_select <- function(input_id) {
    shinyjs::runjs(sprintf("
    $('#shiny-modal').one('shown.bs.modal', function(){
      var el = document.getElementById('%s');
      if (el) { el.focus(); el.select(); }
    });", input_id))
  }
  
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
  
  read_scalar_character_input <- function(input_id, reactive = TRUE) {
    value <- if (isTRUE(reactive)) input[[input_id]] else isolate(input[[input_id]])
    if (is.null(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
      return(NULL)
    }
    as.character(value)
  }
  
  read_scalar_numeric_input <- function(input_id, reactive = TRUE) {
    raw_value <- if (isTRUE(reactive)) input[[input_id]] else isolate(input[[input_id]])
    if (is.null(raw_value) || length(raw_value) != 1) {
      return(NA_real_)
    }
    value <- suppressWarnings(as.numeric(raw_value))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      return(NA_real_)
    }
    value
  }

  read_scalar_integer_input <- function(input_id, default = NA_integer_, reactive = TRUE) {
    raw_value <- if (isTRUE(reactive)) input[[input_id]] else isolate(input[[input_id]])
    if (is.null(raw_value) || length(raw_value) < 1) {
      return(default)
    }
    value <- suppressWarnings(as.integer(raw_value[[1]]))
    if (length(value) != 1 || is.na(value) || !is.finite(value)) {
      return(default)
    }
    value
  }

  scalar_text_or_default <- function(value, default = "") {
    if (is.null(value) || !length(value) || is.na(value[[1]])) {
      return(default)
    }
    as.character(value[[1]])
  }

  same_trimmed_text <- function(left, right) {
    identical(trimws(scalar_text_or_default(left)), trimws(scalar_text_or_default(right)))
  }

  same_sanitized_table <- function(left, right, sanitizer_name = NULL) {
    if (!is.null(sanitizer_name) && exists(sanitizer_name, mode = "function")) {
      sanitizer <- get(sanitizer_name, mode = "function")
      left <- sanitizer(left)
      right <- sanitizer(right)
    }
    left <- as.data.frame(left, stringsAsFactors = FALSE)
    right <- as.data.frame(right, stringsAsFactors = FALSE)
    isTRUE(all.equal(left, right, check.attributes = FALSE))
  }

  same_gs_hypothesis_plan_tbl <- function(left, right) {
    same_sanitized_table(left, right, sanitizer_name = "sanitize_gs_hypothesis_plan_tbl")
  }

  same_gs_analysis_schedule_tbl <- function(left, right) {
    same_sanitized_table(left, right, sanitizer_name = "sanitize_gs_analysis_schedule_tbl")
  }

  same_gs_boundary_preview_tbl <- function(left, right) {
    same_sanitized_table(left, right, sanitizer_name = "sanitize_gs_boundary_preview_tbl")
  }

  reactivity_debug_enabled <- function() {
    isTRUE(getOption("gmt.debug.reactivity", FALSE)) ||
      identical(Sys.getenv("GMT_DEBUG_REACTIVITY"), "1")
  }

  reactivity_debug_state <- new.env(parent = emptyenv())
  reactivity_debug_state$metrics <- list()

  profile_reactivity <- function(label, expr, note = NULL) {
    if (!isTRUE(reactivity_debug_enabled())) {
      return(force(expr))
    }

    start_time <- proc.time()[["elapsed"]]
    note_text <- if (!is.null(note) && length(note)) {
      trimws(as.character(note[[1]]))
    } else {
      ""
    }

    on.exit({
      elapsed_ms <- (proc.time()[["elapsed"]] - start_time) * 1000
      metric <- reactivity_debug_state$metrics[[label]]
      if (is.null(metric)) {
        metric <- list(calls = 0L, total_ms = 0, last_ms = 0, note = "")
      }
      metric$calls <- as.integer(metric$calls) + 1L
      metric$total_ms <- as.numeric(metric$total_ms) + elapsed_ms
      metric$last_ms <- elapsed_ms
      metric$note <- note_text
      reactivity_debug_state$metrics[[label]] <- metric

      message(
        sprintf(
          "[gmt-reactivity] %s call=%s elapsed=%.1fms%s",
          label,
          metric$calls,
          elapsed_ms,
          if (nzchar(note_text)) paste0(" note=", note_text) else ""
        )
      )
    }, add = TRUE)

    force(expr)
  }

  snapshot_reactivity_metrics <- function(label = NULL) {
    metrics <- reactivity_debug_state$metrics
    if (!length(metrics)) {
      return(tibble::tibble(
        label = character(),
        calls = integer(),
        total_ms = numeric(),
        last_ms = numeric(),
        note = character()
      ))
    }

    rows <- lapply(names(metrics), function(metric_label) {
      metric <- metrics[[metric_label]]
      tibble::tibble(
        label = metric_label,
        calls = as.integer(metric$calls),
        total_ms = as.numeric(metric$total_ms),
        last_ms = as.numeric(metric$last_ms),
        note = as.character(metric$note)
      )
    })

    out <- dplyr::bind_rows(rows)
    if (!is.null(label)) {
      out <- out %>% dplyr::filter(.data$label == !!as.character(label))
    }
    out %>% dplyr::arrange(.data$label)
  }

  reset_reactivity_metrics <- function() {
    reactivity_debug_state$metrics <- list()
    invisible(NULL)
  }

  output_is_visible <- function(output_id) {
    hidden_flag <- session$clientData[[paste0("output_", output_id, "_hidden")]]
    !isTRUE(hidden_flag)
  }

  boundary_preview_cache_key <- function(
    plan_tbl = rv$gs_hypothesis_plan,
    schedule_tbl = rv$gs_analysis_schedule,
    ts_state_value = ts_state_tick()
  ) {
    paste(
      if (exists("gs_current_design_signature", mode = "function")) {
        gs_current_design_signature(plan_tbl, schedule_tbl)
      } else {
        ""
      },
      as.integer(ts_state_value),
      sep = "||"
    )
  }

  sync_group_sequential_inputs <- function(
    plan_tbl = rv$gs_hypothesis_plan,
    schedule_tbl = rv$gs_analysis_schedule
  ) {
    if (!exists("sanitize_gs_hypothesis_plan_tbl", mode = "function")) {
      return(invisible(NULL))
    }
    plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
    schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)
    alpha_lookup <- gs_design_alpha_lookup(fallback = get_current_allocations())

    if (nrow(plan_tbl)) {
      for (i in seq_len(nrow(plan_tbl))) {
        id <- plan_tbl$id[[i]]
        planned_analyses <- max(1L, as.integer(plan_tbl$planned_analyses[[i]]))
        rule <- normalize_spending_rule(plan_tbl$alpha_spending[[i]])
        alpha_now <- as.numeric(alpha_lookup[[plan_tbl$hypothesis[[i]]]])

        updateNumericInput(
          session,
          paste0("gs_plan_k_", id),
          value = planned_analyses
        )
        updateSelectInput(
          session,
          paste0("gs_plan_rule_", id),
          selected = rule
        )
        updateTextInput(
          session,
          paste0("gs_plan_custom_", id),
          value = scalar_text_or_default(plan_tbl$custom_cumulative_alpha[[i]]),
          placeholder = gs_custom_cumulative_alpha_placeholder(planned_analyses, alpha_now)
        )
        updateTextInput(
          session,
          paste0("gs_plan_gamma_", id),
          value = if (is.finite(plan_tbl$hsd_gamma[[i]]) && plan_tbl$hsd_gamma[[i]] != -4) format_plain_number(plan_tbl$hsd_gamma[[i]]) else "",
          placeholder = "e.g. -4"
        )
        updateTextInput(
          session,
          paste0("gs_plan_haybittle_p1_", id),
          value = if (is.finite(plan_tbl$haybittle_p1[[i]])) format_plain_number(plan_tbl$haybittle_p1[[i]]) else "0.0003",
          placeholder = "e.g. 0.0003"
        )
      }
    }

    if (nrow(schedule_tbl)) {
      for (i in seq_len(nrow(schedule_tbl))) {
        key <- schedule_tbl$schedule_key[[i]]
        updateNumericInput(
          session,
          paste0("gs_schedule_round_", key),
          value = schedule_tbl$analysis_round[[i]]
        )
        updateTextInput(
          session,
          paste0("gs_schedule_info_", key),
          value = format_plain_number(schedule_tbl$information_fraction[[i]]),
          placeholder = "e.g. 0.5"
        )
      }
    }

    invisible(NULL)
  }

  persist_group_sequential_design_state <- function(update_boundary = FALSE) {
    plan_tbl <- collect_gs_hypothesis_plan(persist = TRUE)
    schedule_tbl <- collect_gs_analysis_schedule(plan_tbl = plan_tbl, persist = TRUE)
    rv$gs_settings <- legacy_settings_from_group_sequential_design(plan_tbl, schedule_tbl)
    if (isTRUE(update_boundary)) {
      rv$gs_boundary_preview <- build_gs_boundary_schedule(
        plan_tbl = plan_tbl,
        schedule_tbl = schedule_tbl,
        notify = FALSE
      )
    }
    invisible(list(plan = plan_tbl, schedule = schedule_tbl))
  }
  
  quiet_jsonlite_warning <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        msg <- conditionMessage(w)
        if (is_named_vector_jsonlite_warning(msg)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  
  with_edge_label <- function(df) {
    if (nrow(df) == 0) return(df)
    key        <- paste(pmin(df$from, df$to), pmax(df$from, df$to), sep = "_")
    dup_counts <- ave(key, key, FUN = length)
    has_pair   <- dup_counts > 1
    smooth_list <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
      if (has_pair[i]) {
        smooth_list[[i]] <- list(enabled = TRUE, type = "curvedCW", roundness = 0.20)
      } else {
        smooth_list[[i]] <- list(enabled = FALSE)
      }
    }
    out <- df
    out$label <- format(df$weight, trim = TRUE, scientific = FALSE)
    out$arrows <- "to"
    out$smooth <- smooth_list
    out$hoverWidth     <- 3
    out$selectionWidth <- 3
    out$font <- lapply(seq_len(nrow(df)), function(i) list(background = "white"))
    out
  }

  sanitize_nodes_tbl <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(tibble::as_tibble(df))
    }
    out <- tibble::as_tibble(df)
    for (col in intersect(c("id", "x", "y", "alpha"), names(out))) {
      out[[col]] <- unname(out[[col]])
    }
    out
  }

  sanitize_edges_tbl <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(tibble::as_tibble(df))
    }
    out <- tibble::as_tibble(df)
    for (col in intersect(c("id", "from", "to", "weight"), names(out))) {
      out[[col]] <- unname(out[[col]])
    }
    out
  }

  set_ts_log <- function(...) {
    log_lines <- unlist(list(...), use.names = FALSE)
    log_lines <- trimws(as.character(log_lines))
    rv$ts_log <- paste(log_lines[nzchar(log_lines)], collapse = "\n")
    invisible(NULL)
  }

  build_design_reject_log <- function(selected_hypothesis) {
    c(
      sprintf("Rejected %s in the graphical procedure.", selected_hypothesis),
      format_alpha_snapshot()
    )
  }

  validate_transition_matrix <- function(mat, hypotheses = rownames(mat), tol = 1e-6) {
    if (is.null(mat) || !length(mat)) {
      return(list(valid = TRUE, message = NULL))
    }
    row_sums <- rowSums(mat)
    invalid_rows <- which(!(abs(row_sums) < tol | abs(row_sums - 1) < tol))
    if (!length(invalid_rows)) {
      return(list(valid = TRUE, message = NULL))
    }
    if (is.null(hypotheses) || length(hypotheses) != nrow(mat)) {
      hypotheses <- paste0("Row ", seq_len(nrow(mat)))
    }
    details <- paste(
      vapply(
        invalid_rows,
        function(i) sprintf("%s = %s", hypotheses[i], format_plain_number(row_sums[i])),
        character(1)
      ),
      collapse = "; "
    )
    list(
      valid = FALSE,
      message = paste0(
        "Outgoing edge weights must sum to 0 or 1 for each hypothesis. ",
        "Invalid totals: ", details,
        ". If a hypothesis has outgoing edges, make their weights add to 1."
      )
    )
  }
  
  next_hypothesis <- function(existing) {
    k <- 1L
    while (paste0("H", k) %in% existing) k <- k + 1L
    paste0("H", k)
  }
  
  is_valid_alpha_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0(\\.\\d+)?|1(\\.0+)?)$", s)
  }
  
  is_valid_weight_str <- function(s) {
    if (is.null(s) || !is.character(s) || length(s) != 1) return(FALSE)
    grepl("^(0\\.[0-9]+|1(\\.0+)?)$", s)
  }
  
  cancel_pending_js <- "Shiny.setInputValue('cancel_pending', Math.random(), {priority:'event'});"
  graph_view_cache <- new.env(parent = emptyenv())
  graph_view_cache$graph <- list(content_signature = NULL, position_map = stats::setNames(character(0), character(0)))
  graph_view_cache$seq_graph <- list(content_signature = NULL, position_map = stats::setNames(character(0), character(0)))
  boundary_preview_cache <- new.env(parent = emptyenv())
  boundary_preview_cache$key <- NULL
  
  rv <- reactiveValues(
    nodes = tibble::tibble(
      id    = 1:3,
      x     = c(-150, 0, 150),
      y     = c(-40, 40, -20),
      hypothesis = c("H1", "H2", "H3"),
      alpha      = c(0, 0, 0)
    ),
    edges = tibble::tibble(
      id = integer(), from = integer(), to = integer(), weight = numeric()
    ),
    ctx = list(node = NULL, edge = NULL, canvas = c(0,0), 
               edit_node_id = NULL, edit_edge_id = NULL, pending_delete_edge_id = NULL),
    pending_source = NULL,
    ts_object = NULL,
    ts_log = "",
    ts_summary = NULL,
    gs_settings = tibble::tibble(
      id = 1:3,
      hypothesis = c("H1", "H2", "H3"),
      use_override = rep(FALSE, 3),
      alpha_spending = rep("OF", 3),
      planned_analyses = rep(2L, 3),
      info_timing = rep("0.5, 1", 3),
      spending_values = rep("", 3),
      haybittle_p1 = rep(0.0003, 3)
    ),
    gs_boundary_preview = tibble::tibble(
      hypothesis = character(),
      alpha_spending = character(),
      planned_analyses = integer(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      stage_alpha = numeric(),
      cumulative_alpha_spent = numeric(),
      z_boundary = numeric(),
      p_boundary = numeric(),
      status = character(),
      analysis_round = integer(),
      hypothesis_stage = integer(),
      schedule_key = character(),
      is_final = logical(),
      max_info = numeric()
    ),
    gs_boundary_preview_message = NULL,
    gs_stage_history = tibble::tibble(
      submission = integer(),
      hypothesis = character(),
      alpha_spending = character(),
      analysis = integer(),
      timing = numeric(),
      current_alpha = numeric(),
      p_value = numeric(),
      boundary_p = numeric(),
      boundary_z = numeric(),
      decision = character()
    ),
    gs_hypothesis_plan = tibble::tibble(
      id = integer(),
      hypothesis = character(),
      planned_analyses = integer(),
      alpha_spending = character(),
      custom_cumulative_alpha = character(),
      hsd_gamma = numeric(),
      haybittle_p1 = numeric()
    ),
    gs_analysis_schedule = tibble::tibble(
      schedule_key = character(),
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_id = integer(),
      hypothesis_stage = integer(),
      planned_analyses = integer(),
      information_fraction = numeric(),
      is_final = logical()
    ),
    gs_analysis_schedule_round_signature = "",
    gs_analysis_history = tibble::tibble(
      submission = integer(),
      schedule_key = character(),
      analysis_round = integer(),
      hypothesis = character(),
      hypothesis_stage = integer(),
      alpha_spending = character(),
      runtime_spending_code = character(),
      information_fraction = numeric(),
      current_alpha = numeric(),
      cumulative_alpha_spent = numeric(),
      p_value = numeric(),
      boundary_p = numeric(),
      boundary_z = numeric(),
      decision = character(),
      is_final = logical(),
      max_info = numeric()
    ),
    gs_round_feedback = NULL,
    gs_finalize_feedback = NULL,
    gs_design_finalized = FALSE,
    gs_applied_design_signature = "",
    gs_wizard_step = 1L,
    gs_round_selection_programmatic = FALSE,
    gs_force_first_actionable_round = FALSE,
    alpha_spending = character(0),
    planned_max_info = numeric(0),
    transition = matrix(0, 0, 0)
  )
  
  update_manual_reject_choices <- function(choices = rv$nodes$hypothesis) {
    selected_choice <- if (length(choices)) choices[[1]] else character(0)
    updateSelectInput(session, "design_graph_selected", choices = choices, selected = selected_choice)
  }

  update_sequential_test_choices <- function(choices = NULL) {
    if (is.null(choices)) {
      status_tbl <- build_ts_status_table()
      choices <- if (!is.null(status_tbl) && nrow(status_tbl)) {
        status_tbl$hypothesis[status_tbl$in_graph]
      } else {
        rv$nodes$hypothesis
      }
    }
    selected_choice <- isolate(input$gs_test_hypothesis)
    if (is.null(selected_choice) || !selected_choice %in% choices) {
      selected_choice <- if (length(choices)) choices[[1]] else character(0)
    }
    updateSelectInput(session, "gs_test_hypothesis", choices = choices, selected = selected_choice)
  }
  
  tables_tick <- reactiveVal(0)
  bump_tables <- function() tables_tick(tables_tick() + 1)
  ts_state_tick <- reactiveVal(0)
  bump_ts_state <- function() ts_state_tick(ts_state_tick() + 1)
  
  get_current_allocations <- function() {
    ts_state_tick()
    if (is.null(rv$ts_object)) {
      return(stats::setNames(rv$nodes$alpha, rv$nodes$hypothesis))
    }
    stats::setNames(
      vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch({
          hid <- rv$ts_object$get_hid(hyp)
          if (!rv$ts_object$is_in_graph(hid)) {
            return(0)
          }
          rv$ts_object$get_alpha(hid)
        }, error = function(e) {
          rv$nodes$alpha[match(hyp, rv$nodes$hypothesis)]
        })
      }, numeric(1)),
      rv$nodes$hypothesis
    )
  }
  
  build_ts_status_table <- function() {
    ts_state_tick()
    if (!nrow(rv$nodes)) {
      return(NULL)
    }
    allocations <- if (is.null(rv$ts_object)) {
      stats::setNames(rv$nodes$alpha, rv$nodes$hypothesis)
    } else {
      get_current_allocations()
    }
    in_graph <- if (is.null(rv$ts_object)) {
      rep(TRUE, nrow(rv$nodes))
    } else {
      vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_in_graph(rv$ts_object$get_hid(hyp)), error = function(e) TRUE)
      }, logical(1))
    }
    testable <- if (is.null(rv$ts_object)) {
      rv$nodes$alpha > 0
    } else {
      vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_testable(rv$ts_object$get_hid(hyp)), error = function(e) FALSE)
      }, logical(1))
    }
    decision_map <- if (nrow(rv$gs_analysis_history)) {
      latest_gs_history_decision_map(rv$gs_analysis_history, rv$nodes$hypothesis)
    } else {
      stats::setNames(rep("Pending", nrow(rv$nodes)), rv$nodes$hypothesis)
    }
    status_tbl <- tibble::tibble(
      hypothesis = rv$nodes$hypothesis,
      current_alpha = as.numeric(allocations[rv$nodes$hypothesis]),
      decision = unname(decision_map[rv$nodes$hypothesis]),
      in_graph = in_graph,
      testable = testable
    )
    status_tbl$decision[!status_tbl$in_graph] <- "Reject"
    if (is.null(rv$ts_object)) {
      return(status_tbl)
    }
    status_tbl
  }
  
  for (server_module in c(
    file.path("R", "server", "common_helpers.R"),
    file.path("R", "server", "graph_events.R"),
    file.path("R", "server", "sequential_settings.R"),
    file.path("R", "server", "sequential_boundaries.R"),
    file.path("R", "server", "sequential_state.R"),
    file.path("R", "server", "sequential_outputs.R"),
    file.path("R", "server", "sequential_events.R")
  )) {
    server_module_path <- file.path(getwd(), server_module)
    if (file.exists(server_module_path)) {
      source(server_module_path, local = TRUE)
    }
  }
}

shinyApp(ui, server)
