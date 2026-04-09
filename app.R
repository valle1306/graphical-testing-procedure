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
    format(as.numeric(x), trim = TRUE, scientific = FALSE)
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

  sync_group_sequential_inputs <- function(
    plan_tbl = rv$gs_hypothesis_plan,
    schedule_tbl = rv$gs_analysis_schedule
  ) {
    if (!exists("sanitize_gs_hypothesis_plan_tbl", mode = "function")) {
      return(invisible(NULL))
    }
    plan_tbl <- sanitize_gs_hypothesis_plan_tbl(plan_tbl)
    schedule_tbl <- sanitize_gs_analysis_schedule_tbl(schedule_tbl)

    if (nrow(plan_tbl)) {
      for (i in seq_len(nrow(plan_tbl))) {
        id <- plan_tbl$id[[i]]
        planned_analyses <- max(1L, as.integer(plan_tbl$planned_analyses[[i]]))
        rule <- normalize_spending_rule(plan_tbl$alpha_spending[[i]])

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
          placeholder = sprintf("e.g. 0.001, 0.025 for %s analyses", planned_analyses)
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
      status = character()
    ),
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
    if (is.null(rv$ts_object)) {
      return(tibble::tibble(
        hypothesis = rv$nodes$hypothesis,
        current_alpha = as.numeric(rv$nodes$alpha),
        decision = rep("ready", nrow(rv$nodes)),
        in_graph = rep(TRUE, nrow(rv$nodes)),
        testable = rv$nodes$alpha > 0
      ))
    }
    decisions <- tryCatch(rv$ts_object$get_current_decision(), error = function(e) {
      stats::setNames(rep("accept", nrow(rv$nodes)), rv$nodes$hypothesis)
    })
    allocations <- get_current_allocations()
    status_tbl <- tibble::tibble(
      hypothesis = rv$nodes$hypothesis,
      current_alpha = as.numeric(allocations[rv$nodes$hypothesis]),
      decision = unname(decisions[rv$nodes$hypothesis]),
      in_graph = vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_in_graph(rv$ts_object$get_hid(hyp)), error = function(e) TRUE)
      }, logical(1)),
      testable = vapply(rv$nodes$hypothesis, function(hyp) {
        tryCatch(rv$ts_object$is_testable(rv$ts_object$get_hid(hyp)), error = function(e) FALSE)
      }, logical(1))
    )
    status_tbl$decision[!status_tbl$in_graph] <- "reject"
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
