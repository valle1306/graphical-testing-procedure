# Verify JSON import compatibility for both app-native and lightweight formats
local_lib <- file.path(getwd(), ".Rlibs")
if (dir.exists(local_lib)) .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = TRUE), .libPaths()))

library(jsonlite)
library(dplyr)
library(tibble)

source(file.path("R", "server", "common_helpers.R"), local = TRUE)

# --- Helper: simulate the import normalization logic from graph_events.R ---
normalize_import <- function(json_path) {
  dat <- fromJSON(json_path, simplifyDataFrame = TRUE)

  coerce_import_df <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x) && !is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (is.data.frame(x)) {
      x[] <- lapply(x, function(col) if (is.list(col)) unlist(col, recursive = TRUE, use.names = FALSE) else col)
    }
    x
  }

  nodes <- coerce_import_df(dat$nodes)
  edges <- coerce_import_df(dat$edges)

  stopifnot("nodes must have rows" = !is.null(nodes) && nrow(nodes) > 0)

  # Normalize nodes

  if (is.null(nodes$hypothesis) && !is.null(nodes$label)) nodes$hypothesis <- as.character(nodes$label)
  if (is.null(nodes$hypothesis)) nodes$hypothesis <- paste0("H", seq_len(nrow(nodes)))
  if (is.null(nodes$id)) nodes$id <- seq_len(nrow(nodes)) else nodes$id <- as.integer(nodes$id)
  if (!is.null(nodes$alpha)) nodes$alpha <- as.numeric(nodes$alpha) else nodes$alpha <- rep(0, nrow(nodes))
  if (is.null(nodes$x) || is.null(nodes$y)) {
    nodes <- auto_layout_nodes(nodes)
  }
  nodes <- nodes[, intersect(c("id", "x", "y", "hypothesis", "alpha"), names(nodes)), drop = FALSE]

  # Normalize edges
  if (!is.null(edges) && nrow(edges)) {
    label_to_id <- stats::setNames(nodes$id, nodes$hypothesis)
    if (is.character(edges$from)) edges$from <- as.integer(label_to_id[edges$from]) else edges$from <- as.integer(edges$from)
    if (is.character(edges$to)) edges$to <- as.integer(label_to_id[edges$to]) else edges$to <- as.integer(edges$to)
    if (!is.null(edges$weight)) edges$weight <- as.numeric(edges$weight)
    else if (!is.null(edges$label)) edges$weight <- suppressWarnings(as.numeric(edges$label))
    if (is.null(edges$weight)) edges$weight <- rep(0, nrow(edges))
    if (is.null(edges$id)) edges$id <- seq_len(nrow(edges)) else edges$id <- as.integer(edges$id)
    valid_edges <- !is.na(edges$from) & !is.na(edges$to)
    edges <- edges[valid_edges, , drop = FALSE]
    edges <- edges[, intersect(c("id", "from", "to", "weight"), names(edges)), drop = FALSE]
  } else {
    edges <- tibble::tibble(id = integer(), from = integer(), to = integer(), weight = numeric())
  }

  list(nodes = tibble::as_tibble(nodes), edges = tibble::as_tibble(edges))
}

# ============================================================================
# Test 1: App-native format (upload_example.json)
# ============================================================================
cat("Test 1: App-native format import...\n")
native <- normalize_import(file.path("examples", "upload_example.json"))
stopifnot(nrow(native$nodes) == 4)
stopifnot(all(c("id", "x", "y", "hypothesis", "alpha") %in% names(native$nodes)))
stopifnot(identical(native$nodes$hypothesis, c("H1", "H2", "H3", "H4")))
stopifnot(identical(native$nodes$id, 1:4))
stopifnot(all(is.numeric(native$nodes$alpha)))
stopifnot(nrow(native$edges) == 7)
stopifnot(all(c("id", "from", "to", "weight") %in% names(native$edges)))
stopifnot(all(native$edges$from %in% native$nodes$id))
stopifnot(all(native$edges$to %in% native$nodes$id))
cat("  PASS: 4 nodes, 7 edges, all fields present\n")

# ============================================================================
# Test 2: HSD case format (hsd_case.json)
# ============================================================================
cat("Test 2: HSD case format import...\n")
hsd <- normalize_import(file.path("examples", "hsd_case.json"))
stopifnot(nrow(hsd$nodes) == 3)
stopifnot(all(c("id", "x", "y", "hypothesis", "alpha") %in% names(hsd$nodes)))
stopifnot(identical(hsd$nodes$hypothesis, c("H1", "H2", "H3")))
stopifnot(identical(hsd$nodes$id, 1:3))
stopifnot(all(is.numeric(hsd$nodes$x)))
stopifnot(all(is.numeric(hsd$nodes$y)))
stopifnot(nrow(hsd$edges) == 6)
stopifnot(all(hsd$edges$from %in% hsd$nodes$id))
stopifnot(all(hsd$edges$to %in% hsd$nodes$id))
cat("  PASS: 3 nodes, 6 edges, all fields present\n")

# ============================================================================
# Test 3: Auto-layout assigns valid coordinates
# ============================================================================
cat("Test 3: Auto-layout coordinates...\n")
stopifnot(all(is.finite(hsd$nodes$x)))
stopifnot(all(is.finite(hsd$nodes$y)))
cat("  PASS: all coordinates are finite\n")

# ============================================================================
# Test 4: Transition matrix builds correctly from imported nodes/edges
# ============================================================================
cat("Test 4: Transition matrix from imported data...\n")
n <- nrow(native$nodes)
mat <- matrix(0, n, n)
rownames(mat) <- colnames(mat) <- native$nodes$hypothesis
for (i in seq_len(nrow(native$edges))) {
  from_idx <- which(native$nodes$id == native$edges$from[[i]])
  to_idx <- which(native$nodes$id == native$edges$to[[i]])
  if (length(from_idx) == 1 && length(to_idx) == 1) {
    mat[from_idx, to_idx] <- native$edges$weight[[i]]
  }
}
# H1: 0.25 to H2 + 0.75 to H3 = 1.0
stopifnot(abs(sum(mat["H1", ]) - 1.0) < 1e-12)
# H2: 0.25 to H1 + 0.75 to H4 = 1.0
stopifnot(abs(sum(mat["H2", ]) - 1.0) < 1e-12)
# H3: 0.5 to H2 + 0.5 to H4 = 1.0
stopifnot(abs(sum(mat["H3", ]) - 1.0) < 1e-12)
# H4: 1.0 to H2
stopifnot(abs(sum(mat["H4", ]) - 1.0) < 1e-12)
cat("  PASS: all row sums = 1, edges map correctly\n")

cat("\nAll JSON import assertions passed.\n")
