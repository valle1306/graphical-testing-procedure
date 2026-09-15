# Validate an imported graph before changing reactive state. Legacy labels and
# omitted layout fields are accepted; invalid statistical inputs are not repaired.
normalize_graph_import <- function(dat) {
  if (!is.list(dat)) stop("The imported JSON must contain a graph object.")
  if (!is.null(dat$format_version) && (length(dat$format_version) != 1L ||
      !as.integer(dat$format_version) %in% c(2L, 3L))) {
    stop("Unsupported graph file version.")
  }
  nodes <- tryCatch(as.data.frame(dat$nodes, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(nodes) || !nrow(nodes)) stop("Imported file contains no nodes.")
  if (is.null(nodes$hypothesis)) nodes$hypothesis <- nodes$label
  if (is.null(nodes$hypothesis)) nodes$hypothesis <- paste0("H", seq_len(nrow(nodes)))
  nodes$hypothesis <- trimws(as.character(nodes$hypothesis))
  if (anyNA(nodes$hypothesis) || any(!nzchar(nodes$hypothesis)) || anyDuplicated(nodes$hypothesis)) {
    stop("Hypothesis labels must be nonempty and unique.")
  }
  if (is.null(nodes$id)) nodes$id <- seq_len(nrow(nodes))
  raw_ids <- suppressWarnings(as.numeric(nodes$id))
  if (any(!is.finite(raw_ids)) || any(raw_ids < 1 | raw_ids > .Machine$integer.max) ||
      any(raw_ids != floor(raw_ids)) || anyDuplicated(raw_ids)) stop("Node identifiers must be unique positive integers.")
  nodes$id <- as.integer(raw_ids)
  if (is.null(nodes$alpha)) nodes$alpha <- 0
  nodes$alpha <- suppressWarnings(as.numeric(nodes$alpha))
  if (any(!is.finite(nodes$alpha)) || any(nodes$alpha < 0 | nodes$alpha > 1) || sum(nodes$alpha) > 1 + 1e-12) {
    stop("Initial local alpha levels must be finite, nonnegative, and sum to at most 1.")
  }
  if (is.null(nodes$x) || is.null(nodes$y)) nodes <- auto_layout_nodes(nodes)
  nodes$x <- suppressWarnings(as.numeric(nodes$x))
  nodes$y <- suppressWarnings(as.numeric(nodes$y))
  if (any(!is.finite(nodes$x)) || any(!is.finite(nodes$y))) stop("Node positions must be finite.")
  nodes <- nodes[, c("id", "x", "y", "hypothesis", "alpha"), drop = FALSE]
  edges <- tryCatch(as.data.frame(dat$edges, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(edges) || !nrow(edges)) {
    edges <- data.frame(id = integer(), from = integer(), to = integer(), weight = numeric())
  } else {
    if (!all(c("from", "to") %in% names(edges))) stop("Every edge needs from and to endpoints.")
    resolve <- function(x) {
      if (is.character(x)) {
        mapped <- nodes$id[match(x, nodes$hypothesis)]
        numeric_ids <- suppressWarnings(as.numeric(x))
        mapped[is.na(mapped)] <- numeric_ids[is.na(mapped)]
        x <- mapped
      }
      if (anyNA(x) || any(!x %in% nodes$id)) stop("An edge refers to an unknown node.")
      as.integer(x)
    }
    edges$from <- resolve(edges$from)
    edges$to <- resolve(edges$to)
    if (is.null(edges$weight)) edges$weight <- edges$label
    if (is.null(edges$weight)) stop("Every edge needs a transition weight.")
    edges$weight <- suppressWarnings(as.numeric(edges$weight))
    if (any(!is.finite(edges$weight)) || any(edges$weight < 0 | edges$weight > 1)) stop("Edge weights must lie in [0, 1].")
    if (any(edges$from == edges$to)) stop("Self-loops are not allowed.")
    if (anyDuplicated(paste(edges$from, edges$to))) stop("Duplicate directed edges are not allowed.")
    edges$id <- seq_len(nrow(edges))
    edges <- edges[, c("id", "from", "to", "weight"), drop = FALSE]
  }
  list(nodes = tibble::as_tibble(nodes), edges = tibble::as_tibble(edges))
}
