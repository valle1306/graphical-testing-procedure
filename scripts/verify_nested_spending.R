# Verify the corrected engine against independent canonical integration and
# explicit intersection tests, including a strict current-only/closure subset.
args <- commandArgs(FALSE)
entry <- sub("^--file=", "", args[startsWith(args, "--file=")][1])
root <- normalizePath(file.path(dirname(entry), ".."), winslash = "/")
setwd(root)
.libPaths(c(file.path(root, ".Rlibs"), .libPaths()))
suppressPackageStartupMessages(source("app.R", local = TRUE))
source("R/server/sequential_boundary_helpers.R")
source("R/server/numerical_guards.R")
source("Haybittle-Peto.r")
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
spec <- function(rule = "OF") list(timing = c(.33,.67,1), information = c(33,67,100),
  maximum = 100, rounds = 1:3, rule = rule,
  proportions = switch(rule, OF = NULL, Pocock = gsDesign::sfLDPocock(1,c(.33,.67,1))$spend,
    HSD = gsDesign::sfHSD(1,c(.33,.67,1),-4)$spend,
    Custom = c(.2,.6,1), `Haybittle-Peto` = HP(p1=.0003,overall.alpha=.0125,
      timing=c(.33,.67,1))$cum.alpha/.0125))
corr <- outer(c(.33,.67,1), c(.33,.67,1), function(s,t) sqrt(pmin(s,t)/pmax(s,t)))
for (rule in c("OF","Pocock","HSD","Custom","Haybittle-Peto")) {
  local <- spec(rule)
  levels <- c(.00625,.0125,.025)
  b <- lapply(levels, function(a) graphmtp_local_boundaries(local, a))
  cutoffs <- sapply(b, function(x) x$stageLevels)
  stopifnot(all(apply(cutoffs, 1, diff) >= -1e-10))
  for (i in seq_along(levels)) {
    cross <- 1-as.numeric(mvtnorm::pmvnorm(upper=b[[i]]$criticalValues,
      corr=corr, algorithm=mvtnorm::Miwa(steps=256)))
    stopifnot(abs(cross-levels[i]) < 1e-7)
  }
}
expect_error(graphmtp_local_boundaries(spec(), 1e-6))
expect_error(graphmtp_local_boundaries(spec(), .4))
tiny <- spec(); tiny$timing <- c(.01,.67,1)
expect_error(graphmtp_local_boundaries(tiny, .025))

# Independent intersection weights via absorption, not runtime edge updates.
intersection_levels <- function(a, G, J) {
  C <- setdiff(seq_along(a), J)
  if (!length(C)) return(a[J])
  # Only complement nodes with a positive-weight path to J can contribute.
  # Closed terminal classes (and nodes leading only to them) absorb level
  # without delivering it to J; retaining them would make I - G_CC singular.
  reachable <- J
  repeat {
    predecessors <- C[rowSums(G[C, reachable, drop=FALSE] > 0) > 0]
    expanded <- union(reachable, predecessors)
    if (setequal(expanded, reachable)) break
    reachable <- expanded
  }
  transient <- intersect(C, reachable)
  if (!length(transient)) return(a[J])
  as.numeric(a[J] + a[transient] %*% solve(
    diag(length(transient))-G[transient,transient,drop=FALSE],
    G[transient,J,drop=FALSE]))
}
closed_rejections <- function(a, G, P, specs, through) {
  m <- length(a)
  intersections <- lapply(seq_len(2^m-1L), function(mask) which(as.logical(intToBits(mask)[seq_len(m)])))
  rejected <- vapply(intersections, function(J) {
    local_alpha <- intersection_levels(a,G,J)
    any(vapply(seq_along(J), function(i) {
      if (local_alpha[i] == 0) return(FALSE)
      cutoff <- graphmtp_local_boundaries(specs[[J[i]]], local_alpha[i])$stageLevels
      any(P[J[i],seq_len(through)] < cutoff[seq_len(through)])
    }, logical(1)))
  }, logical(1))
  vapply(seq_len(m), function(i) all(rejected[vapply(intersections,function(J) i %in% J,logical(1))]),logical(1))
}
G <- matrix(c(0,.5,.5,0,0,1,0,0,0),3,byrow=TRUE)
a <- c(.01,.01,.005)
specs <- setNames(rep(list(spec()),3), paste0("H",1:3))
set.seed(20260910)
for (fixture in 1:16) {
  P <- matrix(sample(c(.000001,.001,.005,.015,.04,.5),9,replace=TRUE),3)
  g <- new_guarded_graphical_testing(alpha=a,transition=G,
    alpha_spending=rep("asOF",3),planned_max_info=rep(100L,3),
    hypotheses=names(specs),silent=TRUE,local_specs=specs)
  for (k in 1:3) {
    active <- which(vapply(1:3,g$is_in_graph,logical(1)))
    if (length(active)) g$test(data.frame(order=k,hypotheses=names(specs)[active],
      p=P[active,k],info=c(33,67,100)[k],is_final=k==3,max_info=100))
    actual <- !vapply(1:3,g$is_in_graph,logical(1))
    closure <- closed_rejections(a,G,P,specs,k)
    stopifnot(all(!actual | closure))
  }
}
# Terminal H2-H3 cycle cannot transfer its level to H1. A fourth node
# additionally splits its allocation between H1 and the closed cycle.
terminal_cycle <- matrix(c(0,0,0, 0,0,1, 0,1,0), 3, byrow=TRUE)
split_cycle <- matrix(0, 4, 4)
split_cycle[2,3] <- split_cycle[3,2] <- 1
split_cycle[4,c(1,2)] <- .5
cycle_cases <- list(
  list(G=terminal_cycle, a=c(.005,.01,.01),
    P=rbind(c(1,1,.004), c(0,1,1), c(0,1,1)),
    expected_h1=.005),
  list(G=split_cycle, a=c(.005,.005,.005,.01),
    P=rbind(c(1,1,.008), c(0,1,1), c(0,1,1), c(0,1,1)),
    expected_h1=.01)
)
stopifnot(
  abs(intersection_levels(c(.005,.01,.01), terminal_cycle, 1)-.005) < 1e-12,
  abs(intersection_levels(c(.005,.01,.01), terminal_cycle, 2)-.02) < 1e-12,
  abs(intersection_levels(c(.005,.005,.005,.01), split_cycle, 1)-.01) < 1e-12,
  max(abs(intersection_levels(c(.005,.005,.005,.01), split_cycle, c(1,2))-
    c(.01,.015))) < 1e-12
)
for (case in cycle_cases) {
  m <- length(case$a)
  # This deliberately exercises the singular complement of singleton H1.
  stopifnot(qr(diag(m-1L)-case$G[-1,-1,drop=FALSE])$rank < m-1L)
  specs <- setNames(rep(list(spec()),m), paste0("H",seq_len(m)))
  g <- new_guarded_graphical_testing(alpha=case$a, transition=case$G,
    alpha_spending=rep("asOF",m), planned_max_info=rep(100L,m),
    hypotheses=names(specs), silent=TRUE, local_specs=specs)
  for (k in 1:3) {
    active <- which(vapply(seq_len(m),g$is_in_graph,logical(1)))
    if (length(active)) g$test(data.frame(order=k,hypotheses=names(specs)[active],
      p=case$P[active,k],info=c(33,67,100)[k],is_final=k==3,max_info=100))
    actual <- !vapply(seq_len(m),g$is_in_graph,logical(1))
    closure <- closed_rejections(case$a,case$G,case$P,specs,k)
    stopifnot(all(!actual | closure))
    if (k == 1L) stopifnot(!actual[1], all(actual[-1]),
      abs(g$get_alpha(1L)-case$expected_h1) < 1e-12)
  }
  stopifnot(all(actual), all(closure))
}
# A later allocation increase does not retest a past p-value.
G <- matrix(c(0,0,1,0),2,byrow=TRUE)
a <- c(.0125,.0125)
specs <- setNames(rep(list(spec()),2),c("H1","H2"))
old_first <- graphmtp_local_boundaries(spec(),.0125)$stageLevels[1]
new_first <- graphmtp_local_boundaries(spec(),.025)$stageLevels[1]
P <- rbind(c(mean(c(old_first,new_first)),1,1),c(1,.000001,1))
g <- new_guarded_graphical_testing(alpha=a,transition=G,alpha_spending=rep("asOF",2),
  planned_max_info=rep(100L,2),hypotheses=names(specs),silent=TRUE,local_specs=specs)
g$test(data.frame(order=1L,hypotheses=names(specs),p=P[,1],info=33,is_final=FALSE,max_info=100))
frozen <- g$get_trajectory()
g$test(data.frame(order=2L,hypotheses=names(specs),p=P[,2],info=67,is_final=FALSE,max_info=100))
stopifnot(g$is_in_graph(1L),all(closed_rejections(a,G,P,specs,2)),
  isTRUE(all.equal(frozen,g$get_trajectory()[seq_len(nrow(frozen)),,drop=FALSE],
    tolerance=0,check.attributes=FALSE)))
# Unsupported information and terminal changes fail before direct mutation.
before <- g$get_trajectory()
expect_error(g$test(data.frame(order=3L,hypotheses="H1",p=.01,info=110,is_final=TRUE,max_info=110)))
stopifnot(identical(before,g$get_trajectory()))
cat("Five local families: integration and nesting passed; 16 three-node and two terminal-cycle fixtures satisfy explicit closure containment; known absorption levels, immutable history, no look-back, and unsupported adaptation checks passed.\n")
