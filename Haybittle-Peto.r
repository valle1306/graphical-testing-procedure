#Haybittle-Peto spending function and rejection boundary
HP <- function (p1 = 3e-04, overall.alpha = 0.025, timing = c(0.5, 0.7, 1)) {
  M = length(timing)
  if (M < 1) {
    stop("Haybittle-Peto requires at least one planned analysis.")
  }
  if (!is.finite(p1) || p1 <= 0 || p1 >= 1) {
    stop("Haybittle-Peto early-look p-value must lie strictly between 0 and 1.")
  }
  if (!is.finite(overall.alpha) || overall.alpha <= 0 || overall.alpha >= 1) {
    stop("Haybittle-Peto overall alpha must lie strictly between 0 and 1.")
  }

  # The single-look case is a fixed design. It is handled first: the correlation
  # loop below is only defined for M >= 2 (with M = 1, `1:(M - 1)` counts *down*
  # to zero and indexes corr[1, 2], which does not exist).
  if (M == 1) {
    z1 = qnorm(1 - overall.alpha)
    return(data.frame(
      p = overall.alpha,
      z = z1,
      alpha = overall.alpha,
      cum.alpha = overall.alpha,
      overall.alpha = overall.alpha
    ))
  }

  corr = matrix(1, nrow = M, ncol = M)
  for (i in 1:(M - 1)) {
    for (j in (i + 1):M) {
      corr[i, j] = corr[j, i] = sqrt(timing[i]/timing[j])
    }
  }

  z1 = qnorm(1 - p1)
  a = rep(NA, M)
  if (M == 2) {
    a[1] = p1
    a[2] = overall.alpha - a[1]
  }
  if (M > 2) {
    a[1] = p1
    for (i in 2:(M - 1)) {
      a[i] = mvtnorm::pmvnorm(lower = c(rep(-Inf, i - 1), z1),
                              upper = c(rep(z1, i - 1), Inf),
                              corr = corr[1:i, 1:i],
                              abseps = 1e-08, maxpts = 1e+05)[1]
    }
    a[M] = overall.alpha - sum(a[1:(M - 1)])
  }

  # The interim looks each spend roughly p1, so a large p1 (or many looks) can
  # exhaust the budget before the final analysis. Left alone this drives the
  # final stage alpha negative and uniroot() then fails on an uninformative
  # error, or returns a meaningless root.
  if (!is.finite(a[M]) || a[M] <= 0) {
    stop(sprintf(
      paste(
        "Haybittle-Peto: the interim looks spend %s of the available %s, leaving",
        "nothing for the final analysis. Reduce the early-look p-value (currently",
        "%s) or the number of looks (currently %s)."
      ),
      format(sum(a[1:(M - 1)])), format(overall.alpha), format(p1), M
    ))
  }

  f.x = function(x) {
    I = mvtnorm::pmvnorm(lower = c(rep(-Inf, M - 1), x),
                         upper = c(rep(z1, M - 1), Inf),
                         corr = corr,
                         abseps = 1e-08, maxpts = 1e+05)[1]
    return(I - a[M])
  }
  zf = uniroot(f = f.x, interval = c(1, 10), tol = 1e-08)$root
  pf = 1 - pnorm(zf)

  p = c(rep(p1, M - 1), pf)
  z = c(rep(z1, M - 1), zf)
  alpha = a; cum.alpha = rep(NA, M)
  for (i in 1:M){cum.alpha[i] = sum(alpha[1:i])}
  o = data.frame(cbind(p, z, alpha, cum.alpha, overall.alpha))
  return(o)
}


