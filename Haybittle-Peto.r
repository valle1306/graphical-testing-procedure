# One-sided Haybittle-Peto boundaries under canonical joint normal statistics.
# The interim nominal cutoff must leave a positive error budget for the final look.
HP <- function(p1 = 3e-04, overall.alpha = 0.025, timing = c(0.5, 0.7, 1)) {
  stopifnot(length(overall.alpha) == 1L, is.finite(overall.alpha),
            overall.alpha > 0, overall.alpha < 1)
  timing <- as.numeric(timing)
  if (!length(timing) || any(!is.finite(timing)) || any(timing <= 0) ||
      any(diff(timing) <= 0) || abs(tail(timing, 1) - 1) > 1e-8) {
    stop("Information fractions must increase from a positive value to 1.")
  }
  M <- length(timing)
  if (M == 1L) {
    return(data.frame(p = overall.alpha, z = qnorm(overall.alpha, lower.tail = FALSE),
      alpha = overall.alpha, cum.alpha = overall.alpha, overall.alpha = overall.alpha))
  }
  if (length(p1) != 1L || !is.finite(p1) || p1 <= 0 || p1 >= overall.alpha) {
    stop("The Haybittle-Peto interim cutoff must be positive and below the local alpha.")
  }
  z1 <- qnorm(p1, lower.tail = FALSE)
  crossing <- function(bounds, information) {
    as.numeric(gsDesign::gsProbability(k = length(bounds), theta = 0,
      n.I = information, a = rep(-20, length(bounds)), b = bounds, r = 32)$upper$prob)
  }
  interim <- crossing(rep(z1, M - 1L), timing[seq_len(M - 1L)])
  remaining <- overall.alpha - sum(interim)
  if (remaining <= 0) stop("Haybittle-Peto interim looks exhaust the local alpha.")
  f <- function(z) tail(crossing(c(rep(z1, M - 1L), z), timing), 1) - remaining
  zf <- uniroot(f, interval = c(-12, 12), tol = 1e-10)$root
  z <- c(rep(z1, M - 1L), zf)
  spent <- c(interim, remaining)
  data.frame(p = pnorm(z, lower.tail = FALSE), z = z, alpha = spent,
    cum.alpha = cumsum(spent), overall.alpha = overall.alpha)
}


