#Haybittle-Peto spending function and rejection boundary
HP <- function (p1 = 3e-04, overall.alpha = 0.025, timing = c(0.5, 0.7, 1)) {
  M = length(timing)
  corr = matrix(1, nrow = M, ncol = M)
  for (i in 1:(M - 1)) {
    for (j in (i + 1):M) {
      corr[i, j] = corr[j, i] = sqrt(timing[i]/timing[j])
    }
  }
  if (M == 1) {
    pf = p1 = overall.alpha
    zf = z1 = qnorm(1 - overall.alpha)
  }
  else {
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
    f.x = function(x) {
      I = mvtnorm::pmvnorm(lower = c(rep(-Inf, M - 1), x), 
                           upper = c(rep(z1, M - 1), Inf), 
                           corr = corr, 
                           abseps = 1e-08, maxpts = 1e+05)[1]
      return(I - a[M])
    }
    zf = uniroot(f = f.x, interval = c(1, 10), tol = 1e-08)$root
    pf = 1 - pnorm(zf)
  }
  p = c(rep(p1, M - 1), pf)
  z = c(rep(z1, M - 1), zf)
  alpha = a; cum.alpha = rep(NA, M)
  for (i in 1:M){cum.alpha[i] = sum(alpha[1:i])}
  o = data.frame(cbind(p, z, alpha, cum.alpha, overall.alpha))
  return(o)
}


