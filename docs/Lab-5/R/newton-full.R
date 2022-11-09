newton <- function(f, df, x0, niter=200, epsilon) {
  fx0 <- f(x0)

  for (i in 1:niter) {
    dfx0 <- df(x0)
    xstar <- x0 - fx0 / dfx0
    fxstar <- f(xstar)
    err <- abs(fxstar)

    if (err < epsilon) {
      break
    }

    x0 <- xstar
    fx0 <- fxstar

    if (i == niter) {
      stop("Newton's method failed to converge!")
    }
  }

  list(
    xstar = xstar,
    err = err,
    fxstar = fxstar
  )
}
