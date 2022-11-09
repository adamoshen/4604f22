bisect <- function(f, a, b, epsilon) {
  fa <- f(a)
  fb <- f(b)

  if (fa * fb > 0) {
    stop("There is no zero between the supplied values of `a` and `b`.")
  }

  numiter <- 200

  for (i in 1:numiter) {
    xstar <- (a + b) / 2
    fxstar <- f(xstar)

    if (fa * fxstar <= 0) {
      b <- xstar
      fb <- fxstar
    } else {
      a <- xstar
      fa <- fxstar
    }

    if (abs(b - a) < epsilon) {
      break
    }

    if (i == numiter) {
      stop("Maximum number of iterations exceeded!")
    }
  }

  list(
    xstar = (a + b) / 2,
    err = abs(b - a),
    fxstar = f(xstar)
  )
}
