fdcn <- function(s, k, r, t, sd,
                     n = ceiling(1e3*t), m = 2*ceiling(sqrt(3*n)),
                     type = c("call", "put"), style = c("european", "american"),
                     grid = FALSE) {
  if (t <= 0) stop("t = ", t, " is nonpositive!")
  if (!is.wholenumber(n) || n <= 0) stop("n = ",n," is not a positive integer!")
  if (!is.wholenumber(m) || m <= 0) stop("m = ",m," is not a positive integer!")
  type <- match.arg(type); style <- match.arg(style)
  
  dt <- t / n
  m <- m + m%%2                         # Ensure m is even.
  ## Set stock price limits to +/- 3 standard deviations.
  z.lim <- log(s) + 3*sd*sqrt(t)*c(min=-1, max=1)
  dz <- unname(diff(z.lim)) / m
  z.seq <- z.lim['min'] + 0:m*dz        # vector, m+1 elements
  
  f <- matrix(rep(NA, (n+1)*(m+1)), nrow=n+1)
  g2m <- function(i)  i + 1             # grid index to matrix index
  f[g2m(n),] = switch(type, call=pmax(exp(z.seq)-k,0), put=pmax(k-exp(z.seq),0))
  
  p.u <- -dt/4*(sd^2/dz^2 + (r - 1/2*sd^2)/dz)
  p.m <- 1 + dt*(sd^2/(2*dz^2) + r/2)
  p.d <- -dt/4*(sd^2/dz^2 - (r - 1/2*sd^2)/dz)
  tridiag <- rbind(0, cbind(diag(c(rep(p.u, m-1), 1)), 0)) +
    diag(c(1, rep(p.m, m-1), -1)) +
    rbind(cbind(0, diag(c(-1, rep(p.d, m-1)))), 0)
  c <- tridiag
  d <- diag(2, m+1) - tridiag           # Note: 1st & last rows corrected later.
  for (i in g2m((n-1):0)) {             # Iterate from end to beginning.
    rhs <- d %*% f[i+1,g2m(m:0)]
    if (type == 'call') {
      rhs[g2m(0)] <- exp(z.seq[g2m(m)]) - exp(z.seq[g2m(m-1)])
      rhs[g2m(m)] <- 0
    }
    else if (type == 'put') {
      rhs[g2m(0)] <- 0
      rhs[g2m(m)] <- exp(z.seq[g2m(m)]) - exp(z.seq[g2m(m-1)])
    }
    f[i,g2m(m:0)] <- solve(c, rhs)
    
    if (type == 'put' && style == 'american')
      f[i,] <- pmax(f[i,], k - exp(z.seq))
  }
  
  if (grid) return(f) else return(f[g2m(0), g2m(m/2)])
}

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

## Rotate `x' counterclockwise by 90 degrees.
rotate <- function(x, digits = NULL) {
  if (is.null(digits)) apply(x, 1, rev)
  else round(apply(x, 1, rev), digits=digits)
}

BS_greeks <- function (t, S, r, sigma, K, T, type) 
{
  if (type == "call"){
    d1 <- (log(S/K) + (r + sigma^2/2) * (T - t))/(sigma * sqrt(T - 
                                                                 t))
    d2 <- d1 - sigma * sqrt(T - t)
    cbind(delta = pnorm(d1), theta = -(S * dnorm(d1) * sigma)/(2 * 
                                                                 sqrt(T - t)) - r * K * exp(-r * (T - t)) * pnorm(d2), 
          rho = K * (T - t) * pnorm(d2) * exp(-r * (T - t)), vega = S * 
            dnorm(d1) * sqrt(T - t), gamma = dnorm(d1)/(S * sigma * 
                                                          sqrt(T - t)), vanna = -dnorm(d1) * d2/sigma, vomma = S * 
            dnorm(d1) * sqrt(T - t) * d1 * d2/sigma)
  }else{
    d1 <- (log(S/K) + (r + sigma^2/2) * (T - t))/(sigma * sqrt(T - 
                                                                 t))
    d2 <- d1 - sigma * sqrt(T - t)
    cbind(delta = -pnorm(-d1), theta = -(S * dnorm(d1) * sigma)/(2 * 
                                                                 sqrt(T - t)) + r * K * exp(-r * (T - t)) * pnorm(-d2), 
          rho = -K * (T - t) * pnorm(-d2) * exp(-r * (T - t)), vega = S * 
            dnorm(d1) * sqrt(T - t), gamma = dnorm(d1)/(S * sigma * 
                                                          sqrt(T - t)), vanna = -dnorm(d1) * d2/sigma, vomma = S * 
            dnorm(d1) * sqrt(T - t) * d1 * d2/sigma)
  }
}