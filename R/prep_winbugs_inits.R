# fixed grid of inits (non-random)
# currently not in use
prep_winbugs_inits <- function(type, ns, nt, nchains, dim) {
  # for now type = fixed only
  ll <- list()
  for(i in 1:nchains){
    if(dim > 1)
      ll[[i]] <- list(
        "mu" = matrix(c(rep(-1,ns),
                        rep(0.1, ns)), ns, 2),
        "d" = matrix(c(NA, rep(1,nt-1),
                       NA, rep(-0.1,nt-1)), nt, 2))
    else
      ll[[i]] <- list(
        "mu" = rnorm(ns),
        "d" = c(NA, rnorm(nt-1))
      )
  }
  ll
}

# randomised grid of inits
# dim = number of parameters (1 for exponential, 2 for weibull, ... etc.)
prep_winbugs_inits2 <- function(type, ns, nt, nchains, dim) {
  ll <- list()
  for(i in 1:nchains){
    if(dim > 1)
      ll[[i]] <- list(
        "mu" = matrix(c(rnorm(ns),
                        0.1*rnorm(ns)), ns, 2),
        "d" = matrix(c(NA, rnorm(nt-1, 0, 1),
                       NA, 0.1*rnorm(nt-1)), nt, 2))
    else
      ll[[i]] <- list(
        "mu" = rnorm(ns),
        "d" = c(NA, rnorm(nt-1)))
  }
  ll
}
