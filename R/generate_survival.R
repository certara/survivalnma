#' calculate_survival
#'
#' calculate the survival function based on input distribution family
#'
#' @param family family of distribution - currently supported are
#' \code{c("weibull", "gompertz", "exponential", "log-normal", "log-logistic")}
#' @param params a matrix of parameters to be used with specified distribution
#' @param times a vector of times where hazard will be evaluated at
#'
#' @return matrix of the survival function evaluated at input times
#'
#' @note calculate_survival CANNOT produce the survival function for fp1, fp2. For that,
#'       use  \code{\link{survival_curve}} function, together with a vector of times
#' @noRd


calculate_survival <- function(family, params, times){
  if (family == "weibull"){
    surv.func <- exp(- ((exp(params[1])/(params[2]+1))*(times^(params[2]+1))))
  }else if (family == "gompertz"){
    surv.func <- exp( -( (exp(params[1])/params[2] )*(exp(times*params[2]) - 1)) )
  }else if (family == "exponential"){
    surv.func <- exp( -times* exp(params) )
  }else if (family == "loglogistic"){
    surv.func <- 1/(1+ (exp(-params[1])*times)^(exp(params[2])) )
  }else if (family == "lognormal"){
    surv.func <- pnorm(-(log(times) - params[1])/exp(params[2]))
  }else{
    stop("Wrong or unspecified family of distributions")
  }
  return(surv.func)
}
