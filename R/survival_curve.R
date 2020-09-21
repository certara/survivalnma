#' survival_curve
#'
#' Survival curve calculation for parametric families
#'
#' @param family character; any of the following distributional families:
#'               \code{c("weibull", "gompertz", "exponential", "log-normal", "log-logistic", "fp1", "fp2")}
#' @param param.mtx is a \code{[n x m]} numeric matrix, where \code{m} is the number of parameters
#'                  \code{n} is number of samples from their underlying distribution
#' @param times vector of timepoints at which to calculate survival S(t)
#' @param P vector of powers, used only if \code{family == "fp1"} (uses \code{p[1]})
#'          or \code{family == "fp2"} (uses \code{p[1]} and \code{p[2]})
#' @param type string: interval, mean or all
#' @param warn logical; warn if some of the calculated values are above 1 (this happens for some of the
#'             models in Ouwens et al paper where the treatment parameters move out of the support)
#' @param trt_warn Treatment name (to be used when producing warnings only).
#' @return a vector \code{c(q1, median, q3, mean)}
#' @import stats
#' @noRd

survival_curve <- function(family, param.mtx, times, P = NULL,
                           type = "interval", warn = TRUE, trt_warn = "unknown") {
  if(length(times) == 1){
    warning("Length of time vector should be at least 2")
  }

  # remove column names (if any)
  if( family == "exponential"){
    param.mtx <- as.matrix(param.mtx, ncol = 1)
    if(dim(param.mtx)[2] != 1)
      stop("Number of columns in exponential parameter matrix is wrong! Must be equal to 1")
  }
  colnames(param.mtx) <- NULL

  if (family != "fp1" && family != "fp2"){
    res <- t(apply(param.mtx, 1, function(current_params) {
      sapply(times,
             function(current_time)
               calculate_survival(family, current_params, current_time))
    }
    ))
  }else{
    res <- t(apply(param.mtx, 1, function(current_params) {
      sapply(times, function(current_time) {
        exp(-integrate(generate_hazard, times[1], current_time,
                       params = current_params, family = family, P=P,
                       stop.on.error = F)$value)
      })
    }))
  }

  res_no_na <- res
  res_no_na[is.na(res)] <- 1e06 #flag NAs
  if(warn && (max(res_no_na) > 1)){
    invalid_percent <- round(100*sum(apply(res_no_na, 1, function(x) any(x > 1)))/nrow(res_no_na), 2)
    res[res > 1] <- 1
    res[is.na(res)] <- 1 #flag NAs
    warning("In ", invalid_percent, "% of posterior samples the survival curve for ", trt_warn,
            " was greater than 1 or NA for at least 1 time point. ",
            "These invalid values are replaced by 1. When this % is high, this will make results invalid. ",
            "Please refer to the package vignette to understand why this error sometimes occurs.")
  }
  if(type == "interval"){
    if(any(is.na(res))){
      return(matrix(NA,3,length(times)))
    }else{
    return(apply(res, 2, function(x) c(stats::quantile(x, .025, na.rm = TRUE),
                                       mean= mean(x),
                                       stats::quantile(x, .975, na.rm = TRUE))))
    }
  }
  if(type == "mean"){
    return(apply(res, 2, mean))
  }
  if(type == "all"){
    return(res)
  }
}
