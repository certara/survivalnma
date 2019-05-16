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
#' @return a vector \code{c(q1, median, q3, mean)}
#' @import stats

survival_curve <- function(family, param.mtx, times, P = NULL,
                           type = "interval") {
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

  # in our test scenario
  # dim(res) = 1002, 999

  # dim(param.mtx) = 1002x2
  # and
  # length(times) = 999

  # hence res contains survivals evaluated at all time points, with all parameters
  # and then its mean (per time point) is taken as an average over all parameters

  # idea is to extract which parameter is causing the mean to spike
  # but the extracted parameter is not close to 0...

  # problematic_index <- which.max(res)
  # if(max(res[problematic_index,]) > 1){
  #   warning("There has been a faulty survival value greater than 1.",
  #          " The parameters that caused this were: ", param.mtx[problematic_index, ][1],
  #          " and ", param.mtx[problematic_index, ][2] )
  # }
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
