#'
#' hr_curve
#'
#' A function that calculates the hazard ratio at multiple time points with multiple parameters from
#' the specified param.mtx passed through it. Used internally on \code{\link{prep_all_hazards}}
#'
#' @param family a string indicating the family of distribution used in your survnma object
#' @param param.mtx a matrix containing all the parameters that you want to consider
#' @param times a vector indicating the time points the hazard-ratio will be evaluated at
#' @param P integer/vector of powers to be used solely for fractional polynomials
#' @param type string indating whether you want to obtain all hazard-ratio evaluations, a mean of them
#'             or both means and lower and upper bounds of the confidence interval corresponding to the mean
#'
#' @return a matrix to be used directly on \code{\link{prep_all_hazards}}
#' @import stats
#' @noRd

hr_curve <- function(family, param.mtx, times, P =NULL,
                     type = "interval") {
  colnames(param.mtx) <- NULL
  if(family != "exponential"){
    res <- t(apply(param.mtx, 1, function(current_params){
      sapply(times, function(current_time) generate_hazard(family,
                                                           current_params,
                                                           current_time,
                                                           P))}
    ))
  }
  if(family == "exponential"){
    res <- data.frame()
    for(current_params in param.mtx){
      res <- rbind(res, sapply(times, function(current_time) generate_hazard(family,
                                                                             current_params,
                                                                             current_time)
      ))}
    colnames(res) <- NULL
  }
  if(length(times) == 1){
    warning("Length of time vector should be at least 2")
  }
  if(type == "interval"){
    return(apply(res, 2, function(x) c(stats::quantile(x, .025),
                                       mean= mean(x),
                                       stats::quantile(x, .975))))
  }
  if(type == "mean"){
    return(apply(res, 2, mean))
  }
  if(type == "all"){
    return(res)
  }
}
