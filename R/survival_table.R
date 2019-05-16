#' survival_table
#'
#' internal function that evaluates survivals at each month
#' if you have pre-calculated survival values you can pass
#' them directly to reduce computation time.
#'
#' @param survnma survnma object to be passed
#' @param study study of interest
#' @param treatments a list of desired treatments to be compared
#' @param timesteps Vector of evaluation times. Should be of length > 2
#' @param survivals precalculated survivals (using prep_all_survivals)
#'                to reduce computation time
#' @param print Boolean, if true prints results in console, if false stores dataframe
#'
#' @export
#' @importFrom knitr kable
#' @import magrittr


survival_table <- function(survnma, study, treatments = NULL,
                           timesteps = seq(0,30),
                           survivals = NULL, print = FALSE){
  if(is.null(treatments)){
    treatments <- survnma$treatments
  }
  if(is.null(survivals)){
      survivals <- prep_all_survivals(survnma, study, treatments, timesteps = timesteps)
  }

  survival <- survivals %>%
    split(f = survivals$label)

  treatments <- unique(survivals$label)

  means <- lapply(treatments,
                  function(x) data.frame(survival[[x]]$mean))
  names(means) <- treatments

  if(length(timesteps) == 1){
    stop("Evaluation time vector should have length > 2")
  }else{
    times <- timesteps
    times[times == 0] <- 0.001
  }

  meansdf <- data.frame(sapply(means,c))
  colnames(meansdf) <- treatments

  meansdf <- cbind(time = times,
                   month = round(times, 0),
                   dt = abs(times - round(times,0)),
                   meansdf)

  meansdf <- meansdf[, -c(2,3)]

  if(print == TRUE)
    print(knitr::kable(meansdf,
                       align = c(rep('c',ncol(meansdf))),
                       caption = "Survivals table"))

  else
    return(meansdf)

}
